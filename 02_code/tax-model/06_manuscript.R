# Build manuscript tables and figures from the projection output.

source("/Users/wangmengyao/Desktop/Github/tax-modeling/02_code/tax-model/00_config.R",local = TRUE)
use_packages(c("ggplot2", "gridExtra"))

table_years <- 2027:2032

projection_path <- file.path(model_output_dir, "all_projection.rds")
all_projection <- readRDS(projection_path)
annual_summary_df <- all_projection$annual_summary
base_rev <- readRDS(file.path(model_output_dir, "baseline_revenue.rds"))
manuscript_output_dir <- data_output_dir
dir.create(manuscript_output_dir, recursive = TRUE, showWarnings = FALSE)

# Index 1 keeps the tax increase constant; index 0 applies the 3% annual decay.
inflation_scenarios <- data.frame(
  index = c(1L, 0L),
  scenario = c("model_noinf", "model_inf")
)

state_order <- unique(annual_summary_df[, c("state_fips", "state_abbr")])
state_order <- state_order[order(as.integer(state_order$state_fips)), ]

annual_summary_df$scaled_revenue_per_capita <- ifelse(
  annual_summary_df$population > 0,
  annual_summary_df$state_revenue_model_scaled / annual_summary_df$population,
  NA_real_
)

proj_table <- function(value_col, cumulative_col) {
  rows <- vector("list", nrow(state_order) * nrow(inflation_scenarios) * 5L)
  row_idx <- 1L
  
  for (i in seq_len(nrow(state_order))) {
    fips <- state_order$state_fips[i]
    state <- state_order$state_abbr[i]
    
    baseline <- annual_summary_df[
      annual_summary_df$state_fips == fips &
        annual_summary_df$scenario == "baseline" &
        annual_summary_df$Calendar_Year %in% table_years,
      c("Calendar_Year", value_col), drop = FALSE]
    baseline_values <- baseline[[value_col]][match(table_years, baseline$Calendar_Year)]
    
    for (j in seq_len(nrow(inflation_scenarios))) {
      index <- inflation_scenarios$index[j]
      scenario <- inflation_scenarios$scenario[j]
      
      add_row <- function(label, values) {
        row <- data.frame(
          state_abbr = state, "index for inflation" = index,
          scenario = label, check.names = FALSE
        )
        row[paste0("FY", table_years)] <- as.list(values)
        row[[cumulative_col]] <- sum(values[table_years >= 2028L])
        rows[[row_idx]] <<- row
        row_idx <<- row_idx + 1L
      }
      
      add_row("baseline", baseline_values)
      
      for (tax in c(1, 2)) {
        policy <- annual_summary_df[
          annual_summary_df$state_fips == fips &
            annual_summary_df$scenario == scenario &
            annual_summary_df$tax_increase_dollar == tax &
            annual_summary_df$Calendar_Year %in% table_years,
          c("Calendar_Year", value_col), drop = FALSE]
        policy_values <- policy[[value_col]][match(table_years, policy$Calendar_Year)]
        
        add_row(paste0("$", tax), policy_values)
        add_row(paste0("$", tax, " revenue gain"), policy_values - baseline_values)
      }
    }
  }
  
  do.call(rbind, rows)
}

format_table <- function(df, cumulative_col, digits) {
  cols <- c(paste0("FY", table_years), cumulative_col)
  df[cols] <- lapply(df[cols], function(x)
    paste0("$", formatC(x, format = "f", digits = digits, big.mark = ",")))
  df
}

gain_only <- function(df) {
  df <- df[grepl("revenue gain$", df$scenario), , drop = FALSE]
  df$scenario <- sub(" revenue gain$", "", df$scenario)
  names(df)[names(df) == "scenario"] <- "revenue gain"
  df
}

total_col <- "cumulative revenue (FY28-FY32)"
pc_col <- "cumulative revenue per capita (FY28-FY32)"

total_revenue_raw <- proj_table("state_revenue_model_scaled", total_col)
total_revenue <- format_table(total_revenue_raw, total_col, 0)
utils::write.csv(total_revenue,
                 file.path(manuscript_output_dir, "revenue_table.csv"),
                 row.names = FALSE)

concise_revenue_table_raw <- gain_only(total_revenue_raw)
concise_revenue_table <- gain_only(total_revenue)
utils::write.csv(concise_revenue_table,
                 file.path(manuscript_output_dir, "revenue_gain.csv"),
                 row.names = FALSE)

per_capita_revenue_table_raw <- proj_table("scaled_revenue_per_capita", pc_col)
per_capita_revenue_table <- format_table(per_capita_revenue_table_raw, pc_col, 2)
utils::write.csv(per_capita_revenue_table,
                 file.path(manuscript_output_dir, "revenue_per_capita.csv"),
                 row.names = FALSE)

#===============================================================================
# Figure3: cumulative revenue & cumulative revenue per capita gains by state
#===============================================================================

gain_plot <- function(df, title, x_label, x_labels, state_order) {
  df$state_abbr <- factor(df$state_abbr, levels = state_order)
  
  ggplot2::ggplot(
    df, ggplot2::aes(cumulative_gain, state_abbr, color = tax_scenario,
                     shape = inflation_setting)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(group = interaction(state_abbr, tax_scenario)), linewidth = 0.7
    ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_color_manual(
      values = c("$1 tax increase" = "#0072B2", "$2 tax increase" = "#D55E00")
    ) +
    ggplot2::scale_shape_manual(values = c("Index = 1" = 16, "Index = 0" = 17)) +
    ggplot2::scale_x_continuous(labels = x_labels) +
    ggplot2::labs(
      title = title, x = x_label, y = NULL,
      color = "Tax scenario", shape = "Inflation index"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      legend.box = "vertical",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

gain_data <- function(df, column, divisor = 1) {
  df$cumulative_gain <- as.numeric(df[[column]]) / divisor
  df$tax_scenario <- paste0(df[["revenue gain"]], " tax increase")
  df$inflation_setting <- ifelse(df[["index for inflation"]] == 1, "Index = 1", "Index = 0")
  df
}

state_order <- function(df) {
  state_max <- aggregate(cumulative_gain ~ state_abbr, df, max)
  state_max$state_abbr[order(state_max$cumulative_gain)]
}

cumulative_column <- "cumulative revenue (FY28-FY32)"
per_capita_column <- "cumulative revenue per capita (FY28-FY32)"

absolute_df <- gain_data(concise_revenue_table_raw, cumulative_column, 1e6)

per_capita_df <- per_capita_revenue_table_raw[
  grepl("revenue gain$", per_capita_revenue_table_raw$scenario), , drop = FALSE]
per_capita_df$scenario <- sub(" revenue gain$", "", per_capita_df$scenario)
names(per_capita_df)[names(per_capita_df) == "scenario"] <- "revenue gain"
per_capita_df <- gain_data(per_capita_df, per_capita_column)

combined_plot <- gridExtra::arrangeGrob(
  gain_plot(
    absolute_df, "Absolute cumulative revenue gain",
    "Cumulative revenue gain, FY2028-FY2032",
    function(x) paste0("$", format(x, big.mark = ",", trim = TRUE), "M"),
    state_order(absolute_df)
  ),
  gain_plot(
    per_capita_df, "Per-capita cumulative revenue gain",
    "Cumulative revenue gain per capita, FY2028-FY2032",
    function(x) paste0("$", format(x, big.mark = ",", trim = TRUE)),
    state_order(per_capita_df)
  ),
  ncol = 2,
  top = grid::textGrob(
    "Projected Cumulative Cigarette Tax Revenue Gain by State",
    gp = grid::gpar(fontsize = 16, fontface = "bold")
  )
)

ggplot2::ggsave(
  file.path(manuscript_output_dir, "revenue_ranking.png"),
  combined_plot, width = 13, height = 15, dpi = 300, bg = "white"
)

#===============================================================================
# Supplementary

# --- Supplementary state descriptive table -----------------------------

reference_year <- max(base_rev$Calendar_Year, na.rm = TRUE)
reference_df <- base_rev[base_rev$Calendar_Year == reference_year, , drop = FALSE]

state_rows <- lapply(split(reference_df, reference_df$state_abbr), function(df) {
  valid_cpd <- is.finite(df$avg_cpd) & is.finite(df$smokers) & df$smokers > 0
  population <- sum(df$population, na.rm = TRUE)
  smokers <- sum(df$smokers, na.rm = TRUE)
  cpd_weight <- sum(df$smokers[valid_cpd], na.rm = TRUE)
  price <- df$state_price_per_pack_wt_cent[
    which(is.finite(df$state_price_per_pack_wt_cent))[1]
  ]
  tax_rate <- df$state_tax_rate_dollar[
    which(is.finite(df$state_tax_rate_dollar))[1]
  ]
  
  data.frame(
    state_abbr = df$state_abbr[1],
    cigarette_price_per_pack_dollar = price / 100,
    state_cigarette_excise_tax_per_pack_dollar = tax_rate,
    smoking_prevalence_pct = if (population > 0) smokers / population * 100 else NA_real_,
    cigarettes_per_day = if (cpd_weight > 0) {
      sum(df$avg_cpd[valid_cpd] * df$smokers[valid_cpd], na.rm = TRUE) / cpd_weight
    } else {
      NA_real_
    }
  )
})

descriptive_df <- do.call(rbind, state_rows)
descriptive_df <- descriptive_df[
  match(state_lookup$state_abbr, descriptive_df$state_abbr), ,
  drop = FALSE
]
rownames(descriptive_df) <- NULL

utils::write.csv(
  descriptive_df,
  file.path(manuscript_output_dir, "state_descriptive.csv"),
  row.names = FALSE
)

# --- state projection scatter ----------------------------
policy_year <- all_projection$metadata$policy_year
outcome_years <- policy_year:(policy_year + 4L)

baseline <- annual_summary_df[
  annual_summary_df$scenario == "baseline" & annual_summary_df$Calendar_Year %in% outcome_years,
  c("state_abbr", "Calendar_Year", "state_revenue_model_scaled"), drop = FALSE]
policy <- annual_summary_df[
  annual_summary_df$scenario == "model_inf" & annual_summary_df$tax_increase_dollar == 1 &
    annual_summary_df$Calendar_Year %in% outcome_years,
  c("state_abbr", "Calendar_Year", "population", "state_revenue_model_scaled"), drop = FALSE]

names(baseline)[3] <- "baseline_revenue"
names(policy)[4] <- "policy_revenue"

gain <- merge(policy, baseline, by = c("state_abbr", "Calendar_Year"), all = FALSE, sort = FALSE)
gain$revenue_gain_per_capita <- (gain$policy_revenue - gain$baseline_revenue) / gain$population

outcome <- aggregate(revenue_gain_per_capita ~ state_abbr, gain, sum)
names(outcome)[2] <- "cumulative_revenue_gain_per_capita"
scatter_df <- merge(descriptive_df, outcome, by = "state_abbr", all = FALSE, sort = FALSE)

measures <- c(
  cigarette_price_per_pack_dollar = "Cigarette price per pack ($)",
  state_cigarette_excise_tax_per_pack_dollar = "State cigarette excise tax per pack ($)",
  smoking_prevalence_pct = "Smoking prevalence (%)",
  cigarettes_per_day = "Cigarettes per day among current smokers"
)

scatter_plots <- lapply(names(measures), function(measure)
  ggplot2::ggplot(
    scatter_df,
    ggplot2::aes(x = .data[[measure]], y = cumulative_revenue_gain_per_capita)
  ) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.8,
                         color = "#4C78A8", fill = "#9ECAE9") +
    ggplot2::geom_point(size = 2.2, color = "#D55E00") +
    ggplot2::geom_text(ggplot2::aes(label = state_abbr), size = 2.5, nudge_y = 0.5,
                       check_overlap = TRUE, color = "gray25") +
    ggplot2::labs(x = measures[[measure]], y = "Cumulative revenue gain per capita ($)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
)

scatter_figure <- gridExtra::arrangeGrob(
  grobs = scatter_plots, ncol = 2,
  top = grid::textGrob(
    sprintf("$1 Policy (Index = 0): Cumulative Revenue Gain per Capita, FY%d-FY%d",
            min(outcome_years), max(outcome_years)),
    gp = grid::gpar(fontsize = 15, fontface = "bold")
  )
)

ggplot2::ggsave(file.path(manuscript_output_dir, "revenue_scatter.png"),
                scatter_figure, width = 12, height = 9, dpi = 300, bg = "white")



