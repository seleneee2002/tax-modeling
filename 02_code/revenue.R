

repo_root <- "/Users/wangmengyao/Desktop/Github/tax-modeling"
output_dir <- file.path(repo_root, "03_output", format(Sys.Date(), "%Y-%m-%d"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
year_min <- 2010L
year_max <- 2022L
consumption_elasticity <- -0.2
inflation_adjustment_rate <- 0.97

baseline_results_0803 <- readRDS(
  file.path(repo_root, "01_input_data", "baseline_results_0803.rds")
)
price_df <- utils::read.csv(
  file.path(repo_root, "01_input_data", "cigarette_prices_by_state_2025.csv"),
  stringsAsFactors = FALSE
)
price_df$state_abbr <- toupper(trimws(price_df$State))
price_df$default_init_price <- as.numeric(price_df$default_init_price)

state_policy_configs <- data.frame(
  state_fips = c("42", "40", "21"),
  state_abbr = c("PA", "OK", "KY"),
  state_name = c("Pennsylvania", "Oklahoma", "Kentucky"),
  policy_year = c(2017L, 2019L, 2019L),
  tax_increase_dollar = c(1.00, 1.00, 0.50),
  stringsAsFactors = FALSE
)
state_policy_configs$init_price <- price_df$default_init_price[
  match(state_policy_configs$state_abbr, price_df$state_abbr)
]

inflation_adjusted_tax_increase <- function(base_tax_increase, current_year, policy_year) {
  ifelse(
    current_year >= policy_year,
    base_tax_increase * (inflation_adjustment_rate ^ (current_year - policy_year)),
    0
  )
}

build_long_df <- function(state_fips, state_abbr, sex, smokers_mat, pop_mat) {
  years <- as.integer(colnames(pop_mat))
  keep_years <- years >= year_min & years <= year_max
  years <- years[keep_years]
  smokers_mat <- smokers_mat[, keep_years, drop = FALSE]
  pop_mat <- pop_mat[, keep_years, drop = FALSE]

# -------------------------------------------------------------------------


  age_idx <- seq_len(nrow(pop_mat)) - 1L

  grid <- expand.grid(
    age = age_idx,
    year = years,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  grid$state_fips <- state_fips
  grid$state_abbr <- state_abbr
  grid$sex <- sex
  grid$START_YOB <- grid$year - grid$age
  grid$END_YOB <- grid$START_YOB
  grid$AGE <- grid$age
  grid$Calendar_Year <- grid$year
  grid$smokers <- as.vector(smokers_mat)
  grid$population <- as.vector(pop_mat)
  grid$prevalence <- ifelse(grid$population > 0, grid$smokers / grid$population, NA_real_)

  grid <- grid[, c(
    "state_fips",
    "state_abbr",
    "sex",
    "START_YOB",
    "END_YOB",
    "AGE",
    "Calendar_Year",
    "prevalence",
    "smokers",
    "population"
  )]

  grid
}

out_list <- vector("list", length(baseline_results_0803) * 2L)
idx <- 1L

for (i in seq_along(baseline_results_0803)) {
  state_fips <- names(baseline_results_0803)[i]
  item <- baseline_results_0803[[i]]
  state_abbr <- unique(item$df_mort.outputs$abbr)[1]

  out_list[[idx]] <- build_long_df(
    state_fips = state_fips,
    state_abbr = state_abbr,
    sex = "Male",
    smokers_mat = item$l_prev_out$m_M_smokers,
    pop_mat = item$l_prev_out$m_M_popAP
  )
  idx <- idx + 1L

  out_list[[idx]] <- build_long_df(
    state_fips = state_fips,
    state_abbr = state_abbr,
    sex = "Female",
    smokers_mat = item$l_prev_out$m_F_smokers,
    pop_mat = item$l_prev_out$m_F_popAP
  )
  idx <- idx + 1L
}

output_df <- do.call(rbind, out_list)

build_population_long_df_from_sheet <- function(workbook_path, sheet_name, lookup_df) {
  raw_df <- suppressMessages(
    readxl::read_excel(workbook_path, sheet = sheet_name, .name_repair = "minimal")
  )

  sheet_parts <- strsplit(sheet_name, "-", fixed = TRUE)[[1]]
  state_abbr <- sheet_parts[1]
  sex_value <- sheet_parts[2]
  state_fips <- lookup_df$state_fips[match(state_abbr, lookup_df$state_abbr)][1]

  if (is.na(state_fips) || nrow(raw_df) == 0) {
    return(NULL)
  }

  year_cols <- names(raw_df)[grepl("^[0-9]{4}$", names(raw_df))]
  if (length(year_cols) == 0) {
    return(NULL)
  }

  age_values <- as.integer(raw_df[[1]])
  population_wide_df <- raw_df[, year_cols, drop = FALSE]

  if (max(age_values, na.rm = TRUE) == 85L) {
    age_85_idx <- which(age_values == 85L)

    if (length(age_85_idx) == 1L) {
      expanded_85_df <- population_wide_df[age_85_idx, , drop = FALSE]
      expanded_85_df <- expanded_85_df[rep(1, 15), , drop = FALSE] / 15

      population_wide_df <- rbind(
        population_wide_df[age_values < 85L, , drop = FALSE],
        expanded_85_df
      )
      age_values <- c(age_values[age_values < 85L], 85:99)
    }
  }

  stacked_values <- stack(population_wide_df)

  data.frame(
    state_fips = sprintf("%02s", state_fips),
    state_abbr = state_abbr,
    sex = sex_value,
    AGE = rep(age_values, times = length(year_cols)),
    Calendar_Year = as.integer(as.character(stacked_values$ind)),
    population_update = as.numeric(stacked_values$values),
    stringsAsFactors = FALSE
  )
}

population_state_lookup <- unique(output_df[, c("state_fips", "state_abbr")])
population_state_lookup$state_fips <- sprintf("%02s", population_state_lookup$state_fips)
population_state_lookup$state_abbr <- as.character(population_state_lookup$state_abbr)

population_sheets <- readxl::excel_sheets(
  file.path(repo_root, "01_input_data", "Population2004_2030_byState.xlsx")
)
population_updates_df <- do.call(
  rbind,
  lapply(
    population_sheets,
    build_population_long_df_from_sheet,
    workbook_path = file.path(repo_root, "01_input_data", "Population2004_2030_byState.xlsx"),
    lookup_df = population_state_lookup
  )
)

population_reference_df <- output_df[, c(
  "state_fips",
  "state_abbr",
  "sex",
  "START_YOB",
  "END_YOB",
  "AGE",
  "Calendar_Year",
  "population"
)]
population_reference_df$state_fips <- sprintf("%02s", population_reference_df$state_fips)

population_reference_key <- paste(
  population_reference_df$state_fips,
  population_reference_df$sex,
  population_reference_df$AGE,
  population_reference_df$Calendar_Year,
  sep = "|"
)
population_update_key <- paste(
  population_updates_df$state_fips,
  population_updates_df$sex,
  population_updates_df$AGE,
  population_updates_df$Calendar_Year,
  sep = "|"
)
population_match_idx <- match(population_reference_key, population_update_key)
population_keep_idx <- !is.na(population_match_idx)

population_reference_df$population[population_keep_idx] <-
  population_updates_df$population_update[population_match_idx[population_keep_idx]]

output_df_key <- paste(
  sprintf("%02s", output_df$state_fips),
  output_df$sex,
  output_df$AGE,
  output_df$Calendar_Year,
  sep = "|"
)
output_population_match_idx <- match(output_df_key, population_reference_key)
output_population_keep_idx <- !is.na(output_population_match_idx)

output_df$population[output_population_keep_idx] <-
  population_reference_df$population[output_population_match_idx[output_population_keep_idx]]
output_df$smokers <- output_df$prevalence * output_df$population

utils::write.csv(
  output_df,
  file.path(output_dir, "baseline_results.csv"),
  row.names = FALSE
)


#===============================================================================
# Baseline revenue model
#===============================================================================


# Dataset Merge
baseline_df <- merge(
  output_df[, c(
    "state_fips",
    "state_abbr",
    "sex",
    "START_YOB",
    "END_YOB",
    "AGE",
    "Calendar_Year",
    "prevalence",
    "smokers"
  )],
  population_reference_df,
  by = c(
    "state_fips",
    "state_abbr",
    "sex",
    "START_YOB",
    "END_YOB",
    "AGE",
    "Calendar_Year"
  ),
  all.x = TRUE,
  sort = FALSE
)
baseline_df$state_fips <- sprintf("%02s", baseline_df$state_fips)

state_lookup <- unique(baseline_df[, c("state_fips", "state_abbr")])
state_lookup$state_abbr <- as.character(state_lookup$state_abbr)
state_lookup$state_fips <- sprintf("%02s", state_lookup$state_fips)

build_cpd_long_df <- function(cpd_path, lookup_df) {
  state_abbr <- sub("^US-", "", basename(dirname(dirname(cpd_path))))
  cpd_df <- utils::read.csv(cpd_path, stringsAsFactors = FALSE)
  cpd_df <- cpd_df[cpd_df$RACE == 0, , drop = FALSE]
  cpd_df$state_abbr <- state_abbr
  cpd_df$state_fips <- lookup_df$state_fips[match(state_abbr, lookup_df$state_abbr)]
  cpd_df$sex <- ifelse(cpd_df$SEX == 0, "Male", "Female")
  cpd_df$Calendar_Year <- cpd_df$START_YOB + cpd_df$AGE
  cpd_df <- cpd_df[cpd_df$Calendar_Year >= year_min & cpd_df$Calendar_Year <= year_max, , drop = FALSE]

  cpd_df[, c(
    "state_fips",
    "state_abbr",
    "sex",
    "START_YOB",
    "END_YOB",
    "AGE",
    "Calendar_Year",
    "CAT1",
    "CAT2",
    "CAT3",
    "CAT4",
    "CAT5",
    "CAT6"
  )]
}

cpd_files <- Sys.glob(
  file.path(repo_root, "01_input_data", "2019-TUS-CPS-States", "US-*", "smoking", "cpd.csv")
)
cpd_long_df <- do.call(rbind, lapply(cpd_files, build_cpd_long_df, lookup_df = state_lookup))

merge_keys <- c(
  "state_fips",
  "state_abbr",
  "sex",
  "START_YOB",
  "END_YOB",
  "AGE",
  "Calendar_Year"
)

baseline_df$row_id <- seq_len(nrow(baseline_df))

baseline_revenue <- merge(
  baseline_df,
  cpd_long_df,
  by = merge_keys,
  all.x = TRUE,
  sort = FALSE
)

baseline_revenue <- baseline_revenue[order(baseline_revenue$row_id), ]

tcp_revenue_df <- utils::read.csv(
  file.path(repo_root, "01_input_data", "data_for_tcp_revenue.csv"),
  stringsAsFactors = FALSE
)
names(tcp_revenue_df)[names(tcp_revenue_df) == "state"] <- "state_abbr"
names(tcp_revenue_df)[names(tcp_revenue_df) == "year"] <- "Calendar_Year"

baseline_revenue <- merge(
  baseline_revenue,
  tcp_revenue_df,
  by = c("state_abbr", "Calendar_Year"),
  all.x = TRUE,
  sort = FALSE
)

baseline_revenue <- baseline_revenue[order(baseline_revenue$row_id), ]

final_col_order <- c(
  "state_fips",
  "state_abbr",
  "sex",
  "START_YOB",
  "END_YOB",
  "AGE",
  "Calendar_Year",
  "prevalence",
  "smokers",
  "population",
  "CAT1",
  "CAT2",
  "CAT3",
  "CAT4",
  "CAT5",
  "CAT6",
  setdiff(names(tcp_revenue_df), c("state_abbr", "Calendar_Year"))
)

baseline_revenue <- baseline_revenue[, c(final_col_order, "row_id")]
baseline_revenue$row_id <- NULL

openxlsx::write.xlsx(
  population_reference_df,
  file.path(output_dir, "population_reference.xlsx"),
  overwrite = TRUE
)
openxlsx::write.xlsx(
  baseline_revenue,
  file.path(output_dir, "baseline_revenue.xlsx"),
  overwrite = TRUE
)

# Revenue Calculation
cpd_midpoints <- c(2.5, 10, 20, 30, 40, 50)

baseline_revenue_calculation <- baseline_revenue
baseline_revenue_calculation$state_tax_rate_dollar <-
  baseline_revenue_calculation$state_tax_rate_cent / 100

cat_matrix <- as.matrix(
  baseline_revenue_calculation[, c("CAT1", "CAT2", "CAT3", "CAT4", "CAT5", "CAT6")]
)

baseline_revenue_calculation$average_cpd_smokers <- as.numeric(cat_matrix %*% cpd_midpoints)
baseline_revenue_calculation$average_cpd_smokers[
  rowSums(is.na(cat_matrix)) == ncol(cat_matrix)
] <- NA_real_

baseline_revenue_calculation$total_cigarettes_smoked_per_year_per_smoker <-
  365 * baseline_revenue_calculation$average_cpd_smokers

baseline_revenue_calculation$cigarette_packs_smoked_per_year_per_smoker <-
  baseline_revenue_calculation$total_cigarettes_smoked_per_year_per_smoker / 20

baseline_revenue_calculation$cigarette_packs_smoked_per_year_total_model <-
  baseline_revenue_calculation$smokers *
  baseline_revenue_calculation$cigarette_packs_smoked_per_year_per_smoker

baseline_revenue_calculation$state_revenue_model <-
  baseline_revenue_calculation$state_tax_rate_dollar *
  baseline_revenue_calculation$cigarette_packs_smoked_per_year_total_model

baseline_revenue_calculation$consumption_elasticity <- NA_real_
baseline_revenue_calculation$price_before_tax_dollar <- NA_real_
baseline_revenue_calculation$price_after_tax_dollar <- NA_real_
baseline_revenue_calculation$percentage_change_in_price <- NA_real_
baseline_revenue_calculation$cpd_pct_change <- NA_real_
baseline_revenue_calculation$cpd_after_tax <- NA_real_
baseline_revenue_calculation$tax_increase_dollar_effective <- 0

for (state_idx in seq_len(nrow(state_policy_configs))) {
  state_abbr_value <- state_policy_configs$state_abbr[state_idx]
  policy_year_value <- as.integer(state_policy_configs$policy_year[state_idx])
  tax_increase_dollar_value <- as.numeric(state_policy_configs$tax_increase_dollar[state_idx])
  init_price_value <- as.numeric(state_policy_configs$init_price[state_idx])

  policy_idx <- baseline_revenue_calculation$state_abbr == state_abbr_value &
    baseline_revenue_calculation$Calendar_Year >= policy_year_value

  baseline_revenue_calculation$consumption_elasticity[policy_idx] <- consumption_elasticity
  baseline_revenue_calculation$price_before_tax_dollar[policy_idx] <- init_price_value
  baseline_revenue_calculation$tax_increase_dollar_effective[policy_idx] <-
    inflation_adjusted_tax_increase(
      tax_increase_dollar_value,
      baseline_revenue_calculation$Calendar_Year[policy_idx],
      policy_year_value
    )
  baseline_revenue_calculation$price_after_tax_dollar[policy_idx] <-
    init_price_value + baseline_revenue_calculation$tax_increase_dollar_effective[policy_idx]
  baseline_revenue_calculation$percentage_change_in_price[policy_idx] <-
    baseline_revenue_calculation$tax_increase_dollar_effective[policy_idx] / init_price_value
  baseline_revenue_calculation$cpd_pct_change[policy_idx] <-
    baseline_revenue_calculation$consumption_elasticity[policy_idx] *
    baseline_revenue_calculation$percentage_change_in_price[policy_idx]
  baseline_revenue_calculation$cpd_after_tax[policy_idx] <-
    baseline_revenue_calculation$average_cpd_smokers[policy_idx] *
    (1 + baseline_revenue_calculation$cpd_pct_change[policy_idx])
}

state_year_modeled_packs <- ave(
  baseline_revenue_calculation$cigarette_packs_smoked_per_year_total_model,
  baseline_revenue_calculation$state_abbr,
  baseline_revenue_calculation$Calendar_Year,
  FUN = function(x) sum(x, na.rm = TRUE)
)


baseline_revenue_calculation$state_revenue_real <-
  ifelse(
    state_year_modeled_packs > 0,
    (baseline_revenue_calculation$state_tax_revenue_in_thousand * 1000) *
      baseline_revenue_calculation$cigarette_packs_smoked_per_year_total_model /
      state_year_modeled_packs,
    NA_real_
  )

openxlsx::write.xlsx(
  baseline_revenue_calculation,
  file.path(output_dir, "baseline_revenue_calculation.xlsx"),
  overwrite = TRUE
)


#===============================================================================
# Scaling factor (Pennsylvania only)
#===============================================================================
pa_baseline_revenue_calculation <- baseline_revenue_calculation[
  baseline_revenue_calculation$state_abbr == "PA",
  ,
  drop = FALSE
]

pa_smokers_by_year <- aggregate(
  smokers ~ Calendar_Year,
  data = pa_baseline_revenue_calculation,
  FUN = function(x) sum(x, na.rm = TRUE)
)
names(pa_smokers_by_year)[names(pa_smokers_by_year) == "smokers"] <- "number_of_smokers"

pa_population_by_year <- aggregate(
  population ~ Calendar_Year,
  data = pa_baseline_revenue_calculation,
  FUN = function(x) sum(x, na.rm = TRUE)
)
names(pa_population_by_year)[names(pa_population_by_year) == "population"] <- "total_population"

pa_model_packs_by_year <- aggregate(
  cigarette_packs_smoked_per_year_total_model ~ Calendar_Year,
  data = pa_baseline_revenue_calculation,
  FUN = function(x) sum(x, na.rm = TRUE)
)
names(pa_model_packs_by_year)[
  names(pa_model_packs_by_year) == "cigarette_packs_smoked_per_year_total_model"
] <- "model_packs_smoked"

pa_tbot_by_year <- unique(
  pa_baseline_revenue_calculation[, c(
    "Calendar_Year",
    "state_cig_pack_per_capita",
    "state_cig_sales_pack_in_million"
  )]
)
pa_tbot_by_year <- pa_tbot_by_year[order(pa_tbot_by_year$Calendar_Year), , drop = FALSE]
pa_tbot_by_year$taxed_packs_sold <- pa_tbot_by_year$state_cig_sales_pack_in_million * 1000000
pa_tbot_by_year$state_cig_sales_pack_in_million <- NULL

pa_scaling_factor_df <- Reduce(
  function(x, y) merge(x, y, by = "Calendar_Year", all = TRUE, sort = FALSE),
  list(pa_smokers_by_year, pa_population_by_year, pa_model_packs_by_year, pa_tbot_by_year)
)
pa_scaling_factor_df <- pa_scaling_factor_df[order(pa_scaling_factor_df$Calendar_Year), , drop = FALSE]
pa_scaling_factor_df$state_abbr <- "PA"

pa_scaling_factor_df$tbot_packs_per_capita <- as.numeric(pa_scaling_factor_df$state_cig_pack_per_capita)

pa_scaling_factor_df$model_packs_per_capita <- ifelse(
  !is.na(pa_scaling_factor_df$total_population) &
    pa_scaling_factor_df$total_population > 0,
  pa_scaling_factor_df$model_packs_smoked / pa_scaling_factor_df$total_population,
  NA_real_
)

pa_scaling_factor_df$ratio <- ifelse(
  !is.na(pa_scaling_factor_df$model_packs_per_capita) &
    pa_scaling_factor_df$model_packs_per_capita > 0,
  pa_scaling_factor_df$state_cig_pack_per_capita / pa_scaling_factor_df$model_packs_per_capita,
  NA_real_
)

pa_scaling_factor_df$tbot_to_model_scaling_factor <- pa_scaling_factor_df$ratio

pa_scaling_factor_df <- pa_scaling_factor_df[, c(
  "state_abbr",
  "Calendar_Year",
  "number_of_smokers",
  "total_population",
  "state_cig_pack_per_capita",
  "taxed_packs_sold",
  "tbot_packs_per_capita",
  "model_packs_smoked",
  "model_packs_per_capita",
  "ratio",
  "tbot_to_model_scaling_factor"
)]

baseline_revenue_calculation$tbot_to_model_scaling_factor <- NA_real_
pa_scaling_match_idx <- match(
  baseline_revenue_calculation$Calendar_Year[baseline_revenue_calculation$state_abbr == "PA"],
  pa_scaling_factor_df$Calendar_Year
)
baseline_revenue_calculation$tbot_to_model_scaling_factor[
  baseline_revenue_calculation$state_abbr == "PA"
] <- pa_scaling_factor_df$tbot_to_model_scaling_factor[pa_scaling_match_idx]

baseline_revenue_calculation$state_revenue_model_scaled <-
  baseline_revenue_calculation$state_revenue_model *
  baseline_revenue_calculation$tbot_to_model_scaling_factor

openxlsx::write.xlsx(
  baseline_revenue_calculation,
  file.path(output_dir, "baseline_revenue_calculation.xlsx"),
  overwrite = TRUE
)

openxlsx::write.xlsx(
  pa_scaling_factor_df,
  file.path(output_dir, "pa_scaling_factor.xlsx"),
  overwrite = TRUE
)

#===============================================================================
# Plot
#===============================================================================
dir.create(
  file.path(output_dir, "baseline_revenue_plots"),
  showWarnings = FALSE,
  recursive = TRUE
)
dir.create(
  file.path(output_dir, "baseline_revenue_plots_pdf_6up"),
  showWarnings = FALSE,
  recursive = TRUE
)

fiscal_year_end_date <- function(year_value) {
  as.Date(sprintf("%04d-06-30", as.integer(year_value)))
}

fiscal_year_labels <- function(date_value) {
  paste0("FY", format(date_value, "%Y"))
}

state_year_revenue_plot_df <- aggregate(
  cbind(
    state_revenue_real,
    state_revenue_model
  ) ~ state_abbr + Calendar_Year,
  data = baseline_revenue_calculation,
  FUN = function(x) sum(x, na.rm = TRUE)
)
state_year_revenue_plot_df$Fiscal_Year_End <- fiscal_year_end_date(state_year_revenue_plot_df$Calendar_Year)

state_tax_plot_df <- tcp_revenue_df[
  tcp_revenue_df$Calendar_Year >= year_min & tcp_revenue_df$Calendar_Year <= year_max,
  c("state_abbr", "Calendar_Year", "state_tax_rate_cent")
]
state_tax_plot_df <- state_tax_plot_df[order(
  state_tax_plot_df$state_abbr,
  state_tax_plot_df$Calendar_Year
), ]

state_tax_events_list <- lapply(split(state_tax_plot_df, state_tax_plot_df$state_abbr), function(df) {
  df <- df[order(df$Calendar_Year), ]
  prev_tax <- c(NA_real_, head(df$state_tax_rate_cent, -1))
  increase_cent <- df$state_tax_rate_cent - prev_tax
  keep <- !is.na(increase_cent) & increase_cent > 0

  if (!any(keep)) {
    return(NULL)
  }

  data.frame(
    state_abbr = df$state_abbr[keep],
    Calendar_Year = df$Calendar_Year[keep],
    Fiscal_Year_End = fiscal_year_end_date(df$Calendar_Year[keep]),
    increase_dollar = increase_cent[keep] / 100,
    stringsAsFactors = FALSE
  )
})

state_tax_events_df <- do.call(rbind, state_tax_events_list)

if (!is.null(state_tax_events_df) && nrow(state_tax_events_df) > 0) {
  state_revenue_max_df <- aggregate(
    cbind(
      state_revenue_real,
      state_revenue_model
    ) ~ state_abbr,
    data = state_year_revenue_plot_df,
    FUN = max
  )
  state_revenue_max_df$label_y <- pmax(
    state_revenue_max_df$state_revenue_real,
    state_revenue_max_df$state_revenue_model
  ) * 1.06

  state_tax_events_df <- merge(
    state_tax_events_df,
    state_revenue_max_df[, c("state_abbr", "label_y")],
    by = "state_abbr",
    all.x = TRUE,
    sort = FALSE
  )
  state_tax_events_df$label <- paste0(
    "Tax +$",
    formatC(state_tax_events_df$increase_dollar, format = "f", digits = 2)
  )
}

total_year_revenue_plot_df <- aggregate(
  cbind(
    state_revenue_real,
    state_revenue_model
  ) ~ Calendar_Year,
  data = baseline_revenue_calculation,
  FUN = function(x) sum(x, na.rm = TRUE)
)

total_revenue_plot_df <- rbind(
  data.frame(
    Calendar_Year = total_year_revenue_plot_df$Calendar_Year,
    Fiscal_Year_End = fiscal_year_end_date(total_year_revenue_plot_df$Calendar_Year),
    revenue = total_year_revenue_plot_df$state_revenue_real,
    series = "Actual revenue",
    stringsAsFactors = FALSE
  ),
  data.frame(
    Calendar_Year = total_year_revenue_plot_df$Calendar_Year,
    Fiscal_Year_End = fiscal_year_end_date(total_year_revenue_plot_df$Calendar_Year),
    revenue = total_year_revenue_plot_df$state_revenue_model,
    series = "Modeled revenue",
    stringsAsFactors = FALSE
  )
)

total_revenue_plot <- ggplot2::ggplot(
  total_revenue_plot_df,
  ggplot2::aes(x = Fiscal_Year_End, y = revenue, color = series)
) +
  ggplot2::geom_line(linewidth = 1.1) +
  ggplot2::geom_point(size = 2.2) +
  ggplot2::scale_color_manual(
    values = c("Actual revenue" = "#1f77b4", "Modeled revenue" = "#d62728")
  ) +
  ggplot2::scale_x_date(
    breaks = fiscal_year_end_date(seq(year_min, year_max, by = 1)),
    labels = fiscal_year_labels
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) format(x, scientific = FALSE, trim = TRUE, big.mark = ",")
  ) +
  ggplot2::labs(
    title = "Total Revenue: Actual vs Modeled",
    x = "Fiscal year end (June 30)",
    y = "Revenue",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.key = ggplot2::element_rect(fill = "white", color = NA)
  )

ggplot2::ggsave(
  filename = file.path(
    output_dir,
    "baseline_revenue_plots",
    "revenue_comparison_total.png"
  ),
  plot = total_revenue_plot,
  width = 18,
  height = 6,
  dpi = 300,
  bg = "white"
)

state_abbr_groups <- split(
  sort(unique(state_year_revenue_plot_df$state_abbr)),
  ceiling(seq_along(sort(unique(state_year_revenue_plot_df$state_abbr))) / 6)
)

for (group_idx in seq_along(state_abbr_groups)) {
  state_group <- state_abbr_groups[[group_idx]]
  group_plot_df <- rbind(
    data.frame(
      state_abbr = state_year_revenue_plot_df$state_abbr,
      Calendar_Year = state_year_revenue_plot_df$Calendar_Year,
      Fiscal_Year_End = state_year_revenue_plot_df$Fiscal_Year_End,
      revenue = state_year_revenue_plot_df$state_revenue_real,
      series = "Real revenue",
      stringsAsFactors = FALSE
    ),
    data.frame(
      state_abbr = state_year_revenue_plot_df$state_abbr,
      Calendar_Year = state_year_revenue_plot_df$Calendar_Year,
      Fiscal_Year_End = state_year_revenue_plot_df$Fiscal_Year_End,
      revenue = state_year_revenue_plot_df$state_revenue_model,
      series = "Modeled revenue",
      stringsAsFactors = FALSE
    )
  )
  group_plot_df <- group_plot_df[
    group_plot_df$state_abbr %in% state_group,
    ,
    drop = FALSE
  ]
  group_plot_df$state_abbr <- factor(group_plot_df$state_abbr, levels = state_group)
  group_tax_events_df <- state_tax_events_df[state_tax_events_df$state_abbr %in% state_group, , drop = FALSE]
  if (nrow(group_tax_events_df) > 0) {
    group_tax_events_df$state_abbr <- factor(group_tax_events_df$state_abbr, levels = state_group)
  }

  p_group <- ggplot2::ggplot(
    group_plot_df,
    ggplot2::aes(x = Fiscal_Year_End, y = revenue, color = series)
  ) +
    ggplot2::geom_vline(
      data = group_tax_events_df,
      ggplot2::aes(xintercept = as.numeric(Fiscal_Year_End)),
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.5
    ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::geom_text(
      data = group_tax_events_df,
      ggplot2::aes(x = Fiscal_Year_End, y = label_y, label = label),
      inherit.aes = FALSE,
      angle = 0,
      vjust = 0,
      hjust = 0.5,
      size = 3,
      color = "gray25"
    ) +
    ggplot2::scale_color_manual(
      values = c("Real revenue" = "#1f77b4", "Modeled revenue" = "#d62728")
    ) +
    ggplot2::scale_x_date(
      breaks = fiscal_year_end_date(seq(year_min, year_max, by = 5)),
      labels = fiscal_year_labels
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) format(x, scientific = FALSE, trim = TRUE, big.mark = ","),
      expand = ggplot2::expansion(mult = c(0.02, 0.12))
    ) +
    ggplot2::facet_wrap(~ state_abbr, scales = "free_y", ncol = 2) +
    ggplot2::labs(
      title = paste("Real vs Modeled Revenue -", paste(state_group, collapse = ", ")),
      x = "Fiscal year end (June 30)",
      y = "Revenue",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      strip.text = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      "baseline_revenue_plots_pdf_6up",
      paste0("revenue_comparison_states_", sprintf("%02d", group_idx), ".pdf")
    ),
    plot = p_group,
    width = 14,
    height = 14,
    bg = "white"
  )
}

for (state_abbr in unique(state_year_revenue_plot_df$state_abbr)) {
  state_plot_df <- state_year_revenue_plot_df[
    state_year_revenue_plot_df$state_abbr == state_abbr,
    ,
    drop = FALSE
  ]
  state_tax_events_plot_df <- state_tax_events_df[state_tax_events_df$state_abbr == state_abbr, , drop = FALSE]

  plot_df <- rbind(
    data.frame(
      Calendar_Year = state_plot_df$Calendar_Year,
      Fiscal_Year_End = state_plot_df$Fiscal_Year_End,
      revenue = state_plot_df$state_revenue_real,
      series = "Real revenue",
      stringsAsFactors = FALSE
    ),
    data.frame(
      Calendar_Year = state_plot_df$Calendar_Year,
      Fiscal_Year_End = state_plot_df$Fiscal_Year_End,
      revenue = state_plot_df$state_revenue_model,
      series = "Modeled revenue",
      stringsAsFactors = FALSE
    )
  )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = Fiscal_Year_End, y = revenue, color = series)
  ) +
    ggplot2::geom_vline(
      data = state_tax_events_plot_df,
      ggplot2::aes(xintercept = as.numeric(Fiscal_Year_End)),
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.6
    ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_text(
      data = state_tax_events_plot_df,
      ggplot2::aes(x = Fiscal_Year_End, y = label_y, label = label),
      inherit.aes = FALSE,
      angle = 0,
      vjust = 0,
      hjust = 0.5,
      size = 3.5,
      color = "gray25"
    ) +
    ggplot2::scale_color_manual(
      values = c("Real revenue" = "#1f77b4", "Modeled revenue" = "#d62728")
    ) +
    ggplot2::scale_x_date(
      breaks = fiscal_year_end_date(seq(year_min, year_max, by = 1)),
      labels = fiscal_year_labels
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) format(x, scientific = FALSE, trim = TRUE, big.mark = ","),
      expand = ggplot2::expansion(mult = c(0.02, 0.12))
    ) +
    ggplot2::labs(
      title = paste("Real vs Modeled Revenue -", state_abbr),
      x = "Fiscal year end (June 30)",
      y = "Revenue",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA)
    )

  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      "baseline_revenue_plots",
      paste0("revenue_comparison_", state_abbr, ".png")
    ),
    plot = p,
    width = 9,
    height = 5,
    dpi = 300,
    bg = "white"
  )
}


#===============================================================================
# State baseline vs policy comparison
#===============================================================================
comparison_states <- state_policy_configs[, c("state_fips", "state_abbr", "state_name")]

build_mortality_summary <- function(mort_obj, scenario_name) {
  df <- mort_obj$df_mort.outputs
  df <- df[
    df$gender == "Both" &
      df$year >= year_min &
      df$year <= year_max,
    c("year", "LYG", "SADsAverted"),
    drop = FALSE
  ]
  names(df) <- c("Calendar_Year", "life_years_saved", "deaths_averted")
  df$scenario <- scenario_name
  df
}

for (state_idx in seq_len(nrow(comparison_states))) {
  state_fips_value <- comparison_states$state_fips[state_idx]
  state_abbr_value <- comparison_states$state_abbr[state_idx]
  state_name_value <- comparison_states$state_name[state_idx]
  simulation_prevalence_path <- file.path(
    output_dir,
    paste0(tolower(state_abbr_value), "_tax_simulation_prevalence.csv")
  )

  if (!file.exists(simulation_prevalence_path)) {
    next
  }

  simulation_prevalence_df <- utils::read.csv(
    simulation_prevalence_path,
    stringsAsFactors = FALSE
  )
  simulation_prevalence_df <- simulation_prevalence_df[
    simulation_prevalence_df$state_abbr == state_abbr_value,
    ,
    drop = FALSE
  ]

  if (nrow(simulation_prevalence_df) == 0) {
    next
  }

  policy_year_value <- as.integer(unique(simulation_prevalence_df$policy_year)[1])
  tax_increase_dollar_value <- as.numeric(unique(simulation_prevalence_df$tax_increase_dollar)[1])
  inflation_adjustment_rate_value <- as.numeric(unique(simulation_prevalence_df$inflation_adjustment_rate)[1])
  init_price_value <- price_df$default_init_price[price_df$state_abbr == state_abbr_value][1]

  merge_keys <- c(
    "state_fips",
    "state_abbr",
    "sex",
    "START_YOB",
    "END_YOB",
    "AGE",
    "Calendar_Year"
  )

  revenue_template <- baseline_revenue_calculation[
    baseline_revenue_calculation$state_abbr == state_abbr_value,
    c(
      merge_keys,
      "population",
      "cigarette_packs_smoked_per_year_per_smoker",
      "state_tax_rate_dollar"
    )
  ]

  tax_rate_by_year <- aggregate(
    state_tax_rate_dollar ~ Calendar_Year,
    data = revenue_template,
    FUN = function(x) x[which.max(!is.na(x))][1]
  )
  pre_policy_tax_rate <- tax_rate_by_year$state_tax_rate_dollar[
    tax_rate_by_year$Calendar_Year == max(tax_rate_by_year$Calendar_Year[tax_rate_by_year$Calendar_Year < policy_year_value])
  ][1]

  observed_df <- unique(
    baseline_revenue_calculation[
      baseline_revenue_calculation$state_abbr == state_abbr_value,
      c(
        "Calendar_Year",
        "state_tax_rate_dollar",
        "state_cig_pack_per_capita",
        "state_cig_sales_pack_in_million",
        "state_tax_revenue_in_thousand"
      )
    ]
  )
  observed_df <- observed_df[order(observed_df$Calendar_Year), , drop = FALSE]
  observed_df$observed_taxed_packs_sold <- observed_df$state_cig_sales_pack_in_million * 1000000
  observed_df$observed_tax_revenue <- observed_df$state_tax_revenue_in_thousand * 1000
  names(observed_df)[names(observed_df) == "state_tax_rate_dollar"] <- "observed_state_tax_rate_dollar"
  names(observed_df)[names(observed_df) == "state_cig_pack_per_capita"] <- "observed_state_cig_pack_per_capita"
  observed_df$state_cig_sales_pack_in_million <- NULL
  observed_df$state_tax_revenue_in_thousand <- NULL

  build_state_policy_summary <- function(
    prevalence_df,
    additional_tax_dollar,
    scenario_name,
    apply_inflation_adjustment,
    apply_consumption_adjustment
  ) {
    scenario_df <- merge(
      revenue_template,
      prevalence_df[, c(merge_keys, "prevalence")],
      by = merge_keys,
      all.x = TRUE,
      sort = FALSE
    )

    scenario_df$smokers <- scenario_df$prevalence * scenario_df$population
    scenario_df$scenario <- scenario_name
    scenario_df$tax_increase_dollar_effective <- if (apply_inflation_adjustment) {
      inflation_adjusted_tax_increase(
        additional_tax_dollar,
        scenario_df$Calendar_Year,
        policy_year_value
      )
    } else {
      ifelse(scenario_df$Calendar_Year >= policy_year_value, additional_tax_dollar, 0)
    }
    scenario_df$scenario_tax_rate_dollar <- ifelse(
      scenario_df$Calendar_Year >= policy_year_value,
      pre_policy_tax_rate + scenario_df$tax_increase_dollar_effective,
      scenario_df$state_tax_rate_dollar
    )
    scenario_df$price_before_tax_dollar <- ifelse(
      scenario_df$Calendar_Year >= policy_year_value,
      init_price_value,
      NA_real_
    )
    scenario_df$price_after_tax_dollar <- ifelse(
      scenario_df$Calendar_Year >= policy_year_value,
      init_price_value + scenario_df$tax_increase_dollar_effective,
      NA_real_
    )
    scenario_df$percentage_change_in_price <- ifelse(
      scenario_df$Calendar_Year >= policy_year_value &
        !is.na(scenario_df$price_before_tax_dollar) &
        scenario_df$price_before_tax_dollar > 0,
      (scenario_df$price_after_tax_dollar - scenario_df$price_before_tax_dollar) /
        scenario_df$price_before_tax_dollar,
      0
    )
    scenario_df$consumption_elasticity <- ifelse(
      scenario_df$Calendar_Year >= policy_year_value &
        additional_tax_dollar > 0 &
        apply_consumption_adjustment,
      consumption_elasticity,
      0
    )
    scenario_df$cpd_pct_change <-
      scenario_df$consumption_elasticity * scenario_df$percentage_change_in_price
    scenario_df$cpd_after_tax <-
      baseline_revenue_calculation$average_cpd_smokers[
        match(
          paste(scenario_df$state_fips, scenario_df$sex, scenario_df$AGE, scenario_df$Calendar_Year, sep = "|"),
          paste(
            baseline_revenue_calculation$state_fips,
            baseline_revenue_calculation$sex,
            baseline_revenue_calculation$AGE,
            baseline_revenue_calculation$Calendar_Year,
            sep = "|"
          )
        )
      ] * (1 + scenario_df$cpd_pct_change)
    cpd_adjustment_factor <- 1 + scenario_df$cpd_pct_change
    scenario_df$cigarette_packs_smoked_per_year_per_smoker_adjusted <-
      scenario_df$cigarette_packs_smoked_per_year_per_smoker *
      cpd_adjustment_factor
    scenario_df$packs_smoked_model <-
      scenario_df$smokers * scenario_df$cigarette_packs_smoked_per_year_per_smoker_adjusted
    scenario_df$tax_revenue_model <-
      scenario_df$scenario_tax_rate_dollar * scenario_df$packs_smoked_model

    total_population_df <- aggregate(
      population ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    total_smokers_df <- aggregate(
      smokers ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    total_packs_df <- aggregate(
      packs_smoked_model ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    total_revenue_df <- aggregate(
      tax_revenue_model ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    tax_rate_df <- aggregate(
      scenario_tax_rate_dollar ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) x[which.max(!is.na(x))][1]
    )
    effective_tax_df <- aggregate(
      tax_increase_dollar_effective ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) x[which.max(!is.na(x))][1]
    )
    price_before_df <- aggregate(
      price_before_tax_dollar ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) x[which.max(!is.na(x))][1]
    )
    price_after_df <- aggregate(
      price_after_tax_dollar ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) x[which.max(!is.na(x))][1]
    )
    percentage_change_in_price_df <- aggregate(
      percentage_change_in_price ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) x[which.max(!is.na(x))][1]
    )
    elasticity_df <- aggregate(
      consumption_elasticity ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) x[which.max(!is.na(x))][1]
    )
    cpd_change_df <- aggregate(
      cpd_pct_change ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) x[which.max(!is.na(x))][1]
    )
    cpd_after_tax_df <- aggregate(
      cpd_after_tax ~ Calendar_Year,
      data = scenario_df,
      FUN = function(x) mean(x, na.rm = TRUE)
    )
    summary_df <- Reduce(
      function(x, y) merge(x, y, by = "Calendar_Year", all = TRUE, sort = TRUE),
      list(
        total_population_df,
        total_smokers_df,
        total_packs_df,
        total_revenue_df,
        tax_rate_df,
        effective_tax_df,
        price_before_df,
        price_after_df,
        percentage_change_in_price_df,
        elasticity_df,
        cpd_change_df,
        cpd_after_tax_df
      )
    )

    names(summary_df) <- c(
      "Calendar_Year",
      "total_population",
      "total_smokers",
      "packs_smoked_model",
      "tax_revenue_model",
      "state_tax_rate_dollar",
      "tax_increase_dollar_effective",
      "price_before_tax_dollar",
      "price_after_tax_dollar",
      "percentage_change_in_price",
      "consumption_elasticity",
      "cpd_pct_change",
      "cpd_after_tax"
    )

    summary_df$prevalence <- ifelse(
      summary_df$total_population > 0,
      summary_df$total_smokers / summary_df$total_population,
      NA_real_
    )
    summary_df <- merge(
      summary_df,
      observed_df[, c(
        "Calendar_Year",
        "observed_state_cig_pack_per_capita",
        "observed_state_tax_rate_dollar",
        "observed_taxed_packs_sold",
        "observed_tax_revenue"
      )],
      by = "Calendar_Year",
      all.x = TRUE,
      sort = TRUE
    )
    summary_df$model_packs_per_capita <- ifelse(
      summary_df$total_population > 0,
      summary_df$packs_smoked_model / summary_df$total_population,
      NA_real_
    )
    summary_df$tbot_to_model_scaling_factor <- ifelse(
      !is.na(summary_df$model_packs_per_capita) &
        summary_df$model_packs_per_capita > 0,
      summary_df$observed_state_cig_pack_per_capita / summary_df$model_packs_per_capita,
      NA_real_
    )
    summary_df$tax_revenue_model_scaled <-
      summary_df$tax_revenue_model * summary_df$tbot_to_model_scaling_factor
    summary_df$scenario <- scenario_name
    summary_df$inflation_adjustment_rate <- ifelse(
      apply_inflation_adjustment,
      inflation_adjustment_rate_value,
      NA_real_
    )
    summary_df$inflation_adjustment_applied <- apply_inflation_adjustment

    summary_df[, c(
      "scenario",
      "Calendar_Year",
      "inflation_adjustment_applied",
      "inflation_adjustment_rate",
      "state_tax_rate_dollar",
      "tax_increase_dollar_effective",
      "price_before_tax_dollar",
      "price_after_tax_dollar",
      "percentage_change_in_price",
      "consumption_elasticity",
      "cpd_pct_change",
      "cpd_after_tax",
      "total_population",
      "total_smokers",
      "prevalence",
      "packs_smoked_model",
      "model_packs_per_capita",
      "observed_state_cig_pack_per_capita",
      "observed_state_tax_rate_dollar",
      "observed_taxed_packs_sold",
      "observed_tax_revenue",
      "tax_revenue_model",
      "tbot_to_model_scaling_factor",
      "tax_revenue_model_scaled"
    )]
  }

  baseline_policy_summary <- build_state_policy_summary(
    simulation_prevalence_df[simulation_prevalence_df$scenario == "baseline", , drop = FALSE],
    additional_tax_dollar = 0,
    scenario_name = "baseline",
    apply_inflation_adjustment = FALSE,
    apply_consumption_adjustment = FALSE
  )
  current_model_summary <- build_state_policy_summary(
    simulation_prevalence_df[simulation_prevalence_df$scenario == "current_model", , drop = FALSE],
    additional_tax_dollar = tax_increase_dollar_value,
    scenario_name = "current_model",
    apply_inflation_adjustment = FALSE,
    apply_consumption_adjustment = FALSE
  )
  scaled_model_summary <- build_state_policy_summary(
    simulation_prevalence_df[simulation_prevalence_df$scenario == "current_model", , drop = FALSE],
    additional_tax_dollar = tax_increase_dollar_value,
    scenario_name = "scaled_model",
    apply_inflation_adjustment = FALSE,
    apply_consumption_adjustment = TRUE
  )
  inflation_adjusted_summary <- build_state_policy_summary(
    simulation_prevalence_df[simulation_prevalence_df$scenario == "inflation_adjusted", , drop = FALSE],
    additional_tax_dollar = tax_increase_dollar_value,
    scenario_name = "inflation_adjusted",
    apply_inflation_adjustment = TRUE,
    apply_consumption_adjustment = TRUE
  )

  summary_cols_for_wide <- setdiff(
    names(baseline_policy_summary),
    c(
      "scenario",
      "observed_state_cig_pack_per_capita",
      "observed_state_tax_rate_dollar",
      "observed_taxed_packs_sold",
      "observed_tax_revenue"
    )
  )

  rename_summary_for_wide <- function(summary_df, suffix) {
    out_df <- summary_df[, summary_cols_for_wide, drop = FALSE]
    names(out_df)[names(out_df) != "Calendar_Year"] <-
      paste0(names(out_df)[names(out_df) != "Calendar_Year"], "_", suffix)
    out_df
  }

  policy_comparison_df <- Reduce(
    function(x, y) merge(x, y, by = "Calendar_Year", all = TRUE, sort = TRUE),
    list(
      rename_summary_for_wide(
        baseline_policy_summary[baseline_policy_summary$scenario == "baseline", , drop = FALSE],
        "baseline"
      ),
      rename_summary_for_wide(
        current_model_summary[current_model_summary$scenario == "current_model", , drop = FALSE],
        "current_model"
      ),
      rename_summary_for_wide(
        scaled_model_summary[scaled_model_summary$scenario == "scaled_model", , drop = FALSE],
        "scaled_model"
      ),
      rename_summary_for_wide(
        inflation_adjusted_summary[inflation_adjusted_summary$scenario == "inflation_adjusted", , drop = FALSE],
        "inflation_adjusted"
      ),
      observed_df[, c(
        "Calendar_Year",
        "observed_state_tax_rate_dollar",
        "observed_state_cig_pack_per_capita",
        "observed_taxed_packs_sold",
        "observed_tax_revenue"
      )]
    )
  )

  policy_comparison_df$current_model_vs_observed_revenue_gap <-
    policy_comparison_df$tax_revenue_model_current_model -
    policy_comparison_df$observed_tax_revenue
  policy_comparison_df$scaled_model_vs_observed_revenue_gap <-
    policy_comparison_df$tax_revenue_model_scaled_scaled_model -
    policy_comparison_df$observed_tax_revenue
  policy_comparison_df$inflation_adjusted_vs_observed_revenue_gap <-
    policy_comparison_df$tax_revenue_model_scaled_inflation_adjusted -
    policy_comparison_df$observed_tax_revenue
  policy_comparison_df$current_model_vs_observed_packs_gap <-
    policy_comparison_df$packs_smoked_model_current_model -
    policy_comparison_df$observed_taxed_packs_sold
  policy_comparison_df$scaled_model_vs_observed_packs_gap <-
    policy_comparison_df$packs_smoked_model_scaled_model -
    policy_comparison_df$observed_taxed_packs_sold
  policy_comparison_df$inflation_adjusted_vs_observed_packs_gap <-
    policy_comparison_df$packs_smoked_model_inflation_adjusted -
    policy_comparison_df$observed_taxed_packs_sold

  annual_summary_long <- rbind(
    baseline_policy_summary,
    current_model_summary,
    scaled_model_summary,
    inflation_adjusted_summary
  )

  scaled_revenue_plot_df <- rbind(
    data.frame(
      Calendar_Year = policy_comparison_df$Calendar_Year,
      Fiscal_Year_End = fiscal_year_end_date(policy_comparison_df$Calendar_Year),
      revenue = policy_comparison_df$observed_tax_revenue,
      series = "Real",
      stringsAsFactors = FALSE
    ),
    data.frame(
      Calendar_Year = policy_comparison_df$Calendar_Year,
      Fiscal_Year_End = fiscal_year_end_date(policy_comparison_df$Calendar_Year),
      revenue = policy_comparison_df$tax_revenue_model_current_model,
      series = "Model",
      stringsAsFactors = FALSE
    ),
    data.frame(
      Calendar_Year = policy_comparison_df$Calendar_Year,
      Fiscal_Year_End = fiscal_year_end_date(policy_comparison_df$Calendar_Year),
      revenue = policy_comparison_df$tax_revenue_model_scaled_scaled_model,
      series = "Scaled",
      stringsAsFactors = FALSE
    ),
    data.frame(
      Calendar_Year = policy_comparison_df$Calendar_Year,
      Fiscal_Year_End = fiscal_year_end_date(policy_comparison_df$Calendar_Year),
      revenue = policy_comparison_df$tax_revenue_model_scaled_inflation_adjusted,
      series = "Inflation-adjusted model scaled",
      stringsAsFactors = FALSE
    )
  )

  mortality_summary <- NULL
  simulation_results_path <- file.path(
    output_dir,
    paste0(tolower(state_abbr_value), "_tax_simulation_results.rds")
  )
  if (file.exists(simulation_results_path)) {
    simulation_results <- readRDS(simulation_results_path)
    mortality_summary <- rbind(
      build_mortality_summary(simulation_results$baseline, "baseline"),
      build_mortality_summary(simulation_results$current_model, "current_model"),
      build_mortality_summary(simulation_results$inflation_adjusted, "inflation_adjusted")
    )
  }

  scaled_revenue_plot <- ggplot2::ggplot(
    scaled_revenue_plot_df,
    ggplot2::aes(x = Fiscal_Year_End, y = revenue, color = series)
  ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_color_manual(
      values = c(
        "Real" = "#1f77b4",
        "Model" = "#d62728",
        "Scaled" = "#ff7f0e",
        "Inflation-adjusted model scaled" = "#2ca02c"
      )
    ) +
    ggplot2::scale_x_date(
      breaks = fiscal_year_end_date(seq(year_min, year_max, by = 1)),
      labels = fiscal_year_labels
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) format(x, scientific = FALSE, trim = TRUE, big.mark = ",")
    ) +
    ggplot2::labs(
      title = paste(state_name_value, "Revenue Comparison"),
      subtitle = "Real vs model vs model scaled vs inflation-adjusted model scaled",
      x = "Fiscal year end (June 30)",
      y = "Revenue",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA)
    )

  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(tolower(state_abbr_value), "_revenue_comparison.png")),
    plot = scaled_revenue_plot,
    width = 10,
    height = 6,
    dpi = 300,
    bg = "white"
  )

  workbook_sheets <- list(
    annual_comparison = policy_comparison_df,
    annual_summary_long = annual_summary_long,
    baseline_summary = baseline_policy_summary,
    current_model_summary = current_model_summary,
    scaled_model_summary = scaled_model_summary,
    inflation_adjusted_summary = inflation_adjusted_summary,
    scaled_revenue_plot_data = scaled_revenue_plot_df
  )
  if (!is.null(mortality_summary)) {
    workbook_sheets$mortality_summary <- mortality_summary
  }

  openxlsx::write.xlsx(
    workbook_sheets,
    file.path(output_dir, paste0(tolower(state_abbr_value), "_policy_comparison.xlsx")),
    overwrite = TRUE
  )
}
