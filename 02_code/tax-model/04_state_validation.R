# Internal historical validation of the policy model.
#
# Pennsylvania, Oklahoma, and Kentucky are used because each had a discrete,
# sizable cigarette excise-tax increase within the FY2010-FY2022 observation window.

# PA and OK provide two $1.00 increases, KY adds a smaller $0.50 increase. 

source("/Users/wangmengyao/Desktop/Github/tax-modeling/02_code/tax-model/00_config.R", local = TRUE)
source("/Users/wangmengyao/Desktop/Github/tax-modeling/02_code/tax-model/02_model_functions.R", local = TRUE)

use_packages("ggplot2")

# --- Setup --------------------------------------------------------------

val_min <- 2010L
val_max <- 2022L

state_cfg <- data.frame(
  state_fips = c("42", "40", "21"),
  state_abbr = c("PA", "OK", "KY"),
  state_name = c("Pennsylvania", "Oklahoma", "Kentucky"),
  # Tax changes enacted in 2016, 2018, and 2018 are modeled in the
  # corresponding fiscal years 2017, 2019, and 2019.
  policy_year = c(2017L, 2019L, 2019L),
  tax_increase_dollar = c(1.00, 1.00, 0.50),
  stringsAsFactors = FALSE
)

base_rev <- readRDS(file.path(model_output_dir, "baseline_revenue.rds"))

state_cfg$init_price <- base_rev$state_price_per_pack_wt_cent[match(
  paste(state_cfg$state_abbr, state_cfg$policy_year - 1L),
  paste(base_rev$state_abbr, base_rev$Calendar_Year)
)] / 100

join_keys <- c("state_fips", "state_abbr", "sex", "START_YOB", "END_YOB", "AGE", "Calendar_Year")

# --- Model simulations --------------------------------------------------

sim_state <- function(cfg, year_min = val_min, year_max = val_max) {
  fips <- cfg$state_fips[1]
  state <- cfg$state_abbr[1]
  policy_year <- as.integer(cfg$policy_year[1])
  tax <- as.numeric(cfg$tax_increase_dollar[1])
  price <- as.numeric(cfg$init_price[1])
  message("Running validation simulation: ", state)
  
  id <- matrix(1, 100, cohyears + 100, dimnames = list(NULL, as.character(startbc:endyear)))
  
  noinf <- tax_effectCalculation(
    price, tax, startbc, endyear, policy_year,
    cesdecay = 0.2, apply_inflation_adjustment = FALSE,
    inflation_adjustment_rate = inflation_adjustment_rate
  )
  
  inf <- tax_effectCalculation(
    price, tax, startbc, endyear, policy_year,
    cesdecay = 0.2, apply_inflation_adjustment = TRUE,
    inflation_adjustment_rate = inflation_adjustment_rate
  )
  
  models <- list(
    baseline = runstates(fips, id, id),
    model_noinf = runstates(fips, noinf$m.initiation.effect, noinf$m.cessation.effect),
    model_inf = runstates(fips, inf$m.initiation.effect, inf$m.cessation.effect)
  )
  
  prev <- do.call(rbind, lapply(names(models), function(name)
    prevalence_long_df(models[[name]]$l_prev_out, name, fips, state, policy_year, tax,
                       year_min, year_max, inflation_adjustment_rate)))
  
  result <- c(list(metadata = list(
    state_fips = fips, state_abbr = state, policy_year = policy_year,
    tax_increase_dollar = tax, inflation_adjustment_rate = inflation_adjustment_rate,
    init_price = price, year_min = year_min, year_max = year_max
  )), models)
  
  saveRDS(result, file.path(validation_model_dir, paste0(tolower(state), "_results.rds")), compress = "xz")
  utils::write.csv(prev, file.path(model_validation_dir, paste0(tolower(state), "_prevalence.csv")), row.names = FALSE)
}

# --- Run simulations ----------------------------------------------------

for (i in seq_len(nrow(state_cfg))) {
  sim_state(state_cfg[i, , drop = FALSE])
  gc()
}

# --- Shared helpers -----------------------------------------------------

fy <- function(year) as.Date(sprintf("%04d-06-30", as.integer(year)))
fy_lab <- function(date) paste0("FY", format(date, "%Y"))
num_lab <- function(x) format(x, scientific = FALSE, trim = TRUE, big.mark = ",")

first_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) x[1] else NA_real_
}

# --- State inputs -------------------------------------------------------

state_input <- function(cfg, prev) {
  state <- cfg$state_abbr[1]
  policy_year <- as.integer(cfg$policy_year[1])
  prev <- prev[prev$state_abbr == state, , drop = FALSE]
  
  template <- base_rev[
    base_rev$state_abbr == state,
    c(join_keys, "population", "state_tax_rate_dollar", "scale_factor"),
    drop = FALSE]
  
  tax_by_year <- aggregate(state_tax_rate_dollar ~ Calendar_Year, data = template, FUN = first_value)
  prior_year <- max(tax_by_year$Calendar_Year[tax_by_year$Calendar_Year < policy_year])
  pre_tax <- tax_by_year$state_tax_rate_dollar[tax_by_year$Calendar_Year == prior_year][1]
  
  observed <- unique(base_rev[
    base_rev$state_abbr == state,
    c("Calendar_Year", "state_tax_rate_dollar", "state_cig_pack_per_capita",
      "state_cig_sales_pack_in_million", "state_tax_revenue_in_thousand",
      "state_price_per_pack_wt_cent"), drop = FALSE])
  observed <- observed[order(observed$Calendar_Year), , drop = FALSE]
  names(observed)[2:3] <- c("observed_state_tax_rate_dollar", "observed_state_cig_pack_per_capita")
  observed$observed_taxed_packs_sold <- observed$state_cig_sales_pack_in_million * 1e6
  observed$observed_tax_revenue <- observed$state_tax_revenue_in_thousand * 1000
  observed$observed_price_per_pack_dollar <- observed$state_price_per_pack_wt_cent / 100
  observed <- observed[, c(
    "Calendar_Year", "observed_state_tax_rate_dollar", "observed_state_cig_pack_per_capita",
    "observed_taxed_packs_sold", "observed_tax_revenue", "observed_price_per_pack_dollar")]
  
  cpd <- base_rev[
    base_rev$state_abbr == state,
    c("state_fips", "sex", "AGE", "Calendar_Year", "avg_cpd"), drop = FALSE]
  cpd$key <- paste(cpd$state_fips, cpd$sex, cpd$AGE, cpd$Calendar_Year, sep = "|")
  
  list(cfg = cfg, prev = prev, template = template, observed = observed, cpd = cpd, pre_tax = pre_tax)
}

# --- Policy scenarios ---------------------------------------------------

case_spec <- function(tax) {
  data.frame(
    name = c("baseline", "tax_only", "model_noinf", "scaling", "inflation_scaling"),
    prevalence = c("baseline", "baseline", "model_noinf", "model_noinf", "model_inf"),
    tax = c(0, tax, tax, tax, tax),
    inflation = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    consumption = c(FALSE, FALSE, FALSE, TRUE, TRUE)
  )
}

policy_case <- function(input, spec) {
  cfg <- input$cfg
  policy_year <- as.integer(cfg$policy_year[1])
  base_price <- as.numeric(cfg$init_price[1])
  prev <- input$prev[input$prev$scenario == spec$prevalence, c(join_keys, "prevalence"), drop = FALSE]
  
  df <- merge(input$template, prev, by = join_keys, all.x = TRUE, sort = FALSE)
  post <- df$Calendar_Year >= policy_year
  
  df$smokers <- df$prevalence * df$population
  df$scenario <- spec$name
  df$tax_increase_dollar_effective <- 0
  
  if (spec$inflation)
    df$tax_increase_dollar_effective[post] <- spec$tax * inflation_adjustment_rate ^ (df$Calendar_Year[post] - policy_year)
  else
    df$tax_increase_dollar_effective[post] <- spec$tax
  
  df$scenario_tax_rate_dollar <- ifelse(post, input$pre_tax + df$tax_increase_dollar_effective,
                                        df$state_tax_rate_dollar)
  df$base_price <- ifelse(post, base_price, NA_real_)
  df$policy_price <- ifelse(post, base_price + df$tax_increase_dollar_effective, NA_real_)
  midpoint_price <- (df$base_price + df$policy_price) / 2
  df$percentage_change_in_price <- ifelse(
    post & midpoint_price > 0,
    df$tax_increase_dollar_effective / midpoint_price,
    0
  )
  df$consumption_elasticity <- ifelse(post & spec$tax > 0 & spec$consumption, consumption_elasticity, 0)
  df$cpd_pct_change <- df$consumption_elasticity * df$percentage_change_in_price

  match_idx <- match(paste(df$state_fips, df$sex, df$AGE, df$Calendar_Year, sep = "|"), input$cpd$key)
  df$cpd_after_tax <- input$cpd$avg_cpd[match_idx] * (1 + df$cpd_pct_change)
  revenue <- revenue_calc(df$smokers, df$cpd_after_tax, df$scenario_tax_rate_dollar)
  df$packs_smoked_model <- revenue$total_packs
  df$tax_revenue_model <- revenue$revenue

  df
}

run_cases <- function(input) {
  specs <- case_spec(as.numeric(input$cfg$tax_increase_dollar[1]))
  cases <- lapply(seq_len(nrow(specs)), function(i) policy_case(input, specs[i, , drop = FALSE]))
  names(cases) <- specs$name
  list(specs = specs, cases = cases)
}

# --- Annual summaries ---------------------------------------------------

case_summary <- function(df, observed, spec) {
  sum_cols <- c("population", "smokers", "packs_smoked_model", "tax_revenue_model")
  first_cols <- c("scenario_tax_rate_dollar", "tax_increase_dollar_effective",
                  "base_price", "policy_price", "percentage_change_in_price",
                  "consumption_elasticity", "cpd_pct_change",
                  "scale_factor")
  
  totals <- aggregate(df[sum_cols], by = list(Calendar_Year = df$Calendar_Year),
                      FUN = function(x) sum(x, na.rm = TRUE))
  yearly <- aggregate(df[first_cols], by = list(Calendar_Year = df$Calendar_Year), FUN = first_value)
  cpd <- aggregate(df["cpd_after_tax"], by = list(Calendar_Year = df$Calendar_Year),
                   FUN = function(x) mean(x, na.rm = TRUE))
  
  out <- Reduce(function(x, y) merge(x, y, by = "Calendar_Year", all = TRUE, sort = TRUE),
                list(totals, yearly, cpd, observed))
  names(out)[names(out) == "population"] <- "total_population"
  names(out)[names(out) == "smokers"] <- "total_smokers"
  names(out)[names(out) == "scenario_tax_rate_dollar"] <- "state_tax_rate_dollar"
  
  out$prevalence <- out$total_smokers / out$total_population
  out$model_packs_per_capita <- out$packs_smoked_model / out$total_population
  out$tax_revenue_model_scaled <- out$tax_revenue_model * out$scale_factor
  out$scenario <- spec$name
  out$inflation_adjustment_rate <- if (spec$inflation) inflation_adjustment_rate else NA_real_
  out$inflation_adjustment_applied <- spec$inflation
  
  out[, c(
    "scenario", "Calendar_Year", "inflation_adjustment_applied", "inflation_adjustment_rate",
    "state_tax_rate_dollar", "tax_increase_dollar_effective", "base_price",
    "policy_price", "percentage_change_in_price", "consumption_elasticity",
    "cpd_pct_change", "cpd_after_tax", "total_population", "total_smokers", "prevalence",
    "packs_smoked_model", "model_packs_per_capita", "observed_state_cig_pack_per_capita",
    "observed_state_tax_rate_dollar", "observed_taxed_packs_sold", "observed_tax_revenue",
    "tax_revenue_model", "scale_factor", "tax_revenue_model_scaled",
    "observed_price_per_pack_dollar"
  )]
}

summarize_cases <- function(run, observed) {
  summaries <- lapply(seq_along(run$cases), function(i)
    case_summary(run$cases[[i]], observed, run$specs[i, , drop = FALSE]))
  names(summaries) <- run$specs$name
  summaries
}

wide_compare <- function(summaries, observed) {
  exclude <- c("scenario", "observed_state_cig_pack_per_capita", "observed_state_tax_rate_dollar",
               "observed_taxed_packs_sold", "observed_tax_revenue", "observed_price_per_pack_dollar")
  value_cols <- setdiff(names(summaries[[1]]), exclude)
  
  wide <- lapply(names(summaries), function(name) {
    df <- summaries[[name]][, value_cols, drop = FALSE]
    rename <- names(df) != "Calendar_Year"
    names(df)[rename] <- paste0(names(df)[rename], "_", name)
    df
  })
  
  out <- Reduce(function(x, y) merge(x, y, by = "Calendar_Year", all = TRUE, sort = TRUE),
                c(wide, list(observed)))
  
  out$rev_base <- out$tax_revenue_model_scaled_baseline
  out$rev_tax <- out$tax_revenue_model_tax_only
  out$rev_model <- out$tax_revenue_model_model_noinf
  out$rev_scale <- out$tax_revenue_model_scaled_scaling
  out$rev_inf <- out$tax_revenue_model_scaled_inflation_scaling
  
  out$rev_obs_pc <- out$observed_tax_revenue / out$total_population_model_noinf
  out$rev_base_pc <- out$rev_base / out$total_population_baseline
  out$rev_tax_pc <- out$rev_tax / out$total_population_tax_only
  out$rev_model_pc <- out$rev_model / out$total_population_model_noinf
  out$rev_scale_pc <- out$rev_scale / out$total_population_scaling
  out$rev_inf_pc <- out$rev_inf / out$total_population_inflation_scaling
  
  out$packs_model <- out$packs_smoked_model_model_noinf
  out$packs_tax <- out$packs_smoked_model_tax_only
  out$packs_scale <- out$packs_smoked_model_scaling
  out$packs_inf <- out$packs_smoked_model_inflation_scaling
  out$price <- out$observed_price_per_pack_dollar
  
  out$gap_rev_model <- out$rev_model - out$observed_tax_revenue
  out$gap_rev_scale <- out$rev_scale - out$observed_tax_revenue
  out$gap_rev_inf <- out$rev_inf - out$observed_tax_revenue
  out$gap_packs_model <- out$packs_model - out$observed_taxed_packs_sold
  out$gap_packs_scale <- out$packs_scale - out$observed_taxed_packs_sold
  out$gap_packs_inf <- out$packs_inf - out$observed_taxed_packs_sold
  
  out
}

# --- Mortality and plot data -------------------------------------------

mort_df <- function(model, name) {
  df <- model$df_mort.outputs
  df <- df[df$gender == "Both" & df$year >= val_min & df$year <= val_max,
           c("year", "LYG", "SADsAverted"), drop = FALSE]
  names(df) <- c("Calendar_Year", "life_years_saved", "deaths_averted")
  df$scenario <- name
  df
}

mort_data <- function(sim) {
  do.call(rbind, lapply(c("baseline", "model_noinf", "model_inf"),
                        function(name) mort_df(sim[[name]], name)))
}

# --- Read simulation results -------------------------------------------

# Read the saved simulations and formatted prevalence explicitly. The read
# functions stop here if any result is missing or unreadable.
validation_sims <- setNames(
  lapply(state_cfg$state_abbr, function(state) {
    readRDS(file.path(
      validation_model_dir,
      paste0(tolower(state), "_results.rds")
    ))
  }),
  state_cfg$state_abbr
)
validation_prev <- setNames(
  lapply(state_cfg$state_abbr, function(state) {
    utils::read.csv(
      file.path(model_validation_dir, paste0(tolower(state), "_prevalence.csv")),
      stringsAsFactors = FALSE
    )
  }),
  state_cfg$state_abbr
)

# --- Analyze and plot ---------------------------------------------------

validation_results <- setNames(vector("list", nrow(state_cfg)), state_cfg$state_abbr)

for (i in seq_len(nrow(state_cfg))) {
  cfg <- state_cfg[i, , drop = FALSE]
  state <- cfg$state_abbr
  sim <- validation_sims[[state]]
  input <- state_input(cfg, validation_prev[[state]])
  cases <- run_cases(input)
  summaries <- summarize_cases(cases, input$observed)
  comparison <- wide_compare(summaries, input$observed)
  comparison$state_abbr <- state
  comparison$policy_year <- cfg$policy_year
  comparison$tax_increase_dollar <- cfg$tax_increase_dollar
  comparison <- comparison[, c(
    "state_abbr", "policy_year", "tax_increase_dollar",
    setdiff(names(comparison), c("state_abbr", "policy_year", "tax_increase_dollar"))
  )]

  result <- list(
    cfg = cfg,
    comparison = comparison,
    summaries = summaries,
    summary_long = do.call(rbind, summaries),
    mortality = mort_data(sim)
  )
  validation_results[[state]] <- result
}

validation_comparison <- do.call(rbind, lapply(validation_results, `[[`, "comparison"))
utils::write.csv(
  validation_comparison,
  file.path(model_validation_dir, "validation_comparison.csv"),
  row.names = FALSE
)

# --- Validation figure --------------------------------------------------

plot_df <- do.call(rbind, lapply(seq_len(nrow(state_cfg)), function(i) {
  cfg <- state_cfg[i, , drop = FALSE]
  df <- validation_results[[cfg$state_abbr]]$comparison
  data.frame(
    state = cfg$state_name,
    date = fy(df$Calendar_Year),
    observed = df$rev_obs_pc,
    baseline = df$rev_base_pc,
    policy = df$rev_inf_pc
  )
}))
plot_df$state <- factor(plot_df$state, levels = state_cfg$state_name)

events <- data.frame(
  state = factor(state_cfg$state_name, levels = state_cfg$state_name),
  date = fy(state_cfg$policy_year),
  label = paste0(
    "+$",
    formatC(state_cfg$tax_increase_dollar, format = "f", digits = 2),
    " per pack"
  )
)
events$y <- max(c(plot_df$observed, plot_df$baseline, plot_df$policy), na.rm = TRUE) * 1.05

series_colors <- c(
  "Observed" = "#1f77b4",
  "Baseline" = "#9467bd",
  "Policy" = "#d62728"
)

validation_plot <- ggplot2::ggplot() +
  ggplot2::geom_vline(data = events, ggplot2::aes(xintercept = as.numeric(date)),
                      linetype = "dashed", color = "gray45", linewidth = 0.6) +
  ggplot2::geom_line(data = plot_df, ggplot2::aes(date, baseline, color = "Baseline"), linewidth = 1) +
  ggplot2::geom_line(data = plot_df, ggplot2::aes(date, policy, color = "Policy"), linewidth = 1) +
  ggplot2::geom_point(data = plot_df, ggplot2::aes(date, observed, color = "Observed"), size = 2.2) +
  ggplot2::geom_text(data = events, ggplot2::aes(date, y, label = label),
                     hjust = -0.05, vjust = 0, size = 3.4, color = "gray25") +
  ggplot2::facet_wrap(~ state, ncol = 1, scales = "fixed") +
  ggplot2::scale_color_manual(breaks = c("Observed", "Baseline", "Policy"), values = series_colors) +
  ggplot2::scale_x_date(breaks = fy(seq(val_min, val_max, by = 2L)), labels = fy_lab) +
  ggplot2::scale_y_continuous(labels = num_lab, expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
  ggplot2::labs(title = "Historical Revenue Validation", x = "Fiscal year end (June 30)",
                y = "Cigarette tax revenue per capita", color = NULL) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(face = "bold")) +
  ggplot2::guides(color = ggplot2::guide_legend(
    override.aes = list(shape = c(16, NA, NA), linetype = c("blank", "solid", "solid"))
  ))

ggplot2::ggsave(file.path(model_validation_dir, "validation_revenue.png"), validation_plot,
                width = 10, height = 10, dpi = 300, bg = "white")
