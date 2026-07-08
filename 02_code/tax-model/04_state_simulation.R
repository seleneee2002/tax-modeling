# State tax simulation

repo_root <- "/Users/wangmengyao/Desktop/Github/tax-modeling"
input_root <- file.path(repo_root, "01_input_data")
state_input_root <- file.path(input_root, "data", "state_inputs")
output_dir <- file.path(repo_root, "03_output", format(Sys.Date(), "%Y-%m-%d"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

price_file <- file.path(repo_root, "01_input_data", "cigarette_prices_by_state_2025.csv")
inflation_adjustment_rate <- 0.97
year_min <- 2010L
year_max <- 2022L

state_scenarios <- data.frame(
  state_fips = c("42", "40", "21"),
  state_abbr = c("PA", "OK", "KY"),
  policy_year = c(2017L, 2019L, 2019L),
  tax_increase_dollar = c(1.00, 1.00, 0.50),
  stringsAsFactors = FALSE
)

source(file.path(repo_root, "02_code", "tax-model", "02_model_functions.R"))

build_identity_effect <- function() {
  effect <- matrix(1, nrow = 100, ncol = cohyears + 100)
  colnames(effect) <- as.character(startbc:endyear)
  effect
}

get_init_price <- function(price_df, state_abbr_value) {
  price_df$state_abbr <- toupper(trimws(price_df$State))
  price_df$default_init_price <- as.numeric(price_df$default_init_price)
  init_price <- price_df$default_init_price[price_df$state_abbr == state_abbr_value][1]

  if (is.na(init_price)) {
    stop("Initial price not found for state: ", state_abbr_value)
  }

  init_price
}

run_policy_scenario <- function(state_fips, policy_effects) {
  runstates(
    state_fips,
    policy_effects$m.initiation.effect,
    policy_effects$m.cessation.effect
  )
}

build_prevalence_long_df <- function(prev_out, scenario_name, state_fips, state_abbr, policy_year, tax_increase_dollar) {
  years <- as.integer(colnames(prev_out$m_M_popAP))
  keep_years <- years >= year_min & years <= year_max
  years <- years[keep_years]

  build_sex_df <- function(smokers_mat, pop_mat, sex_name) {
    smokers_sub <- smokers_mat[, keep_years, drop = FALSE]
    pop_sub <- pop_mat[, keep_years, drop = FALSE]

    grid <- expand.grid(
      AGE = seq_len(nrow(pop_sub)) - 1L,
      Calendar_Year = years,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )

    grid$scenario <- scenario_name
    grid$policy_year <- policy_year
    grid$tax_increase_dollar <- tax_increase_dollar
    grid$inflation_adjustment_rate <- inflation_adjustment_rate
    grid$state_fips <- state_fips
    grid$state_abbr <- state_abbr
    grid$sex <- sex_name
    grid$START_YOB <- grid$Calendar_Year - grid$AGE
    grid$END_YOB <- grid$START_YOB
    grid$prevalence <- ifelse(
      as.vector(pop_sub) > 0,
      as.vector(smokers_sub) / as.vector(pop_sub),
      NA_real_
    )

    grid[, c(
      "scenario",
      "policy_year",
      "tax_increase_dollar",
      "inflation_adjustment_rate",
      "state_fips",
      "state_abbr",
      "sex",
      "START_YOB",
      "END_YOB",
      "AGE",
      "Calendar_Year",
      "prevalence"
    )]
  }

  rbind(
    build_sex_df(prev_out$m_M_smokers, prev_out$m_M_popAP, "Male"),
    build_sex_df(prev_out$m_F_smokers, prev_out$m_F_popAP, "Female")
  )
}

price_df <- read.csv(price_file, stringsAsFactors = FALSE)

for (scenario_idx in seq_len(nrow(state_scenarios))) {
  state_fips <- state_scenarios$state_fips[scenario_idx]
  state_abbr <- state_scenarios$state_abbr[scenario_idx]
  policy_year <- as.integer(state_scenarios$policy_year[scenario_idx])
  tax_increase_dollar <- as.numeric(state_scenarios$tax_increase_dollar[scenario_idx])
  init_price <- get_init_price(price_df, state_abbr)

  simulation_rds_output_path <- file.path(
    output_dir,
    paste0(tolower(state_abbr), "_tax_simulation_results.rds")
  )
  simulation_prevalence_output_path <- file.path(
    output_dir,
    paste0(tolower(state_abbr), "_tax_simulation_prevalence.csv")
  )

  baseline_effects <- list(
    m.initiation.effect = build_identity_effect(),
    m.cessation.effect = build_identity_effect()
  )

  policy_effects <- tax_effectCalculation(
    initprice = init_price,
    tax = tax_increase_dollar,
    startbc = startbc,
    endyear = endyear,
    policyYear = policy_year,
    inidecay = 0.0,
    cesdecay = 0.2,
    iniagemod = 1,
    cesagemod = 1,
    apply_inflation_adjustment = FALSE,
    inflation_adjustment_rate = inflation_adjustment_rate
  )

  inflation_adjusted_policy_effects <- tax_effectCalculation(
    initprice = init_price,
    tax = tax_increase_dollar,
    startbc = startbc,
    endyear = endyear,
    policyYear = policy_year,
    inidecay = 0.0,
    cesdecay = 0.2,
    iniagemod = 1,
    cesagemod = 1,
    apply_inflation_adjustment = TRUE,
    inflation_adjustment_rate = inflation_adjustment_rate
  )

  baseline_out <- run_policy_scenario(state_fips, baseline_effects)
  current_model_out <- run_policy_scenario(state_fips, policy_effects)
  inflation_adjusted_out <- run_policy_scenario(state_fips, inflation_adjusted_policy_effects)

  prevalence_long_df <- rbind(
    build_prevalence_long_df(
      baseline_out$l_prev_out,
      "baseline",
      state_fips,
      state_abbr,
      policy_year,
      tax_increase_dollar
    ),
    build_prevalence_long_df(
      current_model_out$l_prev_out,
      "current_model",
      state_fips,
      state_abbr,
      policy_year,
      tax_increase_dollar
    ),
    build_prevalence_long_df(
      inflation_adjusted_out$l_prev_out,
      "inflation_adjusted",
      state_fips,
      state_abbr,
      policy_year,
      tax_increase_dollar
    )
  )

  simulation_results <- list(
    metadata = list(
      state_fips = state_fips,
      state_abbr = state_abbr,
      policy_year = policy_year,
      tax_increase_dollar = tax_increase_dollar,
      inflation_adjustment_rate = inflation_adjustment_rate,
      init_price = init_price,
      year_min = year_min,
      year_max = year_max
    ),
    baseline = baseline_out,
    current_model = current_model_out,
    inflation_adjusted = inflation_adjusted_out
  )

  saveRDS(simulation_results, file = simulation_rds_output_path, compress = "xz")
  utils::write.csv(prevalence_long_df, simulation_prevalence_output_path, row.names = FALSE)
}
