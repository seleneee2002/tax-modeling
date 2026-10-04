# Project state revenue and health outcomes under future tax increases.

source("/Users/wangmengyao/Desktop/Github/tax-modeling/02_code/tax-model/00_config.R", local = TRUE)
source(file.path(repo_root, "02_code", "tax-model", "02_model_functions.R"), local = TRUE)

use_packages("openxlsx")

price_file <- file.path(input_root, "data_for_tcp_revenue.csv")
population_path <- file.path(data_output_dir, "population.xlsx")
projection_output_path <- file.path(model_output_dir, "all_projection.rds")
price_year <- 2025L
year_min <- 2010L
year_max <- 2035L
policy_year_default <- 2028L
tax_increases <- c(1.00, 2.00)

# --- Inputs -------------------------------------------------------------

price_raw <- utils::read.csv(price_file, stringsAsFactors = FALSE)
price_df <- data.frame(
  state_abbr = toupper(trimws(price_raw$state)),
  Calendar_Year = as.integer(price_raw$year),
  state_tax_rate_cent = as.numeric(price_raw$state_tax_rate_cent),
  state_price_per_pack_wt_cent = as.numeric(price_raw$state_price_per_pack_wt_cent),
  stringsAsFactors = FALSE
)

base_rev <- readRDS(file.path(model_output_dir, "baseline_revenue.rds"))
base_rev$state_abbr <- toupper(trimws(base_rev$state_abbr))
scales <- unique(base_rev[, c("state_abbr", "Calendar_Year", "scale_factor")])

pop_updates <- openxlsx::read.xlsx(population_path)
pop_updates$state_fips <- sprintf("%02d", as.integer(pop_updates$state_fips))
pop_updates$AGE <- as.integer(pop_updates$AGE)
pop_updates$Calendar_Year <- as.integer(pop_updates$Calendar_Year)
pop_updates$population_update <- as.numeric(pop_updates$population)
pop_updates <- pop_updates[
  pop_updates$Calendar_Year >= year_min &
    pop_updates$Calendar_Year <= year_max, ,
  drop = FALSE
]

states <- state_lookup[, c("state_abbr", "state_fips")]

# --- Projection helpers ------------------------------------------------

# Replace model population with projection population.
update_pop <- function(prev_df, pop_updates) {
  keys <- c("state_fips", "sex", "AGE", "Calendar_Year")
  prev_df <- merge(
    prev_df, pop_updates[, c(keys, "population_update")],
    by = keys, all.x = TRUE, sort = FALSE
  )
  
  prev_df$population <- prev_df$population_update
  prev_df$smokers <- prev_df$prevalence * prev_df$population
  prev_df$population_update <- NULL
  prev_df
}

# Calculate annual revenue for one projection scenario.
proj_summary <- function(prev_df, rev_input, scenario, tax, inflation) {
  keys <- c("sex", "AGE", "Calendar_Year")
  df <- merge(prev_df[, c(keys, "population", "smokers")], rev_input,
              by = keys, all.x = TRUE, sort = FALSE)
  
  post <- df$Calendar_Year >= policy_year_default
  tax_effect <- tax * (scenario != "baseline")
  inflation_factor <- if (inflation)
    inflation_adjustment_rate^(df$Calendar_Year[post] - policy_year_default) else 1
  
  df$tax_increase_dollar_effective <- 0
  df$tax_increase_dollar_effective[post] <- tax_effect * inflation_factor
  df$state_tax_rate_dollar_proj <- df$baseline_tax_rate_dollar + df$tax_increase_dollar_effective
  df$policy_price <- df$base_price + df$tax_increase_dollar_effective
  
  midpoint_price <- (df$base_price + df$policy_price) / 2
  price_change <- df$tax_increase_dollar_effective / midpoint_price
  df$avg_cpd_proj <- df$avg_cpd * (1 + consumption_elasticity * price_change)
  
  revenue <- revenue_calc(df$smokers, df$avg_cpd_proj, df$state_tax_rate_dollar_proj)
  df$cigarette_packs_smoked_total <- revenue$total_packs
  df$state_revenue_model <- revenue$revenue
  
  annual <- aggregate(
    cbind(population, smokers, cigarette_packs_smoked_total, state_revenue_model) ~ Calendar_Year,
    data = df, FUN = function(x) sum(x, na.rm = TRUE)
  )
  
  annual_policy <- unique(df[, c(
    "Calendar_Year", "state_tax_rate_dollar_proj", "base_price", "policy_price",
    "tax_increase_dollar_effective", "latest_scaling_factor"
  )])
  annual <- merge(annual, annual_policy, by = "Calendar_Year", sort = TRUE)
  
  annual$scenario <- scenario
  annual$policy_year <- policy_year_default
  annual$tax_increase_dollar <- tax
  annual$prevalence_all_ages <- annual$smokers / annual$population
  annual$state_revenue_model_scaled <- annual$state_revenue_model * annual$latest_scaling_factor
  
  annual[, c(
    "scenario", "Calendar_Year", "policy_year", "tax_increase_dollar",
    "population", "smokers", "prevalence_all_ages", "cigarette_packs_smoked_total",
    "state_tax_rate_dollar_proj", "base_price", "policy_price",
    "tax_increase_dollar_effective", "state_revenue_model",
    "latest_scaling_factor", "state_revenue_model_scaled"
  )]
}

# Format annual mortality outcomes for one scenario.
mort_summary <- function(mort_df, scenario_name) {
  out <- mort_df[
    mort_df$gender == "Both" & mort_df$year >= year_min & mort_df$year <= year_max,
    c("year", "LYG", "LYGcum", "SADsAverted", "SADsAvertedcum"),
    drop = FALSE
  ]

  names(out)[names(out) == "year"] <- "Calendar_Year"
  out$scenario <- scenario_name

  out[, c("scenario", "Calendar_Year", "LYG", "LYGcum", "SADsAverted", "SADsAvertedcum")]
}

# Run all projection scenarios for one state and tax increase.
project_state <- function(state_abbr, state_fips, tax) {
  init_price <- price_df$state_price_per_pack_wt_cent[
    price_df$state_abbr == state_abbr & price_df$Calendar_Year == price_year][1] / 100
  
  state_base <- base_rev[base_rev$state_abbr == state_abbr, , drop = FALSE]
  state_price_tax <- price_df[
    price_df$state_abbr == state_abbr &
      price_df$Calendar_Year >= year_min & price_df$Calendar_Year <= price_year, , drop = FALSE]
  
  state_scales <- scales[scales$state_abbr == state_abbr & !is.na(scales$scale_factor), ]
  latest_scaling_year <- max(state_scales$Calendar_Year, na.rm = TRUE)
  latest_scaling_factor <- state_scales$scale_factor[state_scales$Calendar_Year == latest_scaling_year][1]
  
  latest_cpd_year <- max(state_base$Calendar_Year[!is.na(state_base$avg_cpd)], na.rm = TRUE)
  latest_cpd <- state_base[state_base$Calendar_Year == latest_cpd_year,
                           c("sex", "AGE", "avg_cpd"), drop = FALSE]
  latest_cpd <- latest_cpd[!duplicated(latest_cpd[c("sex", "AGE")]), ]
  
  message(sprintf("Running projection for %s (%s): +$%.2f in %d through %d",
                  state_abbr, state_fips, tax, policy_year_default, year_max))
  
  noinf_effects <- tax_effectCalculation(
    init_price, tax, startbc, endyear, policy_year_default,
    cesdecay = 0.2, apply_inflation_adjustment = FALSE,
    inflation_adjustment_rate = inflation_adjustment_rate
  )
  inf_effects <- tax_effectCalculation(
    init_price, tax, startbc, endyear, policy_year_default,
    cesdecay = 0.2, apply_inflation_adjustment = TRUE,
    inflation_adjustment_rate = inflation_adjustment_rate
  )
  
  models <- list(
    model_noinf = runstates(state_fips, noinf_effects$m.initiation.effect, noinf_effects$m.cessation.effect),
    model_inf = runstates(state_fips, inf_effects$m.initiation.effect, inf_effects$m.cessation.effect)
  )
  
  # Extract the no-policy baseline embedded in the index = 1 simulation.
  baseline_prev <- list(
    m_M_smokers = models$model_noinf$l_prev_out$male_base_prev_obj$m_smokersAP,
    m_F_smokers = models$model_noinf$l_prev_out$female_base_prev_obj$m_smokersAP,
    m_M_popAP = models$model_noinf$l_prev_out$male_base_prev_obj$m_popAP,
    m_F_popAP = models$model_noinf$l_prev_out$female_base_prev_obj$m_popAP
  )
  
  prev <- list(
    baseline = prevalence_long_df(baseline_prev, "baseline", state_fips, state_abbr,
                                  policy_year_default, tax, year_min, year_max),
    model_noinf = prevalence_long_df(models$model_noinf$l_prev_out, "model_noinf", state_fips, state_abbr,
                                     policy_year_default, tax, year_min, year_max),
    model_inf = prevalence_long_df(models$model_inf$l_prev_out, "model_inf", state_fips, state_abbr,
                                   policy_year_default, tax, year_min, year_max)
  )
  
  state_pop <- pop_updates[pop_updates$state_fips == state_fips, ]
  prev <- lapply(prev, update_pop, pop_updates = state_pop)
  
  # Prepare revenue inputs shared across scenarios.
  keys <- c("sex", "AGE", "Calendar_Year")
  rev_input <- merge(
    unique(prev$baseline[, keys]),
    state_base[, c(keys, "avg_cpd")],
    by = keys, all.x = TRUE, sort = FALSE
  )
  
  names(latest_cpd)[3] <- "latest_cpd"
  rev_input <- merge(rev_input, latest_cpd, by = c("sex", "AGE"), all.x = TRUE, sort = FALSE)
  rev_input$avg_cpd <- ifelse(is.na(rev_input$avg_cpd), rev_input$latest_cpd, rev_input$avg_cpd)
  rev_input$avg_cpd[is.na(rev_input$avg_cpd)] <- 0
  rev_input$latest_cpd <- NULL
  
  price_match <- match(rev_input$Calendar_Year, state_price_tax$Calendar_Year)
  rev_input$base_price <- state_price_tax$state_price_per_pack_wt_cent[price_match] / 100
  rev_input$base_price[is.na(rev_input$base_price)] <- init_price
  
  rev_input$baseline_tax_rate_dollar <- state_price_tax$state_tax_rate_cent[price_match] / 100
  reference_tax <- state_price_tax$state_tax_rate_cent[state_price_tax$Calendar_Year == price_year][1] / 100
  rev_input$baseline_tax_rate_dollar[is.na(rev_input$baseline_tax_rate_dollar)] <- reference_tax
  rev_input$latest_scaling_factor <- latest_scaling_factor
  
  annual_summary <- do.call(rbind, lapply(names(prev), function(scenario)
    proj_summary(prev[[scenario]], rev_input, scenario, tax,
                 inflation = scenario == "model_inf")))
  
  baseline_mort <- mort_summary(models$model_noinf$df_mort.outputs, "baseline")
  baseline_mort[c("LYG", "LYGcum", "SADsAverted", "SADsAvertedcum")] <- 0
  
  mortality <- rbind(
    baseline_mort,
    mort_summary(models$model_noinf$df_mort.outputs, "model_noinf"),
    mort_summary(models$model_inf$df_mort.outputs, "model_inf")
  )
  
  list(annual_summary = annual_summary, mort_summary = mortality)
}
# --- Projection simulations --------------------------------------------

annual_list <- list()
mort_list <- list()
annual_idx <- 1L
mort_idx <- 1L

for (i in seq_len(nrow(states))) {
  state <- states$state_abbr[i]
  fips <- states$state_fips[i]
  
  for (j in seq_along(tax_increases)) {
    tax <- tax_increases[j]
    result <- project_state(state, fips, tax)
    
    annual <- result$annual_summary
    annual$state_fips <- fips
    annual$state_abbr <- state
    
    mort <- result$mort_summary
    mort$state_fips <- fips
    mort$state_abbr <- state
    mort$policy_year <- policy_year_default
    mort$tax_increase_dollar <- tax
    
    if (j == 1L) {
      baseline_annual <- annual[annual$scenario == "baseline", , drop = FALSE]
      baseline_mort <- mort[mort$scenario == "baseline", , drop = FALSE]
      baseline_annual$tax_increase_dollar <- baseline_mort$tax_increase_dollar <- 0
      
      annual_list[[annual_idx]] <- baseline_annual
      mort_list[[mort_idx]] <- baseline_mort
      annual_idx <- annual_idx + 1L
      mort_idx <- mort_idx + 1L
    }
    
    annual_list[[annual_idx]] <- annual[annual$scenario != "baseline", , drop = FALSE]
    mort_list[[mort_idx]] <- mort[mort$scenario != "baseline", , drop = FALSE]
    annual_idx <- annual_idx + 1L
    mort_idx <- mort_idx + 1L
  }
}

all_projection <- list(
  metadata = list(
    policy_year = policy_year_default,
    tax_increases = tax_increases,
    consumption_elasticity = consumption_elasticity,
    inflation_adjustment_rate = inflation_adjustment_rate,
    policy_baseline_price_year = price_year,
    policy_baseline_price_source = "FY2025 state_price_per_pack_wt_cent from data_for_tcp_revenue.csv",
    policy_baseline_tax_year = price_year,
    policy_baseline_tax_source = "FY2025 state_tax_rate_cent from data_for_tcp_revenue.csv",
    observed_price_tax_years = year_min:price_year,
    future_price_tax_assumption = paste(
      "Carry forward FY2025 observed price and state excise tax from FY2026 onward,",
      "then add the scenario-specific tax increase beginning in the policy year."
    ),
    cpd_future_assumption = "Carry forward the FY2022 age-sex CPD profile for future projection years.",
    price_tax_path = normalizePath(price_file, winslash = "/", mustWork = TRUE),
    baseline_revenue_path = normalizePath(file.path(model_output_dir, "baseline_revenue.rds"),
                                          winslash = "/", mustWork = TRUE),
    population_path = normalizePath(population_path, winslash = "/", mustWork = TRUE),
    target_state_fips = states$state_fips,
    projection_years = year_min:year_max,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  ),
  annual_summary = do.call(rbind, annual_list),
  mort_summary = do.call(rbind, mort_list)
)

saveRDS(all_projection, projection_output_path, compress = "xz")
message("Projection simulations saved to: ", projection_output_path)
