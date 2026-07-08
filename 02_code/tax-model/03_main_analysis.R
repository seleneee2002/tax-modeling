# Main Analysis

# Set policy flags (0 for baseline, 1 for policy analysis)
tax_policy <- 1
policyyear <- 2016
# SAVE
out_dir <- file.path("03_output/analysis_output", format(Sys.Date(), "%Y%m%d"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------ Baseline Analysis -----------------------------------
df_mortality.out    <- data.frame()
df_prev.by.state    <- data.frame()
l_combined_state_prev <- list()

baseline_initiation_effect <- matrix(1, nrow = 100, ncol = cohyears+100)
baseline_cessation_effect  <- matrix(1, nrow = 100, ncol = cohyears+100)

colnames(baseline_initiation_effect) <- colnames(baseline_cessation_effect) <- as.character(startbc:endyear)

baseline_results <- list()  # to store baseline outputs for each state

for (s in v_statefips) {
  print(paste0(fips_to_abbr(s), " (", s, ") - Baseline"))
  # Run the baseline scenario using runstates() (which uses baseline AP->AC conversion)
  baseline_out <- runstates(s, baseline_initiation_effect, baseline_cessation_effect) 
  # Store outputs
  baseline_results[[s]] <- baseline_out
  df_mortality.out <- rbind(df_mortality.out, baseline_out$df_mort.outputs)
  df_prev.by.state <- rbind(df_prev.by.state, baseline_out$df_CSprevs.by.state)
}

file_rds <- file.path(out_dir, "baseline_results.rds")
saveRDS(baseline_results, file = file_rds, compress = "xz")  


# ------------------------ Tobacco Tax Analysis --------------------------------
tax_out_list <- list()
df_mortality.out <- data.frame()
df_prev.by.state <- data.frame()
l_combined_state_prev <- list()

df_prices <- read.csv("01_input_data/cigarette_prices_by_state_2025.csv", stringsAsFactors = FALSE) %>%
  mutate(State = toupper(trimws(State))) %>%
  mutate(fips_code = sprintf("%02d", sapply(State, function(x) fips(x, to = "FIPS"))))

#-------------------------------------------------------------------------------
# each state
#-------------------------------------------------------------------------------

v_policy_years <- c(2026:2030, 2035, 2040)
v_tax_hikes <- c(0, 1.00, 1.50, 2.00, 2.50, 3.00)
v_ppp_targets <- seq(8.00, 15.00, by = 0.50)

if (tax_policy == 1) {
  df_scenarios <- expand.grid(ppp_target = v_ppp_targets, tax_hike = v_tax_hikes)
  
  # RDS Directory
  tax_rds_dir <- file.path(out_dir, "tax_scenario_rds")
  dir.create(tax_rds_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Main Loop
  for (s in c("08")) {
    state_results_list <- list() 
    
    # Get State Price
    initprice_s <- df_prices[df_prices$fips_code == s, "default_init_price"]
    initprice_s <- as.numeric(initprice_s[1])
    if (is.na(initprice_s)) next
    
    abbr <- fips_to_abbr(s)
    message(paste0("Running State: ", abbr, " | Base Price: $", initprice_s))

    # LEVEL 1: Loop Scenarios---
    for (i in 1:nrow(df_scenarios)) {
      row <- df_scenarios[i, ]
      target_ppp <- row$ppp_target
      added_tax  <- row$tax_hike
      
      # Prepare Naming Tag (e.g., "4.00_t0.00")
      scenario_tag <- sprintf("%0.2f_t%0.2f", target_ppp, added_tax)
      
      # Loop Years ---
      for (py in v_policy_years) {
        # Calculation Logic
        gap_to_floor <- max(0, target_ppp - initprice_s)
        total_tax_to_apply <- gap_to_floor + added_tax
        
        tax_effects <- tax_effectCalculation(
          initprice = initprice_s, tax = total_tax_to_apply,
          startbc = startbc, endyear = endyear, policyYear = py,
          inidecay = 0.0, cesdecay = 0.2, iniagemod = 1, cesagemod = 1
        )
        
        tax_out <- runstates(s, tax_effects$m.initiation.effect, tax_effects$m.cessation.effect)

        message(paste("Scenario", scenario_tag, "| Year:", py))
        
        run_key <- paste0("y", py, "_p", target_ppp, "_t", added_tax)
        state_results_list[[run_key]] <- list(
          policy_year = py, ppp = target_ppp, tax_add = added_tax,
          outputs = tax_out$df_mort.outputs,
          prevs = tax_out$df_CSprevs.by.state,
          prev_out = tax_out$l_prev_out
        )
        
        rm(tax_out, tax_effects)
      } 
      
      gc()
    }
    
    # Save RDS Once Per State
    saveRDS(list(state = s, init_price = initprice_s, results = state_results_list),
            file = file.path(tax_rds_dir, paste0(s, "_all_scenarios.rds")),
            compress = "gzip")
  } 
}
