# Run the baseline model and prepare baseline revenue data.

source("/Users/wangmengyao/Desktop/Github/tax-modeling/02_code/tax-model/00_config.R", local = TRUE)
source("/Users/wangmengyao/Desktop/Github/tax-modeling/02_code/tax-model/02_model_functions.R", local = TRUE)
use_packages(c("dplyr", "readxl", "openxlsx"))


# Run the baseline smoking model for all states and DC.

m.initiation.effect <- m.cessation.effect <- matrix(
  1, nrow = 100, ncol = cohyears + 100,
  dimnames = list(NULL, as.character(startbc:endyear))
)

baseline_results <- setNames(vector("list", length(v_statefips)), v_statefips)

for (i in seq_along(v_statefips)) {
  state_fips <- sprintf("%02d", as.integer(v_statefips[i]))
  message("Running baseline: ", fips_abbr(state_fips), " (", state_fips, ")")
  baseline_results[[i]] <- runstates(state_fips, m.initiation.effect, m.cessation.effect)
}

baseline_output_path <- file.path(model_output_dir, "baseline_results.rds")
saveRDS(baseline_results, baseline_output_path, compress = "xz")
message("Saved baseline model results to: ", baseline_output_path)

# --- Baseline prevalence ------------------------------------------------

dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)
year_min <- 2010L
year_max <- 2022L

results_path <- file.path(model_output_dir, "baseline_results.rds")

base_results <- readRDS(results_path)

prev_list <- vector("list", length(base_results))

for (i in seq_along(base_results)) {
  state_fips <- names(base_results)[i]
  item <- base_results[[i]]
  
  prev_list[[i]] <- prevalence_long_df(
    item$l_prev_out, "baseline", state_fips, item$df_mort.outputs$abbr[1],
    NA_integer_, NA_real_, year_min, year_max
  )[, c("state_fips", "state_abbr", "sex", "START_YOB", "END_YOB",
        "AGE", "Calendar_Year", "prevalence", "smokers", "population")]
}

prev_df <- do.call(rbind, prev_list)

# --- Population bridge --------------------------------------------------

pop_lookup <- unique(prev_df[, c("state_fips", "state_abbr")])
pop_lookup$state_fips <- sprintf("%02d", as.integer(pop_lookup$state_fips))
pop_lookup$state_abbr <- as.character(pop_lookup$state_abbr)

pop_path <- file.path(input_root, "Population2004_2030_byState.xlsx")
pop_sheets <- readxl::excel_sheets(pop_path)
pop_source <- do.call(rbind, lapply(
  pop_sheets,
  pop_long_df,
  workbook_path = pop_path,
  lookup_df = pop_lookup
))

# Keep the shared population estimates through 2030, then project 2031-2035
# using the average annual log growth rate from 2025 through 2030 for each
# state/sex/age category.
pop_df <- pop_project(
  pop_source,
  start_year = 2025L,
  base_year = 2030L,
  end_year = 2035L
)
pop_df <- pop_df[
  order(as.integer(pop_df$state_fips), pop_df$sex,
        pop_df$AGE, pop_df$Calendar_Year), , drop = FALSE]
pop_df$START_YOB <- pop_df$Calendar_Year - pop_df$AGE
pop_df$END_YOB <- pop_df$START_YOB
names(pop_df)[names(pop_df) == "population_update"] <- "population"
pop_df <- pop_df[, c("state_fips", "state_abbr", "sex", "START_YOB", "END_YOB", "AGE", "Calendar_Year", "population")]

openxlsx::write.xlsx(pop_df, file.path(data_output_dir, "population.xlsx"), overwrite = TRUE)

pop_key <- paste(pop_df$state_fips, pop_df$sex, pop_df$AGE, pop_df$Calendar_Year, sep = "|")
prev_key <- paste(sprintf("%02d", as.integer(prev_df$state_fips)), prev_df$sex,
                  prev_df$AGE, prev_df$Calendar_Year, sep = "|")
pop_match <- match(prev_key, pop_key)
pop_keep <- !is.na(pop_match)

prev_df$population[pop_keep] <- pop_df$population[pop_match[pop_keep]]
prev_df$smokers <- prev_df$prevalence * prev_df$population

utils::write.csv(prev_df, file.path(data_output_dir, "baseline_prevalence.csv"), row.names = FALSE)


# --- Baseline revenue --------------------------------------------------

tax_data <- utils::read.csv(
  file.path(input_root, "data_for_tcp_revenue.csv"),
  stringsAsFactors = FALSE
)
names(tax_data)[names(tax_data) == "state"] <- "state_abbr"
names(tax_data)[names(tax_data) == "year"] <- "Calendar_Year"
tax_data$state_abbr <- toupper(trimws(tax_data$state_abbr))
tax_data$Calendar_Year <- as.integer(tax_data$Calendar_Year)
tax_data$state_price_per_pack_wt_cent <- as.numeric(tax_data$state_price_per_pack_wt_cent)

merge_keys <- c("state_fips", "state_abbr", "sex", "START_YOB", "END_YOB", "AGE", "Calendar_Year")
base_df <- merge(
  prev_df[, c(merge_keys, "prevalence", "smokers")],
  pop_df,
  by = merge_keys,
  all.x = TRUE,
  sort = FALSE
)
base_df$state_fips <- sprintf("%02d", as.integer(base_df$state_fips))
base_lookup <- unique(base_df[, c("state_fips", "state_abbr")])
base_lookup$state_abbr <- as.character(base_lookup$state_abbr)

# CPD inputs.
cpd_df <- cpd_long_df(
  utils::read.csv(file.path(input_root, "SHG_08252026.csv"), stringsAsFactors = FALSE),
  base_lookup,
  year_min,
  year_max
)

base_df$row_id <- seq_len(nrow(base_df))
base_rev <- merge(base_df, cpd_df, by = merge_keys, all.x = TRUE, sort = FALSE)
base_rev <- base_rev[order(base_rev$row_id), ]
base_rev <- merge(
  base_rev,
  tax_data,
  by = c("state_abbr", "Calendar_Year"),
  all.x = TRUE,
  sort = FALSE
)
base_rev <- base_rev[order(base_rev$row_id), ]
rev_cols <- c(
  "state_fips", "state_abbr", "sex", "START_YOB", "END_YOB", "AGE",
  "Calendar_Year", "prevalence", "smokers", "population", paste0("CAT", 1:6),
  setdiff(names(tax_data), c("state_abbr", "Calendar_Year"))
)
base_rev <- base_rev[, rev_cols]

base_rev$state_tax_rate_dollar <- base_rev$state_tax_rate_cent / 100
cpd_weights <- c(2.5, 10, 20, 30, 40, 50)
base_rev$avg_cpd <- as.numeric(
  as.matrix(base_rev[, paste0("CAT", 1:6)]) %*% cpd_weights
)
revenue <- revenue_calc(
  base_rev$smokers,
  base_rev$avg_cpd,
  base_rev$state_tax_rate_dollar
)
base_rev$packs_per_smoker <- revenue$packs_per_smoker
base_rev$packs_model <- revenue$total_packs
base_rev$rev_model <- revenue$revenue

total_packs <- ave(
  base_rev$packs_model,
  base_rev$state_abbr,
  base_rev$Calendar_Year,
  FUN = function(x) sum(x, na.rm = TRUE)
)

base_rev$rev_obs <- ifelse(total_packs > 0,
  (base_rev$state_tax_revenue_in_thousand * 1000) * base_rev$packs_model / total_packs,
  NA_real_
)

# Scale modeled consumption to observed annual sales.
scales <- scale_factors(base_rev)
base_rev <- dplyr::left_join(
  base_rev,
  dplyr::select(scales, state_abbr, Calendar_Year, scale_factor),
  by = c("state_abbr", "Calendar_Year")
)
base_rev$rev_scaled <- base_rev$rev_model * base_rev$scale_factor

openxlsx::write.xlsx(base_rev,file.path(data_output_dir, "baseline_revenue.xlsx"),overwrite = TRUE)
openxlsx::write.xlsx(scales, file.path(data_output_dir, "scaling_factors.xlsx"), overwrite = TRUE)
saveRDS(base_rev, file.path(model_output_dir, "baseline_revenue.rds"), compress = "xz")
