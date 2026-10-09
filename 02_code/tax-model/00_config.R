repo_root <- "/Users/wangmengyao/Desktop/Github/tax-modeling"

input_root <- file.path(repo_root, "01_input_data")
raw_input_root <- file.path(input_root, "data-raw")
state_input_root <- file.path(input_root, "data", "state_inputs")
data_output_dir <- file.path(repo_root, "03_output")
model_output_dir <- file.path(data_output_dir, "model_objects")
validation_model_dir <- file.path(model_output_dir, "validation")
model_validation_dir <- file.path(data_output_dir, "model_validation")

dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(validation_model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_validation_dir, recursive = TRUE, showWarnings = FALSE)

startbc <- 1908L
endbc <- 2100L
endyear <- 2200L
cohyears <- endbc - startbc + 1L
totalyears <- length(startbc:endyear)
v_calyears <- seq_len(cohyears)
v_stdbirths <- rep(1000000, times = cohyears)

# SEER standard US population distribution used to split the 85+ age group
# into single-year ages 85-99 throughout the workflow.
seer_age_85_plus_weights <- c(
  0.163, 0.143, 0.126, 0.106, 0.091,
  0.077, 0.064, 0.053, 0.042, 0.034,
  0.028, 0.021, 0.015, 0.011, 0.026
)

inflation_adjustment_rate <- 0.97
consumption_elasticity <- -0.28

state_lookup <- data.frame(
  state_fips = c(
    "01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13",
    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
    "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",
    "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48",
    "49", "50", "51", "53", "54", "55", "56"
  ),
  state_abbr = c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
    "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
    "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
    "UT", "VT", "VA", "WA", "WV", "WI", "WY"
  ),
  state_name = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
    "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ),
  stringsAsFactors = FALSE
)

v_statefips <- state_lookup$state_fips

fips_abbr <- function(fips) {
  fips <- sprintf("%02d", as.integer(fips))
  abbr <- state_lookup$state_abbr[match(fips, state_lookup$state_fips)]
  if (is.na(abbr)) stop("Unknown state FIPS: ", fips, call. = FALSE)
  abbr
}

abbr_name <- function(abbr) {
  abbr <- toupper(trimws(abbr))
  name <- state_lookup$state_name[match(abbr, state_lookup$state_abbr)]
  if (is.na(name)) stop("Unknown state abbreviation: ", abbr, call. = FALSE)
  name
}

use_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing required package(s): ", paste(missing, collapse = ", "))
}

tax_model_config_loaded <- TRUE
