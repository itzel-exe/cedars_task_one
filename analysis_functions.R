# ==============================================================================
# functions.R
# Helper functions for the hyperkalemia analysis (sourced by the main script)
# ==============================================================================

HYPERKALEMIA_CUTOFF <- 5.5   # potassium (k) at or above this counts as hyperkalemia

DIVIDER <- strrep("-", 60)


# ------------------------------------------------------------------------------
# check_duplicates
# Reports whether a column contains duplicate values.
# Prints a sentence and invisibly returns TRUE/FALSE.
# ------------------------------------------------------------------------------
check_duplicates <- function(dataset, column_name) {
  
  if (!column_name %in% names(dataset)) {
    cat("Column", column_name, "not found in dataset.\n")
    return(invisible(NULL))
  }
  
  has_dups <- any(duplicated(dataset[[column_name]]))
  cat("It is", has_dups, "that", column_name, "has duplicate values.\n")
  invisible(has_dups)
}


# ------------------------------------------------------------------------------
# k_flag
# Adds a 0/1 marker column: 1 if potassium >= cutoff, otherwise 0
# (missing or blank potassium values are treated as 0).
# Returns the updated data frame.
# ------------------------------------------------------------------------------
k_flag <- function(df, value_col = "k", marker_col = "adverse_result",
                   cutoff = HYPERKALEMIA_CUTOFF) {
  
  # as.numeric guards against k being read in as text; blanks become NA
  k_values <- suppressWarnings(as.numeric(df[[value_col]]))
  
  df[[marker_col]] <- as.integer(!is.na(k_values) & k_values >= cutoff)
  df
}


# ------------------------------------------------------------------------------
# unique_df
# Collapses a many-records-per-person data frame to one row per person.
# Because the marker is 0/1, max() gives 1 if ANY record for that person is 1.
# ------------------------------------------------------------------------------
unique_df <- function(df, id_col = "master_id", marker_col = "lab_result") {
  aggregate(as.formula(paste(marker_col, "~", id_col)),
            data = df, FUN = max)
}


# ------------------------------------------------------------------------------
# check_random_individual
# QC: picks one random person, shows all of their original records, and shows
# the binary result assigned to them in the collapsed data.
# ------------------------------------------------------------------------------
check_random_individual <- function(unique_data, original_data, result_col,
                                    dataset_name = "dataset", value_col = "k") {
  
  random_id <- sample(unique_data$master_id, size = 1)
  
  matching_records <- original_data[original_data$master_id == random_id,
                                    c("master_id", value_col, result_col)]
  
  result_value <- unique_data[[result_col]][unique_data$master_id == random_id]
  
  cat(DIVIDER, "\n")
  cat("Record(s) for individual ", random_id, " found in the ", dataset_name, ":\n", sep = "")
  cat(DIVIDER, "\n")
  print(matching_records)
  cat(DIVIDER, "\n")
  
  status <- if (result_value == 1) "has experienced hyperkalemia"
  else                   "has not experienced hyperkalemia"
  
  cat("The binary result for individual ", random_id, " is ", result_value,
      " - they ", sub("^has", "have", status), ".\n", sep = "")
  
  invisible(random_id)
}


# ------------------------------------------------------------------------------
# check_random_result
# QC: picks one random row and verifies that
#   result == 1 when either col1 or col2 is 1, otherwise 0.
# ------------------------------------------------------------------------------
check_random_result <- function(df, id_col, col1, col2, result_col) {
  
  random_row <- df[sample(nrow(df), 1), ]
  
  val1   <- random_row[[col1]]
  val2   <- random_row[[col2]]
  actual <- random_row[[result_col]]
  
  expected <- as.integer(val1 == 1 | val2 == 1)
  
  cat("Checked", id_col, "=", random_row[[id_col]], "\n")
  cat(col1, "=", val1, "|", col2, "=", val2, "|", result_col, "=", actual, "\n")
  cat(if (expected == actual) "This result is right\n" else "This result is wrong\n")
  
  invisible(expected == actual)
}