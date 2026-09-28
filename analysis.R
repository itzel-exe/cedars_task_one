# ==============================================================================
# Hyperkalemia analysis: drug vs. placebo
# Walks the user through data prep and modeling, pausing at each step.
# ==============================================================================

library(readxl)
source("analysis_functions.R")   # check_duplicates, k_flag, unique_df,
# check_random_individual, check_random_result

# ------------------------------------------------------------------------------
# Settings and helpers
# ------------------------------------------------------------------------------
data_dir <- "C:/Users/Itzel/Desktop/assignment_one"
in_path  <- function(file) file.path(data_dir, file)

LINE  <- strrep("=", 60)
DASH  <- strrep("-", 60)

# Print a message (pieces are pasted together with no separator), then wait
say <- function(..., wait = 1) {
  cat(..., "\n", sep = "")
  Sys.sleep(wait)
}

# Wait for the user before moving on
pause <- function(msg = "Press [Enter] to continue ...") {
  readline(prompt = msg)
  invisible(NULL)
}

# Section header
section <- function(title) {
  cat("\n", LINE, "\n", title, "\n", LINE, "\n", sep = "")
  Sys.sleep(1)
}


# ==============================================================================
# 1. MASTER VARIABLE DATASET
# ==============================================================================
section("1. MASTER VARIABLE DATASET")

say("Importing master variable dataset (xlsx).")
master_variables_tribble <- read_excel(in_path("master_data_variables.xlsx"))

say("Displaying master variable dataset.")
View(master_variables_tribble)
pause()

say("The master variable dataset has ", ncol(master_variables_tribble), " fields.")
print(data.frame(
  col_no   = seq_len(ncol(master_variables_tribble)),
  variable = names(master_variables_tribble),
  type     = sapply(master_variables_tribble, function(x) class(x)[1])
))
print(master_variables_tribble)
pause()

# Variables that have notes attached
say("Checking for notes attached to variable names.")
has_note <- !is.na(master_variables_tribble$notes) & master_variables_tribble$notes != ""
print(master_variables_tribble[has_note, ])
pause()

# Potassium-related variables
say("There are multiple values that can represent potassium levels.")
say("Keyword search on 'pot', 'k', 'potassium', 'hyperkalemia' in the variable names:")
# NOTE: the bare "k" pattern matches any variable name containing the letter k,
# so results should be reviewed by eye.
potassium_rows <- master_variables_tribble[
  grepl("pot|k|hyperkalemia|potassium",
        master_variables_tribble$`variable name`, ignore.case = TRUE), ]
View(potassium_rows)
pause()

say("There appear to be multiple potassium values from different visits,")
say("as shown by the note: ", potassium_rows$notes[1], ".")
say("This cannot be confirmed, and the prompt states the analysis should use the")
say("potassium values 'k' from the Adverse and Lab datasets.")
say("Potassium values not explicitly labeled 'k' are therefore excluded.")
pause()


# ==============================================================================
# 2. MASTER DATA DATASET
# ==============================================================================
section("2. MASTER DATA DATASET")

say("Importing master data (csv).")
master_raw <- read.csv(in_path("master_data.csv"))

say("Displaying master data.")
View(master_raw)
pause("Press [Enter] to continue observation...")

say("There are ", nrow(master_raw), " individuals in this dataset.")
say("Duplicate individuals (master_id): ", any(duplicated(master_raw$master_id)), ".")
say("Duplicate variable names: ", any(duplicated(names(master_raw))), ".")
pause("Press [Enter] to continue observation...")

# Fields of interest
fields <- c("master_id", "treat", "region")
say("Fields of interest in the master data:")
for (f in fields) {
  say(f, " is of type ", class(master_raw[[f]])[1], ".")
}

master_data_df <- master_raw[, fields]
say("Displaying the fields of interest.")
View(master_data_df)
pause()

# Unique values of treat and region
for (f in c("treat", "region")) {
  cat(DASH, "\n")
  vals <- unique(master_data_df[[f]])
  say("The field ", f, " has ", length(vals), " unique value(s): ",
      paste(vals, collapse = ", "), ".")
  pause()
}

# Recode region to 0/1
cat(DASH, "\n")
say("For a binary analysis, region values will be recoded to 0/1.")
say("Converting region values of 2 to 0 and leaving 1 as is...")
master_data_df$region[master_data_df$region == 2] <- 0

say("The field region now has the unique value(s): ",
    paste(unique(master_data_df$region), collapse = ", "), ".")
say("Displaying the updated region values.")
View(master_data_df)
pause()


# ==============================================================================
# 3. ADVERSE DATASET
# ==============================================================================
section("3. ADVERSE DATASET")

# latin1 encoding + no automatic string conversion avoids character corruption
say("Importing Adverse dataset (csv).")
adverse_raw <- read.csv(in_path("t05xc.csv"),
                        fileEncoding = "latin1", stringsAsFactors = FALSE)

say("Displaying Adverse dataset.")
View(adverse_raw)
pause("Press [Enter] to continue observation")

say("There are ", nrow(adverse_raw), " records in the Adverse dataset.")
check_duplicates(adverse_raw, "master_id")
cat(DASH, "\n")
str(adverse_raw)
pause()

say("Keeping only individuals and their potassium values.")
adverse_df <- adverse_raw[, c("master_id", "k")]
View(adverse_df)
pause()


# ==============================================================================
# 4. LAB DATASET
# ==============================================================================
section("4. LAB DATASET")

say("Importing Lab dataset (csv).")
lab_raw <- read.csv(in_path("t016.csv"))

say("Displaying Lab dataset.")
View(lab_raw)
say("There are ", nrow(lab_raw), " records in the Lab dataset.")
check_duplicates(lab_raw, "master_id")
cat(DASH, "\n")
say("Structure of the Lab dataset:")
str(lab_raw)

say("Keeping only the fields of interest.")
lab_df <- lab_raw[, c("master_id", "k")]
View(lab_df)
pause()

say("Both the Adverse and Lab datasets have multiple records per individual.")


# ==============================================================================
# 5. BINARY HYPERKALEMIA FLAGS
# ==============================================================================
section("5. BINARY HYPERKALEMIA FLAGS")

say("Region was converted to a binary marker earlier.")
say("Next, a hyperkalemia flag is set on every Lab and Adverse record,")
say("then collapsed to one row per individual.")

# --- Lab ---
say("Flagging Lab records.")
lab_df <- k_flag(lab_df, value_col = "k", marker_col = "lab_result")
View(lab_df)
pause()

say("Collapsing Lab data to one row per individual (1 = ever experienced hyperkalemia).")
uniquelab_df <- unique_df(lab_df, id_col = "master_id", marker_col = "lab_result")
say("Displaying the collapsed Lab data.")
View(uniquelab_df)
pause()

# --- Adverse ---
cat(DASH, "\n")
say("Repeating the same process for the Adverse dataset.")
adverse_df <- k_flag(adverse_df, value_col = "k", marker_col = "adverse_result")
say("Binary markers added. Displaying the flagged Adverse data.")
View(adverse_df)
pause()

say("Collapsing Adverse data to one row per individual.")
uniqueadverse_df <- unique_df(adverse_df, id_col = "master_id", marker_col = "adverse_result")
say("Displaying the collapsed Adverse data.")
View(uniqueadverse_df)
pause()

# --- Duplicate check on the collapsed data ---
cat(DASH, "\n")
say("Checking the collapsed datasets for duplicate individuals.")
say("Lab: ")
check_duplicates(uniquelab_df, "master_id")
say("Adverse: ")
check_duplicates(uniqueadverse_df, "master_id")
pause()


# ==============================================================================
# 6. QUALITY CHECKS ON THE BINARY MARKERS
# ==============================================================================
section("6. QUALITY CHECKS")

say("Three random individuals from each dataset will be checked.")
say("Expected: 0 = never experienced hyperkalemia, 1 = experienced.")
pause()

say("Lab dataset checks")
for (i in 1:3) {
  say("Check ", i)
  check_random_individual(uniquelab_df, lab_df, "lab_result", "Lab dataset")
  pause()
}
say("Lab sample check is complete.")

say("Adverse dataset checks")
for (i in 1:3) {
  say("Check ", i)
  check_random_individual(uniqueadverse_df, adverse_df, "adverse_result", "Adverse dataset")
  pause()
}
say("Adverse sample check is complete.")
pause()


# ==============================================================================
# 7. OUTPUT DATASET
# ==============================================================================
section("7. OUTPUT DATASET")

say("The output dataset holds each individual's id, treatment, region,")
say("and whether they experienced hyperkalemia.")

output_df <- merge(master_data_df,
                   uniquelab_df[, c("master_id", "lab_result")],
                   by = "master_id", all.x = TRUE)
output_df <- merge(output_df,
                   uniqueadverse_df[, c("master_id", "adverse_result")],
                   by = "master_id", all.x = TRUE)
View(output_df)

# Individuals with no Lab/Adverse record were never flagged, so they get 0
say("Setting missing markers to 0 (never flagged).")
output_df$lab_result[is.na(output_df$lab_result)]         <- 0
output_df$adverse_result[is.na(output_df$adverse_result)] <- 0

# Final outcome: 1 if flagged in either dataset
say("Determining the final binary outcome (1 if flagged in Lab OR Adverse).")
output_df$result <- pmax(output_df$lab_result, output_df$adverse_result, na.rm = TRUE)

for (i in 1:3) {
  cat(DASH, "\n")
  check_random_result(output_df, "master_id", "lab_result", "adverse_result", "result")
}
cat(DASH, "\n")

say("Comparing row counts of the Output and Master Data datasets.")
if (nrow(master_data_df) == nrow(output_df)) {
  say("Both datasets have the same number of records (", nrow(output_df), ").")
} else {
  say("WARNING: the datasets do NOT have the same number of records.")
}
pause()


# ==============================================================================
# 8. HYPERKALEMIA RISK BY TREATMENT ARM
# ==============================================================================
section("8. RISK BY TREATMENT ARM")

cross_table <- table(Treatment = output_df$treat, Hyperkalemia = output_df$result)
View(cross_table)
print(cross_table)

risk_by_arm <- prop.table(cross_table, margin = 1)
print(round(risk_by_arm, 3))

say("Hyperkalemia risk by treatment arm:")
for (arm in rownames(risk_by_arm)) {
  say("  treat = ", arm, ": ", round(risk_by_arm[arm, "1"] * 100, 1), "%", wait = 0)
}

# Use fisher.test(cross_table) instead if any cell count is small
chisq_res <- chisq.test(cross_table)
print(chisq_res)
say("Note: the chi-square p-value is ", signif(chisq_res$p.value, 3),
    ". A very small p-value means the difference in risk between arms is",
    " unlikely to be due to chance.")
pause()


# ==============================================================================
# 9. LOGISTIC REGRESSION MODELS
# ==============================================================================
section("9. LOGISTIC REGRESSION")

# --- Null model (no treatment) ---
say("Null model: overall hyperkalemia rate, ignoring treatment.")
null_model <- glm(result ~ 1, data = output_df, family = binomial)
print(summary(null_model))

# The intercept is a log-odds; plogis() (inverse logit) converts it to a probability
overall_rate <- plogis(unname(coef(null_model)[1]))
say("Overall hyperkalemia rate: ", round(overall_rate * 100, 1),
    "% (drug and placebo combined).")
pause()

# --- Treatment model ---
cat(DASH, "\n")
say("Treatment model: adds treatment as a predictor.")
treatment_model <- glm(result ~ treat, data = output_df, family = binomial)
print(summary(treatment_model))

# Intercept = baseline (reference group) rate
baseline_rate <- plogis(unname(coef(treatment_model)[1]))
say("Hyperkalemia rate in the reference group: ", round(baseline_rate * 100, 1), "%.")

# Exponentiated coefficients: intercept = baseline odds, treat = odds ratio
say("Exponentiated coefficients (baseline odds and odds ratio):")
print(exp(coef(treatment_model)))
say("The odds ratio is how many times higher the odds of hyperkalemia are in the",
    " treated group than in the reference group.")

say("95% confidence intervals for the odds ratios:")
print(exp(confint(treatment_model)))
say("If the interval for 'treat' excludes 1, the treatment is associated with a",
    " statistically significant change in the odds of hyperkalemia.")
pause()

say("Likelihood-ratio test: does adding treatment improve on the null model?")
print(anova(null_model, treatment_model, test = "Chisq"))
say("A very small p-value means treatment explains hyperkalemia better than",
    " the overall rate alone.")
pause()


# ==============================================================================
# 10. TREATMENT x REGION
# ==============================================================================
section("10. TREATMENT x REGION")

say("Counts of individuals by treatment and region:")
treat_region_table <- table(Treatment = output_df$treat, Region = output_df$region)
View(treat_region_table)
print(treat_region_table)
say("Row proportions (region mix within each treatment arm):")
print(round(prop.table(treat_region_table, margin = 1), 3))
pause()

additive_model    <- glm(result ~ treat + region, data = output_df, family = binomial)
interaction_model <- glm(result ~ treat * region, data = output_df, family = binomial)

say("Additive model (treat + region):")
print(summary(additive_model))
pause()

say("Interaction model (treat * region):")
print(summary(interaction_model))
pause()

say("Likelihood-ratio test: does the treatment effect differ by region?")
print(anova(additive_model, interaction_model, test = "Chisq"))
pause()

cat(DASH, "\n")
say("Drug odds ratio within each region:")
for (r in c(0, 1)) {
  m  <- glm(result ~ treat, data = subset(output_df, region == r), family = binomial)
  ci <- exp(confint(m)[2, ])
  say("Region ", r, " | OR: ", round(exp(coef(m)[2]), 2),
      " | 95% CI: ", round(ci[1], 2), " to ", round(ci[2], 2), wait = 0)
}
say("Region 1 = original region 1; region 0 = original region 2 (recoded).")

say("Analysis complete.")