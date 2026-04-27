###############################################################################
# Script 1: Data Preprocessing
# Paper: "Discovering Preference Structure using Randomized Paired Comparisons
#         in Surveys: A Topic Modeling Approach"
# Journal: Sociological Methods & Research
#
# Description:
#   Reads the raw survey data (social_problem.xlsx) and produces two clean
#   datasets: one for the matrix-format (Grid/Likert) subsample and one for
#   the randomized paired comparison (RPC) subsample. Paired-comparison
#   responses are restructured into ordered-pair "text documents" required
#   by the topic modeling step.
#
# Required packages (versions used):
#   tidyverse  2.0.0
#   readxl     1.4.3
#
# Input:  data/social_problem.xlsx  (sheet 2)
# Output: data/df_sp_grid.csv       (matrix-format subsample, N = 302)
#         data/df_sp_rpc.csv         (RPC subsample with text answers, N = 618)
###############################################################################

library(tidyverse)
library(readxl)

# ---------- Set working directory to script location ---------
# (Assumes this script is run from the Final_submmit folder)

cat("=== Script 1: Data Preprocessing ===\n")

# =====================================================================
# 1. Import raw data
# =====================================================================
df_sp_raw <- read_xlsx("data/social_problem.xlsx", sheet = 2)
cat("Raw data loaded:", nrow(df_sp_raw), "respondents x", ncol(df_sp_raw), "columns\n")

# =====================================================================
# 2. Matrix-format (Grid/Likert) subsample
#    Respondents who answered A1s1–A1s10 (Likert items on social problems)
# =====================================================================
df_sp_grid <- df_sp_raw %>% filter(!is.na(A1s1))

# Select relevant columns: ID, demographics, 10 Likert items, politics, education
df_sp_grid <- df_sp_grid[, c(1:6, 25:34, 140, 141)]
colnames(df_sp_grid)[c(1:6, 17:18)] <- c("no", "uid", "sex", "year",
                                          "age_10", "region", "pol", "educ")

# SQ2 is a coded age variable: SQ2 = 1 corresponds to birth year 2006 (age 15 in 2021)
# Therefore: age = 2021 - (2007 - SQ2) = SQ2 + 14
df_sp_grid$age <- 2021 - (2007 - df_sp_grid$year)

# Recode sex: 1 = Female, 2 = Male
df_sp_grid$sex <- ifelse(df_sp_grid$sex == 1, "Female", "Male")

# Recode age decade (original coding starts from 0, so +1)
df_sp_grid$age_10 <- df_sp_grid$age_10 + 1

cat("Grid subsample:", nrow(df_sp_grid), "respondents\n")

# =====================================================================
# 3. Randomized Paired Comparison (RPC) subsample
#    Respondents for whom A1s1 is NA received the pairwise format instead
# =====================================================================
df_sp_rpc <- df_sp_raw %>% filter(is.na(A1s1))

# --- Function to convert raw pairwise answers to ordered-pair text ---
# Each respondent saw 5 randomly generated pairs from 10 social problems.
# The raw data stores each pair as a set of 21 columns (10 + 10 indicator
# columns for the two items + 1 column for the chosen item). This function
# converts them into "loser<winner" ordered-pair strings.
trans_ans <- function(row) {
  # Labels corresponding to column positions (repeated for 20 indicator cols)
  t_label <- c("Wealth", "Labor", "Education", "Wellbeing", "Demography",
               "Integrity", "Safety", "Environment", "Resources", "Disaster",
               "Wealth", "Labor", "Education", "Wellbeing", "Demography",
               "Integrity", "Safety", "Environment", "Resources", "Disaster")

  answer <- as.character(row[, c(35:139)])   # extract response columns
  orders <- c()

  for (i in seq(1, length(answer), 21)) {
    ans_cell <- answer[i:(i + 20)]                          # 21-element block
    q_pair   <- t_label[which(ans_cell > 0)][1:2]           # the two presented items
    ans      <- t_label[as.numeric(ans_cell[21])]           # the chosen item
    # Build ordered pair: loser < winner
    order_cell <- paste0(c(q_pair[!q_pair %in% ans], ans), collapse = "<")
    orders <- append(orders, order_cell)
  }
  paste0(orders, collapse = " ")
}

# Apply conversion to every respondent (may take a moment)
cat("Converting RPC responses to ordered pairs...\n")
df_sp_rpc$ans <- NA
for (i in seq_len(nrow(df_sp_rpc))) {
  df_sp_rpc$ans[i] <- trans_ans(df_sp_rpc[i, ])
}

# Keep only needed columns
df_sp_rpc <- df_sp_rpc[, -c(9:139)]
colnames(df_sp_rpc) <- c("no", "uid", "sex", "year", "age_10", "region",
                          "irb_1", "irb_2", "pol", "educ", "ans")

# Compute age and recode variables
df_sp_rpc$age  <- 2021 - (2007 - df_sp_rpc$year)
df_sp_rpc$sex  <- ifelse(df_sp_rpc$sex == 1, "Female", "Male")
df_sp_rpc$age_10 <- df_sp_rpc$age_10 + 1

cat("RPC subsample:", nrow(df_sp_rpc), "respondents\n")

# =====================================================================
# 4. Export preprocessed data
# =====================================================================
write.csv(df_sp_grid, "data/df_sp_grid.csv", row.names = FALSE)
write.csv(df_sp_rpc,  "data/df_sp_rpc.csv",  row.names = FALSE)

cat("Preprocessed data saved to data/df_sp_grid.csv and data/df_sp_rpc.csv\n")
cat("=== Script 1 complete ===\n")
