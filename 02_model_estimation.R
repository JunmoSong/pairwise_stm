###############################################################################
# Script 2: Model Estimation (STM, LDA, CTM)
# Paper: "Discovering Preference Structure using Randomized Paired Comparisons
#         in Surveys: A Topic Modeling Approach"
# Journal: Sociological Methods & Research
# Authors: Jeong-han Kang, Eunrang Kwon, Junmo Song
#
# Description:
#   Estimates the main Structural Topic Model (STM) on the RPC data,
#   as well as comparison models: LDA and CTM (STM without covariates).
#   Saves all model objects to an .RData file for use by subsequent
#   figure/table scripts.
#
# Required packages (versions used):
#   tidyverse    2.0.0
#   stm          1.3.7
#   topicmodels  0.2-17
#   tidytext     0.4.3
#
# Input:  data/df_sp_grid.csv   (English replication raw data, N = 302)
#         data/df_sp_rpc.csv    (English replication raw data, N = 618)
# Output: data/model_objects.RData
###############################################################################

library(tidyverse)
library(stm)
library(topicmodels)
library(tidytext)

cat("=== Script 2: Model Estimation ===\n")

# =====================================================================
# 1. Load preprocessed data (English replication raw data)
# =====================================================================
df_sp_grid <- read.csv("data/df_sp_grid.csv", stringsAsFactors = FALSE)
df_sp_rpc  <- read.csv("data/df_sp_rpc.csv",  stringsAsFactors = FALSE)

cat("Grid subsample:", nrow(df_sp_grid), "respondents\n")
cat("RPC subsample:",  nrow(df_sp_rpc),  "respondents\n")

# =====================================================================
# 2. STM preprocessing
#    Treat each respondent's ordered-pair answers as a "document".
#    Each token is an ordered pair like "Wealth<Labor" (loser<winner).
#    removepunctuation = FALSE preserves the "<" separator.
# =====================================================================
cat("Preprocessing RPC data for STM...\n")
prep_sp <- textProcessor(df_sp_rpc$ans, metadata = df_sp_rpc,
                         wordLengths      = c(2, Inf),
                         lowercase        = FALSE,
                         removestopwords  = FALSE,
                         stem             = FALSE,
                         removepunctuation = FALSE,
                         removenumbers    = FALSE)
prep_sp <- prepDocuments(prep_sp$documents, prep_sp$vocab,
                         prep_sp$meta, lower.thresh = 0)

# =====================================================================
# 3. Determine optimal number of topics (K)
#    searchK evaluates held-out likelihood and semantic coherence.
#    K = 5 is chosen as the final model for interpretability.
# =====================================================================
# ── Reproducibility note for searchK ─────────────────────────────────
#
# searchK() randomly withholds a proportion of documents to compute
# held-out likelihood, and this sampling is not fully controlled by
# set.seed() — especially under multi-core execution. Diagnostic
# statistics (held-out likelihood, semantic coherence) may therefore
# vary across runs.
#
# This does not affect the final model: stm() with K = 5 is fully
# reproducible under set.seed(2026). Across runs, both diagnostics
# consistently support K = 3–6 as the stable range, corroborating
# the choice of K = 5.
#
# See: stm package documentation (https://rdrr.io/cran/stm/man/searchK.html).
# ─────────────────────────────────────────────────────────────────────
cat("Running searchK for K = 3 to 20 (this may take several minutes)...\n")
k_range <- seq(3, 20, 1)
set.seed(2026)
diag_sp <- searchK(prep_sp$documents, prep_sp$vocab, k_range,
                   prevalence = ~ age_10 + sex,
                   data  = prep_sp$meta,
                   cores = 4)
plot(diag_sp)
cat("searchK complete.\n")

# =====================================================================
# 4. Estimate main STM (K = 5) with prevalence covariates
# =====================================================================
cat("Estimating STM with K = 5...\n")
set.seed(2026)
model_sp <- stm(prep_sp$documents, prep_sp$vocab, K = 5,
                prevalence = ~ age_10 + sex,
                data = prep_sp$meta)

# Estimate covariate effects on topic prevalence
est_stm <- estimateEffect(1:5 ~ age_10 + sex,
                          model_sp,
                          metadata = prep_sp$meta)

# Extract document-topic proportions with metadata
df_stm_sp <- make.dt(model_sp, meta = prep_sp$meta)

# Recode age_10 for plotting (e.g., 2 -> 20, 3 -> 30, etc.)
age_decade_map <- c(`2` = 20, `3` = 30, `4` = 40, `5` = 50)
df_stm_sp$age_10 <- age_decade_map[as.character(df_stm_sp$age_10)]

cat("STM estimation complete. Topics:\n")
print(labelTopics(model_sp, n = 5))

# =====================================================================
# 5. Estimate CTM (STM without prevalence covariates)
#    CTM = Correlated Topic Model, implemented
#    in the stm package as an STM with no prevalence formula.
# =====================================================================
cat("Estimating CTM (STM without covariates)...\n")
set.seed(2026)
model_ctm_sp <- stm(prep_sp$documents, prep_sp$vocab, K = 5,
                    data = prep_sp$meta)

df_ctm_sp <- make.dt(model_ctm_sp, meta = prep_sp$meta)
df_ctm_sp$age_10 <- age_decade_map[as.character(df_ctm_sp$age_10)]


cat("CTM estimation complete.\n")

# =====================================================================
# 6. Estimate LDA (K = 5) for comparison
#    Uses the same vocabulary as the STM preprocessing step.
# =====================================================================
cat("Estimating LDA with K = 5...\n")

# Build document-term matrix from STM preprocessed data
df_stm_final <- model_sp %>%
  make.dt(meta = df_stm_sp[, -c(1:6)])   # drop docnum + Topic1-5 from meta

df_tokens <- df_stm_final %>%
  unnest_tokens(input = ans, output = token,
                token = stringr::str_split, pattern = " ",
                to_lower = FALSE) %>%
  count(docnum, token) %>%
  cast_dtm(document = docnum, term = token, value = n)

set.seed(2026)
model_lda <- LDA(df_tokens, k = 5)

# Create an STM-like list structure for LDA (for reuse of network functions)
lda_beta  <- tidy(model_lda, matrix = "beta")
lda_vocab <- model_lda@terms
topic_word_matrix <- matrix(0, nrow = 5, ncol = length(lda_vocab))
colnames(topic_word_matrix) <- lda_vocab

for (i in 1:5) {
  topic_i <- lda_beta %>% filter(topic == i)
  topic_word_matrix[i, topic_i$term] <- topic_i$beta
}

lda_logbeta <- log(topic_word_matrix + 1e-12)

model_lda_modi <- list(
  beta  = list(logbeta = list(lda_logbeta)),
  vocab = lda_vocab
)

# Merge LDA document-topic proportions (gamma) into the combined data frame
df_stm_final <- df_stm_final %>%
  left_join(
    tidy(model_lda, matrix = "gamma") %>%
      pivot_wider(names_from = topic, values_from = gamma) %>%
      rename_with(~ paste0("Topic_lda_", .), -document) %>%
      mutate(document = as.numeric(document)),
    by = c("docnum" = "document")
  )

cat("LDA estimation complete.\n")

# =====================================================================
# 7. Compute overall selection rates (win ratios) for network node sizing
#    win_ratio = times item chosen / times item appeared in any pair
# =====================================================================
cat("Computing overall selection rates...\n")

df_sp_temp <- df_sp_rpc
rpc_split  <- matrix(NA, nrow(df_sp_temp), 10) %>% as.data.frame()

for (i in seq_len(nrow(df_sp_temp))) {
  rpc_split[i, ] <- (df_sp_temp$ans[i] %>% str_split(" |<"))[[1]]
}

# Total frequency of each item across all presented pairs
freq_sp <- Reduce(`+`, lapply(1:10, function(j) table(rpc_split[, j]))) %>%
  as.data.frame()
colnames(freq_sp) <- c("Label", "freq_all")

# Win frequency (item in winner position: odd positions = winners = cols 2,4,6,8,10)
freq_sp_win <- Reduce(`+`, lapply(c(2, 4, 6, 8, 10),
                                  function(j) table(rpc_split[, j]))) %>%
  as.data.frame()
colnames(freq_sp_win) <- c("Label", "freq_win")

freq_sp <- left_join(freq_sp, freq_sp_win, by = "Label")
freq_sp$win_ratio <- freq_sp$freq_win / freq_sp$freq_all

# Node data frame for network visualizations (consistent label ordering)
item_labels <- c("Wealth", "Labor", "Education", "Wellbeing", "Demography",
                 "Integrity", "Safety", "Environment", "Resources", "Disaster")

df_node <- data.frame(
  label     = item_labels,
  win_ratio = freq_sp$win_ratio[match(item_labels, freq_sp$Label)],
  stringsAsFactors = FALSE
)

cat("Overall selection rates:\n")
print(df_node)

# =====================================================================
# 8. Save all model objects
# =====================================================================
save(df_sp_grid, df_sp_rpc, prep_sp, diag_sp,
     model_sp, est_stm, df_stm_sp,
     model_ctm_sp, df_ctm_sp,
     model_lda, model_lda_modi, df_stm_final,
     freq_sp, df_node,
     file = "data/model_objects.RData")

cat("All model objects saved to data/model_objects.RData\n")
cat("=== Script 2 complete ===\n")
