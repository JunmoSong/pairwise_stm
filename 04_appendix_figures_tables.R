###############################################################################
# Script 4: Appendix Figures and Tables
# Paper: "Discovering Preference Structure using Randomized Paired Comparisons
#         in Surveys: A Topic Modeling Approach"
# Journal: Sociological Methods & Research
# Authors: Jeong-han Kang, Eunrang Kwon, Junmo Song
#
# Description:
#   Generates appendix figures (A1–A3) and tables (A1–A2).
#   - Figure A1 : Model diagnostics (searchK – held-out likelihood &
#                 semantic coherence over K = 3–20)
#   - Figure A2A–A2E : Topic networks using Highest Probable (Prob) terms
#   - Figure A3 : Gender effect on topic prevalence (coefficient plot)
#                 x-axis: "Male <---> Female" (left = Male, right = Female)
#   - Table A1  : Balance check – Matrix vs. RPC respondents on key
#                 demographic variables
#   - Table A2  : Correlation matrix of LDA topic proportions
#
# Required packages (versions used):
#   tidyverse    2.0.0
#   stm          1.3.7
#   igraph       2.1.4
#   ggraph       2.2.1
#
# Input:  data/model_objects.RData
# Output: output/figures/Figure_A*.png
#         output/tables/Table_A*.csv
###############################################################################

library(tidyverse)
library(stm)
library(igraph)
library(ggraph)

cat("=== Script 4: Appendix Figures and Tables ===\n")

# Load model objects from Script 2
load("data/model_objects.RData")

# Create output directories if they do not exist
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

# =====================================================================
# Helper: compute within-topic selection rate for each item
# =====================================================================
calc_topic_winrate <- function(model, topic) {
  vocab <- model$vocab
  beta  <- model$beta$logbeta[[1]][topic, ]
  p     <- exp(beta); p <- p / sum(p)

  df <- tibble(token = vocab, p = p) %>%
    filter(str_detect(token, "<")) %>%
    separate(token, into = c("loser", "winner"), sep = "<", remove = FALSE)

  win_mass  <- df %>% group_by(winner) %>%
    summarise(W = sum(p), .groups = "drop") %>% rename(name = winner)
  lose_mass <- df %>% group_by(loser)  %>%
    summarise(L = sum(p), .groups = "drop") %>% rename(name = loser)

  full_join(win_mass, lose_mass, by = "name") %>%
    mutate(W = coalesce(W, 0), L = coalesce(L, 0),
           total    = W + L,
           win_rate = if_else(total > 0, W / total, NA_real_))
}

# =====================================================================
# Helper: draw topic network using Highest Probable (Prob) terms
#   Identical to draw_topic_network() in Script 3 except it uses
#   labelTopics()$prob instead of $frex for edge selection.
# =====================================================================
draw_topic_network_prob <- function(model, df_node, topic, top_word = 20,
                                    layout = "fr", title_name = NULL,
                                    top_n = 2, bottom_n = 2,
                                    cap_mm_max = 22) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  vertices_all <- df_node %>%
    transmute(name = as.character(label), overall_rate = as.numeric(win_ratio))

  wr       <- calc_topic_winrate(model, topic) %>% filter(!is.na(win_rate))
  top_nodes <- wr %>% slice_max(win_rate, n = top_n,    with_ties = FALSE) %>% pull(name)
  bot_nodes <- wr %>% slice_min(win_rate, n = bottom_n, with_ties = FALSE) %>% pull(name)

  highlight_tbl <- tibble(
    name      = c(top_nodes, bot_nodes),
    sel_group = c(rep("Top", length(top_nodes)), rep("Bottom", length(bot_nodes)))
  )

  # Use Highest Probable terms (prob) instead of FREX
  prob_words <- labelTopics(model, n = top_word)$prob[topic, ]

  beta_all <- model$beta$logbeta[[1]][topic, ]
  p_all    <- exp(beta_all); p_all <- p_all / sum(p_all)
  p_map    <- setNames(p_all, model$vocab)

  net_df <- tibble(token = as.character(prob_words)) %>%
    mutate(from       = str_split(token, "<", simplify = TRUE)[, 1],
           to         = str_split(token, "<", simplify = TRUE)[, 2],
           Prevalence = unname(p_map[token])) %>%
    filter(from != "", to != "") %>%
    transmute(from, to, Prevalence)

  topic_net <- graph_from_data_frame(net_df, directed = TRUE)

  vdf <- tibble(name = V(topic_net)$name) %>%
    left_join(vertices_all,  by = "name") %>%
    left_join(highlight_tbl, by = "name") %>%
    mutate(sel_group         = if_else(is.na(sel_group), "Normal", sel_group),
           is_highlight      = sel_group != "Normal",
           overall_rate_plot = if_else(is.na(overall_rate), 0.05, overall_rate))

  V(topic_net)$sel_group         <- vdf$sel_group
  V(topic_net)$is_highlight      <- vdf$is_highlight
  V(topic_net)$overall_rate_plot <- vdf$overall_rate_plot

  p <- ggraph(topic_net, layout = layout) +

    geom_edge_fan(aes(edge_width = Prevalence),
                  arrow     = arrow(type = "closed", length = unit(3.5, "mm")),
                  start_cap = circle(cap_mm_max, "mm"),
                  end_cap   = circle(cap_mm_max, "mm"),
                  color = "grey30", alpha = 0.9) +

    geom_node_text(data = function(x) x[x$sel_group == "Normal", ],
                   aes(label = name, size = overall_rate_plot),
                   color = "gray15", fontface = "bold", show.legend = FALSE) +

    # Top nodes: thick solid border, black text, white fill
    geom_node_label(data = function(x) x[x$sel_group == "Top", ],
                    aes(label = name, size = overall_rate_plot, color = sel_group),
                    fill = "white", fontface = "bold",
                    label.size = 0.7,
                    label.padding = unit(0.25, "lines"),
                    label.r = unit(0.15, "lines"), show.legend = TRUE) +

    # Bottom nodes: lighter fill (gray94) + thin border to visually distinguish
    geom_node_label(data = function(x) x[x$sel_group == "Bottom", ],
                    aes(label = name, size = overall_rate_plot, color = sel_group),
                    fill = "gray94", fontface = "bold",
                    label.size = 0.35,
                    label.padding = unit(0.25, "lines"),
                    label.r = unit(0.15, "lines"), show.legend = TRUE) +

    scale_edge_width(range = c(0.8, 4.0), name = "Prevalence") +
    scale_color_manual(name   = "Selection rate\n(within Topic)",
                       values = c(Top = "black", Bottom = "gray40"),
                       breaks = c("Top", "Bottom")) +
    scale_size_continuous(range = c(4.8, 9.0),
                          name  = "Selection rate\n(Overall)") +

    labs(title = title_name %||% paste0("Topic ", topic)) +

    theme_graph(base_family = "Times New Roman") +
    theme(
      plot.title       = element_text(face = "bold", size = 26, hjust = 0.5,
                                      margin = margin(b = 20)),
      legend.position  = "bottom",
      legend.box       = "vertical",
      legend.title     = element_text(face = "bold", size = 14),
      legend.text      = element_text(size = 12),
      legend.spacing.y = unit(0.3, "cm"),
      legend.key       = element_blank(),
      plot.margin      = margin(30, 30, 30, 30)
    ) +
    
    guides(
      # Prevalence legend: Removing box
      edge_width = guide_legend(order = 1, override.aes = list(
        fill = NA, color = NA, label.size = 0, label = "")),
      
      # Selection rate legend
      color = guide_legend(order = 2, override.aes = list(
        label = "a", size = 6,
        fill = c("white", "gray94"),
        color = c("black", "gray40"),
        label.size = c(0.7, 0.4))),
      
      size = guide_legend(order = 3, override.aes = list(
        label = "a", label.size = 0, fill = NA, color = "black",
        stroke = 0, linetype = 0))
    ) +
    coord_cartesian(clip = "off")
  
  return(p)
}

# =====================================================================
# Figure A1: Diagnostic values from searchK
#   Held-out likelihood (global peak ≈ K=12, local peak ≈ K=5) and
#   semantic coherence over the range K = 3 to 20.
# =====================================================================
cat("Generating Figure A1...\n")

fig_a1 <- diag_sp$results %>%
  select(K, heldout, semcoh) %>%
  mutate(K       = unlist(K),
         heldout = unlist(heldout),
         semcoh  = unlist(semcoh)) %>%
  pivot_longer(cols = c(heldout, semcoh), names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         heldout = "Held-Out Likelihood",
                         semcoh  = "Semantic Coherence")) %>%
  ggplot(aes(x = K, y = value)) +
  geom_line(color = "gray20") +
  geom_point(shape = 21, size = 2, fill = "white", color = "black") +
  theme_bw() +
  xlab("Number of Topics (K)") + ylab("") +
  facet_wrap(~ metric, scales = "free_y") +
  theme(strip.text  = element_text(face = "bold", size = 13),
        axis.title  = element_text(face = "bold", size = 12),
        axis.text   = element_text(size = 11))

ggsave("output/figures/Figure_A1.png", fig_a1, width = 10, height = 5.5, dpi = 300)

# =====================================================================
# Figures A2A–A2E: Topic networks using Highest Probable terms
#   Serves as a robustness check against the FREX-based Figure 5 series.
# =====================================================================
cat("Generating Figures A2A-A2E...\n")

topic_names <- c("Topic1: Sustainable Economy",
                 "Topic2: Life security",
                 "Topic3: Social Safety net",
                 "Topic4: Economic wellbeing",
                 "Topic5: Environments")
cap_sizes   <- c(23, 21, 22, 22, 26)

for (k in 1:5) {
  set.seed(42)
  p <- draw_topic_network_prob(model_sp, df_node, k,
                               top_word   = 20,
                               layout     = "fr",
                               title_name = paste0("<", topic_names[k], ">"),
                               cap_mm_max = cap_sizes[k])
  ggsave(sprintf("output/figures/Figure_A2%s.png", LETTERS[k]),
         p, width = 11, height = 9, dpi = 300)
}

# =====================================================================
# Figure A3: Gender effect on topic prevalence (coefficient plot)
#
#   Extracts the "sexMale" coefficient from estimateEffect().
#   Reference category = Female (alphabetically first in R factor).
#   Raw coefficient: positive = Male > Female.
#
#   To align with the axis label "Male <---> Female" (left = Male,
#   right = Female), the coefficient is NEGATED so that a positive
#   value means Female > Male (Female side = right of zero).
# =====================================================================
cat("Generating Figure A3...\n")

eff_data <- data.frame(
  topic    = 1:5,
  # Negate sexMale coefficient: positive = Female has higher prevalence
  estimate = sapply(1:5, function(t) {
    ct <- summary(est_stm, topics = t)$tables[[1]]
    -1 * ct["sexMale", "Estimate"]
  }),
  se = sapply(1:5, function(t) {
    ct <- summary(est_stm, topics = t)$tables[[1]]
    ct["sexMale", "Std. Error"]
  })
) %>%
  mutate(ci_lower = estimate - 1.96 * se,
         ci_upper = estimate + 1.96 * se)

fig_a3 <- eff_data %>%
  ggplot(aes(x = factor(topic), y = estimate)) +
  geom_point(size = 3, shape = 15) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  coord_flip() +
  scale_y_continuous(limits = c(-0.15, 0.15),
                     breaks = c(-0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15),
                     labels = function(x) sprintf("%.2f", x)) +
  scale_x_discrete(labels = c("1. Sustainable Economy",
                               "2. Life Security",
                               "3. Social Safety Net",
                               "4. Economic Wellbeing",
                               "5. Environments")) +
  labs(x = "Topic", y = "Male <---> Female",
       caption = "* 95% CI") +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold", size = 13),
        axis.text    = element_text(face = "bold", size = 11),
        plot.caption = element_text(face = "bold", size = 11,
                                    hjust = 1, margin = margin(t = 4)))

ggsave("output/figures/Figure_A3.png", fig_a3,
       width = 6.5, height = 4.5, dpi = 300)

# =====================================================================
# Table A1: Balance check – Matrix format vs. RPC respondents
#   Compares key demographic variables between the two subsamples.
#   Continuous variables: mean (SD), two-sample t-test.
#   Binary variable (sex): % female, Pearson chi-square test.
#   Ordinal/categorical (pol, educ): chi-square test.

# pol (Q1): 1=strongly progressive, 2=progressive, 3=moderate, 4=conservative, 5=strongly conservative
# educ (Q2): 1=high school or less, 2=some college or associate, 3=bachelor's or some 4-year, 4=graduate degree
# =====================================================================
cat("Generating Table A1...\n")

# --- Age (continuous) ---
age_t    <- t.test(df_sp_grid$age, df_sp_rpc$age)

# --- Sex (binary) ---
sex_tab  <- table(
  Sex   = c(df_sp_grid$sex,  df_sp_rpc$sex),
  Group = c(rep("Matrix", nrow(df_sp_grid)), rep("RPC", nrow(df_sp_rpc)))
)
sex_chi  <- chisq.test(sex_tab)

# --- Political ideology (ordinal → chi-square) ---
pol_tab  <- table(
  Pol   = c(df_sp_grid$pol,  df_sp_rpc$pol),
  Group = c(rep("Matrix", nrow(df_sp_grid)), rep("RPC", nrow(df_sp_rpc)))
)
pol_chi  <- chisq.test(pol_tab)

# --- Education (ordinal → chi-square) ---
educ_tab <- table(
  Educ  = c(df_sp_grid$educ, df_sp_rpc$educ),
  Group = c(rep("Matrix", nrow(df_sp_grid)), rep("RPC", nrow(df_sp_rpc)))
)
educ_chi <- chisq.test(educ_tab)

# --- Helper: compute mode proportion within each group ---
pct_mode <- function(x) {
  tbl <- sort(table(x), decreasing = TRUE)
  sprintf("%.1f (mode=%s)", prop.table(tbl)[1] * 100, names(tbl)[1])
}

# --- Assemble table ---
table_a1 <- data.frame(
  Variable = c(
    "Age (years)",
    "Female (%)",
    "Political ideology",
    "Education level"
  ),
  `Matrix (N=302)` = c(
    sprintf("%.1f (%.1f)", mean(df_sp_grid$age, na.rm = TRUE),
            sd(df_sp_grid$age,   na.rm = TRUE)),
    sprintf("%.1f", mean(df_sp_grid$sex == "Female", na.rm = TRUE) * 100),
    pct_mode(df_sp_grid$pol),
    pct_mode(df_sp_grid$educ)
  ),
  `RPC (N=618)` = c(
    sprintf("%.1f (%.1f)", mean(df_sp_rpc$age, na.rm = TRUE),
            sd(df_sp_rpc$age,   na.rm = TRUE)),
    sprintf("%.1f", mean(df_sp_rpc$sex == "Female", na.rm = TRUE) * 100),
    pct_mode(df_sp_rpc$pol),
    pct_mode(df_sp_rpc$educ)
  ),
  `Test statistic` = c(
    sprintf("t = %.2f",   age_t$statistic),
    sprintf("χ² = %.2f", sex_chi$statistic),
    sprintf("χ² = %.2f", pol_chi$statistic),
    sprintf("χ² = %.2f", educ_chi$statistic)
  ),
  `p-value` = c(
    sprintf("%.3f", age_t$p.value),
    sprintf("%.3f", sex_chi$p.value),
    sprintf("%.3f", pol_chi$p.value),
    sprintf("%.3f", educ_chi$p.value)
  ),
  check.names = FALSE
)



write.csv(table_a1, "output/tables/Table_A1.csv", row.names = FALSE)

# =====================================================================
# Table A2: Correlation matrix of LDA topic proportions (gamma values)
#   All five topics are included (column names Topic_lda_1 … Topic_lda_5).
# =====================================================================
cat("Generating Table A2...\n")

lda_cols <- grep("^Topic_lda_", colnames(df_stm_final), value = TRUE)
lda_cor  <- cor(as.data.frame(df_stm_final)[, lda_cols])
colnames(lda_cor) <- rownames(lda_cor) <- paste0("LDA Topic ", 1:5)

write.csv(round(lda_cor, 3), "output/tables/Table_A2.csv")

cat("=== Script 4 complete ===\n")
