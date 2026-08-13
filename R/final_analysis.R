# Load Packages

library(tidyverse)
library(purrr)

# Load data
adult_paired <- read_csv("data/Adult_Paired.csv")

# Select relevant columns
q_entry <- paste0("Q", sprintf("%02d", 1:30), "_entry")
q_exit  <- paste0("Q", sprintf("%02d", 1:30), "_exit")

data_q <- adult_paired %>%
  select(Adult_ID, Subgroup_115, all_of(c(q_entry, q_exit)))

# Reverse code Q11 (sodas) and Q17 (thaw food)
data_q <- data_q %>%
  mutate(
    Q11_rc_entry = 8 - Q11_entry,
    Q11_rc_exit  = 8 - Q11_exit,
    Q17_rc_entry = 7 - Q17_entry,
    Q17_rc_exit  = 7 - Q17_exit
  )

# Domain question lists

all_questions  <- c("Q01","Q02","Q03","Q04","Q05","Q06","Q07","Q08",
                    "Q09","Q10","Q11_rc","Q12","Q13","Q14","Q15",
                    "Q16","Q17_rc","Q18","Q19","Q20","Q21","Q22",
                    "Q23","Q24","Q25","Q26","Q27","Q28","Q29","Q30")
diet_questions <- c("Q01","Q02","Q03","Q04","Q05","Q06","Q07","Q08",
                    "Q09","Q10","Q11_rc")
frm_questions  <- c("Q19","Q20","Q21","Q22","Q23","Q24","Q25","Q26","Q27")
pa_questions   <- c("Q12","Q13","Q14")
fs_questions   <- c("Q15","Q16","Q17_rc","Q18")
fsec_questions <- c("Q28","Q29","Q30")

domains <- c("All", "Diet", "FRM", "PA", "FSafety", "FSecurity")

# Helper: compute paired domain scores (entry & exit row means)
paired_domain_score <- function(df, qs) {
  entry_mat <- as.matrix(df[, paste0(qs, "_entry")])
  exit_mat  <- as.matrix(df[, paste0(qs, "_exit")])

  pair_mask           <- !is.na(entry_mat) & !is.na(exit_mat)
  entry_mat[!pair_mask] <- NA
  exit_mat[!pair_mask]  <- NA

  list(
    entry  = rowMeans(entry_mat, na.rm = TRUE),
    exit   = rowMeans(exit_mat,  na.rm = TRUE),
    n_items = rowSums(pair_mask)
  )
}

# Compute domain scores and append to data_q

domain_map <- list(
  All      = all_questions,
  Diet     = diet_questions,
  FRM      = frm_questions,
  PA       = pa_questions,
  FSafety  = fs_questions,
  FSecurity = fsec_questions
)

for (d in names(domain_map)) {
  scores <- paired_domain_score(data_q, domain_map[[d]])
  data_q[[paste0(d, "_entry")]]  <- scores$entry
  data_q[[paste0(d, "_exit")]]   <- scores$exit
  data_q[[paste0(d, "_change")]] <- scores$exit - scores$entry
}

# Helper: effect size r for paired Wilcoxon
get_wilcox_effect <- function(entry, exit) {
  complete  <- complete.cases(entry, exit)
  entry     <- entry[complete]
  exit      <- exit[complete]
  n         <- length(entry)

  test  <- wilcox.test(exit, entry, paired = TRUE, exact = FALSE)
  mu    <- n * (n + 1) / 4
  sigma <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  z     <- (test$statistic - mu) / sigma
  r     <- abs(z) / sqrt(n)

  tibble(N = n, V = as.numeric(test$statistic), Effect_Size_r = r)
}

# Helper: effect size r for two-sample Mann-Whitney U
get_mwu_effect <- function(g0, g1) {
  n0      <- sum(!is.na(g0))
  n1      <- sum(!is.na(g1))
  n_total <- n0 + n1
  test    <- wilcox.test(g0, g1, exact = FALSE)
  z       <- qnorm(test$p.value / 2) * sign(test$statistic - (n0 * n1 / 2))
  r       <- abs(z) / sqrt(n_total)
  list(test = test, n0 = n0, n1 = n1, r = r)
}

# Helper: effect size interpretation
interpret_r <- function(r) {
  case_when(
    r < 0.10 ~ "Negligible",
    r < 0.30 ~ "Small",
    r < 0.50 ~ "Medium",
    TRUE     ~ "Large"
  )
}

# OVERALL within-group results (all participants, entry vs exit per domain)
#    H0: Median change = 0 | H1: Median change > 0
#    Test: Wilcoxon Signed-Rank (paired)

overall_results <- map_df(domains, function(d) {
  entry <- data_q[[paste0(d, "_entry")]]
  exit  <- data_q[[paste0(d, "_exit")]]
  eff   <- get_wilcox_effect(entry, exit)

  test  <- wilcox.test(exit, entry, paired = TRUE, exact = FALSE)

  tibble(
    Domain                     = d,
    N                          = eff$N,
    Entry_Median               = median(entry, na.rm = TRUE),
    Exit_Median                = median(exit,  na.rm = TRUE),
    P_Value                    = test$p.value,
    Effect_Size_r              = eff$Effect_Size_r,
    Effect_Size_Interpretation = interpret_r(eff$Effect_Size_r)
  )
}) %>%
  mutate(P_Value_Display = format.pval(P_Value, digits = 4, eps = 1e-20))

overall_results

# WITHIN-GROUP results per subgroup (entry vs exit, paired Wilcoxon)
#    H0: Median change = 0 within subgroup | H1: Median change > 0
#    Test: Wilcoxon Signed-Rank (paired), separately per subgroup
within_group_results <- map_df(domains, function(d) {
  run_within <- function(grp) {
    sub   <- data_q %>% filter(Subgroup_115 == grp)
    entry <- sub[[paste0(d, "_entry")]]
    exit  <- sub[[paste0(d, "_exit")]]
    eff   <- get_wilcox_effect(entry, exit)
    test  <- wilcox.test(exit, entry, paired = TRUE, exact = FALSE)

    tibble(
      N             = eff$N,
      Entry_Median  = median(entry, na.rm = TRUE),
      Exit_Median   = median(exit,  na.rm = TRUE),
      P_Value       = test$p.value,
      Effect_Size_r = eff$Effect_Size_r,
      Interpretation = interpret_r(eff$Effect_Size_r)
    )
  }

  bind_cols(
    tibble(Domain = d),
    run_within(0) %>% rename_with(~ paste0("Group0_", .)),
    run_within(1) %>% rename_with(~ paste0("Group1_", .))
  )
}) %>%
  mutate(
    Group0_P_Display = format.pval(Group0_P_Value, digits = 4, eps = 1e-20),
    Group1_P_Display = format.pval(Group1_P_Value, digits = 4, eps = 1e-20)
  )

within_group_results

# BETWEEN-GROUP results per domain (Mann-Whitney U on change scores)
#    H0: Change score distribution equal across subgroups
#    H1: Change score distribution differs across subgroups
#    Test: Mann-Whitney U (two-sample, unpaired)

domain_between_results <- map_df(domains, function(d) {
  g0 <- data_q[[paste0(d, "_change")]][data_q$Subgroup_115 == 0]
  g1 <- data_q[[paste0(d, "_change")]][data_q$Subgroup_115 == 1]
  eff <- get_mwu_effect(g0, g1)

  tibble(
    Domain               = d,
    N_Group0             = eff$n0,
    N_Group1             = eff$n1,
    Group0_Median_Change = median(g0, na.rm = TRUE),
    Group1_Median_Change = median(g1, na.rm = TRUE),
    W_Statistic          = as.numeric(eff$test$statistic),
    P_Value              = eff$test$p.value,
    Effect_Size_r        = eff$r,
    Interpretation       = interpret_r(eff$r)
  )
}) %>%
  mutate(P_Value_Display = format.pval(P_Value, digits = 4, eps = 1e-20))

domain_between_results

# BETWEEN-GROUP results per question (Mann-Whitney U on change scores)
#    H0: Change score distribution equal across subgroups for this question
#    H1: Change score distribution differs across subgroups for this question
#    Test: Mann-Whitney U (two-sample, unpaired)

per_question_results <- map_df(all_questions, function(q) {
  change <- data_q[[paste0(q, "_exit")]] - data_q[[paste0(q, "_entry")]]
  g0     <- change[data_q$Subgroup_115 == 0]
  g1     <- change[data_q$Subgroup_115 == 1]
  eff    <- get_mwu_effect(g0, g1)

  tibble(
    Question             = q,
    N_Group0             = eff$n0,
    N_Group1             = eff$n1,
    Group0_Median_Change = median(g0, na.rm = TRUE),
    Group1_Median_Change = median(g1, na.rm = TRUE),
    W_Statistic          = as.numeric(eff$test$statistic),
    P_Value              = eff$test$p.value,
    Effect_Size_r        = eff$r,
    Interpretation       = interpret_r(eff$r)
  )
}) %>%
  mutate(P_Value_Display = format.pval(P_Value, digits = 4, eps = 1e-20))

per_question_results

# WRITE OUTPUTS

write_csv(overall_results,        "Reports/overall_results.csv")
write_csv(within_group_results,   "Reports/within_group_results.csv")
write_csv(domain_between_results, "Reports/domain_between_results.csv")
write_csv(per_question_results,   "Reports/per_question_results.csv")

# WRITE ALL RESULTS INTO ONE CSV

all_results_combined <- bind_rows(
  overall_results %>%
    mutate(Section = "1. Overall (All Participants)") %>%
    select(Section, everything()),

  within_group_results %>%
    mutate(Section = "2. Within-Group (Entry vs Exit per Subgroup)") %>%
    select(Section, everything()),

  domain_between_results %>%
    mutate(Section = "3. Between-Group per Domain") %>%
    select(Section, everything()),

  per_question_results %>%
    mutate(Section = "4. Between-Group per Question") %>%
    select(Section, everything())
)

write_csv(all_results_combined, "Reports/all_results.csv")

cat("Written: Reports/all_results.csv\n")