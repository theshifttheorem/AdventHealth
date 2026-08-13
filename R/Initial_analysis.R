# load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rstatix)
library(tidyr)
library(purrr)
library(emmeans)


# load data
adult_paired <- read_csv("data/Adult_Paired.csv")

# Range validation (ensuring responses are 1‑7 or 0‑7)
# question columns
q_entry <- paste0("Q", sprintf("%02d", 1:30), "_entry")
q_exit <- paste0("Q", sprintf("%02d", 1:30), "_exit")
# q_all   <- c(q_entry, q_exit)
# min‑max for each question
# min_vals <- c(rep(1, 11), 0, 0, rep(1, 17))   # Q12 & Q13 allow 0
# max_vals <- c( 7,  6,  5,  5,  7,  7,  7,  7,  7,  7, 7,  7,  7,  6,  6,  6,  6,  6,  6,  6, 6,  6,  6,  6,  6,  6,  6,  4,  4,  3)

# Only take Adult_ID, entry and exit questions
data_q <- adult_paired %>% select(Adult_ID, Subgroup_115, all_of(c(q_entry, q_exit)))

# Reverse code Q11 (sodas), Q17(thaw food)
data_q <- data_q %>%
  mutate(
    Q11_rc_entry = 8 - Q11_entry,
    Q11_rc_exit = 8 - Q11_exit,
    Q17_rc_entry = 7 - Q17_entry,
    Q17_rc_exit = 7 - Q17_exit
  )

#adult_clean <- adult_paired %>% drop_na(all_of(q_all))
#cat("\nClean file rows:    ", nrow(adult_clean),                    "\n")
#cat("Clean file columns: ", ncol(adult_clean),                    "\n")
#cat("Adults in Subgroup 115:      ", sum(adult_clean$Subgroup_115 == 1),   "\n")
#cat("Adults NOT in Subgroup 115:  ", sum(adult_clean$Subgroup_115 == 0),   "\n")

# domain scores for diet quality, food resource management, physical activity, food safety, food security

paired_domain_score <- function(df, qs){

  entry_mat <- as.matrix(df[, paste0(qs, "_entry")])
  exit_mat  <- as.matrix(df[, paste0(qs, "_exit")])

  pair_mask <- !is.na(entry_mat) & !is.na(exit_mat)

  entry_mat[!pair_mask] <- NA
  exit_mat[!pair_mask]  <- NA

  list(
    entry = rowMeans(entry_mat, na.rm = TRUE),
    exit  = rowMeans(exit_mat, na.rm = TRUE),
    n_items = rowSums(pair_mask)
  )
}

all_questions <- c("Q01","Q02","Q03","Q04","Q05",
  "Q06","Q07","Q08","Q09","Q10", "Q11_rc",
  "Q12","Q13","Q14", "Q15","Q16","Q17_rc","Q18",
  "Q19","Q20","Q21","Q22",
  "Q23","Q24","Q25","Q26","Q27",
  "Q28","Q29","Q30")

all <- paired_domain_score(data_q, all_questions)

data_q$All_entry <- all$entry
data_q$All_exit  <- all$exit

diet_questions <- c("Q01","Q02","Q03","Q04","Q05",
  "Q06","Q07","Q08","Q09","Q10", "Q11_rc")

diet <- paired_domain_score(data_q, diet_questions)

data_q$Diet_entry <- diet$entry
data_q$Diet_exit  <- diet$exit

frm_questions <- c("Q19","Q20","Q21","Q22","Q23","Q24","Q25","Q26","Q27")

frm2 <- paired_domain_score(data_q, frm_questions)

data_q$FRM_entry <- frm2$entry
data_q$FRM_exit  <- frm2$exit

pa_questions <- c("Q12","Q13","Q14")

pa <- paired_domain_score(data_q, pa_questions)

data_q$PA_entry <- pa$entry
data_q$PA_exit  <- pa$exit

fs_questions <- c("Q15","Q16","Q17_rc","Q18")

fs <- paired_domain_score(data_q, fs_questions)

data_q$FSafety_entry <- fs$entry
data_q$FSafety_exit  <- fs$exit

fsec_questions <- c("Q28","Q29","Q30")

fsec <- paired_domain_score(data_q, fsec_questions)

data_q$FSecurity_entry <- fsec$entry
data_q$FSecurity_exit  <- fsec$exit

# wilcox test for each domain

wilcox.test(data_q$All_exit,
            data_q$All_entry,
            paired = TRUE)

wilcox.test(data_q$Diet_exit,
            data_q$Diet_entry,
            paired = TRUE)

wilcox.test(data_q$FRM_exit,
            data_q$FRM_entry,
            paired = TRUE)

wilcox.test(data_q$PA_exit,
            data_q$PA_entry,
            paired = TRUE)

wilcox.test(data_q$FSafety_exit,
            data_q$FSafety_entry,
            paired = TRUE)

wilcox.test(data_q$FSecurity_exit,
            data_q$FSecurity_entry,
            paired = TRUE)



# summary table
domains <- c("All", "Diet", "FRM", "PA", "FSafety", "FSecurity")

overall_results <- map_df(domains, function(x){

  test <- wilcox.test(
    data_q[[paste0(x,"_exit")]],
    data_q[[paste0(x,"_entry")]],
    paired = TRUE
  )

  tibble(
    Domain = x,
    Entry_Median =
      median(data_q[[paste0(x,"_entry")]], na.rm=TRUE),
    Exit_Median =
      median(data_q[[paste0(x,"_exit")]], na.rm=TRUE),
    P_Value = test$p.value
  )
})

overall_results %>%
  mutate(
    P_Value = format.pval(
      P_Value,
      digits = 4,
      eps = 1e-20
    )
  )

# effect sizes
get_wilcox_effect <- function(entry, exit){

  # Keep only complete pairs
  complete <- complete.cases(entry, exit)

  entry <- entry[complete]
  exit  <- exit[complete]

  test <- wilcox.test(exit, entry,
                      paired = TRUE,
                      exact = FALSE)

  n <- length(entry)

  # Expected value
  mu <- n * (n + 1) / 4

  # Standard deviation
  sigma <- sqrt(n * (n + 1) * (2 * n + 1) / 24)

  z <- (test$statistic - mu) / sigma

  r <- abs(z) / sqrt(n)

  tibble(
    N = n,
    V = as.numeric(test$statistic),
    Effect_Size_r = r
  )
}

overall_effect_results <- map_df(domains, function(x){

  eff <- get_wilcox_effect(
    data_q[[paste0(x, "_entry")]],
    data_q[[paste0(x, "_exit")]]
  )

  tibble(
    Domain = x,
    N = eff$N,
    Effect_Size_r = eff$Effect_Size_r
  )
})

overall_effect_results

# overall results

overallresults <- overall_results %>%
  left_join(overall_effect_results, by = "Domain") %>%
  mutate(
    Effect_Size_Interpretation = case_when(
      Effect_Size_r < 0.10 ~ "Negligible",
      Effect_Size_r < 0.30 ~ "Small",
      Effect_Size_r < 0.50 ~ "Medium",
      TRUE ~ "Large"
    ),
    P_Value_Display = format.pval(
      P_Value,
      digits = 4,
      eps = 1e-20
    )
  ) %>%
  select(
    Domain,
    N,
    Entry_Median,
    Exit_Median,
    P_Value_Display,
    Effect_Size_r,
    Effect_Size_Interpretation
  )

overallresults

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

# Subgroup_115

within_group_results <- map_df(domains, function(d){

  g0 <- wilcox.test(
    subset(data_q, Subgroup_115 == 0)[[paste0(d, "_exit")]],
    subset(data_q, Subgroup_115 == 0)[[paste0(d, "_entry")]],
    paired = TRUE
  )

  g1 <- wilcox.test(
    subset(data_q, Subgroup_115 == 1)[[paste0(d, "_exit")]],
    subset(data_q, Subgroup_115 == 1)[[paste0(d, "_entry")]],
    paired = TRUE
  )

  tibble(
    Domain = d,
    Group0_P = g0$p.value,
    Group1_P = g1$p.value
  )

})

within_group_results

# change in domains

data_q <- data_q %>%
  mutate(
    All_change = All_exit - All_entry,
    Diet_change = Diet_exit - Diet_entry,
    FRM_change = FRM_exit - FRM_entry,
    PA_change = PA_exit - PA_entry,
    FSafety_change = FSafety_exit - FSafety_entry,
    FSecurity_change = FSecurity_exit - FSecurity_entry
  )

# Compute per-question change score, then Mann-Whitney U between groups
per_question_results2 <- map_df(all_questions, function(q) {

  entry_col <- paste0(q, "_entry")
  exit_col  <- paste0(q, "_exit")

  change <- data_q[[exit_col]] - data_q[[entry_col]]

  group <- data_q$Subgroup_115

  g0_change <- change[group == 0]
  g1_change <- change[group == 1]

  test <- wilcox.test(g0_change, g1_change, exact = FALSE)

  n0 <- sum(!is.na(g0_change))
  n1 <- sum(!is.na(g1_change))
  n_total <- n0 + n1
  z <- qnorm(test$p.value / 2) * sign(test$statistic - (n0 * n1 / 2))
  r <- abs(z) / sqrt(n_total)

  tibble(
    Question               = q,
    N_Group0               = n0,
    N_Group1               = n1,
    Group0_Median_Change   = median(g0_change, na.rm = TRUE),
    Group1_Median_Change   = median(g1_change, na.rm = TRUE),
    W_Statistic            = as.numeric(test$statistic),
    P_Value                = test$p.value,
    Effect_Size_r          = r,
    Interpretation         = case_when(
      r < 0.10 ~ "Negligible",
      r < 0.30 ~ "Small",
      r < 0.50 ~ "Medium",
      TRUE     ~ "Large"
    )
  )
})

# View results
per_question_results2 %>%
  mutate(P_Value = format.pval(P_Value, digits = 4, eps = 1e-20))

# Write to file
write_csv(per_question_results2, "Reports/per_question_results2.csv")

# Between-group Mann-Whitney U per domain (on change scores)

get_domain_mwu <- function(df, domain_label) {

  change_col <- paste0(domain_label, "_change")

  g0_change <- df[[change_col]][df$Subgroup_115 == 0]
  g1_change <- df[[change_col]][df$Subgroup_115 == 1]

  test <- wilcox.test(g0_change, g1_change, exact = FALSE)

  n0      <- sum(!is.na(g0_change))
  n1      <- sum(!is.na(g1_change))
  n_total <- n0 + n1
  z       <- qnorm(test$p.value / 2) * sign(test$statistic - (n0 * n1 / 2))
  r       <- abs(z) / sqrt(n_total)

  tibble(
    Domain                 = domain_label,
    N_Group0               = n0,
    N_Group1               = n1,
    Group0_Median_Change   = median(g0_change, na.rm = TRUE),
    Group1_Median_Change   = median(g1_change, na.rm = TRUE),
    W_Statistic            = as.numeric(test$statistic),
    P_Value                = test$p.value,
    Effect_Size_r          = r,
    Interpretation         = case_when(
      r < 0.10 ~ "Negligible",
      r < 0.30 ~ "Small",
      r < 0.50 ~ "Medium",
      TRUE     ~ "Large"
    )
  )
}

# Run for each domain

domain_between_results <- map_df(
  list("All", "Diet", "FRM", "PA", "FSafety", "FSecurity"),
  ~ get_domain_mwu(data_q, .x)
)

# Display

domain_between_results %>%
  mutate(P_Value = format.pval(P_Value, digits = 4, eps = 1e-20))


