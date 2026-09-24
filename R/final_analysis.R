# Load Packages

library(tidyverse)
library(purrr)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(ggplot2)
library(car)   
library(DescTools)  
library(dplyr)
library(stringr)

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


# Compute domain‑level means for each participant

domain_means <- data_q %>% 
  rowwise() %>%                           
  mutate(

    ## All
    All_entry_mean = mean(
      c_across(all_of(paste0(all_questions, "_entry")))[
        !is.na(c_across(all_of(paste0(all_questions, "_exit"))))
      ],
      na.rm = TRUE
    ),
    All_exit_mean  = mean(
      c_across(all_of(paste0(all_questions, "_exit")))[
        !is.na(c_across(all_of(paste0(all_questions, "_entry"))))
      ],
      na.rm = TRUE
    ),

    ## Diet 
    Diet_entry_mean = mean(
      c_across(all_of(paste0(diet_questions, "_entry")))[
        !is.na(c_across(all_of(paste0(diet_questions, "_exit"))))
      ],
      na.rm = TRUE
    ),
    Diet_exit_mean  = mean(
      c_across(all_of(paste0(diet_questions, "_exit")))[
        !is.na(c_across(all_of(paste0(diet_questions, "_entry"))))
      ],
      na.rm = TRUE
    ),

    ## FRM
    FRM_entry_mean = mean(
      c_across(all_of(paste0(frm_questions, "_entry")))[
        !is.na(c_across(all_of(paste0(frm_questions, "_exit"))))
      ],
      na.rm = TRUE
    ),
    FRM_exit_mean  = mean(
      c_across(all_of(paste0(frm_questions, "_exit")))[
        !is.na(c_across(all_of(paste0(frm_questions, "_entry"))))
      ],
      na.rm = TRUE
    ),

    ## PA 
    PA_entry_mean = mean(
      c_across(all_of(paste0(pa_questions, "_entry")))[
        !is.na(c_across(all_of(paste0(pa_questions, "_exit"))))
      ],
      na.rm = TRUE
    ),
    PA_exit_mean  = mean(
      c_across(all_of(paste0(pa_questions, "_exit")))[
        !is.na(c_across(all_of(paste0(pa_questions, "_entry"))))
      ],
      na.rm = TRUE
    ),

    ## FSafety
    FSafety_entry_mean = mean(
      c_across(all_of(paste0(fs_questions, "_entry")))[
        !is.na(c_across(all_of(paste0(fs_questions, "_exit"))))
      ],
      na.rm = TRUE
    ),
    FSafety_exit_mean  = mean(
      c_across(all_of(paste0(fs_questions, "_exit")))[
        !is.na(c_across(all_of(paste0(fs_questions, "_entry"))))
      ],
      na.rm = TRUE
    ),

    ## FSecurity
    FSecurity_entry_mean = mean(
      c_across(all_of(paste0(fsec_questions, "_entry")))[
        !is.na(c_across(all_of(paste0(fsec_questions, "_exit"))))
      ],
      na.rm = TRUE
    ),
    FSecurity_exit_mean  = mean(
      c_across(all_of(paste0(fsec_questions, "_exit")))[
        !is.na(c_across(all_of(paste0(fsec_questions, "_entry"))))
      ],
      na.rm = TRUE
    )
  ) %>% 
  ungroup()

# Arrange the final CSV

final_table <- domain_means %>% 
  select(
    Adult_ID,
    Subgroup_115,

    All_entry_mean,  All_exit_mean,
    Diet_entry_mean, Diet_exit_mean,
    FRM_entry_mean,  FRM_exit_mean,
    PA_entry_mean,   PA_exit_mean,
    FSafety_entry_mean, FSafety_exit_mean,
    FSecurity_entry_mean, FSecurity_exit_mean
  )

write_csv(final_table, "data/adult_domain_means.csv")

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

#write_csv(overall_results,        "Reports/overall_results.csv")
#write_csv(within_group_results,   "Reports/within_group_results.csv")
#write_csv(domain_between_results, "Reports/domain_between_results.csv")
#write_csv(per_question_results,   "Reports/per_question_results.csv")

#plot q-q
for (d in domains) {
  chg <- data_q[[paste0(d, "_change")]][!is.na(data_q[[paste0(d, "_change")]])]
  p  <- ggplot(data.frame(chg), aes(x = chg)) +
        geom_histogram(binwidth = diff(range(chg))/30,
                       fill      = "steelblue",
                       color     = "white") +
        geom_density(fill = "orange", alpha = 0.4) +
        labs(title   = paste("Histogram & density –", d),
             x       = "Change",
             y       = "Density") +
        theme_minimal()
  print(p)
}

# Check Normality

cat("\n\nAll:      Shapiro p =", shapiro.test(data_q$All_change)$p.value, 
    " | Skew =", skewness(data_q$All_change, na.rm = TRUE), "\n")
cat("Diet:      Shapiro p =", shapiro.test(data_q$Diet_change)$p.value, 
    " | Skew =", skewness(data_q$All_change, na.rm = TRUE), "\n")
cat("FRM:      Shapiro p =", shapiro.test(data_q$FRM_change)$p.value, 
    " | Skew =", skewness(data_q$All_change, na.rm = TRUE), "\n")
cat("PA:       Shapiro p =", shapiro.test(data_q$PA_change)$p.value,
    " | Skew =", skewness(data_q$PA_change, na.rm = TRUE), "\n")
cat("FSafety:  Shapiro p =", shapiro.test(data_q$FSafety_change)$p.value,
    " | Skew =", skewness(data_q$FSafety_change, na.rm = TRUE), "\n")
cat("FSecurity: Shapiro p =", shapiro.test(data_q$FSecurity_change)$p.value,
    " | Skew =", skewness(data_q$FSecurity_change, na.rm = TRUE), "\n\n")

# Visual check
par(mfrow = c(3, 3))
hist(data_q$All_change, main = "All")
hist(data_q$Diet_change, main = "Diet")
hist(data_q$FRM_change, main = "FRM")
hist(data_q$PA_change, main = "PA")
hist(data_q$FSafety_change, main = "FSafety")
hist(data_q$FSecurity_change, main = "FSecurity")
par(mfrow = c(1, 1))


# Non-normal domains: All, PA, FSafety, FSecurity

# For RIGHT-SKEWED data (positive skew): use LOG or SQRT
# For LEFT-SKEWED data (negative skew): use SQUARE or RECIPROCAL

# Add transformed variables to your data
data_q <- data_q %>%
  mutate(
    # Shift to positive if needed (add minimum + 0.1)
    All_shift = All_change + abs(min(All_change, na.rm = TRUE)) + 0.1,
    PA_shift = PA_change + abs(min(PA_change, na.rm = TRUE)) + 0.1,
    FSafety_shift = FSafety_change + abs(min(FSafety_change, na.rm = TRUE)) + 0.1,
    
    # Apply transformations
    All_trans = log(All_shift),
    PA_trans = log(PA_shift),
    FSafety_trans = log(FSafety_shift),
    FSecurity_trans = FSecurity_change^2
  )

# After transformation
cat("AFTER TRANSFORMATION:\n")
cat("All:      Shapiro p =", shapiro.test(data_q$All_trans)$p.value,
    " | Skew =", skewness(data_q$All_trans, na.rm = TRUE), "\n")
cat("PA:       Shapiro p =", shapiro.test(data_q$PA_trans)$p.value,
    " | Skew =", skewness(data_q$PA_trans, na.rm = TRUE), "\n")
cat("FSafety:  Shapiro p =", shapiro.test(data_q$FSafety_trans)$p.value,
    " | Skew =", skewness(data_q$FSafety_trans, na.rm = TRUE), "\n")
cat("FSecurity: Shapiro p =", shapiro.test(data_q$FSecurity_trans)$p.value,
    " | Skew =", skewness(data_q$FSecurity_trans, na.rm = TRUE), "\n")

# After transformation visual check
par(mfrow = c(2, 2))
hist(data_q$All_trans, main = "All (log)")
hist(data_q$PA_trans, main = "PA (log)")
hist(data_q$FSafety_trans, main = "FSafety (log)")
hist(data_q$FSecurity_trans, main = "FSecurity (square)")
par(mfrow = c(1, 1))

# Linear Mixed effects model

#    Model: Score ~ Time * Subgroup + (1 | Adult_ID)
#    Key term: Time:Subgroup interaction — tests whether change over time
#    differs between intervention (Group 1) and control (Group 0)
#    Fit separately per domain, then per question

# Per Domain 

lmm_domain_results <- map_df(domains, function(d) {

  data_long <- data_q %>%
    select(Adult_ID, Subgroup_115,
           entry = all_of(paste0(d, "_entry")),
           exit  = all_of(paste0(d, "_exit"))) %>%
    pivot_longer(
      cols      = c(entry, exit),
      names_to  = "Time",
      values_to = "Score"
    ) %>%
    mutate(
      Time     = ifelse(Time == "entry", 0, 1),
      Subgroup = factor(Subgroup_115, levels = c(0, 1))
    ) %>%
    filter(!is.na(Score))

  model <- lmer(Score ~ Time * Subgroup + (1 | Adult_ID),
                data = data_long, REML = TRUE)

  tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(Domain = d)
}) %>%
  mutate(
    Term_Label = recode(term,
      "(Intercept)"    = "Intercept (Group 0, Entry)",
      "Time"           = "Time Effect (Group 0)",
      "Subgroup1"      = "Group 1 vs Group 0 (at Entry)",
      "Time:Subgroup1" = "Time x Subgroup (Interaction)"
    ),
    Reject_H0 = p.value < 0.05
  ) %>%
  select(Domain, Term_Label, estimate, std.error, statistic, df, p.value,
         conf.low, conf.high, Reject_H0)

lmm_domain_results

# Per Question

lmm_question_results <- map_df(all_questions, function(q) {

  data_long <- data_q %>%
    select(Adult_ID, Subgroup_115,
           entry = all_of(paste0(q, "_entry")),
           exit  = all_of(paste0(q, "_exit"))) %>%
    pivot_longer(
      cols      = c(entry, exit),
      names_to  = "Time",
      values_to = "Score"
    ) %>%
    mutate(
      Time     = ifelse(Time == "entry", 0, 1),
      Subgroup = factor(Subgroup_115, levels = c(0, 1))
    ) %>%
    filter(!is.na(Score))

  model <- lmer(Score ~ Time * Subgroup + (1 | Adult_ID),
                data = data_long, REML = TRUE)

  tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(Question = q)
}) %>%
  mutate(
    Term_Label = recode(term,
      "(Intercept)"    = "Intercept (Group 0, Entry)",
      "Time"           = "Time Effect (Group 0)",
      "Subgroup1"      = "Group 1 vs Group 0 (at Entry)",
      "Time:Subgroup1" = "Time x Subgroup (Interaction)"
    ),
    Reject_H0 = p.value < 0.05
  ) %>%
  select(Question, Term_Label, estimate, std.error, statistic, df, p.value,
         conf.low, conf.high, Reject_H0)

lmm_question_results

# WRITE OUTPUTS FOR LINEAR FIXED EFFECTS MODEL

#write_csv(lmm_domain_results,     "Reports/lmm_domain_results.csv")
#write_csv(lmm_question_results,   "Reports/lmm_question_results.csv")