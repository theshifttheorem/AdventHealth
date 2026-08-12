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

# ANCOVA model 

ancova_results <- purrr::map_df(domains, function(d){

  model <- lm(
    as.formula(
      paste0(d, "_entry ~ ", d, "_exit + Subgroup_115")
    ),
    data = data_q
  )

  coef_row <- summary(model)$coefficients["Subgroup_115", ]

  tibble(
    Domain = d,
    Estimate = coef_row["Estimate"],
    P_Value = coef_row["Pr(>|t|)"]
  )
})

ancova_results
summary(ancova_results)

model1 <- lm(All_exit ~ All_entry + Subgroup_115, data = data_q)
model1
summary(model1)

summary(lm(All_exit ~ Subgroup_115, data = data_q))
summary(lm(All_change ~ Subgroup_115, data = data_q))
t.test(All_exit ~ Subgroup_115, data = data_q)
t.test(All_change ~ Subgroup_115, data = data_q)

model1 <- lm(All_exit ~ All_entry + Subgroup_115, data = data_q)
emmeans(model1, "Subgroup_115")

# wilcox groups comparison

subgroup_comparison <- map_df(domains, function(d){
  change_var <- paste0(d,"_change")
  test <- wilcox.test(
    as.formula(
      paste(change_var,"~ Subgroup_115")
    ),
    data = data_q
  )
  tibble(
    Domain = d,
    Group0_Median_Improvement =
      median(data_q[[change_var]][data_q$Subgroup_115==0],
             na.rm=TRUE),
    Group1_Median_Improvement =
      median(data_q[[change_var]][data_q$Subgroup_115==1],
             na.rm=TRUE),
    P_Value = test$p.value
  )
})
subgroup_comparison



