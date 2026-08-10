# load libraries

library(tidyverse)
library(readr)

# load data

adult     <- read_csv("data/Adult.csv")
adult_q   <- read_csv("data/Adult Questionnaire.csv")
subgroups <- read_csv("data/AdultSubgroups.csv")

# DESCRIPTIVE DIAGNOSTICS

# status distribution in full adult dataset
adult %>%
  count(Status, sort = FALSE) %>%
  mutate(
    Reason = case_when(
      Status == 0  ~ "Active",
      Status == 1  ~ "Ed. objectives met (Graduated)",
      Status == 2  ~ "Returned to school",
      Status == 3  ~ "Took a job",
      Status == 4  ~ "Family Concerns",
      Status == 5  ~ "Staff vacancy",
      Status == 6  ~ "Moved",
      Status == 7  ~ "Lost interest",
      Status == 8  ~ "Other",
      Status == 9  ~ "Other Obligations",
      Status == 10 ~ "Lost contact with client"
    ),
    Pct = round(n / sum(n) * 100, 1)
  ) %>%
  select(Status, Reason, n, Pct)

# questionnaire entry vs exit split
adult_q %>%
  count(ExitQuestionnaire) %>%
  mutate(
    Label = ifelse(ExitQuestionnaire == 0, "Entry", "Exit"),
    Pct   = round(n / sum(n) * 100, 1)
  ) %>%
  select(ExitQuestionnaire, Label, n, Pct)

# unique adult counts across files
cat("Unique Adults in adult:    ", n_distinct(adult$Adult_ID),   "\n")
cat("Unique Adults in adult_q:  ", n_distinct(adult_q$Adult_ID), "\n")

# entry/exit pairing check
paired_check <- adult_q %>%
  group_by(Adult_ID) %>%
  summarise(
    n_entry  = sum(ExitQuestionnaire == 0),
    n_exit   = sum(ExitQuestionnaire == 1),
    has_both = n_entry >= 1 & n_exit >= 1,
    .groups  = "drop"
  )

cat("\nAdults with both entry & exit:", sum(paired_check$has_both),                              "\n")
cat("Adults with entry only:       ", sum(paired_check$n_entry > 0 & paired_check$n_exit == 0), "\n")
cat("Adults with exit only:        ", sum(paired_check$n_entry == 0 & paired_check$n_exit > 0), "\n")

# paired graduates count
graduated_ids <- adult %>%
  filter(Status == 1) %>%
  pull(Adult_ID)

paired_graduates_check <- paired_check %>%
  filter(has_both, Adult_ID %in% graduated_ids)

cat("\nPaired graduates available for analysis:", nrow(paired_graduates_check), "\n")

# missing values in Q01:Q30
adult_q %>%
  select(Q01:Q30) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Question", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  print()

# subgroup 115 name and type
subgroups %>%
  filter(Subgroup_ID == 115) %>%
  count(Subgroup_Name, Subgroup_Type)


# DATA PREPARATION

# separate entry questionnaires for graduates
entry <- adult_q %>%
  filter(Adult_ID %in% graduated_ids, ExitQuestionnaire == 0) %>%
  select(Adult_ID, Q01:Q30) %>%
  rename_with(~ paste0(.x, "_entry"), Q01:Q30)

# separate exit questionnaires for graduates
exit <- adult_q %>%
  filter(Adult_ID %in% graduated_ids, ExitQuestionnaire == 1) %>%
  select(Adult_ID, Q01:Q30) %>%
  rename_with(~ paste0(.x, "_exit"), Q01:Q30)

# build paired dataset
paired <- entry %>%
  inner_join(exit, by = "Adult_ID")

cat("Participants in paired analysis:", nrow(paired), "\n")

# add subgroup 115 flag to paired dataset
subgroup_115_ids <- subgroups %>%
  filter(Subgroup_ID == 115) %>%
  pull(Adult_ID)

paired <- paired %>%
  mutate(Subgroup_115 = ifelse(Adult_ID %in% subgroup_115_ids, 1, 0))

# subgroup 115 split counts
cat("\nAdults in Subgroup 115:              ", length(subgroup_115_ids),          "\n")
cat("Paired graduates in Subgroup 115:    ", sum(paired$Subgroup_115 == 1),      "\n")
cat("Paired graduates NOT in Subgroup 115:", sum(paired$Subgroup_115 == 0),      "\n")

paired %>%
  count(Subgroup_115) %>%
  mutate(
    Label = ifelse(Subgroup_115 == 1, "In Subgroup 115", "Not in Subgroup 115"),
    Pct   = round(n / sum(n) * 100, 1)
  ) %>%
  select(Subgroup_115, Label, n, Pct)