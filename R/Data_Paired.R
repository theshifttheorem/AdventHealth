# load libraries

library(tidyverse)
library(readr)

# set column types

adult_col_types <- cols(
  Region_ID                          = col_character(),
  Region_Name                        = col_character(),
  Adult_ID                           = col_character(),
  Adult_Custom_ID                    = col_character(),
  Staff_ID                           = col_character(),
  Staff_Name                         = col_character(),
  City                               = col_character(),
  Enrollment_Date                    = col_date(format = "%m/%d/%Y"),
  State                              = col_character(),
  Zip                                = col_character(),
  Age                                = col_integer(),
  Sex                                = col_character(),
  Is_Pregnant                        = col_logical(),
  Is_Nursing                         = col_logical(),
  Ethnicity_Code                     = col_integer(),
  Race_Code_Binary                   = col_integer(),
  Sub_Race                           = col_integer(),
  Highest_Grade                      = col_integer(),
  Residence                          = col_integer(),
  Household_Income                   = col_character(),
  Children_Ages                      = col_character(),
  nChildren                          = col_integer(),
  Others_Household                   = col_integer(),
  Total_Household                    = col_integer(),
  Lesson_Type                        = col_integer(),
  Number_Recalls                     = col_integer(),
  ID_Recall_Entry                    = col_character(),
  ID_Recall_Exit                     = col_character(),
  Number_Questionnaires              = col_integer(),
  ID_Questionnaire_Entry             = col_character(),
  ID_Questionnaire_Exit              = col_character(),
  ID_Additional_Questionnaire_Entry  = col_character(),
  ID_Additional_Questionnaire_Exit   = col_character(),
  Additional_Set_ID                  = col_character(),
  Status                             = col_integer(),
  Exit_Date                          = col_date(format = "%m/%d/%Y"),
  nLessons                           = col_integer(),
  nSessions                          = col_integer(),
  nHours                             = col_double(),
  PubAsstHelp                        = col_logical(),
  Address_Status                     = col_integer(),
  County_Name                        = col_character(),
  County_FIPS                        = col_character(),
  Congressional_District             = col_character(),
  CBSA                               = col_character(),
  LastMod                            = col_date(format = "%m/%d/%Y"),
  Date_Created                       = col_date(format = "%m/%d/%Y"),
  Creator_ID                         = col_character(),
  Low_Recruit                        = col_integer(),
  Med_Edu_Minutes                    = col_integer(),
  High_Edu_Minutes                   = col_integer(),
  secondary_staff                    = col_character(),
  lessons_sync_in_person             = col_integer(),
  lessons_sync_via_tech              = col_integer(),
  lessons_async_recorded             = col_integer(),
  lessons_async_self_guided          = col_integer(),
  hours_sync_in_person               = col_double(),
  hours_sync_via_tech                = col_double(),
  hours_async_recorded               = col_double(),
  hours_async_self_guided            = col_double()
)

questionnaire_col_types <- cols(
  Region_ID         = col_character(),
  Region_Name       = col_character(),
  Adult_ID          = col_character(),
  Adult_Custom_ID   = col_character(),
  Questionnaire_ID  = col_character(),
  Checklist_Date    = col_date(),
  ExitQuestionnaire = col_logical(),
  Q01               = col_integer(),
  Q02               = col_integer(),
  Q03               = col_integer(),
  Q04               = col_integer(),
  Q05               = col_integer(),
  Q06               = col_integer(),
  Q07               = col_integer(),
  Q08               = col_integer(),
  Q09               = col_integer(),
  Q10               = col_integer(),
  Q11               = col_integer(),
  Q12               = col_integer(),
  Q13               = col_integer(),
  Q14               = col_integer(),
  Q15               = col_integer(),
  Q16               = col_integer(),
  Q17               = col_integer(),
  Q18               = col_integer(),
  Q19               = col_integer(),
  Q20               = col_integer(),
  Q21               = col_integer(),
  Q22               = col_integer(),
  Q23               = col_integer(),
  Q24               = col_integer(),
  Q25               = col_integer(),
  Q26               = col_integer(),
  Q27               = col_integer(),
  Q28               = col_integer(),
  Q29               = col_integer(),
  Q30               = col_integer(),
  Date_Created      = col_date()
)

subgroup_col_types <- cols(
  Region_ID       = col_character(),
  Region_Name     = col_character(),
  Adult_ID        = col_character(),
  Adult_Custom_ID = col_character(),
  Subgroup_ID     = col_character(),
  Subgroup_Name   = col_character(),
  Subgroup_Type   = col_character()
)

pubasst_col_types <- cols(
  Region_ID      = col_integer(),
  Region_Name    = col_character(),
  Adult_ID       = col_character(),
  Adult_Custom_ID = col_character(),
  PubAsstID      = col_integer(),
  PubAsstProg    = col_character(),
  FoodAsst       = col_integer(),
  PubAsstLevel   = col_character(),
  PubAsstType    = col_character()
)

# load fy25 files
adult_fy25     <- read_csv("data/Adult FY25.csv", col_types = adult_col_types)
adult_q_fy25   <- read_csv("data/Adult Questionnaire FY25.csv", col_types = questionnaire_col_types)
subgroups_fy25 <- read_csv("data/AdultSubgroups FY25.csv", col_types = subgroup_col_types)
pubasst_fy25 <- read_csv("data/AdultPublicAssistance FY25.csv", col_types = pubasst_col_types)

# load fy26 files
adult_fy26     <- read_csv("data/Adult FY26.csv", col_types = adult_col_types)
adult_q_fy26   <- read_csv("data/Adult Questionnaire FY26.csv", col_types = questionnaire_col_types)
subgroups_fy26 <- read_csv("data/AdultSubgroups FY26.csv", col_types = subgroup_col_types)
pubasst_fy26 <- read_csv("data/AdultPublicAssistance FY26.csv", col_types = pubasst_col_types)

#problems(adult_fy25)

iwalk(
  list(
    adult_fy25     = adult_fy25,
    adult_q_fy25   = adult_q_fy25,
    subgroups_fy25 = subgroups_fy25,
    adult_fy26     = adult_fy26,
    adult_q_fy26   = adult_q_fy26,
    subgroups_fy26 = subgroups_fy26
  ),
  ~ { cat("\n---", .y, "---\n"); print(sapply(.x, class)) }
)

# get all adult ids in fy26
fy26_ids <- adult_fy26 %>% pull(Adult_ID)

# check how many fy25 adults are duplicated in fy26
cat("FY25 adults total:                     ", nrow(adult_fy25), "\n")
cat("FY25 adults also appearing in FY26:    ", sum(adult_fy25$Adult_ID %in% fy26_ids), "\n")
cat("FY25 adults kept after deduplication:  ", sum(!adult_fy25$Adult_ID %in% fy26_ids), "\n")

# remove duplicated ids from fy25
adult_fy25_clean     <- adult_fy25     %>% filter(!Adult_ID %in% fy26_ids)
adult_q_fy25_clean   <- adult_q_fy25   %>% filter(!Adult_ID %in% fy26_ids)
subgroups_fy25_clean <- subgroups_fy25 %>% filter(!Adult_ID %in% fy26_ids)

# combine adult files
adult_combined     <- bind_rows(adult_fy25_clean, adult_fy26)
adult_q_combined   <- bind_rows(adult_q_fy25_clean, adult_q_fy26)
subgroups_combined <- bind_rows(subgroups_fy25_clean, subgroups_fy26)
  
# new columns for children ages
adult_combined <- adult_combined %>%
  mutate(child_ages_vec = map(
    str_split(Children_Ages, ","),
    ~ suppressWarnings(as.numeric(trimws(.x))) %>% na.omit()
  )) %>%
  rowwise() %>%
  mutate(
    child_0_5   = if_else(any(child_ages_vec <= 5),  1L, 0L),
    child_6_10  = if_else(any(child_ages_vec >= 6 & child_ages_vec <= 10), 1L, 0L),
    child_11_15 = if_else(any(child_ages_vec >= 11 & child_ages_vec <= 15), 1L, 0L),
    child_16_19 = if_else(any(child_ages_vec >= 16 & child_ages_vec <= 19), 1L, 0L)
  ) %>%
  ungroup() %>%
  select(-child_ages_vec)

# Add YAWC (Young Adult With Children) flag
adult_combined <- adult_combined %>% 
  mutate(YAWC = if_else(Age <= 34 & nChildren == 0, 1L, 0L))

# change 0 to NA for Age & highestgrade
adult_combined  <- adult_combined  %>% mutate(
  Age          = na_if(Age, 0),
  Highest_Grade = na_if(Highest_Grade, 0)
)

# change N/S to NA
adult_combined <- adult_combined %>%
  rename(Monthly_Household_Income = Household_Income) %>%
  mutate(
    Monthly_Household_Income = na_if(Monthly_Household_Income, "N/S")
  )

# clean values and remove column
adult_combined <- adult_combined %>%
  # drop unnecessary columns
  select(-c(Adult_Custom_ID,Children_Ages,ID_Recall_Entry,ID_Recall_Exit,ID_Questionnaire_Entry,
      ID_Questionnaire_Exit,ID_Additional_Questionnaire_Entry,ID_Additional_Questionnaire_Exit,
      nSessions,nHours,County_Name,County_FIPS,Congressional_District,CBSA,Med_Edu_Minutes,
      High_Edu_Minutes,secondary_staff
    )) %>%
  # strip spaces, commas, punctuation from all character columns
  mutate(across(where(is.character), ~ gsub("[[:space:][:punct:]]", "", .))) %>%
  # turn the cleaned household income into a numeric
  mutate(Monthly_Household_Income = as.numeric(Monthly_Household_Income))

# https://aspe.hhs.gov/sites/default/files/documents/b1bfa16b20ae9b89d525bc35de7c1643/detailed-guidelines-2026.pdf
# add federal poverty level column
fpl <- data.frame(
  Total_Household = 1:14,
  MonthlyFPL = c(
    1330, 1803, 2277, 2750,
    3223, 3697, 4170, 4643,
    5117, 5590, 6063, 6537, 
    7010, 7483
  )
)

adult_combined <- adult_combined %>%
  left_join(fpl, by = "Total_Household") %>%
  mutate(
    Monthly_Household_Income = Monthly_Household_Income
  ) %>%
  mutate(
    FPL_Percent = Monthly_Household_Income / MonthlyFPL * 100,
    fpl_185 = as.integer(FPL_Percent >= 185)
  ) %>%
  select(-MonthlyFPL)

# status distribution
adult_combined %>%
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

#cat("\nCombined adult records:               ", nrow(adult_combined),     "\n")
#cat("Combined questionnaire records:       ", nrow(adult_q_combined),   "\n")
#cat("Combined subgroup records:            ", nrow(subgroups_combined),  "\n")

# questionnaire entry vs exit split
adult_q_combined %>%
  count(ExitQuestionnaire) %>%
  mutate(
    Label = ifelse(ExitQuestionnaire == 0, "Entry", "Exit"),
    Pct   = round(n / sum(n) * 100, 1)
  ) %>%
  select(ExitQuestionnaire, Label, n, Pct)

# unique adult counts
#cat("\nUnique Adults in adult_combined:    ", n_distinct(adult_combined$Adult_ID),   "\n")
#cat("Unique Adults in adult_q_combined:  ", n_distinct(adult_q_combined$Adult_ID), "\n")

# entry/exit pairing check
paired_check <- adult_q_combined %>%
  group_by(Adult_ID) %>%
  summarise(
    n_entry  = sum(ExitQuestionnaire == 0),
    n_exit   = sum(ExitQuestionnaire == 1),
    has_both = n_entry >= 1 & n_exit >= 1,
    .groups  = "drop"
  )

#cat("\nAdults with both entry & exit:  ", sum(paired_check$has_both),                                    "\n")
#cat("Adults with entry only:         ", sum(paired_check$n_entry > 0 & paired_check$n_exit == 0),       "\n")
#cat("Adults with exit only:          ", sum(paired_check$n_entry == 0 & paired_check$n_exit > 0),       "\n")

# paired graduates count
graduated_ids <- adult_combined %>%
  filter(Status == 1) %>%
  pull(Adult_ID)

paired_graduates_check <- paired_check %>%
  filter(has_both, Adult_ID %in% graduated_ids)

cat("\nPaired graduates available for analysis:", nrow(paired_graduates_check), "\n")

# missing values in Q01:Q30
adult_q_combined %>%
  select(Q01:Q30) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Question", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  print()

# subgroup 115 - food is health
subgroups_combined %>%
  filter(Subgroup_ID == 115) %>%
  count(Subgroup_Name, Subgroup_Type)

# separate entry questionnaires for graduates
entry <- adult_q_combined %>%
  filter(ExitQuestionnaire == 0) %>%
  select(Adult_ID, Q01:Q30) %>%
  rename_with(~ paste0(.x, "_entry"), Q01:Q30)

# separate exit questionnaires for graduates
exit <- adult_q_combined %>%
  filter(ExitQuestionnaire == 1) %>%
  select(Adult_ID, Q01:Q30) %>%
  rename_with(~ paste0(.x, "_exit"), Q01:Q30)

# build paired dataset with entry and exit
paired <- entry %>%
  inner_join(exit, by = "Adult_ID")

# add subgroup 115 binary flag
subgroup_115_ids <- subgroups_combined %>%
  filter(Subgroup_ID == 115) %>%
  pull(Adult_ID)

# join adult demographics + wide questionnaire + subgroup 115 flag
adult_paired <- adult_combined %>%
  inner_join(paired, by = "Adult_ID") %>%
  mutate(Subgroup_115 = ifelse(Adult_ID %in% subgroup_115_ids, 1, 0))

# combine & cleanpubasst fy25 & fy26
pubasst_clean <- bind_rows(pubasst_fy25, pubasst_fy26) %>%
  select(Adult_ID, PubAsstProg) %>% 
  mutate(
    Adult_ID   = as.character(Adult_ID),
    PubAsstProg = str_replace_all(
      as.character(PubAsstProg),
      "[^[:alnum:]]",
      "_"
    ),
    PubAsstProg = str_replace_all(PubAsstProg, "_+", "_")
  ) %>%
  distinct(Adult_ID, PubAsstProg, .keep_all = TRUE)

# pivot pubasst to wide columns
pubasst_wide <- pubasst_clean %>%
  mutate(val = 1L) %>%
  pivot_wider(
    names_from  = PubAsstProg,
    values_from = val,
    values_fill = list(val = 0L),
    names_glue  = "has_{PubAsstProg}"
  ) %>% 
  # overall flag: any program present?
  mutate(
    has_pubasst = if_else(rowSums(select(., -Adult_ID)) > 0,
                         1L, 0L)
  )

# merge pubasst into adult_paired
adult_paired <- adult_paired %>%
  left_join(pubasst_wide, by = "Adult_ID") %>%
  mutate(across(
    .cols    = starts_with("has_") & !all_of("has_pubasst"),
    .fns     = ~replace_na(., 0L)
  )) %>%
  mutate(has_pubasst = replace_na(has_pubasst, 0L))


# save single combined wide file
write_csv(adult_paired, "data/Adult_Paired.csv")


# confirm final file
cat("\nFinal combined file rows:    ", nrow(adult_paired),                    "\n")
cat("Final combined file columns: ", ncol(adult_paired),                    "\n")
cat("Adults in Subgroup 115:      ", sum(adult_paired$Subgroup_115 == 1),   "\n")
cat("Adults NOT in Subgroup 115:  ", sum(adult_paired$Subgroup_115 == 0),   "\n")

# print columns & classes
col_classes <- sapply(adult_paired_clean, class)   # one‑element vector per column

cols_tbl <- tibble(
  column = names(col_classes),
  class  = unlist(col_classes)
)

write_csv(cols_tbl, "data/Adult_Paired_column_info.csv")

# glimpse(adult_paired_clean)