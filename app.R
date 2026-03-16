# app.R

# SET-UP ----------------
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(tidyverse)
library(rio)
library(here)
library(janitor)
library(plotly)
library(shinyWidgets)
library(xml2)
library(robumeta)

# IMPORT -----------------

## Depression Prevention Data --------------------

# Import meta-analytic data
# Get sheet names
sheets <- readxl::excel_sheets(here("data","Depression_Overview_Meta_Analysis_Data.xlsx"))
# Import all sheets into one df
dpo_ma_raw <- rio::import_list(here::here("data","Depression_Overview_Meta_Analysis_Data.xlsx"), which = sheets[-1], rbind=TRUE) %>%
  clean_names()

# Import study level characteristics with refid
dpo_raw <- import(here("data", "Depression_Overview_Study_Level_Distiller.xlsx")) %>% 
  clean_names()

# Import group level data
dpo_group_raw <- import(here("data", "Depression_Overview_Primary_Study_Data.xlsx"), sheet = "group_level") 

# Extract FM and DistillerSR IDs
dpo_ids <- dpo_raw %>% 
  select(refid, fm_substudy_id)

## Anxiety Prevention Data -----------------------

# Import Anxiety study level data
apo_raw <- import(here("data", "APO_study_level.xlsx")) %>% 
  janitor::clean_names()

# Import Anxiety group level data
apo_group_raw <- import(here("data", "Anxiety", "APO_group_level.xlsx")) %>% 
  janitor::clean_names() %>% 
  distinct(across(-user), .keep_all = TRUE)

# Import Anxiety MA data

d_anxiety     <- import(here("data", "Anxiety", "Anxiety_Symptoms.xlsx"))
d_depression  <- import(here("data", "Anxiety", "Depression_Symptoms.xlsx"))
d_wellbeing   <- import(here("data", "Anxiety", "Well_being.xlsx"))
d_adiagnosis  <- import(here("data", "Anxiety", "Anxiety_diagnosis.xlsx"))
d_educational <- import(here("data", "Anxiety", "Educational_achievement.xlsx"))
d_suicidal    <- import(here("data", "Anxiety", "Suicidal_ideation.xlsx"))

dfs <- list(
  d_adiagnosis,
  d_anxiety,
  d_depression,
  d_educational,
  d_suicidal,
  d_wellbeing
)

names(dfs) <- c("diagnosis","anxiety","depression","educational","suicidal","wellbeing")


# Remove TAU_m and school_mod variables (if present), add source, and bind--#
apo_ma_raw <- imap_dfr(
  dfs,
  ~ .x %>%
    select(-any_of(c("TAU_m","school_mod"))) %>%  #Remove vars if present
    mutate(source = .y)                           #Add source column
)

# BIND PROJECT FILES -----------------------------------------------------------
## Meta-Analytic Files ----------------------------------
### Helper Functions ------------------------

# Function to clean up effect size keys (string leading index and re-code HTML)
clean_es_key <- function(x) {
  x %>% 
    str_remove("^[0-9]+-") %>% 
    map_chr(~ xml_text(read_html(paste0("<x>", .x, "</x>"))))
}

### Transform MA files for bind --------------

# Subset Depression file for merge
dpo_ma_subset <- dpo_ma_raw %>% 
  transmute(
    refid,
    study,       
    yi, vi,
    intervention, comparison,
    across(starts_with("outcome")),
    #across(ends_with("_n")),
    across(ends_with("_mean")),
    across(ends_with("_sd")),
    file, #intervention_distiller,
    high_rob, percent_white, secondary_school, #baseline_depression,
    public_school, united_states, primary_prevention, tau_comparator,
    publication_year, percent_female,
    project = "DPO"
  ) %>% 
  select(-outcome_metric)


# Subset Anxiety file for merge
apo_ma_subset <- apo_ma_raw %>% 
  rename(
    # core IDs / effect
    intervention = effect_intervention_group,
    comparison   = effect_comparison_group,
    
    # DPO uses outcome_* names
    outcome_domain = study_outcome_domain,
    outcome_aggregation   = effect_outcome_type,
    outcome_timepoint = effect_timing,
    
    # # DPO baseline n names
    # baseline_intervention_n = effect_intervention_baseline_participants,
    # baseline_comparison_n   = effect_comparison_baseline_participants,
    # followup_intervention_n = effect_intervention_followup_participants,
    # followup_comparison_n   = effect_comparison_followup_participants,
    
    # means
    baseline_intervention_mean = effect_intervention_baseline_mean,
    followup_intervention_mean = effect_intervention_followup_mean,
    baseline_comparison_mean   = effect_comparison_baseline_mean,
    followup_comparison_mean   = effect_comparison_followup_mean,
    
    # sds
    baseline_intervention_sd = effect_intervention_baseline_sd,
    followup_intervention_sd = effect_intervention_followup_sd,
    baseline_comparison_sd   = effect_comparison_baseline_sd,
    followup_comparison_sd   = effect_comparison_followup_sd,
    
    # moderator names to match DPO
    publication_year = year_m,
    percent_female   = female_m,
    public_school    = public_m,
    secondary_school = secondary_m,
    high_rob         = rob_high_m
    ) %>%
  mutate(
    file = source,                        
    outcome_measure = clean_es_key(effect_outcome),
    intervention = clean_es_key(intervention),
    outcome_domain = case_when(outcome_domain == "Educational achievement" ~ "Educational Achievement",
                               outcome_domain == "Suicidal ideation" ~ "Suicidal Ideation",
                               outcome_domain == "Anxiety symptoms" ~ "Anxiety Symptoms",
                               outcome_domain == "Anxiety diagnosis" ~ "Anxiety Diagnosis",
                               TRUE ~ outcome_domain),
    
    # empty columns for bind rows
    percent_white = NA_real_,
    tau_comparator = NA,                  
    primary_prevention = NA,             
    united_states = NA ,                  
    project = "APO"
  ) %>% 
  # Join in for study name %>% 
  left_join(
    apo_raw %>%
      select(refid, study = study_author_year),
    by = "refid"
  ) %>% 
  select(refid, study, yi, vi,
         intervention, comparison, outcome_domain, outcome_measure, outcome_aggregation, outcome_timepoint,
         #ends_with("_n"), 
         #study_number_participants,
         baseline_intervention_mean, baseline_comparison_mean, followup_intervention_mean, followup_comparison_mean, #ends_with("_mean"), 
         baseline_intervention_sd, baseline_comparison_sd, followup_intervention_sd, followup_comparison_sd,#ends_with("_sd"),
         file, #intervention_distiller, 
         high_rob, percent_white, secondary_school, #baseline_depression,
         public_school, united_states, primary_prevention, tau_comparator, 
         publication_year, percent_female, project)


# Bind together
combined_ma_df <- rbind(dpo_ma_subset, apo_ma_subset)

## Study Level Files -------------------------------------------------

# Clean and bind study level files
study_level <- apo_raw %>% 
  dplyr::rename(
    # school level (APO has *_school suffix)
    study_school_level_elementary              = study_school_level_elementary_school,
    study_school_level_middle                  = study_school_level_middle_school,
    study_school_level_high                    = study_school_level_high_school,
    study_school_level_only_reported_primary   = study_school_level_only_reported_primary_school,
    study_school_level_only_reported_secondary = study_school_level_only_reported_secondary_school) %>% 
  mutate(project = "APO") %>% 
  bind_rows(dpo_raw %>% mutate(project = "DPO"))



# STANDARDIZE DATA FILES FOR MERGE ---------------------------------------------

## Meta-Analytic data ----------------------------------------------

# Critical value from standard Z distribution to make a 95% CI from vi
z <- qnorm(1 - .05/2)

# Re-format meta-analytic data
combined_ma_td <- combined_ma_df %>% 
  mutate(se = sqrt(vi),
         ci_lower = yi - z*se,
         ci_upper = yi + z*se,
         # total_n = rowSums(cbind(baseline_intervention_n, baseline_comparison_n), na.rm = TRUE),
         # total_n = ifelse(total_n == 0, NA_real_, total_n)
         ) %>% 
  select(refid, study, yi, vi, se, ci_lower, ci_upper, everything())


## Study Level Data ----------------------------------------------

### Helper functions ---------------------------------
# A value means selected; NA or "" means not selected
is_on <- function(x) {
  if (is.logical(x)) !is.na(x) & x
  else if (is.character(x)) !is.na(x) & trimws(x) != ""
  else !is.na(x)
}

# Convert a suffix like "new_mexico" -> "New Mexico"
pretty_geo_label <- function(suffix){
  s <- gsub("^_+", "", suffix)
  s <- gsub("_+", " ", s)
  
  #Title Case (base R)
  s <- tools::toTitleCase(tolower(s))
  
  #Special cases (edit/add as you need)
  s <- dplyr::recode(
    s,
    "United States"  = "United States",
    "United Kingdom" = "United Kingdom",
    "Hong Kong"      = "Hong Kong",
    "New Zealand"    = "New Zealand",
    "South Africa"   = "South Africa",
    "Cannot Tell"    = "Cannot Tell",
    .default = s
  )
  
  s
}

#Collapse any checkbox set based on a column-name prefix (e.g., "study_country_")
collapse_from_prefix <- function(df, prefix){
  cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  if (length(cols) == 0) return(rep(NA_character_, nrow(df)))
  
  suffixes <- sub(paste0("^", prefix), "", cols)
  labels <- vapply(suffixes, pretty_geo_label, character(1))
  
  mat <- sapply(df[cols], is_on)
  
  apply(mat, 1, function(r){
    picked <- labels[which(r)]
    if (length(picked) == 0) NA_character_
    else paste(unique(picked), collapse = "; ")
  })
}

### Clean Study Data ---------------------------------

collapse_checks <- function(df, cols, labels){
  mat <- sapply(df[cols], is_on)
  
  apply(mat, 1, function(r){
    picked <- labels[which(r)]
    if(length(picked) == 0) NA_character_
    else paste(unique(picked), collapse = "; ")
  })
}

study_td <- study_level %>%
  mutate(
    # Grades (value/NA style). Include K if you have it (see note below).
    k8_present = if_any(all_of(paste0("study_grade_level_", 1:8)),  ~ is_on(.x)),
    hs_present = if_any(all_of(paste0("study_grade_level_", 9:12)), ~ is_on(.x)),
    grades_known = k8_present | hs_present,
    
    # School level flags
    elem_school = is_on(study_school_level_elementary),
    mid_school  = is_on(study_school_level_middle),
    high_school = is_on(study_school_level_high),
    
    # Only-reported + cannot tell
    primary_only     = is_on(study_school_level_only_reported_primary),
    secondary_only   = is_on(study_school_level_only_reported_secondary),
    school_cant_tell = is_on(study_school_level_cannot_tell),
    
    # Prevention Level
    study_prevention_level = {
      universal <- is_on(study_prevention_level_universal)
      targeted  <- is_on(study_prevention_level_targeted)
      
      dplyr::case_when(
        universal & targeted ~ "Universal; Targeted",
        universal            ~ "Universal",
        targeted             ~ "Targeted",
        TRUE                 ~ NA_character_
      )
    },
    
    # School Level
    study_school_level = {
      labs <- c(
        "Elementary School",
        "Middle School",
        "High School",
        "Only reported Primary School",
        "Only reported Secondary School",
        "Cannot Tell"
      )
      
      cols <- c(
        "study_school_level_elementary",
        "study_school_level_middle",
        "study_school_level_high",
        "study_school_level_only_reported_primary",
        "study_school_level_only_reported_secondary",
        "study_school_level_cannot_tell"
      )
      
      collapse_checks(cur_data(), cols = cols, labels = labs)
    },
    
    #School area
    study_school_area = collapse_checks(
      cur_data(),
      cols = c(
        "study_school_area_rural",
        "study_school_area_suburban",
        "study_school_area_urban",
        "study_school_area_cannot_tell"
      ),
      labels = c(
        "Rural",
        "Suburban",
        "Urban",
        "Cannot Tell"
      )
    ),
    
    #School type
    study_school_type = collapse_checks(
      cur_data(),
      cols = c(
        "study_school_type_public",
        "study_school_type_private",
        "study_school_type_parochial",
        "study_school_type_charter",
        "study_school_type_cannot_tell"
      ),
      labels = c(
        "Public",
        "Private",
        "Parochial",
        "Charter",
        "Cannot Tell"
      )
    ),
    
    # Country and State
    study_country = collapse_from_prefix(cur_data(), "study_country_"),
    study_state   = collapse_from_prefix(cur_data(), "study_state_"),
    
    # FINAL band (K-8 / 9-12 / K-12 / Not Reported)
    age_group = case_when(
      # 1) Prefer explicit grades
      grades_known & k8_present & hs_present ~ "K-12",
      grades_known & hs_present              ~ "9-12",
      grades_known & k8_present              ~ "K-8",
      
      # 2) Only-reported flags (now BEFORE Not Reported)
      !grades_known & primary_only & secondary_only ~ "K-12",
      !grades_known & primary_only                  ~ "K-8",
      !grades_known & secondary_only                ~ "9-12",
      
      # 3) Regular school-level flags
      !grades_known & high_school & (elem_school | mid_school) ~ "K-12",
      !grades_known & (elem_school | mid_school) & !high_school ~ "K-8",
      !grades_known & high_school & !(elem_school | mid_school) ~ "9-12",
      
      # 4) Explicit "cannot tell"
      school_cant_tell ~ "Not Reported",
      
      TRUE ~ "Not Reported"
    ),
    study_number_participants = suppressWarnings(as.numeric(study_number_participants))
  )


# GROUP LEVEL BIND & CLEAN FOR MERGE ------------------------------------------

# Helper function to clean text responses from FileMaker
clean_num_prefix <- function(x){
  x <- as.character(x)
  x <- stringr::str_trim(x)
  
  vapply(x, function(s){
    
    #Missing codes
    if(is.na(s) || s == "" || s == "-999" || stringr::str_detect(s, "^\\s*-999\\b")){
      return(NA_character_)
    }
    
    #Extract text after each code like "3. " or "3 - "
    m <- stringr::str_match_all(
      s,
      "(?:^|\\s)(\\d+)\\s*[\\.-]\\s*([^\\d]+?)(?=\\s\\d+\\s*[\\.-]|$)"
    )[[1]]
    
    if(nrow(m) > 0){
      parts <- stringr::str_trim(m[,3])
      parts <- parts[parts != ""]
      return(paste(unique(parts), collapse = "; "))
    }
    
    #Fallback: just remove a single leading code if present
    s2 <- stringr::str_trim(stringr::str_remove(s, "^\\s*\\d+\\s*[\\.-]\\s*"))
    if(s2 == "") NA_character_ else s2
    
  }, character(1))
}

# Import crosswalk to harmonize intervention names across files (MA and group level)
crosswalk_gname <- rio::import(here("data","crosswalk_dpo_groupname_match_ma.xlsx")) %>% 
  select(primary_study_id, gname, intervention_distiller) %>% 
  rename(fm_substudy_id = primary_study_id)



dpo_group_td <- dpo_group_raw %>% 
  rename(fm_substudy_id = primary_study_id) %>% 
  left_join(dpo_ids) %>% 
  left_join(crosswalk_gname, by = c("fm_substudy_id", "gname")) %>% #merge in crosswalk
  mutate(gname = dplyr::coalesce(dplyr::na_if(trimws(intervention_distiller), ""), gname)) %>% #correct names
  pivot_longer(
    cols = c(
      intervention_format,
      intervention_frequency,
      intervention_provider,
      intervention_recipients,
      intervention_location,
      comparison_type,
      comparison_format,
      comparison_frequency,
      comparison_provider,
      comparison_recipients,
      comparison_location
    ),
    names_to = "var",
    values_to = "value"
  ) %>%
  
  # Clean response text
  mutate(value = clean_num_prefix(value)) %>%
  
  # Map to final Study_* variable names
  mutate(
    study_var = case_when(
      str_detect(var, "format")                    ~ "study_group_format",
      str_detect(var, "frequency")                 ~ "study_group_frequency",
      str_detect(var, "provider")                  ~ "study_group_provider",
      str_detect(var, "recipients")                ~ "study_group_recipients",
      str_detect(var, "location")                  ~ "study_group_location",
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(!is.na(study_var)) %>%
  
  # Collapse intervention + comparison values into one field per refid
  group_by(refid, fm_substudy_id, group_number, group_type, gname, ig_group_type, study_var) %>%
  summarise(
    value = paste(unique(na.omit(value)), collapse = "; "),
    .groups = "drop"
  ) %>%
  
  pivot_wider(
    names_from = study_var,
    values_from = value
  ) %>% 
  select(refid, fm_substudy_id, group_number, group_type, gname, ig_group_type, study_group_format, study_group_frequency, 
         study_group_provider, study_group_recipients, study_group_location) #%>% 
  #filter(group_type == "1. Intervention")

# ANXIETY



apo_group_td <- apo_group_raw %>%
  transmute(
    refid,
    fm_substudy_id = NA_character_,              #APO doesn't have FM id in this file
    group_number   = as.integer(group_id),                   
    group_type     = study_group_type,              
    gname          = coalesce(study_intervention_name, study_comparison_name),
    ig_group_type  = NA_character_,           #APO doesn't have ig group type
    
    #FORMAT = collapse checkbox columns
    study_group_format = collapse_checks(
      cur_data(),
      cols = c(
        "study_group_format_self_administered",
        "study_group_format_individual",
        "study_group_format_small_group",
        "study_group_format_whole_class",
        "study_group_format_school",
        "study_group_format_cannot_tell"
      ),
      labels = c(
        "Self-Administered",
        "Individual",
        "Small Group",
        "Whole Class",
        "School",
        "Cannot Tell"
      )
    ),
    
    #FREQUENCY already exists as one variable in APO
    study_group_frequency = clean_num_prefix(study_group_frequency),
    
    #PROVIDER = collapse checkbox columns
    study_group_provider = collapse_checks(
      cur_data(),
      cols = c(
        "study_group_provider_teacher",
        "study_group_provider_counselor",
        "study_group_provider_behavioral_health_personnel",
        "study_group_provider_researcher",
        "study_group_provider_other_school_personnel",
        "study_group_provider_self_administered",
        "study_group_provider_other",
        "study_group_provider_cannot_tell"
      ),
      labels = c(
        "Teacher",
        "Counselor",
        "Behavioral Health Personnel",
        "Researcher",
        "Other School Personnel",
        "Self-Administered",
        "Other",
        "Cannot Tell"
      )
    ),
    
    #RECIPIENTS = collapse checkbox columns
    study_group_recipients = collapse_checks(
      cur_data(),
      cols = c(
        "study_group_recipients_student",
        "study_group_recipients_peers",
        "study_group_recipients_teachers",
        "study_group_recipients_staff",
        "study_group_recipients_family",
        "study_group_recipients_community",
        "study_group_recipients_cannot_tell"
      ),
      labels = c(
        "Students",
        "Peers",
        "Teachers",
        "Staff",
        "Family",
        "Community",
        "Cannot Tell"
      )
    ),
    
    #LOCATION = collapse checkbox columns
    study_group_location = collapse_checks(
      cur_data(),
      cols = c(
        "study_group_location_school_during_school_hours",
        "study_group_location_school_out_of_school_time",
        "study_group_location_home",
        "study_group_location_community",
        "study_group_location_residential_institution",
        "study_group_location_other",
        "study_group_location_cannot_tell"
      ),
      labels = c(
        "School (during school hours)",
        "School (out of school time)",
        "Home",
        "Community",
        "Residential Institution",
        "Other",
        "Cannot Tell"
      )
    )
  ) %>% 
  filter(group_type == "Intervention")

# Add rows for combined groups in MA file (anxiety)

## Helper function to collapse responses while removing duplicates
collapse_union <- function(x){
  vals <- unlist(strsplit(na.omit(x), ";\\s*"))
  vals <- trimws(vals)
  vals <- vals[vals != ""]
  if(length(vals) == 0) NA_character_
  else paste(sort(unique(vals)), collapse = "; ")
}

## Create combined groups for Lessons for Living (refid 10114)
apo_lfl_combined <- apo_group_td %>%
  filter(
    refid == 10114,
    gname %in% c(
      "Lessons for Living: Think well, do well (Teacher-led)",
      "Lessons for Living: Think well, do well (Psychologist-led)"
    )
  ) %>%
  summarise(
    refid = first(refid),
    gname = "Lessons for Living: Think well, do well (Psychologist and Teacher led)",
    group_type = "Intervention",
    
    study_group_format     = collapse_union(study_group_format),
    study_group_frequency  = collapse_union(study_group_frequency),
    study_group_provider   = collapse_union(study_group_provider),
    study_group_recipients = collapse_union(study_group_recipients),
    study_group_location   = collapse_union(study_group_location)
  )

# FRIENDS (refid 10129)
apo_friends_combined <- apo_group_td %>%
  filter(
    refid == 10129,
    gname %in% c(
      "FRIENDS - teacher led",
      "FRIENDS - psychologist led"
    )
  ) %>%
  summarise(
    refid = first(refid),
    gname = "FRIENDS - Psychologist and Teacher led",
    group_type = "Intervention",
    
    study_group_format     = collapse_union(study_group_format),
    study_group_frequency  = collapse_union(study_group_frequency),
    study_group_provider   = collapse_union(study_group_provider),
    study_group_recipients = collapse_union(study_group_recipients),
    study_group_location   = collapse_union(study_group_location)
  )

# FRIENDS (refid 10064)
apo_school_health_combined <- apo_group_td %>%
  filter(
    refid == 10064,
    gname %in% c(
      "School-led Friends",
      "Health-led Friends"
    )
  ) %>%
  summarise(
    refid = first(refid),
    gname = "School and Health-led Friends",
    group_type = "Intervention",
    
    study_group_format     = collapse_union(study_group_format),
    study_group_frequency  = collapse_union(study_group_frequency),
    study_group_provider   = collapse_union(study_group_provider),
    study_group_recipients = collapse_union(study_group_recipients),
    study_group_location   = collapse_union(study_group_location)
  )


# e-GAD (refid 10121)
apo_egad_combined <- apo_group_td %>%
  filter(
    refid == 10121,
    gname %in% c(
      "e-GAD school",
      "e-GAD Health"
    )
  ) %>%
  summarise(
    refid = first(refid),
    gname = "e-GAD school and health",
    group_type = "Intervention",
    
    study_group_format     = collapse_union(study_group_format),
    study_group_frequency  = collapse_union(study_group_frequency),
    study_group_provider   = collapse_union(study_group_provider),
    study_group_recipients = collapse_union(study_group_recipients),
    study_group_location   = collapse_union(study_group_location)
  )

apo_group_td <- bind_rows(
  apo_group_td,
  apo_lfl_combined,
  apo_friends_combined,
  apo_school_health_combined,
  apo_egad_combined
)


# BIND GROUP ROWS
full_group_td <- bind_rows(
  dpo_group_td %>%
    mutate(project = "DPO"),
  apo_group_td %>%
    mutate(project = "APO")
) %>% 
  select(-group_type, -ig_group_type, -fm_substudy_id) %>% 
  rename(intervention = gname)
#TODO: do we need to filter to only interventions (some control groups have intervention components)

# TESTING CHECKS
# check to see which interventions don't have match


# MERGE ALL DATA --------------------------------------------------------------

# Merge all binded data levels (MA, study level, group level)
merged_df <- combined_ma_td %>% 
  left_join(full_group_td, by = c("refid", "intervention", "project")) %>% 
  left_join(study_td, by = c("refid", "project")) %>% 
  rename(study_name = study) %>% 
  filter(outcome_domain != "Educational Achievement") # Removed educational achievement outcomes from app

# Check which Intervention names have missing group info after merge
check <- merged_df %>% 
  select(refid, study_name, intervention, comparison, outcome_domain, starts_with("study_group"), project) %>% 
  filter(is.na(study_group_format)) %>% 
  distinct(intervention, refid, project)

# CLEAN & TRANSFORM DATA FOR APP ----------------------------------------------

# Create lookup table for flagging ES direction based on outcome
beneficial_negative_flags <- tibble::tribble(
  ~outcome_domain,                ~beneficial_when_negative,
  "Depression Symptoms",    TRUE,
  "Depression Diagnosis",   TRUE,
  "Subsyndromal Depression", TRUE,
  "Anxiety",                TRUE,
  "Anxiety Symptoms",       TRUE,
  "Anxiety Diagnosis",      TRUE,
  "Suicidal Ideation",      TRUE,
  "Self-Harm",              TRUE,
  "Stress",                 TRUE, 
  "Substance Use",          TRUE,
  "Well-being",             FALSE,
  "Educational Achievement", FALSE
)

# Flag outcomes for direction color mapping
merged_df <- merged_df %>%
  left_join(beneficial_negative_flags, by = "outcome_domain") %>%
  mutate(
    beneficial_when_negative = dplyr::coalesce(beneficial_when_negative, TRUE), #TRUE = default
    targeted_behavior_label = case_when(
      project == "DPO" ~ "Depression Prevention",
      project == "APO" ~ "Anxiety Prevention",
      TRUE ~ "All")
  )

# Clean intervention names
intervention_df <- merged_df %>% 
  select(targeted_behavior_label, refid, study_author_year, intervention) %>% 
  distinct() %>% 
  arrange(intervention, refid)

export_date <- "2026-02-09"

# Export and manually update names/filters if needed
#rio::export(intervention_df, here("data", paste0("intervention_names_", export_date, ".xlsx")))

# Import crosswalk for cleaning intervention names
intervention_df <- rio::import(here("data", paste0("intervention_names_crosswalk_", export_date, ".xlsx")))

# Clean intervention names based on crosswalk
merged_td <- merged_df %>% 
  left_join(intervention_df %>% select(refid, intervention, branded, group_broad), 
            by = c("refid", "intervention")) %>% 
  mutate(intervention_group = dplyr::coalesce(
    na_if(trimws(group_broad), ""),
    intervention),
    branded_flag = toupper(trimws(branded)) == "YES")

# Function to harmonize response options based on input string
harmonize_response <- function(x, options, to, sep = ";"){
  x <- stringr::str_squish(as.character(x))
  
  #Build token-safe pattern like: (^|;\s*)(opt1|opt2|opt3)(\s*;|$)
  options_rx <- paste0(stringr::str_replace_all(options, "([\\.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1"),
                       collapse = "|")
  pat <- paste0("(^|", sep, "\\s*)(", options_rx, ")(\\s*", sep, "|$)")
  
  stringr::str_replace_all(x, pat, paste0("\\1", to, "\\3"))
}

# Harmonize response options
app_df <- merged_td %>%
  mutate(
    study_group_recipients = harmonize_response(
      study_group_recipients,
      options = c("Student,","student","students","Students", "Student"),
      to = "Students"
    ),
    study_group_provider = harmonize_response(
      study_group_provider,
      options = c("teacher","Teachers","Teacher,", "Teacher"),
      to = "Teacher"
    ),
    study_group_location = harmonize_response(
      study_group_location,
      options = c("School (During School Time)","School (during school hours)"),
      to = "School (During School Time)"
    ),
    study_group_location = harmonize_response(
      study_group_location,
      options = c("School (Out-of-School Time)","School (out of school time)"),
      to = "School (Out-of-School Time)"
    ),
    study_group_location = harmonize_response(
      study_group_location,
      options = c("Cannot tell", "Cannot Tell"),
                  to = "Cannot Tell"
      ),
    study_group_frequency = case_when(
      study_group_frequency == "4 times a week" ~ "2-4 Times A Week",
      study_group_frequency == "Cannot tell" ~ "Cannot Tell",
      study_group_frequency == "Less than weekly" ~ "Less Than Weekly",
      study_group_frequency %in% c("Once A Week", "Once a week") ~ "Once a Week",
      TRUE ~ study_group_frequency
    )
  )


# EXPORT APP DATA --------------------------------------------------------------

dpo_app <- app_df


# SET THEME  --------------------------------------------------

my_teal    <- "#489D46"
my_dark    <- "#4D5859"
my_lightbg <- "#D8DCDA"

my_theme <- bs_theme(
  version      = 4,
  bootswatch    = "flatly",
  primary      = my_teal,
  secondary    = my_dark,
  bg           = my_lightbg,
  fg           = my_dark,
  base_font    = font_google("Source Sans Pro"),
  heading_font = font_google("Source Sans Pro"),
  font_scale   = 1.2
)



# PLOT FUNCTIONS -----------------------------------------------

# # Function to plot jitter
# plot_jitter <- function(df, effect_better = "positive") {
#   pos_col <- if (effect_better=="positive") "#007030" else "#964B00"
#   neg_col <- if (effect_better=="positive") "#964B00" else "#007030"
#   
#   # Detect which field varies within a study
#   df2 <- df %>%
#     mutate(
#       study_name = dplyr::if_else(is.na(study_name) | study_name=="", paste0("Ref ", refid), study_name),
#       sd_i      = (x95_upper - x95_lower)/(2*1.96),
#       precision = 1/sd_i
#     ) %>%
#     group_by(study_name) %>%
#     mutate(
#       varies_tp   = dplyr::n_distinct(outcome_timepoint,  na.rm = TRUE) > 1,
#       varies_meas = dplyr::n_distinct(outcome_measure,    na.rm = TRUE) > 1,
#       varies_int  = dplyr::n_distinct(intervention,       na.rm = TRUE) > 1,
#       varies_comp = dplyr::n_distinct(comparison,         na.rm = TRUE) > 1,
#       sub_label = dplyr::case_when(
#         varies_tp   ~ paste0("TP=", outcome_timepoint),
#         varies_meas ~ outcome_measure,
#         varies_int  ~ paste0("Int=", intervention),
#         varies_comp ~ paste0("Comp=", comparison),
#         TRUE ~ ""
#       ),
#       y_label_raw = ifelse(sub_label == "", study_name, paste0(study_name, " — ", sub_label))
#     ) %>%
#     ungroup()
#   
#   # unique, non-NA factor levels
#   levels_y <- unique(as.character(df2$y_label_raw))
#   levels_y <- levels_y[!is.na(levels_y)]
#   
#   set.seed(2025)
#   cloud <- df2 %>%
#     rowwise() %>%
#     mutate(draws = list(rnorm(200, mean_effect, sd = sd_i))) %>%
#     tidyr::unnest(draws) %>%
#     ungroup() %>%
#     mutate(
#       y_label  = factor(y_label_raw, levels = levels_y),
#       draw_dir = draws > 0
#     )
#   
#   pts <- df2 %>%
#     mutate(
#       y_label = factor(y_label_raw, levels = levels_y),
#       dir     = mean_effect > 0
#     )
#   
#   precs      <- pts$precision
#   candidate  <- as.numeric(quantile(precs, c(0, 0.5, 1), na.rm = TRUE))
#   labs       <- c("Least precise", "Medium precise", "Most precise")
#   brks       <- unique(candidate)
#   labs_final <- labs[match(brks, candidate)]
#   
#   ggplot() +
#     annotate("rect", xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf, fill=neg_col, alpha=0.05) +
#     annotate("rect", xmin=0,    xmax=Inf, ymin=-Inf, ymax=Inf, fill=pos_col, alpha=0.05) +
#     geom_vline(xintercept=0, linetype="dashed", color="gray70") +
#     geom_jitter(data=cloud,
#                 aes(x=draws, y=y_label, fill=draw_dir),
#                 shape=21, color="black", alpha=0.04,
#                 size=1.2, height=0.1, show.legend=FALSE) +
#     geom_point(data=pts,
#                aes(x=mean_effect, y=y_label, size=precision, color=dir),
#                shape=19, show.legend=FALSE) +
#     scale_fill_manual(values = c(`TRUE`=pos_col, `FALSE`=neg_col), guide="none") +
#     scale_color_manual(values = c(`TRUE`=pos_col, `FALSE`=neg_col), guide="none") +
#     scale_size_continuous(
#       name   = "Precision",
#       range  = c(2, 6),
#       breaks = brks,
#       labels = labs_final
#     ) +
#     labs(x="Effect Size", y=NULL) +
#     theme_minimal(base_family="Source Sans Pro") +
#     theme(axis.text.y = element_text(color = my_dark))
# }


# # Function to plot Density of overall 
# 
# plot_density <- function(df, effect_better = "positive") {
#   pos_col <- if (effect_better=="positive") "#007030" else "#964B00"
#   neg_col <- if (effect_better=="positive") "#964B00" else "#007030"
#   
#   total_n    <- sum(df$sample_size)
#   overall_m  <- sum(df$mean_effect * df$sample_size) / total_n
#   sd_overall <- sd(df$mean_effect) / sqrt(nrow(df))
#   
#   # only sensible when at least two studies
#   if (nrow(df) < 2 || is.na(sd_overall) || sd_overall == 0) {
#     return(NULL)
#   }
#   
#   dens_df <- tibble(draws = rnorm(500, overall_m, sd_overall))
#   fill_col <- if (overall_m > 0) pos_col else neg_col
#   
#   ggplot(dens_df, aes(x = draws)) +
#     geom_density(fill = fill_col, alpha = 0.4, color = NA) +
#     labs(x="Overall Effect Size", y="Density") +
#     theme_minimal(base_family="Source Sans Pro") +
#     theme(
#       axis.title  = element_text(color = my_dark),
#       axis.text   = element_text(color = my_dark),
#       plot.margin = margin(5,5,20,5)
#     )
# }


plot_overall_jitter <- function(df, fit = NULL, effect_better = "positive") {
  pos_col <- if (effect_better=="positive") "#007030" else "#964B00"
  neg_col <- if (effect_better=="positive") "#964B00" else "#007030"
  
  df2 <- df %>%
    mutate(
      sd_i      = (ci_upper - ci_lower) / (2*1.96),
      precision = 1 / pmax(sd_i, .Machine$double.eps),
      beneficial = dplyr::if_else(
        beneficial_when_negative,
        yi < 0,
        yi > 0
      ),
      study_name = dplyr::if_else(is.na(study_name) | study_name=="", paste0("Ref ", refid), study_name),
      y_label   = factor("All studies"),
      tooltip = paste0(
        #"<b>", targeted_behavior_label, "</b>",
        "<br>Effect size: <b>", sprintf("%.2f", yi), "</b>",
        "<br>95% CI: <b>[", sprintf("%.2f", ci_lower), ", ", sprintf("%.2f", ci_upper), "]</b>",
        "<br>N: <b>", ifelse(is.na(study_number_participants), "—", format(study_number_participants, big.mark=",")), "</b>",
        "<br>Intervention: <b>", ifelse(is.na(intervention), "—", intervention), "</b>",
        "<br>Outcome: <b>", ifelse(is.na(outcome_domain), "—", outcome_domain), "</b>",
        "<br>Measure: <b>", ifelse(is.na(outcome_measure), "—", outcome_measure), "</b>",
        "<br>Timepoint: <b>", ifelse(is.na(outcome_timepoint), "—", outcome_timepoint), "</b>",
        "<br>Study: <b>", study_name, "</b>"
      )
    )
  
  # --- overall from RVE if available ---
  overall_m  <- NA_real_
  overall_lo <- NA_real_
  overall_hi <- NA_real_
  
  if (!is.null(fit) && !is.null(fit$reg_table)) {
    rt <- fit$reg_table
    overall_m  <- rt$b.r[1]
    overall_lo <- rt$CI.L[1]
    overall_hi <- rt$CI.U[1]
  }
  
  neg_better_majority <- mean(df2$beneficial_when_negative, na.rm = TRUE) >= 0.5
  overall_beneficial <- if (neg_better_majority) overall_m < 0 else overall_m > 0
  
  overall_df <- data.frame(
    y_label = factor("All studies"),
    m = overall_m, lo = overall_lo, hi = overall_hi,
    beneficial = overall_beneficial
  )
  
  gg <- ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
    geom_jitter(
      data = df2,
      aes(x = yi, y = y_label, fill = beneficial, text = tooltip),
      shape = 21,
      color = NA,
      alpha = 0.25,
      size = 2,
      height = 0.12,
      width = 0,
      show.legend = FALSE
    ) +
    geom_point(
      data = overall_df,
      aes(x = m, y = y_label),
      shape = 23, size = 5,
      fill = if (isTRUE(overall_beneficial)) pos_col else neg_col,
      color = "black"
    ) +
    scale_fill_manual(values = c(`TRUE`=pos_col, `FALSE`=neg_col), guide="none") +
    labs(x = "Effect Size", y = NULL) +
    theme_minimal(base_family = "Source Sans Pro") +
    theme(
      panel.grid.major.y = element_blank(),
      plot.margin = margin(5,5,20,5),
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.text.y  = element_blank()
    )
  
  # add CI only if we have it
  if (!is.na(overall_lo) && !is.na(overall_hi)) {
    gg <- gg + geom_errorbarh(
      data = overall_df,
      aes(y = y_label, xmin = lo, xmax = hi),
      height = 0.14, linewidth = 0.9, color = "black"
    )
  }
  
  gg
}


is_categorical_outcome <- function(df_outcome) {
  vals <- unique(tolower(na.omit(df_outcome$outcome_aggregation)))
  any(vals %in% c("binary", "dichotomous", "categorical"))
}

# `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


## Global totals -----------------
TOTAL_STUDIES <- dplyr::n_distinct(dpo_app$refid)
TOTAL_EFF     <- nrow(dpo_app)
TOTAL_N       <- dpo_app %>%
  dplyr::group_by(refid) %>%
  dplyr::summarise(study_n = suppressWarnings(max(as.numeric(study_number_participants), na.rm = TRUE)), .groups="drop") %>%
  dplyr::mutate(study_n = ifelse(is.finite(study_n), study_n, NA_real_)) %>%
  dplyr::summarise(total_n = sum(study_n, na.rm=TRUE)) %>%
  dplyr::pull(total_n)

# int_filter_choices <- dpo_app %>%
#   filter(branded_flag) %>%
#   distinct(intervention_group) %>%
#   arrange(tolower(intervention_group)) %>%
#   pull(intervention_group)


# APP ------------------------------------------------------------------
# UI -------------------------------------------------------------------

ui <- fluidPage(
  theme = my_theme,
  tags$head(tags$style(HTML("
    body { background-color: #F7F7F7; }
    .wizard-container {
      max-width: 900px;
      margin: 50px auto;
      display: flex;
      gap: 30px;
      align-items: flex-start;
    }
    .sidebar {
      width: 140px;
      background: white;
      border: 1px solid #DDD;
      border-radius: 4px;
      padding: 20px 0;
      box-shadow: 0 2px 8px rgba(0,0,0,0.05);
      display: flex;
      flex-direction: column;
      align-items: center;
      flex-shrink: 0;
    }
    .step-item {
      display: flex;
      flex-direction: column;
      align-items: center;
      margin-bottom: 12px;
    }
    .step-icon {
      font-size: 24px;
      color: #AAA;
    }
    .step-icon.completed { color: #4D5859; }
    .step-icon.active    { color: #489D46; }
    .step-label {
      margin-top: 6px;
      color: #489D46;
      font-weight: bold;
      opacity: 0;
      transition: opacity 0.2s;
    }
    .step-icon.active + .step-label { opacity: 1; }
    .wizard-panel {
      flex: 1;
      background: white;
      border: 1px solid #DDD;
      border-radius: 4px;
      padding: 25px 30px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.05);
    }
    .plot-panel {
      max-width: 700px;
      margin: 20px auto;
    }
    .table-container {
      overflow-x: auto;
      margin-top: 10px;
    }
    .wizard-footer {
      margin-top: 25px;
      text-align: right;
    }
    .wizard-footer .btn {
      min-width: 100px;
    }
    h2 { color: #489D46; margin-bottom: 20px; }
    hr { border-top: 1px solid #EEE; }
  "))),
  
  div(class="wizard-container",
      div(class="sidebar", uiOutput("sidebar_steps")),
      div(class="wizard-panel", uiOutput("wizard_ui"))
  )
)



# SERVER ---------------------------------------------------------------

server <- function(input, output, session) {
  
  ## Reactive Values ---------------------
  rv <- reactiveValues(
    page        = 1,
    
    #Page 1: Students
    chosen_prob = "All",
    chosen_age  = "All Ages",
    
    #Page 2: School
    chosen_school_area = "All",
    chosen_school_type = "All",
    
    #Page 3: Contexts
    chosen_country = "All",
    chosen_state   = "All",
    
    #Page 4: Interventions (keep yours)
    chosen_int  = "All",
    chosen_branded = character(0),
    chosen_generic = character(0),
    
    #Results drill down
    drill_down = FALSE,
    chosen_drill_filters = character(0),
    chosen_prevention_level = "All",
    chosen_school_level = "All",
    chosen_research_design = "All",
    chosen_group_format = "All",
    chosen_group_frequency = "All",
    chosen_group_provider = "All",
    chosen_group_recipients = "All",
    chosen_group_location = "All",
    
    view_outcome = NULL
  )
  
  prev_problem <- reactiveVal("All")
  prev_age <- reactiveVal("All Ages")
  
  branded_choices <- reactive({
    dpo_app %>%
      dplyr::filter(branded_flag) %>%
      dplyr::distinct(intervention_group) %>%
      dplyr::arrange(tolower(intervention_group)) %>%
      dplyr::pull(intervention_group)
  })
  
  generic_choices <- reactive({
    dpo_app %>%
      dplyr::filter(!branded_flag) %>%
      dplyr::distinct(intervention_group) %>%
      dplyr::arrange(tolower(intervention_group)) %>%
      dplyr::pull(intervention_group)
  })
  
  
  # Helper for total baseline N per outcome
  calc_total_study_n <- function(df) {
    df %>%
      dplyr::group_by(refid) %>%
      dplyr::summarise(
        study_n = suppressWarnings(max(as.numeric(study_number_participants), na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(study_n = ifelse(is.finite(study_n), study_n, NA_real_)) %>%
      dplyr::summarise(total_n = sum(study_n, na.rm = TRUE)) %>%
      dplyr::pull(total_n)
  }
  
  # Create reactive object for outcomes selected
  selected_outcomes <- reactive({
    df <- filtered_df()
    ods <- sort(unique(na.omit(df$outcome_domain)))
    ods
  })
  
  # Optional filter hints for restrictive filters
  filter_hints <- reactive({
    hints <- character(0)
    
    # Outcome: if not All
    if (!is.null(rv$chosen_prob) && !("All" %in% rv$chosen_prob)) {
      hints <- c(hints, "Try selecting <b>All</b> outcomes.")
    }
    
    # Intervention hint
    if (!is.null(rv$chosen_int) && rv$chosen_int %in% c("Branded","Generic")) {
      if (rv$chosen_int == "Branded" && length(rv$chosen_branded) == 0) {
        hints <- c(hints, "Select at least one <b>branded</b> intervention, or switch to <b>Show all interventions</b>.")
      }
      if (rv$chosen_int == "Generic" && length(rv$chosen_generic) == 0) {
        hints <- c(hints, "Select at least one <b>non-branded</b> intervention, or switch to <b>Show all interventions</b>.")
      }
    }
    
    # Age: if not All Ages
    if (!is.null(rv$chosen_age) && !("All Ages" %in% rv$chosen_age)) {
      hints <- c(hints, "Try selecting <b>All Ages</b>.")
    }
    
    hints
  })
  
  
  # Helper to print selected filters on results
  # Helper: pretty-print multi-select values
  fmt_sel <- function(x, all_label = "All", none_label = "All") {
    x <- x %||% character(0)
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) return(none_label)
    if (all_label %in% x) return(all_label)
    paste(x, collapse = ", ")
  }
  
  # Helper: only show a drill filter value if that drill filter is enabled
  fmt_drill_if_enabled <- function(key, label, chosen) {
    enabled <- rv$chosen_drill_filters %||% character(0)
    if (!(key %in% enabled)) return(NULL)
    
    #If the picker is still at "All", you may or may not want to show it.
    #Here, we show it anyway as "All" so user sees it's active.
    tags$li(strong(paste0(label, ": ")), fmt_sel(chosen, all_label = "All"),
            actionLink("edit_drill", "(Edit)"))
  }
  
  
  
  # # Helper to run RVE on single outcome
  # run_rve <- function(df) {
  #   df_rve <- df %>%
  #     transmute(
  #       yi = as.numeric(yi),
  #       vi = as.numeric(vi),
  #       studynum = as.character(refid)
  #     ) %>%
  #     filter(!is.na(yi), !is.na(vi), is.finite(vi), vi > 0)
  #   
  #   if (nrow(df_rve) < 2 || dplyr::n_distinct(df_rve$studynum) < 2) return(NULL)
  #   
  #   fit <- tryCatch(
  #     robumeta::robu(
  #       yi ~ 1,
  #       data = df_rve,
  #       studynum = studynum,
  #       var.eff.size = vi,
  #       rho = 0.8,
  #       small = TRUE
  #     ),
  #     error = function(e) NULL
  #   )
  #   
  #   if (is.null(fit) || is.null(fit$reg_table)) return(NULL)
  #   
  #   rt <- fit$reg_table
  #   list(
  #     fit = fit,
  #     est = rt$b.r[1],
  #     lo  = rt$CI.L[1],
  #     hi  = rt$CI.U[1]
  #   )
  # }
  

  
  
  
  ## Sidebar -------------------------------
  output$sidebar_steps <- renderUI({
    steps <- list(
      list(icon="rocket",    label="Welcome",      idx=1),
      list(icon="users",     label="Students",     idx=2),
      list(icon="school",    label="School",       idx=3),
      list(icon="map",       label="Contexts",     idx=4),
      list(icon="clipboard", label="Intervention", idx=5),
      list(icon="magic",     label="Results",      idx=6)
    )
    
    tags$div(lapply(steps, function(st) {
      cls <- if      (st$idx < rv$page) "step-icon completed"
      else if (st$idx == rv$page) "step-icon active"
      else                         "step-icon disabled"
      btn <- actionButton(paste0("nav", st$idx), NULL,
                          icon = icon(st$icon, verify_fa=FALSE),
                          class=cls, style="background:none;border:none;padding:0;")
      lbl <- tags$span(class="step-label", st$label)
      tags$div(class="step-item", btn, lbl)
    }))
  })
  
  ## UI Wizard pages ----------------------------
  output$wizard_ui <- renderUI({
    
    page <- rv$page  # <- dependency ONLY on page
    
    switch(as.character(page),
    
    #switch(as.character(rv$page),
           "1" = div(class="wizard-panel",
                     h2("Welcome"),
                     p(HTML("Explore evidence on K–12 school‐based Depression and Anxiety prevention interventions. <br>
                             <br>
                             Answers the question: Do anxiety and depression prevention programs work for my population and context of interest, and by how much. <br>
                             <br>
                             Currently only data from our reviews on Depression and Anxiety prevention interventions are included. More data will be added as we complete new reviews on K-12 school-based mental health prevention. 
                             <br>")),
                     div(class="wizard-footer",
                         actionButton("to_page2", "Get Started →", class="btn btn-primary")
                     )
           ),
           "2" = div(class="wizard-panel",
                     h2("Students"),
                     uiOutput("filter_status_box"),
                     hr(),
                     
                     h4("Student Outcome(s)"),
                     checkboxGroupInput("problem", NULL,
                                        choices = c("All", "Anxiety","Depression","Suicidal Ideation", "Self-Harm", "Stress", "Substance Use", "Well-being"),
                                        selected = rv$chosen_prob),
                     hr(),
                     
                     h4("Grade Level / Age Group(s)"),
                     checkboxGroupInput("age_group", NULL,
                                        choices = c("K-8","9-12","K-12", "All Ages"),
                                        selected = rv$chosen_age),
                     
                     div(class="wizard-footer",
                         actionButton("back_to1","← Back", class="btn btn-secondary"),
                         actionButton("to_page3","Proceed →", class="btn btn-primary")
                     )
           ),
           "3" = div(class="wizard-panel",
                     h2("School"),
                     uiOutput("filter_status_box"),
                     hr(),
                     
                     # shinyWidgets::pickerInput(
                     #   inputId  = "school_area",
                     #   label    = "School area:",
                     #   choices  = c("All", sort(setdiff(unique(unlist(strsplit(na.omit(dpo_app$study_school_area), ";\\s*"))), "Cannot Tell"))),
                     #   selected = rv$chosen_school_area,
                     #   multiple = TRUE,
                     #   options  = list(`actions-box` = TRUE, `live-search` = TRUE, `none-selected-text` = "All")
                     # ),
                     h4("School Area"),
                     checkboxGroupInput(
                       inputId = "school_area",
                       label = NULL,
                       choices = c(
                         "All",
                         sort(
                           setdiff(
                             unique(unlist(strsplit(na.omit(dpo_app$study_school_area), ";\\s*"))),
                             "Cannot Tell"
                           )
                         )
                       ),
                       selected = rv$chosen_school_area
                     ),
                     
                     h4("School Type"),
                     
                     checkboxGroupInput(
                       inputId = "school_type",
                       label =  NULL,
                       choices = c(
                         "All",
                         sort(
                           setdiff(
                             unique(unlist(strsplit(na.omit(dpo_app$study_school_type), ";\\s*"))),
                             "Cannot Tell"
                           )
                         )
                       ),
                       selected = rv$chosen_school_type
                     )
                     
                     
                     # shinyWidgets::pickerInput(
                     #   inputId  = "school_type",
                     #   label    = "School type:",
                     #   choices  = c("All", sort(setdiff(unique(unlist(strsplit(na.omit(dpo_app$study_school_type), ";\\s*"))), "Cannot Tell"))),
                     #   selected = rv$chosen_school_type,
                     #   multiple = TRUE,
                     #   options  = list(`actions-box` = TRUE, `live-search` = TRUE, `none-selected-text` = "All")
                     # ),
                     # 
                     # div(class="wizard-footer",
                     #     actionButton("back_to2","← Back", class="btn btn-secondary"),
                     #     actionButton("to_page4","Proceed →", class="btn btn-primary")
                     # )
           ),
           "4" = div(class="wizard-panel",
                     h2("Contexts"),
                     uiOutput("filter_status_box"),
                     hr(),
                     
                     shinyWidgets::pickerInput(
                       inputId  = "country",
                       label    = "Country:",
                       choices  = c("All", sort(setdiff(unique(unlist(strsplit(na.omit(dpo_app$study_country), ";\\s*"))), "Cannot Tell"))),
                       selected = isolate(rv$chosen_country),
                       multiple = TRUE,
                       options  = list(`actions-box` = TRUE, `live-search` = TRUE, `none-selected-text` = "All",
                                       `done-button` = TRUE,
                                       `done-button-text` = "Done")
                     ),
                     
                     shinyWidgets::pickerInput(
                       inputId  = "state",
                       label    = "State:",
                       choices  = c("All", sort(setdiff(unique(unlist(strsplit(na.omit(dpo_app$study_state), ";\\s*"))), "Cannot Tell"))),
                       selected = isolate(rv$chosen_state),
                       multiple = TRUE,
                       options  = list(`actions-box` = TRUE, `live-search` = TRUE, `none-selected-text` = "All",
                                       `done-button` = TRUE,
                                       `done-button-text` = "Done")
                     ),
                     
                     div(class="wizard-footer",
                         actionButton("back_to3","← Back", class="btn btn-secondary"),
                         actionButton("to_page5","Proceed →", class="btn btn-primary")
                     )
           ),
           "5" = div(class="wizard-panel",
                     # h2("Step 2: Select Intervention Type(s)"),
                     # checkboxGroupInput("intervention", NULL,
                     #                    choices = c("All Interventions"="All","CBT","Mindfulness","SEL","Family","Grouping (component/characteristic) OR specific interventions?"),
                     #                    selected = rv$chosen_int),
                     h2("Select Intervention(s)"),
                     uiOutput("filter_status_box"),
                     hr(),
                     
                     radioButtons(
                       inputId = "intervention_filter",
                       label   = NULL,
                       choices = c(
                         "Show all interventions" = "All",
                         "Filter by branded intervention(s)" = "Branded",
                         "Filter by non-branded intervention(s)" = "Generic"
                       ),
                       selected = rv$chosen_int
                     ),
                     
                     conditionalPanel(
                       condition = "input.intervention_filter == 'Branded'",
                       shinyWidgets::pickerInput(
                         inputId  = "branded_interventions",
                         label    = "Choose branded intervention(s):",
                         choices  = branded_choices(), #sort(unique(na.omit(dpo_app$intervention_group))),
                         selected = rv$chosen_branded,
                         multiple = TRUE,
                         options  = list(
                           `live-search` = TRUE,
                           `actions-box` = TRUE,
                           `closeOnSelect` = FALSE,
                           `selected-text-format` = "count > 2",
                           `none-selected-text` = "Select interventions",
                           `done-button` = TRUE,
                           `done-button-text` = "Done",
                           style = "btn-bg"
                         )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.intervention_filter == 'Generic'",
                       shinyWidgets::pickerInput(
                         inputId  = "generic_interventions",
                         label    = "Choose generic intervention(s):",
                         choices  = generic_choices(),
                         selected = rv$chosen_generic,
                         multiple = TRUE,
                         options  = list(
                           `live-search` = TRUE,
                           `actions-box` = TRUE,
                           `closeOnSelect` = FALSE,
                           `selected-text-format` = "count > 2",
                           `none-selected-text` = "Select interventions",
                           `done-button` = TRUE,
                           `done-button-text` = "Done"
                         )
                       )
                     ),
                     
                     div(class="wizard-footer",
                         actionButton("back_to4","← Back", class="btn btn-secondary"),
                         actionButton("to_page6","Proceed →", class="btn btn-primary")
                     )
           ),
           "6" = div(class="wizard-panel",
                     h2("Results"),
                     p("You selected:"),
                     tags$ul(
                       # Students page
                       tags$li(
                         strong("Outcome(s): "),
                         fmt_sel(rv$chosen_prob, all_label = "All"),
                         actionLink("edit_students", "(Edit)")
                       ),
                       tags$li(
                         strong("Age group(s): "),
                         fmt_sel(rv$chosen_age, all_label = "All Ages"),
                         actionLink("edit_students", "(Edit)")
                       ),
                       
                       # School page
                       tags$li(
                         strong("School area(s): "),
                         fmt_sel(rv$chosen_school_area, all_label = "All"),
                         actionLink("edit_school", "(Edit)")
                       ),
                       tags$li(
                         strong("School type(s): "),
                         fmt_sel(rv$chosen_school_type, all_label = "All"),
                         actionLink("edit_school", "(Edit)")
                       ),
                       
                       # Context page
                       tags$li(
                         strong("Country(ies): "),
                         fmt_sel(rv$chosen_country, all_label = "All"),
                         actionLink("edit_context", "(Edit)")
                       ),
                       tags$li(
                         strong("State(s): "),
                         fmt_sel(rv$chosen_state, all_label = "All"),
                         actionLink("edit_context", "(Edit)")
                       ),
                       
                       # Intervention page
                       tags$li(
                         strong("Intervention(s): "),
                         if (rv$chosen_int == "All") {
                           "All"
                         } else if (rv$chosen_int == "Branded") {
                           paste0("Branded: ", fmt_sel(rv$chosen_branded, all_label = "All", none_label = "None selected"))
                         } else if (rv$chosen_int == "Generic") {
                           paste0("Non-branded: ", fmt_sel(rv$chosen_generic, all_label = "All", none_label = "None selected"))
                         } else {
                           "All"
                         },
                         actionLink("edit_int", "(Edit)")
                       ),
                       
                       # # Drill-down summary (only show lines if that drill filter is selected)
                       # tags$li(
                       #   strong("Drill-down filters enabled: "),
                       #   if (length(rv$chosen_drill_filters %||% character(0)) == 0) "None"
                       #   else paste(rv$chosen_drill_filters, collapse = ", "),
                       #   actionLink("edit_drill", "(Edit)")
                       # ),
                       # 
                       # fmt_drill_if_enabled("prevention_level", "Prevention level", rv$chosen_prevention_level),
                       # fmt_drill_if_enabled("school_level",     "School level",     rv$chosen_school_level),
                       # fmt_drill_if_enabled("research_design",  "Research design",  rv$chosen_research_design),
                       # fmt_drill_if_enabled("group_format",     "Intervention format", rv$chosen_group_format),
                       # fmt_drill_if_enabled("group_frequency",  "Intervention frequency", rv$chosen_group_frequency),
                       # fmt_drill_if_enabled("group_provider",   "Provider",           rv$chosen_group_provider),
                       # fmt_drill_if_enabled("group_recipients", "Recipients",         rv$chosen_group_recipients),
                       # fmt_drill_if_enabled("group_location",   "Location",           rv$chosen_group_location)
                     ), 
                     p(strong(HTML("What else to show on this page?"))), #NOTES TO REMOVE TODO
                     hr(),
                     uiOutput("results_text"),
                     uiOutput("results_panels"),
                     # Drill Down Section
                     hr(),
                     h5("Want to drill down further?"),
                     
                     shinyWidgets::pickerInput(
                       inputId  = "drill_filters",
                       label    = NULL,
                       choices  = c(
                         "Prevention level (inconsistently coded)" = "prevention_level",
                         "School level"     = "school_level",
                         "Research design"  = "research_design",
                         "Intervention format"    = "group_format",
                         "Intervention frequency" = "group_frequency",
                         "Provider"         = "group_provider",
                         "Recipients"       = "group_recipients",
                         "Location"         = "group_location"
                       ),
                       selected = isolate(rv$chosen_drill_filters),
                       multiple = TRUE,
                       options  = list(
                         `live-search` = TRUE,
                         `actions-box` = TRUE,
                         `closeOnSelect` = FALSE,
                         `none-selected-text` = "Select additional filters to use",
                         `selected-text-format` = "count > 2",
                         `done-button` = TRUE,
                         `done-button-text` = "Done"
                       ),
                       width = "520px"
                     ),
                     
                     # Two-column layout for whichever filters they picked
                     tags$div(
                       style = "display:grid; grid-template-columns: 1fr 1fr; gap: 10px 18px; margin-top: 10px; align-items:start;",
                       
                       # Prevention level
                       conditionalPanel(
                         condition = "input.drill_filters && input.drill_filters.indexOf('prevention_level') >= 0",
                         shinyWidgets::pickerInput(
                           inputId  = "prevention_level",
                           label    = "Prevention level (inconsistently coded)",
                           choices  = c("All", sort(unique(na.omit(dpo_app$study_prevention_level)))),
                           selected = rv$chosen_prevention_level,
                           multiple = TRUE,
                           options  = list(`actions-box`=TRUE, `live-search`=TRUE, `none-selected-text`="All"),
                           width = "100%"
                         )
                       ),
                       
                       # School level
                       conditionalPanel(
                         condition = "input.drill_filters && input.drill_filters.indexOf('school_level') >= 0",
                         shinyWidgets::pickerInput(
                           inputId  = "school_level",
                           label    = "School level",
                           choices = c(
                             "All",
                             sort(
                               setdiff(
                                 unique(unlist(strsplit(na.omit(dpo_app$study_school_level), ";\\s*"))),
                                 "Cannot Tell"
                               )
                             )
                           ),
                           selected = rv$chosen_school_level,
                           multiple = TRUE,
                           options  = list(`actions-box`=TRUE, `live-search`=TRUE, `none-selected-text`="All"),
                           width = "100%"
                         )
                       ),
                       
                       # Research design
                       conditionalPanel(
                         condition = "input.drill_filters && input.drill_filters.indexOf('research_design') >= 0",
                         shinyWidgets::pickerInput(
                           inputId  = "research_design",
                           label    = "Research design",
                           choices  = c("All", sort(unique(na.omit(dpo_app$study_research_design)))),
                           selected = rv$chosen_research_design,
                           multiple = TRUE,
                           options  = list(`actions-box`=TRUE, `live-search`=TRUE, `none-selected-text`="All"),
                           width = "100%"
                         )
                       ),
                       
                       # Intervention format (NOTE: see note below about semicolon columns)
                       conditionalPanel(
                         condition = "input.drill_filters && input.drill_filters.indexOf('group_format') >= 0",
                         shinyWidgets::pickerInput(
                           inputId  = "group_format",
                           label    = "Intervention format",
                           choices = c(
                             "All",
                             sort(
                               setdiff(
                                 unique(unlist(strsplit(na.omit(dpo_app$study_group_format), ";\\s*"))),
                                 "Cannot Tell"
                               )
                             )
                           ),
                           selected = rv$chosen_group_format,
                           multiple = TRUE,
                           options  = list(`actions-box`=TRUE, `live-search`=TRUE, `none-selected-text`="All"),
                           width = "100%"
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "input.drill_filters && input.drill_filters.indexOf('group_frequency') >= 0",
                         shinyWidgets::pickerInput(
                           inputId  = "group_frequency",
                           label    = "Intervention frequency",
                           choices = c(
                             "All",
                             sort(
                               setdiff(
                                 unique(unlist(strsplit(na.omit(dpo_app$study_group_frequency), ";\\s*"))),
                                 "Cannot Tell"
                               )
                             )
                           ),
                           selected = rv$chosen_group_frequency,
                           multiple = TRUE,
                           options  = list(`actions-box`=TRUE, `live-search`=TRUE, `none-selected-text`="All"),
                           width = "100%"
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "input.drill_filters && input.drill_filters.indexOf('group_provider') >= 0",
                         shinyWidgets::pickerInput(
                           inputId  = "group_provider",
                           label    = "Provider",
                           choices = c(
                             "All",
                             sort(
                               setdiff(
                                 unique(
                                   na.omit(
                                     stringr::str_trim(
                                       stringr::str_remove(
                                         unlist(strsplit(as.character(dpo_app$study_group_provider), ";\\s*")),
                                         ",\\s*$"
                                       )
                                     )
                                   )
                                 ),
                                 "Cannot Tell"
                               )
                             )
                           ),
                           selected = rv$chosen_group_provider,
                           multiple = TRUE,
                           options  = list(`actions-box`=TRUE, `live-search`=TRUE, `none-selected-text`="All"),
                           width = "100%"
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "input.drill_filters && input.drill_filters.indexOf('group_recipients') >= 0",
                         shinyWidgets::pickerInput(
                           inputId  = "group_recipients",
                           label    = "Recipients",
                           choices = c(
                             "All",
                             sort(
                               setdiff(
                                 unique(
                                   na.omit(
                                     stringr::str_trim(
                                       stringr::str_remove(
                                         unlist(strsplit(as.character(dpo_app$study_group_recipients), ";\\s*")),
                                         ",\\s*$"
                                       )
                                     )
                                   )
                                 ),
                                 "Cannot Tell"
                               )
                             )
                           ),
                           selected = rv$chosen_group_recipients,
                           multiple = TRUE,
                           options  = list(`actions-box`=TRUE, `live-search`=TRUE, `none-selected-text`="All"),
                           width = "100%"
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "input.drill_filters && input.drill_filters.indexOf('group_location') >= 0",
                         shinyWidgets::pickerInput(
                           inputId  = "group_location",
                           label    = "Location",
                           choices = c(
                             "All",
                             sort(
                               setdiff(
                                 unique(
                                   na.omit(
                                     stringr::str_trim(
                                       stringr::str_remove(
                                         unlist(strsplit(as.character(dpo_app$study_group_location), ";\\s*")),
                                         ",\\s*$"
                                       )
                                     )
                                   )
                                 ),
                                 "Cannot Tell"
                               )
                             )
                           ),
                           selected = rv$chosen_group_location,
                           multiple = TRUE,
                           options  = list(`actions-box`=TRUE, `live-search`=TRUE, `none-selected-text`="All"),
                           width = "100%"
                         )
                       )
                     ),
                     # Footer
                     div(class="wizard-footer",
                         actionButton("back_to5","← Back", class="btn btn-secondary"),
                         actionButton("start_over","Start Over", class="btn",
                                      style="background-color:#8D1D58;color:white;")
                     )
           )
    )
  })
  
  ## Observers for Filters ------------------------
  #observeEvent(input$targeted_behavior, rv$chosen_target  <- input$targeted_behavior, ignoreNULL=TRUE)
  observeEvent(input$problem,     rv$chosen_prob <- input$problem,     ignoreNULL=TRUE)
  #observeEvent(input$intervention_filter, rv$chosen_int  <- input$intervention_filter,ignoreNULL=TRUE)
  
  observeEvent(input$branded_interventions, {
    rv$chosen_branded <- input$branded_interventions %||% character(0)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$generic_interventions, {
    rv$chosen_generic <- input$generic_interventions %||% character(0)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$age_group,   rv$chosen_age  <- input$age_group,   ignoreNULL=TRUE)
  # New filters
  observeEvent(input$school_area, rv$chosen_school_area <- input$school_area, ignoreNULL = FALSE)
  observeEvent(input$school_type, rv$chosen_school_type <- input$school_type, ignoreNULL = FALSE)
  observeEvent(input$country, rv$chosen_country <- input$country, ignoreNULL = FALSE)
  observeEvent(input$state,   rv$chosen_state   <- input$state,   ignoreNULL = FALSE)
  #observeEvent(input$drill_down, rv$drill_down <- input$drill_down, ignoreNULL = FALSE)
  observeEvent(input$prevention_level, rv$chosen_prevention_level <- input$prevention_level, ignoreNULL = FALSE)
  observeEvent(input$school_level,     rv$chosen_school_level     <- input$school_level, ignoreNULL = FALSE)
  observeEvent(input$research_design,  rv$chosen_research_design  <- input$research_design, ignoreNULL = FALSE)
  observeEvent(input$group_format,     rv$chosen_group_format     <- input$group_format, ignoreNULL = FALSE)
  observeEvent(input$group_frequency,  rv$chosen_group_frequency  <- input$group_frequency, ignoreNULL = FALSE)
  observeEvent(input$group_provider,   rv$chosen_group_provider   <- input$group_provider, ignoreNULL = FALSE)
  observeEvent(input$group_recipients, rv$chosen_group_recipients <- input$group_recipients, ignoreNULL = FALSE)
  observeEvent(input$group_location,   rv$chosen_group_location   <- input$group_location, ignoreNULL = FALSE)
  
  observeEvent(input$drill_filters, {
    rv$chosen_drill_filters <- input$drill_filters %||% character(0)
  }, ignoreNULL = FALSE)
  
  
  
  # Reset other picker when user switches between radio buttons
  observeEvent(input$intervention_filter, {
    rv$chosen_int <- input$intervention_filter
    
    #If they switch modes, clear the other selection
    if (rv$chosen_int == "Branded") rv$chosen_generic <- character(0)
    if (rv$chosen_int == "Generic") rv$chosen_branded <- character(0)
    if (rv$chosen_int == "All") {
      rv$chosen_branded <- character(0)
      rv$chosen_generic <- character(0)
    }
  }, ignoreInit = TRUE)
  
  # Update pickers to UI clears immediately
  observeEvent(rv$chosen_int, {
    if (rv$chosen_int != "Branded") shinyWidgets::updatePickerInput(session, "branded_interventions", selected = character(0))
    if (rv$chosen_int != "Generic") shinyWidgets::updatePickerInput(session, "generic_interventions", selected = character(0))
  }, ignoreInit = TRUE)
  
  
  #Outcome: Uncheck All when users select another outcome
  observeEvent(input$problem, {
    prev <- prev_problem()
    sel  <- input$problem %||% character(0)
    
    # If user just clicked "All" (it wasn't in prev, but is now), keep ONLY All
    if (!("All" %in% prev) && ("All" %in% sel)) {
      updateCheckboxGroupInput(session, "problem", selected = "All")
      prev_problem("All")
      return()
    }
    
    # Otherwise, if All is selected and user picked something else, remove All
    if ("All" %in% sel && length(sel) > 1) {
      sel2 <- setdiff(sel, "All")
      updateCheckboxGroupInput(session, "problem", selected = sel2)
      prev_problem(sel2)
      return()
    }
    
    # If nothing selected, default back to All
    if (length(sel) == 0) {
      updateCheckboxGroupInput(session, "problem", selected = "All")
      prev_problem("All")
      return()
    }
    
    # Normal case: store current selection
    prev_problem(sel)
  }, ignoreInit = TRUE)
  
  observeEvent(input$age_group, {
    prev <- prev_age()
    sel  <- input$age_group %||% character(0)
    
    # If user just clicked "All Ages", keep ONLY it
    if (!("All Ages" %in% prev) && ("All Ages" %in% sel)) {
      updateCheckboxGroupInput(session, "age_group", selected = "All Ages")
      prev_age("All Ages")
      return()
    }
    
    # Otherwise, if All Ages is selected and user picked something else, remove All Ages
    if ("All Ages" %in% sel && length(sel) > 1) {
      sel2 <- setdiff(sel, "All Ages")
      updateCheckboxGroupInput(session, "age_group", selected = sel2)
      prev_age(sel2)
      return()
    }
    
    # If nothing selected, default back to All Ages
    if (length(sel) == 0) {
      updateCheckboxGroupInput(session, "age_group", selected = "All Ages")
      prev_age("All Ages")
      return()
    }
    
    prev_age(sel)
  }, ignoreInit = TRUE)
  
  
  
  # Observers for navigation ---------------------------------
  observeEvent(input$to_page2, rv$page <- 2)
  observeEvent(input$to_page3, rv$page <- 3)
  observeEvent(input$to_page4, rv$page <- 4)
  observeEvent(input$to_page5, rv$page <- 5)
  observeEvent(input$to_page6, rv$page <- 6)
  
  observeEvent(input$back_to1, rv$page <- 1)
  observeEvent(input$back_to2, rv$page <- 2)
  observeEvent(input$back_to3, rv$page <- 3)
  observeEvent(input$back_to4, rv$page <- 4)
  observeEvent(input$back_to5, rv$page <- 5)
  
  # Observer for Start Over button
  observeEvent(input$start_over, {
    rv$page <- 1
    rv$chosen_prob <- "All"
    rv$chosen_age  <- "All Ages"
    
    rv$chosen_school_area <- "All"
    rv$chosen_school_type <- "All"
    rv$chosen_country <- "All"
    rv$chosen_state   <- "All"
    
    rv$chosen_int  <- "All"
    rv$chosen_branded <- character(0)
    rv$chosen_generic <- character(0)
    
    #rv$drill_down <- FALSE
    rv$chosen_drill_filters <- character(0)
    rv$chosen_prevention_level <- "All"
    rv$chosen_school_level <- "All"
    rv$chosen_research_design <- "All"
    rv$chosen_group_format <- "All"
    rv$chosen_group_frequency <- "All"
    rv$chosen_group_provider <- "All"
    rv$chosen_group_recipients <- "All"
    rv$chosen_group_location <- "All"
    
    # Sync UI immediately (important when using isolate() for selected=)
    updateCheckboxGroupInput(session, "problem", selected = "All")
    updateCheckboxGroupInput(session, "age_group", selected = "All Ages")
    
    shinyWidgets::updatePickerInput(session, "school_area", selected = "All")
    shinyWidgets::updatePickerInput(session, "school_type", selected = "All")
    
    shinyWidgets::updatePickerInput(session, "country", selected = "All")
    shinyWidgets::updatePickerInput(session, "state", selected = "All")
    
    updateRadioButtons(session, "intervention_filter", selected = "All")
    shinyWidgets::updatePickerInput(session, "branded_interventions", selected = character(0))
    shinyWidgets::updatePickerInput(session, "generic_interventions", selected = character(0))
    
    shinyWidgets::updatePickerInput(session, "drill_filters", selected = character(0))
    shinyWidgets::updatePickerInput(session, "prevention_level", selected = "All")
    shinyWidgets::updatePickerInput(session, "school_level", selected = "All")
    shinyWidgets::updatePickerInput(session, "research_design", selected = "All")
    shinyWidgets::updatePickerInput(session, "group_format", selected = "All")
    shinyWidgets::updatePickerInput(session, "group_frequency", selected = "All")
    shinyWidgets::updatePickerInput(session, "group_provider", selected = "All")
    shinyWidgets::updatePickerInput(session, "group_recipients", selected = "All")
    shinyWidgets::updatePickerInput(session, "group_location", selected = "All")
    
  })
  
  
  # Sidebar clicks
  observeEvent(input$nav1, rv$page <- 1)
  observeEvent(input$nav2, rv$page <- 2)
  observeEvent(input$nav3, rv$page <- 3)
  observeEvent(input$nav4, rv$page <- 4)
  observeEvent(input$nav5, rv$page <- 5)
  observeEvent(input$nav6, rv$page <- 6)
  
  # Edit links
  # Edit links (Results page)
  observeEvent(input$edit_students, { rv$page <- 2 })
  observeEvent(input$edit_school,   { rv$page <- 3 })
  observeEvent(input$edit_context,  { rv$page <- 4 })
  observeEvent(input$edit_int,      { rv$page <- 5 })
  observeEvent(input$edit_drill,    { rv$page <- 6 }) 
  
  



  ## Reactive Datasets ----------------------------
  
  #Helper: match ANY selected value against a semicolon-separated column
  #-selected: character vector of selected labels from UI
  #-x: semicolon-separated string column in df (may be NA)
  #-also trims whitespace; optionally trims trailing commas in the data
  match_any_semicolon <- function(x, selected, trim_trailing_commas = FALSE) {
    selected <- selected %||% character(0)
    selected <- selected[!is.na(selected) & selected != ""]
    if (length(selected) == 0) return(rep(TRUE, length(x)))
    
    xx <- as.character(x)
    if (trim_trailing_commas) {
      xx <- stringr::str_trim(stringr::str_remove(xx, ",\\s*$"))
    }
    xx <- stringr::str_squish(xx)
    
    #Escape regex metacharacters in selections
    esc <- function(s) stringr::str_replace_all(s, "([\\.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1")
    sel_rx <- paste(vapply(selected, esc, character(1)), collapse = "|")
    
    #Token-aware match in semicolon lists
    pat <- paste0("(^|;\\s*)(", sel_rx, ")(\\s*;|$)")
    !is.na(xx) & stringr::str_detect(xx, pat)
  }
  
  
  # Filtered Data
  
  filtered_df <- reactive({
    df <- dpo_app
    
    #1) Outcome-level filter (family matching)
    if (!is.null(rv$chosen_prob) && !("All" %in% rv$chosen_prob)) {
      
      pats <- dplyr::case_when(
        rv$chosen_prob == "Depression" ~ "depression",
        rv$chosen_prob == "Anxiety"    ~ "anxiety",
        TRUE ~ tolower(rv$chosen_prob)
      )
      
      pat <- paste(unique(pats), collapse = "|")
      
      df <- df %>%
        dplyr::filter(stringr::str_detect(tolower(outcome_domain), pat))
    }
    
    #2) Intervention (grouped)
    if (!is.null(rv$chosen_int)) {
      
      if (rv$chosen_int == "Branded") {
        df <- df %>% dplyr::filter(branded_flag)
        if (!is.null(rv$chosen_branded) && length(rv$chosen_branded) > 0) {
          df <- df %>% dplyr::filter(intervention_group %in% rv$chosen_branded)
        } else {
          df <- df[0, ]
        }
      }
      
      if (rv$chosen_int == "Generic") {
        df <- df %>% dplyr::filter(!branded_flag)
        if (!is.null(rv$chosen_generic) && length(rv$chosen_generic) > 0) {
          df <- df %>% dplyr::filter(intervention_group %in% rv$chosen_generic)
        } else {
          df <- df[0, ]
        }
      }
      
      #If "All", do nothing
    }
    
    #3) Age
    if (!is.null(rv$chosen_age) && !("All Ages" %in% rv$chosen_age)) {
      df <- df %>% dplyr::filter(age_group %in% rv$chosen_age)
    }
    
    #4) School page filters (NOW ASSUMED SEMICOLON-SEP STRINGS)
    if (!is.null(rv$chosen_school_area) && !("All" %in% rv$chosen_school_area)) {
      df <- df %>% dplyr::filter(match_any_semicolon(study_school_area, rv$chosen_school_area))
    }
    
    if (!is.null(rv$chosen_school_type) && !("All" %in% rv$chosen_school_type)) {
      df <- df %>% dplyr::filter(match_any_semicolon(study_school_type, rv$chosen_school_type))
    }
    
    #5) Context page filters (SEMICOLON-SEP STRINGS from checkbox-derived vars)
    if (!is.null(rv$chosen_country) && !("All" %in% rv$chosen_country)) {
      df <- df %>% dplyr::filter(match_any_semicolon(study_country, rv$chosen_country))
    }
    
    if (!is.null(rv$chosen_state) && !("All" %in% rv$chosen_state)) {
      df <- df %>% dplyr::filter(match_any_semicolon(study_state, rv$chosen_state))
    }
    
    #6) Drill-down filters
    drills <- rv$chosen_drill_filters %||% character(0)
    if (length(drills) > 0) {
      
      if (!is.null(rv$chosen_prevention_level) && !("All" %in% rv$chosen_prevention_level)) {
        df <- df %>% dplyr::filter(match_any_semicolon(study_prevention_level, rv$chosen_prevention_level))
      }
      
      if (!is.null(rv$chosen_school_level) && !("All" %in% rv$chosen_school_level)) {
        df <- df %>% dplyr::filter(match_any_semicolon(study_school_level, rv$chosen_school_level))
      }
      
      if (!is.null(rv$chosen_research_design) && !("All" %in% rv$chosen_research_design)) {
        df <- df %>% dplyr::filter(study_research_design %in% rv$chosen_research_design)
      }
      
      #Semicolon multi-select columns
      if (!is.null(rv$chosen_group_format) && !("All" %in% rv$chosen_group_format)) {
        df <- df %>% dplyr::filter(match_any_semicolon(study_group_format, rv$chosen_group_format))
      }
      
      if (!is.null(rv$chosen_group_frequency) && !("All" %in% rv$chosen_group_frequency)) {
        df <- df %>% dplyr::filter(match_any_semicolon(study_group_frequency, rv$chosen_group_frequency))
      }
      
      if (!is.null(rv$chosen_group_provider) && !("All" %in% rv$chosen_group_provider)) {
        df <- df %>% dplyr::filter(match_any_semicolon(study_group_provider, rv$chosen_group_provider, trim_trailing_commas = TRUE))
      }
      
      if (!is.null(rv$chosen_group_recipients) && !("All" %in% rv$chosen_group_recipients)) {
        df <- df %>% dplyr::filter(match_any_semicolon(study_group_recipients, rv$chosen_group_recipients, trim_trailing_commas = TRUE))
      }
      
      if (!is.null(rv$chosen_group_location) && !("All" %in% rv$chosen_group_location)) {
        df <- df %>% dplyr::filter(match_any_semicolon(study_group_location, rv$chosen_group_location, trim_trailing_commas = TRUE))
      }
    }
    
    df
  })
  
  
  
  selected_df <- reactive({
    df <- filtered_df()
    od <- input$view_outcome
    
    if (is.null(od) || od == "") od <- rv$view_outcome
    
    if (is.null(od) || od == "") return(df[0, , drop = FALSE])
    
    df[df$outcome_domain == od, , drop = FALSE]
  })
  
  
  
  filter_summary <- reactive({
    df <- filtered_df()
    
    n_eff <- nrow(df)
    n_studies <- dplyr::n_distinct(df$refid)
    
    # total N using your study-level sample_size
    totN <- calc_total_study_n(df)
    
    list(
      n_eff = n_eff,
      n_studies = n_studies,
      totN = totN
    )
  })
  
  
  
  
  # ma_selected <- reactive({
  #   df1 <- selected_df()
  #   if (nrow(df1) == 0) return(NULL)
  #   
  #   cat_flag <- is_categorical_outcome(df1)
  #   
  #   # Prepare data (always)
  #   base <- df1 %>%
  #     transmute(
  #       yi = as.numeric(yi),
  #       vi = as.numeric(vi),
  #       refid_chr = as.character(refid),
  #       study_name_chr = as.character(study_name)
  #     ) %>%
  #     filter(!is.na(yi), !is.na(vi), is.finite(vi), vi > 0)
  #   
  #   # Need at least 2 effects AND at least 2 clusters
  #   if (nrow(base) < 2) return(NULL)
  #   
  #   # --- BRANCH: categorical vs continuous ---
  #   if (cat_flag) {
  #     # Match your example: studynum = study
  #     # In-app: create a 'study' column (use refid as safest unique study ID)
  #     dd_escalc <- base %>%
  #       transmute(yi, vi, study = refid_chr)
  #     
  #     if (dplyr::n_distinct(dd_escalc$study) < 2) return(NULL)
  #     
  #     fit <- tryCatch(
  #       robumeta::robu(
  #         formula = yi ~ 1,
  #         data = dd_escalc,
  #         studynum = study,
  #         var.eff.size = vi,
  #         rho = 0.8,
  #         small = TRUE
  #       ),
  #       error = function(e) NULL
  #     )
  #     
  #   } else {
  #     # Continuous outcomes (your original approach)
  #     as_escalc <- base %>%
  #       transmute(yi, vi, studynum = refid_chr)
  #     
  #     if (dplyr::n_distinct(as_escalc$studynum) < 2) return(NULL)
  #     
  #     fit <- tryCatch(
  #       robumeta::robu(
  #         formula = yi ~ 1,
  #         data = as_escalc,
  #         studynum = studynum,
  #         var.eff.size = vi,
  #         rho = 0.8,
  #         small = TRUE
  #       ),
  #       error = function(e) NULL
  #     )
  #   }
  #   
  #   if (is.null(fit) || is.null(fit$reg_table)) return(NULL)
  #   
  #   rt <- fit$reg_table
  #   
  #   list(
  #     fit = fit,
  #     est = rt$b.r[1],
  #     lo  = rt$CI.L[1],
  #     hi  = rt$CI.U[1],
  #     is_categorical = cat_flag
  #   )
  # })
  
  
  ma_selected <- reactive({
    df1 <- selected_df()
    if (nrow(df1) == 0) return(list(ok = FALSE, reason = "No rows for selected outcome."))
    
    cat_flag <- is_categorical_outcome(df1)
    
    base <- df1 %>%
      transmute(
        yi = as.numeric(yi),
        vi = as.numeric(vi),
        refid_chr = as.character(refid)
      )
    
    # keep all rows for diagnostics
    n_all <- nrow(base)
    n_good <- sum(!is.na(base$yi) & !is.na(base$vi) & is.finite(base$vi) & base$vi > 0)
    
    base2 <- base %>% filter(!is.na(yi), !is.na(vi), is.finite(vi), vi > 0)
    
    if (nrow(base2) < 2) {
      return(list(ok = FALSE, reason = sprintf("Not enough usable effects (usable=%d of %d).", n_good, n_all)))
    }
    
    if (dplyr::n_distinct(base2$refid_chr) < 2) {
      return(list(ok = FALSE, reason = "Need at least 2 unique studies (refid) to fit RVE."))
    }
    
    # Use refid as cluster id for BOTH continuous & categorical

    fit <- tryCatch(
      robumeta::robu(
        yi ~ 1,
        data = base2 %>% transmute(yi, vi, study = refid_chr),
        studynum = study,
        var.eff.size = vi,
        rho = 0.8,
        small = TRUE
      ),
      error = function(e) e
    )
    
    if (inherits(fit, "error")) {
      return(list(ok = FALSE, reason = paste0("robu() error: ", fit$message)))
    }
    
    rt <- fit$reg_table
    list(
      ok = TRUE,
      fit = fit,
      est = rt$b.r[1],
      lo  = rt$CI.L[1],
      hi  = rt$CI.U[1],
      is_categorical = cat_flag
    )
  })
  
  
  
  
  
  
  # output$filtered_table <- renderTable({
  #   df <- filtered_df()
  #   if (nrow(df)==0) return(data.frame(Note="No matching rows"))
  #   df %>% select(study_name, problem, intervention, outcome_measure, outcome_timepoint, #age_group,
  #                 mean_effect, x95_lower, x95_upper, sample_size) %>% 
  #     arrange(study_name, intervention, outcome_measure, outcome_timepoint)
  # })
  
  # output$results_text <- renderUI({
  #   df <- filtered_df()
  #   
  #   # counts
  #   n_eff     <- nrow(df)                        # effect sizes
  #   n_studies <- dplyr::n_distinct(df$refid)     # unique studies (use refid)
  #   
  #   # ----- N participants: sum over studies of max baseline total N
  #   by_study <- df %>%
  #     mutate(
  #       baseline_total_n = rowSums(cbind(baseline_intervention_n, baseline_comparison_n), na.rm = TRUE),
  #       baseline_total_n = ifelse(baseline_total_n == 0, NA_real_, baseline_total_n)
  #     ) %>%
  #     group_by(refid) %>%
  #     summarise(
  #       study_n = suppressWarnings(max(baseline_total_n, na.rm = TRUE)),
  #       .groups = "drop"
  #     )
  #   by_study$study_n[!is.finite(by_study$study_n)] <- NA
  #   tot_study_n <- sum(by_study$study_n, na.rm = TRUE)
  #   
  #   # ----- RVE meta-analysis on filtered data
  #   # IMPORTANT: robu needs a "studynum" id that identifies clusters.
  #   # Use refid for clustering (study-level). (Your example uses 'study', but study names can repeat/NA.)
  #   
  #   rve_ok <- n_eff > 0 &&
  #     sum(!is.na(df$yi) & !is.na(df$vi) & is.finite(df$vi) & df$vi > 0) >= 2 &&
  #     n_studies >= 2
  #   
  #   if (rve_ok) {
  #     df_rve <- df %>%
  #       transmute(
  #         yi = as.numeric(yi),
  #         vi = as.numeric(vi),
  #         studynum = as.character(refid)
  #       ) %>%
  #       filter(!is.na(yi), !is.na(vi), is.finite(vi), vi > 0)
  #     
  #     fit <- tryCatch(
  #       robumeta::robu(
  #         formula = yi ~ 1,
  #         data = df_rve,
  #         studynum = studynum,
  #         var.eff.size = vi,
  #         rho = 0.8,
  #         small = TRUE
  #       ),
  #       error = function(e) NULL
  #     )
  #     
  #     if (!is.null(fit)) {
  #       # Extract intercept estimate and CI
  #       # robumeta output has a "reg_table" with b.r and confidence interval cols.
  #       rt <- fit$reg_table
  #       
  #       est <- rt$b.r[1]
  #       lo  <- rt$CI.L[1]
  #       hi  <- rt$CI.U[1]
  #       
  #       HTML(sprintf(
  #         "<p><em>Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants), RVE mean effect size = <strong>%.2f</strong> (95%% CI <strong>[%.2f, %.2f]</strong>).</em></p>",
  #         n_eff,
  #         n_studies,
  #         ifelse(tot_study_n == 0, "—", format(tot_study_n, big.mark=",")),
  #         est, lo, hi
  #       ))
  #       
  #     } else {
  #       HTML(sprintf(
  #         "<p><em>Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants), RVE mean effect size = <strong>—</strong>.</em></p>",
  #         n_eff,
  #         n_studies,
  #         ifelse(tot_study_n == 0, "—", format(tot_study_n, big.mark=","))        
  #       ))
  #     }
  #     
  #   } else {
  #     # Not enough data to fit RVE
  #     HTML(sprintf(
  #       "<p><em>Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants), RVE mean effect size = <strong>—</strong>.</em></p>",
  #       n_eff,
  #       n_studies,
  #       ifelse(tot_study_n == 0, "—", format(tot_study_n, big.mark=","))      
  #     ))
  #   }
  # })
  
  ## Output: status box across pages ---------------------------------
  output$filter_status_box <- renderUI({
    if (!(rv$page %in% c(2,3,4, 5))) return(NULL)
    
    s <- filter_summary()
    
    # message + style
    if (is.null(s) || s$n_studies == 0 || s$n_eff == 0) {
      # Optional filter hints
      hints <- filter_hints()
      hint_html <- if (length(hints) > 0) paste0("<br><span>", paste(hints, collapse=" "), "</span>") else ""
      
      return(
        div(
          style = "border:1px solid #E0B4B4; background:#FFF6F6; color:#9F3A38; padding:10px 12px; border-radius:6px; margin-bottom:10px;",
          strong("No studies match your current selections.")
        )
      )
    }
    
    div(
      style = "border:1px solid #DDD; background:#F9FAFB; padding:10px 12px; border-radius:6px; margin-bottom:10px;",
      HTML(sprintf(
        "<strong>Matches:</strong> %s of %s studies &nbsp;•&nbsp; %s of %s effect sizes &nbsp;•&nbsp; N = %s participants",
        format(s$n_studies, big.mark=","),
        format(TOTAL_STUDIES, big.mark=","),
        format(s$n_eff, big.mark=","),
        format(TOTAL_EFF, big.mark=","),
        ifelse(is.na(s$totN) || s$totN == 0, "—", format(s$totN, big.mark=","))
      ))
    )
  })
  
  
  ## Output: overall results sentence ---------------------------------
  
  output$results_text <- renderUI({
    df <- filtered_df()
    
    n_eff     <- nrow(df)
    n_studies <- dplyr::n_distinct(df$refid)
    
    # # total baseline N (one per study, then sum)
    # by_study <- df %>%
    #   mutate(
    #     baseline_total_n = rowSums(cbind(baseline_intervention_n, baseline_comparison_n), na.rm = TRUE),
    #     baseline_total_n = ifelse(baseline_total_n == 0, NA_real_, baseline_total_n)
    #   ) %>%
    #   group_by(refid) %>%
    #   summarise(study_n = suppressWarnings(max(baseline_total_n, na.rm = TRUE)), .groups = "drop")
    # 
    # by_study$study_n[!is.finite(by_study$study_n)] <- NA
    # tot_study_n <- sum(by_study$study_n, na.rm = TRUE)
    
    tot_study_n <- calc_total_study_n(df) 
    
    HTML(sprintf(
      "<p><em>Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants).</em></p>",
      n_eff,
      n_studies,
      ifelse(tot_study_n == 0, "—", format(tot_study_n, big.mark=","))
    ))
  })
  
  
  # output$results_jitter <- renderPlot({
  #   df <- filtered_df()
  #   if (nrow(df)==0) return(NULL)
  #   plot_jitter(df)
  # })
  
  # output$results_density <- renderPlot({
  #   df <- filtered_df()
  #   if (nrow(df) < 2) return(NULL)
  #   plot_overall_jitter(df)
  # }, height = 180)
  
  # rve_fit <- reactive({
  #   df <- filtered_df()
  #   
  #   df_rve <- df %>%
  #     transmute(
  #       yi = as.numeric(yi),
  #       vi = as.numeric(vi),
  #       studynum = as.character(refid)
  #     ) %>%
  #     filter(!is.na(yi), !is.na(vi), is.finite(vi), vi > 0)
  #   
  #   if (nrow(df_rve) < 2 || dplyr::n_distinct(df_rve$studynum) < 2) return(NULL)
  #   
  #   tryCatch(
  #     robumeta::robu(
  #       yi ~ 1,
  #       data = df_rve,
  #       studynum = studynum,
  #       var.eff.size = vi,
  #       rho = 0.8,
  #       small = TRUE
  #     ),
  #     error = function(e) NULL
  #   )
  # })
  
  
  # output$results_density <- renderPlotly({
  #   df <- filtered_df()
  #   if (nrow(df) < 1) return(NULL)
  #   
  #   p <- plot_overall_jitter(df)
  #   
  #   ggplotly(p, tooltip = "text") %>%
  #     layout(
  #       hoverlabel = list(
  #         bgcolor = "white",     # <- force white background
  #         bordercolor = "#333",  # subtle border (optional)
  #         font = list(
  #           color = "black",
  #           size = 12
  #         ),
  #         align = "left"
  #       )
  #     )
  # })
  
  # output$results_density <- renderPlotly({
  #   df <- filtered_df()
  #   if (nrow(df) < 1) return(NULL)
  #   
  #   fit <- rve_fit()
  #   
  #   p <- plot_overall_jitter(df, fit)  # <- pass fit
  #   
  #   ggplotly(p, tooltip = "text") %>%
  #     layout(
  #       hoverlabel = list(
  #         bgcolor = "white",
  #         bordercolor = "#333",
  #         font = list(color = "black", size = 12),
  #         align = "left"
  #       )
  #     )
  # })
  
  ## Selected outcome dropdown --------------------------
  # When selected outcome changes, set default dropdown choice 
  observeEvent(selected_outcomes(), {
    ods <- selected_outcomes()
    if (length(ods) >= 1) {
      rv$view_outcome <- ods[1]
      shinyWidgets::updatePickerInput(session, "view_outcome",
                                      choices = ods, selected = ods[1])
    } else {
      rv$view_outcome <- NULL
    }
  }, ignoreInit = FALSE)
  
  
  
  # output$results_panels <- renderUI({
  #   ods <- selected_outcomes()
  #   
  #   if (length(ods) == 0) {
  #     return(HTML("<p><em>No matching studies for the selected outcome(s).</em></p>"))
  #   }
  #   
  #   # If exactly 1 outcome, no tabs (single outcome view)
  #   if (length(ods) == 1) {
  #     return(tagList(
  #       uiOutput("results_text_single"),
  #       div(class="plot-panel", plotlyOutput("results_plot_single", height="200px"))
  #     ))
  #   }
  #   
  #   # If multiple outcomes, create tabs (one per outcome)
  #   panels <- lapply(ods, function(od) {
  #     safe_id <- make.names(od)
  #     tabPanel(
  #       title = od,
  #       uiOutput(paste0("results_text_", safe_id)),
  #       div(class="plot-panel", plotlyOutput(paste0("results_plot_", safe_id), height="200px"))
  #     )
  #   })
  #   
  #   # IMPORTANT: splice panels into tabsetPanel (not as a list argument)
  #   do.call(tabsetPanel, c(list(id = "outcome_tabs"), panels))
  # })
  
  ## Output: results panel show results sentence and plot per outcome --------------------
  output$results_panels <- renderUI({
    ods <- selected_outcomes()
    
    if (length(ods) == 0) {
      return(HTML("<p><em>No matching studies for the selected outcome(s).</em></p>"))
    }
    
    # If only 1 outcome exists, just show it (no dropdown)
    if (length(ods) == 1) {
      return(tagList(
        uiOutput("results_text_selected"),
        div(class="plot-panel", plotlyOutput("results_plot_selected", height="200px"))
      ))
    }
    
    # If multiple outcomes: dropdown selector + single outcome display
    tagList(
      div(style = "max-width: 420px; margin-bottom: 10px;",
          shinyWidgets::pickerInput(
            inputId = "view_outcome",
            label = "View results for outcome:",
            choices = ods,
            selected = rv$view_outcome,
            options = list(`live-search` = TRUE),
            width = "420px"
          )
      ),
      uiOutput("results_text_selected"),
      div(class="plot-panel", plotlyOutput("results_plot_selected", height="200px"))
    )
  })
  
  
  
  
  # # Render single results sentence per outcome tab
  # output$results_text_single <- renderUI({
  #   df <- filtered_df()
  #   ods <- selected_outcomes()
  #   df1 <- df[df$outcome_domain == ods[1], , drop = FALSE]
  #   
  #   n_eff <- nrow(df1)
  #   n_studies <- dplyr::n_distinct(df1$refid)
  #   totN <- calc_total_study_n(df1)
  #   
  #   rve <- run_rve(df1)
  #   
  #   if (is.null(rve)) {
  #     return(HTML(sprintf(
  #       "<p><em>Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants), RVE mean effect size = <strong>—</strong>.</em></p>",
  #       n_eff, n_studies, ifelse(totN == 0, "—", format(totN, big.mark=","))
  #     )))
  #   }
  #   
  #   HTML(sprintf(
  #     "<p><em>Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants), RVE mean effect size = <strong>%.2f</strong> (95%% CI <strong>[%.2f, %.2f]</strong>).</em></p>",
  #     n_eff, n_studies, ifelse(totN == 0, "—", format(totN, big.mark=",")),
  #     rve$est, rve$lo, rve$hi
  #   ))
  # })
  # 
  # output$results_plot_single <- renderPlotly({
  #   df <- filtered_df()
  #   ods <- selected_outcomes()
  #   df1 <- df[df$outcome_domain == ods[1], , drop = FALSE]
  #   rve <- run_rve(df1)
  #   
  #   p <- plot_overall_jitter(df1, if (is.null(rve)) NULL else rve$fit)
  #   ggplotly(p, tooltip = "text") %>%
  #     layout(
  #       hoverlabel = list(
  #         bgcolor = "white",
  #         bordercolor = "#333",
  #         font = list(color = "black", size = 12),
  #         align = "left"
  #       )
  #     )
  # })
  # 
  # # Render multi-outcome tabs (one set per tab)
  # observe({
  #   ods <- selected_outcomes()
  #   
  #   # Only create per-outcome outputs when >1 outcomes (tabs mode)
  #   if (length(ods) <= 1) return(NULL)
  #   
  #   lapply(ods, function(od) {
  #     safe_id <- make.names(od)
  #     local({
  #       od_local <- od
  #       id_text <- paste0("results_text_", safe_id)
  #       id_plot <- paste0("results_plot_", safe_id)
  #       
  #       output[[id_text]] <- renderUI({
  #         df <- filtered_df()
  #         df1 <- df[df$outcome_domain == od_local, , drop = FALSE]
  #         
  #         n_eff <- nrow(df1)
  #         n_studies <- dplyr::n_distinct(df1$refid)
  #         totN <- calc_total_study_n(df1)
  #         rve <- run_rve(df1)
  #         
  #         if (is.null(rve)) {
  #           return(HTML(sprintf(
  #             "<p><em>Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants), RVE mean effect size = <strong>—</strong>.</em></p>",
  #             n_eff, n_studies, ifelse(totN == 0, "—", format(totN, big.mark=","))
  #           )))
  #         }
  #         
  #         HTML(sprintf(
  #           "<p><em>Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants), RVE mean effect size = <strong>%.2f</strong> (95%% CI <strong>[%.2f, %.2f]</strong>).</em></p>",
  #           n_eff, n_studies, ifelse(totN == 0, "—", format(totN, big.mark=",")),
  #           rve$est, rve$lo, rve$hi
  #         ))
  #       })
  #       
  #       output[[id_plot]] <- renderPlotly({
  #         df <- filtered_df()
  #         df1 <- df[df$outcome_domain == od_local, , drop = FALSE]
  #         rve <- run_rve(df1)
  #         
  #         p <- plot_overall_jitter(df1, if (is.null(rve)) NULL else rve$fit)
  #         ggplotly(p, tooltip = "text") %>%
  #           layout(
  #             hoverlabel = list(
  #               bgcolor = "white",
  #               bordercolor = "#333",
  #               font = list(color = "black", size = 12),
  #               align = "left"
  #             )
  #           )
  #       })
  #     })
  #   })
  # })
  
  
  
  ## Output: results sentence per outcome ---------------------
  
  output$results_text_selected <- renderUI({
    df1 <- selected_df()
    od  <- input$view_outcome
    
    if (is.null(od) || od == "") od <- rv$view_outcome
    
    if (nrow(df1) == 0 || is.null(od) || od == "") return(NULL)
    
    n_eff <- nrow(df1)
    n_studies <- dplyr::n_distinct(df1$refid)
    totN <- calc_total_study_n(df1)
    
    ma <- ma_selected()  # ALWAYS a list now
    
    # If model failed, show WHY (and still show counts)
    if (!isTRUE(ma$ok)) {
      return(HTML(sprintf(
        "<p><em><strong>%s</strong>: Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants).<br><span style='color:#8D1D58;'><strong>Meta-analysis not run:</strong> %s</span></em></p>",
        od,
        n_eff,
        n_studies,
        ifelse(totN == 0, "—", format(totN, big.mark=",")),
        htmltools::htmlEscape(ma$reason %||% "Unknown reason.")
      )))
    }
    
    # Otherwise, show estimate
    HTML(sprintf(
      "<p><em><strong>%s</strong>: Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants), RVE mean effect size = <strong>%.2f</strong> (95%% CI <strong>[%.2f, %.2f]</strong>).</em></p>",
      od,
      n_eff,
      n_studies,
      ifelse(totN == 0, "—", format(totN, big.mark=",")),
      ma$est, ma$lo, ma$hi
    ))
  })
  
  
  ## Output: plot of MA results per outcome ----------------------
  output$results_plot_selected <- renderPlotly({
    df1 <- selected_df()
    if (nrow(df1) == 0) return(NULL)
    
    ma <- ma_selected()
    fit_obj <- if (isTRUE(ma$ok)) ma$fit else NULL
    
    p <- plot_overall_jitter(df1, fit_obj)
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          bordercolor = "#333",
          font = list(color = "black", size = 12),
          align = "left"
        )
      )
  })
  
  
  
  
}

# RUN APP --------------------------------------------------------------

shinyApp(ui, server)

#rsconnect::deployApp()