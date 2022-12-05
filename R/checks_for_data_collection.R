library(checksupporteR)
library(tidyverse)
library(lubridate)
library(glue)
library(cluster)

source("R/support_functions.R")


# read data ---------------------------------------------------------------

dataset_location_pa <- "inputs/partipatory_assessment_data.xlsx"

df_tool_data <- readxl::read_excel(path = dataset_location_pa) %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = as.character(enumerator_id),
         i.check.district_name = case_when(settlement %in% c("rhino_camp") ~ "madi_okollo",
                                           settlement %in% c("bidibidi") ~ "yumbe",
                                           settlement %in% c("imvepi") ~ "terego",
                                           settlement %in% c("palabek") ~ "lamwo",
                                           settlement %in% c("kyangwali") ~ "kikube",
                                           settlement %in% c("lobule") ~ "koboko",
                                           settlement %in% c("nakivale") ~ "isingiro",
                                           settlement %in% c("oruchinga") ~ "isingiro",
                                           settlement %in% c("palorinya") ~ "obongi",
                                           settlement %in% c("rwamwanja") ~ "kamwenge",
                                           settlement %in% c("kyaka_ii") ~ "kyegegwa",
                                           settlement %in% c("any_adjumani_settlements") ~ "adjumani",
                                           settlement %in% c("kampala") ~ "kampala",
                                           TRUE ~ settlement),
         district_name = i.check.district_name,
         i.check.settlement = settlement,
         i.check.point_number = household_id,
         point_number = i.check.point_number) %>% 
  filter(i.check.start_date > as_date("2022-11-22") & end_result == 1)

hh_roster_data <- readxl::read_excel(path = dataset_location_pa, sheet = "hh_roster")
df_repeat_hh_roster_data <- df_tool_data %>% 
  inner_join(hh_roster_data, by = c("_uuid" = "_submission__uuid"))

df_survey <- readxl::read_excel(path = "inputs/participatory_assessment_tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel(path = "inputs/participatory_assessment_tool.xlsx", sheet = "choices")


# output holder -----------------------------------------------------------

logic_output <- list()


# data not meeting minimum requirements -----------------------------------

# # testing_data
# df_testing_data <- df_tool_data %>% 
#   filter(i.check.start_date < as_date("2022-11-15") | str_detect(string = household_id, pattern = fixed('test', ignore_case = TRUE))) %>% 
#   mutate(i.check.type = "remove_survey",
#          i.check.name = "",
#          i.check.current_value = "",
#          i.check.value = "",
#          i.check.issue_id = "logic_c_testing_data",
#          i.check.issue = "testing_data",
#          i.check.other_text = "",
#          i.check.checked_by = "",
#          i.check.checked_date = as_date(today()),
#          i.check.comment = "", 
#          i.check.reviewed = "1",
#          i.check.adjust_log = "",
#          i.check.so_sm_choices = "") %>% 
#   dplyr::select(starts_with("i.check.")) %>% 
#   rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
# 
# add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_testing_data")

# time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 20
max_time_of_survey <- 60

df_survey_time <- check_survey_time(input_tool_data = df_tool_data, 
                                    input_min_time = min_time_of_survey,
                                    input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_survey_time")


# check the time between surveys

# min_time_btn_surveys <- 5
# 
# df_time_btn_surveys <- check_time_interval_btn_surveys(input_tool_data = df_tool_data, 
#                                                        input_min_time = min_time_btn_surveys)
# 
# add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_time_btn_surveys")

# outlier checks ----------------------------------------------------------

# df_c_outliers <- checksupporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data)
# 
# add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_c_outliers")
# 
# df_c_outliers_hh_roster <- checksupporteR::check_outliers_cleaninginspector_repeats(input_tool_data = df_repeat_hh_roster_data,
#                                                                                     input_sheet_name = "hh_roster", input_repeat_cols = c("age"))
# 
# add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_outliers_hh_roster") 

# # duplicate hh_id numbers
# df_c_duplicate_hhid_nos <- check_duplicate_hhid_numbers(input_tool_data = df_tool_data,
#                                                         input_sample_hhid_nos_list = sample_hhid_nos)
# 
# add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_hhid_nos")

# others checks -----------------------------------------------------------

df_others_data <- extract_other_specify_data(input_tool_data = df_tool_data %>% mutate(feedback_mechanism_prefered_social_media = NA),
                                             input_survey = df_survey, 
                                             input_choices = df_choices)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_others_data")

# logical checks ----------------------------------------------------------


# Respondent says relation with the community is good but reported disputes within the community i.e. 
# ${rank_rship_within_ref_community} = 'poor' or ${rank_rship_within_ref_community} = 'very_poor'
# AND ${main_security_issues_faced_list} = 'disputes_within_the_community'
df_community_relations_1 <- df_tool_data %>% 
  filter(rank_rship_within_ref_community %in% c("good", "very_good") & 
           str_detect(string = main_security_issues_faced, pattern = "disputes_within_the_community")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "rank_rship_within_ref_community",
         i.check.current_value = rank_rship_within_ref_community,
         i.check.value = "",
         i.check.issue_id = "logic_c_community_relations_1",
         i.check.issue = glue("rank_rship_within_ref_community: {rank_rship_within_ref_community}, but main_security_issues_faced: {main_security_issues_faced}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_community_relations_1")


# Respondent says he received assistance in the past 6 months that is related to livelihood but declared having received no livelihood 
# assistance in the past 6 months i.e. ${kind_asistance_received_list} = 'training_for_improving_skills'AND 
# ${hh_received_livelihood_support} = 'no'
df_hh_received_livelihood_support_2 <- df_tool_data %>%
  filter(hh_received_livelihood_support %in% c("no") & 
           str_detect(string = kind_assistance_received, pattern = "training_for_improving_skills"))%>%
  mutate(i.check.type = "change_response",
         i.check.name = "hh_received_livelihood_support",
         i.check.current_value = hh_received_livelihood_support,
         i.check.value = "yes",
         i.check.issue_id = "logic_c_hh_received_livelihood_support_2",
         i.check.issue = glue("kind_assistance_received: {kind_assistance_received},
                              but hh_received_livelihood_support: {hh_received_livelihood_support}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check.")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_received_livelihood_support_2")


# Respondent says he did not receive assistance in the past 6 months but did say he received assistance related to livelihood in the past 6 months
# i.e. {hh_received_livelihood_support}='yes' and ${received_humanitarian_assistance} = 'no' 

df_received_humanitarian_assistance_3 <- df_tool_data %>%
  filter(hh_received_livelihood_support %in% c("yes") & received_humanitarian_assistance %in% c("no")) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "received_humanitarian_assistance",
         i.check.current_value = received_humanitarian_assistance,
         i.check.value = "yes",
         i.check.issue_id = "logic_c_received_humanitarian_assistance_3",
         i.check.issue = glue("received_humanitarian_assistance: {received_humanitarian_assistance},
                              but hh_received_livelihood_support: {hh_received_livelihood_support}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check.")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_received_humanitarian_assistance_3")


# Respondent says he never reached out to community structures for help but also replied he asked for help to community structures in other questions
# ${reaching_out_to_community_structures} = 'no' AND ${registration_challenges_coping_strategy} = 'asked_for_help_to_local_leaders_religious_leaders_' 
# or ${legal_service_challenges_coping_strategy} = 'asked_for_help_to_local_leaders_religious_leaders_' or ${health_service_challenge_coping_strategy} =
# 'asked_for_help_to_local_leaders_religious_leaders_' or ${livelihood_challenge_coping_strategy} = 'asked_for_help_to_local_leaders_religious_leaders_' 
# or  ${mental_health_service_challenge_coping_strategy} = 'asked_for_help_to_local_leaders_religious_leaders_' or 
# ${police_service_challenge_coping_strategy} =  'asked_for_help_to_local_leaders_religious_leaders_' or ${security_or_safety_challenge_coping_strategy} = 
# 'asked_for_help_to_local_leaders_religious_leaders_' or ${shelter_challenge_coping_strategy} = 'asked_for_help_to_local_leaders_religious_leaders_' or
# ${water_access_challenge_coping_strategy} = 'asked_for_help_to_local_leaders_religious_leaders_' or ${child_not_attending_school_coping_strategy} = 
# 'asked_for_help_to_local_leaders_religious_leaders_'
df_reaching_out_to_community_structures_4 <- df_tool_data %>%
  filter(reaching_out_to_community_structures %in% c("no") & 
           if_any(c(registration_challenges_coping_strategy, legal_service_challenges_coping_strategy, health_service_challenge_coping_strategy, 
                    livelihood_challenge_coping_strategy, mental_health_service_challenge_coping_strategy, police_service_challenge_coping_strategy, 
                    security_or_safety_challenge_coping_strategy, shelter_challenge_coping_strategy, water_access_challenge_coping_strategy,
                    child_not_attending_school_coping_strategy), ~str_detect(., "asked_for_help_to_local_leaders_religious_leaders_"))) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "reaching_out_to_community_structures",
         i.check.current_value = reaching_out_to_community_structures,
         i.check.value = "yes",
         i.check.issue_id = "logic_c_reaching_out_to_community_structures_4",
         i.check.issue = glue("reaching_out_to_community_structures: {reaching_out_to_community_structures}, but on some coping strategy qns, respondent reached out to community structures"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check.")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_reaching_out_to_community_structures_4")


# Respondent says needs of the household have increased but also says no need for assistance i.e. 
# ${hh_changes_due_to_aid} = 'the_household_needs_have_increased' AND ${future_assistance_type_prefered} = 'do_not_want_to_receive_assistance'
df_hh_changes_due_to_aid_5 <- df_tool_data %>% 
  filter(hh_changes_due_to_aid %in% c("the_household_needs_have_increased") &  
           str_detect(string = future_assistance_type_prefered, pattern = "do_not_want_to_receive_assistance")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "hh_changes_due_to_aid",
         i.check.current_value = hh_changes_due_to_aid,
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_changes_due_to_aid_5",
         i.check.issue = glue("hh_changes_due_to_aid: {hh_changes_due_to_aid}, but future_assistance_type_prefered: {future_assistance_type_prefered}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_changes_due_to_aid_5")


# Respondent replies he has been in the settlement longer than its age allows. i.e. ${date_arrival} > (${today} - ${respondent_age})

# df_hh_changes_due_to_aid_6 <- df_tool_data %>%
#   filter(as.numeric(respondent_age) >= (as_date(today()) - as_date(date_arrival))) %>%
#   mutate(i.check.type = "change_response",
#          i.check.name = "respondent_age",
#          i.check.current_value = as.numeric(respondent_age),
#          i.check.value = "",
#          i.check.issue_id = "logic_c_hh_changes_due_to_aid_7",
#          i.check.issue = glue("respondent_age: {respondent_age}, but date_arrival: {date_arrival}"),
#          i.check.other_text = "",
#          i.check.checked_by = "",
#          i.check.checked_date = as_date(today()),
#          i.check.comment = "",
#          i.check.reviewed = "",
#          i.check.adjust_log = "",
#          i.check.so_sm_choices = "") %>%
#   dplyr::select(starts_with("i.check.")) %>%
#   rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
# 
# add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_changes_due_to_aid_6")
# 
# write_csv(x = df_hh_changes_due_to_aid_6, file = "outputs/age.csv")

# Respondent is not HoH & did not include HoH details in the household composition i.e. hoh_yn == "no" & relationship_to_hoh != hh_head in the hh roster

df_hoh_details_and_hh_roster_6 <- df_repeat_hh_roster_data %>%
  filter(hoh_yn == "no")  %>%
  group_by(`_uuid`) %>%
  mutate(int.hoh_bio = ifelse(relation_to_hoh_hhmembers %in% c("hh_head"), "given", "not")) %>% 
  filter(!str_detect(string = paste(int.hoh_bio, collapse = ":"), pattern = "given")) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "relation_to_hoh_hhmembers ",
         i.check.current_value = relation_to_hoh_hhmembers,
         i.check.value = "",
         i.check.issue_id = "logic_c_hoh_details_and_hh_roster_6",
         i.check.issue = glue("relation_to_hoh_hhmembers : {relation_to_hoh_hhmembers}, hoh not in roster"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check.")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hoh_details_and_hh_roster_6")


# Respondent willing to participate in another exercise i.e. future_survey_participation "yes" and respondent_telephone given

df_hh_wiiling_to_participate_7 <- df_tool_data %>% 
  filter(future_survey_participation %in% c("yes") & !is.na(number)) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "number",
         i.check.current_value = number,
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_wiiling_to_participate_7",
         i.check.issue = glue("respondent willing to participate in another exercise"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_wiiling_to_participate_7")


# combine and output checks -----------------------------------------------

# combine checks
df_combined_checks <- bind_rows(logic_output) %>% 
  rename(hh_id = point_number) %>% 
  mutate(name = case_when(name %in%c("point_number") ~ "hh_id",
                          TRUE ~ name))
# output the combined checks
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "combined_checks_PA.csv"), na = "")