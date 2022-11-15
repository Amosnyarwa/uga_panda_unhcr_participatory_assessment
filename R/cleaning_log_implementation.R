# using the cleaning log to clean the data

library(tidyverse)
library(lubridate)
library(glue)

source("R/composite_indicators.R")

# read data

df_cleaning_log <- read_csv("inputs/combined_checks_pa.csv") %>% 
  filter(!adjust_log %in% c("delete_log")) %>%
  mutate(sheet = NA, index = NA, relevant = NA) %>% 
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) %>%
  filter(!is.na(value), !is.na(uuid)) %>%
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         relevant = NA) %>%
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

data_nms <- names(readxl::read_excel(path = "inputs/UNHCR_PA_Assessment_Data.xlsx", n_max = 100))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = "inputs/UNHCR_PA_Assessment_Data.xlsx", col_types = c_types) %>% 
  filter(as_date(as_datetime(start)) > as_date("2022-07-27")) %>%
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., 
                                                                pattern = fixed(pattern = "N/A", ignore_case = TRUE)), 
                                                     "NA", .))) %>% 
  mutate(start = as_datetime(start), end = as_datetime(end), today = as_date(as_datetime(today)), date_arrival = as_date(as_datetime(date_arrival)))

df_survey <- readxl::read_excel("inputs/UNHCR_PA_Assessment_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/UNHCR_PA_Assessment_Tool.xlsx", sheet = "choices")



# find all new choices to add to choices sheet ----------------------------

# gather choice options based on unique choices list
df_grouped_choices<- df_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))

# get new name and ad_option pairs to add to the choices sheet
new_vars <- df_cleaning_log %>% 
  filter(type %in% c("change_response", "add_option")) %>% 
  left_join(df_survey, by = "name") %>% 
  filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>% 
  separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>% 
  left_join(df_grouped_choices, by = "list_name") %>%
  filter(!str_detect(string = choice_options, pattern = value ) ) %>%
  rename(choice = value ) %>%
  select(name, choice) %>%
  distinct() %>% # to make sure there are no duplicates
  arrange(name)


# create kobold object ----------------------------------------------------

kbo <- kobold::kobold(survey = df_survey, 
                      choices = df_choices, 
                      data = df_raw_data, 
                      cleaning = df_cleaning_log)


# modified choices for the survey tool ------------------------------------

df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)

# special treat for variables for select_multiple, we need to add the columns to the data itself

df_survey_sm <- df_survey %>% 
  mutate(q_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                            str_detect(string = type, pattern = "select_one|select one") ~ "so",
                            TRUE ~ type)) %>% 
  select(name, q_type)

# construct new columns for select multiple

new_vars_sm <- new_vars %>% 
  left_join(df_survey_sm, by = "name") %>% 
  filter(q_type == "sm") %>% 
  mutate(new_cols = paste0(name,"/",choice))


# add new columns to the raw data -----------------------------------------

df_raw_data_modified <- df_raw_data %>% 
  butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )


# make some clean up ------------------------------------------------------

kbo_modified <- kobold::kobold(survey = df_survey %>% filter(name %in% colnames(df_raw_data_modified)), 
                               choices = df_choises_modified, 
                               data = df_raw_data_modified, 
                               cleaning = df_cleaning_log )
kbo_cleaned <- kobold::kobold_cleaner(kbo_modified)


# handling Personally Identifiable Information(PII)
input_vars_to_remove_from_data <- c("deviceid", 
                                    "audit",
                                    "audit_URL",
                                    "instance_name",
                                    "complainant_name",
                                    "complainant_id",
                                    "respondent_telephone",
                                    "name_pers_recording",
                                    "geopoint",
                                    "_geopoint_latitude",
                                    "_geopoint_longitude",
                                    "_geopoint_altitude",
                                    "_geopoint_precision")

df_handle_pii <- kbo_cleaned$data %>% 
  mutate(across(any_of(input_vars_to_remove_from_data), .fns = ~na_if(., .)))

# handling added responses after starting data collection and added responses in the cleaning process

sm_colnames <-  df_handle_pii %>% 
  select(contains("/")) %>% 
  colnames() %>% 
  str_replace_all(pattern = "/.+", replacement = "") %>% 
  unique()

df_handle_sm_data <- df_handle_pii

for (cur_sm_col in sm_colnames) {
  df_updated_data <- df_handle_sm_data %>% 
    mutate(
      across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(!is.na(!!sym(cur_sm_col)) & is.na(.) , FALSE, .)),
      across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(is.na(!!sym(cur_sm_col)), NA, .))
    )
  df_handle_sm_data <- df_updated_data
}

df_final_cleaned_data <- df_handle_sm_data


# write final modified data -----------------------------------------------

write_csv(df_final_cleaned_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data_pa.csv"))
write_csv(df_final_cleaned_data, file = paste0("inputs/clean_data_pa.csv"))

# output data with composite indicators

df_with_composites <- create_composite_indicators_anif(input_df = df_final_cleaned_data)

write_csv(df_with_composites, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data_with_composite_indicators.csv"))
