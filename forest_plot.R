
library("fastmap")
library("devtools")
library(forester)
library(forestploter)
library(tidyverse)
library(readxl)

analysisdata_c <- read_excel("analysisdata.xlsx", sheet = "crude") %>% 
  mutate(across(c(HR:CI_high), ~ as.numeric(.x)),
         CI_low = round(CI_low, digits = 3),
         contents = case_when(contents=="Exercise and walking" | contents=="Exercise or walking" | contents=="No physical activity" ~ paste0("  　　  ", contents),
                              contents=="All participants" | contents=="Age groups" | contents=="Cancer type groups" ~ contents,
                              TRUE ~ paste0("　  ", contents))) %>% 
  rename(Groups = contents)

analysisdata <- read_excel("analysisdata.xlsx", sheet = "adjust") %>% 
  mutate(across(c(HR:CI_high), ~ as.numeric(.x)),
         CI_low = round(CI_low, digits = 3),
         contents = case_when(contents=="Exercise and walking" | contents=="Exercise or walking" | contents=="No physical activity" ~ paste0("  　　  ", contents),
                              contents=="All participants" | contents=="Age groups" | contents=="Cancer type groups" ~ contents,
                              TRUE ~ paste0("　  ", contents))) %>% 
  rename(Groups = contents)

  forester(left_side_data = analysisdata_c[, 1],
           estimate = analysisdata_c$HR,
           ci_low = analysisdata_c$CI_low,
           ci_high = analysisdata_c$CI_high,
           estimate_precision = 2,
           estimate_col_name = "Hazard ratio (95% Confidence interval)",
           null_line_at = 1,
           font_family = "sans",
           xlim = c(0, 3.5),
           arrows = TRUE, 
           arrow_labels = c("", "Increase the onset of outcome"), 
           display = TRUE)
  
  forester(left_side_data = analysisdata[, 1],
           estimate = analysisdata$HR,
           ci_low = analysisdata$CI_low,
           ci_high = analysisdata$CI_high,
           estimate_precision = 2,
           estimate_col_name = "Hazard ratio (95% Confidence interval)",
           null_line_at = 1,
           font_family = "sans",
           xlim = c(0, 3.5),
           arrows = TRUE, 
           arrow_labels = c("", "Increase the onset of outcome"), 
           display = TRUE)
  
  