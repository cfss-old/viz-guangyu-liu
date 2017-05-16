#---
# May 15, 2017
# Final Project -- Data Preparation
#---

library(tidyr)
library(tidyverse)
library(dplyr)

# Read data, remove weight (since there is no cross panel computation) and duplicate variables
raw_data <- read.csv("raw_data.csv") %>% 
  select(-1, -BYP80, -F4QWT, -F4BYPNWT, -F4PNLWT, -F4F1PNWT, -F4F2PNWT, -F4CXTWT, -F4PAQWT, -F4TRSCWT, -F4QWT92G)

# Rename Variabels
names(raw_data) <- c("ID",
                     "BY_momocc",
                     "BY_dadocc",
                     "BY_dadedu",
                     "BY_momedu",
                     "sex",
                     "race",
                     "birth_mo",
                     "birth_yr",
                     "BY_SES",
                     "BY_faminc",
                     "BY_stdsc_r",
                     "BY_stdsc_m",
                     "BY_stdsc_s",
                     "BY_stdsc_h",
                     "BY_stdsc_c",
                     "F1_stdsc_r",
                     "F1_stdsc_m",
                     "F1_stdsc_s",
                     "F1_stdsc_h",
                     "F1_stdsc_c",
                     "F2_stdsc_r",
                     "F2_stdsc_m",
                     "F2_stdsc_s",
                     "F2_stdsc_h",
                     "F2_stdsc_c",
                     "F3_occ",
                     "F3_inc",
                     "F3_PSE_mo",
                     "F4_occ",
                     "F4_inc",
                     "F4_edu")

# Handle Missing Values
attach(raw_data)
raw_data$BY_momocc[BY_momocc == 99 | BY_momocc == 98] <- NA # 98: Missing value; 99: Legitimate skip
raw_data$BY_dadocc[BY_dadocc == 99 | BY_dadocc == 98] <- NA
raw_data$BY_dadedu[BY_dadedu == 99 | BY_dadedu == 98 | BY_dadedu == 97 | BY_dadedu == 8] <- NA # 97: Refusal; 8: Dont know
raw_data$BY_momedu[BY_momedu == 99 | BY_momedu == 98 | BY_momedu == 97 | BY_momedu == 8] <- NA
raw_data$sex[sex == 9] <- NA # Legitimate skip
raw_data$race[race == 9 | race ==8] <- NA # 8: Missing; 9: Legitimate skip
raw_data$birth_mo[birth_mo == 99 | birth_mo == 98] <- NA
raw_data$birth_yr[birth_yr == 99 | birth_yr == 98] <- NA
raw_data$BY_SES[BY_SES == 99.999] <- NA # Legitimate skip
raw_data$BY_faminc[BY_faminc == 99 | BY_faminc == 98] <- NA
raw_data$BY_stdsc_r[BY_stdsc_r == 99.99 | BY_stdsc_r == 99.98 | BY_stdsc_r == -9.00] <- NA
raw_data$BY_stdsc_m[BY_stdsc_m == 99.99 | BY_stdsc_m == 99.98 | BY_stdsc_m == -9.00] <- NA
raw_data$BY_stdsc_s[BY_stdsc_s == 99.99 | BY_stdsc_s == 99.98 | BY_stdsc_s == -9.00] <- NA
raw_data$BY_stdsc_h[BY_stdsc_h == 99.99 | BY_stdsc_h == 99.98 | BY_stdsc_h == -9.00] <- NA
raw_data$BY_stdsc_c[BY_stdsc_c == 99.99 | BY_stdsc_c == 99.98 | BY_stdsc_c == -9.00] <- NA
raw_data$F1_stdsc_r[F1_stdsc_r == 99.99 | F1_stdsc_r == 99.98 | F1_stdsc_r == -9.00] <- NA
raw_data$F1_stdsc_m[F1_stdsc_m == 99.99 | F1_stdsc_m == 99.98 | F1_stdsc_m == -9.00] <- NA
raw_data$F1_stdsc_s[F1_stdsc_s == 99.99 | F1_stdsc_s == 99.98 | F1_stdsc_s == -9.00] <- NA
raw_data$F1_stdsc_h[F1_stdsc_h == 99.99 | F1_stdsc_h == 99.98 | F1_stdsc_h == -9.00] <- NA
raw_data$F1_stdsc_c[F1_stdsc_c == 99.99 | F1_stdsc_c == 99.98 | F1_stdsc_c == -9.00] <- NA
raw_data$F2_stdsc_r[F2_stdsc_r == 99.99 | F2_stdsc_r == 99.98 | F2_stdsc_r == -9.00] <- NA
raw_data$F2_stdsc_m[F2_stdsc_m == 99.99 | F2_stdsc_m == 99.98 | F2_stdsc_m == -9.00] <- NA
raw_data$F2_stdsc_s[F2_stdsc_s == 99.99 | F2_stdsc_s == 99.98 | F2_stdsc_s == -9.00] <- NA
raw_data$F2_stdsc_h[F2_stdsc_h == 99.99 | F2_stdsc_h == 99.98 | F2_stdsc_h == -9.00] <- NA
raw_data$F2_stdsc_c[F2_stdsc_c == 99.99 | F2_stdsc_c == 99.98 | F2_stdsc_c == -9.00] <- NA
raw_data$F3_occ[F3_occ < 0] <- NA
raw_data$F3_inc[F3_inc < 0] <- NA
raw_data$F3_PSE_mo[F3_PSE_mo == 99 | F3_PSE_mo == -9 | F3_PSE_mo == -6] <- NA
raw_data$F4_occ[F4_occ < 0] <- NA
raw_data$F4_inc[F4_inc < 0] <- NA
raw_data$F4_edu[F4_edu < 0] <- NA

# Handle Variable Name, Label and Levels of Factors
raw_data$BY_momocc <- factor(BY_momocc,
                             levels = c(1:19),
                             labels = c("Clerical",
                                        "Craftsperson",
                                        "Farmer, Farm manager",
                                        "Homemaker",
                                        "Laborer",
                                        "Manager/Administrator",
                                        "Military",
                                        "Operative",
                                        "Professional 1",
                                        "Professional 2",
                                        "Proprietor/Owner",
                                        "Protective Service",
                                        "Sales",
                                        "School teacher",
                                        "Service",
                                        "Technical",
                                        "Never worked",
                                        "Dont know",
                                        "Student"))
raw_data$BY_dadocc <- factor(BY_dadocc,
                             levels = c(1:19),
                             labels = c("Clerical",
                                        "Craftsperson",
                                        "Farmer, Farm manager",
                                        "Homemaker",
                                        "Laborer",
                                        "Manager/Administrator",
                                        "Military",
                                        "Operative",
                                        "Professional 1",
                                        "Professional 2",
                                        "Proprietor/Owner",
                                        "Protective Service",
                                        "Sales",
                                        "School teacher",
                                        "Service",
                                        "Technical",
                                        "Never worked",
                                        "Dont know",
                                        "Student"))
raw_data$BY_dadedu <- factor(BY_dadedu,
                             levels = c(1:7),
                             labels = c("Not finish High School",
                                        "Graduated High School",
                                        "Junior College",
                                        "College Less Than 4 Years",
                                        "Graduated College",
                                        "Masters Degree",
                                        "PhD, M.D., etc."))
raw_data$BY_momedu <- factor(BY_momedu,
                             levels = c(1:7),
                             labels = c("Not finish High School",
                                        "Graduated High School",
                                        "Junior College",
                                        "College Less Than 4 Years",
                                        "Graduated College",
                                        "Masters Degree",
                                        "PhD, M.D., etc."))
raw_data$sex <- factor(sex,
                       levels = c(1, 2),
                       labels = c("Male", "Female"))

raw_data$race <- factor(race,
                        levels = c(1:5),
                        labels = c("Asian/Pacific Islander",
                                   "Hispanic",
                                   "Black Not Hispanic",
                                   "White Not Hispanic",
                                   "American Indian/Alaska Native"))
raw_data$F3_occ <- factor(F3_occ,
                          levels = c(1:5,7:18,20:30),
                          labels = c("Clerical-secretarial",
                                     "Clerical-financial",
                                     "Clerical-other",
                                     "Craftsmen",
                                     "Farmer/Farm manager",
                                     "Laborer",
                                     "Manager/Admn-sales",
                                     "Manager/Admn-goverment",
                                     "Manager/Admn-retail",
                                     "Manage-manufactory/Consitution",
                                     "Manage-admin-other",
                                     "Military",
                                     "Skilled operative",
                                     "Prof-arts/entertainment",
                                     "Prof-medicine(not MD)",
                                     "Prof-engineer",
                                     "Prof-physician",
                                     "Professional-other",
                                     "Owner-retail/hospitality",
                                     "Owner-manufactory/constitution",
                                     "Owner-other",
                                     "Protective service",
                                     "Sales",
                                     "School teacher",
                                     "Service",
                                     "Technical-computer",
                                     "Technical-non computer",
                                     "Not working"))
raw_data$F4_occ <- factor(F4_occ,
                          levels = c(1:39, 41, 42),
                          labels = c("Secretaries and receptionists",
                                     "Cashiers, tellers, sales clerks",
                                     "Clerks, data entry",
                                     "Clerical other",
                                     "Farmers, foresters, farm laborers",
                                     "Personal services",
                                     "Cooks, chefs, bakers, cake decorators",
                                     "Laborers(other than farm)",
                                     "Mechanic, repairer, service technicians",
                                     "Craftsmen",
                                     "Skilled operatives",
                                     "Transport operatives (not pilots)",
                                     "Protective services, criminal justice",
                                     "Military",
                                     "Business/financial support services",
                                     "Financial services professionals",
                                     "Sales/purchasing",
                                     "Customer service",
                                     "Legal professionals",
                                     "Legal support",
                                     "Medical practice professionals",
                                     "Medical licensed professionals",
                                     "Medical services",
                                     "Educators-K-12 teachers",
                                     "Educators-instructors other than K-12",
                                     "Human services professionals",
                                     "Engineers architects software engineers",
                                     "Scientist, statistician professionals",
                                     "Research assistant/lab technicians",
                                     "Technical/professional workers, other",
                                     "Computer systems/related professionals",
                                     "Computer programmers",
                                     "Computer/computer equipment operators",
                                     "Editors, writers, reporters",
                                     "Performers/artists",
                                     "Managers-executive",
                                     "Managers-midlevel",
                                     "Managers-supervisory, office, other admin",
                                     "Health/recreation services",
                                     "Unemployed-homemakers",
                                     "Unemployed-other"))
raw_data$F4_edu <- factor(F4_edu,
                          levels = c(1:6),
                          labels = c("Some PSE, no degree attained",
                                     "Certificate/license",
                                     "Associate's degree",
                                     "Bachelor's degree",
                                     "Master's degree/equivalent",
                                     "Ph.D or a professional degree"))

# Origin Family's SES Percentile
# F1 and F2 Standardized Test Percentile
# F3 and F4 Compute Respondent's SES Percentile
  # Calculate F3_edu, considering drop out