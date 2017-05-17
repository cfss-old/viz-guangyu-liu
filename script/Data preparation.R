#---
# May 15, 2017
# Final Project -- Data Preparation
#---

library(tidyr)
library(tidyverse)
library(dplyr)

# Read data, remove weight (since there is no cross panel computation)
raw_data <- read.csv("data/raw_data.csv") %>% 
  select(-1, -F4QWT, -F4BYPNWT, -F4PNLWT, -F4F1PNWT, -F4F2PNWT, -F4CXTWT, -F4PAQWT, -F4TRSCWT, -F4QWT92G)

# Rename Variables
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
                     "F3_hsstatus",
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
  raw_data$F3_hsstatus[F3_hsstatus == -9 | F3_hsstatus == -6] <- NA
  raw_data$F3_occ[F3_occ < 0] <- NA
  raw_data$F3_inc[F3_inc < 0] <- NA
  raw_data$F3_PSE_mo[F3_PSE_mo == 99 | F3_PSE_mo == -9 | F3_PSE_mo == -6] <- NA
  raw_data$F4_occ[F4_occ < 0] <- NA
  raw_data$F4_inc[F4_inc < 0] <- NA
  raw_data$F4_edu[F4_edu < 0] <- NA
  
  detach(raw_data)

# Handle Variable Name, Label and Levels of Factors
  attach(raw_data)
  
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
  
  raw_data$F3_hsstatus <- factor(F3_hsstatus,
                                 levels = c(1:6),
                                 labels = c("HS diploma",
                                            "GED diploma",
                                            "Cert attend",
                                            "Enrolled in HS",
                                            "Work equivalent HS diploma",
                                            "Not grad/not work on"))
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
                                       "Manage-manufactory/Construction",
                                       "Manage-admin-other",
                                       "Military",
                                       "Skilled operative",
                                       "Prof-arts/entertainment",
                                       "Prof-medicine(not MD)",
                                       "Prof-engineer",
                                       "Prof-physician",
                                       "Professional-other",
                                       "Owner-retail/hospitality",
                                       "Owner-manufactory/construction",
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
  detach(raw_data)

# Compute prestige for F3 and F4 occupations, based on NORC - 1980 Census Occupational Category
  raw_data <- mutate(raw_data, F3_prestige = NA)
  
  attach(raw_data)
  
  raw_data$F3_prestige[F3_occ == "Clerical-secretarial"] <- 46.08
  raw_data$F3_prestige[F3_occ == "Clerical-financial"] <- 48.40
  raw_data$F3_prestige[F3_occ == "Clerical-other"] <- 38.16
  raw_data$F3_prestige[F3_occ == "Craftsmen"] <- 38.51
  raw_data$F3_prestige[F3_occ == "Farmer/Farm manager"] <- 43.24
  raw_data$F3_prestige[F3_occ == "Laborer"] <- 33.38
  raw_data$F3_prestige[F3_occ == "Manager/Admn-sales"] <- 51.43
  raw_data$F3_prestige[F3_occ == "Manager/Admn-goverment"] <- 51.23
  raw_data$F3_prestige[F3_occ == "Manager/Admn-retail"] <- 50.11
  raw_data$F3_prestige[F3_occ == "Manage-manufactory/Construction"] <- 46.85
  raw_data$F3_prestige[F3_occ == "Manage-admin-other"] <- 50.64
  raw_data$F3_prestige[F3_occ == "Military"] <- 62
  raw_data$F3_prestige[F3_occ == "Skilled operative"] <- 37.42
  raw_data$F3_prestige[F3_occ == "Prof-arts/entertainment"] <- 57.62
  raw_data$F3_prestige[F3_occ == "Prof-medicine(not MD)"] <- 64.27
  raw_data$F3_prestige[F3_occ == "Prof-engineer"] <- 64.87
  raw_data$F3_prestige[F3_occ == "Prof-physician"] <- 86.05
  raw_data$F3_prestige[F3_occ == "Professional-other"] <- 64.38
  raw_data$F3_prestige[F3_occ == "Owner-retail/hospitality"] <- 44.15
  raw_data$F3_prestige[F3_occ == "Owner-manufactory/construction"] <- 46.85
  raw_data$F3_prestige[F3_occ == "Owner-other"] <- 53.52
  raw_data$F3_prestige[F3_occ == "Protective service"] <- 48.40
  raw_data$F3_prestige[F3_occ == "Sales"] <- 35.77
  raw_data$F3_prestige[F3_occ == "School teacher"] <- 61.61
  raw_data$F3_prestige[F3_occ == "Service"] <- 34.95
  raw_data$F3_prestige[F3_occ == "Technical-computer"] <- 60.51
  raw_data$F3_prestige[F3_occ == "Technical-non computer"] <- 51.21
  raw_data$F3_prestige[F3_occ == "Not working"] <- 0.00
  
  raw_data <- mutate(raw_data, F4_prestige = NA)
  raw_data$F4_prestige[F4_occ == "Secretaries and receptionists"] <- 42.55
  raw_data$F4_prestige[F4_occ == "Cashiers, tellers, sales clerks"] <- 35.44
  raw_data$F4_prestige[F4_occ == "Clerks, data entry"] <- 41.18
  raw_data$F4_prestige[F4_occ == "Clerical other"] <- 38.16
  raw_data$F4_prestige[F4_occ == "Farmers, foresters, farm laborers"] <- 35.57
  raw_data$F4_prestige[F4_occ == "Personal services"] <- 25.41
  raw_data$F4_prestige[F4_occ == "Cooks, chefs, bakers, cake decorators"] <- 32.81
  raw_data$F4_prestige[F4_occ == "Laborers(other than farm)"] <- 33.38
  raw_data$F4_prestige[F4_occ == "Mechanic, repairer, service technicians"] <- 33.38
  raw_data$F4_prestige[F4_occ == "Craftsmen"] <- 38.51
  raw_data$F4_prestige[F4_occ == "Skilled operatives"] <- 37.42
  raw_data$F4_prestige[F4_occ == "Transport operatives (not pilots)"] <- 35.94
  raw_data$F4_prestige[F4_occ == "Protective services, criminal justice"] <- 48.40
  raw_data$F4_prestige[F4_occ == "Military"] <- 62
  raw_data$F4_prestige[F4_occ == "Business/financial support services"] <- 48.40
  raw_data$F4_prestige[F4_occ == "Financial services professionals"] <- 52.80
  raw_data$F4_prestige[F4_occ == "Sales/purchasing"] <- 40.99
  raw_data$F4_prestige[F4_occ == "Customer service"] <- 31
  raw_data$F4_prestige[F4_occ == "Legal professionals"] <- 74.77
  raw_data$F4_prestige[F4_occ == "Legal support"] <- 56.53
  raw_data$F4_prestige[F4_occ == "Medical practice professionals"] <- 61.53
  raw_data$F4_prestige[F4_occ == "Medical licensed professionals"] <- 78.92
  raw_data$F4_prestige[F4_occ == "Medical services"] <- 55.93
  raw_data$F4_prestige[F4_occ == "Educators-K-12 teachers"] <- 59.23
  raw_data$F4_prestige[F4_occ == "Educators-instructors other than K-12"] <- 73.51
  raw_data$F4_prestige[F4_occ == "Human services professionals"] <- 51.50
  raw_data$F4_prestige[F4_occ == "Engineers architects software engineers"] <- 73.15
  raw_data$F4_prestige[F4_occ == "Scientist, statistician professionals"] <- 55.57
  raw_data$F4_prestige[F4_occ == "Research assistant/lab technicians"] <- 68.40
  raw_data$F4_prestige[F4_occ == "Technical/professional workers, other"] <- 52.41
  raw_data$F4_prestige[F4_occ == "Computer systems/related professionals"] <- 73.61
  raw_data$F4_prestige[F4_occ == "Computer programmers"] <- 60.51
  raw_data$F4_prestige[F4_occ == "Computer/computer equipment operators"] <- 50.32
  raw_data$F4_prestige[F4_occ == "Editors, writers, reporters"] <- 59.75
  raw_data$F4_prestige[F4_occ == "Performers/artists"] <- 35.55
  raw_data$F4_prestige[F4_occ == "Managers-executive"] <- 70.45
  raw_data$F4_prestige[F4_occ == "Managers-midlevel"] <- 59.79
  raw_data$F4_prestige[F4_occ == "Managers-supervisory, office, other admin"] <- 48.72
  raw_data$F4_prestige[F4_occ == "Health/recreation services"] <- 38.06
  raw_data$F4_prestige[F4_occ == "Unemployed-homemakers"] <- 27.84
  raw_data$F4_prestige[F4_occ == "Unemployed-other"] <- 0.00
  
  detach(raw_data)
    
# Origin Family's SES Percentile
  raw_data <- mutate(raw_data, Ori_perc = cume_dist(BY_SES) * 100)

# BY, F1 and F2 Standardized Test Percentile (composite - reading and math)
  raw_data <- mutate(raw_data, 
                     BY_perc_c = cume_dist(BY_stdsc_c) * 100,
                     F1_perc_c = cume_dist(F1_stdsc_c) * 100,
                     F2_perc_c = cume_dist(F2_stdsc_c) * 100)
  # All Four Disciplines
  raw_data <- mutate(raw_data,
                     BY_perc_r = cume_dist(BY_stdsc_r) * 100,
                     BY_perc_m = cume_dist(BY_stdsc_m) * 100,
                     BY_perc_s = cume_dist(BY_stdsc_s) * 100,
                     BY_perc_h = cume_dist(BY_stdsc_h) * 100,
                     F1_perc_r = cume_dist(F1_stdsc_r) * 100,
                     F1_perc_m = cume_dist(F1_stdsc_m) * 100,
                     F1_perc_s = cume_dist(F1_stdsc_s) * 100,
                     F1_perc_h = cume_dist(F1_stdsc_h) * 100,
                     F2_perc_r = cume_dist(F2_stdsc_r) * 100,
                     F2_perc_m = cume_dist(F2_stdsc_m) * 100,
                     F2_perc_s = cume_dist(F2_stdsc_s) * 100,
                     F2_perc_h = cume_dist(F2_stdsc_h) * 100)
  
# F3 and F4 Compute Respondent's SES Percentile
  # Calculate F3_edu
  raw_data <- mutate(raw_data, F3_edu = ifelse(!is.na(raw_data$F3_PSE_mo), 3,
                                               ifelse(raw_data$F3_hsstatus == "HS diploma", 2, 1)))
  raw_data$F3_edu <- factor(raw_data$F3_edu,
                            levels = c(1:3),
                            labels = c("Less than HS",
                                       "HS degree",
                                       "Some PSE"))
  
  # If F4_edu == NA (no PSE), then check if has HS degree
  raw_data <- mutate(raw_data, F4_edu2 = F4_edu)
  raw_data$F4_edu2 <- factor(raw_data$F4_edu2,
                             levels = c(1:6),
                             labels = c("Less than HS",
                                        "HS degree",
                                        "Some PSE",
                                        "Bachelor's degree",
                                        "Master's degree/equivalent",
                                        "Ph.D or a professional degree"))
  raw_data$F4_edu2[raw_data$F4_edu == "Some PSE, no degree attained" |
                     raw_data$F4_edu == "Certificate/license" |
                     raw_data$F4_edu == "Associate's degree"] <- "Some PSE"
  raw_data$F4_edu2[raw_data$F4_edu == "Bachelor's degree"] <- "Bachelor's degree" 
  raw_data$F4_edu2[raw_data$F4_edu == "Master's degree/equivalent"] <- "Master's degree/equivalent"
  raw_data$F4_edu2[raw_data$F4_edu == "Ph.D or a professional degree"] <- "Ph.D or a professional degree"
  raw_data$F4_edu2[is.na(raw_data$F4_edu) & raw_data$F3_edu == "Less than HS"] <- "Less than HS"
  raw_data$F4_edu2[is.na(raw_data$F4_edu) & raw_data$F3_edu == "HS degree"] <- "HS degree"
  raw_data$F4_edu2[is.na(raw_data$F4_edu) & raw_data$F3_edu == "Some PSE"] <- "Some PSE"

  raw_data <- select(raw_data, -F4_edu, F4_edu = F4_edu2)
  
  # Calculate F3 and F4 SES percentile
  raw_data <- mutate(raw_data, 
                     F3_edu_perc = cume_dist(raw_data$F3_edu) * 100,
                     F3_occ_perc = cume_dist(raw_data$F3_prestige) * 100,
                     F3_inc_perc = cume_dist(raw_data$F3_inc) * 100,
                     F4_edu_perc = cume_dist(raw_data$F4_edu) * 100,
                     F4_occ_perc = cume_dist(raw_data$F4_prestige) * 100,
                     F4_inc_perc = cume_dist(raw_data$F4_inc) * 100)
  raw_data <- mutate(raw_data,
                     F3_com_perc = cume_dist(raw_data$F3_edu_perc + raw_data$F3_occ_perc + raw_data$F3_inc_perc),
                     F4_com_perc = cume_dist(raw_data$F4_edu_perc + raw_data$F4_occ_perc + raw_data$F4_inc_perc))

# Select demographic variables and all rankings (percentile)
  nels88 <- select(raw_data, ID, BY_momedu, BY_dadedu, sex, race, BY_faminc, contains("perc"))
  
# Write data
  write_csv(nels88, "data/nels88.csv")
  