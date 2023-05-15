# Health impacts of non-standard employment
# N.M. Kavanagh, T. Etienne, E. Courtin, J. Lynch
# May 15, 2023

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Load packages
library(here)         # Working directory
library(psych)        # Analysis tools
library(car)          # Analysis tools
library(dplyr)        # Analysis tools
library(arsenal)      # Analysis tools
library(readstata13)  # Dataset tools
library(lfe)          # Modeling tools
library(ggplot2)      # Graphing tools
library(cowplot)      # Graphing tools
library(arsenal)      # Table tools
library(modelsummary) # Table tools
library(kableExtra)   # Table tools

##############################################################################
# Understanding Society preparation
##############################################################################

# Read in individual-level data
a_indall  <- read.dta13("ukhls/a_indall.dta",  convert.factors=F)  # Wave 1
a_indresp <- read.dta13("ukhls/a_indresp.dta", convert.factors=F)
a_hhresp  <- read.dta13("ukhls/a_hhresp.dta",  convert.factors=F)

b_indall  <- read.dta13("ukhls/b_indall.dta",  convert.factors=F)  # Wave 2
b_indresp <- read.dta13("ukhls/b_indresp.dta", convert.factors=F)
b_hhresp  <- read.dta13("ukhls/b_hhresp.dta",  convert.factors=F)

c_indall  <- read.dta13("ukhls/c_indall.dta",  convert.factors=F)  # Wave 3
c_indresp <- read.dta13("ukhls/c_indresp.dta", convert.factors=F)
c_hhresp  <- read.dta13("ukhls/c_hhresp.dta",  convert.factors=F)

d_indall  <- read.dta13("ukhls/d_indall.dta",  convert.factors=F)  # Wave 4
d_indresp <- read.dta13("ukhls/d_indresp.dta", convert.factors=F)
d_hhresp  <- read.dta13("ukhls/d_hhresp.dta",  convert.factors=F)

e_indall  <- read.dta13("ukhls/e_indall.dta",  convert.factors=F)  # Wave 5
e_indresp <- read.dta13("ukhls/e_indresp.dta", convert.factors=F)
e_hhresp  <- read.dta13("ukhls/e_hhresp.dta",  convert.factors=F)

f_indall  <- read.dta13("ukhls/f_indall.dta",  convert.factors=F)  # Wave 6
f_indresp <- read.dta13("ukhls/f_indresp.dta", convert.factors=F)
f_hhresp  <- read.dta13("ukhls/f_hhresp.dta",  convert.factors=F)

g_indall  <- read.dta13("ukhls/g_indall.dta",  convert.factors=F)  # Wave 7
g_indresp <- read.dta13("ukhls/g_indresp.dta", convert.factors=F)
g_hhresp  <- read.dta13("ukhls/g_hhresp.dta",  convert.factors=F)

h_indall  <- read.dta13("ukhls/h_indall.dta",  convert.factors=F)  # Wave 8
h_indresp <- read.dta13("ukhls/h_indresp.dta", convert.factors=F)
h_hhresp  <- read.dta13("ukhls/h_hhresp.dta",  convert.factors=F)

i_indall  <- read.dta13("ukhls/i_indall.dta",  convert.factors=F)  # Wave 9
i_indresp <- read.dta13("ukhls/i_indresp.dta", convert.factors=F)
i_hhresp  <- read.dta13("ukhls/i_hhresp.dta",  convert.factors=F)

j_indall  <- read.dta13("ukhls/j_indall.dta",  convert.factors=F)  # Wave 10
j_indresp <- read.dta13("ukhls/j_indresp.dta", convert.factors=F)
j_hhresp  <- read.dta13("ukhls/j_hhresp.dta",  convert.factors=F)

k_indall  <- read.dta13("ukhls/k_indall.dta",  convert.factors=F)  # Wave 11
k_indresp <- read.dta13("ukhls/k_indresp.dta", convert.factors=F)
k_hhresp  <- read.dta13("ukhls/k_hhresp.dta",  convert.factors=F)

l_indall  <- read.dta13("ukhls/l_indall.dta",  convert.factors=F)  # Wave 12
l_indresp <- read.dta13("ukhls/l_indresp.dta", convert.factors=F)
l_hhresp  <- read.dta13("ukhls/l_hhresp.dta",  convert.factors=F)

# Merge waves
us_w01   <- merge(a_indall, a_indresp, by="pidp", all.y=T)
us_w01.2 <- merge(us_w01, a_hhresp, by.x="a_hidp.y", by.y="a_hidp", all.x=T)

us_w02 <- merge(b_indall, b_indresp, by="pidp", all.y=T)
us_w02.2 <- merge(us_w02, b_hhresp, by.x="b_hidp.y", by.y="b_hidp", all.x=T)

us_w03 <- merge(c_indall, c_indresp, by="pidp", all.y=T)
us_w03.2 <- merge(us_w03, c_hhresp, by.x="c_hidp.y", by.y="c_hidp", all.x=T)

us_w04 <- merge(d_indall, d_indresp, by="pidp", all.y=T)
us_w04.2 <- merge(us_w04, d_hhresp, by.x="d_hidp.y", by.y="d_hidp", all.x=T)

us_w05 <- merge(e_indall, e_indresp, by="pidp", all.y=T)
us_w05.2 <- merge(us_w05, e_hhresp, by.x="e_hidp.y", by.y="e_hidp", all.x=T)

us_w06 <- merge(f_indall, f_indresp, by="pidp", all.y=T)
us_w06.2 <- merge(us_w06, f_hhresp, by.x="f_hidp.y", by.y="f_hidp", all.x=T)

us_w07 <- merge(g_indall, g_indresp, by="pidp", all.y=T)
us_w07.2 <- merge(us_w07, g_hhresp, by.x="g_hidp.y", by.y="g_hidp", all.x=T)

us_w08 <- merge(h_indall, h_indresp, by="pidp", all.y=T)
us_w08.2 <- merge(us_w08, h_hhresp, by.x="h_hidp.y", by.y="h_hidp", all.x=T)

us_w09 <- merge(i_indall, i_indresp, by="pidp", all.y=T)
us_w09.2 <- merge(us_w09, i_hhresp, by.x="i_hidp.y", by.y="i_hidp", all.x=T)

us_w10 <- merge(j_indall, j_indresp, by="pidp", all.y=T)
us_w10.2 <- merge(us_w10, j_hhresp, by.x="j_hidp.y", by.y="j_hidp", all.x=T)

us_w11 <- merge(k_indall, k_indresp, by="pidp", all.y=T)
us_w11.2 <- merge(us_w11, k_hhresp, by.x="k_hidp.y", by.y="k_hidp", all.x=T)

us_w12 <- merge(l_indall, l_indresp, by="pidp", all.y=T)
us_w12.2 <- merge(us_w12, l_hhresp, by.x="l_hidp.y", by.y="l_hidp", all.x=T)

# Set number of waves
NUM_WAVES <- 12

# List of waves
list <- list(us_w01.2, us_w02.2, us_w03.2, us_w04.2, us_w05.2, us_w06.2,
             us_w07.2, us_w08.2, us_w09.2, us_w10.2, us_w11.2, us_w12.2)

# Label wave numbers
for(i in 1:NUM_WAVES) {list[[i]]$survey <- "understanding_society"}
for(i in 1:NUM_WAVES) {list[[i]]$wave   <- as.character(i)}

# Rename columns of interest in all waves
# Allows for cleaner merge with consistent names
for(i in 1:NUM_WAVES) {
  list[[i]] <- list[[i]] %>% rename(
    
    # Region of Britain
    region_raw = any_of(
      c("a_gor_dv.y", "b_gor_dv.y", "c_gor_dv.y", "d_gor_dv.y",
        "e_gor_dv.y", "f_gor_dv.y", "g_gor_dv.y", "h_gor_dv.y",
        "i_gor_dv.y", "j_gor_dv.y", "k_gor_dv.y", "l_gor_dv.y")),
    
    # Gender
    sex_raw = any_of(
      c("a_sex.y", "b_sex.y", "c_sex.y", "d_sex.y", "e_sex.y", "f_sex.y",
        "g_sex.y", "h_sex.y", "i_sex.y", "j_sex.y", "k_sex.y", "l_sex.y")),
    
    # Age
    age_raw = any_of(
      c("a_age_dv.y", "b_age_dv.y", "c_age_dv.y", "d_age_dv.y",
        "e_age_dv.y", "f_age_dv.y", "g_age_dv.y", "h_age_dv.y",
        "i_age_dv.y", "j_age_dv.y", "k_age_dv.y", "l_age_dv.y")),
    
    # Marital status
    marital_raw = any_of(
      c("a_marstat.y", "b_marstat.y", "c_marstat.y", "d_marstat.y",
        "e_marstat.y", "f_marstat.y", "g_marstat.y", "h_marstat.y",
        "i_marstat.y", "j_marstat.y", "k_marstat.y", "l_marstat.y")),
    
    # Household size
    hhsize_raw = any_of(
      c("a_hhsize.y", "b_hhsize.y", "c_hhsize.y", "d_hhsize.y",
        "e_hhsize.y", "f_hhsize.y", "g_hhsize.y", "h_hhsize.y",
        "i_hhsize.y", "j_hhsize.y", "k_hhsize.y", "l_hhsize.y")),
    
    # Number of children
    nkids_dv_raw = any_of(
      c("a_nkids_dv", "b_nkids_dv", "c_nkids_dv", "d_nkids_dv",
        "e_nkids_dv", "f_nkids_dv", "g_nkids_dv", "h_nkids_dv",
        "i_nkids_dv", "j_nkids_dv", "k_nkids_dv", "l_nkids_dv")),
    
    # Educational qualifications
    edu_raw = any_of(
      c("a_hiqual_dv", "b_hiqual_dv", "c_hiqual_dv", "d_hiqual_dv",
        "e_hiqual_dv", "f_hiqual_dv", "g_hiqual_dv", "h_hiqual_dv",
        "i_hiqual_dv", "j_hiqual_dv", "k_hiqual_dv", "l_hiqual_dv")),
    
    # Any paid employment
    employ_raw = any_of(
      c("a_employ.y", "b_employ.y", "c_employ.y", "d_employ.y",
        "e_employ.y", "f_employ.y", "g_employ.y", "h_employ.y",
        "i_employ.y", "j_employ.y", "k_employ.y", "l_employ.y")),
    
    # Occupation classification
    jbseg_dv_raw = any_of(
      c("a_jbseg_dv", "b_jbseg_dv", "c_jbseg_dv", "d_jbseg_dv",
        "e_jbseg_dv", "f_jbseg_dv", "g_jbseg_dv", "h_jbseg_dv",
        "i_jbseg_dv", "j_jbseg_dv", "k_jbseg_dv", "l_jbseg_dv")),
    
    # Employed vs. self-employed
    jbsemp_raw = any_of(
      c("a_jbsemp", "b_jbsemp", "c_jbsemp", "d_jbsemp",
        "e_jbsemp", "f_jbsemp", "g_jbsemp", "h_jbsemp",
        "i_jbsemp", "j_jbsemp", "k_jbsemp", "l_jbsemp")),
    
    # If self-employed, any employees
    jsboss_raw = any_of(
      c("a_jsboss", "b_jsboss", "c_jsboss", "d_jsboss",
        "e_jsboss", "f_jsboss", "g_jsboss", "h_jsboss",
        "i_jsboss", "j_jsboss", "k_jsboss", "l_jsboss")),
    
    # Full-time vs. part-time
    jbft_dv_raw = any_of(
      c("a_jbft_dv", "b_jbft_dv", "c_jbft_dv", "d_jbft_dv",
        "e_jbft_dv", "f_jbft_dv", "g_jbft_dv", "h_jbft_dv",
        "i_jbft_dv", "j_jbft_dv", "k_jbft_dv", "l_jbft_dv")),
    
    # Non-permanent work
    jbterm1_raw = any_of(
      c("a_jbterm1", "b_jbterm1", "c_jbterm1", "d_jbterm1",
        "e_jbterm1", "f_jbterm1", "g_jbterm1", "h_jbterm1",
        "i_jbterm1", "j_jbterm1", "k_jbterm1", "l_jbterm1")),
    
    # Gig economy job
    gig_1 = any_of(
      c("a_gelist1", "b_gelist1", "c_gelist1", "d_gelist1", "e_gelist1", "f_gelist1",
        "g_gelist1", "h_gelist1", "i_gelist1", "j_gelist1", "k_gelist1", "l_gelist1")),
    gig_2 = any_of(
      c("a_gelist2", "b_gelist2", "c_gelist2", "d_gelist2", "e_gelist2", "f_gelist2",
        "g_gelist2", "h_gelist2", "i_gelist2", "j_gelist2", "k_gelist2", "l_gelist2")),
    gig_3 = any_of(
      c("a_gelist3", "b_gelist3", "c_gelist3", "d_gelist3", "e_gelist3", "f_gelist3",
        "g_gelist3", "h_gelist3", "i_gelist3", "j_gelist3", "k_gelist3", "l_gelist3")),
    gig_4 = any_of(
      c("a_gelist4", "b_gelist4", "c_gelist4", "d_gelist4", "e_gelist4", "f_gelist4",
        "g_gelist4", "h_gelist4", "i_gelist4", "j_gelist4", "k_gelist4", "l_gelist4")),
    gig_5 = any_of(
      c("a_gelist5", "b_gelist5", "c_gelist5", "d_gelist5", "e_gelist5", "f_gelist5",
        "g_gelist5", "h_gelist5", "i_gelist5", "j_gelist5", "k_gelist5", "l_gelist5")),
    
    # Total household income
    fihhmnnet1_dv_raw = any_of(
      c("a_fihhmnnet1_dv", "b_fihhmnnet1_dv", "c_fihhmnnet1_dv", "d_fihhmnnet1_dv",
        "e_fihhmnnet1_dv", "f_fihhmnnet1_dv", "g_fihhmnnet1_dv", "h_fihhmnnet1_dv",
        "i_fihhmnnet1_dv", "j_fihhmnnet1_dv", "k_fihhmnnet1_dv", "l_fihhmnnet1_dv")),
    
    # Usual monthly wage
    wage_raw = any_of(
      c("a_paynu_dv", "b_paynu_dv", "c_paynu_dv", "d_paynu_dv",
        "e_paynu_dv", "f_paynu_dv", "g_paynu_dv", "h_paynu_dv",
        "i_paynu_dv", "j_paynu_dv", "k_paynu_dv", "l_paynu_dv")),
    
    # Perceived job security
    jbsec_raw = any_of(
      c("a_jbsec", "b_jbsec", "c_jbsec", "d_jbsec",
        "e_jbsec", "f_jbsec", "g_jbsec", "h_jbsec",
        "i_jbsec", "j_jbsec", "k_jbsec", "l_jbsec")),
    
    # General health
    sf1_raw = any_of(
      c("a_sf1", "b_sf1", "c_sf1", "d_sf1", "e_sf1", "f_sf1",
        "g_sf1", "h_sf1", "i_sf1", "j_sf1", "k_sf1", "l_sf1")),
    scsf1_raw = any_of(
      c("a_scsf1", "b_scsf1", "c_scsf1", "d_scsf1", "e_scsf1", "f_scsf1",
        "g_scsf1", "h_scsf1", "i_scsf1", "j_scsf1", "k_scsf1", "l_scsf1")),
    
    # Long-standing illness
    illness_raw = any_of(
      c("a_health", "b_health", "c_health", "d_health", "e_health", "f_health",
        "g_health", "h_health", "i_health", "j_health", "k_health", "l_health")),
    
    # Mental health score
    mcs_raw = any_of(
      c("a_sf12mcs_dv", "b_sf12mcs_dv", "c_sf12mcs_dv", "d_sf12mcs_dv",
        "e_sf12mcs_dv", "f_sf12mcs_dv", "g_sf12mcs_dv", "h_sf12mcs_dv",
        "i_sf12mcs_dv", "j_sf12mcs_dv", "k_sf12mcs_dv", "l_sf12mcs_dv")),
    
    # Physical health score
    pcs_raw = any_of(
      c("a_sf12pcs_dv", "b_sf12pcs_dv", "c_sf12pcs_dv", "d_sf12pcs_dv",
        "e_sf12pcs_dv", "f_sf12pcs_dv", "g_sf12pcs_dv", "h_sf12pcs_dv",
        "i_sf12pcs_dv", "j_sf12pcs_dv", "k_sf12pcs_dv", "l_sf12pcs_dv"))
    )
}

# Select columns of interest
# Note: Not all variables exist in all waves
for(i in 1:NUM_WAVES) {
  list[[i]] <- list[[i]] %>% select(any_of(c(
    # ID vars
    "pidp", "survey", "wave", "region_raw",
    
    # Demographics
    "sex_raw", "age_raw", "marital_raw", "hhsize_raw", "nkids_dv_raw",
    
    # Socioeconomics
    "edu_raw", "employ_raw", "jbseg_dv_raw", "fihhmnnet1_dv_raw", "wage_raw",
    
    # Employment
    "jbsemp_raw", "jsboss_raw", "jbft_dv_raw", "jbterm1_raw",
    "gig_1", "gig_2", "gig_3", "gig_4", "gig_5", "jbsec_raw",
    
    # Health
    "sf1_raw", "scsf1_raw", "illness_raw", "mcs_raw", "pcs_raw"
  )))
}

# Merge waves
us_all <- bind_rows(
  list[[1]],  list[[2]],  list[[3]],  list[[4]],  list[[5]],  list[[6]],
  list[[7]],  list[[8]],  list[[9]],  list[[10]], list[[11]], list[[12]])

##############################################################################
# Variable preparation
##############################################################################

# Subset to working age only
us_all <- subset(us_all, age_raw %in% c(22:65))

# Region of Britain
us_all <- us_all %>% mutate(
  region = case_when(
    region_raw == 1  ~ "North East",
    region_raw == 2  ~ "North West",
    region_raw == 3  ~ "Yorkshire and the Humber",
    region_raw == 4  ~ "East Midlands",
    region_raw == 5  ~ "West Midlands",
    region_raw == 6  ~ "East of England",
    region_raw == 7  ~ "London",
    region_raw == 8  ~ "South East",
    region_raw == 9  ~ "South West",
    region_raw == 10 ~ "Wales",
    region_raw == 11 ~ "Scotland",
    region_raw == 12 ~ "Northern Ireland"))

# Gender
us_all <- us_all %>% mutate(
  sex = case_when(
    sex_raw == 1 ~ "Male",
    sex_raw == 2 ~ "Female"))

# Age
us_all <- us_all %>% mutate(
  age = case_when(
    age_raw %in% c(0:115) ~ age_raw))

# Marital status
us_all <- us_all %>% mutate(
  marital = case_when(
    marital_raw %in% c(2:3) ~ "Married or civil partnership",
    marital_raw %in% c(4:9) ~ "Separated, divorced, or widowed",
    marital_raw %in% c(1)   ~ "Never married"))

# Educational qualification
us_all <- us_all %>% mutate(
  edu = case_when(
    edu_raw %in% c(1:2) ~ "Degree or other higher qual.",
    edu_raw %in% c(3)   ~ "A-level, etc.",
    edu_raw %in% c(4)   ~ "GCSE, etc.",
    edu_raw %in% c(5)   ~ "Other qualification",
    edu_raw %in% c(9)   ~ "No qualification"))

# Occupation classification
us_all <- us_all %>% mutate(
  job = case_when(
    jbseg_dv_raw %in% c(1:6)   ~ "Managerial and professional class",
    jbseg_dv_raw %in% c(7:10)  ~ "Routine non-manual workers",
    jbseg_dv_raw %in% c(11:13) ~ "Skilled workers",
    jbseg_dv_raw %in% c(14:19) ~ "Non-skilled workers",
    TRUE                  ~ "Not working or not indicated"))
us_all$job <- factor(us_all$job, levels = c("Managerial and professional class", "Routine non-manual workers", "Skilled workers", "Non-skilled workers", "Not working or not indicated"))

# Solo self-employed
# Worker is self-employed with no employees
us_all <- us_all %>% mutate(
  solo_self = case_when(
    jbsemp_raw == 2 & jsboss_raw == 2 ~ "Solo self-employed",
    employ_raw == 1                   ~ "Other paid employment",
    employ_raw == 2                   ~ "No paid employment"))
us_all$solo_self <- factor(us_all$solo_self, levels = c("Other paid employment", "Solo self-employed", "No paid employment"))

# Atypical employment
# Worker is part-time, non-permanent, OR solo self-employed
us_all <- us_all %>% mutate(
  atypical = case_when(
    jbft_dv_raw == 2                  ~ "Atypical employment", # Part-time
    jbterm1_raw == 2                  ~ "Atypical employment", # Non-permanent
    jbsemp_raw == 2 & jsboss_raw == 2 ~ "Atypical employment", # Solo self-employed
    employ_raw == 1                   ~ "Other paid employment",
    employ_raw == 2                   ~ "No paid employment"))
us_all$atypical <- factor(us_all$atypical, levels = c("Other paid employment", "Atypical employment", "No paid employment"))

# Gig workers
# Any work using website, platform, or app
# Note: Only in waves 11-12, so must restrict comparison groups
us_all <- us_all %>% mutate(
  gig_work = case_when(
    gig_1 == 1 ~ "Gig work", # Any gig work engagement
    gig_2 == 1 ~ "Gig work",
    gig_3 == 1 ~ "Gig work",
    gig_4 == 1 ~ "Gig work",
    gig_5 == 1 ~ "Gig work",
    employ_raw == 1 & wave %in% c(11:12) ~ "Other paid employment",
    employ_raw == 2 & wave %in% c(11:12) ~ "No paid employment"))
us_all$gig_work <- factor(us_all$gig_work, levels = c("Other paid employment", "Gig work", "No paid employment"))

# Number of adults in household
us_all$n_adults <- us_all$hhsize_raw - us_all$nkids_dv_raw

# In-work poverty
# Compute equivalence weighted household income
# Then dichotomize as less than 60% of median
us_all$equiv_hh_size <- 1 + (us_all$n_adults - 1)*0.5 + (us_all$nkids_dv_raw)*0.3
us_all$equiv_income  <- us_all$fihhmnnet1_dv_raw/us_all$equiv_hh_size
median <- median(us_all$equiv_income, na.rm=T)

us_all <- us_all %>% mutate(
  poverty = case_when(
    equiv_income <  0.6*median ~ 1,
    equiv_income >= 0.6*median ~ 0))

# Usual monthly wage
us_all <- us_all %>% mutate(
  wage = case_when(
    wage_raw >=  0 ~ wage_raw,
    wage_raw == -8 ~ 0)) # Inapplicable set to 0

# Perceived job security
# "Likely" or "very likely" to lose job = 1
us_all <- us_all %>% mutate(
  insecure = case_when(
    jbsec_raw %in% c(1:2) ~ 1,
    jbsec_raw %in% c(3:4) ~ 0))

# Precarious atypical employment
# Atypical employment AND either poverty or insecure
# Only possible in even waves (when insecure measured)
us_all <- us_all %>% mutate(
  precarious = case_when(
    wave %in% c(2,4,6,8,10,12) &
      atypical == "Atypical employment" &
      (insecure == 1 | poverty == 1) ~ "Precarious atypical emp.",
    wave %in% c(2,4,6,8,10,12) & employ_raw == 1 ~ "Other paid employment",
    wave %in% c(2,4,6,8,10,12) & employ_raw == 2 ~ "No paid employment"))
us_all$precarious <- factor(us_all$precarious, levels = c("Other paid employment", "Precarious atypical emp.", "No paid employment"))

# General health
us_all <- us_all %>% mutate(
  gen_hlth = case_when(
    # Give preference to self-completion
    scsf1_raw %in% c(1:5) ~ 6-scsf1_raw, # Poor (5) to excellent (1)
    
    # If self-completion unavailable, use proxy
    sf1_raw %in% c(1:5)   ~ 6-sf1_raw))

# Long-standing illness
us_all <- us_all %>% mutate(
  illness = case_when(
    illness_raw == 1 ~ 1,  # Yes
    illness_raw == 2 ~ 0)) # No

# MCS scale
us_all$mcs3 = ifelse(us_all$mcs_raw %in% c(-7,-8,-9), NA, us_all$mcs_raw)

# PCS scale
us_all$pcs3 = ifelse(us_all$pcs_raw %in% c(-7,-8,-9), NA, us_all$pcs_raw)

##############################################################################
# Demographics
##############################################################################

# Has at least one working defintion
# Complete cases for covariates
us_demo <- us_all %>%
  subset(!is.na(gig_work) | !is.na(solo_self) | !is.na(atypical) | !is.na(precarious)) %>%
  filter_at(vars(age, marital, edu), all_vars(!is.na(.)))

# Unique respondents
unique <- unique(us_demo$pidp)

# Wave 12 for comparisons
us_demo_12 <- subset(us_demo, wave == 12)

# Count of each definition
us_demo_12 <- us_demo_12 %>% mutate(
  gig_count = case_when(
    gig_work == "Gig work" ~ 1,
    TRUE ~ 0),
  sol_count = case_when(
    solo_self == "Solo self-employed" ~ 1,
    TRUE ~ 0),
  aty_count = case_when(
    atypical == "Atypical employment" ~ 1,
    TRUE ~ 0),
  pre_count = case_when(
    precarious == "Precarious atypical emp." ~ 1,
    TRUE ~ 0),
  )

# Sum of definitions
us_demo_12$nonst_count <- us_demo_12$gig_count + us_demo_12$sol_count +
  us_demo_12$aty_count + us_demo_12$pre_count
table(us_demo_12$nonst_count > 0); table(us_demo_12$nonst_count)

# Venn diagram
total <- sum(us_demo_12$nonst_count > 0)
us_demo_12 %>%
  subset(nonst_count > 0) %>%
  group_by(gig_count, sol_count, aty_count, pre_count) %>% 
  summarise(n = n()) %>%
  mutate(percentage = n/total*100)

# Demographics by definition
summary(tableby(~ sex + age + marital + edu + job + equiv_income,
                us_demo_12, digits.pct=0), text=T)
summary(tableby(gig_work   ~ sex + age + marital + edu + job + equiv_income,
                us_demo_12, digits.pct=0), text=T)
summary(tableby(solo_self  ~ sex + age + marital + edu + job + equiv_income,
                us_demo_12, digits.pct=0), text=T)
summary(tableby(atypical   ~ sex + age + marital + edu + job + equiv_income,
                us_demo_12, digits.pct=0), text=T)
summary(tableby(precarious ~ sex + age + marital + edu + job + equiv_income,
                us_demo_12, digits.pct=0), text=T)

##############################################################################
# Job transitions
##############################################################################

# Reorder dataset by respondent and wave
# Necessary for counting transitions
us_all <- us_all %>% arrange(as.numeric(pidp), as.numeric(wave))

# Count job transitions
# Gig work, waves 11-12
us_all %>%
  # Subset to waves 11-12
  subset(wave %in% c(11:12)) %>%
  
  # Subset to complete cases
  subset(!is.na(gig_work)) %>%
  
  # Count numbers of transitions
  group_by(pidp) %>% 
  mutate(change = ifelse(gig_work != lag(gig_work), 1, 0)) %>%
  summarise(total = sum(change, na.rm=T)) %>%
  
  # Bucket 3+ transitions
  mutate(bin = ifelse(total >= 3, "3+", total)) %>%
  
  # Present distribution
  group_by(bin) %>% 
  summarise(n = n()) %>% 
  mutate(proportion = n / sum(n))

# Count job transitions
# Solo self-employment, waves 11-12
us_all %>%
  # Subset to waves 11-12
  subset(wave %in% c(11:12)) %>%
  
  # Subset to complete cases
  subset(!is.na(solo_self)) %>%
  
  # Count numbers of transitions
  group_by(pidp) %>% 
  mutate(change = ifelse(solo_self != lag(solo_self), 1, 0)) %>%
  summarise(total = sum(change, na.rm=T)) %>%
  
  # Bucket 3+ transitions
  mutate(bin = ifelse(total >= 3, "3+", total)) %>%
  
  # Present distribution
  group_by(bin) %>% 
  summarise(n = n()) %>% 
  mutate(proportion = n / sum(n))

# Count job transitions
# Atypical employment, waves 11-12
us_all %>%
  # Subset to waves 11-12
  subset(wave %in% c(11:12)) %>%
  
  # Subset to complete cases
  subset(!is.na(atypical)) %>%
  
  # Count numbers of transitions
  group_by(pidp) %>% 
  mutate(change = ifelse(atypical != lag(atypical), 1, 0)) %>%
  summarise(total = sum(change, na.rm=T)) %>%
  
  # Bucket 3+ transitions
  mutate(bin = ifelse(total >= 3, "3+", total)) %>%
  
  # Present distribution
  group_by(bin) %>% 
  summarise(n = n()) %>% 
  mutate(proportion = n / sum(n))

# Count job transitions
# Solo self-employment, all waves
us_all %>%
  # Subset to complete cases
  subset(!is.na(solo_self)) %>%
  
  # Count numbers of transitions
  group_by(pidp) %>% 
  mutate(change = ifelse(solo_self != lag(solo_self), 1, 0)) %>%
  summarise(total = sum(change, na.rm=T)) %>%
  
  # Bucket 3+ transitions
  mutate(bin = ifelse(total >= 3, "3+", total)) %>%
  
  # Present distribution
  group_by(bin) %>% 
  summarise(n = n()) %>% 
  mutate(proportion = n / sum(n))

# Count job transitions
# Atypical employment, all waves
us_all %>%
  # Subset to complete cases
  subset(!is.na(atypical)) %>%
  
  # Count numbers of transitions
  group_by(pidp) %>% 
  mutate(change = ifelse(atypical != lag(atypical), 1, 0)) %>%
  summarise(total = sum(change, na.rm=T)) %>%
  
  # Bucket 3+ transitions
  mutate(bin = ifelse(total >= 3, "3+", total)) %>%
  
  # Present distribution
  group_by(bin) %>% 
  summarise(n = n()) %>% 
  mutate(proportion = n / sum(n))

# Count job transitions
# Precarious atypical emp., all waves
us_all %>%
  # Subset to complete cases
  subset(!is.na(precarious)) %>%
  
  # Count numbers of transitions
  group_by(pidp) %>% 
  mutate(change = ifelse(precarious != lag(precarious), 1, 0)) %>%
  summarise(total = sum(change, na.rm=T)) %>%
  
  # Bucket 3+ transitions
  mutate(bin = ifelse(total >= 3, "3+", total)) %>%
  
  # Present distribution
  group_by(bin) %>% 
  summarise(n = n()) %>% 
  mutate(proportion = n / sum(n))

##############################################################################
# Cross-sectional health by work arrangement
##############################################################################

# Subset to waves 11-12
us_demo_11_12 <- subset(us_demo, wave %in% c(11:12))

# Self-reported health, waves 11-12
describeBy(us_demo_11_12$gen_hlth, us_demo_11_12$gig_work)
describeBy(us_demo_11_12$gen_hlth, us_demo_11_12$solo_self)
describeBy(us_demo_11_12$gen_hlth, us_demo_11_12$atypical)

# Self-reported health, all waves
describeBy(us_demo$gen_hlth, us_demo$solo_self)
describeBy(us_demo$gen_hlth, us_demo$atypical)
describeBy(us_demo$gen_hlth, us_demo$precarious)

# Long-standing illness, waves 11-12
describeBy(us_demo_11_12$illness, us_demo_11_12$gig_work)
describeBy(us_demo_11_12$illness, us_demo_11_12$solo_self)
describeBy(us_demo_11_12$illness, us_demo_11_12$atypical)

# Long-standing illness, all waves
describeBy(us_demo$illness, us_demo$solo_self)
describeBy(us_demo$illness, us_demo$atypical)
describeBy(us_demo$illness, us_demo$precarious)

# SF-12 Physical Component, waves 11-12
describeBy(us_demo_11_12$pcs3, us_demo_11_12$gig_work)
describeBy(us_demo_11_12$pcs3, us_demo_11_12$solo_self)
describeBy(us_demo_11_12$pcs3, us_demo_11_12$atypical)

# SF-12 Physical Component, all waves
describeBy(us_demo$pcs3, us_demo$solo_self)
describeBy(us_demo$pcs3, us_demo$atypical)
describeBy(us_demo$pcs3, us_demo$precarious)

# SF-12 Mental Component, waves 11-12
describeBy(us_demo_11_12$mcs3, us_demo_11_12$gig_work)
describeBy(us_demo_11_12$mcs3, us_demo_11_12$solo_self)
describeBy(us_demo_11_12$mcs3, us_demo_11_12$atypical)

# SF-12 Mental Component, all waves
describeBy(us_demo$mcs3, us_demo$solo_self)
describeBy(us_demo$mcs3, us_demo$atypical)
describeBy(us_demo$mcs3, us_demo$precarious)

##############################################################################
# Fixed effects models
##############################################################################

# Models for general health
gen_gig_sub <- felm(gen_hlth ~ gig_work +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))
gen_aty_sub <- felm(gen_hlth ~ atypical +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))
gen_sol_sub <- felm(gen_hlth ~ solo_self +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))

gen_aty_all <- felm(gen_hlth ~ atypical +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)
gen_sol_all <- felm(gen_hlth ~ solo_self +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)
gen_pre_all <- felm(gen_hlth ~ precarious +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)

# Model for long-standing illness
ill_gig_sub <- felm(illness ~ gig_work +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))
ill_aty_sub <- felm(illness ~ atypical +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))
ill_sol_sub <- felm(illness ~ solo_self +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))

ill_aty_all <- felm(illness ~ atypical +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)
ill_sol_all <- felm(illness ~ solo_self +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)
ill_pre_all <- felm(illness ~ precarious +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)

# Models for SF-12 Physical Component
pcs_gig_sub <- felm(pcs3 ~ gig_work +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))
pcs_aty_sub <- felm(pcs3 ~ atypical +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))
pcs_sol_sub <- felm(pcs3 ~ solo_self +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))

pcs_aty_all <- felm(pcs3 ~ atypical +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)
pcs_sol_all <- felm(pcs3 ~ solo_self +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)
pcs_pre_all <- felm(pcs3 ~ precarious +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)

# Models for SF-12 Mental Component
mcs_gig_sub <- felm(mcs3 ~ gig_work +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))
mcs_aty_sub <- felm(mcs3 ~ atypical +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))
mcs_sol_sub <- felm(mcs3 ~ solo_self +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=subset(us_all, wave %in% c(11:12)))

mcs_aty_all <- felm(mcs3 ~ atypical +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)
mcs_sol_all <- felm(mcs3 ~ solo_self +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)
mcs_pre_all <- felm(mcs3 ~ precarious +
                      age + marital + edu |
                      pidp + wave + region | 0 | pidp,
                    data=us_all)

# List of variable names
var_names <- c("gig_workGig work"                   = "Gig work",
               "solo_selfSolo self-employed"        = "Solo self-employment",
               "atypicalAtypical employment"        = "Atypical employment",
               "precariousPrecarious atypical emp." = "Precarious atypical emp.",
               "gig_workNo paid employment"         = "No paid employment",
               "atypicalNo paid employment"         = "No paid employment",
               "solo_selfNo paid employment"        = "No paid employment",
               "precariousNo paid employment"       = "No paid employment")

# Additional row for covariates
cov_row <- as.data.frame(
  rbind(cbind("Survey waves", "11-12", "11-12", "11-12", "All", "All", "Even"),
        cbind("Demographic characteristics",   t(rep("Yes", 6))),
        cbind("Individual fixed effects",      t(rep("Yes", 6))),
        cbind("Region and year fixed effects", t(rep("Yes", 6))),
        cbind("Clustered standard errors",     t(rep("Individual", 6)))))

# Compile results into table
modelsummary(list("(1)" = gen_gig_sub,
                  "(2)" = gen_sol_sub,
                  "(3)" = gen_aty_sub,
                  "(4)" = gen_sol_all,
                  "(5)" = gen_aty_all,
                  "(6)" = gen_pre_all),
             coef_omit   = "age|marital|edu",
             fmt         = "%.3f",
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std|Adj",
             coef_map    = var_names,
             add_rows    = cov_row,
             title       = "Impact of transitioning to several non-standard employment arrangements from standard employment on self-reported general health (1 = poor and 5 = excellent).",
             stars       = c("*"=0.05, "**"=0.01, "***"=0.001),
             output      = "Self-reported health.tex") %>%
  add_header_above(c(" " = 1, "2019-2022" = 3, "2009-2022" = 3)) %>%
  add_header_above(c(" " = 1, "Self-reported general health" = 6)) %>%
  row_spec(row=c(12), hline_after=TRUE)

# Compile results into table
modelsummary(list("(1)" = ill_gig_sub,
                  "(2)" = ill_sol_sub,
                  "(3)" = ill_aty_sub,
                  "(4)" = ill_sol_all,
                  "(5)" = ill_aty_all,
                  "(6)" = ill_pre_all),
             coef_omit   = "age|marital|edu",
             fmt         = "%.3f",
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std|Adj",
             coef_map    = var_names,
             add_rows    = cov_row,
             title       = "Impact of transitioning to non-standard employment from standard employment on having long-standing illness or impairment (0 = no and 1 = yes).",
             stars       = c("*"=0.05, "**"=0.01, "***"=0.001),
             output      = "Long-standing illness.tex") %>%
  add_header_above(c(" " = 1, "2019-2022" = 3, "2009-2022" = 3)) %>%
  add_header_above(c(" " = 1, "Long-standing illness or impairment" = 6)) %>%
  row_spec(row=c(12), hline_after=TRUE)

# Compile results into table
modelsummary(list("(1)" = pcs_gig_sub,
                  "(2)" = pcs_sol_sub,
                  "(3)" = pcs_aty_sub,
                  "(4)" = pcs_sol_all,
                  "(5)" = pcs_aty_all,
                  "(6)" = pcs_pre_all),
             coef_omit   = "age|marital|edu",
             fmt         = "%.3f",
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std|Adj",
             coef_map    = var_names,
             add_rows    = cov_row,
             title       = "Impact of transitioning to non-standard employment from standard employment on SF-12 Physical Component Summary (0 = low function and 100 = high).",
             stars       = c("*"=0.05, "**"=0.01, "***"=0.001),
             output      = "SF-12 physical.tex") %>%
  add_header_above(c(" " = 1, "2019-2022" = 3, "2009-2022" = 3)) %>%
  add_header_above(c(" " = 1, "SF-12 Physical Component Summary" = 6)) %>%
  row_spec(row=c(12), hline_after=TRUE)

# Compile results into table
modelsummary(list("(1)" = mcs_gig_sub,
                  "(2)" = mcs_sol_sub,
                  "(3)" = mcs_aty_sub,
                  "(4)" = mcs_sol_all,
                  "(5)" = mcs_aty_all,
                  "(6)" = mcs_pre_all),
             coef_omit   = "age|marital|edu",
             fmt         = "%.3f",
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std|Adj",
             coef_map    = var_names,
             add_rows    = cov_row,
             title       = "Impact of transitioning to non-standard employment from standard employment on SF-12 Mental Component Summary (0 = low function and 100 = high).",
             stars       = c("*"=0.05, "**"=0.01, "***"=0.001),
             output      = "SF-12 mental.tex") %>%
  add_header_above(c(" " = 1, "2019-2022" = 3, "2009-2022" = 3)) %>%
  add_header_above(c(" " = 1, "SF-12 Mental Component Summary" = 6)) %>%
  row_spec(row=c(12), hline_after=TRUE)
