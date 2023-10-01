# Preliminaries ####

# Load packages
library(here)         # Working directory
library(psych)        # Analysis tools
library(car)          # Analysis tools
library(dplyr)        # Analysis tools
library(arsenal)      # Analysis tools
library(lfe)          # Modeling tools
library(ggplot2)      # Graphing tools
library(cowplot)      # Graphing tools
library(arsenal)      # Table tools
library(modelsummary) # Table tools
library(kableExtra)   # Table tools
library(DescTools)    # Table tools

nl_data = read.csv("nl_data.csv")

# Set factors
nl_data[nl_data == "No paid employment"] = "No paid \nemployment"
nl_data[nl_data == "Other paid employment"] = "Other paid \nemployment"

nl_data$temp_work <- factor(nl_data$temp_work, levels = c("Other paid \nemployment",
                                                          "Temp/on call",
                                                          "No paid \nemployment"))
nl_data$solo_self <- factor(nl_data$solo_self, levels = c("Other paid \nemployment",
                                                          "Solo self-employed",
                                                          "No paid \nemployment"))
nl_data$atypical   <- factor(nl_data$atypical, levels = c("Other paid \nemployment",
                                                          "Atypical employment",
                                                          "No paid \nemployment"))
nl_data$precarious <- factor(nl_data$precarious, levels = c("Other paid \nemployment",
                                                          "Precarious",
                                                          "No paid \nemployment"))

# Panel descriptives ####
min(nl_data$StartDate)
max(nl_data$StartDate)

n_perwave = nl_data %>%
  group_by(wave) %>%
  summarise(n = n())
n_perwave

length(unique(nl_data$RID))
nrow(nl_data)

summary(tableby(~ sex_rec + age_cont + edu_3_rec + as.factor(etn_3_rec) + inc_equivalent,
                nl_data, digits.pct=0), text=T)

# Unweighted analyses ####

## 1. Demographics ####

### 1.0 Definitions ####

PercTable(table(nl_data$temp_work))
PercTable(table(nl_data$solo_self))
PercTable(table(nl_data$atypical))
PercTable(table(nl_data$precarious))

# Per wave
table(nl_data$temp_work,      nl_data$wave, useNA = "always")
table(nl_data$solo_self,      nl_data$wave, useNA = "always")
table(nl_data$atypical,       nl_data$wave, useNA = "always")
table(nl_data$precarious,     nl_data$wave, useNA = "always")


### 1.1 Demographics by definition ####

tab_temp = as.data.frame(summary(tableby(temp_work  ~ sex_rec + age_cont + as.factor(edu_3_rec) + as.factor(etn_3_rec) + inc_equivalent
                , nl_data
                , digits.pct=0), text=T))

tab_solo = as.data.frame(summary(tableby(solo_self  ~ sex_rec + age_cont + as.factor(edu_3_rec) + as.factor(etn_3_rec) + inc_equivalent
                , nl_data
                , digits.pct=0), text=T))

tab_atyp = as.data.frame(summary(tableby(atypical   ~ sex_rec + age_cont + as.factor(edu_3_rec) + as.factor(etn_3_rec) + inc_equivalent
                , nl_data
                , digits.pct=0), text=T))

tab_prec = as.data.frame(summary(tableby(precarious ~ sex_rec + age_cont + as.factor(edu_3_rec) + as.factor(etn_3_rec) + inc_equivalent
                , nl_data
                , digits.pct=0), text=T))

vars = c(colnames(tab_temp)[1], tab_temp[,1])
temp = c(colnames(tab_temp)[3], tab_temp[,3])
solo = c(colnames(tab_solo)[3], tab_solo[,3])
atyp = c(colnames(tab_atyp)[3], tab_atyp[,3])
prec = c(colnames(tab_prec)[3], tab_prec[,3])
paid = c(colnames(tab_temp)[2], tab_temp[,2])
unpd = c(colnames(tab_temp)[4], tab_temp[,4])
totl = c(colnames(tab_temp)[5], tab_temp[,5])

tab_demg = data.frame(vars = vars,
                      temp = temp,
                      solo = solo,
                      atyp = atyp,
                      prec = prec,
                      paid = paid,
                      unpd = unpd,
                      totl = totl)

tab_demg = tab_demg[-c(7,17,19),]

tab_demg$vars = c("",
                  "Gender","Female","Male",
                  "Age (22-67)","Mean (SD)",
                  "Education","Low education", "Middle education", "High education",
                  "Migration background","None", "Western", "Non-Western",
                  "Income","Mean (SD)")


write.csv(tab_demg, "./Tables/Demographics.csv", row.names = F)

### 1.2 Venn diagram ####

n_solo <- sum(nl_data$solo_self   == "Solo self-employed",  na.rm=T)
n_atyp <- sum(nl_data$atypical    == "Atypical employment", na.rm=T)
n_temp <- sum(nl_data$temp_work   == "Temp/on call",        na.rm=T)
n_prec <- sum(nl_data$precarious  == "Precarious",          na.rm=T)

tot = n_atyp

p_solo <- n_solo/tot
p_atyp <- n_atyp/tot
p_temp <- n_temp/tot
p_prec <- n_prec/tot

# overlap with atyp should equal entire definitions
n_atyp_solo <- sum(nl_data$atypical   == "Atypical employment" &
                   nl_data$solo_self  == "Solo self-employed", na.rm=T)
n_atyp_prec <- sum(nl_data$atypical   == "Atypical employment" &
                   nl_data$precarious == "Precarious", na.rm=T)
n_atyp_temp <- sum(nl_data$atypical   == "Atypical employment" &
                   nl_data$temp_work  == "Temp/on call", na.rm=T)

# overlap between small definitions
n_solo_temp <- sum(nl_data$solo_self  == "Solo self-employed" &
                   nl_data$temp_work  == "Temp/on call", na.rm=T)
n_solo_prec <- sum(nl_data$solo_self  == "Solo self-employed" &
                   nl_data$precarious == "Precarious", na.rm=T)
n_temp_prec <- sum(nl_data$temp_work  == "Temp/on call" &
                   nl_data$precarious == "Precarious", na.rm=T)

p_atyp_solo = n_atyp_solo/tot
p_atyp_prec = n_atyp_prec/tot
p_atyp_temp = n_atyp_temp/tot
p_solo_temp = n_solo_temp/tot
p_solo_prec = n_solo_prec/tot
p_temp_prec = n_temp_prec/tot

# overlap between all 4 defs
n3 <- sum(nl_data$solo_self  == "Solo self-employed" &
          nl_data$precarious == "Precarious" &
          nl_data$temp_work  == "Temp/on call", na.rm=T)

p3 = n3/tot

# fill in cells
n_solo_temp-n3
p_solo_temp-p3

n_solo_prec-n3
p_solo_prec-p3

n_temp_prec-n3
p_temp_prec-p3

n_temp-n_solo_temp-n_temp_prec-n3
p_temp-p_solo_temp-p_temp_prec-p3

n_solo-n_solo_temp-n_solo_prec-n3
p_solo-p_solo_temp-p_solo_prec-p3

n_prec-n_solo_prec-n_temp_prec-n3
p_prec-p_solo_prec-p_temp_prec-p3

# OVERLAP
# atyp exclusively
n_atyp-n_solo-n_temp-n_prec+n_solo_prec+n_temp_prec+n_solo_temp+n3
p_atyp-p_solo-p_temp-p_prec+p_solo_prec+p_temp_prec+p_solo_temp+p3

# 2 or more defs
sum(nl_data$solo_self == "Solo self-employed" |
    nl_data$precarious == "Precarious" |
    nl_data$temp_work == "Temp/on call", na.rm=T)
sum(nl_data$solo_self == "Solo self-employed" |
    nl_data$precarious == "Precarious" |
    nl_data$temp_work == "Temp/on call", na.rm=T)/n_atyp

# 3 or more defs
n_solo_prec + n_solo_temp + n_temp_prec - n3 - n3
(n_solo_prec + n_solo_temp + n_temp_prec - n3 - n3)/tot
# 4 defs
n3

### 1.3 Transitions ####

# Reorder dataset
nl_data <- nl_data %>% arrange(as.numeric(RID), as.numeric(wave))
# Count job transitions
us_transitions <- nl_data %>%
  # subset(wave %in% c(11:12)) %>%
  group_by(RID) %>% 
  mutate(change_aty = ifelse(atypical   != lag(atypical),   1, 0)) %>% 
  mutate(change_sol = ifelse(solo_self  != lag(solo_self),  1, 0)) %>%
  mutate(change_gig = ifelse(temp_work  != lag(temp_work),  1, 0)) %>%
  mutate(change_pre = ifelse(precarious != lag(precarious), 1, 0)) %>%
  summarise(total_aty = sum(change_aty, na.rm=T),
            total_sol = sum(change_sol, na.rm=T),
            total_gig = sum(change_gig, na.rm=T),
            total_pre = sum(change_pre, na.rm=T))

table_sol <- as.data.frame(prop.table(table(us_transitions$total_sol)))
table_aty <- as.data.frame(prop.table(table(us_transitions$total_aty)))
table_gig <- as.data.frame(prop.table(table(us_transitions$total_gig)))
table_pre <- as.data.frame(prop.table(table(us_transitions$total_pre)))
table_sol$Freq = round(table_sol$Freq*100, 1)
table_aty$Freq = round(table_aty$Freq*100, 1)
table_gig$Freq = round(table_gig$Freq*100, 1)
table_pre$Freq = round(table_pre$Freq*100, 1)

## 2. Unadjusted, pooled figures ####

# Load package
library(survey)

# Survey design
design <- svydesign(id=~1, data=nl_data)

# Table of populism by health
table_health_temp <- svyby(~health_5, ~temp_work,  design, svymean, na.rm=T)
table_mental_temp <- svyby(~mental,   ~temp_work,  design, svymean, na.rm=T)
table_health_solo <- svyby(~health_5, ~solo_self,  design, svymean, na.rm=T)
table_mental_solo <- svyby(~mental,   ~solo_self,  design, svymean, na.rm=T)
table_health_atyp <- svyby(~health_5, ~atypical,   design, svymean, na.rm=T)
table_mental_atyp <- svyby(~mental,   ~atypical,   design, svymean, na.rm=T)
table_health_prec <- svyby(~health_5, ~precarious, design, svymean, na.rm=T)
table_mental_prec <- svyby(~mental,   ~precarious, design, svymean, na.rm=T)

# t.tests
t.test(nl_data$health_5[nl_data$temp_work=="Temp/on call" | nl_data$temp_work=="Other paid \nemployment"] ~ nl_data$temp_work[nl_data$temp_work=="Temp/on call" | nl_data$temp_work=="Other paid \nemployment"])

t.test(nl_data$health_5[nl_data$solo_self=="Solo self-employed" | nl_data$solo_self=="Other paid \nemployment"] ~ nl_data$solo_self[nl_data$solo_self=="Solo self-employed" | nl_data$solo_self=="Other paid \nemployment"])

t.test(nl_data$health_5[nl_data$atypical=="Atypical employment" | nl_data$atypical=="Other paid \nemployment"] ~ nl_data$atypical[nl_data$atypical=="Atypical employment" | nl_data$atypical=="Other paid \nemployment"])

t.test(nl_data$health_5[nl_data$precarious=="Precarious" | nl_data$precarious=="Other paid \nemployment"] ~ nl_data$precarious[nl_data$precarious=="Precarious" | nl_data$precarious=="Other paid \nemployment"])

t.test(nl_data$mental[nl_data$temp_work=="Temp/on call" | nl_data$temp_work=="Other paid \nemployment"] ~ nl_data$temp_work[nl_data$temp_work=="Temp/on call" | nl_data$temp_work=="Other paid \nemployment"])

t.test(nl_data$mental[nl_data$solo_self=="Solo self-employed" | nl_data$solo_self=="Other paid \nemployment"] ~ nl_data$solo_self[nl_data$solo_self=="Solo self-employed" | nl_data$solo_self=="Other paid \nemployment"])

t.test(nl_data$mental[nl_data$atypical=="Atypical employment" | nl_data$atypical=="Other paid \nemployment"] ~ nl_data$atypical[nl_data$atypical=="Atypical employment" | nl_data$atypical=="Other paid \nemployment"])

t.test(nl_data$mental[nl_data$precarious=="Precarious" | nl_data$precarious=="Other paid \nemployment"] ~ nl_data$precarious[nl_data$precarious=="Precarious" | nl_data$precarious=="Other paid \nemployment"])


# Graph general health by temp work
fig_health_temp <- 
  ggplot(data = table_health_temp, 
         aes(x = factor(temp_work), 
             y = health_5,
             ymin = health_5 - 1.96*se,
             ymax = health_5 + 1.96*se)) +
  geom_linerange() +
  geom_point(stat = "identity") +
  xlab("Temp work") +
  ylab("Self-rated health (1-5)") +
  theme_test() +
  theme(legend.position = "none",
        #axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  scale_y_continuous(breaks = seq(1,5,1), limits = c(1,5))

# Graph mental well-being by temp work
fig_mental_temp <- 
  ggplot(data = table_mental_temp, 
         aes(x = factor(temp_work), 
             y = mental,
             ymin = mental - 1.96*se,
             ymax = mental + 1.96*se)) +
  geom_linerange() +
  geom_point(stat = "identity") +
  xlab("Temp work") +
  ylab("Mental well-being (1-5)") +
  theme_test() +
  theme(legend.position = "none",
        #axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  scale_y_continuous(breaks = seq(1,6,1), limits = c(1,6))

# Graph general health by solo_self work
fig_health_solo <- 
  ggplot(data = table_health_solo, 
         aes(x = factor(solo_self), 
             y = health_5,
             ymin = health_5 - 1.96*se,
             ymax = health_5 + 1.96*se)) +
  geom_linerange() +
  geom_point(stat = "identity") +
  xlab("Solo-self employed work") +
  ylab("Self-rated health (1-5)") +
  theme_test() +
  theme(legend.position = "none",
        #axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  scale_y_continuous(breaks = seq(1,5,1), limits = c(1,5))

# Graph mental well-being by solo_self work
fig_mental_solo <- 
  ggplot(data = table_mental_solo, 
         aes(x = factor(solo_self), 
             y = mental,
             ymin = mental - 1.96*se,
             ymax = mental + 1.96*se)) +
  geom_linerange() +
  geom_point(stat = "identity") +
  xlab("Solo-self employed work") +
  ylab("Mental well-being (1-5)") +
  theme_test() +
  theme(legend.position = "none",
        #axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  scale_y_continuous(breaks = seq(1,6,1), limits = c(1,6))

# Graph general health by atypical work
fig_health_atyp <- 
  ggplot(data = table_health_atyp, 
         aes(x = factor(atypical), 
             y = health_5,
             ymin = health_5 - 1.96*se,
             ymax = health_5 + 1.96*se)) +
  geom_linerange() +
  geom_point(stat = "identity") +
  xlab("Atypical work") +
  ylab("Self-rated health (1-5)") +
  theme_test() +
  theme(legend.position = "none",
        #axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  scale_y_continuous(breaks = seq(1,5,1), limits = c(1,5))

# Graph mental well-being by atypical work
fig_mental_atyp <- 
  ggplot(data = table_mental_atyp, 
         aes(x = factor(atypical), 
             y = mental,
             ymin = mental - 1.96*se,
             ymax = mental + 1.96*se)) +
  geom_linerange() +
  geom_point(stat = "identity") +
  xlab("Atypical work") +
  ylab("Mental well-being (1-5)") +
  theme_test() +
  theme(legend.position = "none",
        #axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  scale_y_continuous(breaks = seq(1,6,1), limits = c(1,6))

# Graph general health by precarious work
fig_health_prec <- 
  ggplot(data = table_health_prec, 
         aes(x = factor(precarious), 
             y = health_5,
             ymin = health_5 - 1.96*se,
             ymax = health_5 + 1.96*se)) +
  geom_linerange() +
  geom_point(stat = "identity") +
  xlab("Precarious work") +
  ylab("Self-rated health (1-5)") +
  theme_test() +
  theme(legend.position = "none",
        #axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  scale_y_continuous(breaks = seq(1,5,1), limits = c(1,5))

# Graph mental well-being by precarious work
fig_mental_prec <- 
  ggplot(data = table_mental_prec, 
         aes(x = factor(precarious), 
             y = mental,
             ymin = mental - 1.96*se,
             ymax = mental + 1.96*se)) +
  geom_linerange() +
  geom_point(stat = "identity") +
  xlab("Precarious work") +
  ylab("Mental well-being (1-5)") +
  theme_test() +
  theme(legend.position = "none",
        #axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  scale_y_continuous(breaks = seq(1,6,1), limits = c(1,6))



# Compile figures into object
figure <- cowplot::plot_grid(fig_health_temp, fig_mental_temp, 
                             fig_health_solo, fig_mental_solo,
                             fig_health_atyp, fig_mental_atyp, 
                             fig_health_prec, fig_mental_prec,
                             nrow=5, rel_widths=c(1,1))

# Print figure
ggsave(plot=figure, file="./Plots/Unadjusted figure (uw).png", width=10, height=14, units='in', dpi=600)


## 3. Fixed effects models ####

table(nl_data$wave)

mental_temp_all <- felm(mental ~ temp_work +
                         age_cont + as.factor(edu_3_rec) |
                         RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                       data=nl_data)

health3_temp_all <- felm(health_3 ~ temp_work +
                          age_cont + as.factor(edu_3_rec) |
                          RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                        data=nl_data)

health5_temp_all <- felm(health_5 ~ temp_work +
                          age_cont + as.factor(edu_3_rec) |
                          RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                        data=nl_data)

mental_solo_all <- felm(mental ~ solo_self +
                         age_cont + as.factor(edu_3_rec) |
                         RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                       data=nl_data)

health3_solo_all <- felm(health_3 ~ solo_self +
                          age_cont + as.factor(edu_3_rec) |
                          RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                        data=nl_data)

health5_solo_all <- felm(health_5 ~ solo_self +
                          age_cont + as.factor(edu_3_rec) |
                          RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                        data=nl_data)

mental_atyp_all <- felm(mental ~ atypical +
                         age_cont + as.factor(edu_3_rec) |
                         RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                       data=nl_data)

health3_atyp_all <- felm(health_3 ~ atypical +
                          age_cont + as.factor(edu_3_rec) |
                          RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                        data=nl_data)

health5_atyp_all <- felm(health_5 ~ atypical +
                          age_cont + as.factor(edu_3_rec) |
                          RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                        data=nl_data)

mental_prec_all <- felm(mental ~ precarious +
                         age_cont + as.factor(edu_3_rec) |
                         RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                       data=nl_data)

health3_prec_all <- felm(health_3 ~ precarious +
                          age_cont + as.factor(edu_3_rec) |
                          RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                        data=nl_data)

health5_prec_all <- felm(health_5 ~ precarious +
                          age_cont + as.factor(edu_3_rec) |
                          RID + wave + as.factor(geo_prov_rec) | 0 | RID,
                        data=nl_data)

library(modelsummary)

# List of variable names
var_names <- c(#"gig_workGig work"            = "Gig work",
  "temp_workTemp/on call"         = "Temp/on call",
  "solo_selfSolo self-employed"   = "Solo self-employed",
  "atypicalAtypical employment"   = "Atypical employment",
  "precariousPrecarious"          = "Precarious employment",
  "temp_workNo paid \nemployment" = "No paid employment",
  "solo_selfNo paid \nemployment" = "No paid employment",
  "atypicalNo paid \nemployment"  = "No paid employment",
  "precariousNo paid \nemployment"= "No paid employment")

# Additional row for covariates
cov_row <- as.data.frame(
  rbind(cbind("Demographic characteristics",   t(rep("Yes", 8))),
        cbind("Individual fixed effects",      t(rep("Yes", 8))),
        cbind("Number of waves",               t(c("2-6","2-6","2-6","2-6","1-3, 5-6","1-3, 5-6","1-3, 5-6","1-3, 5-6"))),
        cbind("Wave fixed effects",            t(rep("Yes", 8))),
        cbind("Clustered standard errors",     t(rep("Individual", 8)))))

# Compile results into table
modelsummary(list(#"(1: 1-3)" = health3_sol_all,
  "(1)" = health5_temp_all,
  #"(1: 1-3)" = health3_sol_all,
  "(2)" = health5_solo_all,
  #"(3: 1-3)" = health3_aty_all,
  "(3)" = health5_atyp_all,
  #"(5: 1-3)" = health3_pre_all,
  "(4)" = health5_prec_all,
  "(5)" = mental_temp_all,
  "(6)" = mental_solo_all,
  "(7)" = mental_atyp_all,
  "(8)" = mental_prec_all),
  coef_omit  = "age_cont|edu_3_rec",
  fmt        = "%.3f",
  gof_omit   = "Log*|AIC|BIC|F|RMSE|Std|Adj",
  coef_map   = var_names,
  add_rows   = cov_row,
  title      = "Impact of transitioning to non-standard employment from standard employment on indicated health outcome.",
  stars      = c("*"=0.05, "**"=0.01, "***"=0.001),
  output     = "Dutch models.tex") %>%
  add_header_above(c(" " = 1, 
                     "Self-reported general health (1-5)" = 4,
                     "Mental well-being index (1-5)" = 4))
