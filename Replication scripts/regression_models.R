#######################
## REGRESSION MODELS ##
#######################

# Load packages
library(cshapes)
library(separationplot)
library(sf)
library(RColorBrewer)
library(tmap)
library(lubridate)
library(zoo)
library(spacetime)
library(MASS)
library(plm)
library(tidyverse)
library(lmtest)
library(multiwayvcov)
library(stargazer)
library(car)
library(corrplot)
library(gvlma)
library(nnet)
library(locfit)
library(pROC)
library(fishmethods)
library(e1071)
library(LogisticDx)
library(interactions)

# Load dataset
# load("C:/Users/villar/Dropbox/MA_Vilde/R/acled_210420.RData")

############################
# Baseline models, table 3 #
############################

# Model 1: Only past level of violence
empty_mod <- lm(chg_viol ~ prop_viol_lag, data = acled_cleaned)
cluster_gid0 <- cluster.vcov(empty_mod, acled_cleaned$country)
ols0_gid <- sqrt(diag(cluster_gid0))
ols0_se <- coeftest(empty_mod, cluster_gid0)

cis_0 <- confint(ols0_se)
empty_df <- broom::tidy(ols0_se) %>%
  mutate(ci.lwr = cis_0[1:2, 1],
         ci.upr = cis_0[1:2, 2],
         model = "empty") 

# Model 2: Only intervention
interv <- lm(chg_viol ~ prop_viol_lag + interv_lag_prop, data = acled_cleaned)
cluster_interv <- cluster.vcov(interv, acled_cleaned$country)
interv_gid <- sqrt(diag(cluster_interv))

# Model 3: Only excessive force
exforc <- lm(chg_viol ~ prop_viol_lag + exforc_lag_prop, data = acled_cleaned)
cluster_exforc <- cluster.vcov(exforc, acled_cleaned$country)
exforc_gid <- sqrt(diag(cluster_exforc))

# Model 4: Only repression of violent protest
repviol <- lm(chg_viol ~ prop_viol_lag + repviol_lag_prop, data = acled_cleaned)
cluster_repviol <- cluster.vcov(repviol, acled_cleaned$country)
repviol_gid <- sqrt(diag(cluster_repviol))

# Model 5: Only proportion organized
prop_org <- lm(chg_viol ~ prop_viol_lag + prop_organized, data = acled_cleaned)
cluster_org <- cluster.vcov(prop_org, acled_cleaned$country)
org_gid <- sqrt(diag(cluster_org))

# Model 6: Baseline + past level of violence
base_lagpv <- lm(chg_viol ~ interv_lag_prop + exforc_lag_prop + repviol_lag_prop + prop_organized + prop_viol_lag,
                 data = acled_cleaned)
cluster_gid2 <- cluster.vcov(base_lagpv, acled_cleaned$country)
ols2_gid <- sqrt(diag(cluster_gid2))
ols2_se <- coeftest(base_lagpv, cluster_gid2)

cis_lagpv <- confint(ols2_se)
lagpv_df <- broom::tidy(ols2_se) %>%
  mutate(ci.lwr = cis_lagpv[1:6, 1],
         ci.upr = cis_lagpv[1:6, 2],
         model = "combined") 

# Model 7: Base variables + lagged DV + lagged proportion of violence
autoreg2 <- lm(chg_viol ~ interv_lag_prop + exforc_lag_prop + repviol_lag_prop + 
                 prop_organized + 
                 prop_viol_lag +
                 lag_dv, data = acled_cleaned)
cluster_gid_auto2 <- cluster.vcov(autoreg2, acled_cleaned$country)
auto2_gid <- sqrt(diag(cluster_gid_auto2))
auto2_se <- coeftest(autoreg2, cluster_gid_auto2)

# Model 8: Fixed effects - GC - without lagged DV
ols_lim_fegc <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + prop_organized + prop_viol_lag + factor(gid), data = acled_cleaned)
cluster_lim_fegc <- cluster.vcov(ols_lim_fegc, acled_cleaned$country)
lim_fe_segc <- sqrt(diag(cluster_lim_fegc))

# Model 9: Fixed effects - country-year - conservative model
ols_lim_fecy <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + 
                     prop_organized +
                     prop_viol_lag + 
                     factor(country) + factor(year), data = acled_cleaned)
fecy_cluster <- cluster.vcov(ols_lim_fecy, acled_cleaned$gid)
fecy_se <- sqrt(diag(fecy_cluster))

# Create table
stargazer(empty_mod, interv, exforc, repviol, prop_org, base_lagpv, autoreg2, ols_lim_fegc, ols_lim_fecy,
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = "Change in prop. of violent events in week$_t$",
          covariate.labels = c("Constant",
                               "Prop. VP in week$_{t-1}$",
                               "Y$_{t-1}$",
                               "Mild rep. NVP in week$_{t-1}$",
                               "Harsh rep. NVP in week$_{t-1}$",
                               "Rep. VP in week$_{t-1}$",
                               "Prop. org. NVP"),
          type = 'latex',
          header = FALSE,
          no.space = TRUE,
          single.row = FALSE,
          title = "Baseline models",
          intercept.bottom = FALSE,
          intercept.top = TRUE,
          float.env = "sidewaystable",
          omit = c("gid", "country", "year"),
          se = list(ols0_gid, interv_gid, exforc_gid, repviol_gid, org_gid, ols2_gid, auto2_gid, lim_fe_segc, fecy_se),
          notes = "Country-level clustered SEs in parentheses",
          font.size = "footnotesize",
          column.sep.width = "3pt",
          omit.stat = c("f", "ser", "rsq"),
          add.lines = list(c("$\\Delta$ Adjusted R$^2$", " ", "0.002", "0.000", "0.017", "0.112", "0.132", "0.180", "0.251", "0.205"),
                           c("Fixed effects", " ", " ", " ", " ", " ", " ", " ", "Grid cell", "Country-year")),
          table.layout = "=ldc#-t-sa=n",
          column.labels = c("Baseline", "AR(1)", "FE"),
          column.separate = c(6, 1, 2))



########################
# Full models, table 4 #
########################

# 1: Base variables + lagged DV + lagged proportion of violence
autoreg1 <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + 
                 prop_organized + 
                 prop_viol_lag +
                 lag_dv, data = acled_cleaned)
cluster_gid_auto1 <- cluster.vcov(autoreg1, acled_cleaned$country)
auto1_gid <- sqrt(diag(cluster_gid_auto1))
auto1_se <- coeftest(autoreg1, cluster_gid_auto1)

cis_auto1 <- confint(auto1_se)

auto2_df <- broom::tidy(auto1_se) %>%
  mutate(ci.lwr = cis_auto1[1:6, 1],
         ci.upr = cis_auto1[1:6, 2],
         model = "autoreg_2")

# 2: OLS, structural variables only
ols_structural <- lm(chg_viol ~ 
                       ttime_avg_log + 
                       libdem + libdem_sq + gdp_cap_log + 
                       tenure_lag + elected_lag + 
                       unemp_rate, data = acled_cleaned) 
cluster_gidstr <- cluster.vcov(ols_structural, acled_cleaned$country)
structural_se <- sqrt(diag(cluster_gidstr))

# 3: OLS, structural variables with repression and organization
ols_str_rep <- lm(chg_viol ~  
                    ttime_avg_log + 
                    libdem + libdem_sq + 
                    gdp_cap_log +
                    tenure_lag + elected_lag + 
                    unemp_rate +
                    interv_lag_prop + repviol_lag_prop + 
                    prop_organized +
                    prop_viol_lag, data = acled_cleaned)
cluster_gidstrrep <- cluster.vcov(ols_str_rep, acled_cleaned$country)
strrep_se <- sqrt(diag(cluster_gidstrrep))

# 4: OLS, full model
ols_full <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + 
                 prop_organized +
                 prop_viol_lag + 
                 ttime_avg_log +
                 libdem + libdem_sq +
                 gdp_cap_log +
                 tenure_lag + elected_lag + 
                 unemp_rate +
                 duration_log + 
                 n_ongoing_lag + diffusion, data = acled_cleaned)
cluster_gidfull <- cluster.vcov(ols_full, acled_cleaned$country)
full_se <- sqrt(diag(cluster_gidfull))
full_se2 <- coeftest(ols_full, cluster_gidfull)

# 5: Full autoregressive model 
auto_lim <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + 
                 prop_organized +
                 prop_viol_lag +
                 lag_dv +
                 ttime_avg_log +
                 libdem + libdem_sq +
                 gdp_cap_log +
                 tenure_lag +
                 elected_lag + 
                 unemp_rate +
                 duration_log +
                 n_ongoing_lag + diffusion, data = acled_cleaned)
cluster_autolim <- cluster.vcov(auto_lim, acled_cleaned$country)
autolim_gid <- sqrt(diag(cluster_autolim))

autolim_se <- coeftest(auto_lim, cluster_autolim)

cis_autolim <- confint(autolim_se)

autolim_df <- broom::tidy(autolim_se) %>%
  slice(1:6) %>%
  mutate(ci.lwr = cis_autolim[1:6, 1],
         ci.upr = cis_autolim[1:6, 2],
         model = "auto_full")


# Create table
stargazer(autoreg1, ols_structural, ols_str_rep, ols_full, auto_lim, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = "Change in proportion of violent events in week$_t$",
          covariate.labels = c("Constant",
                               "Mild rep. NVP$^\\diamond$",
                               "Rep. VP$^\\diamond$",
                               "Prop. org. NVP",
                               "Prop. VP$^\\diamond$",
                               "Y$_{t-1}$",
                               "Avg. travel time",
                               "Lib. dem.",
                               "Lib. dem.$^2$",
                               "GDP/cap. (log)",
                               "Leader tenure",
                               "Elected",
                               "Unemp. rate",
                               "Duration (log)",
                               "Ongoing VP nb.$^\\diamond$",
                               "Diffusion$^\\diamond$"),
          model.names = FALSE,
          type = 'latex',
          header = FALSE,
          no.space = FALSE,
          single.row = FALSE,
          title = "Full models",
          intercept.bottom = FALSE,
          intercept.top = TRUE,
          # float.env = "sidewaystable",
          se = list(auto1_gid, structural_se, strrep_se, full_se, autolim_gid),
          omit.stat = c("rsq", "f", "ser"),
          notes = c("Country-level clustered SEs in parentheses", "$^\\diamond$ Lagged one week"),
          font.size = "scriptsize",
          table.layout = "=ldc#-t-sa=n",
          column.labels = c("AR(1)", "Base", "AR(1)"),
          column.separate = c(1, 3, 1))


###################################
# Coefficient estimates, figure 8 #
###################################
parsimoneous <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + prop_organized + prop_viol_lag, data = acled_cleaned)
cluster_pars <- cluster.vcov(parsimoneous, acled_cleaned$country)
pars_gid <- sqrt(diag(cluster_pars))
pars_se <- coeftest(parsimoneous, cluster_pars)
cis_pars <- confint(pars_se)
pars_df <- broom::tidy(pars_se) %>%
  mutate(ci.lwr = cis_pars[1:5, 1],
         ci.upr = cis_pars[1:5, 2],
         model = "parsimoneous")

cis_full <- confint(full_se2)

full_df <- broom::tidy(full_se2) %>%
  slice(1:5) %>%
  mutate(ci.lwr = cis_full[1:5, 1],
         ci.upr = cis_full[1:5, 2],
         model = "full") 

base_auto <- rbind(pars_df, auto2_df, full_df, autolim_df)

base_auto1 <- base_auto %>% 
  mutate(term = factor(term, levels = c("(Intercept)", "interv_lag_prop", "repviol_lag_prop", "prop_organized", "prop_viol_lag", "lag_viol")))

palette_ba <- c("parsimoneous" = "#FF7F50", "autoreg_2" = "#008B8B", "full" = "#76D5F2", "auto_full" = "#FF0000")

ggplot(base_auto1, aes(x = estimate, y = term)) +
  geom_point(data = filter(base_auto1, model == "parsimoneous"), aes(color = "parsimoneous"), size = 2, position = position_nudge(y = 0.1)) +
  geom_errorbar(data = filter(base_auto1, model == "parsimoneous"), aes(xmin = ci.lwr, xmax = ci.upr, color = "parsimoneous"),
                width = 0.1, size = 0.75, position = position_nudge(y = 0.1)) +
  geom_point(data = filter(base_auto1, model == "autoreg_2"), aes(color = "autoreg_2"), size = 2, position = position_nudge(y = 0.3)) +
  geom_errorbar(data = filter(base_auto1, model == "autoreg_2"), aes(xmin = ci.lwr, xmax = ci.upr, color = "autoreg_2"),
                width = 0.1, size = 0.75, position = position_nudge(y = 0.3)) +
  geom_point(data = filter(base_auto1, model == "full"), aes(color = "full"), size = 2, position = position_nudge(y = -0.1)) +
  geom_errorbar(data = filter(base_auto1, model == "full"), aes(xmin = ci.lwr, xmax = ci.upr, color = "full"),
                width = 0.1, size = 0.75, position = position_nudge(y = -0.1)) +
  geom_point(data = filter(base_auto1, model == "auto_full"), aes(color = "auto_full"), size = 2, position = position_nudge(y = -0.3)) +
  geom_errorbar(data = filter(base_auto1, model == "auto_full"), aes(xmin = ci.lwr, xmax = ci.upr, color = "auto_full"),
                width = 0.1, size = 0.75, position = position_nudge(y = -0.3)) +
  scale_y_discrete(labels = c("(Intercept)" = "Intercept",
                              "interv_lag_prop" = "Mild repression of NVP (lag)",
                              "repviol_lag_prop" = "Repression of VP (lag)",
                              "prop_organized" = "Proportion of organized protest",
                              "prop_viol_lag" = "Proportion of violent protest (lag)",
                              "lag_viol" = "Lagged DV"),
                   limits = rev(levels(base_auto1$term))) +
  theme_bw() +
  ylab(NULL) + xlab("Coefficient") +
  geom_vline(aes(xintercept = 0), linetype = 2, color = "orange3") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_colour_manual(values = palette_ba, name = NULL,
                      breaks = c("parsimoneous", "autoreg_2", "full", "auto_full"),
                      labels = c("Base model (no controls)", "AR(1) model (no controls)", "Base model (with controls)", "AR(1) model (with controls)")) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


###############################
# Interaction models, table 5 #
###############################

nona <- acled_cleaned %>% 
  filter(!is.na(lag_dv))

interact_1 <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + 
                   prop_organized + 
                   interv_lag_prop*prop_organized +
                   repviol_lag_prop*prop_organized +
                   prop_viol_lag, data = acled_cleaned)
cluster_gid4 <- cluster.vcov(interact_1, acled_cleaned$country)
ols4_gid <- sqrt(diag(cluster_gid4))

interact_2 <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + 
                   prop_organized +
                   interv_lag_prop*prop_organized +
                   repviol_lag_prop*prop_organized +
                   prop_viol_lag +
                   ttime_avg_log +
                   libdem + libdem_sq +
                   gdp_cap_log +
                   tenure_lag +
                   elected_lag + 
                   unemp_rate +
                   duration_log +
                   n_ongoing_lag + diffusion, 
                 data = acled_cleaned)
cluster_gid5 <- cluster.vcov(interact_2, acled_cleaned$country)
ols5_gid <- sqrt(diag(cluster_gid5))

interact_3 <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + 
                   prop_organized + 
                   interv_lag_prop*prop_organized +
                   repviol_lag_prop*prop_organized +
                   prop_viol_lag + lag_dv, data = acled_cleaned)
cluster_gid6 <- cluster.vcov(interact_3, acled_cleaned$country)
ols6_gid <- sqrt(diag(cluster_gid6))

interact_4 <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + 
                   prop_organized + 
                   interv_lag_prop*prop_organized +
                   repviol_lag_prop*prop_organized + 
                   prop_viol_lag + 
                   factor(gid), data = acled_cleaned)
cluster_gid7 <- cluster.vcov(interact_4, acled_cleaned$country)
ols7_gid <- sqrt(diag(cluster_gid7))

interact_5 <- lm(chg_viol ~ interv_lag_prop + repviol_lag_prop + 
                   prop_organized + 
                   interv_lag_prop*prop_organized +
                   repviol_lag_prop*prop_organized + 
                   prop_viol_lag + 
                   factor(country) + factor(year), data = acled_cleaned)
cluster_gid8 <- cluster.vcov(interact_5, acled_cleaned$country)
ols8_gid <- sqrt(diag(cluster_gid8))

# Create table 

stargazer(interact_1, interact_2, interact_3, interact_4, interact_5,
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = "Change in proportion of violent events in week$_t$",
          covariate.labels = c("Constant",
                               "Mild rep. NVP$^\\diamond$",
                               "Rep. VP$^\\diamond$",
                               "Prop. org. NVP",
                               "Prop. VP$^\\diamond$",
                               "Avg. travel time",
                               "Lib. dem.",
                               "Lib. dem.$^2$",
                               "GDP/cap. (log)",
                               "Leader tenure",
                               "Elected",
                               "Unemp. rate",
                               "Duration (log)",
                               "Ongoing VP nb.$^\\diamond$",
                               "Diffusion$^\\diamond$",
                               "Y$_{t-1}$",
                               "Mild rep. (NVP) $\\times$ Prop. org.",
                               "Rep. (VP) $\\times$ Prop. org."),
          type = 'latex',
          header = FALSE,
          no.space = TRUE,
          single.row = TRUE,
          title = "Interaction models",
          intercept.bottom = FALSE,
          intercept.top = TRUE,
          float.env = "sidewaystable",
          se = list(ols4_gid, ols5_gid, ols6_gid, ols7_gid, ols8_gid),
          omit.stat = c("rsq", "ser", "f"),
          notes = "Country-level clustered SEs in parentheses",
          font.size = "footnotesize",
          omit = c("gid", "country", "year"),
          add.lines = list(c("Fixed effects", " ", " ", " ", "Grid cell", "Country-year")),
          column.labels = c("Base", "AR(1)", "FE"),
          column.separate = c(2, 1, 2),
          table.layout = "=ldc#-t-sa=n")

# Interaction figures

# Fig. 9
inter_nvp <- johnson_neyman(interact_3, pred = interv_lag_prop, modx = prop_organized, vmat = cluster_gid6, mod.range = c(0, 1), sig.color = "cyan4", insig.color = "coral", title = NULL, control.fdr = TRUE)
inter_nvp$plot + xlab("Proportion of organized protest") + ylab("Slope of mild rep. of NVP")

# Fig. 10
inter_rv <- johnson_neyman(interact_3, pred = repviol_lag_prop, modx = prop_organized, vmat = cluster_gid6, mod.range = c(0, 1), sig.color = "cyan4", insig.color = "coral", title = NULL, control.fdr = TRUE) 
inter_rv$plot + xlab("Proportion of organized protest") + ylab("Slope of repression of VP")

# Fig. 11
inter_nvp_fe <- johnson_neyman(interact_4, pred = interv_lag_prop, modx = prop_organized, vmat = cluster_gid7, mod.range = c(0, 1), sig.color = "cyan4", insig.color = "coral", title = NULL, control.fdr = TRUE)
inter_nvp_fe$plot + xlab("Proportion of organized protest") + ylab("Slope of mild rep. of NVP")

# Fig. 12
inter_rv_fe <- johnson_neyman(interact_4, pred = repviol_lag_prop, modx = prop_organized, vmat = cluster_gid7, mod.range = c(0, 1), sig.color = "cyan4", insig.color = "coral", title = NULL, control.fdr = TRUE) 
inter_rv_fe$plot + xlab("Proportion of organized protest") + ylab("Slope of repression of VP")

# Fig. 13
inter_ori_ar1 <- johnson_neyman(interact_3, pred = prop_organized, modx = interv_lag_prop, vmat = cluster_gid6, mod.range = c(0, 1), sig.color = "cyan4", insig.color = "coral", title = NULL, control.fdr = TRUE)
inter_ori_ar1$plot + xlab("Mild rep. of NVP") + ylab("Slope of organized protest")

# Fig. 14
inter_orv_ar1 <- johnson_neyman(interact_3, pred = prop_organized, modx = repviol_lag_prop, vmat = cluster_gid6, mod.range = c(0, 1), sig.color = "cyan4", insig.color = "coral", title = NULL, control.fdr = TRUE) 
inter_orv_ar1$plot + xlab("Repression of VP") + ylab("Slope of organized protest")


##########################
# Temporal lags, table 8 #
##########################

# Lagged independent variables - two weeks

test_lag <- acled_cleaned %>%
  group_by(prot_cycle) %>%
  mutate(int_lag2 = lag(interv_lag_prop, n = 1),
         exf_lag2 = lag(exforc_lag_prop, n = 1),
         rvp_lag2 = lag(repviol_lag_prop, n = 1),
         org_lag = lag(prop_organized, n = 1),
         org_lag2 = lag(prop_organized, n = 2)) %>%
  filter(!is.na(int_lag2) & !is.na(exf_lag2) & !is.na(rvp_lag2) & !is.na(org_lag2)) %>%
  mutate(lag_dv2 = lag(chg_viol, n = 1)) %>%
  ungroup()

extra_lag_base <- lm(chg_viol ~ interv_lag_prop + int_lag2 + 
                       repviol_lag_prop + rvp_lag2 + 
                       prop_organized + org_lag + org_lag2 +
                       prop_viol_lag,
                     data = test_lag)
cl_base <- cluster.vcov(extra_lag_base, test_lag$country)
extra_base_se <- sqrt(diag(cl_base))

extra_lag_auto1 <- lm(chg_viol ~ interv_lag_prop + int_lag2 + 
                        repviol_lag_prop + rvp_lag2 + 
                        prop_organized + org_lag + org_lag2 +
                        prop_viol_lag + 
                        lag_dv2,
                      data = test_lag)
cl_auto1 <- cluster.vcov(extra_lag_auto1, test_lag$country)
se_auto1_extra <- sqrt(diag(cl_auto1))

extra_lag_fe <- lm(chg_viol ~ interv_lag_prop + int_lag2 + 
                     repviol_lag_prop + rvp_lag2 + 
                     prop_organized + org_lag + org_lag2 +
                     prop_viol_lag + factor(gid),
                   data = test_lag)
cl_efe <- cluster.vcov(extra_lag_fe, test_lag$country)
se_efe <- sqrt(diag(cl_efe))

# Lagged independent variables, three weeks
test_lag3 <- test_lag %>%
  group_by(prot_cycle) %>%
  mutate(int_lag3 = lag(int_lag2, n = 1),
         rvp_lag3 = lag(rvp_lag2, n = 1),
         org_lag3 = lag(org_lag2, n = 1)) %>%
  filter(!is.na(int_lag3) & !is.na(rvp_lag3) & !is.na(org_lag3)) %>% 
  mutate(lag_dv3 = lag(chg_viol, n = 1)) %>%
  ungroup() 

# Base
base3 <- lm(chg_viol ~ interv_lag_prop + int_lag2 + int_lag3 +
              repviol_lag_prop + rvp_lag2 + rvp_lag3 +
              prop_organized + org_lag + org_lag2 + org_lag3 +
              prop_viol_lag,
            data = test_lag3)
cl_base3 <- cluster.vcov(base3, test_lag3$country)
se_base3 <- sqrt(diag(cl_base3)) 

# AR(1)
auto3 <- lm(chg_viol ~ interv_lag_prop + int_lag2 + int_lag3 +
              repviol_lag_prop + rvp_lag2 + rvp_lag3 +
              prop_organized + org_lag + org_lag2 + org_lag3 +
              prop_viol_lag + lag_dv3,
            data = test_lag3)
cl_auto3 <- cluster.vcov(auto3, test_lag3$country)
se_auto3 <- sqrt(diag(cl_auto3))

# FE
fe3 <- lm(chg_viol ~ interv_lag_prop + int_lag2 + int_lag3 +
            repviol_lag_prop + rvp_lag2 + rvp_lag3 +
            prop_organized + org_lag + org_lag2 + org_lag3 +
            prop_viol_lag + factor(gid),
          data = test_lag3)
cl_fe3 <- cluster.vcov(fe3, test_lag3$country)
se_fe3 <- sqrt(diag(cl_fe3))

# Create table
stargazer(extra_lag_base, base3, extra_lag_auto1, auto3, extra_lag_fe, fe3,
          se = list(extra_base_se, se_base3, se_auto1_extra, se_auto3, se_efe, se_fe3),
          header = FALSE,
          title = "Additional temporal lags",
          # float.env = "sidewaystable",
          font.size = "scriptsize",
          omit.stat = c("rsq", "ser", "f"),
          omit = "gid",
          type = "latex",
          column.labels = c("OLS", "AR(1)", "FE"),
          column.separate = c(2, 2, 2),
          dep.var.labels = "Change in proportion of violent events in week$_t$",
          covariate.labels = c("Constant",
                               "Interv. NVP (t-1)",
                               "Interv. NVP (t-2)",
                               "Interv. NVP (t-3)",
                               "Repr. VP (t-1)",
                               "Repr. VP (t-2)",
                               "Repr. VP (t-3)",
                               "Prop. Org. (t)",
                               "Prop. Org. (t-1)",
                               "Prop. Org. (t-2)",
                               "Prop. Org. (t-3)",
                               "Prop. VP (t-1)",
                               "Y$_{t-2}$",
                               "Y$_{t-3}$"),
          intercept.bottom = FALSE,
          intercept.top = TRUE)
