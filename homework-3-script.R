library(curl)
library(readr)
library(broom)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
f <- curl("https://raw.githubusercontent.com/raneyej/ada-2021-datasets/main/KamilarAndCooperData.csv")
d <- read.csv(f, header = TRUE, sep = ",")
head(d)

bs_lm <- lm(WeaningAge_d ~ Brain_Size_Species_Mean, data = d)
summary(bs_lm)

bs_lm_log <- lm(log(WeaningAge_d) ~ log(Brain_Size_Species_Mean), data = d)
summary(bs_lm_log)

brain_plot <- ggplot(data = d, aes(x = Brain_Size_Species_Mean, y = WeaningAge_d)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color = "blue") +
  stat_regline_equation(label.y = 1500, aes(label = ..eq.label..)) +
  ggtitle(label = "Brain size and weaning age") + xlab("Brain size") + ylab("Weaning age in days")
brain_plot

log_brain_plot <- ggplot(data = d, aes(x = log(Brain_Size_Species_Mean), y = log(WeaningAge_d))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue") +
  stat_regline_equation(label.y = 8, aes(label = ..eq.label..)) +
  ggtitle(label = "Log transformed brain size and weaning age") + xlab("Log brain size") + ylab("Log weaning age in days")
log_brain_plot

tidy(bs_lm)
tidy(bs_lm_log)

beta0 <- tidy(bs_lm) %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)
beta0
beta1 <- tidy(bs_lm_log) %>%
  filter(term == "log(Brain_Size_Species_Mean)") %>%
  pull(estimate)
beta1

beta1 == 0
beta1!=0

ci_beta1 <- predict(bs_lm_log, newdata = data.frame(Brain_Size_Species_Mean = 2.64), interval = "confidence", level = 0.90)
ci_beta1

ci_bs_lm <- predict(bs_lm, newdata = data.frame(Brain_Size_Species_Mean <- d$Brain_Size_Species_Mean), interval = "confidence", level = 0.90)
ci_bs_lm

ci_bs_lm_log <- predict(bs_lm_log, newdata = data.frame(Brain_Size_Species_Mean <- d$Brain_Size_Species_Mean), interval = "confidence", level = 0.90)
ci_bs_lm_log

ci_array <- cbind(d$Brain_Size_Species_Mean, ci_bs_lm)
ci_df <- as.data.frame(ci_array)
ci_df

x1 <- d$Brain_Size_Species_Mean
x1
x2 <- log(x1)
x2

ci_array2 <- cbind(x2, ci_bs_lm_log)
ci_df2 <- as.data.frame(ci_array2)
ci_df2

ci_plot1 <- ggplot(data = d, aes(x = Brain_Size_Species_Mean, y = WeaningAge_d)) +
  geom_point() +
  geom_line(data = ci_df, aes( x = V1, y = fit, color = "Fit")) +
  geom_line(data = ci_df, aes(x = V1, y = lwr, color = "Lower")) +
  geom_line(data = ci_df, aes(x = V1, y = upr, color = "Upper")) +
  stat_regline_equation(label.y = 1500, aes(label = ..eq.label..)) +
  ggtitle(label = "Brain size and weaning age") + xlab("Brain size") + ylab("Weaning age in days")
ci_plot1

ci_plot2 <- ggplot(data = d, aes(x = log(Brain_Size_Species_Mean), y = log(WeaningAge_d))) +
  geom_point() +
  geom_line(data = ci_df2, aes( x = x2, y = fit, color = "Fit")) +
  geom_line(data = ci_df2, aes(x = x2, y = lwr, color = "Lower")) +
  geom_line(data = ci_df2, aes(x = x2, y = upr, color = "Upper")) +
  stat_regline_equation(label.y = 7, aes(label = ..eq.label..)) +
  ggtitle(label = "Brain size and weaning age") + xlab("Log brain size") + ylab("Log weaning age in days")
ci_plot2

prediction <- predict(bs_lm, newdata = data.frame(Brain_Size_Species_Mean = 750), interval = "prediction", level = 0.90)
prediction




library(boot)

gs_lm_log <-  lm(log(MeanGroupSize) ~ log(Body_mass_female_mean), data = d)
gs_lm_log
summary(gs_lm_log)
tidy(gs_lm_log)

gs_beta0 <- tidy(gs_lm_log) %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)
gs_beta0
gs_beta1 <- tidy(gs_lm_log) %>%
  filter(term == "log(Body_mass_female_mean)") %>%
  pull(estimate)
gs_beta1


boot_betas <- data.frame(gs_beta0_boot = 1:1000, gs_beta1_boot = 1:1000)
n = nrow(d)
for(i in 1:1000) {
  s <- sample_n(d, size = n, replace = TRUE)
  gs_boot <- lm(log(MeanGroupSize) ~ log(Body_mass_female_mean), data = s)
  betas <- gs_boot$coefficients
  gs_beta0_boot <- as.numeric(betas[1])
  gs_beta1_boot <- as.numeric(betas[2])
  boot_betas$gs_beta0_boot[[i]] <- gs_beta0_boot
  boot_betas$gs_beta1_boot[[i]] <- gs_beta1_boot
}
boot_betas


hist_beta0 <- hist(boot_betas$gs_beta0_boot, main = "Distribution of intercepts", xlab = "β0")
hist_beta1 <- hist(boot_betas$gs_beta1_boot, main = "Distribution of slopes", xlab = "β1")
hist_beta0
hist_beta1

beta0_se <- sd(boot_betas$gs_beta0_boot)
beta0_se
beta1_se <- sd(boot_betas$gs_beta1_boot)
beta1_se


quantile(boot_betas$gs_beta0_boot, prob = 0.025)
quantile(boot_betas$gs_beta0_boot, prob = 0.975)

quantile(boot_betas$gs_beta1_boot, prob = 0.025)
quantile(boot_betas$gs_beta1_boot, prob = 0.975)


gs_tidied <- tidy(gs_lm_log)
gs_tidied

se_comp <- data.frame(Method = c("b0_lm", "b0_se", "b1_lm", "b1_se"), 
           Value = c(0.438, 0.483, 0.0556, 0.0616))
se_comp

summary(gs_lm_log)

gs_lm_cis <- confint(gs_lm_log, level = 0.95)
gs_lm_cis

cis_comp <- data.frame(Method = c("qt_b0_lower", "lm_b0_lower", "qt_b0_upper", "lm_b0_upper", "qt_b1_lower", "lm_b1_lower", "qt_b1_upper", "lm_b1_upper"), 
                       Value = c(-2.684, -2.642, -0.837, -0.912, 0.378, 0.396, 0.616, 0.625))
cis_comp


boot_lm <- function(d, model, conf.level = 0.95, reps = 1000) {
  df <- data.frame(ce = c("Beta0", "Beta1"), ce_value = c(0,0), ce_se = c(0,0), ci_lower = c(0,0), ci_upper = c(0,0), beta_bs_mean = c(0,0), beta_bs_se = c(0,0), bs_ci_lower = c(0,0), bs_ci_upper = c(0,0))
  mod = lm(eval(parse(text = model)), data = d)
  tid_mod <- tidy(mod)
  df$ce_value[1] <- as.numeric(tid_mod[1,2])
  df$ce_value[2] <- as.numeric(tid_mod[2,2])
  df$ce_se[1] = as.numeric(tid_mod[1,3])
  df$ce_se[2] = as.numeric(tid_mod[2,3])
  mod_ci <- confint(mod, level = conf.level)
  df$ci_lower[1] <- mod_ci[1,1]
  df$ci_lower[2] <- mod_ci[2,1]
  df$ci_upper[1] <- mod_ci[1,2]
  df$ci_upper[2] <- mod_ci[2,2]
  set.seed(1)
  bootstrap <- data.frame(beta0 = 1:reps, beta1 = 1:reps)
  n <- nrow(d)
  for(i in 1:reps){
    s <- sample_n(d, size = n, replace = TRUE)
    mod_boot <- lm(eval(parse(text = model)), data = s)
    ce_boot <- mod_boot$coefficients
    beta0 <- as.numeric(ce_boot[1])
    beta1 <- as.numeric(ce_boot[2])
    bootstrap$beta0[[i]] <- beta0
    bootstrap$beta1[[i]] <- beta1
  }
  df$beta_bs_mean[1] <- mean(bootstrap$beta0)
  df$beta_bs_mean[2] <- mean(bootstrap$beta1)
  df$beta_bs_se[1] <- sd(bootstrap$beta0)
  df$beta_bs_se[2] <- sd(bootstrap$beta1)
  alpha <- 1 - conf.level
  df$bs_ci_lower[1] <- quantile(bootstrap$beta0, alpha / 2)
  df$bs_ci_lower[2] <- quantile(bootstrap$beta1, alpha / 2)
  df$bs_ci_upper[1] <- quantile(bootstrap$beta0, 1 - (alpha / 2))
  df$bs_ci_upper[2] <- quantile(bootstrap$beta1, 1 - (alpha / 2))
  df
}

group_size <- boot_lm(d, "log(MeanGroupSize) ~ log(Body_mass_female_mean)")
group_size
day_length <- boot_lm(d, "log(DayLength_km) ~log(Body_mass_female_mean)")
day_length
day_length2 <- boot_lm(d, "log(DayLength_km) ~ log(Body_mass_female_mean) + log(MeanGroupSize)")
day_length2
