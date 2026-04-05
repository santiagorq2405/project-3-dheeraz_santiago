# MATH-516 Applied Statistics - Project 3: Credit Repayment Risk Assessment
# Causal effect of PPP enrollment (A) on credit default (Y)


install.packages(c("tidyverse", "scales", "patchwork",
                     "knitr", "kableExtra",
                     "glmnet", "bnlearn", "boot", "igraph"))

library(tidyverse)
library(scales)
library(patchwork)
library(knitr)
library(kableExtra)
library(glmnet)
library(bnlearn)
library(boot)
library(igraph)

set.seed(516)

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(size = 13, face = "bold"),
      plot.subtitle    = element_text(size = 11, color = "grey40"),
      axis.title       = element_text(size = 11),
      axis.text        = element_text(size = 10),
      legend.text      = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
)

# Load and Preprocess Data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df_raw <- read.csv("../updated_datasets/low3007upd.csv")

cat("Dimensions:", nrow(df_raw), "rows x", ncol(df_raw), "columns\n")
cat("Columns:", paste(names(df_raw), collapse = ", "), "\n")
str(df_raw)
cat("\nMissing values:", sum(is.na(df_raw)), "\n")

# Collapse sparse EDUCATION levels (4, 5, 6 -> 4)
df_raw <- df_raw %>%
  mutate(EDUCATION = ifelse(EDUCATION %in% c(4, 5, 6), 4L, as.integer(EDUCATION)))

cat("\nOutcome Y distribution:\n")
print(table(df_raw$Y))
cat("Default rate:", mean(df_raw$Y), "\n")

cat("\nTreatment A distribution:\n")
print(table(df_raw$A))
cat("PPP enrollment rate:", mean(df_raw$A), "\n")

cat("\nY x A cross-tabulation:\n")
print(table(Y = df_raw$Y, A = df_raw$A))

cat("\nEducation distribution (after collapsing):\n")
print(table(df_raw$EDUCATION))

cat("\nSex distribution:\n")
print(table(df_raw$SEX))

cat("\nAge summary:\n")
print(summary(df_raw$AGE))

cat("\nCredit limit summary:\n")
print(summary(df_raw$LIMIT_BAL))


# Exploratory Data Analysis

# Naive association (not causal)
naive_p1 <- mean(df_raw$Y[df_raw$A == 1])
naive_p0 <- mean(df_raw$Y[df_raw$A == 0])
naive_diff <- naive_p1 - naive_p0

cat("\nNaive P(Y=1|A=1) =", round(naive_p1, 4), "\n")
cat("Naive P(Y=1|A=0) =", round(naive_p0, 4), "\n")
cat("Naive risk difference =", round(naive_diff, 4), "\n")

# 2b. Treatment distribution by sex
p_sex <- ggplot(df_raw, aes(x = factor(SEX, labels = c("Female", "Male")),
                            fill = factor(A, labels = c("No PPP", "PPP")))) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(x = "Sex", y = "Count", fill = "Treatment") +
  scale_fill_manual(values = c("No PPP" = "grey60", "PPP" = "steelblue"))

ggsave("p_sex_treatment.png", p_sex, width = 6, height = 4)
print(p_sex)

# Treatment distribution by education
edu_labels <- c("1" = "Graduate", "2" = "University", "3" = "HighSchool", "4" = "Other")
p_edu <- ggplot(df_raw, aes(x = factor(EDUCATION, labels = edu_labels),
                            fill = factor(A, labels = c("No PPP", "PPP")))) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(x = "Education", y = "Count", fill = "Treatment") +
  scale_fill_manual(values = c("No PPP" = "grey60", "PPP" = "steelblue"))

ggsave("p_edu_treatment.png", p_edu, width = 7, height = 4)
print(p_edu)

# Default rate by treatment and age
p_age <- ggplot(df_raw, aes(x = AGE, fill = factor(Y, labels = c("No default", "Default")))) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
  facet_wrap(~ factor(A, labels = c("No PPP", "PPP"))) +
  labs(x = "Age", y = "Count", fill = "Outcome") +
  scale_fill_manual(values = c("No default" = "grey60", "Default" = "firebrick"))

ggsave("p_age_default.png", p_age, width = 8, height = 4)
print(p_age)

# Default rate by treatment and credit limit
p_limit <- ggplot(df_raw, aes(x = LIMIT_BAL,
                              fill = factor(Y, labels = c("No default", "Default")))) +
  geom_histogram(bins = 25, alpha = 0.7, position = "identity") +
  facet_wrap(~ factor(A, labels = c("No PPP", "PPP"))) +
  labs(x = "Credit Limit (NT$)", y = "Count", fill = "Outcome") +
  scale_fill_manual(values = c("No default" = "grey60", "Default" = "firebrick")) +
  scale_x_continuous(labels = comma)

ggsave("p_limit_default.png", p_limit, width = 8, height = 4)
print(p_limit)

# PAY_0 distribution by treatment
p_pay0 <- ggplot(df_raw, aes(x = factor(PAY_0),
                             fill = factor(A, labels = c("No PPP", "PPP")))) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(x = "PAY_0 (most recent repayment status)", y = "Count", fill = "Treatment") +
  scale_fill_manual(values = c("No PPP" = "grey60", "PPP" = "steelblue"))

ggsave("p_pay0_treatment.png", p_pay0, width = 7, height = 4)
print(p_pay0)

# Confounding evidence: treatment rate varies by covariates
cat("\nPPP rate by sex:\n")
print(df_raw %>% group_by(SEX) %>%
        summarise(ppp_rate = mean(A), n = n(), .groups = "drop"))

cat("\nPPP rate by education:\n")
print(df_raw %>% group_by(EDUCATION) %>%
        summarise(ppp_rate = mean(A), n = n(), .groups = "drop"))

cat("\nDefault rate by education and treatment:\n")
print(df_raw %>% group_by(EDUCATION, A) %>%
        summarise(default_rate = mean(Y), n = n(), .groups = "drop") %>%
        arrange(EDUCATION, A))



# Model Data Preparation

# Covariate names (everything except Y and A)
covar_names <- setdiff(names(df_raw), c("Y", "A"))
cat("\nCovariates:", paste(covar_names, collapse = ", "), "\n")
cat("Number of covariates:", length(covar_names), "\n")


# Adjustment Set 1 - All Covariates

cat("\nAdjustment Set 1: All covariates\n")

# Outcome regression (standardization)
fit_or_all <- glm(Y ~ ., data = df_raw, family = binomial)
cat("\n logistic fit (all covariates):\n")
summary(fit_or_all)

# Standardise: predict under do(A=1) and do(A=0) for the whole sample
d1 <- df_raw; d1$A <- 1
d0 <- df_raw; d0$A <- 0

ey1_or_all <- mean(predict(fit_or_all, newdata = d1, type = "response"))
ey0_or_all <- mean(predict(fit_or_all, newdata = d0, type = "response"))
ace_or_all <- ey1_or_all - ey0_or_all

cat(sprintf("OR (all): E[Y^1]=%.4f, E[Y^0]=%.4f, ACE=%.4f\n",
            ey1_or_all, ey0_or_all, ace_or_all))

# IPW estimator
fit_ps_all <- glm(A ~ ., data = df_raw[, c("A", covar_names)], family = binomial)
ps_all <- predict(fit_ps_all, type = "response")

cat("\nPropensity score summary (all covariates):\n")
print(summary(ps_all))

# Trim extreme propensity scores
PS_TRIM_LO <- 0.01
PS_TRIM_HI <- 0.99
ps_all_t <- pmin(pmax(ps_all, PS_TRIM_LO), PS_TRIM_HI)

n_trimmed <- sum(ps_all < PS_TRIM_LO | ps_all > PS_TRIM_HI)
cat("Trimmed", n_trimmed, "extreme propensity scores\n")

# Hajek estimator (stabilised)
w1_all <- (df_raw$A) / ps_all_t
w0_all <- (1 - df_raw$A) / (1 - ps_all_t)

ey1_ipw_all <- sum(w1_all * df_raw$Y) / sum(w1_all)
ey0_ipw_all <- sum(w0_all * df_raw$Y) / sum(w0_all)
ace_ipw_all <- ey1_ipw_all - ey0_ipw_all

cat(sprintf("IPW (all): E[Y^1]=%.4f, E[Y^0]=%.4f, ACE=%.4f\n",
            ey1_ipw_all, ey0_ipw_all, ace_ipw_all))

# Propensity score overlap plot
ps_df_all <- tibble(ps = ps_all, A = factor(df_raw$A, labels = c("No PPP", "PPP")))

p_ps_all <- ggplot(ps_df_all, aes(x = ps, fill = A)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(x = "Propensity score P(A=1|L)", y = "Count",
       fill = "Treatment", title = "PS overlap (all covariates)") +
  scale_fill_manual(values = c("No PPP" = "grey60", "PPP" = "steelblue"))

ggsave("p_ps_all.png", p_ps_all, width = 7, height = 4)
print(p_ps_all)

# Weight diagnostics
cat("\nIPW weights (treated):\n")
print(summary(w1_all[df_raw$A == 1]))
cat("IPW weights (control):\n")
print(summary(w0_all[df_raw$A == 0]))



# Adjustment Set 2 - LASSO Variable Selection

cat("\nAdjustment Set 2: LASSO\n")

# LASSO on outcome model Y ~ L to select covariates
X_lasso <- model.matrix(~ . - 1, data = df_raw[, covar_names])
y_lasso <- df_raw$Y

set.seed(516)
cv_fit <- cv.glmnet(X_lasso, y_lasso, family = "binomial",
                    alpha = 1, nfolds = 10)

cat("Lambda min:", cv_fit$lambda.min, "\n")
cat("Lambda 1se:", cv_fit$lambda.1se, "\n")

# Plot CV curve
png("p_lasso_cv.png", width = 700, height = 500)
plot(cv_fit, main = "LASSO cross-validation")
dev.off()
plot(cv_fit, main = "LASSO cross-validation")

# Extract selected covariates at lambda.1se
lasso_coefs <- coef(cv_fit, s = "lambda.1se")
lasso_vars <- rownames(lasso_coefs)[which(lasso_coefs != 0)]
lasso_vars <- setdiff(lasso_vars, "(Intercept)")

cat("\nLASSO selected covariates (lambda.1se):\n")
print(lasso_vars)
cat("Number selected:", length(lasso_vars), "\n")

# Also check lambda min for comparison
lasso_coefs_min <- coef(cv_fit, s = "lambda.min")
lasso_vars_min <- rownames(lasso_coefs_min)[which(lasso_coefs_min != 0)]
lasso_vars_min <- setdiff(lasso_vars_min, "(Intercept)")
cat("\nLASSO selected covariates (lambda.min):", length(lasso_vars_min), "variables\n")

#  OR with LASSO-selected set
if (length(lasso_vars) > 0) {
  lasso_frm <- as.formula(paste("Y ~ A +", paste(lasso_vars, collapse = " + ")))
  fit_or_lasso <- glm(lasso_frm, data = df_raw, family = binomial)
  
  d1_l <- df_raw; d1_l$A <- 1
  d0_l <- df_raw; d0_l$A <- 0
  
  ey1_or_lasso <- mean(predict(fit_or_lasso, newdata = d1_l, type = "response"))
  ey0_or_lasso <- mean(predict(fit_or_lasso, newdata = d0_l, type = "response"))
  ace_or_lasso <- ey1_or_lasso - ey0_or_lasso
  
  cat(sprintf("\nOR (LASSO): E[Y^1]=%.4f, E[Y^0]=%.4f, ACE=%.4f\n",
              ey1_or_lasso, ey0_or_lasso, ace_or_lasso))
  
  # IPW with LASSO-selected set
  ps_frm_l <- as.formula(paste("A ~", paste(lasso_vars, collapse = " + ")))
  fit_ps_lasso <- glm(ps_frm_l, data = df_raw, family = binomial)
  ps_lasso <- predict(fit_ps_lasso, type = "response")
  ps_lasso_t <- pmin(pmax(ps_lasso, PS_TRIM_LO), PS_TRIM_HI)
  
  cat("PS range (LASSO):", round(min(ps_lasso), 4), "-", round(max(ps_lasso), 4), "\n")
  
  w1_lasso <- (df_raw$A) / ps_lasso_t
  w0_lasso <- (1 - df_raw$A) / (1 - ps_lasso_t)
  
  ey1_ipw_lasso <- sum(w1_lasso * df_raw$Y) / sum(w1_lasso)
  ey0_ipw_lasso <- sum(w0_lasso * df_raw$Y) / sum(w0_lasso)
  ace_ipw_lasso <- ey1_ipw_lasso - ey0_ipw_lasso
  
  cat(sprintf("IPW (LASSO): E[Y^1]=%.4f, E[Y^0]=%.4f, ACE=%.4f\n",
              ey1_ipw_lasso, ey0_ipw_lasso, ace_ipw_lasso))
} else {
  cat("LASSO selected no covariates at lambda.1se\n")
  ace_or_lasso <- naive_diff
  ace_ipw_lasso <- naive_diff
  lasso_vars <- covar_names
}



# Adjustment Set 3 - PC Algorithm (Markov Blanket)

cat("\nAdjustment Set 3: PC Algorithm\n")

# Convert all columns to numeric for bnlearn with Pearson correlation test
df_bn_num <- as.data.frame(lapply(df_raw, as.numeric))

set.seed(516)
pc_fit <- pc.stable(df_bn_num, test = "cor", alpha = 0.05)

cat("\nPC-stable result:\n")
print(pc_fit)

mb_Y_pc <- mb(pc_fit, "Y")
cat("\nMarkov blanket of Y from PC:\n")
print(mb_Y_pc)

L_pc <- setdiff(mb_Y_pc, c("Y", "A"))
cat("\nPC adjustment set:", paste(L_pc, collapse = ", "), "\n")
cat("Size:", length(L_pc), "\n")
cat("A is in MB(Y):", "A" %in% mb_Y_pc, "\n")

# Save the learned graph
bn_igraph <- as.igraph(pc_fit)

png("p_pc_graph.png", width = 900, height = 700)
set.seed(516)
plot(bn_igraph,
     vertex.size = 18,
     vertex.label.cex = 0.7,
     vertex.color = ifelse(V(bn_igraph)$name == "Y", "firebrick",
                           ifelse(V(bn_igraph)$name == "A", "steelblue",
                                  ifelse(V(bn_igraph)$name %in% mb_Y_pc, "gold", "grey80"))),
     vertex.label.color = "black",
     edge.arrow.size = 0.4,
     main = "PC-stable learned CPDAG (cor test)")
legend("bottomright",
       legend = c("Outcome (Y)", "Treatment (A)", "MB(Y)", "Other"),
       fill = c("firebrick", "steelblue", "gold", "grey80"),
       cex = 0.8)
dev.off()

set.seed(516)
plot(bn_igraph,
     vertex.size = 18,
     vertex.label.cex = 0.7,
     vertex.color = ifelse(V(bn_igraph)$name == "Y", "firebrick",
                           ifelse(V(bn_igraph)$name == "A", "steelblue",
                                  ifelse(V(bn_igraph)$name %in% mb_Y_pc, "gold", "grey80"))),
     vertex.label.color = "black",
     edge.arrow.size = 0.4,
     main = "PC-stable learned CPDAG (cor test)")
legend("bottomright",
       legend = c("Outcome (Y)", "Treatment (A)", "MB(Y)", "Other"),
       fill = c("firebrick", "steelblue", "gold", "grey80"),
       cex = 0.8)

cat("\nNeighbours of Y in CPDAG:\n")
print(bnlearn::nbr(pc_fit, "Y"))
cat("Parents of Y in CPDAG:\n")
print(bnlearn::parents(pc_fit, "Y"))
cat("Children of Y in CPDAG:\n")
print(bnlearn::children(pc_fit, "Y"))

# OR and IPW with PC set
pc_frm_or <- as.formula(paste("Y ~ A +", paste(L_pc, collapse = " + ")))
fit_or_pc <- glm(pc_frm_or, data = df_raw, family = binomial)

d1_pc <- df_raw; d1_pc$A <- 1
d0_pc <- df_raw; d0_pc$A <- 0

ey1_or_pc <- mean(predict(fit_or_pc, newdata = d1_pc, type = "response"))
ey0_or_pc <- mean(predict(fit_or_pc, newdata = d0_pc, type = "response"))
ace_or_pc <- ey1_or_pc - ey0_or_pc

cat(sprintf("\nOR (PC): E[Y^1]=%.4f, E[Y^0]=%.4f, ACE=%.4f\n",
            ey1_or_pc, ey0_or_pc, ace_or_pc))

pc_frm_ps <- as.formula(paste("A ~", paste(L_pc, collapse = " + ")))
fit_ps_pc <- glm(pc_frm_ps, data = df_raw, family = binomial)
ps_pc <- predict(fit_ps_pc, type = "response")
ps_pc_t <- pmin(pmax(ps_pc, PS_TRIM_LO), PS_TRIM_HI)

cat("PS range (PC):", round(min(ps_pc), 4), "-", round(max(ps_pc), 4), "\n")

w1_pc <- (df_raw$A) / ps_pc_t
w0_pc <- (1 - df_raw$A) / (1 - ps_pc_t)

ey1_ipw_pc <- sum(w1_pc * df_raw$Y) / sum(w1_pc)
ey0_ipw_pc <- sum(w0_pc * df_raw$Y) / sum(w0_pc)
ace_ipw_pc <- ey1_ipw_pc - ey0_ipw_pc

cat(sprintf("IPW (PC): E[Y^1]=%.4f, E[Y^0]=%.4f, ACE=%.4f\n",
            ey1_ipw_pc, ey0_ipw_pc, ace_ipw_pc))


#Local Causal Discovery (HITON-PC)

cat("\n Local Causal Discovery (HITON-PC)\n")

set.seed(516)
local_fit <- si.hiton.pc(df_bn_num, test = "cor", alpha = 0.05)

cat("\nSI-HITON-PC result:\n")
print(local_fit)

mb_Y_local <- mb(local_fit, "Y")
cat("\nMarkov blanket of Y from SI-HITON-PC:\n")
print(mb_Y_local)

L_local <- setdiff(mb_Y_local, c("Y", "A"))
cat("\nLocal adjustment set:", paste(L_local, collapse = ", "), "\n")
cat("Size:", length(L_local), "\n")
cat("A is in MB(Y):", "A" %in% mb_Y_local, "\n")

# Save graph
local_igraph <- as.igraph(local_fit)

png("p_local_graph.png", width = 900, height = 700)
set.seed(516)
plot(local_igraph,
     vertex.size = 18,
     vertex.label.cex = 0.7,
     vertex.color = ifelse(V(local_igraph)$name == "Y", "firebrick",
                           ifelse(V(local_igraph)$name == "A", "steelblue",
                                  ifelse(V(local_igraph)$name %in% mb_Y_local, "gold", "grey80"))),
     vertex.label.color = "black",
     edge.arrow.size = 0.4,
     main = "SI-HITON-PC learned graph (cor test)")
legend("bottomright",
       legend = c("Outcome (Y)", "Treatment (A)", "MB(Y)", "Other"),
       fill = c("firebrick", "steelblue", "gold", "grey80"),
       cex = 0.8)
dev.off()

set.seed(516)
plot(local_igraph,
     vertex.size = 18,
     vertex.label.cex = 0.7,
     vertex.color = ifelse(V(local_igraph)$name == "Y", "firebrick",
                           ifelse(V(local_igraph)$name == "A", "steelblue",
                                  ifelse(V(local_igraph)$name %in% mb_Y_local, "gold", "grey80"))),
     vertex.label.color = "black",
     edge.arrow.size = 0.4,
     main = "SI-HITON-PC learned graph (cor test)")
legend("bottomright",
       legend = c("Outcome (Y)", "Treatment (A)", "MB(Y)", "Other"),
       fill = c("firebrick", "steelblue", "gold", "grey80"),
       cex = 0.8)

# OR and IPW with local set
if (length(L_local) > 0) {
  loc_frm_or <- as.formula(paste("Y ~ A +", paste(L_local, collapse = " + ")))
  fit_or_local <- glm(loc_frm_or, data = df_raw, family = binomial)
  
  d1_loc <- df_raw; d1_loc$A <- 1
  d0_loc <- df_raw; d0_loc$A <- 0
  
  ey1_or_local <- mean(predict(fit_or_local, newdata = d1_loc, type = "response"))
  ey0_or_local <- mean(predict(fit_or_local, newdata = d0_loc, type = "response"))
  ace_or_local <- ey1_or_local - ey0_or_local
  
  cat(sprintf("\nOR (Local): E[Y^1]=%.4f, E[Y^0]=%.4f, ACE=%.4f\n",
              ey1_or_local, ey0_or_local, ace_or_local))
  
  loc_frm_ps <- as.formula(paste("A ~", paste(L_local, collapse = " + ")))
  fit_ps_local <- glm(loc_frm_ps, data = df_raw, family = binomial)
  ps_local <- predict(fit_ps_local, type = "response")
  ps_local_t <- pmin(pmax(ps_local, PS_TRIM_LO), PS_TRIM_HI)
  
  cat("PS range (Local):", round(min(ps_local), 4), "-", round(max(ps_local), 4), "\n")
  
  w1_local <- (df_raw$A) / ps_local_t
  w0_local <- (1 - df_raw$A) / (1 - ps_local_t)
  
  ey1_ipw_local <- sum(w1_local * df_raw$Y) / sum(w1_local)
  ey0_ipw_local <- sum(w0_local * df_raw$Y) / sum(w0_local)
  ace_ipw_local <- ey1_ipw_local - ey0_ipw_local
  
  cat(sprintf("IPW (Local): E[Y^1]=%.4f, E[Y^0]=%.4f, ACE=%.4f\n",
              ey1_ipw_local, ey0_ipw_local, ace_ipw_local))
} else {
  cat("Local blanket of Y is empty, falling back to all covariates\n")
  ace_or_local <- ace_or_all
  ace_ipw_local <- ace_ipw_all
  L_local <- covar_names
}


# Bootstrap Confidence Intervals

cat("\nBootstrap Confidence Intervals\n")

boot_ace <- function(data, indices, covar_set, method) {
  d <- data[indices, ]
  
  if (method == "OR") {
    frm <- as.formula(paste("Y ~ A +", paste(covar_set, collapse = " + ")))
    fit <- tryCatch(glm(frm, data = d, family = binomial), error = function(e) NULL)
    if (is.null(fit)) return(NA)
    d1 <- d; d1$A <- 1
    d0 <- d; d0$A <- 0
    ey1 <- mean(predict(fit, newdata = d1, type = "response"))
    ey0 <- mean(predict(fit, newdata = d0, type = "response"))
    return(ey1 - ey0)
  }
  
  if (method == "IPW") {
    frm_ps <- as.formula(paste("A ~", paste(covar_set, collapse = " + ")))
    fit_ps <- tryCatch(glm(frm_ps, data = d, family = binomial), error = function(e) NULL)
    if (is.null(fit_ps)) return(NA)
    ps <- predict(fit_ps, type = "response")
    ps <- pmin(pmax(ps, 0.01), 0.99)
    w1 <- (d$A) / ps
    w0 <- (1 - d$A) / (1 - ps)
    ey1 <- sum(w1 * d$Y) / sum(w1)
    ey0 <- sum(w0 * d$Y) / sum(w0)
    return(ey1 - ey0)
  }
}

N_BOOT <- 1000

run_bootstrap <- function(data, covars, method, R = N_BOOT) {
  set.seed(516)
  b <- boot(data, statistic = boot_ace, R = R,
            covar_set = covars, method = method)
  ci <- boot.ci(b, type = "perc", conf = 0.95)
  tibble(
    ACE    = round(b$t0, 4),
    SE     = round(sd(b$t, na.rm = TRUE), 4),
    CI_lo  = round(ci$percent[4], 4),
    CI_hi  = round(ci$percent[5], 4)
  )
}

# Covariate sets for each adjustment strategy
sets <- list(
  All   = covar_names,
  LASSO = lasso_vars,
  PC    = L_pc,
  Local = L_local
)

cat("Running", N_BOOT, "bootstrap replicates per combination\n")

results <- list()
for (adj in names(sets)) {
  for (meth in c("OR", "IPW")) {
    cat(sprintf("  %s x %s ...\n", adj, meth))
    res <- run_bootstrap(df_raw, sets[[adj]], meth)
    results[[paste(adj, meth, sep = "_")]] <- res %>%
      mutate(Adjustment = adj, Method = meth, .before = 1)
  }
}

ace_table <- bind_rows(results)
cat("\nFinal ACE comparison table:\n")
print(ace_table, n = 20)

cat(sprintf("\nNaive (unadjusted) risk difference: %.4f\n", naive_diff))



# Summary Plots and Diagnostics


# PS overlap across all four adjustment sets
ps_compare <- bind_rows(
  tibble(ps = ps_all, A = factor(df_raw$A), Set = "All"),
  tibble(ps = if (exists("ps_lasso")) ps_lasso else ps_all,
         A = factor(df_raw$A), Set = "LASSO"),
  tibble(ps = if (exists("ps_pc")) ps_pc else ps_all,
         A = factor(df_raw$A), Set = "PC"),
  tibble(ps = if (exists("ps_local")) ps_local else ps_all,
         A = factor(df_raw$A), Set = "Local")
)

p_ps_compare <- ggplot(ps_compare, aes(x = ps, fill = A)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Set) +
  labs(x = "Propensity Score P(A=1|L)", y = "Density", fill = "Treatment") +
  scale_fill_manual(values = c("0" = "grey60", "1" = "steelblue"))

ggsave("p_ps_compare.png", p_ps_compare, width = 9, height = 6)
print(p_ps_compare)

# ACE comparison plot with CIs
p_ace <- ggplot(ace_table, aes(x = Adjustment, y = ACE, color = Method)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi),
                width = 0.15, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = naive_diff, linetype = "dotted", color = "firebrick",
             linewidth = 0.7) +
  annotate("text", x = 0.6, y = naive_diff + 0.008,
           label = "Naive", color = "firebrick", size = 3.5, hjust = 0) +
  labs(x = "Adjustment Set", y = "ACE (risk difference)",
       color = "Estimator") +
  scale_color_manual(values = c("OR" = "steelblue", "IPW" = "darkorange"))

ggsave("p_ace_compare.png", p_ace, width = 8, height = 5)
print(p_ace)

# Set comparison
cat("\nAdjustment set comparison:\n")
cat("All:", length(covar_names), "variables\n")
cat("LASSO:", length(lasso_vars), "variables:",
    paste(lasso_vars, collapse = ", "), "\n")
cat("PC:", length(L_pc), "variables:",
    paste(L_pc, collapse = ", "), "\n")
cat("Local:", length(L_local), "variables:",
    paste(L_local, collapse = ", "), "\n")

# Overlap between discovered sets
cat("\nIn PC but not LASSO:",
    paste(setdiff(L_pc, lasso_vars), collapse = ", "), "\n")
cat("In LASSO but not PC:",
    paste(setdiff(lasso_vars, L_pc), collapse = ", "), "\n")
cat("In both PC and Local:",
    paste(intersect(L_pc, L_local), collapse = ", "), "\n")
cat("In PC but not Local:",
    paste(setdiff(L_pc, L_local), collapse = ", "), "\n")
cat("In Local but not PC:",
    paste(setdiff(L_local, L_pc), collapse = ", "), "\n")

