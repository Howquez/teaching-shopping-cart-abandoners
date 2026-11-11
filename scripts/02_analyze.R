# This script answers two questions: Did the cart reminder email work, and did
# it pay off? It proceeds in four steps:

#   1. Vendor view: conversion rates as typically reported
#   2. Causal effect: treatment vs. randomized holdout (intention to treat)
#   3. Regression adjustment for precision
#   4. Economics: incremental revenue, profit and ROI vs. vendor claims

# This script only computes results. Figures and tables are produced by
# 04_figures_tables.R, and the report reads the same saved results.

# Input:  data/processed/analysis_data.csv, data/processed/sessions_clean.csv
# Output: outputs/results/analysis_results.rds

# Packages -----
# As before, groundhog loads the package versions that were available on the given date,
# so that everyone runs the code with exactly the same versions.
if (!requireNamespace("groundhog", quietly = TRUE)) {
  install.packages("groundhog")
}
pkgs <- c("here", # makes it easier to use relative file paths (i.e., relative to the root, where .Rproj is located)
          "data.table") # an alternative to base R and tidyverse which I like
groundhog::groundhog.library(pkg = pkgs, date = "2026-06-30") # R-4.6.1
rm(pkgs)


# Prep -----
dir.create(path = here("outputs", "results"), recursive = TRUE, showWarnings = FALSE)

# Business assumptions (change these and discuss how conclusions change)
assumptions <- list(gross_margin = 0.40,     # share of net revenue left after cost of goods
                    cost_per_email = 0.002,  # EUR, variable sending cost
                    fixed_cost_period = 600) # EUR, tool license and setup for Sep to Oct


# Read data -----
ad   <- fread(input = here("data", "processed", "analysis_data.csv"))
sess <- fread(input = here("data", "processed", "sessions_clean.csv"))

# Analysis sample: enrollments with a complete 7-day outcome window
a <- ad[full_window == TRUE]
a[, group := factor(x = group, levels = c("holdout", "treatment"))]
a[, .N, by = group]


# 1. Vendor view -----
# These rates compare self-selected groups (clickers, openers) or the wrong
# population (all website sessions). None of them is a causal effect.
tg <- a[group == "treatment"]
vendor_view <- data.table(
  group = c("Clicked the email", "Opened the email", "All email recipients",
            "Holdout (no email)", "Website sessions (per session)"),
  conversion_rate = c(tg[clicked == 1, mean(ordered_7d)],
                      tg[opened == 1, mean(ordered_7d)],
                      tg[, mean(ordered_7d)],
                      a[group == "holdout", mean(ordered_7d)],
                      sess[!is.na(customer_id), mean(purchased)]),
  n = c(tg[clicked == 1, .N],
        tg[opened == 1, .N],
        tg[, .N],
        a[group == "holdout", .N],
        sess[!is.na(customer_id), .N])
)
vendor_view


# 2. Causal effect: treatment vs. holdout (intention to treat) -----
# Everyone assigned to treatment counts, including bounced emails and
# non-openers, because only the assignment was randomized.
by_group <- a[, .(n = .N,
                  orders = sum(ordered_7d),
                  order_rate = mean(ordered_7d),
                  revenue_net_per_customer = mean(revenue_net_7d)),
              by = group]
setorder(by_group, group)
by_group

# Compare the two order rates with a test for two proportions
n_t      <- a[group == "treatment", .N]
n_c      <- a[group == "holdout", .N]
orders_t <- a[group == "treatment", sum(ordered_7d)]
orders_c <- a[group == "holdout", sum(ordered_7d)]
pt <- prop.test(x = c(orders_t, orders_c), n = c(n_t, n_c), correct = FALSE)
pt

rate_t <- orders_t / n_t
rate_c <- orders_c / n_c
itt <- data.table(outcome        = "Ordered within 7 days",
                  holdout_rate   = rate_c,
                  treatment_rate = rate_t,
                  diff_pp        = 100 * (rate_t - rate_c),
                  ci_low_pp      = 100 * pt$conf.int[1],
                  ci_high_pp     = 100 * pt$conf.int[2],
                  relative_lift  = rate_t / rate_c - 1,
                  p_value        = pt$p.value)
itt


# 3. Regression adjustment (linear probability model) -----
# Randomization makes covariates unnecessary for unbiasedness, but adjusting
# for strong predictors of the outcome can shrink the standard error.
m0 <- lm(formula = ordered_7d ~ treated, data = a)
m1 <- lm(formula = ordered_7d ~ treated + log(abandoned_cart_value) + device +
           new_customer + loyalty_tier,
         data = a)
summary(object = m0)
summary(object = m1)

# Collect the treatment effect from both models in one table
coef_m0 <- summary(object = m0)$coefficients["treated", ]
coef_m1 <- summary(object = m1)$coefficients["treated", ]
ci_m0   <- confint(object = m0, parm = "treated")
ci_m1   <- confint(object = m1, parm = "treated")

regression <- data.table(model       = c("unadjusted", "adjusted"),
                         estimate_pp = 100 * c(coef_m0["Estimate"], coef_m1["Estimate"]),
                         se_pp       = 100 * c(coef_m0["Std. Error"], coef_m1["Std. Error"]),
                         ci_low_pp   = 100 * c(ci_m0[1], ci_m1[1]),
                         ci_high_pp  = 100 * c(ci_m0[2], ci_m1[2]),
                         p_value     = c(coef_m0["Pr(>|t|)"], coef_m1["Pr(>|t|)"]))
regression


# 4. Economics -----
# Difference in net revenue per customer between treatment and holdout,
# tested with a Welch t-test (allows for unequal variances)
rev_t <- a[group == "treatment", revenue_net_7d]
rev_c <- a[group == "holdout", revenue_net_7d]
tt <- t.test(x = rev_t, y = rev_c)
tt

# Campaign cost
emails_sent <- a[group == "treatment" & !is.na(email_sent_at), .N]
cost_total  <- emails_sent * assumptions$cost_per_email + assumptions$fixed_cost_period

# Incremental revenue and profit (estimate and 95% confidence interval)
incr_rev_per_cust <- c(estimate = unname(obj = tt$estimate[1] - tt$estimate[2]),
                       ci_low   = tt$conf.int[1],
                       ci_high  = tt$conf.int[2])
incr_rev    <- incr_rev_per_cust * n_t
incr_profit <- incr_rev * assumptions$gross_margin - cost_total

# What the vendor claims for the same customers
vendor_rev <- tg[, sum(attributed_revenue_eur, na.rm = TRUE)]

economics <- data.table(
  quantity = c("Incremental net revenue per treated customer (EUR)",
               "Incremental net revenue, all treated (EUR)",
               "Incremental gross profit after campaign cost (EUR)",
               "ROI (incremental profit / campaign cost)",
               "Vendor-attributed revenue (EUR)",
               "Vendor-attributed revenue / incremental revenue"),
  estimate = c(incr_rev_per_cust["estimate"], 
               incr_rev["estimate"],
               incr_profit["estimate"], 
               incr_profit["estimate"] / cost_total,
               vendor_rev, vendor_rev / incr_rev["estimate"]),
  ci_low   = c(incr_rev_per_cust["ci_low"], 
               incr_rev["ci_low"], 
               incr_profit["ci_low"],
               incr_profit["ci_low"] / cost_total, 
               NA, 
               NA),
  ci_high  = c(incr_rev_per_cust["ci_high"], 
               incr_rev["ci_high"], 
               incr_profit["ci_high"],
               incr_profit["ci_high"] / cost_total, 
               NA, 
               NA))
economics


# Save results -----
# One list with everything the figures, tables and the report need
results <- list(
  assumptions = assumptions,
  cohort      = list(n_enrolled        = ad[, .N],
                     n_analysis        = a[, .N],
                     n_treatment       = n_t,
                     n_holdout         = n_c,
                     n_excluded_window = ad[full_window == FALSE, .N],
                     emails_sent       = emails_sent,
                     cost_total        = cost_total),
  vendor_view = vendor_view,
  by_group    = by_group,
  itt         = itt,
  regression  = regression,
  economics   = economics,
  incremental = list(rev_per_cust = incr_rev_per_cust,
                     rev          = incr_rev,
                     profit       = incr_profit,
                     vendor_rev   = vendor_rev)
)
saveRDS(object = results, file = here("outputs", "results", "analysis_results.rds"))


# Document R and package versions -----
sessionInfo()