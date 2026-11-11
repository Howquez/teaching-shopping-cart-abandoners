# This script simulates raw, deliberately messy data for a cart abandonment
# email campaign run by a fictional online shop (Sep to Oct 2026).
# INSTRUCTOR ONLY: do not share this script or instructor_only/ with students,
# because both reveal the true effect.
#
# Business story: Customers who leave items in their cart receive a "You forgot
# something" email. A random 10% of abandoners is kept as a holdout (no email).
# The email vendor reports spectacular conversion rates. The true causal effect
# of the email is small, and the holdout is too small to detect it reliably.
#
# Output: data/raw/customers.csv, web_sessions.csv, orders.csv, campaign_log.csv,
#         vendor_dashboard_export.csv (see codebook.md)
#         instructor_only/answer_key.txt (true parameters and the clean ITT estimate)
#
# Note: the simulation code below is deliberately left unchanged, because any
# change (even reordering two lines) changes the random draws and thus the data.

# Packages -----
# groundhog loads the package versions that were available on the given date,
# so that everyone runs the code with exactly the same versions.
if (!requireNamespace("groundhog", quietly = TRUE)) {
  install.packages("groundhog")
}
pkgs <- c("here", # makes it easier to use relative file paths (i.e., relative to the root, where .Rproj is located)
          "data.table") # an alternative to base R and tidyverse which I like
groundhog::groundhog.library(pkg = pkgs, date = "2026-06-30") # R-4.6.1
rm(pkgs)


# Prep -----
seed <- 2   # other seeds give other realizations of the same true effect
set.seed(seed)

# ---- Parameters ---------------------------------------------------------------

params <- list(
  n_customers         = 60000,
  period_start        = as.POSIXct("2026-09-01 00:00:00", tz = "UTC"),
  period_days         = 61,       # Sep 1 to Oct 31
  export_lag_days     = 3,        # data exported on Nov 4, 00:00 UTC
  holdout_share       = 0.10,     # share of enrolled abandoners without email
  recovery_intercept  = -1.75,    # logit, sets baseline 7-day recovery rate
  email_effect_logit  = 0.16,     # TRUE causal effect of the email (logit)
  outcome_window_days = 7,
  vendor_window_days  = 30        # vendor attributes any purchase within 30d
)

raw_dir <- here("data", "raw")
key_dir <- here("instructor_only")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(key_dir, recursive = TRUE, showWarnings = FALSE)

period_end <- params$period_start + params$period_days * 86400
export_ts  <- period_end + params$export_lag_days * 86400
na_time    <- as.POSIXct(NA_real_, tz = "UTC")

# Helpers ------------------------------------------------------------------------

mess_up <- function(x, share, replacement_fun) {
  # Replace a random share of x with values produced by replacement_fun()
  idx <- which(runif(length(x)) < share)
  if (length(idx) > 0) x[idx] <- replacement_fun(x[idx])
  x
}

german_number <- function(x) {
  # 1234.5 -> "1.234,50"
  formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
}

# ---- 1. Customers -------------------------------------------------------------

n <- params$n_customers

cust <- data.table(
  cust_idx    = seq_len(n),
  customer_id = sprintf("C%06d", sample(100000:999999, n)),
  returning   = rbinom(n, 1, 0.45)
)
cust[, intent := rnorm(.N) + 0.5 * returning]          # latent, never exported
cust[, signup_date := as.IDate(params$period_start) -
       fifelse(returning == 1, round(runif(.N, 60, 1800)), round(runif(.N, 0, 59)))]
cust[, country := sample(c("DE", "AT", "CH", "NL"), .N, TRUE, c(.70, .15, .10, .05))]
cust[, age := pmin(pmax(round(rnorm(.N, 38, 12)), 16), 85)]
cust[, gender := sample(c("f", "m", "d", ""), .N, TRUE, c(.52, .44, .01, .03))]
cust[, newsletter := fifelse(rbinom(.N, 1, 0.25 + 0.15 * returning) == 1, "Y", "N")]
cust[, loyalty_tier := fifelse(returning == 0, "none",
                               sample(c("none", "silver", "gold"), .N, TRUE, c(.5, .35, .15)))]
cust[, has_email := rbinom(.N, 1, fifelse(returning == 1, 0.90, 0.65))]

# ---- 2. Web sessions ----------------------------------------------------------

n_sess <- 1 + rpois(n, 1.2 + 1.2 * cust$returning)
sess <- cust[rep(cust_idx, n_sess), .(cust_idx, intent)]
ns <- nrow(sess)

sess[, `:=`(
  session_id = sprintf("S%08d", sample(1e7:9e7, .N)),
  start_ts   = params$period_start + runif(.N, 0, params$period_days * 86400),
  device     = sample(c("mobile", "desktop", "tablet"), .N, TRUE, c(.60, .33, .07)),
  source     = sample(c("organic", "paid_search", "social", "direct", "newsletter",
                        "affiliate"), .N, TRUE, c(.30, .25, .15, .18, .07, .05)),
  browser    = sample(c("Chrome", "Safari", "Firefox", "Edge", "Samsung Internet"),
                      .N, TRUE, c(.45, .32, .08, .08, .07)),
  pages_viewed = 1L + rnbinom(.N, mu = 5, size = 1.5)
)]
sess[, duration_sec := round(rlnorm(.N, log(40 * pages_viewed), 0.5))]
sess[, page_load_ms := round(rlnorm(.N, log(1800), 0.4))]
sess[, added_to_cart := rbinom(.N, 1, plogis(-3.3 + 0.10 * pmin(pages_viewed, 20) +
                                               0.5 * intent + 0.2 * (device == "desktop")))]
sess[, cart_value := fifelse(added_to_cart == 1, round(rlnorm(.N, log(65), 0.6), 2), NA_real_)]
sess[, purchased := fifelse(
  added_to_cart == 1,
  rbinom(.N, 1, plogis(-1.9 + 0.7 * intent + 0.4 * (device == "desktop") -
                         0.004 * (fcoalesce(cart_value, 65) - 65))),
  0L)]
sess[, checkout_started := fifelse(purchased == 1, 1L,
                                   fifelse(added_to_cart == 1, rbinom(.N, 1, 0.3), 0L))]
setorder(sess, cust_idx, start_ts)

# ---- 3. Campaign enrollment (first abandonment per customer) ------------------

ab <- unique(sess[added_to_cart == 1 & purchased == 0], by = "cust_idx")
ab <- ab[cust[ab$cust_idx, has_email] == 1]
ab[cust, on = "cust_idx", returning := i.returning]
na_ab <- nrow(ab)

ab[, holdout := rbinom(.N, 1, params$holdout_share)]
ab[, delivered := fifelse(holdout == 0, rbinom(.N, 1, 0.96), 0L)]    # 4% bounce
ab[, sent_ts := na_time]
ab[holdout == 0, sent_ts := start_ts + 3600 + runif(.N, 0, 1800)]

# Reset all later in-session purchases of enrolled customers; recovery purchases
# are simulated explicitly below so that the true effect is known.
sess[ab, on = "cust_idx", ab_ts := i.start_ts]
sess[!is.na(ab_ts) & start_ts > ab_ts, `:=`(
  purchased = 0L,
  checkout_started = fifelse(added_to_cart == 1, rbinom(.N, 1, 0.3), 0L)
)]
sess[, ab_ts := NULL]

ab[, lin := params$recovery_intercept + 0.8 * intent + 0.2 * returning -
     0.004 * (cart_value - 65)]
ab[, p0 := plogis(lin)]                                              # no email
ab[, p1 := plogis(lin + params$email_effect_logit * delivered)]      # actual exposure

ab[, buys := rbinom(.N, 1, p1)]                     # purchase within 7 days
ab[, buy_ts := na_time]
ab[buys == 1, buy_ts := start_ts + runif(.N, 0.05, params$outcome_window_days) * 86400]
ab[, late_ts := na_time]                            # later purchases, no email effect
ab[buys == 0 & runif(.N) < 0.10, late_ts := start_ts + runif(.N, 7, 40) * 86400]

# Email engagement (opens inflated by Apple Mail Privacy Protection)
ab[, opened := fifelse(delivered == 1, rbinom(.N, 1, plogis(0.2 + 0.3 * intent)), NA_integer_)]
ab[, clicked := fifelse(holdout == 1, NA_integer_,
                        fifelse(!is.na(opened) & opened == 1,
                                rbinom(.N, 1, plogis(-3.0 + 2.4 * buys + 0.4 * intent)), 0L))]
ab[, unsubscribed := fifelse(delivered == 1, rbinom(.N, 1, 0.006), NA_integer_)]
ab[, template_version := fifelse(holdout == 0,
                                 sample(c("v3_subject_emoji", "v3_subject_plain"), .N, TRUE),
                                 NA_character_)]

# Recovery and late purchases become new web sessions
mk_purchase_sessions <- function(d, ts_col, via_email) {
  d[, .(
    cust_idx, intent,
    session_id       = sprintf("S%08d", sample(9e7:99999999, .N)),
    start_ts         = get(ts_col),
    device,
    source           = fifelse(rep_len(via_email, .N), "email_cart_reminder",
                               sample(c("direct", "organic", "paid_search"), .N, TRUE)),
    browser,
    pages_viewed     = 2L + rpois(.N, 3),
    duration_sec     = round(rlnorm(.N, log(240), 0.5)),
    page_load_ms     = round(rlnorm(.N, log(1800), 0.4)),
    added_to_cart    = 1L,
    cart_value       = round(cart_value * runif(.N, 0.85, 1.25), 2),
    purchased        = 1L,
    checkout_started = 1L
  )]
}
buyers <- ab[buys == 1]
sess <- rbind(
  sess,
  mk_purchase_sessions(buyers, "buy_ts", !is.na(buyers$clicked) & buyers$clicked == 1),
  mk_purchase_sessions(ab[!is.na(late_ts)], "late_ts", FALSE),
  use.names = TRUE
)

# Vendor attribution: any purchase within 30 days after an open counts
ab[, first_purchase_ts := fcoalesce(buy_ts, late_ts)]
ab[, vendor_conv := fifelse(
  !is.na(opened) & opened == 1 & !is.na(first_purchase_ts) &
    as.numeric(first_purchase_ts - sent_ts, units = "days") <= params$vendor_window_days,
  1L, 0L)]
ab[holdout == 1, vendor_conv := NA_integer_]

# ---- 4. Answer key quantities (before censoring and messing) ------------------

ab[, full_window := start_ts + params$outcome_window_days * 86400 <= export_ts]
kd <- ab[full_window == TRUE]
itt_t <- kd[holdout == 0, mean(buys)]
itt_c <- kd[holdout == 1, mean(buys)]
n_t <- kd[holdout == 0, .N]; n_c <- kd[holdout == 1, .N]
true_ate <- kd[holdout == 0, mean(p1 - p0)]
pc <- kd[, mean(p0)]; pt <- pc + true_ate
se <- sqrt(pc * (1 - pc) / n_c + pt * (1 - pt) / n_t)
power_design <- pnorm(true_ate / se - qnorm(0.975)) + pnorm(-true_ate / se - qnorm(0.975))

# ---- 5. Orders ---------------------------------------------------------------

orders <- sess[purchased == 1, .(
  order_id    = sprintf("ORD-%07d", sample(1e6:9e6, .N)),
  cust_idx,
  session_id,
  order_ts    = start_ts + runif(.N, 60, 900),
  order_value = round(cart_value * runif(.N, 0.9, 1.05) + fifelse(cart_value < 50, 4.95, 0), 2),
  status      = sample(c("completed", "cancelled", "returned"), .N, TRUE, c(.89, .04, .07)),
  payment_method = sample(c("PayPal", "Klarna", "Credit card", "Invoice", "Apple Pay"),
                          .N, TRUE, c(.35, .25, .20, .10, .10)),
  discount_code  = fifelse(runif(.N) < 0.12,
                           sample(c("WELCOME10", "AUTUMN15", "NL5"), .N, TRUE), "")
)]
orders[cust, on = "cust_idx", shipping_country := i.country]

# True economic quantities for the answer key (completed orders, 7-day window)
kd_rev <- merge(kd[, .(cust_idx, holdout, start_ts)],
                orders[status == "completed", .(cust_idx, order_ts, order_value)],
                by = "cust_idx", all.x = TRUE)
kd_rev <- kd_rev[, .(rev = sum(order_value[!is.na(order_ts) & order_ts > start_ts &
                                             order_ts <= start_ts + 7 * 86400])),
                 by = .(cust_idx, holdout)]
rev_t <- kd_rev[holdout == 0, mean(rev)]; rev_c <- kd_rev[holdout == 1, mean(rev)]

# ---- 6. Censoring at export --------------------------------------------------

sess   <- sess[start_ts <= export_ts]
orders <- orders[order_ts <= export_ts]

# ---- 7. Test accounts and bots -----------------------------------------------

n_test <- 30
test_cust <- data.table(
  cust_idx = (n + 1):(n + n_test), customer_id = sprintf("TEST_%03d", seq_len(n_test)),
  returning = 0L, intent = 0, signup_date = as.IDate("2026-08-15"), country = "DE",
  age = 99, gender = "", newsletter = "N", loyalty_tier = "none", has_email = 1L
)
cust <- rbind(cust, test_cust)

orders <- rbind(orders, data.table(
  order_id = sprintf("ORD-TEST-%03d", 1:120),
  cust_idx = sample(test_cust$cust_idx, 120, TRUE),
  session_id = NA_character_,
  order_ts = params$period_start + runif(120, 0, params$period_days * 86400),
  order_value = sample(c(0.01, 1, 999.99), 120, TRUE),
  status = "completed", payment_method = "Invoice", discount_code = "",
  shipping_country = "DE"
))

test_enroll <- data.table(
  cust_idx = sample(test_cust$cust_idx, 40, TRUE),
  session_id = sprintf("S%08d", sample(1e7:9e7, 40)),
  start_ts = params$period_start + runif(40, 0, 10 * 86400),
  holdout = 0L, delivered = 1L, opened = 1L, clicked = 1L, buys = 1L,
  unsubscribed = 0L, template_version = "v3_subject_plain",
  vendor_conv = 1L, cart_value = 999.99
)
test_enroll[, sent_ts := start_ts + 3600]

n_bots <- round(0.02 * nrow(sess))
bots <- data.table(
  cust_idx = NA_integer_, intent = NA_real_,
  session_id = sprintf("B%08d", sample(1e7:9e7, n_bots)),
  start_ts = params$period_start + runif(n_bots, 0, params$period_days * 86400),
  device = "desktop", source = "direct",
  browser = sample(c("HeadlessChrome", "Chrome", "python-requests"), n_bots, TRUE),
  pages_viewed = sample(80:400, n_bots, TRUE),
  duration_sec = as.numeric(sample(1:20, n_bots, TRUE)),
  page_load_ms = round(rlnorm(n_bots, log(300), 0.3)),
  added_to_cart = rbinom(n_bots, 1, 0.4), purchased = 0L, checkout_started = 0L
)
bots[, cart_value := fifelse(added_to_cart == 1, round(runif(.N, 5, 300), 2), NA_real_)]
sess <- rbind(sess, bots, use.names = TRUE)

# ---- 8. Make it messy and write files ----------------------------------------

## 8a. customers.csv (CRM export) ---------------------------------------------
cust_out <- cust[, .(
  `Customer ID`     = customer_id,
  signup_date       = format(signup_date, "%Y-%m-%d"),
  country, age, gender,
  newsletter_opt_in = newsletter,
  loyalty_tier,
  email_known       = has_email
)]
cust_out[, country := mess_up(country, 0.08, function(x)
  sample(c("Germany", "germany", "Deutschland", " DE", "de", "D", "Austria",
           "Österreich", "Switzerland", "CH ", "Netherlands", ""), length(x), TRUE))]
cust_out[, age := mess_up(age, 0.02, function(x)
  sample(c(NA, 0, 1, 199, 1987, -5), length(x), TRUE))]
cust_out[, signup_date := mess_up(signup_date, 0.05, function(x)
  format(as.Date(x), "%d.%m.%Y"))]
cust_out[, newsletter_opt_in := mess_up(newsletter_opt_in, 0.05, function(x)
  fifelse(x == "Y", sample(c("yes", "1", "TRUE"), length(x), TRUE),
          sample(c("no", "0", "FALSE", ""), length(x), TRUE)))]
cust_out <- rbind(cust_out, cust_out[sample(.N, 150)])      # CRM merge duplicates
cust_out <- cust_out[sample(.N)]
fwrite(cust_out, file.path(raw_dir, "customers.csv"))

## 8b. web_sessions.csv (web analytics export) ---------------------------------
sess_out <- sess[, .(
  `Session ID`         = session_id,
  customerId           = fifelse(is.na(cust_idx), "", cust$customer_id[cust_idx]),
  `Start Time (UTC)`   = format(start_ts, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  Device               = device,
  traffic_source       = source,
  browser,
  pages_viewed,
  session_duration_sec = duration_sec,
  page_load_time_ms    = page_load_ms,
  added_to_cart        = as.character(added_to_cart),
  cart_value_eur       = fifelse(is.na(cart_value), "", sprintf("%.2f", cart_value)),
  checkout_started,
  purchase_flag        = purchased
)]
# Anonymous (not logged-in) sessions
sess_out[, customerId := mess_up(customerId, 0.12, function(x) "")]
# Tracking update on Oct 15: half of the later sessions use Unix timestamps
oct <- sess$start_ts >= as.POSIXct("2026-10-15", tz = "UTC") & runif(nrow(sess)) < 0.5
sess_out[oct, `Start Time (UTC)` := as.character(as.integer(sess$start_ts[oct]))]
sess_out[, Device := mess_up(Device, 0.10, function(x)
  fifelse(x == "mobile", sample(c("Mobile", "MOBILE", "smartphone", "iOS", "Android"),
                                length(x), TRUE),
          fifelse(x == "desktop", sample(c("Desktop", "PC", "desktop "), length(x), TRUE),
                  sample(c("Tablet", "iPad"), length(x), TRUE))))]
sess_out[, added_to_cart := mess_up(added_to_cart, 0.06, function(x)
  fifelse(x == "1", sample(c("TRUE", "yes", "Y"), length(x), TRUE),
          sample(c("FALSE", "no", ""), length(x), TRUE)))]
sess_out[, cart_value_eur := mess_up(cart_value_eur, 0.05, function(x)
  fifelse(x == "", x, sub(".", ",", x, fixed = TRUE)))]
sess_out[, cart_value_eur := mess_up(cart_value_eur, 0.01, function(x)
  fifelse(x == "", x, paste0("€", x)))]
sess_out <- rbind(sess_out, sess_out[sample(.N, round(0.01 * .N))])   # duplicates
sess_out <- sess_out[sample(.N)]
fwrite(sess_out, file.path(raw_dir, "web_sessions.csv"))

## 8c. orders.csv (German ERP export, local time) ------------------------------
ord_out <- orders[, .(
  Bestellnummer   = order_id,
  Kundennummer    = cust$customer_id[cust_idx],
  session_ref     = fcoalesce(session_id, ""),
  Bestelldatum    = format(order_ts, "%d.%m.%Y %H:%M", tz = "Europe/Berlin"),
  Bestellwert_EUR = german_number(order_value),
  Status          = status,
  Zahlungsart     = payment_method,
  Gutscheincode   = discount_code,
  Lieferland      = shipping_country
)]
ord_out[, session_ref := mess_up(session_ref, 0.15, function(x) "")]
ord_out[, Status := mess_up(Status, 0.08, function(x)
  fifelse(x == "completed", sample(c("Completed", "abgeschlossen", "COMPLETED"), length(x), TRUE),
          fifelse(x == "returned", sample(c("Retoure", "returned "), length(x), TRUE),
                  "storniert")))]
ord_out <- rbind(ord_out, ord_out[sample(.N, round(0.015 * .N))])     # ERP sync ran twice
ord_out <- ord_out[sample(.N)]
fwrite(ord_out, file.path(raw_dir, "orders.csv"), sep = ";", quote = TRUE)

## 8d. campaign_log.csv (email tool export) -----------------------------------
keep <- c("cust_idx", "session_id", "start_ts", "holdout", "delivered", "sent_ts",
          "opened", "clicked", "unsubscribed", "template_version", "vendor_conv",
          "cart_value", "buys")
camp <- rbind(ab[, ..keep], test_enroll[, ..keep])
# Vendor revenue = cart value at abandonment, not the actual order value
camp[, vendor_rev := fifelse(holdout == 1, NA_real_,
                             fifelse(vendor_conv == 1, cart_value, 0))]

grp <- fifelse(camp$holdout == 1, "holdout", "treatment")
grp <- mess_up(grp, 0.12, function(x)
  fifelse(x == "holdout", sample(c("Holdout", "control", "CTRL", "hold-out", "HO"), length(x), TRUE),
          sample(c("Treatment", "treatment ", "email", "TREATMENT", "TG"), length(x), TRUE)))
grp <- mess_up(grp, 0.004, function(x) NA_character_)

camp_out <- camp[, .(
  CustomerID             = cust$customer_id[cust_idx],
  trigger_session        = session_id,
  enrolled_at            = format(start_ts + 300, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  Group                  = grp,
  email_sent_at          = fifelse(is.na(sent_ts), "",
                                   format(sent_ts, "%Y-%m-%d %H:%M:%S", tz = "UTC")),
  delivery_status        = fifelse(holdout == 1, "not_sent",
                                   fifelse(delivered == 1, "delivered", "bounced")),
  opened, clicked, unsubscribed, template_version,
  attributed_conversion  = vendor_conv,
  attributed_revenue_eur = vendor_rev
)]
# Holdout rows: engagement sometimes exported as 0 instead of missing
ho <- camp$holdout == 1 & runif(nrow(camp)) < 0.5
camp_out[ho, `:=`(opened = 0L, clicked = 0L, attributed_conversion = 0L)]
# Sync bug: some customers enrolled twice, half of them with the other group label
dup <- camp_out[sample(.N, 60)]
flip <- runif(60) < 0.5
dup[flip, Group := fifelse(grepl("^(h|c|H|C)", Group), "treatment", "holdout")]
camp_out <- rbind(camp_out, dup)[sample(.N)]
fwrite(camp_out, file.path(raw_dir, "campaign_log.csv"))

## 8e. vendor_dashboard_export.csv --------------------------------------------
tg <- camp[holdout == 0]
sent   <- nrow(tg)
deliv  <- tg[, sum(delivered)]
opens  <- tg[, sum(opened, na.rm = TRUE)]
clicks <- tg[, sum(clicked, na.rm = TRUE)]
conv_clickers <- tg[clicked == 1, mean(buys)]
conv_openers  <- tg[opened == 1, mean(vendor_conv)]
site_conv     <- sess[!is.na(cust_idx), mean(purchased)]

vendor <- data.table(
  metric = c("Emails sent", "Delivery rate", "Open rate", "Click rate (CTR)",
             "Click-to-open rate", "Conversion rate (clickers)",
             "Conversion rate (openers)", "Attributed orders",
             "Attributed revenue (EUR)", "Revenue per email (EUR)",
             "Site-wide session conversion rate (benchmark)",
             "Uplift vs. site benchmark"),
  value = c(sent,
            sprintf("%.1f%%", 100 * deliv / sent),
            sprintf("%.1f%%", 100 * opens / deliv),
            sprintf("%.1f%%", 100 * clicks / deliv),
            sprintf("%.1f%%", 100 * clicks / opens),
            sprintf("%.1f%%", 100 * conv_clickers),
            sprintf("%.1f%%", 100 * conv_openers),
            tg[, sum(vendor_conv)],
            round(tg[, sum(vendor_rev)]),
            sprintf("%.2f", tg[, sum(vendor_rev)] / sent),
            sprintf("%.1f%%", 100 * site_conv),
            sprintf("%.1fx", conv_openers / site_conv)),
  period = "2026-09-01 to 2026-10-31"
)
fwrite(vendor, file.path(raw_dir, "vendor_dashboard_export.csv"))

# ---- 9. Answer key -------------------------------------------------------------

key <- c(
  "ANSWER KEY (do not share with students)",
  "=======================================",
  sprintf("Seed: %d | customers: %d | enrolled abandoners: %d", seed, n, na_ab),
  sprintf("Enrolled with full %d-day window before export: %d (treatment %d, holdout %d)",
          params$outcome_window_days, nrow(kd), n_t, n_c),
  "",
  "Clean intention-to-treat estimate (7-day order rate, full-window cohort,",
  "orders of any status counted):",
  sprintf("  Treatment: %.2f%% | Holdout: %.2f%% | Difference: %.2f pp",
          100 * itt_t, 100 * itt_c, 100 * (itt_t - itt_c)),
  sprintf("True average effect in treatment group: %.2f pp (logit effect %.2f,",
          100 * true_ate, params$email_effect_logit),
  "  diluted by about 4% bounced emails, which stay in the treatment group)",
  sprintf("Power of this design (two-sided, alpha = .05): %.0f%%", 100 * power_design),
  "",
  "Net revenue per enrolled customer (completed orders within 7 days):",
  sprintf("  Treatment: EUR %.2f | Holdout: EUR %.2f | Difference: EUR %.2f",
          rev_t, rev_c, rev_t - rev_c),
  "",
  "Vendor dashboard claims:",
  sprintf("  Conversion rate among clickers: %.1f%%", 100 * conv_clickers),
  sprintf("  Conversion rate among openers: %.1f%%", 100 * conv_openers),
  sprintf("  'Uplift' vs site-wide session conversion: %.1fx", conv_openers / site_conv),
  sprintf("  Attributed revenue: EUR %s", format(round(tg[, sum(vendor_rev)]), big.mark = ",")),
  "",
  "Traps built into the raw data:",
  "  - Abandoners have high intent: the holdout also converts at a high rate",
  "  - Clickers are self-selected buyers; open rates inflated (Apple MPP)",
  "  - Vendor attributes any purchase within 30 days after an open, and uses",
  "    cart value instead of actual order value as revenue",
  "  - Site-wide session conversion is the wrong benchmark",
  "  - Abandonments in the last days of October lack a full 7-day window",
  "  - Orders are in local time (Europe/Berlin), sessions in UTC",
  "  - Orders use ';' and German number format; status labels in German/English",
  "  - Cancelled and returned orders, duplicate order lines, TEST_ accounts",
  "  - Bot sessions (no customer id, many pages, very short duration)",
  "  - Mixed timestamp formats (ISO and Unix epoch) in sessions after Oct 15",
  "  - Inconsistent and missing group labels, duplicated enrollments with",
  "    conflicting labels (delivery_status reveals the true assignment);",
  "    holdout engagement sometimes 0 instead of missing",
  "  - Duplicate customers and sessions; messy country, age, device values",
  "  - Irrelevant variables: browser, page load time, gender, template version,",
  "    payment method, discount code, etc."
)
writeLines(key, file.path(key_dir, "answer_key.txt"))


# Document R and package versions -----
sessionInfo()