# This script reads the four raw exports, cleans them, and merges them into one
# analysis table with one row per enrolled cart abandoner.
#
# Input:  data/raw/customers.csv, web_sessions.csv, orders.csv, campaign_log.csv
# Output: data/processed/customers_clean.csv, sessions_clean.csv,
#         orders_clean.csv, campaign_clean.csv, analysis_data.csv
#
# When this script runs via run_all.R, its log (outputs/logs/01_read_data.Rout)
# shows every line below together with its output. We therefore print the
# number of rows and the results of key checks after each cleaning step, so that
# every cleaning decision and its consequences can be traced in the log.

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
dir.create(path = here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

# Study design (see codebook.md)
export_ts   <- as.POSIXct(x = "2026-11-04 00:00:00", tz = "UTC")  # time of data export
window_days <- 7                                                  # outcome window in days

# Two small helpers for messy text values
norm_txt <- function(x) tolower(x = trimws(x = x))
blank_to_na <- function(x) fifelse(test = trimws(x = x) == "", yes = NA_character_, no = x)

# Read data -----
cu <- fread(input = here("data", "raw", "customers.csv"),
            colClasses = "character", encoding = "UTF-8") |> unique()
se <- fread(input = here("data", "raw", "web_sessions.csv"),
            colClasses = "character", encoding = "UTF-8") |> unique()
or <- fread(input = here("data", "raw", "orders.csv"), sep = ";",
            colClasses = "character", encoding = "UTF-8") |> unique()
ca <- fread(input = here("data", "raw", "campaign_log.csv"),
            colClasses = "character", encoding = "UTF-8") |> unique()


# Customers (CRM export) -----
setnames(x = cu, old = "Customer ID", new = "customer_id")

# remove test accounts
cu <- cu[!startsWith(x = customer_id, prefix = "TEST_")]

# dimensions:
cu[, .N]
anyDuplicated(x = cu$customer_id)   # 0 = every customer appears exactly once

# Two date formats: ISO (2026-09-01) and German (01.09.2026)
cu[, signup_date := as.IDate(fcoalesce(as.Date(x = signup_date, format = "%Y-%m-%d"),
                                       as.Date(x = signup_date, format = "%d.%m.%Y")))]
cu[is.na(signup_date), .N] # dates that could not be parsed

# Country names in different spellings -> two-letter codes
country_map <- c(de = "DE", germany = "DE", deutschland = "DE", d = "DE",
                 at = "AT", austria = "AT", "österreich" = "AT", "Österreich" = "AT",
                 ch = "CH", switzerland = "CH",
                 nl = "NL", netherlands = "NL")
cu[, country := unname(obj = country_map[norm_txt(x = country)])]
cu[, .N, by = country] # NA = missing or unknown

# Implausible ages (outside 16 to 100) -> NA
cu[, age := suppressWarnings(expr = as.integer(x = age))]
cu[!is.na(age) & (age < 16 | age > 100), .N] # implausible ages
cu[!is.na(age) & (age < 16 | age > 100), age := NA_integer_]

cu[, newsletter_opt_in := fcase(
  norm_txt(x = newsletter_opt_in) %in% c("y", "yes", "1", "true"),  TRUE,
  norm_txt(x = newsletter_opt_in) %in% c("n", "no", "0", "false"), FALSE,
  default = NA)]
cu[, gender := blank_to_na(x = gender)]
cu[, email_known := as.integer(x = email_known)]

fwrite(x = cu, file = here("data", "processed", "customers_clean.csv"))


# Web sessions (web analytics export, UTC) -----
setnames(x = se,
         old = c("Session ID", "customerId", "Start Time (UTC)", "Device", "purchase_flag"),
         new = c("session_id", "customer_id", "start_raw", "device", "purchased"))

# dimensions
se[, .N]
anyDuplicated(x = se$session_id)

# Timestamps: ISO 8601 before Oct 15, partly Unix epoch seconds afterwards
se[, is_epoch := grepl(pattern = "^[0-9]+$", x = start_raw)]
se[, .N, by = is_epoch] # how many timestamps per format
se[, start_ts := as.POSIXct(x = NA_real_, tz = "UTC")]
se[is_epoch == TRUE,
   start_ts := as.POSIXct(x = as.numeric(start_raw), origin = "1970-01-01", tz = "UTC")]
se[is_epoch == FALSE,
   start_ts := as.POSIXct(x = start_raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
se[is.na(start_ts), .N] # timestamps that could not be parsed
se[, c("start_raw", "is_epoch") := NULL]

se[, customer_id := blank_to_na(x = customer_id)]
se[, device := fcase(
  norm_txt(x = device) %in% c("mobile", "smartphone", "ios", "android"), "mobile",
  norm_txt(x = device) %in% c("desktop", "pc"), "desktop",
  norm_txt(x = device) %in% c("tablet", "ipad"), "tablet",
  default = NA_character_)]

# Cart value: remove everything except digits, "." and ",", then use "." as decimal sign
se[, cart_value_eur := gsub(pattern = "[^0-9.,]", replacement = "", x = cart_value_eur)]
se[, cart_value_eur := as.numeric(sub(pattern = ",", replacement = ".", x = cart_value_eur,
                                      fixed = TRUE))]

# added_to_cart comes as 1/0, TRUE/FALSE, yes/no, Y, or blank
se[, added_to_cart := fcase(
  norm_txt(x = added_to_cart) %in% c("1", "true", "yes", "y"), 1L,
  norm_txt(x = added_to_cart) %in% c("0", "false", "no"), 0L,
  default = NA_integer_)]
se[is.na(added_to_cart), .N] # blanks, inferred from the cart value
se[is.na(added_to_cart), added_to_cart := as.integer(!is.na(cart_value_eur))]

# set certain cols to integers
int_cols <- c("pages_viewed", "session_duration_sec", "page_load_time_ms",
              "checkout_started", "purchased")
se[, (int_cols) := lapply(X = .SD, FUN = as.integer), .SDcols = int_cols]

# Bots: anonymous, implausibly many pages within seconds, or automated browsers
se[, is_bot := is.na(customer_id) &
     ((pages_viewed >= 80 & session_duration_sec <= 20) |
        browser %in% c("HeadlessChrome", "python-requests"))]
se[is_bot == TRUE, .N] # bot sessions
se <- se[is_bot == FALSE]
se[, is_bot := NULL]

se <- se[!startsWith(x = fcoalesce(customer_id, ""), prefix = "TEST_")]
se[, .N] # after removing bots and test accounts

setcolorder(x = se, neworder = c("session_id", "customer_id", "start_ts"))
fwrite(x = se, file = here("data", "processed", "sessions_clean.csv"))


# Orders (German ERP export, local time Europe/Berlin) -----
setnames(x = or,
         old = c("Bestellnummer", "Kundennummer", "session_ref", "Bestelldatum",
                 "Bestellwert_EUR", "Status", "Zahlungsart", "Gutscheincode", "Lieferland"),
         new = c("order_id", "customer_id", "session_id", "order_time_local",
                 "order_value_eur", "status", "payment_method", "discount_code",
                 "shipping_country"))

# remove test users
or <- or[!startsWith(x = customer_id, prefix = "TEST_")]

# dimensions
or[, .N]
anyDuplicated(x = or$order_id)

# German number format: "1.234,56" -> 1234.56
or[, order_value_eur := gsub(pattern = ".", replacement = "", x = order_value_eur, fixed = TRUE)]
or[, order_value_eur := as.numeric(sub(pattern = ",", replacement = ".", x = order_value_eur,
                                       fixed = TRUE))]

# Local time -> UTC, so that orders can be compared with session timestamps
or[, order_ts := as.POSIXct(x = order_time_local, format = "%d.%m.%Y %H:%M",
                            tz = "Europe/Berlin")]
attr(x = or$order_ts, which = "tzone") <- "UTC"
or[is.na(order_ts), .N] # timestamps that could not be parsed
or[, order_time_local := NULL]

# Status in English and German -> English
or[, status := fcase(
  norm_txt(x = status) %in% c("completed", "abgeschlossen"), "completed",
  norm_txt(x = status) %in% c("returned", "retoure"), "returned",
  norm_txt(x = status) %in% c("cancelled", "storniert"), "cancelled",
  default = NA_character_)]
or[, .N, by = status]

or[, session_id := blank_to_na(x = session_id)]
or[, discount_code := blank_to_na(x = discount_code)]
setcolorder(x = or, neworder = c("order_id", "customer_id", "order_ts"))
fwrite(x = or, file = here("data", "processed", "orders_clean.csv"))


# Campaign log (email tool export) -----
setnames(x = ca,
         old = c("CustomerID", "trigger_session", "Group"),
         new = c("customer_id", "trigger_session_id", "group_label"))

ca <- ca[!startsWith(x = customer_id, prefix = "TEST_")]
ca[, .N] # after removing test enrollments

# Group labels are typed inconsistently. delivery_status is set automatically
# by the email tool based on the random assignment ("not_sent" = holdout),
# so we take the group from there and only compare it with the labels.
ca[, group_from_label := fcase(
  norm_txt(x = group_label) %in% c("holdout", "control", "ctrl", "hold-out", "ho"), "holdout",
  norm_txt(x = group_label) %in% c("treatment", "email", "tg"), "treatment",
  default = NA_character_)]
ca[, group := fifelse(test = delivery_status == "not_sent", yes = "holdout", no = "treatment")]
ca[, .N, by = .(group_from_label, group)] # labels vs. delivery_status

# Duplicated enrollments (sync bug): same customer, same trigger session
ca[, uniqueN(x = trigger_session_id), by = customer_id][, max(V1)] # 1 = one trigger session per customer
ca <- unique(x = ca, by = "customer_id")
ca[, .N] # after removing duplicated enrollments
ca[, c("group_label", "group_from_label") := NULL]

ca[, enrolled_at := as.POSIXct(x = enrolled_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
ca[, email_sent_at := as.POSIXct(x = blank_to_na(x = email_sent_at),
                                 format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
num_cols <- c("opened", "clicked", "unsubscribed", "attributed_conversion")
ca[, (num_cols) := lapply(X = .SD, FUN = as.integer), .SDcols = num_cols]
ca[, attributed_revenue_eur := as.numeric(attributed_revenue_eur)]

# The holdout received no email, so its engagement is undefined (NA, not 0)
ca[group == "holdout", c(num_cols, "attributed_revenue_eur") := NA]
ca[, template_version := blank_to_na(x = template_version)]

# Only abandonments whose 7-day outcome window ended before the data export
ca[, full_window := enrolled_at + window_days * 86400 <= export_ts]
ca[, .N, by = full_window]

fwrite(x = ca, file = here("data", "processed", "campaign_clean.csv"))

# Merge -----
# into table with one row per enrolled abandoner
ad <- copy(x = ca)
ad[, treated := as.integer(group == "treatment")]

# Add the abandoned cart from the trigger session
# all.x = TRUE keeps every enrollment, even if no matching session is found
merge_1 <- merge(x = ad,
                 y = se[, .(session_id, abandoned_cart_value = cart_value_eur, device,
                            pages_viewed, traffic_source)],
                 by.x = "trigger_session_id", 
                 by.y = "session_id", 
                 all.x = TRUE)
merge_1[is.na(abandoned_cart_value), .N] # enrollments without a matching session

# Add customer attributes
merge_2 <- merge(x = merge_1,
                 y = cu[, .(customer_id, country, age, loyalty_tier, newsletter_opt_in,
                            signup_date)],
                 by = "customer_id",
                 all.x = TRUE)
merge_2[, tenure_days := as.integer(as.IDate(enrolled_at) - signup_date)]
merge_2[, new_customer := as.integer(tenure_days < 60)]

# Add outcomes: orders placed after enrollment and within the next 7 days
# Step 1: attach all orders of each enrolled customer (customers without orders drop out)
o_win <- merge(x = merge_2[, .(customer_id, enrolled_at)],
               y = or[, .(customer_id, order_id, order_ts, order_value_eur, status)],
               by = "customer_id")

# Step 2: keep only orders within the 7-day window
o_win <- o_win[order_ts > enrolled_at & order_ts <= enrolled_at + window_days * 86400]

# Step 3: summarize the orders per customer
outcomes <- o_win[, .(
  orders_7d          = .N,
  revenue_gross_7d   = sum(order_value_eur),
  revenue_net_7d     = sum(order_value_eur[status == "completed"]),
  completed_order_7d = as.integer(any(status == "completed"))
), by = customer_id]

# Step 4: add the summaries to the analysis table
final <- merge(x = merge_2, 
               y = outcomes, 
               by = "customer_id", 
               all.x = TRUE)

# Customers without an order in the window get 0 instead of NA
final[is.na(orders_7d), `:=`(orders_7d = 0L, revenue_gross_7d = 0, revenue_net_7d = 0,
                             completed_order_7d = 0L)]
final[, ordered_7d := as.integer(orders_7d > 0)]

setcolorder(x = final, neworder = c("customer_id", "group", "treated", "full_window",
                                    "enrolled_at", "ordered_7d", "completed_order_7d",
                                    "orders_7d", "revenue_gross_7d", "revenue_net_7d"))
setorder(final, enrolled_at)
final[, .N, by = .(full_window, group)] # final sample sizes

# Write data -----
fwrite(x = final, file = here("data", "processed", "analysis_data.csv"))


# Document R and package versions -----
sessionInfo()