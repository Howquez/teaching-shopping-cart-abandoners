# Codebook: Cart Abandonment Email Campaign

## Study context

All data are simulated by `scripts/00_simulate_raw_data.R` and describe a fictional online shop; they contain no information on real persons or companies (see README.md for the simulation design).

The shop (customers in DE, AT, CH, NL) sends a reminder email ("You left something in your cart") to customers who add items to their cart but leave without buying. The campaign ran from **1 September to 31 October 2026**.

Each customer can enter the campaign once, at their first cart abandonment in the period, and only if their email address is known. At enrollment, a random **10%** of customers are assigned to a **holdout** group that receives no email. All other customers are assigned to **treatment** and receive the email about one hour after abandonment.

All data were exported on **4 November 2026, 00:00 UTC**. Events after this point are not observed.

**Primary outcome:** the customer places an order within 7 days after enrollment.

## Raw data (`data/raw/`)

Raw files are never edited by hand. All cleaning happens in `scripts/01_read_data.R`.

### `customers.csv` (CRM export, one row per customer)

| Variable | Type | Description |
|---|---|---|
| `Customer ID` | string | Customer identifier (`C` + 6 digits). Test accounts start with `TEST_`. |
| `signup_date` | date | Account creation date. Format `YYYY-MM-DD` or `DD.MM.YYYY`. |
| `country` | string | Country of residence. Intended values `DE`, `AT`, `CH`, `NL`; free-text variants occur. |
| `age` | integer | Self-reported age in years. Contains implausible values. |
| `gender` | string | `f`, `m`, `d`, or blank. |
| `newsletter_opt_in` | string | Newsletter subscription. Intended `Y`/`N`; variants such as `yes`, `1`, `FALSE` occur. |
| `loyalty_tier` | string | `none`, `silver`, `gold`. |
| `email_known` | 0/1 | 1 if an email address is on file (precondition for enrollment). |

### `web_sessions.csv` (web analytics export, one row per visit)

| Variable | Type | Description |
|---|---|---|
| `Session ID` | string | Session identifier. |
| `customerId` | string | Customer identifier; blank if the visitor was not logged in. |
| `Start Time (UTC)` | datetime | Session start in UTC. ISO 8601 (`2026-09-01T14:03:11Z`); after a tracking update on 15 October partly Unix epoch seconds. |
| `Device` | string | Device type. Intended `mobile`, `desktop`, `tablet`; variants occur. |
| `traffic_source` | string | `organic`, `paid_search`, `social`, `direct`, `newsletter`, `affiliate`, `email_cart_reminder`. |
| `browser` | string | Browser name. |
| `pages_viewed` | integer | Number of pages viewed in the session. |
| `session_duration_sec` | integer | Session length in seconds. |
| `page_load_time_ms` | integer | Average page load time in milliseconds. |
| `added_to_cart` | string | Whether an item was added to the cart. Intended `1`/`0`; variants and blanks occur. |
| `cart_value_eur` | string | Cart value in EUR at the end of the session; blank if no cart. Decimal point or comma, sometimes with `€`. |
| `checkout_started` | 0/1 | Whether the checkout page was reached. |
| `purchase_flag` | 0/1 | Whether the session ended with a purchase. |

### `orders.csv` (ERP export, one row per order)

German export format: fields separated by `;`, numbers with `.` as thousands and `,` as decimal separator, timestamps in **local time (Europe/Berlin)**.

| Variable | Type | Description |
|---|---|---|
| `Bestellnummer` | string | Order ID. |
| `Kundennummer` | string | Customer ID. |
| `session_ref` | string | Session in which the order was placed; often blank. |
| `Bestelldatum` | datetime | Order time, `DD.MM.YYYY HH:MM`, Europe/Berlin. |
| `Bestellwert_EUR` | string | Order value in EUR incl. shipping (e.g. `1.234,56`). |
| `Status` | string | `completed`, `cancelled`, `returned`; German variants (`abgeschlossen`, `storniert`, `Retoure`) occur. |
| `Zahlungsart` | string | Payment method. |
| `Gutscheincode` | string | Discount code used, if any. |
| `Lieferland` | string | Shipping country. |

### `campaign_log.csv` (email tool export, one row per enrollment)

| Variable | Type | Description |
|---|---|---|
| `CustomerID` | string | Customer ID. |
| `trigger_session` | string | Session in which the cart was abandoned (links to `web_sessions.csv`). |
| `enrolled_at` | datetime | Enrollment time (UTC), about 5 minutes after the abandoning session started. |
| `Group` | string | Assigned group as labeled by the CRM team. Intended `treatment`/`holdout`; free-text variants and blanks occur. |
| `email_sent_at` | datetime | Time the email was sent (UTC); blank for the holdout. |
| `delivery_status` | string | `delivered`, `bounced`, or `not_sent` (holdout). Set automatically by the email tool. |
| `opened` | 0/1 | Email opened (tracking pixel; inflated by Apple Mail Privacy Protection). |
| `clicked` | 0/1 | Link in the email clicked. |
| `unsubscribed` | 0/1 | Unsubscribed via the email. |
| `template_version` | string | Subject line variant. |
| `attributed_conversion` | 0/1 | Vendor attribution: 1 if the customer made **any** purchase within 30 days after opening the email. |
| `attributed_revenue_eur` | numeric | Vendor-attributed revenue: the **cart value at abandonment** for attributed conversions. |

### `vendor_dashboard_export.csv`

Summary metrics as shown in the email vendor's dashboard (`metric`, `value`, `period`). Computed by the vendor with its own definitions; reproduced here as reported.

## Processed data (`data/processed/`)

Created by `scripts/01_read_data.R`. The cleaned tables (`customers_clean.csv`, `sessions_clean.csv`, `orders_clean.csv`, `campaign_clean.csv`) keep the raw structure with standardized names, types and values; test accounts, bots and duplicates are removed. Timestamps are stored in UTC. All cleaning decisions and their consequences (for example, the number of rows removed) are documented in the log file `outputs/logs/01_read_data.Rout`. The processed files are not part of the repository and are recreated by `run_all.R`.

### `analysis_data.csv` (one row per enrolled customer)

| Variable | Type | Description |
|---|---|---|
| `customer_id` | string | Customer ID. |
| `group` | string | `treatment` or `holdout`, derived from `delivery_status`. |
| `treated` | 0/1 | 1 if assigned to treatment (intention to treat; includes bounced emails). |
| `full_window` | logical | TRUE if enrollment + 7 days lies before the export; only these rows enter the analysis. |
| `enrolled_at` | datetime | Enrollment time (UTC). |
| `ordered_7d` | 0/1 | **Primary outcome.** Placed at least one order (any status) within 7 days after enrollment. |
| `completed_order_7d` | 0/1 | Placed at least one order that was neither cancelled nor returned. |
| `orders_7d` | integer | Number of orders within 7 days. |
| `revenue_gross_7d` | numeric | Sum of order values within 7 days, EUR, all statuses. |
| `revenue_net_7d` | numeric | Sum of order values within 7 days, EUR, completed orders only. |
| `trigger_session_id` | string | Abandoning session. |
| `email_sent_at` | datetime | Send time (UTC); NA for the holdout. |
| `delivery_status` | string | `delivered`, `bounced`, `not_sent`. |
| `opened`, `clicked`, `unsubscribed` | 0/1 | Email engagement; NA for the holdout. |
| `template_version` | string | Subject line variant; NA for the holdout. |
| `attributed_conversion`, `attributed_revenue_eur` | 0/1, numeric | Vendor attribution (see above); NA for the holdout. |
| `abandoned_cart_value` | numeric | Cart value of the abandoned cart, EUR. |
| `device` | string | Device of the abandoning session. |
| `pages_viewed` | integer | Pages viewed in the abandoning session. |
| `traffic_source` | string | Traffic source of the abandoning session. |
| `country`, `age`, `loyalty_tier`, `newsletter_opt_in` | various | Customer attributes from the CRM (cleaned). |
| `signup_date` | date | Account creation date. |
| `tenure_days` | integer | Days between signup and enrollment. |
| `new_customer` | 0/1 | 1 if tenure is below 60 days. |
