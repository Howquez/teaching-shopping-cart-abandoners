# This script turns the saved results into figures and tables. No estimation
# happens here, so figures can be restyled without rerunning the analysis.
#
# Input:  outputs/results/analysis_results.rds, outputs/results/power_results.rds
# Output: outputs/figures/*.png, outputs/tables/*.csv

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
dir.create(path = here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(path = here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)


# Read data -----
res <- readRDS(file = here("outputs", "results", "analysis_results.rds"))
pow <- readRDS(file = here("outputs", "results", "power_results.rds"))


# Tables -----
fwrite(x = res$vendor_view, file = here("outputs", "tables", "01_vendor_view.csv"))
fwrite(x = res$itt,         file = here("outputs", "tables", "02_itt_effect.csv"))
fwrite(x = res$regression,  file = here("outputs", "tables", "03_regression.csv"))
fwrite(x = res$economics,   file = here("outputs", "tables", "04_economics.csv"))
fwrite(x = pow$curse,       file = here("outputs", "tables", "05_winners_curse.csv"))
fwrite(x = pow$grid,        file = here("outputs", "tables", "05_power_grid.csv"))
fwrite(x = pow$needed,      file = here("outputs", "tables", "05_n_needed.csv"))


# Figures -----
# Each figure follows the same pattern:
#   png()     opens an empty image file,
#   par(mar)  sets the margins around the plot (bottom, left, top, right),
#   plotting functions draw into the file,
#   dev.off() closes and saves the file.

# Figure 1: Which comparison shows the effect of the email? -----
vv <- res$vendor_view
rates_pct <- rev(x = 100 * vv$conversion_rate)   # rev(): first group ends up on top

png(filename = here("outputs", "figures", "01_conversion_rates.png"),
    width = 1600, height = 900, res = 200)
par(mar = c(4, 13, 1, 2))
bar_pos <- barplot(height = rates_pct,
                   names.arg = rev(x = vv$group),
                   horiz = TRUE, las = 1, xlim = c(0, 100), border = NA,
                   col = rev(x = c("grey70", "grey70", "steelblue", "darkorange", "grey85")),
                   xlab = "Conversion rate (%)")
text(x = rates_pct, y = bar_pos, labels = sprintf("%.1f%%", rates_pct), pos = 4, cex = 0.8)
dev.off()

# Figure 2: Winner's curse -----
# Histogram of all simulated estimates (grey) with the significant ones on top (blue)
ours <- pow$ours
breaks <- seq(from = floor(x = min(ours$estimate) * 100) - 0.5,
              to   = ceiling(x = max(ours$estimate) * 100) + 0.5,
              by   = 0.5)
h_all <- hist(x = 100 * ours$estimate, breaks = breaks, plot = FALSE)
h_sig <- hist(x = 100 * ours[significant == TRUE, estimate], breaks = breaks, plot = FALSE)

png(filename = here("outputs", "figures", "02_winners_curse.png"),
    width = 1600, height = 900, res = 200)
par(mar = c(4, 4, 1, 1))
plot(x = h_all, col = "grey85", border = "white", main = "",
     xlab = "Estimated lift (percentage points)", ylab = "Number of simulated experiments")
plot(x = h_sig, col = "steelblue", border = "white", add = TRUE)
abline(v = 100 * pow$assumptions$lift, lwd = 2, lty = 2)
legend(x = "topright", bty = "n",
       legend = c("Not significant", "Significant (p < .05)", "True effect"),
       fill = c("grey85", "steelblue", NA), border = NA,
       lty = c(NA, NA, 2), lwd = c(NA, NA, 2))
dev.off()

# Figure 3: Power curves -----
# One line per holdout share, showing power as the sample size grows
grid <- pow$grid
shares <- sort(x = unique(x = grid$holdout_share))
cols <- c("firebrick", "darkorange", "steelblue", "darkgreen")

png(filename = here("outputs", "figures", "03_power_curves.png"),
    width = 1600, height = 1000, res = 200)
par(mar = c(4, 4, 1, 1))
plot(x = NA, xlim = range(grid$n_total), ylim = c(0, 1), las = 1,   # empty plot
     xlab = "Total enrolled customers", ylab = "Power")
abline(h = 0.8, lty = 2, col = "grey50")                            # 80% power
abline(v = pow$assumptions$n_ours, lty = 3, col = "grey50")         # our sample size
text(x = pow$assumptions$n_ours, y = 0.03, labels = "our study",
     pos = 4, cex = 0.7, col = "grey40")
for (i in seq_along(shares)) {                                      # one line per share
  lines(x = grid[holdout_share == shares[i], n_total],
        y = grid[holdout_share == shares[i], power],
        col = cols[i], lwd = 2)
}
legend(x = "bottomright", bty = "n", lwd = 2, col = cols,
       legend = sprintf("%.0f%% holdout", 100 * shares))
dev.off()


# Document R and package versions -----
sessionInfo()