# Descriptive summary, outlier detection, and plotting for cattle body weight
# --------------------------------------------------------------------------
# This script creates a variable named `body_weight` with 20 cattle
# body-weight observations, calculates descriptive statistics, detects
# possible outliers using the interquartile range (IQR) rule, and saves
# plots that make the body-weight distribution and outliers easier to see.

body_weight <- c(
  430, 455, 462, 478, 489,
  495, 501, 512, 520, 530,
  542, 555, 563, 575, 588,
  600, 615, 628, 640, 760
)

# Descriptive summary
summary_statistics <- data.frame(
  count = length(body_weight),
  mean = mean(body_weight),
  median = median(body_weight),
  standard_deviation = sd(body_weight),
  variance = var(body_weight),
  minimum = min(body_weight),
  first_quartile = quantile(body_weight, 0.25),
  third_quartile = quantile(body_weight, 0.75),
  maximum = max(body_weight),
  range = diff(range(body_weight)),
  iqr = IQR(body_weight),
  row.names = NULL
)

print("Descriptive summary for cattle body weight:")
print(summary_statistics)

# Outlier detection with the 1.5 * IQR rule
q1 <- quantile(body_weight, 0.25)
q3 <- quantile(body_weight, 0.75)
iqr_value <- IQR(body_weight)
lower_limit <- q1 - 1.5 * iqr_value
upper_limit <- q3 + 1.5 * iqr_value

outliers <- body_weight[body_weight < lower_limit | body_weight > upper_limit]

cat("\nOutlier detection using the 1.5 * IQR rule:\n")
cat("Lower limit:", lower_limit, "\n")
cat("Upper limit:", upper_limit, "\n")

if (length(outliers) == 0) {
  cat("No outliers detected.\n")
} else {
  cat("Outliers detected:", paste(outliers, collapse = ", "), "\n")
}

# Plot the results and save them to a PNG file. The file is useful when
# running this script in a non-interactive session, because it can be opened
# after the script finishes.
plot_file <- "cattle_body_weight_plots.png"
png(filename = plot_file, width = 1000, height = 500)
par(mfrow = c(1, 2))

hist(
  body_weight,
  main = "Distribution of Cattle Body Weight",
  xlab = "Body weight",
  col = "lightblue",
  border = "white"
)
abline(v = mean(body_weight), col = "blue", lwd = 2, lty = 2)
abline(v = median(body_weight), col = "darkgreen", lwd = 2, lty = 3)
legend(
  "topright",
  legend = c("Mean", "Median"),
  col = c("blue", "darkgreen"),
  lty = c(2, 3),
  lwd = 2,
  bty = "n"
)

boxplot(
  body_weight,
  main = "Outlier Check for Cattle Body Weight",
  ylab = "Body weight",
  col = "lightyellow",
  outline = TRUE
)
abline(h = lower_limit, col = "red", lwd = 2, lty = 2)
abline(h = upper_limit, col = "red", lwd = 2, lty = 2)
stripchart(
  body_weight,
  method = "jitter",
  vertical = TRUE,
  pch = 19,
  col = "darkblue",
  add = TRUE
)

if (length(outliers) > 0) {
  points(
    rep(1, length(outliers)),
    outliers,
    col = "red",
    pch = 19,
    cex = 1.5
  )
}

dev.off()
cat("\nPlot saved to:", plot_file, "\n")
