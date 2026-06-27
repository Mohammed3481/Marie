# Descriptive summary and outlier detection for cattle body weight
# -----------------------------------------------------------------
# This script creates a variable named `body_weight` with 20 cattle
# body-weight observations, calculates descriptive statistics, and
# detects possible outliers using the interquartile range (IQR) rule.

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

# Optional visual check
boxplot(
  body_weight,
  main = "Cattle Body Weight Outlier Check",
  ylab = "Body weight",
  col = "lightblue"
)
