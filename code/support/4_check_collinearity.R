# ===============================================================
# 4_check_collinearity.R
# ---------------------------------------------------------------
# PURPOSE
#   Check collinearity between two land-cover proportion outputs
#   from 3_calculate_nlcd.R (generically "LndCov1" and "LndCov2").
#
# CONFIGURE THE TWO CATEGORIES TO COMPARE
#   Set `LndCov1_name` / `LndCov2_name` below to any `target_name`
#   already computed by 3_calculate_nlcd.R (i.e. any output/<name>.csv
#   that exists). Currently set to "developed" and "grasslands".
#
# INPUT
#   output/<LndCov1_name>.csv
#   output/<LndCov2_name>.csv
#
# OUTPUT
#   Console summary: Pearson correlation (overall + per year) and
#     variance inflation factor (VIF).
#   output/check_collinearity_<LndCov1_name>_vs_<LndCov2_name>.png -
#     scatter plot with regression line.
# ---------------------------------------------------------------

library(here)
library(tidyverse)

here::i_am("code/support/4_check_collinearity.R")

## Settings ####
LndCov1_name <- "developed"
LndCov2_name <- "grasslands"

## Load data ####
LndCov1_path <- here::here("output", paste0(LndCov1_name, ".csv"))
LndCov2_path <- here::here("output", paste0(LndCov2_name, ".csv"))

LndCov1_df <- read.csv(LndCov1_path, header = TRUE) %>%
  rename(LndCov1 = all_of(LndCov1_name))
LndCov2_df <- read.csv(LndCov2_path, header = TRUE) %>%
  rename(LndCov2 = all_of(LndCov2_name))

## Join on shared route/year identifiers ####
id_cols <- c("CountryNum", "StateNum", "Route", "year")

combined <- inner_join(
  LndCov1_df %>% select(all_of(id_cols), LndCov1),
  LndCov2_df %>% select(all_of(id_cols), LndCov2),
  by = id_cols
) %>%
  filter(!is.na(LndCov1), !is.na(LndCov2))

cat("Comparing:", LndCov1_name, "(LndCov1) vs.", LndCov2_name, "(LndCov2)\n")
cat("Rows with both values present:", nrow(combined), "\n\n")

## Overall correlation ####
cor_test <- cor.test(combined$LndCov1, combined$LndCov2)
r <- unname(cor_test$estimate)
vif <- 1 / (1 - r^2)

cat("== Overall ==\n")
cat("Pearson r:", round(r, 4), "\n")
cat("p-value:  ", format.pval(cor_test$p.value, digits = 3), "\n")
cat("VIF (1/(1-r^2)):", round(vif, 3), "\n\n")

## Per-year correlation ####
cat("== Per year ==\n")
per_year <- combined %>%
  group_by(year) %>%
  summarise(
    n = n(),
    r = cor(LndCov1, LndCov2, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(vif = 1 / (1 - r^2))

print(per_year, n = Inf)

## Rule of thumb ####
cat("\nNote: |r| > 0.7 (VIF > ~2) is often flagged as a collinearity concern.\n")

# ## Scatter plot ####
# p <- ggplot(combined, aes(x = LndCov1, y = LndCov2)) +
#   geom_point(alpha = 0.2, size = 0.6) +
#   geom_smooth(method = "lm", color = "red", se = FALSE) +
#   labs(
#     title = paste0(LndCov1_name, " vs. ", LndCov2_name, " (r = ", round(r, 3), ")"),
#     x = paste0(LndCov1_name, " (proportion)"),
#     y = paste0(LndCov2_name, " (proportion)")
#   ) +
#   theme_minimal()
# 
# p

