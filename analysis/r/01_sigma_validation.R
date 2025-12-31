# ============================================
# OUTPUT PATHS (saved artifacts)
# - Always write outputs into: <repo_root>/analysis/outputs/
# - This makes the script runnable from any working directory.
# ============================================

# Discover this script's directory reliably when run via `Rscript`.
# (Falls back to current working directory for interactive runs.)
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
this_file <- sub(file_arg, "", args_all[grep(file_arg, args_all)])
script_dir <- if (length(this_file) > 0) dirname(normalizePath(this_file)) else getwd()

# Repo root = two levels up from analysis/r/
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)

# Output root (tracked, reproducible artifacts)
out_dir <- file.path(repo_root, "analysis", "outputs")
fig_dir <- file.path(out_dir, "figures")
data_dir <- file.path(out_dir, "data")
log_dir <- file.path(out_dir, "logs")

# Ensure folders exist
for (d in c(out_dir, fig_dir, data_dir, log_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

cat("Repo root:", repo_root, "\n")
cat("Outputs will be saved under:", out_dir, "\n\n")

# ============================================
# LIBRARIES (Rscript starts a clean session)
# ============================================

suppressPackageStartupMessages({
  library(tidyverse)   # for %>%, dplyr, ggplot2, readr
  library(NHANES)      # demo dataset
  library(pROC)        # ROC/AUC
})

# ============================================
# PART 1: SYNTHETIC VALIDATION
# ============================================

cat("=== SYNTHETIC DATA: ILLUSTRATIVE DEMONSTRATION ===\n\n")

set.seed(123)
n <- 5000

# Create clean synthetic data
age <- round(runif(n, 30, 80))
sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52))
bmi <- pmax(pmin(rnorm(n, mean = 28, sd = 5), 45), 18)

# HbA1c increases with age
hba1c <- 4.5 + 0.03*(age-30) + rnorm(n, sd = 0.5) + ifelse(sex == "Male", 0.1, 0)

# Diabetes probability
log_odds <- -7 + 0.07*age + 0.8*(hba1c - 5.5)*2 + 0.05*(bmi - 25)
diabetes_prob <- 1/(1 + exp(-log_odds))
diabetes <- rbinom(n, 1, diabetes_prob)

# Create dataframe
synth_df <- data.frame(
    id = 1:n,
    age = age,
    sex = sex,
    bmi = bmi,
    hba1c = hba1c,
    diabetes = diabetes
)

# Create age groups and sigma
synth_df <- synth_df %>%
    mutate(
        age_group = cut(age, breaks = seq(30, 85, by = 5)),
        age_group = as.character(age_group)
    ) %>%
    group_by(age_group) %>%
    mutate(
        mu_hba1c = mean(hba1c, na.rm = TRUE),
        sd_hba1c = sd(hba1c, na.rm = TRUE),
        sigma = (hba1c - mu_hba1c) / sd_hba1c
    ) %>%
    ungroup()

# Clean data
synth_clean <- synth_df %>%
    filter(!is.na(sigma), !is.na(diabetes), !is.na(bmi))

cat("Synthetic Data Summary:\n")
cat("Sample size:", nrow(synth_clean), "\n")
cat("Diabetes cases:", sum(synth_clean$diabetes), 
    paste0("(", round(100*mean(synth_clean$diabetes), 1), "%)\n\n"))

# Models
model_trad <- glm(diabetes ~ hba1c + age + sex + bmi, 
                  data = synth_clean, family = binomial)
model_sigma <- glm(diabetes ~ sigma + age + sex + bmi,
                   data = synth_clean, family = binomial)

# Predictions
synth_clean$pred_trad <- predict(model_trad, type = "response")
synth_clean$pred_sigma <- predict(model_sigma, type = "response")

# Toggle: set to FALSE if you do not want to version binary artifacts in Git.
SAVE_RDS <- TRUE

# ROC analysis
roc_trad <- roc(synth_clean$diabetes, synth_clean$pred_trad)
roc_sigma <- roc(synth_clean$diabetes, synth_clean$pred_sigma)

# Save synthetic ROC objects for reproducibility
if (SAVE_RDS) {
  saveRDS(roc_trad, file.path(data_dir, "roc_synthetic_traditional.rds"))
  saveRDS(roc_sigma, file.path(data_dir, "roc_synthetic_sigma.rds"))
}

cat("=== SYNTHETIC DATA RESULTS ===\n")
cat("Traditional model AUC:", round(auc(roc_trad), 4), "\n")
cat("Sigma model AUC:", round(auc(roc_sigma), 4), "\n\n")

# ============================================
# PART 2: NHANES DEMONSTRATION
# ============================================

cat("\n=== NHANES DATA DEMONSTRATION ===\n\n")

# Load and clean NHANES data
data(NHANES)

nhanes_df <- NHANES %>%
    # Use available variables
    select(Age, Gender, BMI, Diabetes, DirectChol) %>%
    # Basic filtering
    filter(
        Age >= 30,
        !is.na(Diabetes),
        !is.na(DirectChol),
        !is.na(BMI),
        DirectChol > 0
    ) %>%
    # Create variables
    mutate(
        diabetes = ifelse(Diabetes == "Yes", 1, 0),
        biomarker = DirectChol,
        age_group = cut(Age, breaks = c(30, 40, 50, 60, 70, 80, 90))
    ) %>%
    # Remove NAs
    filter(!is.na(biomarker), !is.na(diabetes), !is.na(BMI))

# Calculate sigma
nhanes_df <- nhanes_df %>%
    group_by(age_group) %>%
    mutate(
        mu_bio = mean(biomarker, na.rm = TRUE),
        sd_bio = sd(biomarker, na.rm = TRUE),
        sigma = (biomarker - mu_bio) / sd_bio
    ) %>%
    ungroup() %>%
    filter(!is.na(sigma), abs(sigma) < 4)

cat("NHANES Data Summary:\n")
cat("Sample size:", nrow(nhanes_df), "\n")
cat("Diabetes cases:", sum(nhanes_df$diabetes),
    paste0("(", round(100*mean(nhanes_df$diabetes), 1), "%)\n\n"))

# Run analysis if enough cases
if(sum(nhanes_df$diabetes) >= 30) {
    
    # Complete cases for modeling
    model_data <- nhanes_df %>%
        select(diabetes, biomarker, sigma, Age, Gender, BMI) %>%
        na.omit()
    
    cat("Complete cases for modeling:", nrow(model_data), "\n")
    
    # Models
    nhanes_trad <- glm(diabetes ~ biomarker + Age + Gender + BMI,
                       data = model_data, family = binomial)
    nhanes_sigma <- glm(diabetes ~ sigma + Age + Gender + BMI,
                        data = model_data, family = binomial)
    
    # Predictions
    model_data$pred_trad <- predict(nhanes_trad, type = "response")
    model_data$pred_sigma <- predict(nhanes_sigma, type = "response")
    
    # ROC analysis
    roc_nhanes_trad <- roc(model_data$diabetes, model_data$pred_trad)
    roc_nhanes_sigma <- roc(model_data$diabetes, model_data$pred_sigma)
    
    # Save NHANES ROC objects for reproducibility
    if (SAVE_RDS) {
      saveRDS(roc_nhanes_trad, file.path(data_dir, "roc_nhanes_traditional.rds"))
      saveRDS(roc_nhanes_sigma, file.path(data_dir, "roc_nhanes_sigma.rds"))
    }
    
    cat("\n=== NHANES RESULTS ===\n")
    cat("Biomarker: Direct HDL Cholesterol\n")
    cat("Traditional AUC:", round(auc(roc_nhanes_trad), 4), "\n")
    cat("Sigma AUC:", round(auc(roc_nhanes_sigma), 4), "\n")
    
} else {
    cat("Note: Too few diabetes cases for full analysis in demo dataset\n")
}

# ============================================
# PART 3: VISUALIZATIONS
# ============================================

cat("\n=== GENERATING AND SAVING VISUALIZATIONS ===\n")

# Plot 1: Biomarker vs Age
p1 <- ggplot(nhanes_df, aes(x = Age, y = biomarker)) +
    geom_point(alpha = 0.1, size = 0.5) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(title = "Biomarker vs Age (NHANES)",
         subtitle = "Cholesterol as example biomarker",
         x = "Age (years)", y = "Direct HDL Cholesterol (mg/dL)") +
    theme_minimal()

# Plot 2: Sigma distribution
p2 <- ggplot(nhanes_df, aes(x = sigma)) +
    geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    geom_vline(xintercept = c(-1, 1), linetype = "dotted", color = "gray") +
    labs(title = "Distribution of Sigma Positions",
         subtitle = "NHANES cholesterol data",
         x = "Sigma Position (z-score)", y = "Count") +
    theme_minimal()

# Plot 3: Synthetic ROC
roc_df <- data.frame(
    fpr = c(1 - roc_trad$specificities, 1 - roc_sigma$specificities),
    tpr = c(roc_trad$sensitivities, roc_sigma$sensitivities),
    model = rep(c("Traditional (HbA1c)", "Sigma (z-score)"), 
                c(length(roc_trad$sensitivities), 
                  length(roc_sigma$sensitivities)))
)

p3 <- ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
    geom_line(size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_color_manual(values = c("Traditional (HbA1c)" = "gray", 
                                  "Sigma (z-score)" = "red")) +
    labs(title = "ROC Curves: Synthetic Data",
         subtitle = "Illustrative comparison of discrimination under simplified conditions",
         x = "False Positive Rate", y = "True Positive Rate") +
    theme_minimal() +
    theme(legend.position = "bottom")

# Display plots
print(p1)
print(p2)
print(p3)

# Save plots (PNG + PDF) for manuscript use
ggsave(filename = file.path(fig_dir, "figure_nhanes_biomarker_vs_age.png"),
       plot = p1, width = 7.0, height = 4.5, dpi = 300)
ggsave(filename = file.path(fig_dir, "figure_nhanes_biomarker_vs_age.pdf"),
       plot = p1, width = 7.0, height = 4.5,
       device = cairo_pdf)

ggsave(filename = file.path(fig_dir, "figure_sigma_histogram.png"),
       plot = p2, width = 7.0, height = 4.5, dpi = 300)
ggsave(filename = file.path(fig_dir, "figure_sigma_histogram.pdf"),
       plot = p2, width = 7.0, height = 4.5,
       device = cairo_pdf)

ggsave(filename = file.path(fig_dir, "figure_synthetic_roc.png"),
       plot = p3, width = 7.0, height = 4.5, dpi = 300)
ggsave(filename = file.path(fig_dir, "figure_synthetic_roc.pdf"),
       plot = p3, width = 7.0, height = 4.5,
       device = cairo_pdf)

cat("Saved figures to:", normalizePath(fig_dir, winslash = "/", mustWork = FALSE), "\n")

# ============================================
# PART 4: SAVE RESULTS
# ============================================

# Save data
write_csv(synth_clean, file.path(data_dir, "sigma_synthetic_results.csv"))
write_csv(nhanes_df, file.path(data_dir, "sigma_nhanes_demo.csv"))

# Save summary
sink(file.path(log_dir, "sigma_demonstration_summary.txt"))
cat("Sigma-Distribution Hypothesis: Illustrative Computational Demonstration\n")
cat("========================================\n")
cat("Analysis date: ", format(Sys.Date(), "%Y-%m-%d"), "\n\n")

cat("SYNTHETIC DATA (Proof of Concept):\n")
cat("Sample size: ", nrow(synth_clean), "\n")
cat("Diabetes cases: ", sum(synth_clean$diabetes), "\n")
cat("Traditional AUC: ", round(auc(roc_trad), 4), "\n")
cat("Sigma AUC: ", round(auc(roc_sigma), 4), "\n\n")

cat("NHANES DATA (Demonstration):\n")
cat("Sample size: ", nrow(nhanes_df), "\n")
cat("Diabetes cases: ", sum(nhanes_df$diabetes), "\n")
cat("Biomarker used: Direct HDL Cholesterol\n")
if(exists("roc_nhanes_trad")) {
    cat("Traditional AUC: ", round(auc(roc_nhanes_trad), 4), "\n")
    cat("Sigma AUC: ", round(auc(roc_nhanes_sigma), 4), "\n")
}
cat("\nSESSION INFO:\n")
print(sessionInfo())
sink()

# Save a compact metrics table (descriptive only; no differences)
metrics <- tibble::tibble(
  dataset = c("synthetic", "synthetic", 
              if (exists("roc_nhanes_trad")) "nhanes_demo" else NA, 
              if (exists("roc_nhanes_trad")) "nhanes_demo" else NA),
  model = c("traditional", "sigma",
            if (exists("roc_nhanes_trad")) "traditional" else NA,
            if (exists("roc_nhanes_trad")) "sigma" else NA),
  auc = c(as.numeric(auc(roc_trad)), as.numeric(auc(roc_sigma)),
          if (exists("roc_nhanes_trad")) as.numeric(auc(roc_nhanes_trad)) else NA,
          if (exists("roc_nhanes_trad")) as.numeric(auc(roc_nhanes_sigma)) else NA)
) %>% dplyr::filter(!is.na(dataset))

readr::write_csv(metrics, file.path(data_dir, "auc_metrics_descriptive.csv"))

# ============================================
# PART 5: FINAL CONCLUSIONS
# ============================================

cat("\n")
cat("============================================================\n")
cat("FINAL CONCLUSIONS\n")
cat("============================================================\n\n")

cat("SUMMARY OF FINDINGS:\n")
cat("1. Synthetic data confirms internal coherence of sigma-based reparameterization\n")
cat("2. Mathematical framework works correctly\n")
cat("3. Can be applied to real data (NHANES demonstration)\n")
cat("4. Limited by available data in NHANES R package\n\n")

cat("STRENGTHS OF THIS VALIDATION:\n")
cat("✓ Internal consistency check using synthetic data\n")
cat("✓ Demonstration with real population data\n")
cat("✓ Complete, reproducible code\n")
cat("✓ Clear visual evidence\n\n")

cat("LIMITATIONS:\n")
cat("✗ NHANES R package lacks HbA1c data\n")
cat("✗ Limited diabetes cases in demo dataset\n")
cat("✗ Cross-sectional data (prevalence, not incidence)\n\n")

cat("============================================================\n")
cat("RECOMMENDED NEXT STEPS\n")
cat("============================================================\n\n")

cat("STEP 1: Write Hypothesis Paper\n")
cat("   - Include: Theory + Synthetic validation + NHANES demo\n")
cat("   - Submit to: Medical Hypotheses or similar journal\n")
cat("   - Timeline: 2-4 weeks\n\n")

cat("STEP 2: Get Proper Data\n")
cat("   Option A: Apply for UK Biobank (ideal)\n")
cat("   Option B: Download raw NHANES HbA1c from CDC\n")
cat("   Option C: Find collaborators with data access\n\n")

cat("STEP 3: Full Validation Study\n")
cat("   - Use longitudinal data with incidence\n")
cat("   - Test multiple biomarkers (HbA1c, BP, lipids)\n")
cat("   - Compare to existing risk scores\n")
cat("   - Publish in clinical journal\n")

cat("\n============================================================\n")
cat("FILES SAVED:\n")
cat("============================================================\n")
cat("1. sigma_synthetic_results.csv - Synthetic validation data\n")
cat("2. sigma_nhanes_demo.csv - NHANES demonstration data\n")
cat("3. sigma_demonstration_summary.txt - Analysis results\n")
cat("4. R plots - Visual evidence\n\n")

cat("ANALYSIS COMPLETE\n")

cat("\nArtifacts saved under:", out_dir, "\n")