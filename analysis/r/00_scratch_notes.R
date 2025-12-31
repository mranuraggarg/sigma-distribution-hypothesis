# ============================================
# SIGMA-DISTRIBUTION HYPOTHESIS VALIDATION
# FINAL WORKING CODE WITH CORRECT VARIABLE NAMES
# ============================================

# 1. INSTALL PACKAGES (run once if needed)
# install.packages(c("tidyverse", "NHANES", "pROC", "survival", "gtsummary"))

# 2. LOAD LIBRARIES
library(tidyverse)
library(NHANES)      # Pre-cleaned NHANES data
library(pROC)        # ROC analysis
library(survival)    # Survival analysis

# 3. CHECK AVAILABLE VARIABLES IN NHANES PACKAGE
cat("Checking NHANES variable names...\n")
cat("Total variables:", ncol(NHANES), "\n")
cat("\nFirst few variables:\n")
print(names(NHANES)[1:20])

# Look for HbA1c variable
cat("\nLooking for HbA1c variable...\n")
hba1c_vars <- grep("A1C|HbA1c|glyco", names(NHANES), ignore.case = TRUE, value = TRUE)
cat("Possible HbA1c variables:", paste(hba1c_vars, collapse = ", "), "\n")

# Look for diabetes variables
diab_vars <- grep("diab|glucose", names(NHANES), ignore.case = TRUE, value = TRUE)
cat("Possible diabetes variables:", paste(diab_vars, collapse = ", "), "\n")

# Look for blood pressure variables
bp_vars <- grep("bp|blood.pressure", names(NHANES), ignore.case = TRUE, value = TRUE)
cat("Possible BP variables:", paste(bp_vars, collapse = ", "), "\n")

# ============================================
# PART 1: SYNTHETIC DATA PROOF-OF-CONCEPT
# ============================================

cat("\n\n=== PART 1: SYNTHETIC DATA VALIDATION ===\n\n")

set.seed(123)
n <- 2000  # Reasonable sample size

# Create realistic synthetic data - STEP BY STEP
synthetic_data <- data.frame(
    id = 1:n,
    age = round(runif(n, 30, 80)),  # Ages 30-80
    sex = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52))
)

# Add BMI
synthetic_data$bmi <- pmax(pmin(rnorm(n, mean = 28, sd = 5), 45), 18)

# Now calculate HbA1c components
synthetic_data$hba1c_base <- 4.5 + 0.025*(synthetic_data$age - 30)
synthetic_data$hba1c_noise <- rnorm(n, sd = 0.4)
synthetic_data$hba1c_sex_effect <- ifelse(synthetic_data$sex == "Male", 0.15, 0)
synthetic_data$hba1c_raw <- with(synthetic_data, 
                                 hba1c_base + hba1c_noise + hba1c_sex_effect)

# Add diabetes events (probability increases with age and HbA1c)
synthetic_data <- synthetic_data %>%
    mutate(
        log_odds = -8 + 0.08*age + 0.5*(hba1c_raw - 5)*2,
        diabetes_prob = 1/(1 + exp(-log_odds)),
        diabetes_event = rbinom(n, 1, diabetes_prob)
    )

# Calculate age-specific distributions (5-year groups)
synthetic_data <- synthetic_data %>%
    mutate(
        age_group = cut(age, 
                        breaks = seq(30, 85, by = 5),
                        labels = paste0(seq(30, 80, by = 5), "-", seq(34, 84, by = 5))),
        age_group = as.character(age_group)
    ) %>%
    group_by(age_group) %>%
    mutate(
        mu_hba1c = mean(hba1c_raw, na.rm = TRUE),
        sd_hba1c = sd(hba1c_raw, na.rm = TRUE),
        sigma_position = (hba1c_raw - mu_hba1c) / sd_hba1c
    ) %>%
    ungroup()

# Remove any missing or extreme values
synthetic_clean <- synthetic_data %>%
    filter(!is.na(sigma_position), 
           abs(sigma_position) < 5,  # Remove extreme outliers
           !is.na(diabetes_event))

cat("Synthetic data created successfully!\n")
cat("Sample size:", nrow(synthetic_clean), "\n")
cat("Diabetes prevalence:", 
    round(100 * mean(synthetic_clean$diabetes_event), 1), "%\n\n")

# Model 1: Traditional (raw HbA1c)
model_trad <- glm(diabetes_event ~ hba1c_raw + age + sex + bmi,
                  data = synthetic_clean, family = binomial)

# Model 2: Sigma position
model_sigma <- glm(diabetes_event ~ sigma_position + age + sex + bmi,
                   data = synthetic_clean, family = binomial)

# Compare AUC
roc_trad <- roc(synthetic_clean$diabetes_event, 
                predict(model_trad, type = "response"))
roc_sigma <- roc(synthetic_clean$diabetes_event, 
                 predict(model_sigma, type = "response"))

cat("=== SYNTHETIC DATA RESULTS ===\n")
cat("Traditional model AUC:", round(auc(roc_trad), 4), "\n")
cat("Sigma model AUC:", round(auc(roc_sigma), 4), "\n")
cat("Difference (Sigma - Traditional):", 
    round(auc(roc_sigma) - auc(roc_trad), 4), "\n\n")

# Statistical test for AUC difference
roc_test <- roc.test(roc_trad, roc_sigma)
cat("ROC comparison p-value:", format.pval(roc_test$p.value, digits = 3), "\n")

if(roc_test$p.value < 0.05) {
    cat("✅ Statistically significant difference (p < 0.05)\n")
} else {
    cat("⚠️  No statistically significant difference\n")
}

# ============================================
# PART 2: REAL NHANES DATA ANALYSIS WITH CORRECT VARIABLES
# ============================================

cat("\n\n=== PART 2: REAL NHANES DATA ANALYSIS ===\n\n")

# Load NHANES data
data(NHANES)

# Check the structure to see actual variable names
cat("NHANES dataset structure:\n")
str(NHANES[, 1:10])  # First 10 columns

# Based on the NHANES package documentation, let's use:
# DirectChol = Direct HDL Cholesterol
# TotChol = Total Cholesterol
# BPSysAve = Systolic BP average
# BPDiaAve = Diastolic BP average
# BMI = Body Mass Index
# Diabetes = "Has a doctor ever told you that you have diabetes?"
# PhysActive = Physical activity

# Let me check what's actually available
cat("\nAvailable variables in NHANES:\n")
print(names(NHANES))

# The NHANES package doesn't have HbA1c! Let's check:
# install.packages("NHANES") and look at documentation
# Actually, let's use what IS available: DirectChol and Diabetes

# Use alternative approach: Cholesterol as biomarker
df <- NHANES %>%
    # Select available variables
    select(ID, SurveyYr, Gender, Age, Race1, BMI, 
           Diabetes, DirectChol, TotChol, BPSysAve, BPDiaAve) %>%
    # Filter for analysis
    filter(
        Age >= 30,                    # Adults 30+
        !is.na(DirectChol),          # Has cholesterol measurement
        !is.na(Diabetes),            # Known diabetes status
        !is.na(BMI),                 # Has BMI
        DirectChol >= 20 & DirectChol <= 200  # Physiologically plausible range
    ) %>%
    # Code diabetes outcome
    mutate(
        diabetes_event = ifelse(Diabetes == "Yes", 1, 0),
        # Create 5-year age groups
        age_group = cut(Age, 
                        breaks = seq(30, 85, by = 5),
                        labels = paste0(seq(30, 80, by = 5), "-", seq(34, 84, by = 5))),
        age_group = as.character(age_group),
        # For demonstration, use cholesterol as biomarker
        biomarker = DirectChol
    )

cat("NHANES sample size:", nrow(df), "\n")
cat("Diabetes prevalence:", 
    round(100 * mean(df$diabetes_event), 1), "%\n")

# Calculate sigma position for cholesterol
df <- df %>%
    group_by(age_group) %>%
    mutate(
        mu_biomarker = mean(biomarker, na.rm = TRUE),
        sd_biomarker = sd(biomarker, na.rm = TRUE),
        sigma_position = (biomarker - mu_biomarker) / sd_biomarker
    ) %>%
    ungroup()

# Clean data - remove extremes
df_clean <- df %>%
    filter(
        !is.na(sigma_position),
        abs(sigma_position) < 4,      # Remove extreme z-scores
        !is.na(diabetes_event),
        !is.na(BMI)
    )

cat("\nAfter cleaning:\n")
cat("Final sample size:", nrow(df_clean), "\n")
cat("Diabetes cases:", sum(df_clean$diabetes_event), "\n\n")

# ============================================
# MODEL COMPARISON ON NHANES DATA
# ============================================

if(sum(df_clean$diabetes_event) >= 20) {  # Need enough cases
    
    # Model 1: Traditional approach (raw cholesterol)
    model_nhanes_trad <- glm(diabetes_event ~ biomarker + Age + Gender + BMI,
                             data = df_clean, family = binomial)
    
    # Model 2: Sigma position approach
    model_nhanes_sigma <- glm(diabetes_event ~ sigma_position + Age + Gender + BMI,
                              data = df_clean, family = binomial)
    
    # Calculate predictions
    df_clean <- df_clean %>%
        mutate(
            pred_trad = predict(model_nhanes_trad, type = "response"),
            pred_sigma = predict(model_nhanes_sigma, type = "response")
        )
    
    # ROC analysis
    roc_nhanes_trad <- roc(df_clean$diabetes_event, df_clean$pred_trad)
    roc_nhanes_sigma <- roc(df_clean$diabetes_event, df_clean$pred_sigma)
    
    cat("=== NHANES RESULTS (Using Cholesterol) ===\n")
    cat("Biomarker: Direct HDL Cholesterol\n")
    cat("Traditional model AUC:", round(auc(roc_nhanes_trad), 4), "\n")
    cat("Sigma model AUC:", round(auc(roc_nhanes_sigma), 4), "\n")
    cat("Difference (Sigma - Traditional):", 
        round(auc(roc_nhanes_sigma) - auc(roc_nhanes_trad), 4), "\n")
    
    # Statistical test
    if(sum(df_clean$diabetes_event) >= 50) {  # Need enough events for valid test
        roc_test_nhanes <- roc.test(roc_nhanes_trad, roc_nhanes_sigma)
        cat("ROC comparison p-value:", 
            format.pval(roc_test_nhanes$p.value, digits = 3), "\n")
        
        if(roc_test_nhanes$p.value < 0.05) {
            cat("✅ Statistically significant in NHANES data!\n")
        } else {
            cat("⚠️  Not statistically significant in NHANES data\n")
        }
    }
    
    # ============================================
    # CREATE SUMMARY TABLE
    # ============================================
    
    cat("\n=== MODEL COEFFICIENTS ===\n")
    
    # Traditional model summary
    cat("\nTraditional Model (Cholesterol raw):\n")
    print(round(summary(model_nhanes_trad)$coefficients, 4))
    
    cat("\nSigma Model (z-score):\n")
    print(round(summary(model_nhanes_sigma)$coefficients, 4))
    
} else {
    cat("⚠️  Too few diabetes cases for meaningful analysis\n")
    cat("Using cholesterol instead of HbA1c for demonstration\n")
}

# ============================================
# PART 3: VISUALIZATIONS
# ============================================

cat("\n=== GENERATING VISUALIZATIONS ===\n")

# Plot 1: Age vs Cholesterol with sigma bands
p1 <- ggplot(df_clean, aes(x = Age, y = biomarker)) +
    geom_point(alpha = 0.1, size = 0.5, color = "gray50") +
    geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 1) +
    geom_smooth(aes(y = mu_biomarker), method = "loess", se = FALSE, 
                color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = "Cholesterol vs Age with Mean Trajectory",
         subtitle = "Blue: LOESS smooth | Red: Age-group means",
         x = "Age (years)", y = "Direct HDL Cholesterol (mg/dL)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

# Plot 2: Distribution of sigma positions
p2 <- ggplot(df_clean, aes(x = sigma_position)) +
    geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7, 
                   color = "white") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    geom_vline(xintercept = c(-1, 1), linetype = "dotted", color = "gray") +
    annotate("text", x = -2.5, y = Inf, label = "Left Tail\n(Earlier expected onset)", 
             vjust = 1.5, hjust = 0, color = "darkred", size = 3) +
    annotate("text", x = 2.5, y = Inf, label = "Right Tail\n(Later expected onset)", 
             vjust = 1.5, hjust = 1, color = "darkgreen", size = 3) +
    labs(title = "Distribution of Sigma Positions (Cholesterol)",
         subtitle = paste("Mean =", round(mean(df_clean$sigma_position, na.rm = TRUE), 2),
                          "SD =", round(sd(df_clean$sigma_position, na.rm = TRUE), 2)),
         x = "Sigma Position (z-score)", y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

# Plot 3: ROC curves comparison (if models exist)
if(exists("roc_nhanes_trad") && exists("roc_nhanes_sigma")) {
    roc_data <- data.frame(
        Specificity = c(1 - roc_nhanes_trad$specificities, 
                        1 - roc_nhanes_sigma$specificities),
        Sensitivity = c(roc_nhanes_trad$sensitivities, 
                        roc_nhanes_sigma$sensitivities),
        Model = rep(c("Traditional (Cholesterol raw)", "Sigma (z-score)"), 
                    c(length(roc_nhanes_trad$sensitivities),
                      length(roc_nhanes_sigma$sensitivities)))
    )
    
    p3 <- ggplot(roc_data, aes(x = Specificity, y = Sensitivity, color = Model)) +
        geom_line(linewidth = 1) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
                    alpha = 0.5, color = "gray") +
        scale_color_manual(values = c("Traditional (Cholesterol raw)" = "gray40", 
                                      "Sigma (z-score)" = "red")) +
        labs(title = "ROC Curve Comparison",
             subtitle = paste("AUC: Traditional =", round(auc(roc_nhanes_trad), 3),
                              "| Sigma =", round(auc(roc_nhanes_sigma), 3)),
             x = "1 - Specificity (False Positive Rate)",
             y = "Sensitivity (True Positive Rate)") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"),
              legend.position = "bottom")
}

# Plot 4: Diabetes probability vs sigma position
p4 <- ggplot(df_clean, aes(x = sigma_position, y = diabetes_event)) +
    geom_point(alpha = 0.1, position = position_jitter(height = 0.02)) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"),
                color = "red", fill = "red", alpha = 0.2) +
    labs(title = "Diabetes Probability vs Sigma Position",
         subtitle = "Using Cholesterol as Biomarker",
         x = "Sigma Position (z-score)", 
         y = "Diabetes (0=No, 1=Yes)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

# Display plots
print(p1)
print(p2)
print(p4)
if(exists("p3")) print(p3)

# ============================================
# PART 4: SAVE RESULTS
# ============================================

# Save the processed data
write_csv(df_clean, "sigma_validation_nhanes_results.csv")

# Save model summaries
sink("sigma_validation_summary.txt")
cat("Sigma-Distribution Hypothesis Validation\n")
cat("========================================\n")
cat("Analysis date:", format(Sys.Date(), "%Y-%m-%d"), "\n")
cat("\nNOTE: Using Cholesterol as biomarker (HbA1c not in NHANES package)\n")
cat("\nSYNTHETIC DATA:\n")
cat("Traditional AUC:", round(auc(roc_trad), 4), "\n")
cat("Sigma AUC:", round(auc(roc_sigma), 4), "\n")
cat("Difference:", round(auc(roc_sigma) - auc(roc_trad), 4), "\n")
cat("P-value:", format.pval(roc_test$p.value, digits = 3), "\n")

cat("\nNHANES DATA:\n")
cat("Biomarker used: Direct HDL Cholesterol\n")
cat("Sample size:", nrow(df_clean), "\n")
cat("Diabetes cases:", sum(df_clean$diabetes_event), "\n")
if(exists("roc_nhanes_trad")) {
    cat("Traditional AUC:", round(auc(roc_nhanes_trad), 4), "\n")
    cat("Sigma AUC:", round(auc(roc_nhanes_sigma), 4), "\n")
    cat("Difference:", round(auc(roc_nhanes_sigma) - auc(roc_nhanes_trad), 4), "\n")
    if(exists("roc_test_nhanes")) {
        cat("P-value:", format.pval(roc_test_nhanes$p.value, digits = 3), "\n")
    }
}
sink()

# ============================================
# PART 5: INTERPRETATION AND NEXT STEPS
# ============================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("FINAL INTERPRETATION\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

if(exists("roc_nhanes_sigma")) {
    diff_auc <- auc(roc_nhanes_sigma) - auc(roc_nhanes_trad)
    
    if(diff_auc > 0.03) {
        cat("🎯 STRONG EMPIRICAL SUPPORT FOR YOUR HYPOTHESIS!\n")
        cat("   Sigma positioning improves prediction by", round(100*diff_auc, 1), 
            "AUC percentage points.\n")
        cat("   This suggests your framework has real predictive value.\n\n")
        
    } else if(diff_auc > 0.01) {
        cat("⚠️  MODEST SUPPORT\n")
        cat("   Sigma is slightly better, but not dramatically.\n\n")
        
    } else if(diff_auc > 0) {
        cat("🔍 MINIMAL DIFFERENCE\n")
        cat("   Sigma performs similarly to traditional approach.\n\n")
        
    } else {
        cat("❌ NO EMPIRICAL ADVANTAGE IN PREDICTION\n")
        cat("   Traditional method performs as well or better.\n\n")
    }
} else {
    cat("📊 INSUFFICIENT DATA FOR CONCLUSION\n")
    cat("   Need more diabetes cases for reliable analysis.\n\n")
}

cat("IMPORTANT LIMITATION:\n")
cat("The NHANES R package doesn't include HbA1c data.\n")
cat("We used cholesterol as a demonstration biomarker.\n")
cat("For HbA1c validation, you need to download raw NHANES data from CDC.\n")

cat("\nNEXT STEPS FOR PROPER VALIDATION:\n")
cat("1. Download NHANES HbA1c data from: https://wwwn.cdc.gov/nchs/nhanes/\n")
cat("2. Use datasets: 'GHB_J.XPT' (2017-2018) for HbA1c\n")
cat("3. Merge with demographic data 'DEMO_J.XPT'\n")
cat("4. Or use UK Biobank (has HbA1c and longitudinal diabetes incidence)\n")

cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Files saved:\n")
cat("1. sigma_validation_nhanes_results.csv - Clean dataset\n")
cat("2. sigma_validation_summary.txt - Analysis results\n")
cat("3. R plots - Visual evidence\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# ============================================
# BONUS: HOW TO GET ACTUAL HBA1C DATA
# ============================================

cat("\n\n=== HOW TO GET REAL HBA1C DATA ===\n")
cat("1. Go to: https://wwwn.cdc.gov/nchs/nhanes/\n")
cat("2. Download these files for 2017-2018:\n")
cat("   - DEMO_J.XPT (Demographics)\n")
cat("   - GHB_J.XPT (Glycohemoglobin/HbA1c)\n")
cat("   - DIQ_J.XPT (Diabetes questionnaire)\n")
cat("3. Load in R with: foreign::read.xport('GHB_J.XPT')\n")
cat("4. Merge datasets by SEQN (subject ID)\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("You now have proof-of-concept with synthetic data\n")
cat("and demonstration with real cholesterol data.\n")
cat("Next: Get real HbA1c data for proper validation.\n")