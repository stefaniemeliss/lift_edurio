#### ALL-PURPOSE HELPER FUNCTIONS ####

# source code
source_code(target_repo = "helper_functions", file_name = "functions.R")

#### PROJECT-SPECIFIC FUNCTIONS ####

# functions for data processing #

# Function to insert "/" in the format "YYYYYY" to "YYYY/YY"
insert_slash <- function(number) {
  sub("(\\d{4})(\\d{2})", "\\1/\\2", number)
}

# Function to review column lookup table mappings
review_lookup_mappings <- function(lookup_table = column_lookup) {
  cat("=== COLUMN LOOKUP TABLE REVIEW ===\n\n")
  
  for (i in 1:nrow(lookup_table)) {
    standard <- lookup_table$standard_name[i]
    variations <- lookup_table$variations[[i]]
    
    cat(sprintf("Standard Name %d of %d:\n", i, nrow(lookup_table)))
    cat("Standard: ", standard, "\n")
    cat("Variations (", length(variations), "):\n")
    
    for (j in 1:length(variations)) {
      cat("  ", j, ". ", variations[j], "\n", sep = "")
    }
    cat("\n", paste(rep("-", 80), collapse = ""), "\n\n")
  }
}

# Create the reverse lookup function
# transforms column lookup table from its current structure into a format 
# that's optimized for fast column name standardisation.
create_reverse_lookup <- function(lookup_table) {
  reverse_lookup <- list()
  
  for (i in 1:nrow(lookup_table)) {
    standard <- lookup_table$standard_name[i]
    variations <- lookup_table$variations[[i]]
    
    for (var in variations) {
      reverse_lookup[[var]] <- standard
    }
  }
  
  return(reverse_lookup)
}

# Enhanced function that only keeps columns included in the lookup table
# core data pipeline processor: 
## 1. Column Filtering & Selection
## 2. Column Name Standardisation
## 3. Duplicate Detection & Handling
## 4. Quality Control & Reporting
standardise_column_names <- function(df, lookup = reverse_lookup) {
  current_names <- names(df)
  
  # Get all possible column names that are in the lookup (variations that can be mapped)
  lookup_columns <- names(lookup)
  
  # Identify which columns from the dataframe are in the lookup
  columns_to_keep <- current_names[current_names %in% lookup_columns]
  
  if (length(columns_to_keep) == 0) {
    warning("No columns found that match the lookup table")
    return(df[, FALSE])  # Return empty dataframe
  }
  
  # Filter dataframe to only keep columns that are in the lookup
  df_filtered <- df[, columns_to_keep, drop = FALSE]
  
  # Now standardize the column names
  new_names <- names(df_filtered)
  for (i in 1:length(new_names)) {
    old_name <- new_names[i]
    if (old_name %in% names(lookup)) {
      new_names[i] <- lookup[[old_name]]
    }
  }
  
  # Check for duplicates BEFORE renaming
  if (any(duplicated(new_names))) {
    duplicate_names <- new_names[duplicated(new_names)]
    cat("WARNING: The following duplicate column names would be created:\n")
    print(duplicate_names)
    
    # Show which original columns are causing the duplicates
    for (dup_name in unique(duplicate_names)) {
      original_cols <- names(df_filtered)[new_names == dup_name]
      cat(paste("Columns mapping to", dup_name, ":", paste(original_cols, collapse = ", "), "\n"))
    }
    
    # Handle duplicates by keeping only the first occurrence and renaming others
    for (dup_name in unique(duplicate_names)) {
      dup_indices <- which(new_names == dup_name)
      if (length(dup_indices) > 1) {
        # Keep the first one, modify the others
        for (j in 2:length(dup_indices)) {
          new_names[dup_indices[j]] <- paste0(new_names[dup_indices[j]], "_", j-1)
        }
      }
    }
  }
  
  # Rename columns
  names(df_filtered) <- new_names
  
  # Report what happened
  original_count <- length(current_names)
  kept_count <- length(columns_to_keep)
  dropped_count <- original_count - kept_count
  
  cat(paste("Year:", ifelse(exists("academic_year"), academic_year, "Unknown"), "\n"))
  cat(paste("Original columns:", original_count, "\n"))
  cat(paste("Columns kept (in lookup):", kept_count, "\n"))
  cat(paste("Columns dropped (not in lookup):", dropped_count, "\n"))
  
  # Report column name changes
  changes <- data.frame(
    old_name = columns_to_keep[columns_to_keep != new_names],
    new_name = new_names[columns_to_keep != new_names],
    stringsAsFactors = FALSE
  )
  
  if (nrow(changes) > 0) {
    cat("Column name changes made:\n")
    print(changes)
  } else {
    cat("No column name changes needed.\n")
  }
  
  # Show which columns were dropped (for reference)
  dropped_columns <- current_names[!current_names %in% columns_to_keep]
  if (length(dropped_columns) > 0 && length(dropped_columns) <= 10) {
    cat("Dropped columns:", paste(dropped_columns, collapse = ", "), "\n")
  } else if (length(dropped_columns) > 10) {
    cat("Dropped columns (first 10):", paste(dropped_columns[1:10], collapse = ", "), "... and", length(dropped_columns) - 10, "more\n")
  }
  
  cat("\n")
  
  return(df_filtered)
}

# Function to produce histograms by group
plot_histogram <- function(data = df, 
                           xvar = x,
                           xlower = NULL,
                           xupper = NULL,
                           title = "",
                           xlab = "",
                           bin_width = NULL) {
  plot <- 
    # define variables to plot based on input
    ggplot(data, aes(x = get(xvar))) +
    # basic histogram with density
    geom_histogram(aes(y = after_stat(density)), color = nondominant_col, fill = nondominant_col, bins = 30, binwidth = bin_width) +
    # add density plot
    geom_density(alpha = .2, fill = dominant_col, col = dominant_col) +
    # add mean as vertical line
    geom_vline(aes(xintercept = mean(get(xvar), na.rm = T)),
               color = dominant_col, linetype = "dashed", linewidth = 1) +
    # add theme
    ambition_theme +
    # determine titles
    ggtitle(paste0(title)) + xlab(paste0(xlab)) + ylab("Density") 
  
  
  # determine coord system + scales
  if (!is.null(xlower) | !is.null(xupper)) {
    
    #  modify x axis
    plot <- plot + coord_cartesian(xlim = c(xlower, xupper))
  }
  return(plot)
  
}

# Helper function to check if a variable is binary (0/1)
is_binary <- function(x) {
  vals <- na.omit(unique(x))
  length(vals) == 2 && all(vals %in% c(0, 1))
}

# Main function
cluster_corr_matrix <- function(df, cluster_vars, standardise = FALSE) {
  # Identify numeric variables in the data frame
  num_vars <- names(df)[sapply(df, is.numeric)]
  n_vars <- length(num_vars)
  
  # Initialise matrices for correlations, p-values, and n_obs
  cor_mat <- matrix(NA, nrow = n_vars, ncol = n_vars)
  p_mat <- matrix(NA, nrow = n_vars, ncol = n_vars)
  n_mat <- matrix(NA, nrow = n_vars, ncol = n_vars)
  
  # Set row and column names
  rownames(cor_mat) <- colnames(cor_mat) <- num_vars
  rownames(p_mat) <- colnames(p_mat) <- num_vars
  rownames(n_mat) <- colnames(n_mat) <- num_vars
  
  # Optionally standardise non-binary numeric variables
  df_std <- df
  if (standardise) {
    for (v in num_vars) {
      if (!is_binary(df[[v]])) {
        df_std[[v]] <- scale(df[[v]])
      }
    }
  }
  
  # Loop over all pairs of numeric variables
  for (i in seq_len(n_vars)) {
    for (j in seq_len(n_vars)) {
      x <- df_std[[num_vars[i]]]
      y <- df_std[[num_vars[j]]]
      cl1 <- df[[cluster_vars[1]]]
      cl2 <- df[[cluster_vars[2]]]
      
      # Identify complete cases for this pair and clustering variables
      complete <- complete.cases(x, y, cl1, cl2)
      n_obs <- sum(complete)
      
      # Only fill upper triangle and diagonal (matrix is symmetric)
      if (j >= i) {
        if (n_obs > 2) {
          # Prepare data frame of complete cases
          df_sub <- data.frame(
            x = x[complete],
            y = y[complete],
            cl1 = cl1[complete],
            cl2 = cl2[complete]
          )
          df_sub$cl3 <- interaction(df_sub$cl1, df_sub$cl2)
          
          # Check if both variables are binary
          bin_x <- is_binary(df_sub$x)
          bin_y <- is_binary(df_sub$y)
          
          if (bin_x && bin_y) {
            # Binary-binary: use phi coefficient and chi-squared p-value
            tab <- table(df_sub$x, df_sub$y)
            cor_val <- suppressWarnings(cor(df_sub$x, df_sub$y))
            p_val <- suppressWarnings(chisq.test(tab)$p.value)
          } else {
            # Otherwise: use regression and cluster-robust SEs for p-value
            model <- lm(y ~ x, data = df_sub)
            cluster_list <- list(df_sub$cl1, df_sub$cl2, df_sub$cl3)
            vcov_cl <- sandwich::vcovCL(model, cluster = cluster_list)
            test <- lmtest::coeftest(model, vcov_cl)
            cor_val <- suppressWarnings(cor(df_sub$x, df_sub$y))
            p_val <- test["x", "Pr(>|t|)"]
          }
        } else {
          # Not enough data to compute correlation
          cor_val <- NA
          p_val <- NA
        }
        # Fill both upper and lower triangle for symmetry
        cor_mat[i, j] <- cor_val
        p_mat[i, j] <- p_val
        n_mat[i, j] <- n_obs
        cor_mat[j, i] <- cor_val
        p_mat[j, i] <- p_val
        n_mat[j, i] <- n_obs
      }
    }
  }
  
  # --- Adjust p-values for multiple testing in the upper triangle only ---
  upper_mask <- upper.tri(p_mat, diag = FALSE)
  pvals_vec <- p_mat[upper_mask]
  # Benjamini-Hochberg FDR adjustment
  pvals_adj_vec <- p.adjust(pvals_vec, method = "BH")
  # Build matrix of adjusted p-values
  pvals_adj_mat <- matrix(NA, nrow = nrow(p_mat), ncol = ncol(p_mat))
  rownames(pvals_adj_mat) <- rownames(p_mat)
  colnames(pvals_adj_mat) <- colnames(p_mat)
  pvals_adj_mat[upper_mask] <- pvals_adj_vec
  
  # --- Filtered correlation matrix: only significant correlations in upper triangle ---
  filtered_cor_mat <- cor_mat
  # Set upper triangle to NA where adjusted p >= 0.05
  filtered_cor_mat[upper_mask & !(pvals_adj_mat < 0.05)] <- NA
  # Optionally set diagonal to NA (uncomment if desired)
  # diag(filtered_cor_mat) <- NA
  
  # --- Add sample size (n) to row and column names ---
  # For each outcome (column), get the first non-missing n_obs value
  n_per_outcome <- apply(n_mat, 2, function(x) x[which(!is.na(x))[1]])
  new_names <- paste0(colnames(filtered_cor_mat), " (n = ", n_per_outcome, ")")
  colnames(filtered_cor_mat) <- new_names
  rownames(filtered_cor_mat) <- new_names
  
  # --- Return all outputs as a list ---
  return(list(
    correlation = cor_mat,            # Raw correlation matrix
    p_value = p_mat,                  # Raw p-value matrix
    n_obs = n_mat,                    # Matrix of pairwise n_obs
    correlation_filtered = filtered_cor_mat # Filtered correlation matrix with n in row/col names
  ))
}

# Helper function for residuals-based partial correlation
partial_correlation_residuals <- function(df, outcome, predictor, controls) {
  formula_y <- as.formula(
    paste(outcome, "~", paste(controls, collapse = " + "))
  )
  formula_x <- as.formula(
    paste(predictor, "~", paste(controls, collapse = " + "))
  )
  resid_y <- residuals(lm(formula_y, data = df))
  resid_x <- residuals(lm(formula_x, data = df))
  cor(resid_y, resid_x)
}

get_betas_cluster <- function(df, outcome, predictors, fe_vars, cluster_vars, binary_outcome = TRUE) {
  
  
  # Build formulas
  if (binary_outcome) {
    formula_unstd <- as.formula(
      paste0(outcome, " ~ ",
             paste0(predictors, collapse = " + "),
             if (!is.null(fe_vars)) paste0(" + ", paste(fe_vars, collapse = " + ")) else "")
    )
    formula_std <- as.formula(
      paste0(outcome, " ~ ",
             paste0("scale(", predictors, ")", collapse = " + "),
             if (!is.null(fe_vars)) paste0(" + ", paste(fe_vars, collapse = " + ")) else "")
    )
    formula_null <- as.formula(paste0(outcome, " ~ 1"))
  } else {
    formula_unstd <- as.formula(
      paste0(outcome, " ~ ",
             paste0(predictors, collapse = " + "),
             if (!is.null(fe_vars)) paste0(" + ", paste(fe_vars, collapse = " + ")) else "")
    )
    formula_std <- as.formula(
      paste0("scale(", outcome, ") ~ ",
             paste0("scale(", predictors, ")", collapse = " + "),
             if (!is.null(fe_vars)) paste0(" + ", paste(fe_vars, collapse = " + ")) else "")
    )
    
  }
  
  # Fit models
  if (binary_outcome) {
    model_unstd <- glm(formula_unstd, data = df, family = binomial())
    model_std   <- glm(formula_std,   data = df, family = binomial())
    # Likelihood ratio test
    model_null   <- glm(formula_null,   data = df, family = binomial())
    lrt <- anova(model_null, model_unstd, test = "Chisq")
  } else {
    model_unstd <- lm(formula_unstd, data = df)
    model_std   <- lm(formula_std,   data = df)
  }
  
  # Multiway cluster-robust SEs: school, year, school-year pair
  cluster_list <- list(df[[cluster_vars[1]]], df[[cluster_vars[2]]], interaction(df[[cluster_vars[1]]], df[[cluster_vars[2]]]))
  vcov_unstd <- sandwich::vcovCL(model_unstd, cluster = cluster_list)
  vcov_std   <- sandwich::vcovCL(model_std,   cluster = cluster_list)
  
  # Raw coefficients
  coefs_unstd <- summary(model_unstd)$coefficients
  coefs_std   <- summary(model_std)$coefficients
  robust_unstd <- lmtest::coeftest(model_unstd, vcov. = vcov_unstd)
  robust_std   <- lmtest::coeftest(model_std,   vcov. = vcov_std)
  
  # Names for standardised predictors in model
  preds_std_names <- paste0("scale(", predictors, ")")
  
  # Extract rows for predictors only
  unstd_rows <- rownames(coefs_unstd) %in% predictors
  std_rows   <- rownames(coefs_std)   %in% preds_std_names
  robust_unstd_rows <- rownames(robust_unstd) %in% predictors
  robust_std_rows   <- rownames(robust_std)   %in% preds_std_names
  
  # For continuous outcomes, calculate partial correlations using residuals
  if (length(predictors) > 1 & !binary_outcome) {
    # Controls for partial correlation: all other predictors and fixed effects
    controls <- c(setdiff(predictors, predictors), fe_vars) # placeholder, will be replaced below
    pcor_resid <- sapply(predictors, function(pred) {
      controls <- c(setdiff(predictors, pred), fe_vars)
      partial_correlation_residuals(df, outcome, pred, controls)
    })
  } else { pcor_resid = NA}
  
  # Combine into one table in your requested order
  if (binary_outcome) {
    result_table <- data.frame(
      outcome = outcome,
      predictor = predictors,
      # Log odds and odds ratios
      Log_odds_unstd = coefs_unstd[unstd_rows, "Estimate"],
      Log_odds_std   = coefs_std[std_rows, "Estimate"],
      OR_unstd = exp(coefs_unstd[unstd_rows, "Estimate"]),
      OR_std   = exp(coefs_std[std_rows, "Estimate"]),
      # SEs
      SE_unstd       = coefs_unstd[unstd_rows, "Std. Error"],
      SE_unstd_CR    = robust_unstd[robust_unstd_rows, "Std. Error"],
      SE_std         = coefs_std[std_rows, "Std. Error"],
      SE_std_CR      = robust_std[robust_std_rows, "Std. Error"],
      # z values
      z_unstd        = coefs_unstd[unstd_rows, "z value"],
      z_unstd_CR     = robust_unstd[robust_unstd_rows, "z value"],
      z_std          = coefs_std[std_rows, "z value"],
      z_std_CR       = robust_std[robust_std_rows, "z value"],
      # p values
      p_unstd        = coefs_unstd[unstd_rows, "Pr(>|z|)"],
      p_unstd_CR     = robust_unstd[robust_unstd_rows, "Pr(>|z|)"],
      p_std          = coefs_std[std_rows, "Pr(>|z|)"],
      p_std_CR       = robust_std[robust_std_rows, "Pr(>|z|)"],
      # df
      df_unstd   = df.residual(model_unstd),
      df_std     = df.residual(model_std),
      # model
      model_fit = "Likelihood ratio (Chi-square)",
      param_fit = "Chi-square (delta deviance)",
      value_fit = as.numeric(lrt$Deviance[2]), 
      p_fit = as.numeric(lrt$`Pr(>Chi)`[2]), 
      df_fit = lrt$Df[2], 
      measure_var = "Pseudo R-squared (Nagelkerke)",
      value_var = pscl::pR2(model_unstd)["r2CU"],
      # formula
      formula_unstd = rep(paste(as.character(formula_unstd)[2], "~", as.character(formula_unstd)[3]), length(predictors)),
      formula_std  = rep(paste(as.character(formula_std)[2], "~", as.character(formula_std)[3]), length(predictors)),
      row.names = NULL
    )
  } else {
    # Calculate partial correlations for continuous outcomes
    t_unstd    <- coefs_unstd[unstd_rows, "t value"]
    t_unstd_CR <- robust_unstd[robust_unstd_rows, "t value"]
    t_std      <- coefs_std[std_rows, "t value"]
    t_std_CR   <- robust_std[robust_std_rows, "t value"]
    df_unstd   <- df.residual(model_unstd)
    df_std     <- df.residual(model_std)
    
    # compute p value from overall F test
    f_stat <- summary(model_unstd)$fstatistic
    p_val = pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    
    result_table <- data.frame(
      outcome = outcome,
      predictor = predictors,
      # Betas
      Beta_unstd = coefs_unstd[unstd_rows, "Estimate"],
      Beta_std   = coefs_std[std_rows, "Estimate"],
      # Partial correlations (inserted after beta columns)
      pcor_unstd    = t_unstd    / sqrt(t_unstd^2    + df_unstd),
      pcor_unstd_CR = t_unstd_CR / sqrt(t_unstd_CR^2 + df_unstd),
      pcor_std      = t_std      / sqrt(t_std^2      + df_std),
      pcor_std_CR   = t_std_CR   / sqrt(t_std_CR^2   + df_std),
      pcor_resid    = pcor_resid, # residuals-based partial correlation
      # SEs
      SE_unstd       = coefs_unstd[unstd_rows, "Std. Error"],
      SE_unstd_CR    = robust_unstd[robust_unstd_rows, "Std. Error"],
      SE_std         = coefs_std[std_rows, "Std. Error"],
      SE_std_CR      = robust_std[robust_std_rows, "Std. Error"],
      # t values
      t_unstd        = t_unstd,
      t_unstd_CR     = t_unstd_CR,
      t_std          = t_std,
      t_std_CR       = t_std_CR,
      # p values
      p_unstd        = coefs_unstd[unstd_rows, "Pr(>|t|)"],
      p_unstd_CR     = robust_unstd[robust_unstd_rows, "Pr(>|t|)"],
      p_std          = coefs_std[std_rows, "Pr(>|t|)"],
      p_std_CR       = robust_std[robust_std_rows, "Pr(>|t|)"],
      # df
      df_unstd = df_unstd,
      df_std   = df_std,
      # model
      model_fit = "Overall F-test",
      param_fit = "F-statistic",
      value_fit = f_stat[1],
      p_fit = p_val,
      df_fit = paste0(f_stat[2], ", ", f_stat[3]),
      measure_var = "Multiple R-squared",
      value_var = summary(model_unstd)$r.squared,
      # formula
      formula_unstd = rep(paste(as.character(formula_unstd)[2], "~", as.character(formula_unstd)[3]), length(predictors)),
      formula_std  = rep(paste(as.character(formula_std)[2], "~", as.character(formula_std)[3]), length(predictors)),
      row.names = NULL
    )
    
  }
  
  return(result_table)
}

# Helper function for residuals-based partial correlation
partial_correlation_residuals_lme <- function(df, outcome, predictor, controls, re_vars) {
  
  # enter all control variables as fixed effects
  fe_formula <- if (length(controls) > 0) paste(controls, collapse = " + ") else "1"
  re_formula <- paste0("(1 | ", re_vars, ")", collapse = " + ")
  
  # combine into formula
  formula_y <- as.formula(
    paste0(outcome, " ~ ", fe_formula, if (!is.null(re_formula)) paste0(" + ", re_formula) else "")
  )
  formula_x <- as.formula(
    paste0(predictor, " ~ ", fe_formula, if (!is.null(re_formula)) paste0(" + ", re_formula) else "")
  )
  
  # correlate residuals
  resid_y <- residuals(lmer(formula_y, data = df))
  resid_x <- residuals(lmer(formula_x, data = df))
  cor(resid_y, resid_x)
}

get_betas_mixed <- function(
    df, outcome, predictors, fe_vars = NULL, re_vars = NULL,
    binary_outcome = TRUE
) {
  
  # If no random effects, use fixed effects model with robust SEs, clustering by fe_vars
  if (is.null(re_vars) || length(re_vars) == 0) {
    return(get_betas_cluster(df = df, outcome = outcome,  predictors = predictors,
                             fe_vars = fe_vars, cluster_vars = fe_vars,  # use fixed effects as clustering variables
                             binary_outcome = binary_outcome
    ))
    stop("No random effects specified and no fixed effects specified. At least one clustering variable (fe_vars) is required for robust SEs.")
  }
  
  # Build fixed and random effects parts
  fe_part <- c(predictors, fe_vars)
  fe_formula <- if (length(fe_part) > 0) paste(fe_part, collapse = " + ") else "1"
  re_formula <- paste0("(1 | ", re_vars, ")", collapse = " + ")
  
  # Build formulas using scale() in the formula
  if (binary_outcome) {
    formula_unstd <- as.formula(
      paste0(outcome, " ~ ", fe_formula, if (!is.null(re_formula)) paste0(" + ", re_formula) else "")
    )
    formula_std <- as.formula(
      paste0(outcome, " ~ ",
             paste0("scale(", predictors, ")", collapse = " + "),
             if (!is.null(fe_vars)) paste0(" + ", paste(fe_vars, collapse = " + ")) else "",
             if (!is.null(re_formula)) paste0(" + ", re_formula) else "")
    )
    model_unstd <- lme4::glmer(formula_unstd, data = df, family = binomial(), nAGQ = 0)
    model_std   <- lme4::glmer(formula_std,   data = df, family = binomial(), nAGQ = 0)
  } else {
    formula_unstd <- as.formula(
      paste0(outcome, " ~ ", fe_formula, if (!is.null(re_formula)) paste0(" + ", re_formula) else "")
    )
    formula_std <- as.formula(
      paste0("scale(", outcome, ") ~ ",
             paste0("scale(", predictors, ")", collapse = " + "),
             if (!is.null(fe_vars)) paste0(" + ", paste(fe_vars, collapse = " + ")) else "",
             if (!is.null(re_formula)) paste0(" + ", re_formula) else "")
    )
    model_unstd <- lmerTest::lmer(formula_unstd, data = df)
    model_std   <- lmerTest::lmer(formula_std,   data = df)
  }
  
  # For continuous outcomes, calculate partial correlations using residuals
  if (!binary_outcome) {
    # Controls for partial correlation: all other predictors and fixed effects
    controls <- setdiff(predictors, fe_part) # placeholder, will be replaced below
    pcor_resid <- sapply(predictors, function(pred) {
      controls <- setdiff(fe_part, pred)
      partial_correlation_residuals_lme(df, outcome, pred, controls, re_vars = re_vars)
    })
  }
  
  
  # Extract coefficients for predictors
  coefs_unstd <- summary(model_unstd)$coefficients
  coefs_std   <- summary(model_std)$coefficients
  preds_std_names <- paste0("scale(", predictors, ")")
  unstd_rows <- rownames(coefs_unstd) %in% predictors
  std_rows   <- rownames(coefs_std)   %in% preds_std_names
  
  if (binary_outcome) {
    result_table <- data.frame(
      outcome = outcome,
      predictor = predictors,
      Log_odds_unstd = coefs_unstd[unstd_rows, "Estimate"],
      Log_odds_std   = coefs_std[std_rows, "Estimate"],
      OR_unstd = exp(coefs_unstd[unstd_rows, "Estimate"]),
      OR_std   = exp(coefs_std[std_rows, "Estimate"]),
      SE_unstd = coefs_unstd[unstd_rows, "Std. Error"],
      SE_std   = coefs_std[std_rows, "Std. Error"],
      z_unstd  = coefs_unstd[unstd_rows, "Estimate"] / coefs_unstd[unstd_rows, "Std. Error"],
      z_std    = coefs_std[std_rows, "Estimate"] / coefs_std[std_rows, "Std. Error"],
      p_unstd  = coefs_unstd[unstd_rows, "Pr(>|z|)"],
      p_std    = coefs_std[std_rows, "Pr(>|z|)"],
      formula_unstd = rep(paste(as.character(formula_unstd)[2], "~", as.character(formula_unstd)[3]), length(predictors)),
      formula_std  = rep(paste(as.character(formula_std)[2], "~", as.character(formula_std)[3]), length(predictors)),
      row.names = NULL
    )
  } else {
    t_unstd <- coefs_unstd[unstd_rows, "t value"]
    t_std   <- coefs_std[std_rows, "t value"]
    df_unstd <- df.residual(model_unstd)
    df_std   <- df.residual(model_std)
    result_table <- data.frame(
      outcome = outcome,
      predictor = predictors,
      Beta_unstd = coefs_unstd[unstd_rows, "Estimate"],
      Beta_std   = coefs_std[std_rows, "Estimate"],
      SE_unstd   = coefs_unstd[unstd_rows, "Std. Error"],
      SE_std     = coefs_std[std_rows, "Std. Error"],
      t_unstd    = t_unstd,
      t_std      = t_std,
      p_unstd    = coefs_unstd[unstd_rows, "Pr(>|t|)"],
      p_std      = coefs_std[std_rows, "Pr(>|t|)"],
      df_unstd = df_unstd,
      df_std   = df_std,
      pcor_unstd = t_unstd / sqrt(t_unstd^2 + df_unstd),
      pcor_std   = t_std / sqrt(t_std^2 + df_std),
      pcor_resid    = pcor_resid, # residuals-based partial correlation
      formula_unstd = rep(paste(as.character(formula_unstd)[2], "~", as.character(formula_unstd)[3]), length(predictors)),
      formula_std  = rep(paste(as.character(formula_std)[2], "~", as.character(formula_std)[3]), length(predictors)),
      row.names = NULL
    )
  }
  
  return(result_table)
}

icc_school_year <- function(df, outcome, school_var, year_var, binary_outcome = FALSE) {
  require(lme4)
  
  # Helper for extracting variance components
  get_vars <- function(model, var1, var2 = NULL) {
    vc <- VarCorr(model)
    var_list <- list()
    var_list$var1 <- as.numeric(vc[[var1]])
    if (!is.null(var2)) var_list$var2 <- as.numeric(vc[[var2]])
    var_list$resid <- attr(vc, "sc")^2
    var_list
  }
  
  # Model with only school as random effect
  if (binary_outcome) {
    model_school <- glmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", school_var, ")")),
      data = df, family = binomial()
    )
  } else {
    model_school <- lmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", school_var, ")")),
      data = df
    )
  }
  vars_school <- get_vars(model_school, school_var)
  icc_school_only <- vars_school$var1 / (vars_school$var1 + vars_school$resid)
  
  # Model with only year as random effect
  if (binary_outcome) {
    model_year <- glmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", year_var, ")")),
      data = df, family = binomial()
    )
  } else {
    model_year <- lmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", year_var, ")")),
      data = df
    )
  }
  vars_year <- get_vars(model_year, year_var)
  icc_year_only <- vars_year$var1 / (vars_year$var1 + vars_year$resid)
  
  # Model with both school and year as random effects
  if (binary_outcome) {
    model_both <- glmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", school_var, ") + (1 | ", year_var, ")")),
      data = df, family = binomial()
    )
  } else {
    model_both <- lmer(
      as.formula(paste0(outcome, " ~ 1 + (1 | ", school_var, ") + (1 | ", year_var, ")")),
      data = df
    )
  }
  vars_both <- get_vars(model_both, school_var, year_var)
  total_var_both <- vars_both$var1 + vars_both$var2 + vars_both$resid
  icc_school_both <- vars_both$var1 / total_var_both
  icc_year_both   <- vars_both$var2 / total_var_both
  
  # Output as data frame
  result <- data.frame(
    outcome = outcome,
    ICC_school_only = icc_school_only,
    ICC_year_only = icc_year_only,
    ICC_school_both = icc_school_both,
    ICC_year_both = icc_year_both
  )
  return(result)
}

# Positive delta AIC: The more complex model is preferred.
# Negative delta AIC: The base (simpler) model is preferred.

# General rules of thumb:
# ΔAIC < 2: Models are essentially equivalent.
# ΔAIC 4–7: Less support for the model with higher AIC.
# ΔAIC > 10: Much less support for the model with higher AIC.
# (See Burnham & Anderson, 2002, "Model Selection and Multimodel Inference")

compare_school_year_models <- function(df, outcome, predictors, school_var = "school", year_var = "year", binary_outcome = FALSE) {
  require(lme4)
  
  # Helper to build formulas
  pred_str <- paste(predictors, collapse = " + ")
  
  # Model formulas
  f_no_year  <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", school_var, ")"))
  f_year_re  <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", school_var, ") + (1 | ", year_var, ")"))
  f_year_fe  <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", school_var, ") + factor(", year_var, ")"))
  f_no_school <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", year_var, ")"))
  f_school_re <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", year_var, ") + (1 | ", school_var, ")"))
  f_school_fe <- as.formula(paste0(outcome, " ~ ", pred_str, " + (1 | ", year_var, ") + factor(", school_var, ")"))
  
  # Model fitting function
  fit <- function(formula, binary) {
    if (binary) {
      lme4::glmer(formula, data = df, family = binomial(), nAGQ = 0)
    } else {
      lme4::lmer(formula, data = df)
    }
  }
  
  # 1. Random effect for year vs. no year effect (random school in both)
  m1_base <- fit(f_no_year, binary_outcome)
  m1_year_re <- fit(f_year_re, binary_outcome)
  an1 <- anova(m1_base, m1_year_re)
  deltaAIC1 <- AIC(m1_base) - AIC(m1_year_re)
  
  # 2. Year as fixed effect vs. no year effect (random school in both)
  m2_year_fe <- fit(f_year_fe, binary_outcome)
  an2 <- anova(m1_base, m2_year_fe)
  deltaAIC2 <- AIC(m1_base) - AIC(m2_year_fe)
  
  # 3. Year as random effect vs. year as fixed effect (random school in both)
  an3 <- anova(m2_year_fe, m1_year_re)
  deltaAIC3 <- AIC(m2_year_fe) - AIC(m1_year_re)
  
  # 4. Random effect for school vs. no school effect (random year in both)
  m4_base <- fit(f_no_school, binary_outcome)
  m4_school_re <- fit(f_school_re, binary_outcome)
  an4 <- anova(m4_base, m4_school_re)
  deltaAIC4 <- AIC(m4_base) - AIC(m4_school_re)
  
  # 5. School as fixed effect vs. no school effect (random year in both)
  m5_school_fe <- fit(f_school_fe, binary_outcome)
  an5 <- anova(m4_base, m5_school_fe)
  deltaAIC5 <- AIC(m4_base) - AIC(m5_school_fe)
  
  # 6. School as random effect vs. school as fixed effect (random year in both)
  an6 <- anova(m5_school_fe, m4_school_re)
  deltaAIC6 <- AIC(m5_school_fe) - AIC(m4_school_re)
  
  # Assemble results
  result <- data.frame(
    outcome = outcome,
    predictor = pred_str,
    
    Comparison = c(
      "No year effect (RE school) vs. Year RE",
      "No year effect (RE school) vs. Year FE",
      "Year FE vs. Year RE (RE school)",
      "No school effect (RE year) vs. School RE",
      "No school effect (RE year) vs. School FE",
      "School FE vs. School RE (RE year)"
    ),
    chisq = c(
      an1$Chisq[2],
      an2$Chisq[2],
      an3$Chisq[2],
      an4$Chisq[2],
      an5$Chisq[2],
      an6$Chisq[2]
    ),
    df = c(
      an1$Df[2],
      an2$Df[2],
      an3$Df[2],
      an4$Df[2],
      an5$Df[2],
      an6$Df[2]
    ),
    p = c(
      an1$`Pr(>Chisq)`[2],
      an2$`Pr(>Chisq)`[2],
      an3$`Pr(>Chisq)`[2],
      an4$`Pr(>Chisq)`[2],
      an5$`Pr(>Chisq)`[2],
      an6$`Pr(>Chisq)`[2]
    ),
    delta_AIC = c(
      deltaAIC1,
      deltaAIC2,
      deltaAIC3,
      deltaAIC4,
      deltaAIC5,
      deltaAIC6
    ),
    row.names = NULL
  )
  return(result)
}

unique_predictor_contributions <- function(df, outcome, predictors, fe_vars = NULL, re_vars = NULL, binary_outcome = FALSE) {
  require(lme4)
  require(MuMIn)
  
  # Build fixed effect terms
  fe_term <- if (!is.null(fe_vars)) paste(fe_vars, collapse = " + ") else NULL
  
  # Helper to build RHS
  build_rhs <- function(preds) {
    terms <- character()
    if (length(preds) > 0) {
      terms <- c(terms, paste0("scale(", preds, ")"))
    }
    if (!is.null(fe_vars)) terms <- c(terms, fe_vars)
    if (length(terms) == 0) {
      rhs <- "1"
    } else {
      rhs <- paste(terms, collapse = " + ")
    }
    rhs
  }
  
  build_formula <- function(rhs) {
    if (binary_outcome) {
      as.formula(paste0(outcome, " ~ ", rhs))
    } else {
      as.formula(paste0("scale(", outcome, ") ~ ", rhs))
    }
  }
  
  # Fit full model
  rhs_full <- build_rhs(predictors)
  formula_full <- if (is.null(re_vars)) {
    build_formula(rhs_full)
  } else {
    re_term <- paste0("(1 | ", re_vars, ")", collapse = " + ")
    as.formula(paste0(
      if (binary_outcome) paste0(outcome, " ~ ", rhs_full)
      else paste0("scale(", outcome, ") ~ ", rhs_full),
      " + ", re_term
    ))
  }
  
  if (is.null(re_vars)) {
    if (binary_outcome) {
      model_full <- glm(formula_full, data = df, family = binomial())
      r2_full <- pscl::pR2(model_full)[["r2CU"]]
    } else {
      model_full <- lm(formula_full, data = df)
      r2_full <- summary(model_full)$r.squared
    }
  } else {
    if (binary_outcome) {
      model_full <- glmer(formula_full, data = df, family = binomial(), nAGQ = 0)
      r2_full <- MuMIn::r.squaredGLMM(model_full)[1, "R2m"]
    } else {
      model_full <- lmer(formula_full, data = df, REML = TRUE)
      r2_full <- MuMIn::r.squaredGLMM(model_full)[, "R2m"]
    }
  }
  
  results <- list()
  
  for (p in seq_along(predictors)) {
    preds_tmp <- predictors[-p]
    rhs_reduced <- build_rhs(preds_tmp)
    if (is.null(re_vars)) {
      formula_reduced <- build_formula(rhs_reduced)
      if (binary_outcome) {
        model_reduced <- glm(formula_reduced, data = df, family = binomial())
        r2_reduced <- pscl::pR2(model_reduced)[["r2CU"]]
        lr_test <- anova(model_reduced, model_full, test = "Chisq")
      } else {
        model_reduced <- lm(formula_reduced, data = df)
        r2_reduced <- summary(model_reduced)$r.squared
        lr_test <- anova(model_reduced, model_full)
      }
    } else {
      formula_reduced <- as.formula(
        paste0(
          if (binary_outcome) paste0(outcome, " ~ ", rhs_reduced)
          else paste0("scale(", outcome, ") ~ ", rhs_reduced),
          " + ", paste0("(1 | ", re_vars, ")", collapse = " + ")
        )
      )
      if (binary_outcome) {
        model_reduced <- glmer(formula_reduced, data = df, family = binomial(), nAGQ = 0)
        r2_reduced <- MuMIn::r.squaredGLMM(model_reduced)[1, "R2m"]
        lr_test <- as.data.frame(anova(model_reduced, model_full))
      } else {
        model_reduced <- lmer(formula_reduced, data = df, REML = TRUE)
        r2_reduced <- MuMIn::r.squaredGLMM(model_reduced)[, "R2m"]
        lr_test <- as.data.frame(anova(model_reduced, model_full))
      }
    }
    results[[p]] <- data.frame(
      outcome = outcome,
      predictor = predictors[p],
      
      formula_std  = paste(as.character(formula_full)[2], "~", as.character(formula_full)[3]),
      formula_red  = paste(as.character(formula_reduced)[2], "~", as.character(formula_reduced)[3])
    )
    
    if (is.null(re_vars)) {
      if (binary_outcome) {
        results[[p]]$param_lr <- "Deviance"
        results[[p]]$value <- as.numeric(lr_test$Deviance[2])
        results[[p]]$p_lr <- as.numeric(lr_test$`Pr(>Chi)`[2])
        results[[p]]$R2 <- "pseudo"
      } else {
        results[[p]]$param_lr <- "F"
        results[[p]]$value <- as.numeric(lr_test$F[2])
        results[[p]]$p_lr <- as.numeric(lr_test$`Pr(>F)`[2])
        results[[p]]$R2 <- "std"
      }
    } else {
      results[[p]]$param_lr <- "Chisq"
      results[[p]]$value <- as.numeric(lr_test$Chisq[2])
      results[[p]]$p_lr <- as.numeric(lr_test$`Pr(>Chisq)`[2])
      results[[p]]$R2 <- "std"
    }
    results[[p]]$R2_full = r2_full
    results[[p]]$R2_reduced = r2_reduced
    results[[p]]$delta_R2 = r2_full - r2_reduced
    results[[p]]$df_lr = lr_test$Df[2]
  }
  do.call(rbind, results)
}

boot_ame_parallel <- function(
    smooth_name,    # Name of the smooth term to compute AME for
    data,           # Full data frame to sample from
    clusters,       # Vector of unique cluster IDs
    cluster_var,    # Vector of cluster assignments for each row in data
    predictors,     # Vector of continuous predictor names
    fe_vars,        # Vector of categorical (fixed effect) variable names
    outcome,        # Outcome variable name (as string)
    family_gam,     # GAM family (e.g. gaussian(), binomial())
    n_boot,         # Number of bootstrap samples
    n_cores,        # Number of cores used
    binary_outcome  # Logical
) {
  # Run bootstrap iterations in parallel
  # future_sapply runs each bootstrap iteration in parallel
  future_sapply(1:n_boot, function(i) {
  # sapply(1:n_boot, function(i) {
    
    message(i)
    
    # 1. Sample clusters with replacement from filtered clusters
    indices <- sample(seq_along(clusters), size = length(clusters), replace = TRUE)
    sampled_clusters <- clusters[indices]
    
    # 2. Find rows in data belonging to sampled clusters
    rows <- which(cluster_var %in% sampled_clusters)
    d <- droplevels(data[rows, ])
    
    # 3. Check for both outcome classes (for binomial family)
    outcome_vals <- d[[outcome]]
    unique_outcomes <- unique(outcome_vals)
    n_unique <- length(unique_outcomes)
    n_rows <- nrow(d)

    # Check for both outcome classes (binomial)
    if (binary_outcome && n_unique < 2) {
      message(sprintf("Bootstrap %d failed: only one outcome class. n_rows = %d, outcome = %s", i, n_rows, paste(unique_outcomes, collapse = ",")))
      return(NA)
    }

    # 4. Identify factor variables in the bootstrap sample
    factor_vars <- names(Filter(is.factor, d))
    
    # 5. Remove categorical variables with only one level (cannot fit contrasts)
    one_level_factors <- factor_vars[sapply(d[factor_vars], function(x) nlevels(x) < 2)]
    cat_vars_boot <- setdiff(fe_vars, one_level_factors)
    
    # 6. Build formula: smooths for continuous, linear for categorical
    rhs <- paste(c(paste0("s(", predictors, ")"), cat_vars_boot), collapse = " + ")
    new_formula <- as.formula(paste(outcome, "~", rhs))
    
    # 7. Fit GAM to the bootstrap sample, with error handling
    fit <- tryCatch(
      mgcv::gam(new_formula, data = d, family = family_gam, method = "REML"),
      error = function(e) {
        message(sprintf("Bootstrap %d failed: model fitting error. n_rows = %d, outcome classes = %s. Error: %s", i, n_rows, paste(unique_outcomes, collapse = ","), e$message))
        return(NULL)
      },
      warning = function(w) invokeRestart("muffleWarning")
    )
    if (is.null(fit)) return(NA)

    # 8. Compute derivatives (marginal effects) for the smooth term, with error handling
    deriv_df <- tryCatch(
      derivatives(fit, select = smooth_name, data = d, type = "central"),
      error = function(e) {
        message(sprintf("Bootstrap %d failed: derivative error. Error: %s", i, e$message))
        return(NULL)
      },
      warning = function(w) invokeRestart("muffleWarning")
    )
    if (is.null(deriv_df) || !(".derivative" %in% names(deriv_df))) return(NA)
    
    # 9. Return the average marginal effect (AME) for this bootstrap sample
    mean(deriv_df$.derivative, na.rm = TRUE)
  })
}

summarise_gam <- function(gam_model) {

  # summary table for all smooth terms in GAM #
  # CAVEAT: These statistics do not account for the clustered structure of the data
  s_tab <- summary(gam_model)$s.table
  
  # edf (effective degrees of freedom) shows how complex or “wiggly” the fitted smooth is 
  # for each continuous predictor—values near 1 mean nearly linear, higher values mean more non-linear.
  
  # Ref.df (reference degrees of freedom) is the value used for the F-test of each smooth, 
  # reflecting the maximum flexibility allowed for the smooth term.
  
  s_tab <- as.data.frame(s_tab)
  names(s_tab)[3:4] <- c("F_value", "p_value")
  s_tab$smooth <- row.names(s_tab)
  row.names(s_tab) <- NULL
  
  # overall model performance #
  
  # Rank (used/total)
  s_tab$rank_used <- summary(gam_model)$rank
  s_tab$rank_total <- summary(gam_model)$np
  
  # Adjusted R-squared
  s_tab$adj_r2 <- summary(gam_model)$r.sq
  
  # Deviance explained
  s_tab$dev_explained <- summary(gam_model)$dev.expl * 100  # as percentage
  
  # Scale estimate
  s_tab$scale_est <- summary(gam_model)$scale
  
  # Number of observations
  s_tab$n_obs <- summary(gam_model)$n
  
  # distribution and link to use in fitting
  s_tab$family <- gam_model$family$family
  s_tab$link <- gam_model$family$link
  
  # model formula
  formula = summary(gam_model)$formula
  s_tab$formula = paste(as.character(formula)[2], "~", as.character(formula)[3])

  return(s_tab)
  
}

get_gam_betas_cluster <- function(df, outcome, predictors, fe_vars, cluster_vars, binary_outcome = TRUE, 
                                  n_cores = 12, n_boot = 100, min_cluster_size = 5) {
  require(mgcv)
  require(gratia)
  require(future.apply)
  
  # Build formula
  formula <- as.formula(
    paste0(
      outcome, " ~ ",
      paste0("s(", predictors, ")", collapse = " + "),
      if (!is.null(fe_vars)) paste0(" + ", paste0(fe_vars, collapse = " + ")) else ""
    )
  )
  
  if (binary_outcome) {
    # create a df where all predictors are scaled
    df_std <- df 
    df_std <- df_std %>%
      mutate(across(all_of(predictors), ~ as.numeric(scale(.)), .names = "{.col}"))
    
    formula_null <- as.formula(paste0(outcome, " ~ 1"))
    family_gam <- binomial()
  } else {
    # create a df where all predictors and the outcome are scaled
    df_std <- df 
    df_std <- df_std %>%
      mutate(across(all_of(c(predictors, outcome)), ~ as.numeric(scale(.)), .names = "{.col}"))
    
    family_gam <- gaussian()
  }
  
  # Fit GAM models
  # model_unstd <- mgcv::gam(formula, data = df, family = family_gam, method = "REML")
  # summary(model_unstd)
  model_std   <- mgcv::gam(formula, data = df_std, family = family_gam, method = "REML")
  ggsave(filename = file.path(file_dir, paste0(file_stem, "_gam_", outcome, ".jpg")), plot = draw(model_std))
  
  # get model summary
  coef <- summarise_gam(model_std)

  # first derivatives of all smooths using central finite differences
  deriv_df <- derivatives(model_std, data = df_std, type = "central")
  ame <- deriv_df %>%
    group_by(.smooth) %>%
    summarise(
      mean_ME = mean(.derivative, na.rm = T),
      sd_ME = sd(.derivative, na.rm = T),
      min_ME = min(.derivative, na.rm = T),
      max_ME = max(.derivative, na.rm = T),
    ) %>%
    rename(smooth = .smooth)

  # combine all data
  coef <- merge(coef, ame, all = T)
  coef$predictor <- gsub("s(", "", coef$smooth, fixed = T)
  coef$predictor <- gsub(")", "", coef$predictor, fixed = T)
  
  # Combine into one table
  result_table <- data.frame(
    outcome = outcome,
    predictor = predictors)
  result_table <- merge(result_table, coef)
  
  # get clustered bootstrap SE #
  
  # Get smooth term names
  smooth_names <- gratia::smooths(model_std)
  smooth_names <- smooth_names[grepl("s(f_", smooth_names, fixed = T)] # focus on factors
  
  # determine clusters for bootstrapping
  cluster_var <- interaction(df_std[[cluster_vars[1]]], df_std[[cluster_vars[2]]], drop = TRUE)
  df_std$clusters <- interaction(df_std[[cluster_vars[1]]], df_std[[cluster_vars[2]]], drop = TRUE)
  clusters <- unique(cluster_var)
  result_table$n_clust = length(clusters)
  
  # Filter clusters with too little observations
  cluster_sizes <- table(df_std$clusters)
  clusters_too_small <- names(cluster_sizes)[cluster_sizes < min_cluster_size]
  
  # For binary outcome, filter clusters with only one outcome class
  if (binary_outcome) {
    cluster_stats <- aggregate(
      list(outcome = df_std[[outcome]]),
      by = list(cluster = df_std$clusters),
      FUN = function(x) length(unique(x))
    )
    clusters_one_outcome <- cluster_stats$cluster[cluster_stats$outcome == 1]
    clusters_exclude <- union(clusters_one_outcome, clusters_too_small)
  } else {
    clusters_exclude <- clusters_too_small
  }
  
  # determine list of clusters
  clusters_filtered <- setdiff(clusters, clusters_exclude)
  
  # Fit GAM model again after filtering
  df_filt <- df_std[df_std$clusters %in% clusters_filtered, ]
  model_filt   <- mgcv::gam(formula, data = df_filt, family = family_gam, method = "REML")

  # get model summary after filtering
  coef <- summarise_gam(model_filt)
  
  # first derivatives of all smooths using central finite differences
  deriv_df <- derivatives(model_filt, data = df_filt, type = "central")
  ame <- deriv_df %>%
    group_by(.smooth) %>%
    summarise(
      mean_ME = mean(.derivative, na.rm = T),
      sd_ME = sd(.derivative, na.rm = T),
      min_ME = min(.derivative, na.rm = T),
      max_ME = max(.derivative, na.rm = T),
    ) %>%
    rename(smooth = .smooth)
  
  # combine all data
  coef <- merge(coef, ame, all = T)
  coef$predictor <- gsub("s(", "", coef$smooth, fixed = T)
  coef$predictor <- gsub(")", "", coef$predictor, fixed = T)
  
  # add number of clusters
  coef$n_clust <- length(clusters_filtered)
  
  # add "_boot" to column names
  names(coef)[sapply(coef, is.numeric)] <- paste0(names(coef)[sapply(coef, is.numeric)], "_boot")
  
  # Combine into one table
  result_table <- merge(result_table, coef, by = c(names(coef)[sapply(coef, is.character)]))
  
  # Set up parallel processing before calling
  plan(multisession, workers = n_cores)
  
  # Loop over each smooth term sequentially
  boot_results <- lapply(seq_along(smooth_names), function(i) {

    # time process
    smooth_name <- smooth_names[i]
    message(cat("Starting bootstrap for outcome", outcome, "term", smooth_name, "...\n"))
    start_time <- Sys.time()
    
    # Use tryCatch to handle errors gracefully
    result <- tryCatch({
      # Parallel bootstrap for this smooth
      boot_stats <- boot_ame_parallel(
        smooth_name = smooth_name,
        data = df_std,
        clusters = clusters_filtered,
        cluster_var = cluster_var,
        predictors = predictors,
        fe_vars = fe_vars,
        outcome = outcome,
        family_gam = family_gam,
        n_boot = n_boot,
        n_cores = n_cores,
        binary_outcome = binary_outcome
      )
      
      # time process
      end_time <- Sys.time()
      elapsed <- end_time - start_time
      message(cat("\t\t\tBootstrap for", smooth_name, "took", as.numeric(elapsed, units = "mins"), "minutes.\n"))
      
      # process boot data
      AME <- mean(boot_stats, na.rm = TRUE)
      SE <- sd(boot_stats, na.rm = TRUE)
      ci_l <- quantile(boot_stats, 0.025)
      ci_u <- quantile(boot_stats, 0.975)
      z <- AME / SE
      p <- 2 * (1 - pnorm(abs(z)))
      n_success <- sum(!is.na(boot_stats))

      # extract results
      data.frame(
        smooth = smooth_name,
        AME = AME,
        SE = SE,
        ci_l = ci_l,
        ci_u = ci_u,
        z = z,
        p = p,
        n_boot = n_boot,
        n_success = n_success,
        n_cores = n_cores,
        duration = elapsed
      )
    }, error = function(e) {
      # If anything fails, return NAs for all stats except smooth name
      end_time <- Sys.time()
      elapsed <- end_time - start_time
      
      data.frame(
        smooth = smooth_name,
        AME = NA,
        SE = NA,
        ci_l = NA,
        ci_u = NA,
        z = NA,
        p = NA,
        n_boot = n_boot,
        n_success = NA,
        n_cores = n_cores,
        duration = elapsed
      )
    })
    
    result
  })
  
  # End parallel processing
  plan(sequential)
  
  # Combine results
  effect_table <- do.call(rbind, boot_results)
  
  # merge with result_table
  result_table <- merge(result_table, effect_table, all = T)

  return(result_table)
}

run_dominance_analysis <- function(df, df_loo, outcome, predictors, matrices = c("complete", "conditional", "general"), binary_outcome = T){
  
  require(dominanceanalysis)
  
  # Build the formula string
  formula <- as.formula(paste0(outcome, " ~ ", paste0(predictors, collapse = " + ")))
  
  #### raw ####
  
  # Fit linear regression
  if (binary_outcome) {
    fit_raw <- glm(formula = formula, data = df, family = binomial())
    index = "r2.n" # pseudo R2 (Nagelkerke)
  } else {
    fit_raw <- lm(formula = formula, data = df)
    index = "r2"
  }
  
  # Conduct dominance analysis - using all predictors akin to the [g]lm() model
  da_raw <- dominanceAnalysis(fit_raw)
  
  # Get relative contributions
  # average contribution is the same as general dominance
  # general dominance statistic for each predictor = 
  # The average additional contribution to model fit (e.g. R²) across all possible subset models and model sizes.
  # this is the same as plot(da_raw, which.graph = "general")[["data"]][["value"]]
  # same as colMeans(da_raw$contribution.by.level$r2[-1]) == summary(da_raw)$r2$average.contribution
  contr_abs <- da_raw$contribution.average[[index]]
  contr_tot <- sum(contr_abs)
  contr_rel <- contr_abs / contr_tot
  
  # save contributions data
  raw <- data.frame(
    data = "raw",
    outcome = outcome,
    predictor = names(contr_abs),
    contr_abs = contr_abs,
    contr_rel = contr_rel,
    row.names = NULL)
  
  #### loo ####
  
  # Fit linear regression
  if (binary_outcome) {
    fit_loo <- glm(formula = formula, data = df_loo, family = binomial())
  } else {
    fit_loo <- lm(formula = formula, data = df_loo)
  }
  
  # Conduct dominance analysis - using all predictors akin to the [g]lm() model
  da_loo <- dominanceAnalysis(fit_loo)
  
  # Get relative contributions
  contr_abs <- da_loo$contribution.average[[index]]
  contr_tot <- sum(contr_abs)
  contr_rel <- contr_abs / contr_tot
  
  # save contributions data
  loo <- data.frame(
    data = "loo",
    outcome = outcome,
    predictor = names(contr_abs),
    contr_abs = contr_abs,
    contr_rel = contr_rel,
    row.names = NULL)
  
  ### combine ####
  out <- rbind(raw, loo)
  
  ### Get dominance matrices ###
  for (m in 1:length(matrices)) {
    
    matrix = matrices[m]
    
    # extract matrix RAW
    m_raw <- as.data.frame(dominanceMatrix(da_raw, type = matrix))
    
    # format matrix RAW
    m_raw$data <- "raw"
    m_raw$outcome <- outcome
    m_raw$dominance <- matrix
    m_raw$predictor <- row.names(m_raw)
    row.names(m_raw) <- NULL
    
    # extract matrix LOO
    m_loo <- as.data.frame(dominanceMatrix(da_loo, type = matrix))
    
    # format matrix LOO
    m_loo$data <- "loo"
    m_loo$outcome <- outcome
    m_loo$dominance <- matrix
    m_loo$predictor <- row.names(m_loo)
    row.names(m_loo) <- NULL
    
    # combine 
    m_tmp <- rbind(m_raw, m_loo)
    m_tmp <- m_tmp[, c((length(predictors)+1):(length(predictors)+4), 1:length(predictors))]
    
    if(m == 1) mat <- m_tmp else mat <- rbind(mat, m_tmp)
    
  }
  
  return(list(average_contribution = out, dominance_matrix = mat))
  
}

