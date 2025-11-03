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
#  transforms column lookup table from its current structure into a format 
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
    geom_histogram(aes(y=..density..), color = nondominant_col, fill = nondominant_col, bins = 30, binwidth = bin_width) +
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
