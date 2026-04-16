# Load required libraries
# (Run install.packages(c("dplyr", "tidyr", "readr")) if you don't have them)
library(readr)
library(dplyr)
library(tidyr)

# 1. Define the precise column names based on the structure of your CSV
col_names <- c(
  "ID", "Group_Name", "Category",
  "SurfTemp_Opt", "SurfTemp_Min", "SurfTemp_Max", "SurfTemp_Ref",
  "BotTemp_Opt", "BotTemp_Min", "BotTemp_Max", "BotTemp_Ref",
  "Depth_Opt", "Depth_Min", "Depth_Max", "Depth_Ref",
  "SurfSal_Opt", "SurfSal_Min", "SurfSal_Max", "SurfSal_Ref",
  "BotSal_Opt", "BotSal_Min", "BotSal_Max", "BotSal_Ref"
)

# 2. Read the parameters file
# skip = 4 bypasses the title, description, super-headers, and sub-headers.
raw_params <- read_csv("../Inputs/NGoM/parameters.csv", skip = 4, col_names = col_names, show_col_types = FALSE)

# 3. Clean the wide dataframe
numeric_columns <- c(
  "SurfTemp_Opt", "SurfTemp_Min", "SurfTemp_Max",
  "BotTemp_Opt", "BotTemp_Min", "BotTemp_Max",
  "Depth_Opt", "Depth_Min", "Depth_Max",
  "SurfSal_Opt", "SurfSal_Min", "SurfSal_Max",
  "BotSal_Opt", "BotSal_Min", "BotSal_Max"
)

# Convert metrics to numeric. 
clean_wide_df <- raw_params %>%
  mutate(across(all_of(numeric_columns), ~ suppressWarnings(as.numeric(.))))

# 4. Convert to a Tidy (Long) Format Dataframe 
long_results_df <- clean_wide_df %>%
  pivot_longer(
    cols = matches("_(Opt|Min|Max|Ref)$"),
    names_to = c("Environmental_Variable", ".value"),
    names_sep = "_"
  ) %>%
  # Filter out inapplicable parameters
  filter(!(is.na(Opt) & is.na(Min) & is.na(Max) & is.na(Ref))) %>%
  # Keep only rows that have valid tolerance limits to build the shape
  filter(!is.na(Min) & !is.na(Max)) %>%
  # Create the concatenated column name (e.g., "pelagic sharks_SurfTemp")
  mutate(Name = paste(Group_Name, Environmental_Variable, sep = "_"))

# 5. Function to generate the response shape (Limits + 1200 points)
generate_response_shape <- function(min_val, max_val, opt_val, n_points = 1200) {
  # If Opt is missing, default it to the midpoint
  if (is.na(opt_val)) opt_val <- (min_val + max_val) / 2
  
  # Ensure min and max are ordered correctly
  if (min_val > max_val) {
    temp <- min_val; min_val <- max_val; max_val <- temp
  }
  
  # The first two values are the limits
  limits <- c(min_val, max_val)
  
  # Configuration for the trapezoid shape
  range_val <- max_val - min_val
  plateau_width <- range_val * 0.10 # The flat optimum spans 10% of the total range
  base_y <- 0.01                    # The y-value at step 1 and step 1200 (> 0)
  
  # Calculate the left and right shoulders of the plateau
  opt_left <- max(min_val, opt_val - plateau_width / 2)
  opt_right <- min(max_val, opt_val + plateau_width / 2)
  
  # Generate 1200 evenly spaced X points between min and max
  x_seq <- seq(min_val, max_val, length.out = n_points)
  
  # Calculate Y values (from base_y to 1) for the trapezoidal shape
  y_vals <- sapply(x_seq, function(x) {
    if (x < opt_left) {
      if (opt_left == min_val) return(1) # Prevent division by zero
      return(base_y + (1 - base_y) * ((x - min_val) / (opt_left - min_val)))
    } else if (x >= opt_left && x <= opt_right) {
      return(1) # Plateau at maximum response (1.0)
    } else {
      if (max_val == opt_right) return(1)
      return(base_y + (1 - base_y) * ((max_val - x) / (max_val - opt_right)))
    }
  })
  
  # Clamp values to strictly [0, 1] just in case of floating point drift
  y_vals <- pmax(0, pmin(1, y_vals))
  
  return(c(limits, y_vals))
}

# 6. Apply the function to create the final 1202-row dataframe
# Create a list where each element is the 1202-length vector for a specific group/variable
response_list <- lapply(1:nrow(long_results_df), function(i) {
  row <- long_results_df[i, ]
  generate_response_shape(row$Min, row$Max, row$Opt)
})

# Apply the names corresponding to the functional group and variable
names(response_list) <- long_results_df$Name

# Convert to a dataframe
final_results_df <- as.data.frame(response_list)

# Prepend the Index column
final_results_df <- cbind(
  Index = c("Left limit", "Right limit", paste0("Step_", 1:1200)),
  final_results_df
)

# 7. Save outputs
write_csv(clean_wide_df, "./Outputs-for-ewe/cleaned_parameters_wide.csv")
write_csv(final_results_df, "./Outputs-for-ewe/environmental_results_formatted.csv")

cat("\nSuccessfully generated the trapezoidal response shapes!\n")
cat("Results saved to './Outputs-for-ewe/environmental_results_formatted.csv'.\n")