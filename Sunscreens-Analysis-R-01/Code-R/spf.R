library(tidyverse)
library(stringi)
library(ggplot2)
library(readr)


# Import the CSV file into R
spf <- read_csv('C:/Users/sandr/Desktop/Web scrapping/Project/spfCreams.csv')


# Function to clean and replace
clean_and_replace <- function(column) {
  # Check if the column is "reviews_amount"
  if (identical(column, spf$reviews_amount)) {
    # Extract numeric values from "reviews_amount"
    matches <- stri_extract_all_regex(column, "\\d+")
    replacements <- lapply(matches, function(match) {
      if (length(match) > 0) {
        stri_join(match, collapse = "") %>% as.numeric()
      } else {
        NA
      }
    })
  } else if (identical(column, spf$price)) {
    # Check if the column is "price"
    # Extract numeric values from "price"
    pattern <- "^\\$\\d+\\.\\d{2}$"
    matches <- stri_extract_all_regex(column, pattern)
    replacements <- lapply(matches, function(match) {
      if (length(match) > 0) {
        stri_replace_all_regex(match, "\\$", "") %>% as.numeric()
      } else {
        NA
      }
    })
  } else {
    # Check if the column is "stars"
    # Extract numeric values from "stars"
    matches <- stri_extract_all_regex(column, "\\d+\\.\\d")
    replacements <- lapply(matches, function(match) {
      if (length(match) > 0) {
        as.numeric(match)
      } else {
        NA
      }
    })
  }
  
  # If replacements is not empty, replace the column with the numeric values; otherwise, keep the original column
  if (!all(sapply(replacements, is.na))) {
    column <- unlist(replacements)
  }
  
  return(column)
}

# Apply the function to the "stars," "reviews_amount," and "price" columns in the dataframe
spf_cleaned <- spf %>%
  mutate(stars = clean_and_replace(stars),
         reviews_amount = clean_and_replace(reviews_amount),
         price = clean_and_replace(price))


# Convert the 'price' column to numeric for spf_cleaned
spf_cleaned <- spf_cleaned %>%
  mutate(price = as.numeric(stringi::stri_replace_all_regex(price, "[^0-9.]", "")))

# Remove leading/trailing whitespaces in the 'name' column for spf_cleaned
spf_cleaned <- spf_cleaned %>%
  mutate(name = stringi::stri_trim(name))



# Create a new column 'bronzers'
spf_cleaned <- spf_cleaned %>%
  mutate(bronzers = ifelse(stri_detect_fixed(ingredients, "Dihydroxyacetone"), "Yes", "No"))
# Create 'stars_category' column, filtering out NA values
spf_cleaned <- spf_cleaned %>%
  filter(!is.na(stars)) %>%
  mutate(stars_category = cut(stars, breaks = c(0, 1.9, 2.9, 3.9, 4.9, 5.0), labels = c("1-2", "2-3", "3-4", "4-5", "5")))
# Create a new column for price categories
spf_cleaned <- spf_cleaned %>%
  mutate(price_category = cut(price, breaks = c(0, 10, 20, 30, 40, 50, Inf), labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50+")))


# Function to check for harmful ingredients
check_safety <- function(ingredients) {
  if (any(!is.na(ingredients) & stri_detect_fixed(ingredients, "Dihydroxyacetone", negate = TRUE))) {
    harmful_ingredients <- c("Oxybenzone", "Octinoxate", "Retinyl Palmitate", "Homosalate", "Octocrylene")
    
    # Count the number of harmful ingredients present
    num_harmful_ingredients <- sum(stri_detect_fixed(ingredients, harmful_ingredients))
    
    # Return the count
    return(num_harmful_ingredients)
  } else {
    return("Not Applicable")
  }
}

# Apply the function to the dataframe
spf_cleaned <- spf_cleaned %>%
  mutate(harmful_ingredients_count = sapply(ingredients, check_safety))



# Function to check for harmful ingredients
check_safety <- function(ingredients, bronzers) {
  if (is.na(bronzers)) {
    return("Not Applicable")
  } else if (bronzers == "Yes") {
    return("Not Applicable")
  } else if (any(!is.na(ingredients) & stri_detect_fixed(ingredients, "Dihydroxyacetone", negate = TRUE))) {
    harmful_ingredients <- c("Oxybenzone", "Octinoxate", "Retinyl Palmitate", "Homosalate", "Octocrylene")
    
    # Check if any harmful ingredient is present
    if (any(stri_detect_fixed(ingredients, harmful_ingredients))) {
      return("Not Safe")
    }
  }
  # If no harmful ingredient is present or it's not a bronzer, return "Safe"
  return("Safe")
}

# Apply the function to the dataframe
spf_cleaned <- spf_cleaned %>%
  mutate(safety = mapply(check_safety, ingredients, bronzers))


# Function to clean and replace STARS
clean_and_replace <- function(column) {
  # Check if the column is "stars"
  if (identical(column, spf$stars)) {
    # Extract numeric values from "stars"
    matches <- stri_extract_all_regex(column, "\\d+\\.\\d")
    replacements <- lapply(matches, function(match) {
      if (length(match) > 0) {
        as.numeric(match)
      } else {
        NA
      }
    })
  } else {
    # For other columns, return as is
    return(column)
  }
  
  # If replacements is not empty, replace the column with the numeric values; otherwise, keep the original column
  if (!all(sapply(replacements, is.na))) {
    column <- unlist(replacements)
  }
  
  return(column)
}



# Apply the function to the "stars" column in the dataframe
spf_cleaned <- spf_cleaned %>%
  mutate(stars = clean_and_replace(stars))

# Plot 1: Comparison of safe and not safe sunscreens
spf_cleaned %>%
  filter(safety != "Not Applicable") %>%
  mutate(safety = as.factor(safety)) %>%
  ggplot(aes(x = safety)) +
  geom_bar(width = 0.7, fill = "skyblue", color = "white", position = "dodge") +  
  labs(title = "Comparison of the amount of safe and not safe sunscreens", x = "Safety", y = "Amount") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot 2: Create a bar plot for the star categories
spf_cleaned %>%
  ggplot(aes(x = stars_category)) +
  geom_bar(width = 0.7, fill = "salmon", color = "white", position = "dodge") +
  labs(title = "Star rating categories for tanning products",
       x = "Star Category",
       y = "Amount") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot 3: Correlation between rating and safety
spf_cleaned %>%
  filter(safety != "Not Applicable") %>%
  ggplot(aes(x = stars, y = harmful_ingredients_count, color = safety)) +
  geom_point(size = 3, alpha = 0.7) +  
  labs(title = "Correlation between stars rating and number of harmful ingredients for sunscreens",
       x = "Stars Rating",
       y = "Number of harmful ingredients") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Filter out rows with 'Not Applicable' and NA in the safety column
filtered_data <- spf_cleaned %>% 
  filter(safety != "Not Applicable" & !is.na(safety) & !is.na(price_category))

# Plot 4: Correlation plot between price categories and safety for sunscreens
ggplot(filtered_data, aes(x = price_category, fill = safety)) +
  geom_bar(position = "dodge", width = 0.7, color = "white") +  
  labs(title = "Correlation between price category and safety for sunscreens", x = "Price Category", y = "Amount of sunscreens") +
  scale_fill_manual(values = c("Safe" = "green", "Not Safe" = "red")) +  
  theme_minimal() +
  theme(legend.position = "bottom")  

# Plot 5: Price and Safety correlation plot
spf_cleaned %>%
  filter(safety != "Not Applicable") %>%
  ggplot(aes(x = price, y = harmful_ingredients_count, color = safety)) +
  geom_point(alpha = 0.7) +  
  labs(title = "Correlation between price and number of harmful ingredients",
       x = "Price",
       y = "Number of harmful ingredients") +
  theme_minimal() +
  theme(legend.position = "bottom",  
        plot.title = element_text(hjust = 0.5)) +  
  scale_color_manual(values = c("Safe" = "green", "Not Safe" = "red")) 


# Plot 6: Price and Reviews correlation plot 
spf_cleaned %>%
  filter(!is.na(reviews_amount) & safety != "Not Applicable") %>%
  ggplot(aes(x = price, y = reviews_amount, color = safety)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.2, height = 0)) +  # Add jitter
  labs(title = "Correlation between price and reviews amount",
       x = "Price",
       y = "Reviews amount") +
  theme_minimal() +
  theme(legend.position = "bottom",  
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Safe" = "green", "Not Safe" = "red")) 



# Plot 7: Reviews Amount and Safety correlation plot 
spf_cleaned %>%
  filter(safety != "Not Applicable" & !is.na(reviews_amount)) %>%
  ggplot(aes(x = reviews_amount, y = harmful_ingredients_count, color = safety)) +
  geom_point(alpha = 0.7) +  # Adjust transparency
  labs(title = "Correlation between reviews amount and number of harmful ingredients",
       x = "Reviews amount",
       y = "Number of harmful ingredients") +
  theme_minimal() +
  theme(legend.position = "bottom",  # Change legend position to the bottom
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  scale_color_manual(values = c("Safe" = "green", "Not Safe" = "red"))   


# Plot 8: Bar plot for price category and amount of all products
spf_cleaned %>%
  filter(!is.na(price_category)) %>%
  ggplot(aes(x = price_category)) +
  geom_bar() +
  labs(title = "Amount of products in each price category",
       x = "Price category",
       y = "Amount of products") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5))



# Save the cleaned dataframe to a CSV file
write_csv(spf_cleaned, "C:/Users/sandr/Desktop/Web scrapping/Project/spf_cleaned.csv")



# Display the cleaned data
head(spf)
view(spf_cleaned)
