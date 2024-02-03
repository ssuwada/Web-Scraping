library(tidyverse)
library(rvest)
library(httr)
library(dplyr)

## Assign your own main path
setwd("/Users/sebastiansuwada/Desktop/R/Scrapper")

source('scrapper.R')

### MAIN

#https://www.ulta.com/

baseURL <- "https://www.ulta.com/shop/body-care/suncare?page="
totalPages <- 5  
pageProducts <- list()
pageProductsTemp <- list()

# Get products pages
for (pageCurr in 1:totalPages) {
  pageUrl <- getPageUrl(baseURL, pageCurr)
  Sys.sleep(1)
  pageProductsTemp <- getResponseProductUrls(pageUrl)
  pageProducts <- c(pageProducts,pageProductsTemp)
}

pageProducts <- unlist(pageProducts)
pageProducts


## Get information from the product

# Create an empty data frame to get all of the results
allProductInfo <- data.frame(name = character(),
                             stars = character(),
                             reviews_amount = character(),
                             price = character(),
                             ingredients = character(),
                             stringsAsFactors = FALSE)

# Main loop for scrapping data of the products - SPFs
for (pageCurr in 1:length(pageProducts)) {
  pageUrl <- pageProducts[pageCurr]
  Sys.sleep(1)
  responseProductInformations <- getResponseProductPagesScrapp(pageUrl)
  productInfo <- getProductInfromation(responseProductInformations, pageCurr)
  
  # Append the current product information to the data frame
  allProductInfo <- rbind(allProductInfo, productInfo)
}


# Save the data frame to a CSV file
write.csv(allProductInfo, "spfCreams.csv", row.names = FALSE) %>%
  cat("Product information saved to 'product_information.csv'\n") %>%
  cat("CSV file is saved under this path: ", getwd() , "\n")

