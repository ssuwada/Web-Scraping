library(tidyverse)
library(rvest)
library(httr)
library(dplyr)


## Access to pages with suncreams

# Get link for all of pages that consists products
getPageUrl <- function(baseURL, pageCurr) {
  pageUrl <- sprintf("%s%d", baseURL, pageCurr)
  return(pageUrl)
}

# Function to get product URLs from a page main page ++
getProductURLs <- function(response) {
  ContentSpfs <- content(response, type = "text/html", encoding = "UTF-8")
  
  productURLs <- ContentSpfs %>% 
    html_nodes('.ProductListingResults__productCard a') %>% 
    html_attr("href") %>% 
    unique()
  
  return(productURLs)
}

# Extract information about page and return it also check if page of products list response
getResponseProductUrls <- function(url) {
  response <- tryCatch({
    httr::GET(url, user_agent= "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:108.0) Gecko/20100101 Firefox/108.0")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(response)) {
    cat(" ========= Error accessing ======== :", url, "\n")
  } else {
    cat("Page accessed successfully:", url, "\n")
    listofProductPage <- getProductURLs(response)
    return(listofProductPage)
  }
}

## Get products information

getProductInfromation <- function(response, pageCurr) {
  
  cat("Scrapping product number:", pageCurr, "\n")
  
  ContentSpfs <- content(response, type = "text/html", encoding = "UTF-8")
  
  nameProduct <- ContentSpfs %>% 
    html_node('div.ProductInformation h1') %>%
    html_text()
  
  starsProduct <- ContentSpfs %>% 
    html_node('div.ReviewStars span') %>%
    html_text()
  
  reviewsAmountProduct <- ContentSpfs %>% 
    html_node('div.ReviewStars a') %>%
    html_text()
  
  priceProduct <- ContentSpfs %>% 
    html_node('div.ProductPricing span') %>%
    html_text()
  
  ingredientsProduct <- ContentSpfs %>% 
    html_node("details:nth-child(3)") %>%
    html_text()
  
  productInfo <- data.frame(
    name = nameProduct,
    stars = starsProduct,
    reviews_amount = reviewsAmountProduct,
    price = priceProduct,
    ingredients = ingredientsProduct
  )
  
  cat(" ======================================================== \n")

  return(productInfo)
}

getResponseProductPagesScrapp <- function(url) {
  response <- tryCatch({
    httr::GET(url, user_agent= "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:108.0) Gecko/20100101 Firefox/108.0")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(response)) {
    cat("Error accessing:", url, "\n")
  } else {
    cat("Page accessed successfully:", url, "\n")
    return(response)
  }
}
