require(httr)
require(jsonlite)
require(dplyr)

# Make HTTP GET requests
dem_request <- GET("https://www.predictit.org/api/marketdata/ticker/DNOM20", add_headers(Accept = "application/json"))
rep_request <- GET("https://www.predictit.org/api/marketdata/ticker/RNOM20", add_headers(Accept = "application/json"))

# Transform Dem data into JSON, and then into R dataframe
dem_str <- content(dem_request, 'text', encoding = 'UTF-8')
dem_market <- as.data.frame(fromJSON(dem_str))
dem <- dem_market %>% select(TickerSymbol, Contracts.Name, Contracts.BestBuyYesCost, Image)

# Transofrm Rep data into JSON, and then into R dataframe
rep_str <- content(rep_request, 'text', encoding = 'UTF-8')
rep_market <- as.data.frame(fromJSON(rep_str))
rep <- rep_market %>% select(TickerSymbol, Contracts.Name, Contracts.BestBuyYesCost, Image)

# Combine datasets, fix column names, and write to CSV
candidates <- rbind(dem, rep)
colnames(candidates) <- c("Symbol", "Name", "BestBuyYesCost", "Image")
write.csv(candidates, file="candidates.csv")