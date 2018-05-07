require(httr)
require(jsonlite)
# Make HTTP GET request, transform data into JSON, and then to R dataframe.
gcb_request <- GET("https://www.realclearpolitics.com/epolls/json/6185_historical.js?1453388629140&callback=return_json")
gcb_str <- substring(content(gcb_request, 'text', encoding = 'UTF-8'), 13, nchar(content(gcb_request, 'text', encoding = 'UTF-8'))-2)
gcb_dirty <- as.data.frame(fromJSON(gcb_str)$poll$rcp_avg)
gcb_clean <- data.frame()

# Clean data a little
for (i in 1:nrow(gcb_dirty)) {
  date_str <- substring(toString(gcb_dirty$date[[i]]),6,nchar(toString(gcb_dirty$date[[i]]))-15)
  date <- as.Date(strptime(date_str, format="%d %b %Y"))
  dem_val <- c(as.double(gcb_dirty$candidate[[i]]$value[1]))
  rep_val <- c(as.double(gcb_dirty$candidate[[i]]$value[2]))
  spread <- c(dem_val - rep_val)
  gcb_organized <- data.frame(Date=date, Democrat_Value=dem_val, Republican_Value=rep_val, Spread=spread)
  
  gcb_clean <- rbind(gcb_clean, gcb_organized)
}

# Write to CSV
write.csv(gcb_clean, file="gcb.csv")
