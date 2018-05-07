require(httr)
require(jsonlite)
require(dplyr)

# Make HTTP GET request, transform data into JSON, and then to R dataframe.
senate_request <- GET("https://www.predictit.org/api/marketdata/group/54", add_headers(Accept = "application/json"))
senate_str <- content(senate_request, 'text', encoding = 'UTF-8')
senate_markets <- as.data.frame(fromJSON(senate_str)$Market)
senate_contracts <- data.frame()
for (i in 1:nrow(senate_markets)) {
  contract <- as.data.frame(senate_markets[i,]$Contracts)
  contract$Market <- senate_markets[i,]$Name
  senate_contracts <- rbind(senate_contracts, contract)
}

# PredictIt gives us a lot of different markets that don't matter - we only care about a few
marketIDs = c(4332,5313,5264,5368,5369,5563,5564,7266,7267,7270,7271,7287,7288,7308,7309,7332,7635,8173,8314,8455,9906,9907,9908,9733)
states = c("all", "massachusetts", "texas", "vermont", "west virginia", "indiana", "florida", "north dakota", "montana", "missouri", "wisconsin", "michigan", "ohio", "new jersey", "new mexico", "maine", "nevada", "tennessee", "arizona", "california", "mississippi", "nebraska", "pennsylvania", "minnesota")
senate_unsort <- senate_contracts %>% filter(ID %in% marketIDs)
senate_unsort$State <- states

# Some of the data gets returned in a strange way and must be cleaned.
senate_clean <- senate_unsort %>% select(Image, Name, BestSellYesCost, State)
senate_clean[1,]$Name <- "Will Republicans control the Senate after 2018?"
senate_clean[18,]$Name <- "Will the Democratic party win the Tennessee U.S. Senate election in 2018?"
senate_clean[19,]$Name <- "Will the Democratic party win the Arizona U.S. Senate election in 2018?"
senate_clean[21,]$Name <-"Will the Republican party win the Mississippi U.S. Senate special election in 2018?"

# Certain races that are uncompetitive are left out, so I've added them here to have a complete list
leftout_images <- c("https://pbs.twimg.com/profile_images/871076844904361985/7NorOQHB_400x400.jpg", "https://pbs.twimg.com/profile_images/378800000497501114/77d2bd85a246e66e8f77670018fbaaca_400x400.jpeg", "https://pbs.twimg.com/profile_images/943313772785164288/svG7qRs8_400x400.jpg","https://pbs.twimg.com/profile_images/931526777410850816/jYej2QIV_400x400.jpg","https://pbs.twimg.com/profile_images/1815362035/AJK_Twitter_400x400.jpg","https://pbs.twimg.com/profile_images/694231794602872832/UdbuYlbe_400x400.jpg", "https://pbs.twimg.com/profile_images/378800000051321410/55ff7ed4f6264c8eec19a81a66be6bb0_400x400.png","https://pbs.twimg.com/profile_images/964489716622966784/xbtCe2td_400x400.jpg","https://pbs.twimg.com/profile_images/693192439046103041/I5UrYsCi_400x400.jpg","https://pbs.twimg.com/profile_images/1143907394/twitter_profile_pic_400x400.JPG", "https://pbs.twimg.com/profile_images/828637094260121600/VpJGxC8b_400x400.jpg","https://pbs.twimg.com/profile_images/922472118377476096/XqCk_uDv_400x400.jpg")
leftout_names <- c("Will Chris Murphy be re-elected to the U.S. Senate in Connecticut in 2018?", "Will Tom Carper be re-elected to the U.S. Senate in Delaware in 2018?", "Will Mazie Hirono be re-elected to the U.S. Senate in Hawaii in 2018?", "Will Ben Cardin be re-elected to the U.S. Senate in Maryland in 2018?", "Will Amy Klobuchar be re-elected to the U.S. Senate in Minnesota in 2018?", "Will Roger Wicker be re-elected to the U.S. Senate in Mississippi in 2018?", "Will Sheldon Whitehouse be re-elected to the U.S. Senate in Rhode Island in 2018?", "Will Mitt Romney be elected to the U.S. Senate in Utah in 2018?", "Will Maria Cantwell be re-elected to the U.S. Senate in Washington in 2018?", "Will John Barrasso be re-elected to the U.S. Senate in Wyoming in 2018?", "Will Tim Kaine be re-elected to the U.S. Senate in Virginia in 2018?", "Will Kirsten Gillibrand be re-elected to the U.S. Senate in New York in 2018?")
leftout_odds <- c(1,1,1,1,1,1,1,1,1,1,1,1)
leftout_states <- c("connecticut", "delaware", "hawaii", "maryland", "minnesota", "mississippi", "rhode island", "utah", "washington", "wyoming", "new york", "virginia")
leftout <- data.frame(Image=leftout_images, Name=leftout_names, BestSellYesCost=leftout_odds, State=leftout_states)
almost_done <- rbind(senate_clean, leftout)
senate <- almost_done[order(almost_done$State),]
row.names(senate) <- 1:nrow(senate)
colnames(senate)[3] <- "Odds"

# In order to map this later, we need to have a consistent scale, so for Republican incumbents, I flip their odds
# Therefore, a value closer to 0 is more likely to be a Republican victory, and a value closer to 1 is more likely Democratic.
senate$Odds[15] <- 1 - senate$Odds[15]
senate$Odds[16] <- 1 - senate$Odds[16]
senate$Odds[19] <- 1 - senate$Odds[19]
senate$Odds[20] <- 1 - senate$Odds[20]
senate$Odds[29] <- 1 - senate$Odds[29]
senate$Odds[30] <- 1 - senate$Odds[30]
senate$Odds[36] <- 1 - senate$Odds[36]

# Some images are bad and we need new ones
senate$Image[2] <- "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9d/Flag_of_Arizona.svg/1200px-Flag_of_Arizona.svg.png"
senate$Image[15] <- "https://www.gettysburgflag.com/media/catalog/product/cache/2/small_image/460x368/9df78eab33525d08d6e5fb8d27136e95/m/i/mississippi_1.jpg"
senate$Image[28] <- "http://www.usa4kids.com/images/bigflags/tennessee.gif"

# Fill in states with no election so ensure a good looking map
no_election_states <- c("oregon", "idaho", "colorado", "south dakota", "kansas", "oklahoma", "iowa", "arkansas", "lousiana", "illinois", "kentucky", "alabama", "georgia", "north carolina", "south carolina", "new hampshire")
no_election_odds <- rep(c(NA), times=length(no_election_states))
no_election_names <- no_election_odds
no_election_images <- no_election_odds

senate <- rbind(senate, data.frame(Image=no_election_images, Name=no_election_names, Odds=no_election_odds, State=no_election_states))

# Write to CSV file
write.csv(senate, file="senate.csv")

