#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(shinydashboard)
require(fiftystater)
require(dplyr)
require(mapproj)
require(ggplot2)



# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "PredictIt Politics Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="home"),
      menuItem("The House", tabName = "house"),
      menuItem("The Senate", tabName = "senate"),
      menuItem("The Executive", tabName = "executive")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("PredictIt Political Analysis"),
              fluidRow(
                box(htmlOutput("intro", inline=TRUE), width=12)
              )
      ),
      tabItem(tabName = "house",
              h2("The House of Representatives"),
              fluidRow(
                box(htmlOutput("house_overview", inline=TRUE), width=12)
              ),
              fluidRow(
                box(plotOutput("gcb_plot", width="100%", height="300px"),
                    htmlOutput("gcb_analysis", inline=TRUE), width=12)
              )
      ),
      
      tabItem(tabName = "senate",
              h2("The Senate"),
              fluidRow(
                box(plotOutput("pre2018_senate", width = "100%", height = "300px"), 
                    htmlOutput("pre2018_analysis", inline=TRUE),
                    width=6
                ),
                box(plotOutput("predictit_2018_senate", width = "100%", height = "300px"), 
                    htmlOutput("post2018_analysis", inline=TRUE),
                    width=6)
              ),
              fluidRow(
                box(
                  htmlOutput("senate_facts", inline=TRUE),
                  # Dropdown menu for selecting state for analysis
                  selectInput("senate_state", "State:",
                                c("Select State" = "",
                                  "Arizona" = "arizona", 
                                  "California"="california", 
                                  "Connecticut"="connecticut",
                                  "Delaware"="delaware",
                                  "Florida"="florida",
                                  "Hawaii"="hawaii",
                                  "Indiana"="indiana",
                                  "Maine"="maine", 
                                  "Massachusetts"="massachusetts", 
                                  "Michigan"="michigan", 
                                  "Minnesota"="minnesota",
                                  "Mississippi"="mississippi", 
                                  "Missouri"="missouri", 
                                  "Montana"="montana", 
                                  "Nebraska"="nebraska", 
                                  "Nevada"="nevada", 
                                  "New Jersey"="new jersey", 
                                  "New Mexico"="new mexico", 
                                  "New York"="new york", 
                                  "North Dakota"="north dakota", 
                                  "Ohio"="ohio",
                                  "Pennsylvania"="pennsylvania", 
                                  "Rhode Island"="rhode island", 
                                  "Tennessee"="tennessee", 
                                  "Texas"="texas", 
                                  "Utah"="utah", 
                                  "Vermont"="vermont", 
                                  "Virginia"="virginia", 
                                  "Washington"="washington", 
                                  "West Virginia"="west virginia", 
                                  "Wisconsin"="wisconsin",
                                  "Wyoming"="wyoming")),
                    plotOutput("state_plots", width = "100%", height = "300px"),
                    htmlOutput("senate_opinion", inline=TRUE),
                    width=12
                )
          
              )
      ),
      
      tabItem(tabName = "executive",
              h2("The Presidency"),
              fluidRow(
                box(htmlOutput("pres_intro", inline=TRUE), width=12)
              ),
              fluidRow(
                box(plotOutput("dem_primary", width="100%", height="300px"),
                    htmlOutput("dem_pres_thoughts", inline=TRUE), 
                    width=12)
              ),
              fluidRow(
                box(plotOutput("rep_primary", width="100%", height="300px"), 
                    htmlOutput("rep_pres_thoughts", inline=TRUE),
                    width=12)
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Introduction page explaining project
  output$intro <- renderText({
    paste("<img src='https://pima-county-schools.s3.amazonaws.com/CMS/17/elections__hero.gif' width=100% height=50%><br><br>",
          "<p>Since Summer 2015, politics has permeated the minds of many moreso than it has in a long, long time.",
          "Donald Trump's shocking victory in 2016 left Democrats scrambling to retain any sort of power they had. In November 2018, the midterm elections will be happening, which is the first opportunity the country will have to give a referendum on Trump's presidency.",
          "If Donald Trump and the Republicans manage to hold on to the House and Senate, they will likely be emboldened and continue their attempts to repeal the Affordable Care Act and enact their other legislative priorities.",
          "If not, it could spell trouble for President Trump and his agenda. <br><br>",
          "This project attemps to analyze what people believe about what will happen during the midterm elections utilizing real time data from a political gambling website called PredictIt.",
          "This website functions essentially as a stock market for politics, where an event is framed as a contract, and people can buy 'yes' or 'no' shares on that contract.",
          "For example, one contract might be \"Will Joe Biden win the Democratic nomination for the Presidency in 2020?\" and people can sell shares of 'yes' or 'no' for whatever price they'd like between $1.00 and $0.01.",
          "In some ways, this gives a true reflection of what people's beliefs are about what's going to happen in this country, rather than just pundits giving their uninformed opinions, as on PredictIt, people are actually putting their money where their mouths are. <br><br>",
          "Specifically, I give an analysis of the U.S. House election using real time data from Real Clear Politics with their General Congressional Ballot polling data. Unfortunately, there are too many races to cover each one individually.",
          "Next, I give an analysis of each Senate race. Finally, I give an analysis on what people are thinking about for the 2020 election.<br><br>",
          "Hopefully this project can serve as a way to keep up with the ever changing political landscape of this country.",
          "</p>")
  })
  # House Analysis
  # Gives an overview of the current standing of the House
  gcb <- read.csv("gcb.csv")
  output$house_overview <- renderText({
    paste("<center><img src='https://upload.wikimedia.org/wikipedia/commons/d/de/US_House_Composition_as_of_April_2018.svg'></center>",
          "<br><p>Currently, the House of Representatives has 235 Republican members, 193 Democratic members, and 7 vacant seats.",
            "A majority is needed to elect the speaker of the House, who controls what legislation gets to the floor.",
            "In order for Democrats to get a majority of seats in the next House of Representatives, they will need gain a net of 25 seats to reach 218 seats.")
  })
  
  # Draws a plot of the General Congressional Ballot
  output$gcb_plot <- renderPlot({
    ggplot(gcb[1:90,], aes(as.Date(Date), group=1)) +                    
      geom_line(aes(y=Democrat_Value), colour="blue") +  
      geom_line(aes(y=Republican_Value), colour="red") + 
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title="General Congressional Ballot: Last 90 Days", x = "Date", y = "Percent")
  })
  
  # My own analysis of the GCB
  output$gcb_analysis <- renderText({
    paste("<p>Democrats have a clear lead in the General Congressional Ballot. The most recent average from Real Clear Politics shows Democrats with a lead of ",
          toString(gcb$Spread[1]), ".</p> <p>Unfortunately, such a lead may not be enough for them - gerrymandering in house districts has made the path to a house majority quite difficult for Democrats. That said, it's far from impossible. Experts agree (whatever that means) that Democrats will need a lead in the popular vote of approximately 6-8 percent.</p>")
  })
  # Senate Analysis
  # Introduction to the Senate races
  output$senate_facts <- renderText({
    paste("<p>Having a majority in the Senate is arguably much important than the House, at least in modern times. This is due to the fact that confirmation of cabinet members, district judges, and Supreme Court judges now only require a majority of votes in the Senate to be confirmed.",
          "In 2012, then Senate Majority Leader Harry Reid initiated the so called \"nuclear option\" on cabinet members due to unprecedented obstruction of President Obama's appointees, and current Senate Majority Leader Mitch McConnell did something similar for Supreme Court judges early in Trump's presidency to appoint Neil Gorsuch.",
          "Thus, it is a top priority of Republicans to retain control of the Senate.</p>",
          "<br><br> <p>This years map heavily advantages the Republican party, with Democrats defending ten Senate seats in states won by Donald Trump. To learn more about each race, select one of the states below.</p>")
  })
  
  # Information for drawing map of current senate
  dems_sen <- c("california", "connecticut", "delaware", "florida", "hawaii", "indiana", "maryland", "massachuesetts", "michigan", "minnesota", "missouri", "montana", "new jersey", "new mexico", "new york", "north dakota", "ohio", "pennsylvania", "rhode island",  "virginia", "washington", "west virginia", "wisconsin")
  reps_sen <- c("arizona", "mississippi", "nebraska", "nevada","tennessee", "texas", "utah", "wyoming")
  ind_sen <- c("maine", "vermont")
  
  # List of all states to ensure every state gets drawn
  states <- c("arizona","california", "connecticut", "delaware", "florida", "hawaii", "indiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "minnesota", "mississippi", "mississippi", "missouri", "montana","nebraska", "nevada", "new jersey", "new mexico", "new york", "north dakota", "ohio", "pennsylvania", "rhode island", "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming")
  
  # Information about each race: The incumbent and the Sabato rating for the race
  incumbents <- c("Open", "Dianne Feinstein (D)", "Chris Murphy (D)", "Tom Carper (D)", "Bill Nelson (D)", "Mazie Hirono (D)", "Joe Donnelly (D)", "Angus King (I)", "Ben Cardin (D)", "Elizabeth Warren (D)", "Debbie Stabenow (D)", "Tina Smith (D)", "Amy Klobuchar (D)", "Open", "Roger Wicker (R)", "Claire McCaskill (D)", "Jon Tester (D)", "Deb Fischer (R)", "Dean Heller (R)", "Bob Menendez (D)", "Martin Heinrich (D)", "Kirsten Gillibrand (D)", "Heidi Heitkamp (D)", "Sherrod Brown (D)", "Bob Casey (D)", "Sheldon Whitehouse (D)", "Open", "Ted Cruz (R)", "Open", "Bernie Sanders (I)", "Tim Kaine (D)", "Maria Cantwell (D)", "Joe Manchin (D)", "Tammy Baldwin (D)", "John Barrasso (D)" )
  sabato_rating <- c("Toss-up", "Safe D", "Safe D", "Safe D", "Toss-up", "Safe D", "Toss-up", "Safe D/I", "Safe D", "Safe D", "Likely D", "Likely D", "Safe D", "Likely R","Safe R", "Toss-up", "Leans D", "Safe R", "Toss-up", "Likely D", "Safe D", "Safe D", "Toss-up", "Leans D", "Likely D", "Safe D", "Likely R", "Likely R", "Safe R", "Safe D/I", "Safe D", "Safe D", "Leans D", "Leans D", "Safe R")
  
  # These are all my personal thoughts and analysis on each race, which can be seen by looking click through the dropdown menu.
  az_thoughts <- paste("Following Jeff Flake announcing of his retirement, this seat opened up to become one of the key pickup opportunities for Democrats this Fall.",
                       "Arizona was one of the few states in 2016 to swing towards Democrats. The Democratic nominee wil likely be U.S. Representative Kyrsten Sinema.",
                       "On the Republican side, things are less clear. Veteran Martha McSally, State Senator Kelli Ward, and pardonned former Sheriff Joe Arpaio are all competing for the nomination, with no clear front runner yet.")
  ca_thoughts <- paste("Though this seat may is almost certainly safe for Democrats, it may not be for incumbent Senator Dianne Feinstein. She faces a challenger on the left from State Senate Majority Leader Kevin DeLeon.",
                       "Current polling shows her with a lead against him, but if she gets too complacent, she could end up like one of the many primaried Republican candidates during the Tea Party wave.")
  ct_thoughts <- paste("Chris Murphy is a one term Senator who was elected with 55% of the vote in 2012. This is a relatively uncompetitive seat and will almost certainly remain held by the incumbent Senator.")
  de_thoughts <- paste("Tom Carper is a three term Senator who was elected with 66% of the vote in 2012. This is a relatively uncompetitive seat and will almost certainly remain held by the incumbent Senator.")
  fl_thoughts <- paste("Three term Senator Bill Nelson is facing his toughest opponent yet in Governor Rick Scott this Fall. Bill Nelson was reelected with 55% of the vote in 2012. Though Florida swung towards Trump in 2016, Bill Nelson's previous victories have all been larger than any of Rick Scott's, who barely won his elections in the Republican wave years of 2010 and 2014.",
                       "Still, this will be a tough race for the both of them with no clear frontrunner.")
  hi_thoughts <- paste("Mazie Hirono is a one term Senator who was elected with 63% of the vote in 2012. This is a relatively uncompetitive seat and will almost certainly remain held by the incumbent Senator.")
  in_thoughts <- paste("As one of the ten Democrats in states Trump won in 2016, one term Joe Donnelly is considered a prime pickup opportunity for Republicans this fall.",
                       "Some considered his 2012 win in deep red Indiana a stroke of luck due his opponents controversial gaffes. This year, he'll get his chance to show if he really knows what he's doing.")
  me_thoughts <- paste("One of two independents in the U.S. Senate, one term Senator Angus King was last elected in a three way race with 53% of the vote in 2012.",
                       "Angus King caucuses with the Democrats. Given Hillary Clinton won Maine in 2016, it's unlikely he will lose this race.")
  md_thoughts <- paste("Ben Cardin is a two term Senator who was elected with 56% of the vote in 2012. This is a relatively uncompetitive seat and will almost certainly remain held by the incumbent Senator.")
  ma_thoughts <- paste("A former Harvard Law professor, Elizabeth Warren is a one term Senator who was elected with 54% of the vote in 2012, defeating Republican Scott Brown after his surprise victory in the Massachusetts U.S. Senate special election in 2010.",
                       "Known for her outspoken stances on bank regulations, she fits the mold for her state and faces few serious competitors and will likely be reelected this Fall.")
  mi_thoughts <- paste("Incumbent Senator Debbie Stabenow is a three term Senator who was elected with 59% of the vote. She is one of the ten Democrats running in states won by Donald Trump in 2016.",
                       "However, luckily for her, Michigan the state Donald Trump won with his smallest margin of victory, so she will likely be reelected this Fall.")
  mn_special_thoughts <- paste("Following the retirement of Al Franken from the senate due to sexual misconduct allegations, Lieutenant Governor Tina Smith was temporarily appointed to fill this senate position.",
                               "Though the whole midwest swung towards Trump in 2016, Minnesota still voted for Hillary Clinton, and in a environment favorable to Democrats she will likely see victory this Fall.",
                               "However, this victory is far from guaranteed - the incumbency advantaged from being appointed versus winning an election tends to be much less significant.")
  mn_thoughts <- paste("Two term incumbent Senator Amy Klobuchar was last elected with 65% of the vote. She is often considered a rising star and is a potential candidate in 2020, though she has made nothing official herself.",
                        "Seeing as she won by such a large margin in 2012, it's unlikely she'd lose now in such a favorable environment.")
  ms_special_thoughts <- paste("Normally, this race wouldn't be worth paying any attention to, but following the loss of victory of Doug Jones over Roy Moore in December 2012's special election in Alabama, Republicans are on their toes as they're poised to nominate another controversial candidate.",
                               "This special election is happening due to the retirment of U.S. Senator Thad Cochran, who is retiring due to health reasons.",
                               "Republican Cindy Hyde-Smith was appointed to the fill the seat, but is being challenged in the primary by the controversial Chris Daniels.",
                               "Hyde smith seems poised to win the primary, but it is far from assured as of now.")
  ms_thoughts <- paste("Roger Wicker is a one term Senator who won his last election with 57% of the vote. This is a relatively uncompetitve election and will likey be won by the incumbent.")
  mo_thoughts <- paste("Considered the most vulnerable incumbent Democrat this fall, two term Senator Claire McCaskill has quite the battle ahead of her this fall in a state Trump won by over 20%. ",
                       "Her first victory was in the Democratic wave of 2006. In 2012, she was quite the underdog, but following her opponent's comment about how the \"female body shuts down legitimate rape,\" she soared to victory.",
                       "This year, she faces Missouri Attorney General Josh Hawley, a fierce opponent. However, she may have luck on her side once again with the scandal embroiling the Missouri Governor's office likely to help her out this fall.")
  mt_thoughts <- paste("Jon Tester is vulnerable incumbent who recently came under fire from President Trump for tanking his VA nominee Ronny Jackson. However, Jon Tester is popular in Montana, which has a track record of bipartisanship  voting for both Donald Trump and a Democratic governor in 2016.")
  ne_thoughts <- paste("After being elected to her first term in 2012, Deb Fischer replaced a Democrat. It's extremely unlikely she'll be losing this fall.")
  nv_thoughts <- paste("Considered the most vulnerable Republican incumbent, Dean Heller has quite a battle ahead of him as the only Republican Senate candidate in from a state won by Hillary Clinton in 2016. He faces Representative Jacky Rosen. If Democrats want to take the Senate, winning this seat is a must.")
  nj_thoughts <- paste("After his mistrial for corruption charges last year, many questioned Bob Menendez was out for the count. In a more Republican friendly year, he may have been, but he will more likely cruise to reelection this fall in the current this current environment that favors Democrats.")
  nm_thoughts <- paste("Martin Heinrich is a one term Senator who won his last election with 51% of the vote. This is a relatively uncompetitve election and will likey be won by the incumbent.")
  ny_thoughts <- paste("Kirsten Gillibrand is a one term Senator who was last elected with 72% of the vote. This seat is not very competitive and she will likely win again this fall.")
  nd_thoughts <- paste("One term Senator Heidi Heitkamp barely won her first election in 2012, and this one won't be any easier. This seat is being targetted as a prime pickup opporuntity for Republicans, and in such a cheap media market as North Dakota, it won't be difficult to flood the TV with plenty of ads for either side. This seat is crucial for both sides to win.")
  oh_thoughts <- paste("Sherrod Brown, the staunch progressive from Ohio, is a popular incumbent in a state Trump won in 2016. In such a favorable environment, it's quite likely he'll be reelected this fall, though it won't be an easy race.")
  pa_thoughts <- paste("Two term Senator Bob Casey was last elected with 54% of the vote. Though Pennsylvania voted for Donald Trump in 2016, Bob Casey remains popular and will likely win his reelection.")
  ri_thoughts <- paste("Sheldon Whitehouse is a two term Senator who won his last election with 64% of the vote. This is a relatively uncompetitve election and will likey be won by the incumbent.")
  tn_thoughts <- paste("After Bob Corker announced his retirement, Republican Representative Marsha Blackburn hopped into the race. Originally, her victory was assured as Tennessee voted for Donald Trump by a large margin.",
                       "However, popular former Governor Phil Bredesen hopped in, and polling has showed it to be a close race. This will be a key race to determine control of the Senate.")
  tx_thoughts <- paste("Ted Cruz, one of America's most hateable Senators, is likely to cruise to reelection this fall, but he does face a formidable challenger, Representative Beto O'Rourke. In a wave election, this is a possible pickup for Democrats, though it will be quite difficult.")
  ut_thoughts <- paste("Following Senator Orrin Hatch announcing his retirment, this seat opened up, but not really as a pickup opportunity for Democrats. Former Massachusetts governor and presidential candidate Mitt Romney is poised to win the GOP nomination for the Senate, after which he will likely cruise to victory this fall.")
  vt_thoughts <- paste("Bernie Sanders, one of two independents in the U.S. Senate, is up for reelection following his loss in the 2016 Democratic Primary for the presidency.",
                       "Considered one of the most popular politicians in America, he won his last race with 71% of the vote and will likely win again this fall.")
  va_thoughts <- paste("The former Democratic Vice Presidential Nominee is up for reelection this year, and though Hillary Clinton's victory in this state in 2016 was close, the margins widened in the 2017 gubernatorial election, so Tim Kaine has little to fear this coming fall.")
  wa_thoughts <- paste("Maria Cantwell is a three term Senator who won her last election with 61% of the vote. She received an electoral vote in the 2016 presidential from a faithless Washington elector, and will likely be reelected again this fall considering how much Washington has swung left in recent years.")
  wv_thoughts <- paste("Joe Manchin is running for reelection in a state Trump won with 68% of the vote, so this will be a tough seat for Democrats to defend. However, a chaotic GOP primary and the fact that Joe Manchin remains popular in his state means there is still hope for the incumbent Senator.")
  wi_thoughts <- paste("As the hearts of some millions raised and some millions dropped on election day 2016 when Wisconsin was called for Donald Trump, few likely felt it as hard as U.S. Senator Tammy Baldwin. She is first openly gay Senator in U.S. history and votes with her concurrently serving Senator Ron Johnson less than any other pair of U.S. Senators from a state. Still, Donald Trump's margin in 2016 was small, and if Democrats want any hope of taking the Senate this year, this is a must win seat that they will likely retain.")
  wy_thoughts <- paste("John Barrasso is a one term Senator who was last elected with 76% of the vote. Coming from the state Donald Trump won by his largest margin, it is almost guaranteed John Barrasso will be reelected this fall.")
  thoughts <- c(az_thoughts, ca_thoughts, ct_thoughts, de_thoughts, fl_thoughts, hi_thoughts, in_thoughts, me_thoughts,  md_thoughts,  ma_thoughts, mi_thoughts, mn_special_thoughts,mn_thoughts, ms_special_thoughts, ms_thoughts, mo_thoughts, mt_thoughts, ne_thoughts, nv_thoughts, nj_thoughts, nm_thoughts, ny_thoughts, nd_thoughts,  oh_thoughts,pa_thoughts,ri_thoughts,tn_thoughts,tx_thoughts,ut_thoughts,vt_thoughts,  va_thoughts,wa_thoughts,  wv_thoughts, wi_thoughts, wy_thoughts)
  
  # Draws PredictIt senate map
  pi_senate <- read.csv("senate.csv")
  senate_odds <- pi_senate$Odds
  senate_analysis <- data.frame(State=states, Incumbent=incumbents, Rating=sabato_rating, Thoughts=thoughts)
  output$pre2018_senate <- renderPlot({
    ggplot(fifty_states, aes(x=long, y=lat, group = group)) +
      geom_polygon(fill="grey", colour = "black") + 
      geom_polygon(fill="blue", color='black', data = filter(fifty_states, id %in% dems_sen)) + 
      geom_polygon(fill="red", color='black', data = filter(fifty_states, id %in% reps_sen)) +
      geom_polygon(fill="yellow", color='black', data = filter(fifty_states, id %in% ind_sen)) +
      expand_limits(x = fifty_states$long, y = fifty_states$lat) +
      coord_map() +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = NULL) + 
      scale_y_continuous(breaks = NULL) +
      labs(title="Current Senate Map", x = "", y = "")
  })
  
  # Text intro after showing current senate map
  output$pre2018_analysis <- renderText({
    paste("<p>The is the current senate map. Republicans hold 51 seats, while 47 Democrats and 2 Independents who caucus with Democrats hold the rest. 51 seats, or 50 seats plus the vice president, are needed for a majority, so Republicans can only afford to lose a net of one seat this fall.</p>")
  })
  
  # Draws the Senate map based on PredictIt share values
  output$predictit_2018_senate <- renderPlot({
    ggplot(pi_senate, aes(map_id = State)) + 
      # map points to the fifty_states shape data
      geom_map(aes(fill = Odds), colour="black", map = fifty_states) + 
      expand_limits(x = fifty_states$long, y = fifty_states$lat) +
      coord_map() +
      scale_x_continuous(breaks = NULL) + 
      scale_y_continuous(breaks = NULL) +
      theme_classic() +
      labs(title="PredictIt Midterm Probability Map", x = "", y = "") +
      scale_fill_gradientn(colors=c("red", "lightpink","purple", "cornflowerblue", "blue"))
  })
  
  # Little analysis of the predicted results
  output$post2018_analysis <- renderText ({
    paste("This is the predicted results of the U.S. Senate races in November by PredictIt users, using the Best valued share sold today as a probability the candidate will win.")
  })
  
  # Draws a map of an individual state depending on what the user has chosen
  output$state_plots <- renderPlot({
    if (input$senate_state != "") {
      ggplot(pi_senate, aes(map_id = State)) + 
        # map points to the fifty_states shape data
        geom_map(aes(fill = Odds), colour="black", map = fifty_states %>% filter( id == input$senate_state)) + 
        expand_limits(x = filter(fifty_states, id == input$senate_state)$long, y = filter(fifty_states, id == input$senate_state)$lat) +
        coord_map() +
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
            panel.background = element_blank()) + 
        scale_fill_gradientn(colors=c("red", "lightpink", "purple", "cornflowerblue", "blue"))
      }
  })
  
  # Writes the information for each senate race.
  output$senate_opinion <- renderText({
    if (input$senate_state != "") {
      paste("<p><img src='" , (pi_senate %>% filter(State==input$senate_state) %>% select(Image))[[1]] , "' align='left' width='180px' height='180px' style='margin-right:10px'>",
            '<b>Incumbent</b>: ', (senate_analysis %>% filter(State==input$senate_state) %>% select(Incumbent))[[1]],
            '<br> <b>Sabato Rating</b>: ', (senate_analysis %>% filter(State==input$senate_state) %>% select(Rating))[[1]],
            '<br>', (senate_analysis %>% filter(State==input$senate_state) %>% select(Thoughts))[[1]], "</p><br><br>" )
    }
  })
  
  # Presidency Analysis
  candidates <- read.csv("candidates.csv")
  
  # Filters whether a potential candidate is a Democrat or a Republican
  dem_pres <- candidates %>% filter(Symbol=="DNOM20")
  rep_pres <- candidates %>% filter(Symbol=="RNOM20")
  
  # Draws the plot for the Democratic Primary
  output$dem_primary <- renderPlot({
    ggplot(data=dem_pres, aes(x=reorder(Name, -BestBuyYesCost), y=BestBuyYesCost)) +
      geom_bar(stat="identity", fill="steelblue")+
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
      labs(x = "Candidate", y = "Share Value")
  })
  
  # Draws the plot for the Republican primary
  output$rep_primary <- renderPlot({
    ggplot(data=rep_pres, aes(x=reorder(Name, -BestBuyYesCost), y=BestBuyYesCost)) +
      geom_bar(stat="identity", fill="firebrick3")+
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
      labs(x = "Candidate", y = "Share Value")
  })
  
  # Intro text to President section
  output$pres_intro <- renderText({
    paste("<p>Though in reality it's much too early to be thinking about 2020, that doesn't stop people from doing it anyway.",
          "PredictIt's most popular market remains to be predictions of who will be elected President in 2020 with the parties' respective primaries in a close second.",
          "Below, you'll see a real time analysis of who's leading the race in each party.</p>")
  })
  
  # After thoughts on Democratic primary
  output$dem_pres_thoughts <- renderText({
    paste("<p>Above is the current potential Democratic candidates listed in order of who has the highest share 'yes' share price on PredictIt.",
          "The top three is a tight race between California Senator Kamala Harris, Vermont Senator and former presidential candidate Bernie Sanders, and Former Vice President Joe Biden, with no clear frontrunner amongst the three of them.",
          "Thus, it's likely this will be a volatile list and will change a lot once the primaries actually begin.",
          "</p>")
  })
  
  # After thoughts on Republican primary
  output$rep_pres_thoughts <- renderText({
    paste("<p>Above is the current potential Republican candidates listed in order of who has the highest share 'yes' share price on PredictIt.",
          "Unsurprisingly, Donald Trump leads the pack here. Surprsinginly, however, is the fact that his share is only valued at a little more than $0.50. This is likely based on the fact that many believe the President won't make it through his first term, or that he will be challenged in the 2020 primary.",
          "In any case, it's clear Donald Trump doesn't have the absolute confidence of PredictIt users.",
          "</p>")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

