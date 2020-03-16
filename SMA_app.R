# Import Packages

if (!require("tm")) install.packages("tm", quiet=TRUE) ; require("tm")
if (!require("wordcloud")) install.packages("wordcloud", quiet=TRUE) ; require("wordcloud")
if (!require("memoise")) install.packages("memoise", quiet=TRUE) ; require("memoise")
if (!require("wordcloud2")) install.packages("wordcloud2", quiet=TRUE) ; require("wordcloud2")
if (!require("shinydashboard")) install.packages("shinydashboard", quiet=TRUE) ; require("shinydashboard")
if (!require("shinyWidgets")) install.packages("shinyWidgets", quiet=TRUE) ; require("shinyWidgets")
if (!require("treemapify")) install.packages("treemapify", quiet=TRUE) ; require("treemapify")
if (!require("ggplotify")) install.packages("ggplotify", quiet=TRUE) ; require("ggplotify")
if (!require("packcircles")) install.packages("packcircles", quiet=TRUE) ; require("packcircles")
if (!require("irlba")) install.packages("irlba", quiet=TRUE) ; require("irlba")
if (!require("base64enc")) install.packages("base64enc", quiet=TRUE) ; require("base64enc")
if (!require("httr")) install.packages("httr", quiet=TRUE) ; require("httr")
if (!require("tidytext")) install.packages("tidytext", quiet=TRUE) ; require("tidytext")
if (!require("plyr")) install.packages("plyr", quiet=TRUE) ; require("plyr")
if (!require("jsonlite")) install.packages("jsonlite", quiet=TRUE) ; require("jsonlite")
if (!require("dplyr")) install.packages("dplyr", quiet=TRUE) ; require("dplyr")
if (!require("scales")) install.packages("scales", quiet=TRUE) ; require("scales")
if (!require("rtweet")) install.packages("rtweet", quiet=TRUE) ; require("rtweet")
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; require("stringr")
if (!require("lubridate")) install.packages("lubridate", quiet=TRUE) ; require("lubridate")
if (!require("RColorBrewer")) install.packages("RColorBrewer", quiet=TRUE) ; require("RColorBrewer")
if (!require("SnowballC")) install.packages("SnowballC", quiet=TRUE) ; require("SnowballC")
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")
if (!require("ggrepel")) install.packages("ggrepel", quiet=TRUE) ; require("ggrepel")
if (!require("tidyr")) install.packages("tidyr", quiet=TRUE) ; require("tidyr")
if (!require("widyr")) install.packages("widyr", quiet=TRUE) ; require("widyr")
if (!require("broom")) install.packages("broom", quiet=TRUE) ; require("broom")
if (!require("udpipe")) install.packages("udpipe", quiet=TRUE) ; require("udpipe")
if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")
if (!require("readr")) install.packages("readr", quiet=TRUE) ; require("readr")
if (!require("plotly")) install.packages("plotly", quiet=TRUE) ; require("plotly")
if (!require("shiny")) install.packages("shiny", quiet=TRUE) ; require("shiny")
if (!require("leaflet")) install.packages("leaflet", quiet=TRUE) ; require("leaflet")
if (!require("DT")) install.packages("DT", quiet=TRUE) ; require("DT")



#Load the Dataset
#setwd("C:/Users/pborchert/Documents/IESEG BDA/Social Media Analysis/")

boa_db <- read.csv("bankofamerica.csv", header=TRUE, stringsAsFactors = FALSE)
timeline <- read.csv("timeline.csv", header=TRUE, stringsAsFactors = FALSE)

# Topics identified from the LDA model
x_topics <- read.csv("topics_lda.csv", header=TRUE, stringsAsFactors = FALSE)

# User dataset -> filtered for tweets that are not from BoA accounts
user <- boa_db %>% filter(!screen_name %in% unique(timeline$screen_name))

# Combined Dataset of the followers across Bank of America Twitter Accounts
follower <- read.csv("follower_0205.csv", header=TRUE, stringsAsFactors = FALSE)

ui <- dashboardPage(
    #change the skin layout color
    skin = "red",
    dashboardHeader(title = "Bank Of America Dashboard", titleWidth = 300),
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            menuItem("Summary", tabName = "summary", icon = icon("list-alt")),
            menuItem("Wordcloud", tabName = "wordcloud", icon = icon("cloudversify")),
            menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("comments")),
            menuItem("Buzz", tabName = "topic", icon = icon("bullhorn")),
            menuItem("Follower Profile", tabName = "profile", icon = icon("dna")),
            menuItem("Dataset", tabName = "dataset", icon = icon("table"))
        )
    ),
    dashboardBody(
        # change the size of all icons on the app
        
        tags$head(
            tags$style(HTML(".fa { font-size: 20px;}"))
        ),
        
        # end
        tabItems(
            # 1st tab content
            tabItem(tabName = "summary",
                    
                    # 1st row
                    
                    fluidRow(
                        box(title = "Bank of America",
                            valueBoxOutput("avglikesSumm"),
                            valueBoxOutput("avgtweetsSumm"),
                            valueBoxOutput("avgcommentsSumm"),
                            plotOutput("plot1", height = 100)
                        ),
                        
                        box(
                            title = "Users",
                            valueBoxOutput("avglikesUser"),
                            valueBoxOutput("avgtweetsUser"),
                            valueBoxOutput("avgcommentsUser"),
                            plotOutput("plot2", height = 100)
                        )
                    ),
                    # 2nd Row
                    fluidRow(
                        
                        box(
                            sliderInput("summary_date", 
                                        h3("Select Date Range:"),
                                        min = min(ymd(as.Date(boa_db$created_at))), max = max(ymd(as.Date(boa_db$created_at))), 
                                        value = c(min(ymd(as.Date(boa_db$created_at))), max(ymd(as.Date(boa_db$created_at))))),
                            
                            radioButtons("boa_acc", h3("Twitter Accounts Bank of America:"),
                                         choices = list("All" = "All", "BankofAmerica" = "BankofAmerica",
                                                        "BofA_Careers" = "BofA_Careers", 
                                                        #"BofA_Community" = "BofA_Community", #Account has no Tweets in 2020
                                                        "BofA_Help" = "BofA_Help","BofA_News" = "BofA_News", "BofA_Tips" = "BofA_Tips", 
                                                        "MerrillLynch" = "MerrillLynch"), selected = "All"),
                            width=2
                        ),
                        
                        box(title="Number of Tweets x Source",
                        plotOutput("bubble_tweets", height=350),
                        width=10)
                        
                    ),
                    
                    # 2nd Row
                    
                    fluidRow(
                        box(
                            title= "Avg Tweets per Weekday",
                            plotlyOutput("weekdayBoa", height=250)
                        ),
                        box(
                            title= "Avg Tweets per Weekday",
                            plotlyOutput("weekdayUser", height=250)
                        )
                    ),
                    # 3rd Row
                    fluidRow(
                        box(
                            title= "Avg Tweets per Hour",
                            plotlyOutput("hourBoa", height=250)
                        ),
                        box(
                            title= "Avg Tweets per Hour",
                            plotlyOutput("hourUser", height=250)
                        )
                           
                        
                    )
            ),
            
            # 2nd tab content
            tabItem(tabName = "wordcloud",
                    # h2("Wordcloud tab Body"),
                    
                    fluidRow(
                        
                        box(
                            selectInput("selectionBoa", "Choose a Wordcloud type for Bank Of america:",
                                        choices = list("Unigram", "Bigram")),
                            # actionButton("update", "Change"),
                            hr(),
                            sliderInput("freqBoa",
                                        "Minimum Frequency:",
                                        min = 1,  max = 50, value = 15),
                            sliderInput("maxBoa",
                                        "Maximum Number of Words:",
                                        min = 1,  max = 300,  value = 100)
                        ),
                        box(
                            selectInput("selectionUsers", "Choose a Wordcloud type for Users:",
                                        choices = list("Unigram", "Bigram")),
                            # actionButton("update", "Change"),
                            hr(),
                            sliderInput("freqUsers",
                                        "Minimum Frequency:",
                                        min = 1,  max = 50, value = 15),
                            sliderInput("maxUsers",
                                        "Maximum Number of Words:",
                                        min = 1,  max = 300,  value = 100)
                        )
                    ),
                    
                    fluidRow(
                        
                        box(
                            # Show Word Cloud
                            title = "From Bank Of America",
                            plotOutput(outputId = "plotBoaWc") # Use wordcloud2Output for Wordcloud2
                        ),
                        box(
                            # Show Word Cloud
                            title = "From Users",
                            plotOutput(outputId = "plotUsersWc") # Use wordcloud2Output for Wordcloud2
                        )
                    ),
                    
                    fluidRow(
                        
                        box(
                            title = "Most common Words",
                            plotOutput("common_words_boa", height=250)
                        ),
                        
                        box(
                            title = "Most common Words",
                            plotOutput("common_words_user", height=250)
                        )
                    )
            ),

            
            # 3rd tab content
            tabItem(tabName = "sentiment",
                    h2("Sentiment: How do our Users communicate?"),
                    
                    fluidRow(
                            box(title="Sentiment Score",
                            plotOutput("sentiment", height=350), 
                            width=10 
                            ),
                            box(
                                sliderInput("sentiment_date", 
                                            h3("Select Date Range:"),
                                            min = min(ymd(as.Date(boa_db$created_at))), max = max(ymd(as.Date(boa_db$created_at))), 
                                            value = c(min(ymd(as.Date(boa_db$created_at))), max(ymd(as.Date(boa_db$created_at))))),
                                width=2
                            )

                        
                    ),
                    fluidRow(
                        box(
                            title="Sentiment over Time",
                            plotOutput("sentiment_time", height=350)
                        ),
                        box(
                            title="Contribution to Sentiment",
                            plotOutput("sentiment_pos_neg", height=350)
                        )
                    ),
                    fluidRow(
                        box(
                            title="Engagement Bank of America",
                            plotlyOutput("engagement", height=350),
                            width=8
                        )
                    )
            ),
            # 4th tab content
            tabItem(tabName = "topic",
                    h2("Topics: What are our Users talking about?"),
                    
                    fluidRow(
                        
                        box(
                            checkboxGroupInput("topics", 
                                               h3("Choose Topics to compare:"), 
                                               choices = list("Topic 1" = 1, 
                                                              "Topic 2" = 2, 
                                                              "Topic 3" = 3,
                                                              "Topic 4" = 4,
                                                              "Topic 5" = 5),
                                               selected = 1),
                            width=2), 
                        
                        box(
                            title="Topics: Most used Terms",
                            plotOutput("topic_terms", height=350),
                            width=10
                        )

                    ),

                    fluidRow(
                        box(
                            title="Topic Similarities",
                            plotOutput("topic_vec", height=550),
                            width=10
                        )
                    )
            ),
            tabItem(tabName = "profile",
                    h2("Followers Profile"),
                    # Followers location map
                    fluidRow(
                        box(title="Followers Location",
                            leafletOutput("follower_map", "100%", 450),
                            width=8
                        ),
                        box(title = "Followers' Twitter Behaviour",
                            valueBoxOutput("f_avgfollowerSum"),
                            valueBoxOutput("f_avgfriendSum"),
                            valueBoxOutput("f_avgstatusSum"),
                            width = 4
                        )
                    ),
                    # Country ranking of followers activity
                    fluidRow(
                        box(
                            sliderInput("country_topn", 
                                        h3("Select Number of Countries:"),
                                        min = 1, max = 20, 
                                        value = c(1, 5)),
                            width = 3
                        ),
                        box(title="Followers: Most Active Countries",
                            plotlyOutput("country_rank", height=350),
                            width=9)
                        
                    )
            ),
            
            # 6th tab content
            tabItem(tabName = "dataset",
                    h2("Data Tables"),
                    fluidRow(
                        box(
                            h3("Bank of America:"),
                            DTOutput("table_boa",height = 200),
                            width = 12
                            
                        ),
                        box(
                            h3("Users:"),
                            DTOutput("table_user", height=200),
                            width = 12
                        ),
                        box(
                            h3("Followers:"),
                            DTOutput("table_follower", height=200),
                            width = 12
                            
                        )
                    )
                    
            )
        )
    )
)
server <- function(input, output) {
    
    #--------------------------------------------------------Summary-------------------------------------------------------
    
    # Value boxes for Bank of America
    
    output$avglikesSumm <- renderValueBox({
        
        inp <- input$summary_date
        inp_acc <- input$boa_acc
        if (inp_acc != "All"){
            x <- timeline %>% filter(screen_name == inp_acc)
        } else {
            x <- timeline
        }
        
        x <- x %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        avg_like <- round(mean(x$favourites_count),1) 
        
        valueBox(
            value = tags$p("Avg Likes", style = "font-size: 35%;"), 
            subtitle = tags$p(avg_like, style = "font-size: 75%;"),
            icon = icon("thumbs-up"),
            color = "green"
        )
    })
    
    output$avgtweetsSumm <- renderValueBox({
        
        inp <- input$summary_date
        inp_acc <- input$boa_acc
        if (inp_acc != "All"){
            x <- timeline %>% filter(screen_name == inp_acc)
        } else {
            x <- timeline
        }
        
        x <- x %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        avg_tweets <- round(mean(x$retweet_count),1)
        
        valueBox(
            value = tags$p("Avg Retweets", style = "font-size: 35%;"), 
            subtitle = tags$p(avg_tweets, style = "font-size: 75%;"),
            icon = icon("twitter"),
            color = "green"
        )
    })
    
    output$avgcommentsSumm <- renderValueBox({
        
        # Original Tweet = status_id that has no value in reply_to_user_id column
        # (not a comment)
        
        inp <- input$summary_date
        inp_acc <- input$boa_acc
        if (inp_acc != "All"){
            x <- timeline %>% filter(screen_name == inp_acc)
        } else {
            x <- timeline
        }
        
        x <- x %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        nr_original <- nrow(x %>% dplyr::filter(is.na(reply_to_user_id)==TRUE) )
        
        perc_original <- round(nr_original / nrow(x), 3) * 100
        
        valueBox(
            value = tags$p("Original Tweets (%)", style = "font-size: 35%;"), 
            subtitle = tags$p(paste0(perc_original,"%"), style = "font-size: 75%;"),
            icon = icon("comments"),
            color = "green"
        )
    })
    
    # Value boxes for users
    
    output$avglikesUser <- renderValueBox({
        
        inp <- input$summary_date
        
        
        x <- user %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        avg_like <- round(mean(x$favourites_count),0)
        
        valueBox(
            value = tags$p("Avg Likes", style = "font-size: 35%;"), 
            subtitle = tags$p(avg_like, style = "font-size: 75%;"),
            icon = icon("thumbs-up"),
            color = "yellow"
        )
    })
    
    output$avgtweetsUser <- renderValueBox({
        
        inp <- input$summary_date
        
        
        x <- user %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        avg_tweets <- round(mean(x$retweet_count),1)
        
        valueBox(
            value = tags$p("Avg Retweets", style = "font-size: 35%;"), 
            subtitle = tags$p(avg_tweets, style = "font-size: 75%;"),
            icon = icon("twitter"),
            color = "yellow"
        )
    })
    
    output$avgcommentsUser <- renderValueBox({
        
        inp <- input$summary_date
        
        
        x <- user %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        nr_original <- nrow(x %>% dplyr::filter(is.na(reply_to_user_id)==TRUE) )
        
        perc_original <- round(nr_original / nrow(x), 3) * 100
        
        valueBox(
            value = tags$p("Original Tweets (%)", style = "font-size: 35%;"), 
            subtitle = tags$p(paste0(perc_original, "%"), style = "font-size: 75%;"),
            icon = icon("comments"),
            color = "yellow"
        )
    })
    
    output$weekdayUser <- renderPlotly({
        
        inp <- input$summary_date
        
        x <- user %>% select(status_id, created_at) %>% 
            mutate(time = ymd(as.Date(created_at)))
        
        nr_weeks <- as.numeric(difftime(max(x$time),min(x$time),units="weeks"))
        x$wd <- weekdays(x$time)
        sort_wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")
        
        x <- x %>% filter((time >= inp[1]) & (time <= inp[2])) %>%
            dplyr::group_by(wd) %>%  dplyr::summarise(count = dplyr::n(), avg = dplyr::n()/nr_weeks)
        
        # ggplot(x, aes(x=reorder(wd, match(wd, sort_wd)), y=avg, alpha=avg)) + geom_bar(stat="identity", fill = "#fda73d") + ##0f9fff
        #     scale_alpha(range = c(0.2, 1), guide=FALSE) +
        #     ylab(NULL) +
        #     xlab(NULL)+
        #     theme(panel.background = element_rect(fill="white"))
        
        plot_ly(x, x = ~reorder(x$wd, match(x$wd, sort_wd)), y = ~avg, type = 'bar',
                marker = list(color = "#fda73d",
                              line = list(color = 'rgb(158, 199, 255)', width = 0),
                              opacity=~rescale(avg, to=c(0.4,1)))) %>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
        
    })
    
    output$weekdayBoa <- renderPlotly({
        
        inp <- input$summary_date
        inp_acc <- input$boa_acc
        if (inp_acc != "All"){
            x <- timeline %>% filter(screen_name == inp_acc)
        } else {
            x <- timeline
        }
        
        x <- x %>% select(status_id, created_at) %>% mutate(time = ymd(as.Date(created_at)))
        nr_weeks <- as.numeric(difftime(max(x$time),min(x$time),units="weeks"))
        x$wd <- weekdays(x$time)
        sort_wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")
        
        x <- x %>% filter((time >= inp[1]) & (time <= inp[2])) %>%
            dplyr::group_by(wd) %>% dplyr::summarise(count = dplyr::n(), avg = dplyr::n()/nr_weeks)
        
        # ggplot(x, aes(x=reorder(wd, match(wd, sort_wd)), y=avg, alpha=avg)) + geom_bar(stat="identity", fill = "#43c21c") + ##0f9fff
        #     scale_alpha(range = c(0.3, 1), guide=FALSE) +
        #     ylab(NULL) +
        #     xlab(NULL)+
        #     theme(panel.background = element_rect(fill="white"))
        
        plot_ly(x, x = ~reorder(x$wd, match(x$wd, sort_wd)), y = ~avg, type = 'bar',
                marker = list(color = "#43c21c",
                              line = list(color = 'rgb(158, 199, 255)', width = 0),
                              opacity = ~rescale(avg, to=c(0.4,1)))) %>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    })
    
    output$hourUser <- renderPlotly({
        
        inp <- input$summary_date
        
        #select user dataset and calculate total number of hours (for avg)
        x <- user %>% select(status_id, created_at) %>% mutate(time = ymd_hms(created_at))
        nr_hour <- as.numeric(difftime(max(x$time),min(x$time),units="hour"))/24
        x$hour <- hour(x$time)
        
        # Group  by hour and calculate the average
        x <- x %>% filter((ymd_hms(time) >= inp[1]) & (ymd_hms(time) <= inp[2])) %>%
            dplyr::group_by(hour) %>%  dplyr::summarise(count = dplyr::n(), avg = dplyr::n()/nr_hour)
        #Plot
        # ggplot(x, aes(x=hour, y=avg, alpha=avg)) + geom_bar(stat="identity", fill = "#fda73d") + ##0f9fff
        #     scale_alpha(range = c(0.2, 1), guide=FALSE) +
        #     ylab(NULL) +
        #     xlab(NULL)+
        #     theme(panel.background = element_rect(fill="white"))
        
        plot_ly(x, x = ~hour, y = ~avg, type = 'bar',
                marker = list(color = "#43c21c",
                              line = list(color = 'rgb(158, 199, 255)', width = 0),
                              opacity = ~rescale(avg, to=c(0.4,1)))) %>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    })
    
    output$hourBoa <- renderPlotly({
        
        inp <- input$summary_date
        inp_acc <- input$boa_acc
        if (inp_acc != "All"){
            x <- timeline %>% filter(screen_name == inp_acc)
        } else {
            x <- timeline
        }
        
        #select timeline dataset and calculate total number of hours (for avg)
        x <- x %>% select(status_id, created_at) %>% mutate(time = ymd_hms(created_at))
        nr_hour <- as.numeric(difftime(max(x$time),min(x$time),units="hour"))/24
        x$hour <- hour(x$time)
        
        # Group  by hour and calculate the average
        x <- x %>% filter((ymd_hms(time) >= inp[1]) & (ymd_hms(time) <= inp[2])) %>%
            dplyr::group_by(hour) %>%  dplyr::summarise(count = dplyr::n(), avg = dplyr::n()/nr_hour)
        # Plot
        # ggplot(x, aes(x=hour, y=avg, alpha=avg)) + geom_bar(stat="identity", fill = "#43c21c") + ##0f9fff
        #     scale_alpha(range = c(0.2, 1), guide=FALSE) +
        #     ylab(NULL) +
        #     xlab(NULL)+
        #     theme(panel.background = element_rect(fill="white"))
        
        plot_ly(x, x = ~hour, y = ~avg, type = 'bar',
                marker = list(color = "#43c21c",
                              line = list(color = 'rgb(158, 199, 255)', width = 0),
                              opacity = ~rescale(avg, to=c(0.4,1)))) %>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    })
    
    
    
    output$bubble_tweets <- renderPlot({
        
        # Selected User specified Date and the given Twitter Account for BoA
        
        # ToDo Error Handling for Twitter Account with no tweets in this timeframe
        inp <- input$summary_date
        inp_acc <- input$boa_acc
        if (inp_acc != "All"){
            x <- timeline %>% filter(screen_name == inp_acc)
        } else {
            x <- timeline
        }

        user_filt <- user %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        n_user <- length(unique(user_filt$status_id))
        
        x <- x %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        x <- x %>% group_by(screen_name) %>% dplyr::summarise(count = dplyr::n()) %>% mutate(group = "Bank of America")
        x <- rbind(x, c("User", n_user, "User"))
        x$id <- seq(1,length(unique(x$screen_name)))
        x <- x %>% mutate(count = as.numeric(count), bubble_size = count) %>% arrange(count)
        
        # Increase Bubble size for it to not disappear in the plot
        x$bubble_size <- mapply(function(bubble_size) ifelse(bubble_size < 50, 50, bubble_size), x$bubble_size)
        
        # Generate the layout. This function return a dataframe with one line per bubble. 
        # It gives its center (x and y) and its radius, proportional of the value
        packing <- circleProgressiveLayout(x$bubble_size, sizetype='area')
        
        # Add spacing between the bubbles
        packing$radius <- 0.98*packing$radius
        
        # Add packing information to the data frame
        data <- cbind(x, packing)
        
        # add lines with length of the radius from the center to draw a circle
        dat.gg <- circleLayoutVertices(packing, npoints=50)
        
        #join the information from the dataframe
        dat.gg <- dat.gg %>% inner_join(x, by="id")
        
        # Plot it
        ggplot() + 
            #ggtitle("Number of Tweets x Source")+
            # Create the bubbles
            # x and y is swapped to create a horizontally oriented plot
            geom_polygon(data = dat.gg, aes(y, x, group = id, fill=group, alpha = count), colour = "black") +
            scale_fill_manual(values= c("#43c21c", "#fda73d"), labels = c("Bank of America","User"))+
            scale_alpha(range=c(0.4,1))+
            # Add text in the center of each bubble + control its size
            geom_text(data = data, aes(y, x,size=2, label = paste(screen_name,"\n", count)), check_overlap=TRUE) +
            theme_void() + 
            theme(legend.position="none",
                  plot.title = element_text(size=20, hjust = 0.5)) +
            coord_equal()
    })
    
    #------------------------------------------------Wordcloud Tab ------------------------------------------------------------
    # Bank of america
    
    output$plotBoaWc <- renderPlot({    # Use renderWordcloud2 for Wordcloud2
        if (input$selectionBoa == "Unigram"){
            
            boa_uni <- timeline %>% select(status_id, clean_text)
            
            rm_words <- c("america", "bank", "will", "can", "get", "one", "use", "make")
            
            boa_uni <- boa_uni %>% unnest_tokens(output = "word",
                                                 input = clean_text,
                                                 token = "words", drop=FALSE, to_lower=TRUE) %>%
                mutate(word = wordStem(word, language="english")) %>%
                filter(!word %in% get_stopwords()$word) %>% # Check for each of the words whether it is a stopword
                filter(!word %in% rm_words) %>%
                count(word , sort=TRUE)
            
            wordcloud(words = boa_uni$word, freq = boa_uni$n, min.freq = input$freqBoa,
                      max.words=input$maxBoa, random.order=FALSE, rot.per=0.35,
                      colors=brewer.pal(9, "YlGn")[3:9], scale=c(4,0.5))
            
            # color palette for words
            
            # color_range_number <- length(unique(ddf$word))
            # color <- colorRampPalette(brewer.pal(9,"YlGn")[3:9])(color_range_number)[factor(ddf$word)]
            # 
            # validate(need(ddf, "Awaiting data"))
            # wordcloud2(ddf, backgroundColor = "white", size = 0.5, color = color)
            
        }
        
        else if (input$selectionBoa == "Bigram"){
            
            tmln_bi <- timeline %>% select(status_id, clean_text)
            
            tmln_bi <- tmln_bi %>% unnest_tokens(output = "bigram",
                                                 input = clean_text,
                                                 token = "ngrams",n=2, drop=FALSE, to_lower=TRUE) %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>% # Split bigram into 2
                mutate(word1 = wordStem(word1, language="english"), word2 = wordStem(word2, language="english")) %>%
                filter(!word1 %in% get_stopwords()$word) %>% # Check for each of the words whether it is a stopword
                filter(!word2 %in% get_stopwords()$word) %>% # if they are, delete
                unite(bigram, word1, word2, sep = " ") %>%
                count(bigram , sort=TRUE)#%>%
            
            wordcloud(words = tmln_bi$bigram, freq = tmln_bi$n, min.freq = input$freqBoa,
                      max.words=input$maxBoa, random.order=FALSE, rot.per=0.35,
                      colors=brewer.pal(9, "YlGn")[3:9])
            
            # color_range_number <- length(unique(ddf$word))
            # color <- colorRampPalette(brewer.pal(9,"YlGn")[3:9])(color_range_number)[factor(ddf$word)]
            # 
            # validate(need(tmln_bi, "Awaiting data"))
            # wordcloud2(tmln_bi, color = color, backgroundColor = "white", size = 0.5) #color = "random-light", shape = 'star'
        }
    })
    
    # Users
    
    output$plotUsersWc <- renderPlot({ # Use renderWordcloud2 for Wordcloud2
        if (input$selectionUsers == "Unigram"){
            
            user_uni <- user %>% select(status_id, clean_text)
            
            rm_words <- c("america", "bank", "will", "can", "get", "one", "use", "make")
            
            user_uni <- user_uni %>% unnest_tokens(output = "word",
                                                   input = clean_text,
                                                   token = "words", drop=FALSE, to_lower=TRUE) %>%
                mutate(word = wordStem(word, language="english")) %>%
                filter(!word %in% get_stopwords()$word) %>% # Check for each of the words whether it is a stopword
                filter(!word %in% rm_words) %>%
                count(word , sort=TRUE)
            
            wordcloud(words = user_uni$word, freq = user_uni$n, min.freq = input$freqUsers,
                      max.words=input$maxUsers, random.order=FALSE, rot.per=0.35,
                      colors = brewer.pal(9, "YlOrRd")[3:9])
            
            # color palette for words
            
            # color_range_number <- length(unique(ddf$word))
            # color <- colorRampPalette(brewer.pal(9,"YlOrRd")[3:9])(color_range_number)[factor(ddf$word)]
            # 
            # validate(need(ddf, "Awaiting data"))
            # wordcloud2(ddf, backgroundColor = "white", size = 0.5, color = color)
            
        }
        
        else if (input$selectionUsers == "Bigram"){
            
            boa_bi <- user %>% select(status_id, clean_text)
            
            boa_bi <- boa_bi %>% unnest_tokens(output = "bigram",
                                               input = clean_text,
                                               token = "ngrams",n=2, drop=FALSE, to_lower=TRUE) %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>% # Split bigram into 2
                mutate(word1 = wordStem(word1, language="english"), word2 = wordStem(word2, language="english")) %>%
                filter(!word1 %in% get_stopwords()$word) %>% # Check for each of the words whether it is a stopword
                filter(!word2 %in% get_stopwords()$word) %>% # if they are, delete
                unite(bigram, word1, word2, sep = " ") %>%
                count(bigram , sort=TRUE)#%>%
            # cast_dtm(document = status_id, term = bigram,
            #          value = n, weighting = tm::weightTf) #tfidf can be used as well
            
            wordcloud(words = boa_bi$bigram, freq = boa_bi$n, min.freq = input$freqUsers,
                      max.words=input$maxUsers, random.order=FALSE, rot.per=0.35,
                      colors=brewer.pal(9, "YlOrRd")[3:9])
            
            # color palette for words
            
            # color_range_number <- length(unique(ddf$word))
            # color <- colorRampPalette(brewer.pal(9,"YlOrRd")[3:9])(color_range_number)[factor(ddf$word)]
            # 
            # validate(need(boa_bi, "Awaiting data"))
            # wordcloud2(boa_bi, color = color, backgroundColor = "white", size = 0.5) #, shape = 'star'
        }
    })
    
    
    output$common_words_user <- renderPlot({
        
        user_bigram <- user %>% select(status_id, clean_text)
        
        user_bigram <- user_bigram %>% unnest_tokens(output = "bigram",
                                           input = clean_text,
                                           token = "ngrams",n=2, drop=FALSE, to_lower=TRUE) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% # Split bigram into 2
            mutate(word1 = wordStem(word1, language="english"), word2 = wordStem(word2, language="english")) %>%
            filter(!word1 %in% get_stopwords()$word) %>% # Check for each of the words whether it is a stopword
            filter(!word2 %in% get_stopwords()$word) %>% # if they are, delete
            unite(bigram, word1, word2, sep = " ") %>%
            count(bigram , sort=TRUE)
        
        x <- user_bigram[1:10,]
        
        ggplot(x, aes(x=reorder(bigram, n), y=n)) +
            geom_bar(data = x, stat = "identity", fill="#fda73d", aes(alpha=rescale(n, to=c(0.2,1))))+ 
            #scale_alpha(range=c(0.2, 1)) +
            xlab(NULL) +
            ylab("Frequency")+
            coord_flip() +
            theme(panel.background = element_rect(fill="white"),
                  legend.position="none",
                  axis.ticks.y     = element_blank()
            )
        
    })
    
    output$common_words_boa <- renderPlot({
        
        user_bigram <- timeline %>% select(status_id, clean_text)
        
        user_bigram <- user_bigram %>% unnest_tokens(output = "bigram",
                                                     input = clean_text,
                                                     token = "ngrams",n=2, drop=FALSE, to_lower=TRUE) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% # Split bigram into 2
            mutate(word1 = wordStem(word1, language="english"), word2 = wordStem(word2, language="english")) %>%
            filter(!word1 %in% get_stopwords()$word) %>% # Check for each of the words whether it is a stopword
            filter(!word2 %in% get_stopwords()$word) %>% # if they are, delete
            unite(bigram, word1, word2, sep = " ") %>%
            count(bigram , sort=TRUE)
        
        ggplot(user_bigram[1:10,], aes(x=reorder(bigram, n), y=n, alpha=n)) +
            geom_bar(stat = "identity", fill="#43c21c")+
            scale_alpha(range=c(0.2, 1)) +
            xlab(NULL) +
            ylab("Frequency")+
            coord_flip() +
            theme(panel.background = element_rect(fill="white"),
                  legend.position="none",
                  axis.ticks.y     = element_blank()
            )
        
    })
    
    #---------------------------------------------------------------Sentiment-----------------------------------------------------------------
    
    output$sentiment <- renderPlot({
        
        inp <- input$sentiment_date
        
        user_filt <- user %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        # Rescale sentiment from user table to -1, 1
        sentiment_dict <- user_filt %>% 
            select(created_at, Sentiment_dictionary) %>%
            filter(!is.na(Sentiment_dictionary)) %>%
            mutate(time = created_at,
                   Sentiment = round(rescale(Sentiment_dictionary, to=c(-1, 1), from=c(-5,5)), 5))
        sentiment_dict$time <- as.POSIXct(ymd(as.Date(sentiment_dict$time)))
        
        #Group data by year, month, day
        sentiment_dict <- sentiment_dict %>% group_by(time) %>% dplyr::summarise(Sentiment = sum(Sentiment)) %>% mutate(Sentiment = rescale(Sentiment, to=c(-1,1), from=c(-35,35)))
        
        #Import sentiment per day from Machine Learning Model
        sentiment_ml <- read.csv("sentiment_ml.csv", header=TRUE, stringsAsFactors = FALSE)
        
        #convert date column to as.POSIXct date
        sentiment_ml$time <- as.POSIXct(ymd(as.Date(sentiment_ml$time)))
        
        #Join dictionary and ML approach 
        sentiment_comb <- sentiment_ml %>% inner_join(sentiment_dict, by="time", suffix=c(".ml", ".dict")) %>%
            mutate(Sentiment = (Sentiment.ml + Sentiment.dict)/2)
        
        #Plot Sentiment over time (days) for ML, Dictionary and combined approach
        #3 different Y parameters: Sentiment (combined), Sentiment.ml (Machine Learning), Sentiment.dict (Dictionary)
        ggplot(data = sentiment_comb, aes(x = time, y = Sentiment, color="comb")) + geom_line(size=1.5) + geom_point(size=3) +
            #ggtitle("Sentiment Score over Time")+
            ylab("Negative   /   Positive")+
            xlab(NULL)+
            ylim(-1,1)+
            geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.3) +
            geom_line(data=sentiment_comb, aes(x = time, y = Sentiment.ml, color="ml"), size=1.5) + 
            geom_point(data=sentiment_comb, aes(x = time, y = Sentiment.ml, color="ml"), size=3)+
            geom_line(data=sentiment_comb, aes(x = time, y = Sentiment.dict, color="dict"), size=1.5) + 
            geom_point(data=sentiment_comb, aes(x = time, y = Sentiment.dict, color="dict"), size=3)+
            theme(panel.background = element_rect(fill="white"),
                  panel.grid.major.y = element_line(color="grey", size = 0.1),
                  legend.position = c(0.85, 0.85),
                  plot.title = element_text(size=20, hjust = 0.5))+
            scale_colour_manual(name = NULL, 
                                values =c('comb'='#FD8D3C','ml'='#cc8400', "dict"="#FED976"), labels = c('Combined Approach','Dictionary Score', "Machine Learning Score"))
        
    })
    
    output$sentiment_time <- renderPlot({
        #show number of tweets over time (barchart)
        #color coded sentiment in the bar
        
        inp <- input$sentiment_date
        
        user_filt <- user %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        #Assign labels to the tweets with a sentiment score (>=0.7 is positive, <0.7 neutral >0.3, >=0.3 is negative)
        x_sen <- user_filt %>% select(status_id, created_at, Sentiment_dictionary) %>% 
            filter(!is.na(Sentiment_dictionary)) %>%
            mutate(Sentiment = round(rescale(Sentiment_dictionary, to=c(0, 1), from=c(-5,5)), 3),
                   label = ifelse(Sentiment >= 0.7, "2_positive", ifelse(Sentiment <= 0.3, "3_negative", "1_neutral")))
        
        #tweets without a sentiment scored by the dictionary are labelled as neutral
        x <- user_filt %>% select(status_id, created_at, Sentiment_dictionary) %>% 
            left_join(x_sen, by="status_id") %>% 
            mutate(time = created_at.x)
        x$label <- mapply(function(label) ifelse(is.na(label)==TRUE, "1_neutral", label), x$label)
        
        #Time format for plotting
        x$time <- as.POSIXct(ymd(as.Date(x$time)))
        
        #Plot with manual color scale
        ggplot(x, aes(x=time, fill=label)) + geom_bar(stat="count", alpha=0.7) +
            scale_fill_manual(values= c("#C8C8C8", "#00e600", "#ff3300"), labels = c("neutral","positive", "negative"))+
            ylab("Number of Tweets") +
            xlab(NULL)+
            labs(fill="Sentiment")+
            theme(panel.background = element_rect(fill="white"),
                  legend.position = c(0.85, 0.85))
        
        
    })
    
    output$sentiment_pos_neg <- renderPlot({
        
        inp <- input$sentiment_date
        
        user_filt <- user %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        boa_sen <- user_filt %>% select(status_id, created_at, clean_text)
        
        boa_sen <- boa_sen %>% unnest_tokens(output = "token",
                                             input = clean_text,
                                             token = "words", drop=FALSE, to_lower=TRUE) %>%
            filter(token != "trump") # Dictionary recognizes the term as the verb "trump" <> President of the US
        
        boa_sen1 <- inner_join(boa_sen, get_sentiments("bing"), by=c("token"="word"))
        
        summary_boa <- boa_sen1 %>%  count(token,sentiment,sort=TRUE) %>%
            group_by(sentiment) %>%
            top_n(10) %>%  
            arrange(n) %>%
            as.data.frame(stringsAsFactors=FALSE)
        
        # allows us to plot negative terms with a negative x (aesthetic reasoning)
        summary_boa$n <- mapply(function(n, sen) ifelse(sen == "negative", n * (-1), n), summary_boa$n, summary_boa$sentiment )
        
        summary_boa <- summary_boa %>%
            ungroup() %>%
            mutate(token = reorder(token, n))
        
        ggplot(summary_boa, aes(token, n, fill = sentiment)) +
            scale_fill_manual(values=c("#ff3300", "#00e600"))+
            geom_col(show.legend = FALSE) +
            labs(y = "Frequency",
                 x = "Negative  /   Positive") +
            ylim((max(summary_boa$n) * -1)/2, max(summary_boa$n))+
            geom_text(data= summary_boa, aes(label=token),
                      size=4, 
                      #position=position_stack(vjust=0.5),
                      y = ifelse(summary_boa$sentiment == "negative", 65, -60),
                      color = "black",
                      alpha=1)+
            theme(panel.background = element_rect(fill="white"),
                  axis.ticks.y     = element_blank(),
                  axis.text.y     = element_blank())+
            coord_flip()
        
    })
    
    #--------------------------------------------Topics (Buzz)------------------------------------------------------------------
    
    output$topic_terms <- renderPlot({
        
        # ud_model <- udpipe_download_model(language = "english")
        # ud_model <- udpipe_load_model(ud_model$file_model)
        # 
        # x <- udpipe_annotate(ud_model, x =user$clean_text)
        # x <- as.data.frame(x)
        # 
        # x_dtm <- x %>%
        #     filter(upos %in% c("NOUN", "ADJ")) %>%
        #     anti_join(stop_words, by=c("lemma" = "word")) %>%
        #     count(doc_id, lemma , sort=TRUE) %>%
        #     cast_dtm(document = doc_id, term = lemma,
        #              value = n, weighting = tm::weightTf)
        # 
        # x_lda <- LDA(x_dtm, k = 5,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )
        # x_topics <- tidy(x_lda, matrix = "beta")
        # 
        # #Save dataset with topics identified by LDA to csv
        # write.csv(x_topics,"topics_lda.csv",row.names=FALSE)
        
        # Read dataset with topics identified by LDA (Beginning of the script)
        
        x_topics <- x_topics %>% filter(term != "?") # Remove Punctuation, not removed by the cleaning steps
        
        # you can use the following code to get the top terms per topic
        top_tweet_terms <- x_topics %>%
            group_by(topic) %>%
            top_n(10, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)
        
        inp <- input$topics
        
        top_tweet_terms <- top_tweet_terms %>%
            dplyr::mutate(term = reorder_within(term, beta, topic),
                   c_alpha = 0.5)
        
        ggplot(top_tweet_terms, aes(term, beta, fill = factor(topic), alpha=ifelse(topic %in% inp, 2*c_alpha, c_alpha))) +
            scale_fill_manual(values= brewer.pal(6, "YlOrRd")[2:7])+
            geom_col(show.legend = FALSE) +
            facet_wrap(~ topic, scales = "free") +
            coord_flip() +
            scale_x_reordered()+
            theme(panel.background = element_rect(fill="white"))
        
    })
    
    output$topic_vec <- renderPlot({
        
        # #create context window with length 8
        # tidy_skipgrams <- user %>%
        #     unnest_tokens(ngram, clean_text, token = "ngrams", n = 8) %>%
        #     mutate(ngramID = row_number()) %>% 
        #     tidyr::unite(skipgramID, status_id, ngramID) %>%
        #     unnest_tokens(word, ngram)
        # 
        # #calculate unigram probabilities (used to normalize skipgram probabilities later)
        # unigram_probs <- user %>%
        #     unnest_tokens(word, clean_text) %>%
        #     count(word, sort = TRUE) %>%
        #     mutate(p = n / sum(n))
        # 
        # #calculate probabilities
        # skipgram_probs <- tidy_skipgrams %>%
        #     pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
        #     mutate(p = n / sum(n))
        # 
        # #normalize probabilities
        # normalized_prob <- skipgram_probs %>%
        #     filter(n > 20) %>%
        #     rename(word1 = item1, word2 = item2) %>%
        #     left_join(unigram_probs %>%
        #                   select(word1 = word, p1 = p),
        #               by = "word1") %>%
        #     left_join(unigram_probs %>%
        #                   select(word2 = word, p2 = p),
        #               by = "word2") %>%
        #     mutate(p_together = p / p1 / p2)
        # 
        # pmi_matrix <- normalized_prob %>%
        #     mutate(pmi = log10(p_together)) %>%
        #     cast_sparse(word1, word2, pmi)
        # 
        # #remove missing data
        # pmi_matrix@x[is.na(pmi_matrix@x)] <- 0
        # 
        # #run svd to create 2 dimensions
        # pmi_svd <- irlba(pmi_matrix, 2, maxit = 500)
        # 
        # #extract word vectors
        # word_vectors <- pmi_svd$u
        # rownames(word_vectors) <- rownames(pmi_matrix)
        # 
        # #transform into data frame
        # forplot<-as.data.frame(word_vectors)
        # forplot$word<-rownames(forplot)
        # 
        # save(forplot, file="C:/Users/pborchert/Documents/IESEG BDA/Social Media Analysis/deploy_app/forplot_topic.RData")
        
        # Saved as RData for Performance reasons
        load("forplot_topic.RData")
        
        # Read topics identified from LDA
        x_topics <- read.csv("topics_lda.csv", header=TRUE, stringsAsFactors = FALSE)
        
        #select top 15 words per topic (identified in LDA application before)
        #join dimensions (from svd) on top 15 words per topic to plot them
        x <- x_topics %>%
            group_by(topic) %>%
            top_n(15, beta) %>%
            ungroup() %>%
            arrange(topic, -beta) %>%
            filter(!duplicated(term)) %>%
            inner_join(forplot, by=c("term"="word")) %>%
            mutate(topic = as.character(topic))
        
        
        #user input (selecting the topic to highlight)
        inp <- input$topics
        
        #filter for topic, the user selected
        choose <- x %>% filter(topic %in% inp)
        
        #Computes the subset of points which lie on the convex hull of the set of points specified. Based on the topic the user selected
        find_hull <- function(df) df[chull(df$V1, df$V2), ]
        #apply function to selected dataframe. 
        #keeps resukt in data frame format
        hulls <- ddply(choose, "topic", find_hull)
        
        #plot the words 2 dimensions (x,y) created by svd
        #use hulls to highlight words belonging to a topic 
        #label displays the plotted word
        ggplot(x, aes(x=V1, y=V2, label=term, color=topic, fill=topic))+ geom_point()+
            geom_polygon(data = hulls, alpha = 0.5) +
            geom_text_repel(aes(label=ifelse(term %in% choose$term, term, "")), color="black")+
            theme(panel.background = element_rect(fill="white"),
                  legend.position = c(0.85, 0.85))+
            xlab("First Dimension Created by SVD")+
            ylab("Second Dimension Created by SVD")+
            scale_color_manual(values= brewer.pal(6, "YlOrRd")[2:7])+
            scale_fill_manual(values= brewer.pal(6, "YlOrRd")[2:7])
        
    })
    
    output$engagement <- renderPlotly({
        
        inp <- input$sentiment_date
        
        user_filt <- user %>% filter((ymd(as.Date(created_at)) >= inp[1]) & (ymd(as.Date(created_at)) <= inp[2]))
        
        #Assign labels to the tweets with a sentiment score (>=0.7 is positive, <0.7 neutral >0.3, >=0.3 is negative)
        x <- user_filt %>% select(status_id, created_at, Sentiment_dictionary) %>% 
            filter(!is.na(Sentiment_dictionary)==TRUE) %>%
            mutate(Sentiment = round(rescale(Sentiment_dictionary, to=c(0, 1), from=c(-5,5)), 3),
                   label = ifelse(Sentiment >= 0.7, "2_positive", ifelse(Sentiment <= 0.3, "3_negative", "1_neutral")))
        
        #Time format for plotting
        x$time <- as.POSIXct(ymd(as.Date(x$created_at)))
        
        boa_reply <- timeline %>% filter(is.na(reply_to_status_id)==FALSE) %>% mutate(time=created_at)
        
        boa_reply$time <- as.POSIXct(ymd(as.Date(boa_reply$time)))
        
        boa_reply <- boa_reply %>% group_by(time) %>% dplyr::summarise(nr_reply = n())
        
        x <- x %>% filter(label %in% c("2_positive", "3_negative")) %>%
            group_by(time) %>%
            dplyr::summarise(extreme = n()) %>%
            inner_join(boa_reply, by="time")
        
        graph <- ggplot(x, aes(nr_reply, extreme)) + 
            geom_point(data=x,aes(size=(extreme*nr_reply)+1), alpha=0.8) + 
            geom_smooth(span=0.5, alpha=0.2, method=stats::lm, level=0.5, fill=brewer.pal(6, "YlOrRd")[3], color=brewer.pal(6, "YlOrRd")[3])+
            theme(panel.background = element_rect(fill="white"),
                  panel.grid.major.y = element_line(color="grey", size = 0.05),
                  panel.grid.major.x = element_line(color="grey", size = 0.05))+
            ylab("Tweets with Extreme Sentiment") +
            xlab("Number of replies (BoA)")
        
        #Influence statistically not signficant -> more data?
        # linearMod <- lm(nr_reply ~ extreme, data=x)
        # summary(linearMod)
        
    })
    
    #---------------------------------------------------Follower Profile---------------------------------------------------------------
    
    # summary of followers
    output$f_avgfollowerSum <- renderValueBox({
        avg_flw <- round(sum(follower$followers_count)/nrow(follower),1)
        valueBox(
            value = tags$p("Avg Followers", style = "font-size: 55%;"), 
            subtitle = tags$p(avg_flw, style = "font-size: 95%;"),
            icon = icon("hand-point-up"),
            color = "green"
        )
    })
    
    output$f_avgfriendSum <- renderValueBox({
        avg_fri <- round(sum(follower$friends_count)/nrow(follower),1)
        valueBox(
            value = tags$p("Avg Friends", style = "font-size: 55%;"), 
            subtitle = tags$p(avg_fri, style = "font-size: 95%;"),
            icon = icon("hand-point-up"),
            color = "orange"
        )
    })
    
    output$f_avgstatusSum <- renderValueBox({
        avg_act <- round(sum(follower$statuses_count)/nrow(follower),1)
        valueBox(
            value = tags$p("Avg Activity", style = "font-size: 55%;"), 
            subtitle = tags$p(avg_act, style = "font-size: 95%;"),
            icon = icon("hand-point-up"),
            color = "purple"
        )
    })

    output$follower_map <- renderLeaflet({
        
        # Leaflet Map Output
        # Extract coordinates from followers that shared their location (very low number of disclosed locations)
        
        get_coords <- follower %>% separate(col = coords_coords, into = c("lat", "long"), sep = ", ")
        location <- get_coords %>% select(user_id, favorite_count, retweet_count, place_name, country, country_code, long, lat, location) %>%
            filter(lat!="NA") %>% mutate(lat = as.numeric(lat), long= as.numeric(long))
        
        # Plot extracted coordinates on worldmap
        leaflet()  %>%
            addProviderTiles(providers$CartoDB.Voyager) %>%
            addMarkers(location$lat, location$long,
                       clusterOptions = markerClusterOptions(), 
                       labelOptions = labelOptions(noHide = T, textsize = "15px")) %>%
            setView(lat= 30, lng=10, zoom = 2)
    })
    output$country_rank <- renderPlotly({
        
        inp <- input$country_topn
        
        top_contry_act <- follower %>% select(status_id, country) %>%
            filter(!is.na(country)) %>% 
            group_by(country) %>% 
            dplyr::summarise(count = n()) %>% 
            arrange(desc(count)) %>% 
            top_n(20)
        
        # ggplot(top_contry_act, aes(x=reorder(country, count), y=count, alpha=count)) + geom_bar(stat="identity", fill = "#43c21c") + ##0f9fff
        #     scale_alpha(range = c(0.4, 1), guide=FALSE) +
        #     ylab(NULL) +
        #     xlab(NULL)+
        #     theme(panel.background = element_rect(fill="white"))+
        #     coord_flip()
        
        #Plot top countries (subset based on user input)
        # The ~operator allows row based manipulations
        #x and y are switched to make it horizontal (-> coord_flip() in ggplot)
        plot_ly(top_contry_act[inp[1]:inp[2],], y=~reorder(country, count), x=~count, type="bar", text=~count,
                # Marker manipulates colors, color= specifies color of the bars, line = draws a black outline around the bars
                # opacity = alpha from ggplot -> here I change the opacity based on the count
                # Rescale to c(0.4 , 1) changes the max (min) input of count to 1 (0.4) 
                # and scales the rest of the data according to their differnece from max (min)
                marker = list(color = "#43c21c",
                              line = list(color = "rgb(20, 20, 20)",
                                          width = 1), opacity=~rescale(count, to=c(0.4,1)))) %>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
        
    })
    
    output$table_boa <- renderDT(timeline, options = list(height = 5))
    output$table_user <- renderDT(user, options = list(height = 5))
    output$table_follower <- renderDT(follower, options = list(height = 5))
    
}

shinyApp(ui, server)
