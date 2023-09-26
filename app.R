library(shiny)
library(RColorBrewer)
library(spotifyr)
library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(spotifyr)
library(ggplot2)
library(shinythemes)
library(lubridate)

# read in data
top10s <- read_csv("music_data/top10s.csv")
eilish_lyrics <- read_csv("music_data/eilish_lyrics.csv")
billie_eilish <- read_csv("music_data/billie_eilish.csv")

smart_stopwords <- get_stopwords(source = "smart")
bing_sentiments <- get_sentiments(lexicon = "bing")

################################################################################

# creating input panels 

# introduction page, where we added an image of Billie Eilish with her 2019 Grammy awards and a Billboard image
intro <- 
  fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("Analysis of top songs of the 2010s and 2019 Grammy Winners"),
  mainPanel(fluidRow(splitLayout(cellWidths = c("50%", "50%")), 
                     img(src = 'eilish.png',
                         height = '150', 
                         width = '200'),
                     img(src = 'Billboard_Hot_100.png',
                         height = '150', 
                         width = '200'))),
  mainPanel(includeMarkdown("music_data/intro.Rmd"))
)

################################################################################

# by_year and compare_variables creates "Top Songs Comparison" page
by_year <- fluidPage(
  titlePanel("Compare Variables Throughout Years"),
              selectInput(inputId = "compare0", 
                          label = "y-axis variable",
              choices = c("Tempo" = "tempo",
                          "Energy"= "energy",
                          "Danceability" = "danceability",
                          "Loudness (dB)" = "loudness",
                          "Liveness" = "liveness",
                          "Valence"= "valence",
                          "Duration"= "duration",
                          "Acousticness"= "acousticness",
                          "Speechiness" = "speechiness",
                          "Popularity" = "popularity"),
              selected = "energy"),
  
  mainPanel(
    plotOutput(outputId = "by_year"),
    h3("Defining the Spotify Variables"),
    p("The Spotify package rates songs on multiple variables, which are listed below. These ratings are based on the following criteria:"),
    tags$li("Tempo - The beats per minute for every song."),
    tags$li("Energy - The speed and intensity of each song. Higher values indicate a higher energy level."),
    tags$li("Danceability - The suitability of the song for dancing and how easy each song is to dance to."),
    tags$li("Loudness (dB) - The volume of each song in decibels. Higher decibels indicate a louder song."),
    tags$li("Liveness - The liklihood that the song is a live recording."),
    tags$li("Valence - The positivity of each song. Higher values demonstrate a more positive mood."),
    tags$li("Duration - The length of songs measured in seconds."),
    tags$li("Acoustincess - The liklihood of using acoustics and non-electronic instruments."),
    tags$li("Speechiness - The presence of spoken words. The higher the value, the more spoken words have been included."),
    tags$li("Popularity - The popularity of the song based on Billboard rankings."),
    
    h3("Compare Variables Throughout Years: Analysis"),
    tags$li("In total, there are 10 different variables I am comparing for the most popular songs of the past decade."),
    tags$li("Most of the graphs are shaped as a bell curve, with a peak occuring in 2015."),
    tags$li("Overall, it appears that the presence/popularity of most variables in music increased from 2010 - 2014, peaked in 2015,
      and has since been declining from 2016 - 2019."))
)
  
compare_variables <- fluidPage(
  titlePanel("Compare Two Variables"),
  inputPanel(selectInput(inputId = "compare1", 
                           label = "x-axis variable",
              choices = c("Tempo" = "tempo",
                          "Energy"= "energy",
                          "Danceability" = "danceability",
                          "Loudness (dB)" = "loudness",
                          "Liveness" = "liveness",
                          "Valence"= "valence",
                          "Duration"= "duration",
                          "Acousticness"= "acousticness",
                          "Speechiness" = "speechiness",
                          "Popularity" = "popularity"),
              selected = "energy"),
              selectInput(inputId = "compare2", 
                           label = "y-axis variable",
              choices = c("Tempo" = "tempo",
                          "Energy"= "energy",
                          "Danceability" = "danceability",
                          "Loudness (dB)" = "loudness",
                          "Liveness" = "liveness",
                          "Valence"= "valence",
                          "Duration"= "duration",
                          "Acousticness"= "acousticness",
                          "Speechiness" = "speechiness",
                          "Popularity" = "popularity"),
              selected = "danceability")),
              checkboxInput("type_check2", "Filter by year?", value = FALSE),
              sliderInput("year", 
                          label = "Year",
                          min = 2010,
                          max = 2019, 
                          value = 2010,
                          step = 1, 
                          sep = ""),
  mainPanel(
    plotOutput(outputId = "compare_variables"),
    h3("Positive Correlations"),
    p("I ran correlation tests to find the correlation cofficient between every Spotify variable for the
      'Top Spotify Songs from 2010 - 2019' dataset. 
      In total, there were 25 graphs that demonstrated a positive
      correlation, meaning that as one variable increased, the second variable tended to increase as well. 
      However, several of the correlations were very weak, for example less than 0.10, so we only included graphs with a correlation above 0.25, which is considered to be a 'weak' correlation.
      The 4 graphs that met this criteria are:"),
    tags$li("Energy and Loudness (coefficient: 0.54)"),
    tags$li("Danceability and Valence (coefficient: 0.50)"),
    tags$li("Energy and Valence (coefficient: 0.41)"),
    tags$li("Loudness and Valence (coefficient: 0.28)"),
    
    h3("Negative Correlations"),
    p("There were also 18 graphs that demonstrated a negative correlation, meaning that as one variable increased,
      the second variable tended to decrease. However, several graphs also had very weak correlations, so we are only including correlations below -0.25.
      These 3 graphs are:"),
    tags$li("Energy and Acousticness (coefficient: -0.56)"),
    tags$li("Valence and Duration (coefficient: -0.26)"),
    tags$li("Valence and Acousticness (coefficient: -0.25)"),
    p("The line graph can also be filtered by year if the user wants to see correlations separated by year.")
    
  )
)

################################################################################

# top_song creates "2019 Top Songs" page
top_song <- fluidPage(
  titlePanel("Compare Variables of Top 2019 Songs"),
  inputPanel(selectInput(inputId = "compare5", 
              label = "x-axis variable",
              choices = c("Tempo" = "tempo",
                          "Energy"= "energy",
                          "Danceability" = "danceability",
                          "Loudness (dB)" = "loudness",
                          "Liveness" = "liveness",
                          "Valence"= "valence",
                          "Duration"= "duration",
                          "Acousticness"= "acousticness",
                          "Speechiness" = "speechiness"),
              selected = "energy"),
  selectInput(inputId = "compare6", 
              label = "y-axis variable",
              choices = c("Tempo" = "tempo",
                          "Energy"= "energy",
                          "Danceability" = "danceability",
                          "Loudness (dB)" = "loudness",
                          "Liveness" = "liveness",
                          "Valence"= "valence",
                          "Duration"= "duration",
                          "Acousticness"= "acousticness",
                          "Speechiness" = "speechiness"),
              selected = "danceability")
),
  mainPanel(
    plotOutput(outputId = "top_song"),
    h3("2019 Most Popular Songs vs. Billie Eilish"),
    tags$li("This scatterplot compares the Spotify variables for only the most popular songs in the year 2019 against Billie Eilish's Grammy winning album."), 
    tags$li("Using the same requirements from ‘Top Song Comparison,’ I only analyzed the correlation coefficients that were either above 0.25 or below -0.25, indicating a ‘weak’ correlation that is still significant."),
    tags$li("In total, Billie Eilish’s album had 17 positive correlations and 13 negative correlations. This is in comparison to the 2019 most popular songs, which had 6 positive correlations and 2 negative correlations."),
    tags$li("Based on this graph, it appears that there are more associations/relationships between the Spotify variables for Billie Eilish’s music than other popular songs that came out in the same year."), 
    tags$li("However, since the ‘Other Artist’ category is finding relationships among a bigger data set with more songs and different artists, this might have affected the magnitude of the relationships that were found."),
    
    h3("Billie Eilish"),
    h4("Positive Correlations"),
    tags$li("Danceability and Loudness (coefficient: 0.80)"),
    tags$li("Loudness and Valence (coefficient: 0.79)"),
    tags$li("Loudness and Speechiness (coefficient: 0.77)"),
    tags$li("Danceability and Valence (coefficient: 0.76)"),
    tags$li("Danceability and Speechiness (coefficient: 0.74)"),
    tags$li("Energy and Loudness (coefficient: 0.72)"),
    tags$li("Energy and Valence (coefficient: 0.65)"),
    tags$li("Valence and Speechiness (coefficient: 0.64)"),
    tags$li("Tempo and Loudness (coefficient: 0.63)"),
    tags$li("Tempo and Danceability (coefficient: 0.61)"),
    tags$li("Energy and Danceability (coefficient: 0.61)"),
    tags$li("Tempo and Duration (coefficient: 0.57)"),
    tags$li("Energy and Speechiness (coefficient: 0.50)"),
    tags$li("Tempo and Energy (coefficient: 0.50)"),
    tags$li("Tempo and Valence (coefficient: 0.50)"),
    tags$li("Tempo and Speechiness (coefficient: 0.49)"),
    tags$li("Danceability and Duration (coefficient: 0.36)"),
    h4("Negative Correlations"),
    tags$li("Loudness and Acousticness (coefficient: -0.80)"),
    tags$li("Energy and Acousticness (coefficient: -0.76)"),
    tags$li("Acousticness and Speechiness (coefficient: -0.75)"),
    tags$li("Valence and Acousticness (coefficient: -0.71)"),
    tags$li("Danceability and Acousticness (coefficient: -0.62)"),
    tags$li("Tempo and Acousticness (coefficient: -0.50)"),
    tags$li("Liveness and Valence (coefficient: -0.45)"),
    tags$li("Tempo and Loudness (coefficient: -0.43)"),
    tags$li("Danceability and Liveness (coefficient: -0.35)"),
    tags$li("Liveness and Duration (coefficient: -0.30)"),
    tags$li("Liveness and Speechiness (coefficient: -0.30)"),
    tags$li("Loudness and Liveness (coefficient: -0.27)"),
    tags$li("Energy and Duration (coefficient: -0.26)"),
    h3("Other Artist"),
    h4("Positive Correlations"),
    tags$li("Energy and Loudness (coefficient: 0.75)"),
    tags$li("Danceability and Valence (coefficient: 0.47)"),
    tags$li("Loudness and Valence (coefficient: 0.39)"),
    tags$li("Energy and Valence (coefficient: 0.30)"),
    tags$li("Duration and Speechiness (coefficient: 0.29)"),
    tags$li("Energy and Liveness (coefficient: 0.26)"),
    h4("Negative Correlations"),
    tags$li("Energy and Acousticness (coefficient: -0.40)"),
    tags$li("Loudness and Duration (coefficient: -0.27)"),
    p("Looking at this graph, one can also get an idea of how much Billie Eilish 
    incorporates the Spotify variables in her music compared to the top songs of 2019 by looking 
    at the data points. Billie Eilish’s album, ‘When We All Fall Asleep, Where Do We Go?’ 
    has lower scores in: energy, danceability, loudness, and valence than the 2019 data set. 
    However, she has higher scores in acousticness and speechiness.")
    )
)


################################################################################

# lyric_sentiment creates "Album Lyrics Sentiment Analysis" page
lyric_sentiment <- fluidPage(
  titlePanel("Album Lyrics Sentiment Analysis"),
  (inputPanel(selectInput("track_n", label = "Song",
                          choices = c("bad guy"= "2", 
                                      "xanny"= "3", 
                                      "you should see me in a crown" = "4",
                                      "all the good girls go to hell" = "5",
                                      "wish you were gay" = "6",
                                      "when the party's over" = "7",
                                      "my strange addiction" = "9",
                                      "bury a friend" = "10",
                                      "ilomilo" = "11",
                                      "listen before i go" = "12",
                                      "i love you" = "13",
                                      "goodbye" = "14")))),
  mainPanel(fluidRow(splitLayout(cellWidths = c("70%", "30%"), 
                                 plotOutput("SentimentPlotSong"), 
                                 plotOutput("SentimentPieSong")),
                    h3("Sentiment Analysis by Song"),
                    tags$li("The faceted bar plot shows the top negative and positive words per song selected from Billie Eilish's album, 'When we all fall asleep, where do we go?'."), 
                    tags$li("The pie chart shows the overall positivity or negativity of the song, which can be compared to the overall distribution of the album lyrics as a whole."),
                    tags$li("Almost all of the songs within the album have a primarily negative sentiment except for 'i love you,' which is more positive than negative and 'goodbye,' which is 50% positive and 50% negative."),
                    tags$li("Note that the first introduction song in the album has no non-stop word lyrics, so it does not appear as an option in the bar plot. "),
            fluidRow(splitLayout(cellWidths = c("70%", "30%"), 
                                 plotOutput("SentimentPlotAlbum"), 
                                 plotOutput("SentimentPieAlbum")))),
            h3("Sentiment Analysis in Album"),
            p("Based on the faceted bar plot and pie chart, Billie Eilish's 2019 - 2020 
              Grammy winning album has a primarily negative sentiment (around 75% negative). 
              One can also see that the album's top 3 most negative words are 'bad,' 'lie,' and 'strange.' 
              Her top 3 most positive words are 'love,' 'good,' and 'pretty.'"))
)

################################################################################

# billie_wordcloud creates "Billie Eilish Word Cloud" page
billie_wordcloud <- fluidPage(
  titlePanel("Billie Eilish Album Wordcloud"),
  inputPanel(
    sliderInput("max_num",
                label = "Maximum Number of Words:",
                min = 1,
                max = 100,
                value = 50
    )
  ),
  
  
    mainPanel(
    plotOutput(outputId = "billie_wordcloud"),
    
  h3("Wordcloud Analysis"),
  p("Out of curiosity, I created a wordcloud of Billie Eilish's album ‘When We All Fall Asleep, Where Do We Go?’ 
    The bigger the words are, the higher frequency they had in the album. Based on this plot,
    the most used words in her album are 'wanna,' 'da,' and 'bad.' The user can also change how
    many words are displayed in the wordcloud by changing the 'Maximum Number of Words' slider.")
  )
)



################################################################################

# tab bar where the user can choose what pages to view
ui <- tagList(navbarPage(title = "Analysis of Top Songs from the Years 2010-2019 and 2019 Grammy Winners",
                         theme = shinytheme("lumen"),
                         tabPanel("Introduction",
                                  intro),
                         tabPanel("Top Song Comparison",
                                  by_year,
                                  compare_variables),
                         tabPanel("2019 Top Songs",
                                  top_song),
                         tabPanel("Album Lyrics Sentiment Analysis",
                                  lyric_sentiment),
                         tabPanel("Billie Eilish Word Cloud",
                                  billie_wordcloud)
))

# a .Rmd file was used so we could incorporate text into our introduction page 

################################################################################

# all of our inputs are in the server function
server <- function(input, output) {

################################################################################

# "Top Song Comparison" output
  output$by_year <- renderPlot({
    top10s %>%
      group_by(title) %>%
      ggplot(aes_string(y = input$compare0, 
                        x = "year")) +
        scale_x_continuous(n.breaks = 10) +
        theme_light() +
        geom_col(fill = "#2b2d2f",) + theme(legend.position = "none") 
  })
  
  output$compare_variables <- renderPlot({
    if(input$type_check2)
      top10s %>%
      filter(year == input$year) %>%
      group_by(title) %>%
      ggplot(aes_string(x = input$compare1, 
                        y = input$compare2)) +
      geom_point(size = 2, alpha = 2, color = "#2b2d2f") +
      geom_smooth(se = FALSE, color = "blue") +
      theme_light()
    else if(!input$type_check2)
      top10s %>%
      group_by(title) %>%
      ggplot(aes_string(x = input$compare1, 
                        y = input$compare2)) +
      geom_point(size = 2, alpha = 2, color = "#2b2d2f") +
      geom_smooth(se = FALSE, color = "blue") +
      theme_light() 
    
  })  

################################################################################

# "2019 Top Songs" variable analysis output
output$top_song <- renderPlot({
  billie_eilish %>%
    rename(title = track_name) %>%
    select(-1, -12, -13) %>%
    mutate(artist = "Billie Eilish") %>%
    rbind(top10s %>%
            filter(year == 2019) %>%
            select(-1,-4,-5, -15)) %>%
    mutate(eilish = ifelse(artist == "Billie Eilish", 
                           "Billie Eilish", "Other Artist")) %>%
    ggplot(mapping = aes_string(x = input$compare5, 
                                y = input$compare6, 
                                color = "eilish")) +
    geom_point(mapping = aes_string(color = "eilish"), 
               size = 2, alpha = 2) +
    geom_smooth(mapping = aes_string(color = "eilish"), 
                se = FALSE, method = "lm") +
    theme_light()
})
  
################################################################################
  
# "Album Lyrics Sentiment" analysis output
output$SentimentPlotSong <- renderPlot({
    eilish_lyrics %>%
      filter(track_n == input$track_n) %>%
      select(Lyrics) %>%
      unnest_tokens(word, Lyrics, token = "words") %>%
      inner_join(bing_sentiments) %>%
      anti_join(smart_stopwords, by = "word") %>%
      count(sentiment, word, sort = TRUE) %>%
      group_by(sentiment, word, n) %>%
      arrange(desc(n)) %>%
      group_by(sentiment) %>%
      slice_max(n, n = 10) %>%
      ungroup() %>%
      ggplot(aes(x = fct_reorder(word, n), 
                 y = n, 
                 fill = sentiment)) +
          geom_col(show.legend = FALSE) +  
          coord_flip() +
          facet_wrap(~ sentiment, scales = "free_y") + 
          labs(title = "Negative vs. Positive Words in Song Lyrics", 
                 x = "Word",
                 y = "Word Frequency in Song") +
          theme_light() +
          theme(plot.title = element_text(hjust = 0.5))
    })
    
  output$SentimentPieSong <- renderPlot({
      eilish_lyrics %>%
        filter(track_n == input$track_n) %>%
        select(Lyrics) %>%
        unnest_tokens(word, Lyrics, token = "words") %>%
        inner_join(bing_sentiments) %>%
        anti_join(smart_stopwords, by = "word") %>%
        group_by(sentiment) %>%
        count(sentiment) %>%
        summarise(prop = n /(count(eilish_lyrics %>%
                        filter(track_n == input$track_n) %>%
                        select(Lyrics) %>%
                        unnest_tokens(word, Lyrics, 
                        token = "words") %>%
                        inner_join(bing_sentiments) %>%
                        anti_join(smart_stopwords, by = "word")))) %>%
        ggplot(mapping = aes(x = "sentiment", 
                             y = prop$n,
                             fill = c("negative", "positive"))) +
            geom_bar(stat = "identity", width = 1, color = "white") +
            coord_polar("y", start = 0) +
            theme_void() +
            labs(fill = "Sentiment")
    })
    
  output$SentimentPlotAlbum <- renderPlot({
      eilish_lyrics %>%
          select(Lyrics) %>%
          unnest_tokens(word, Lyrics, token = "words") %>%
          inner_join(bing_sentiments) %>%
          anti_join(smart_stopwords, by = "word") %>%
          count(sentiment, word, sort = TRUE) %>%
          group_by(sentiment, word, n) %>%
          arrange(desc(n)) %>%
          group_by(sentiment) %>%
          slice_max(n, n = 10) %>%
          ungroup() %>%
          ggplot(aes(x = fct_reorder(word, n), y = n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +  
            coord_flip() +
            facet_wrap(~ sentiment, scales = "free_y") + 
            labs(title = "Negative vs. Positive Words in Album Lyrics", 
                 x = "Word",
                 y = "Word Frequency in Album") +
            theme_light() +
            theme(plot.title = element_text(hjust = 0.5))
    })
    
  output$SentimentPieAlbum <- renderPlot({
    eilish_lyrics %>%
          select(Lyrics) %>%
          unnest_tokens(word, Lyrics, token = "words") %>%
          inner_join(bing_sentiments) %>%
          anti_join(smart_stopwords, by = "word") %>%
          group_by(sentiment) %>%
          count(sentiment) %>%
          summarise(prop = n / count(eilish_lyrics %>%
                          select(Lyrics) %>%
                          unnest_tokens(word, Lyrics, token = "words") %>%
                          inner_join(bing_sentiments) %>%
                          anti_join(smart_stopwords, by = "word"))) %>%
          ggplot(mapping = aes(x = "sentiment", 
                               y = prop$n,
                               fill = c("negative", "positive"))) +
            geom_bar(stat = "identity", width = 1, color = "white") +
            coord_polar("y", start = 0) +
            theme_void() +
            labs(fill = "Sentiment")
    })

################################################################################

# "Billie Eilish Word Cloud" output

output$billie_wordcloud <- renderPlot({
  eilish_lyrics %>%
    select(Lyrics) %>%
    unnest_tokens(word, Lyrics, token = "words") %>%
    anti_join(smart_stopwords, by = "word") %>%
    count(word) %>%
    group_by(word) %>%
    with(wordcloud(words = word, 
              freq = n, 
              shape = "circle", 
              size = 0.25,
              minSize = 5,
              max.words=input$max_num, 
              random.order = FALSE, 
              rot.per = 0.35, 
              colors=brewer.pal(8, "Dark2")))
                    
  })

################################################################################
  
}
shinyApp(ui = ui, server = server)
