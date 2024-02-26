library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

df_films = readRDS("data/film-dataframe.rds")
df_directors = readRDS("data/directors-dataframe.rds")

## Calculating number of directors and films with missing gender data

no_gender_data_films = df_films %>%
  filter(has_male_director == FALSE) %>%
  filter(has_female_director == FALSE) %>%
  filter(has_nonbinary_director == FALSE) %>%
  nrow()

no_gender_data_directors = df_directors %>%
  filter(is.na(Gender)) %>%
  nrow()

## DATA ALTERATIONS FOR PLOTS
films_expanded_genders <- rbind(transform(df_films[df_films$has_female_director == TRUE, ], director_gender = "Film has Female Director"),
                            transform(df_films[df_films$has_male_director == TRUE, ], director_gender = "Film has Male Director")
                            #transform(df_films[df_films$has_nonbinary_director == TRUE, ], director_gender = "Film has Non Binary Director")
)

director_film_count <- data.frame(Directors = unlist(df_films$Directors), Gender = unlist(df_films$Directors.Genders)) %>%
  group_by(Directors) %>%
  summarize(Num.Films = n(), Gender = first(Gender)) %>%
  arrange(desc(Num.Films))

female_genres <- data.frame(Genres = unlist(df_films[df_films$has_female_director == TRUE, ]$Genres)) %>%
  group_by(Genres) %>%
  summarize(Genre.Count = n()) %>%
  mutate(Percent = paste0(round(Genre.Count / nrow(df_films[df_films$has_female_director == TRUE, ]) * 100, 2), "%")) %>%
  arrange(desc(Genre.Count))

male_genres <- data.frame(Genres = unlist(df_films[df_films$has_male_director == TRUE, ]$Genres)) %>%
  group_by(Genres) %>%
  summarize(Genre.Count = n()) %>%
  mutate(Percent = paste0(round(Genre.Count / nrow(df_films[df_films$has_male_director == TRUE, ]) * 100, 2), "%")) %>%
  arrange(desc(Genre.Count))

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
                      function post_height() {
                        //console.log('HELLO');
                        if(document.body.scrollHeight){
                          var height = document.body.scrollHeight;
                          console.log(height);
                          parent.postMessage({height: height}, '*');
                        }
                      }
                      window.addEventListener('resize', post_height);
                      window.addEventListener('load', post_height);
                      "
                     )),
    
    tags$style(HTML("
                    body {
                      background-color: #EDF0F3;
                    }
                    .plotly {
                      margin-top: 0px;
                      height: 100%;
                    }
                    .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
                        color: #555;
                        cursor: default;
                        background-color: #D8E1EA;
                        border: 1px solid #ddd;
                        border-bottom-color: transparent;
                    }
                    .well {
                        background-color: #D8E1EA;
                    }
                    .nav-tabs {
                      color: #A8B7C5;
                    }
                    .plotarea {
                        padding: 10px;
                        margin-bottom: 20px;
                        background-color: #D8E1EA;
                        border: 1px solid #e3e3e3;
                        border-radius: 4px;
                    }
                    "))
  ),
  
  # Application title
  titlePanel("Analysis of Director Genders in Popular Films"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar with text
    sidebarPanel(
      h3("The Dataset", align="center"),
      h5(style="font-weight:bold;", "Letterboxd Top 100 Most Popular Films Each Year (1980 - 2023)", align="center"),
      hr(),
      p("Letterboxd is an online film review and rating site. This data was sourced from the site, filtering films by popularity, and taking the top 100 
        for each year. Letterboxd films also include entire TV series' which are considered one film."),
      p("Letterboxd popularity score is calculated by aggregating a range of features from the Letterboxd site such as number of ratings, number of users who reported 
        watching the film, and number of Letterboxd playlists the film has been added to."),
      hr(),
      p("Some of the films in this data have multiple directors, so when a plot says \"Film has Female Director\" it means that one or more of the directors for the film 
        are female."),
      p(paste("There were 3 non-binary directors in this data, which I have left out of the plots as the number is too small to draw any useful statistical insights. 
              There was also ", no_gender_data_directors, " directors over ", no_gender_data_films," films whos gender information was not listed on TMDB. A brief 
              look at these directors showed that they were mostly the last 5 or so less well known directors listed in a large group of 
              directors for a TV series, so this is unlikely to have skewed the data much."))
    ),
    
    # Main panel with plots
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   column(6, 
                          div(class = "plotarea", plotlyOutput("num_directors_plot", height = "100%"))
                   ),
                   column(6, 
                          div(class = "plotarea", plotlyOutput("num_films_plot", height = "100%"))
                   )
                 )
        ),
        tabPanel("Years",
                 div(class = "plotarea", plotlyOutput("year_plot", height = "100%"))
        ),
        tabPanel("Top Directors",
                 fluidRow(
                   column(6, 
                          div(class = "plotarea", plotlyOutput("female_directors_table"))
                   ),
                   column(6, 
                          div(class = "plotarea", plotlyOutput("male_directors_table"))
                   )
                 )
        ),
        tabPanel("Film Ratings",
                 div(class = "plotarea", plotlyOutput("ratings_plot", height = "100%"))
        ),
        tabPanel("Director Ages",
                 div(class = "plotarea", plotlyOutput("age_plot", height = "100%"))
        ),
        tabPanel("Top Genres",
                 fluidRow(
                   column(6, 
                          div(class = "plotarea", plotlyOutput("female_genres"))
                   ),
                   column(6, 
                          div(class = "plotarea", plotlyOutput("male_genres"))
                   )
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$num_directors_plot <- renderPlotly({
    df_directors %>%
      filter(Gender != "Non-binary") %>%
      count(Gender) %>%
      plot_ly(x = ~Gender, y = ~n, color = ~Gender, type = "bar") %>%
      layout(title = "Number of Directors \nby Gender", 
             xaxis = list(title = NA, showticklabels=FALSE),
             yaxis = list(title = "Number of Directors"),
             margin = list(t = "20px"),
             paper_bgcolor = "#D8E1EA",
             legend = list(orientation = "h", xanchor = "center", x = 0.5)
      ) %>%
      add_annotations(
        x = 0, 
        y = 1250,
        text = paste("Total Number\nof Directors\n", nrow(df_directors)),
        showarrow = FALSE
      )
  })
  
  output$num_films_plot <- renderPlotly({
    films_expanded_genders %>%
      count(director_gender) %>%
      plot_ly(x = ~director_gender, y = ~n, color = ~director_gender, type = "bar") %>%
      layout(title = "Number of Films by \nGender of Directors", 
             xaxis = list(title = NA, showticklabels=FALSE),
             yaxis = list(title = "Number of Films"),
             margin = list(t = "20px"),
             paper_bgcolor = "#D8E1EA",
             legend = list(orientation = "h", xanchor = "center", x = 0.5)
      ) %>%
      add_annotations(
        x = 0, 
        y = 2500,
        text = paste("Total Number\nof Films\n", nrow(df_films)),
        showarrow = FALSE
      )
  })
  
  # Render the plot
  output$year_plot <- renderPlotly(
    films_expanded_genders %>%
      count(Year, director_gender) %>%
      plot_ly(x = ~Year, y = ~n, color = ~director_gender, type = "bar") %>%
      layout(title = "Number of Films Each Year with Female/Male Directors",
             xaxis = list(title = NA),
             yaxis = list(title = "Number of Films"),
             margin = "20px",
             paper_bgcolor = "#D8E1EA",
             legend = list(orientation = "h", xanchor = "center", x = 0.5)
             )
  )
  
  output$female_directors_table <- renderPlotly({
    director_film_count %>%
      filter(Gender == "Female") %>%
      head(10) %>%
      plot_ly(
        type = "table",
        header = list(
          values = c("Name", "Number of Films"),
          align= (c("center")),
          fill = list(color = rgb(red = 102/255, g = 194/255, b = 166/255))
        ),
        cells = list(
          values = rbind(.$Directors, .$Num.Films),
          align = (c("left", "center")),
          fill = list(color = rgb(red = 102/255, g = 194/255, b = 166/255))
        )
      ) %>%
      layout(title = "Female Directors with \n Most Films", 
             margin = list(t=100, r=5, l=5, b=0),
             paper_bgcolor = "#D8E1EA"
      )
  })
  
  output$male_directors_table <- renderPlotly({
    director_film_count %>%
      filter(Gender == "Male") %>%
      head(10) %>%
      plot_ly(
        type = "table",
        header = list(
          values = c("Name", "Number of Films"),
          align = c("center"),
          fill = list(color = rgb(red = 126/255, g = 150/255, b = 193/255))
        ),
        cells = list(
          values = rbind(.$Directors, .$Num.Films),
          align = c("left", "center"),
          fill = list(color = rgb(red = 126/255, g = 150/255, b = 193/255))
        )
      ) %>%
      layout(title = "Male Directors with \n Most Films",
             margin = list(t=100, r=5, l=5, b=0),
             paper_bgcolor = "#D8E1EA"
      )
  })
  
  output$ratings_plot <- renderPlotly({
    films_expanded_genders %>%
      plot_ly(y = ~director_gender, x = ~Film.Rating, color = ~director_gender, type="box") %>%
      layout(
        title = "Letterboxd Film Rating (Stars) by Director Gender",
         yaxis = list(title = NA),
         xaxis = list(title = NA, range = c(0, 5)),
         margin = "20px",
         paper_bgcolor = "#D8E1EA",
         legend = list(orientation = "h", xanchor = "center", x = 0.5)
      )
  })
  
  output$age_plot <- renderPlotly({
    df_films %>%
      unnest(cols = c(Directors.Ages, Directors.Genders)) %>%
      filter(Directors.Genders != "Non-binary") %>%
      mutate(Age = as.numeric(Directors.Ages)) %>%
      mutate(Gender = factor(Directors.Genders, levels = c("Female", "Male"))) %>%
      plot_ly(y = ~Gender, x = ~Age, color = ~Gender, type="box") %>%
      layout(
        title = "Age of Directors (At Film Release Year)",
        yaxis = list(title = NA),
        xaxis = list(title = NA),
        margin = "20px",
        paper_bgcolor = "#D8E1EA",
        legend = list(orientation = "h", xanchor = "center", x = 0.5)
      )
  })
  
  output$female_genres <- renderPlotly({
    female_genres %>%
      head(10) %>%
      plot_ly(
        type = "table",
        header = list(
          values = c("Genre", "Number of Films"),
          align = c("center"),
          fill = list(color = rgb(red = 102/255, g = 194/255, b = 166/255))
        ),
        cells = list(
          values = rbind(.$Genres, paste(.$Genre.Count, .$Percent, sep = " - ")),
          align = c("left", "center"),
          fill = list(color = rgb(red = 102/255, g = 194/255, b = 166/255))
        )
      ) %>%
      layout(title = "Films with Female Directors\nTop Genres", 
             margin = list(t=100, r=5, l=5, b=0),
             paper_bgcolor = "#D8E1EA"
      ) %>%
      add_annotations(
        x = 0.5, 
        y = 0,
        text = "Most films have multiple genres",
        showarrow = FALSE
      )
  })
  
  output$male_genres <- renderPlotly({
    male_genres %>%
      head(10) %>%
      plot_ly(
        type = "table",
        header = list(
          values = c("Genre", "Number of Films"),
          align = c("center"),
          fill = list(color = rgb(red = 126/255, g = 150/255, b = 193/255))
        ),
        cells = list(
          values = rbind(.$Genres, paste(.$Genre.Count, .$Percent, sep = " - ")),
          align = c("left", "center"),
          fill = list(color = rgb(red = 126/255, g = 150/255, b = 193/255))
        )
      ) %>%
      layout(title = "Films with Male Directors\nTop Genres", 
             margin = list(t=100, r=5, l=5, b=0),
             paper_bgcolor = "#D8E1EA"
      ) %>%
      add_annotations(
        x = 0.5, 
        y = 0,
        text = "Most films have multiple genres",
        showarrow = FALSE
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)