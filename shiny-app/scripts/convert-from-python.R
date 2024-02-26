library(dplyr)
library(lubridate)
library(tidyr)

df_films <- read.csv("shiny-app/data/film-dataframe.csv")
df_directors <- read.csv("shiny-app/data/directors-dataframe.csv")

na_if_none <- function(words_list){
  words_list[words_list == "None"] <- NA
  return(words_list)
}

str_to_strlist <- function(df, column_names){
  for(col in column_names){
    df[[col]] <- gsub("\\[|\\]", "", df[[col]])
    df[[col]] <- gsub("\\'|\\]", "", df[[col]])
    df[[col]] <- strsplit(df[[col]], ", ")
    df[[col]] <- lapply(df[[col]], function(x) as.list(x))
    df[[col]] <- lapply(df[[col]], function(x) na_if_none(x))
  }
  return(df)
}

nan_if_zero <- function(number_list){
  number_list[number_list == 0] <- NA
  return(number_list)
}

str_to_intlist <- function(df, column_names){
  for(col in column_names){
    df[[col]] <- gsub("\\[|\\]", "", df[[col]])
    df[[col]] <- lapply(df[[col]], function(x) as.integer(strsplit(x, ", ")[[1]]))
    df[[col]] <- lapply(df[[col]], function(x) as.list(x))
    df[[col]] <- lapply(df[[col]], function(x) nan_if_zero(x))
  }
  return(df)
}

str_to_boolean <- function(df, column_names){
  for(col in column_names){
    df[[col]] <- as.logical(df[[col]])
  }
  return(df)
}

df_films <- str_to_strlist(df_films, c("Directors", "Genres", "Directors.Genders"))
df_films <- str_to_intlist(df_films, c("Directors.Ages"))
df_films <- str_to_boolean(df_films, c("has_male_director", "has_female_director", "has_nonbinary_director"))

df_directors <- str_to_strlist(df_directors, c("Genres"))
df_directors$Birthdate <- as.Date(df_directors$Birthdate)
df_directors$Gender[df_directors$Gender == ""] <- NA

saveRDS(df_films, file="shiny-app/data/film-dataframe.rds")
saveRDS(df_directors, file="shiny-app/data/directors-dataframe.rds")

