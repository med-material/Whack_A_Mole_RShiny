library(tidyverse)
library(dplyr)

LoadFromFilePaths <- function(filePathMeta, filePathEvent, filePathSample) {
  print(filePathMeta)
  dataset_meta <- read.csv(filePathMeta, na.strings="NULL", sep=";")
  dataset_event <- read.csv(filePathEvent, na.strings="NULL", sep=";")
  dataset_sample <- read.csv(filePathSample, na.strings="NULL", sep=";")
  dataset <- dataset_event %>% bind_rows(dataset_sample) %>% left_join(dataset_meta, by = "SessionID", suffix=c(".Event",".Meta"))
  return(dataset)
}

LoadFromDirectory <- function(dir, delim = ";", event = "Event", sample = "Sample", meta = "Meta") {
  #delim = ";"
  #dir = "Logs"
  #event = "Game"
  #sample = "Sample"
  #meta = "Meta"
  #
  #main game event data
  df_event <- list.files(recursive=TRUE ,path = dir,
                         pattern = paste("*",event,".csv",sep=""),
                         full.names = T) %>% 
    tibble(filename = .) %>%   
    mutate(file_contents = map(filename,~ read_delim(file.path(.),delim,col_names=T, na = "NULL")))  %>% 
    unnest(cols=-filename) %>%
    mutate(file_contents = NULL)
  
  #sample data
  df_sample <- list.files(recursive=TRUE ,path = dir,
                          pattern = paste("*",sample,".csv",sep=""), 
                          full.names = T) %>% 
    tibble(filename = .) %>%   
    mutate(file_contents = map(filename,~ read_delim(file.path(.), delim, na = "NULL")))  %>% 
    unnest(cols=-filename) %>%
    mutate(file_contents = NULL)
  
  #meta data
  df_meta <- list.files(recursive=TRUE ,path = dir,
                        pattern = paste("*",meta,".csv",sep=""), 
                        full.names = T) %>% 
    tibble(filename = .) %>%   
    mutate(file_contents = map(filename,~ read_delim(file.path(.), delim, na = "NULL")))  %>% 
    unnest(cols=-filename) %>% 
    separate(col=filename,sep="_",into=c("i5","i6","i7","i8","i9"), remove=F) %>%
    separate(col=filename,sep="/",into=c("i0","i1","i2","i3","i4"), remove=T) %>%
    mutate(file_contents = NULL)
  
  
  dataset <- df_event %>% bind_rows(df_sample) %>% left_join(df_meta, by = "SessionID", suffix=c(".Event",".Meta"))
  
  return(dataset)
}