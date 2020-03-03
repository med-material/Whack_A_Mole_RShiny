library(RMySQL)
library(plyr)
library(ggplot2)


my_data <- read.csv("credentials.csv", header=TRUE,sep=",", colClasses=c("character","character","character","character"))

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


mydb = dbConnect(MySQL(),
                 user=my_data[1, "username"],
                 # rstudioapi::askForPassword("Database user"),
                 password=my_data[1, "password"],
                 # rstudioapi::askForPassword("Database password"),
                 dbname=my_data[1, "dbname"],
                 host=my_data[1, "host"])

# RetreiveUniqueColmnVals() Used to get unique values available for a column
# USAGE:
#dtest = RetreiveUniqueColmnVals("Email")
RetreiveUniqueColVals <- function(tablename, column) {
  queryString = paste("SELECT DISTINCT",column,"FROM",tablename,sep=" ")
  res = dbSendQuery(mydb, queryString)
  vals = fetch(res, n=-1)
  dbClearResult(dbListResults(mydb)[[1]])
  unlisted_vals = unname(unlist(vals)) # if there are several values, they arrive as a list, so unlist them on arrival.
  sanitized_vals = gsub("[\r\n]", "", unlisted_vals)
  return(sanitized_vals) 
}

all_accounts = RetreiveUniqueColVals("whack_vr_rtii","Email")


# RetreiveDataSet() Used to query for a specific dataset. 
# Setting colvalue to NULL retreives all data.
# USAGE:
#dtest = RetreiveDataSet("rtii_mar2020","Email","mhel@create.aau.dk")
RetreiveDataSet <- function(tablename, column, colvalue) {
  queryString = "SELECT *"
  queryString = paste(queryString, "FROM",tablename, sep = " ")
  if (colvalue != "NA") {
    queryString = paste(queryString, "WHERE",column,"= ",sep=" ")
    queryString = paste(queryString,"\'",colvalue,"\'",sep="")
  }
  print(queryString)
  res = dbSendQuery(mydb, queryString)
  df = fetch(res, n=-1) 
  dbClearResult(dbListResults(mydb)[[1]])
  return(df)
}

# RefreshDataSet is a helper function called in the app for refreshing TunnelGoalFitts dataset.
# Setting colfilter to NULL retreives all data.
# USAGE:
# RefreshDataSets("mhel@create.aau.dk")
RefreshDataSets <- function(colfilter) {
  if (colfilter == "-1") {
    # -1 is the default value R Shiny uses on startup.
    return()
  }
  # REFRESH REACTION TIME DATASET
  df<<- RetreiveDataSet("whack_vr_rtii","Email",colfilter)
  print(nrow(df))
  df$GameState<<-as.factor(df$GameState)
  df$GameState<<-factor(df$GameState,levels = c("Stopped", "Playing","Paused"))
  df$PID <<- df$ParticipantId
  df$TrialNo <<- df$TestId
  df$PID <<- as.factor(df$PID)
  df$TrialNo <<- as.factor(df$TrialNo)
}


df <- data.frame()

