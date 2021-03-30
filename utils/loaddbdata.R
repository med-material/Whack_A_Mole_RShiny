library(RMySQL)
mydb <<- NULL
db_table_meta <<- NULL
db_table_event <<- NULL
db_table_sample <<- NULL
db_sessionid <<- "NA"

SetTableMeta <- function(newname) {
  db_table_meta <<- newname
}

SetTableEvent <- function(newname) {
  db_table_event <<- newname
}

SetTableSample <- function(newname) {
  db_table_sample <<- newname
}

SetSessionID <- function(newID) {
  db_sessionid <<- newID
}

GetSessionID <- function() {
  return(db_sessionid)
}

ConnectToServer <- function(auth_data) {
  SetTableMeta(auth_data[1, "table_meta"])
  SetTableEvent(auth_data[1, "table_event"])
  SetTableSample(auth_data[1, "table_sample"])
  connected = FALSE
  lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
  tryCatch({
    mydb <<- dbConnect(MySQL(),
                       user=auth_data[1, "username"],
                       # rstudioapi::askForPassword("Database user"),
                       password=auth_data[1, "password"],
                       # rstudioapi::askForPassword("Database password"),
                       dbname=auth_data[1, "dbname"],
                       host=auth_data[1, "host"])
  },error=function(cond) {
    message("Could not connect to database.")
  },finally={
    if (!is.null(mydb)) {
      connected = TRUE
    }
  })
  return(connected)
}

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

RetreiveAllData <- function(type = "Full") {
  df = NULL
  if (type == "Meta") {
    df = RetreiveDataSet(db_table_meta)
  } else if (type == "Event") {
    df = RetreiveDataSet(db_table_event)
  } else if (type == "Sample") {
    df = RetreiveDataSet(db_table_sample)
  } else if (type == "Full") {
    df_event <- RetreiveDataSet(db_table_event)
    df_meta <- RetreiveDataSet(db_table_meta)
    df_sample <- RetreiveDataSet(db_table_sample)
    df <- df_event %>% bind_rows(df_sample) %>% left_join(df_meta, by = "SessionID", suffix=c(".Event",".Meta"))
  }
  return(df)
}

RetreiveCurrentData <- function(type = "Full") {
  df = NULL
  if (type == "Meta") {
    df = RetreiveDataSet(db_table_meta, "SessionID", db_sessionid)
  } else if (type == "Event") {
    df = RetreiveDataSet(db_table_event, "SessionID", db_sessionid)
  } else if (type == "Sample") {
    df = RetreiveDataSet(db_table_sample, "SessionID", db_sessionid)
  } else if (type == "Full") {
    df_event = RetreiveDataSet(db_table_event, "SessionID", db_sessionid)
    df_meta = RetreiveDataSet(db_table_meta, "SessionID", db_sessionid)
    df_sample = RetreiveDataSet(db_table_sample, "SessionID", db_sessionid)
    
    df = df_event %>% bind_rows(df_sample) %>% left_join(df_meta, by = "SessionID", suffix=c(".Event",".Meta"))
  }
  return(df)
}

# RetreiveDataSet() Used to query for a specific dataset. 
# Setting colvalue to NULL retreives all data.
# USAGE:
#dtest = RetreiveDataSet("rtii_mar2020","Email","mhel@create.aau.dk")
RetreiveDataSet <- function(tablename, column = "NA", colvalue= "NA") {
  queryString = "SELECT *"
  queryString = paste(queryString, "FROM",tablename, sep = " ")
  queryString = paste(queryString, "WHERE FlagDelete=0")
  if (colvalue != "NA") {
    queryString = paste(queryString, "AND",column,"= ",sep=" ")
    queryString = paste(queryString,"\'",colvalue,"\'",sep="")
  }
  print(queryString)
  res = dbSendQuery(mydb, queryString)
  df = fetch(res, n=-1)
  dbClearResult(dbListResults(mydb)[[1]])
  return(df)
}

MarkDataForDeletion <- function(type = "Meta", column = "NA", colvalue= "NA", delete=T) {
  queryString = "UPDATE"
  if (type == "Meta") {
    queryString = paste(queryString,db_table_meta)
  } else if (type == "Event") {
    queryString = paste(queryString,db_table_event)
  } else if (type == "Sample") {
    queryString = paste(queryString,db_table_sample)
  }
  queryString = paste(queryString,"SET FlagDelete=")
  shouldDelete = 0
  if (delete) { shouldDelete = 1 }
  if (colvalue != "NA") {
    queryString = paste0(queryString, " CASE WHEN ",column,"= '",colvalue,"' THEN ",shouldDelete)
    queryString = paste(queryString,"ELSE FlagDelete")
  } else {
    queryString = paste(queryString, colvalue)
  }
  queryString = paste(queryString, "END")
  print(queryString)
  res = dbSendQuery(mydb, queryString)
  df = fetch(res, n=-1) 
  dbClearResult(dbListResults(mydb)[[1]])
  return(df)
}
