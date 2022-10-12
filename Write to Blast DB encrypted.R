## This file is the primary method of communication with the MySQL database ##

# install.packages("RMariaDB")
library(RMariaDB)
library(readxl)
library(tidyverse)
library(bayesbio)

# db password is encrypted in this copy of the file
userpassword <- "encrypted"

# read in athletes
# load all of table query
table_name = "athletes"
load_all_query <- paste0("SELECT * FROM ", table_name, ";")
# print(load_all_query)
blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
rs = dbSendQuery(blastDb, load_all_query)
loadedData <- dbFetch(rs)
# Clear the result
dbClearResult(rs)
# Disconnect to clean up the connection to the database.
dbDisconnect(blastDb)
athletes <- loadedData

##### functions #####
{
  nearestTime <- function(df1, df2, timeCol1, timeCol2){
    if(!timeCol1 %in% colnames(df1)) stop("timeCol1 must specify a column name in df1.")
    if(!timeCol2 %in% colnames(df2)) stop("timeCol2 must specify a column name in df2.")
    dfMinTime = data.frame(matrix(ncol = (ncol(df2) - 1), nrow = nrow(df1)))
    colnames(dfMinTime) = colnames(df2)[!colnames(df2) %in% timeCol2]
    ties_count = 0
    for(i in 1:nrow(df1)){
      min_row = numeric()
      min_rows = apply(df2, 1, function(x) abs(as.numeric(difftime(df1[i, timeCol1], x[timeCol2], units = "secs"))))
      mins = (min_rows == min(min_rows))
      dfMinTime[i, ] = df2[which.min(min_rows), !(colnames(df2) %in% timeCol2), drop = FALSE]
      if(sum(mins) > 1) { ties_count = ties_count + 1 }
    }
    dfAll = cbind(df1, dfMinTime)
    if(ties_count > 0){
      message("Warning: there were ", ties_count, " difftime ties, for which the first corresponding row of df2 was chosen for merging.")
    }
    return(dfAll)
  }
  
  tmLive.timestamp <- function(tmData, day_swings, date, tmName) {
    
    # tmName = "Blaser, Bryce"
    # tmData <- tmData %>% filter(Date == date)
    # day_swings = blaser_swings %>% filter(grepl("2022-09-09", formatted_timestamp))

    for (i in 1:nrow(tmData)) {
      if (tmData$blast_sec[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_sec[i] = paste0("0", tmData$blast_sec[i])
      }
      if (tmData$blast_minute[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_minute[i] = paste0("0", tmData$blast_minute[i])
      }
      if (tmData$blast_hour[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_hour[i] = paste0("0", tmData$blast_hour[i])
      }
      tmData$blast_hour[i] <- ifelse(tmData$blast_hour[i] == "13", "01",
                                     ifelse(tmData$blast_hour[i] == "14", "02",
                                            ifelse(tmData$blast_hour[i] == "15", "03",
                                                   ifelse(tmData$blast_hour[i] == "16", "04",
                                                          ifelse(tmData$blast_hour[i] == "17", "05",
                                                                 ifelse(tmData$blast_hour[i] == "18", "06",
                                                                        ifelse(tmData$blast_hour[i] == "19", "07",
                                                                               ifelse(tmData$blast_hour[i] == "20", "08",
                                                                                      ifelse(tmData$blast_hour[i] == "21", "09",
                                                                                             ifelse(tmData$blast_hour[i] == "22", "10",
                                                                                                    ifelse(tmData$blast_hour[i] == "23", "11",
                                                                                                           tmData$blast_hour[i])))))))))))
    }
    # View(tmData %>% select(Time, hour, minute, sec, blast_hour, blast_minute, blast_sec))

    tmData = tmData %>% mutate(
      blast_timestamp = paste0(as.character(strptime(Date, "%m/%d/%y")), " ", blast_hour, ":", blast_minute, ":", blast_sec, " EDT")
    ) %>% arrange(desc(PitchNo))
    # tmData$blast_timestamp
    
    diffs2 = c()
    for (i in 1:nrow(tmData)) {
      diffs1 = c()
      for (j in 1:nrow(day_swings)) {
        # i=3
        diffs1 = append(abs(as.numeric(difftime(tmData$blast_timestamp[i], day_swings$extra_timestamp[j], units = "secs"))), diffs1)
      }
      diffs2 = append(min(diffs1), diffs2)
    }
    # diffs2
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    if (length(diffs2) > 3) {
      timestamp_diff = as.numeric(getmode(diffs2))
    } else {
      timestamp_diff = as.numeric(median(diffs2))
    }
    
    tmData[, 252:254] <- lapply(tmData[, 252:254], as.numeric)
    
    tmData <- tmData %>% mutate(
      blast_sec = blast_sec - timestamp_diff
    )
    
    for (i in 1:nrow(tmData)) {
      if(tmData$blast_sec[i] < 0) {
        tmData$blast_minute[i] = tmData$blast_minute[i] - 1
        tmData$blast_sec[i] = 60 + tmData$blast_sec[i]
      } 
      if (tmData$blast_minute[i] < 0) {
        tmData$blast_hour[i] = tmData$blast_hour[i] - 1
        tmData$blast_minute[i] = 60 + tmData$blast_minute[i]
      }
    }
    for (i in 1:nrow(tmData)) {
      if(tmData$blast_sec[i] == 60) {
        tmData$blast_minute[i] = tmData$blast_minute[i] + 1
        tmData$blast_sec[i] = 0
      }
    }
    tmData[, 252:254] <- lapply(tmData[, 252:254], as.character)
    
    for (i in 1:nrow(tmData)) {
      if (tmData$blast_sec[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_sec[i] = paste0("0", tmData$blast_sec[i])
      }
      if (tmData$blast_minute[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_minute[i] = paste0("0", tmData$blast_minute[i])
      }
      if (tmData$blast_hour[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_hour[i] = paste0("0", tmData$blast_hour[i])
      }
      tmData$blast_hour[i] <- ifelse(tmData$blast_hour[i] == "13", "01",
                                     ifelse(tmData$blast_hour[i] == "14", "02",
                                            ifelse(tmData$blast_hour[i] == "15", "03",
                                                   ifelse(tmData$blast_hour[i] == "16", "04",
                                                          ifelse(tmData$blast_hour[i] == "17", "05",
                                                                 ifelse(tmData$blast_hour[i] == "18", "06",
                                                                        ifelse(tmData$blast_hour[i] == "19", "07",
                                                                               ifelse(tmData$blast_hour[i] == "20", "08",
                                                                                      ifelse(tmData$blast_hour[i] == "21", "09",
                                                                                             ifelse(tmData$blast_hour[i] == "22", "10",
                                                                                                    ifelse(tmData$blast_hour[i] == "23", "11",
                                                                                                           tmData$blast_hour[i])))))))))))
    }
    # View(tmData %>% select(Time, hour, minute, sec, blast_hour, blast_minute, blast_sec))
    
    tmData = tmData %>% mutate(
      blast_timestamp = paste0(as.character(strptime(Date, "%m/%d/%y")), " ", blast_hour, ":", blast_minute, ":", blast_sec, " EDT")
    )
    # tmData$blast_timestamp
    
    return(tmData)
    
  }
  
  tmBP.timestamp <- function(tmData, day_swings, date, tmName) {
    
    # tmName = "Alvarez, Patrick"
    # tmData <- tmBP %>% filter(Batter == tmName, Date == date)
    # day_swings = player_swings %>% filter(grepl("2022-09-20", formatted_timestamp))
    tmData <- tmData
    for (i in 1:nrow(tmData)) {
      if (tmData$blast_sec[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_sec[i] = paste0("0", tmData$blast_sec[i])
      }
      if (tmData$blast_minute[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_minute[i] = paste0("0", tmData$blast_minute[i])
      }
      if (tmData$blast_hour[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_hour[i] = paste0("0", tmData$blast_hour[i])
      }
      tmData$blast_hour[i] <- ifelse(tmData$blast_hour[i] == "13", "01",
                                     ifelse(tmData$blast_hour[i] == "14", "02",
                                            ifelse(tmData$blast_hour[i] == "15", "03",
                                                   ifelse(tmData$blast_hour[i] == "16", "04",
                                                          ifelse(tmData$blast_hour[i] == "17", "05",
                                                                 ifelse(tmData$blast_hour[i] == "18", "06",
                                                                        ifelse(tmData$blast_hour[i] == "19", "07",
                                                                               ifelse(tmData$blast_hour[i] == "20", "08",
                                                                                      ifelse(tmData$blast_hour[i] == "21", "09",
                                                                                             ifelse(tmData$blast_hour[i] == "22", "10",
                                                                                                    ifelse(tmData$blast_hour[i] == "23", "11",
                                                                                                           tmData$blast_hour[i])))))))))))
    }
    # View(tmData %>% select(Time, hour, minute, sec, blast_hour, blast_minute, blast_sec))
    
    tmData = tmData %>% mutate(
      blast_timestamp = paste0(Date, " ", blast_hour, ":", blast_minute, ":", blast_sec, " EDT")
    ) %>% arrange(desc(PitchNo))
    # tmData$blast_timestamp
    
    diffs2 = c()
    for (i in 1:nrow(tmData)) {
      diffs1 = c()
      for (j in 1:nrow(day_swings)) {
        # i=3
        diffs1 = append(abs(as.numeric(difftime(tmData$blast_timestamp[i], day_swings$extra_timestamp[j], units = "secs"))), diffs1)
      }
      diffs2 = append(min(diffs1), diffs2)
    }
    # diffs2
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    if (length(diffs2) > 3) {
      timestamp_diff = as.numeric(getmode(diffs2))
    } else {
      timestamp_diff = as.numeric(median(diffs2))
    }
    
    tmData[, 172:174] <- lapply(tmData[, 172:174], as.numeric)
    
    tmData <- tmData %>% mutate(
      blast_sec = blast_sec - timestamp_diff
    )
    
    for (i in 1:nrow(tmData)) {
      if(tmData$blast_sec[i] < 0) {
        tmData$blast_minute[i] = tmData$blast_minute[i] - 1
        tmData$blast_sec[i] = 60 + tmData$blast_sec[i]
      } 
      if (tmData$blast_minute[i] < 0) {
        tmData$blast_hour[i] = tmData$blast_hour[i] - 1
        tmData$blast_minute[i] = 60 + tmData$blast_minute[i]
      }
    }
    for (i in 1:nrow(tmData)) {
      if(tmData$blast_sec[i] == 60) {
        tmData$blast_minute[i] = tmData$blast_minute[i] + 1
        tmData$blast_sec[i] = 0
      }
    }
    tmData[, 172:174] <- lapply(tmData[, 172:174], as.character)
    
    for (i in 1:nrow(tmData)) {
      if (tmData$blast_sec[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_sec[i] = paste0("0", tmData$blast_sec[i])
      }
      if (tmData$blast_minute[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_minute[i] = paste0("0", tmData$blast_minute[i])
      }
      if (tmData$blast_hour[i] %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        tmData$blast_hour[i] = paste0("0", tmData$blast_hour[i])
      }
      tmData$blast_hour[i] <- ifelse(tmData$blast_hour[i] == "13", "01",
                                     ifelse(tmData$blast_hour[i] == "14", "02",
                                            ifelse(tmData$blast_hour[i] == "15", "03",
                                                   ifelse(tmData$blast_hour[i] == "16", "04",
                                                          ifelse(tmData$blast_hour[i] == "17", "05",
                                                                 ifelse(tmData$blast_hour[i] == "18", "06",
                                                                        ifelse(tmData$blast_hour[i] == "19", "07",
                                                                               ifelse(tmData$blast_hour[i] == "20", "08",
                                                                                      ifelse(tmData$blast_hour[i] == "21", "09",
                                                                                             ifelse(tmData$blast_hour[i] == "22", "10",
                                                                                                    ifelse(tmData$blast_hour[i] == "23", "11",
                                                                                                           tmData$blast_hour[i])))))))))))
    }
    # View(tmData %>% select(Time, hour, minute, sec, blast_hour, blast_minute, blast_sec))
    
    tmData = tmData %>% mutate(
      blast_timestamp = paste0(Date, " ", blast_hour, ":", blast_minute, ":", blast_sec, " EDT")
    )
    # tmData$blast_timestamp
    
    return(tmData)
    
  }
  
  write.player.swings <- function(name) {
    
    file = athletes[which(athletes$blast_name == name), "file_name"]
    player_swings = read.csv(paste0("~/Desktop/UNC Baseball /Blast/", date_range, "/", file), header=T, skip = 7, check.names = F)[, 1:17] # first read headers
    
    player_swings <- player_swings %>% mutate(
      formatted_timestamp = as.character(strptime(Date, "%b %d, %Y %H:%M:%S")),
      extra_timestamp = formatted_timestamp, 
      Note1 = NA, 
      Note2 = NA, 
      Note3 = NA, 
      Note4 = NA, 
      Note5 = NA,
      tmDate = as.character(strptime(Date, "%b %d, %Y"))
      )

    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    # create a new table in db to hold swings
    dbWriteTable(blastDb, value = player_swings, row.names = FALSE, name = paste0(name, "_swings"), append=T)
    # dbWriteTable(blastDb, value = player_swings, row.names = FALSE, name = paste0(name, "_swings"), overwrite=T)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    # # create id col query
    # query <- paste0("ALTER TABLE `blast`.`", name, "_swings` 
    #                 ADD COLUMN `id` INT NOT NULL AUTO_INCREMENT AFTER `Note5`,
    #                 ADD PRIMARY KEY (`id`);
    #                 ;")
    # # print(query)
    # blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    # rs = dbSendQuery(blastDb, query)
    # # Clear the result
    # dbClearResult(rs)
    # # Disconnect to clean up the connection to the database.
    # dbDisconnect(blastDb)
    
  }
  
  write.live <- function() {
    
    binded = data.frame()
    for (file in list.files("~/Desktop/UNC Baseball /Fall 2022 Postgame Files/")) {
      # file = list.files("~/Desktop/UNC Baseball /Fall 2022 Postgame Files/")[1]
      game <- read_xlsx(paste0("~/Desktop/UNC Baseball /Fall 2022 Postgame Files/", file))
      print(file)
      game$HomeTeamForeignID <- as.character(game$HomeTeamForeignID)
      game$AwayTeamForeignID <- as.character(game$AwayTeamForeignID)
      game$Time <- as.character(game$Time)
      game$Tilt <- as.character(game$Tilt)
      game$UTCDate <- as.character(game$UTCDate)
      game$UTCTime <- as.character(game$UTCTime)
      game$Year <- as.character(game$Year)
      game$ManualQAB <- as.character(game$ManualQAB)
      game$CS <- as.character(game$CS)
      game$SB <- as.character(game$SB)
      game$PB <- as.character(game$PB)
      game$ScoredRun <- as.character(game$ScoredRun)
      game$Fielder <- as.character(game$Fielder)
      game$DefensiveError <- as.character(game$DefensiveError)
      game$WebGem <- as.character(game$WebGem)
      game$PickedOff <- as.character(game$PickedOff)
      binded = bind_rows(game, binded)
    }
    tmData <- binded
    # unique(tmData$Date)
    
    tmData$Date <- ifelse(tmData$Date == "9/29/22", "09/29/22", tmData$Date)
    tmData$Time <- gsub("T", " ", tmData$Time)
    tmData$Time <- gsub("Z", "", tmData$Time)
    
    tmData <- tmData %>% mutate(
      Live = TRUE,
      hour = as.numeric(substr(Time, 12, 13)),
      minute = as.numeric(substr(Time, 15, 16)),
      sec = as.numeric(substr(Time, 18, 19)),
      blast_hour = hour,
      blast_minute = minute,
      blast_sec = sec
    ) %>% filter(!(is.na(hour)))
    tmData[, 252:254] <- lapply(tmData[, 252:254], as.character)
    

    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive", overwrite = T)
    # dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive")
    # dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive", append = T)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
  }
  
  write.bp <- function() {
    
    tmDataBP <- do.call(bind_rows, lapply(paste0("~/Desktop/UNC Baseball /Blast/tmBP/", list.files("~/Desktop/UNC Baseball /Blast/tmBP")), read.csv, header = T))
    
    tmDataBP <- tmDataBP %>% mutate(
      Live = FALSE
    )
    
    tmDataBP <- tmDataBP %>% mutate(
      hour = as.numeric(substr(Time, 1, 2)),
      minute = as.numeric(substr(Time, 4, 5)),
      sec = round(as.numeric(substr(Time, 7, 11))),
      blast_hour = hour,
      blast_minute = minute,
      blast_sec = sec
    ) %>% filter(!(is.na(hour)))
    # View(tmData %>% select(Time, hour, minute, sec, blast_hour, blast_minute, blast_sec))
    tmDataBP[, 172:174] <- lapply(tmDataBP[, 172:174], as.character)
    
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    dbWriteTable(blastDb, value = tmDataBP, row.names = FALSE, name = "tmBP", overwrite = T)
    # dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive")
    # dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive", append = T)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
  }
  
  write.player.joined.live <- function(name) {
    
    # name = "rusiecki"
    tmName = athletes[which(athletes$blast_name == name), "tm_name"]
    
    # Get table player_swings from db
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    table_name = paste0(name, "_swings")
    load_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_query)
    rs = dbSendQuery(blastDb, load_query)
    player_swings <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    # Get table tmLive from db
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    table_name = "tmLive"
    load_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_query)
    rs = dbSendQuery(blastDb, load_query)
    tmData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    joined_all = data.frame()
    for (date in unique(tmData$Date, na.rm=T)) {
      # date = "09/05/22"
      date2 = as.character(strptime(date, "%m/%d/%y"))
      if (TRUE %in% grepl(date2, unique(player_swings$formatted_timestamp)) & (tmName %in% filter(tmData, Date == date)$Batter) & 
          (nrow(tmData %>% filter(Batter == tmName, PitchCall %in% c("InPlay", "FoulBall"), Date == date)) > 0)) {
        
        tmDay <- tmData %>% filter(Batter == tmName, PitchCall %in% c("InPlay", "FoulBall"), Date == date)
        day_swings <- player_swings %>% filter(grepl(date2, formatted_timestamp))
        tmDay <- tmLive.timestamp(tmData = tmDay, day_swings = day_swings, tmName = tmName)
        
        joined <- nearestTime(tmDay, day_swings, "blast_timestamp", "formatted_timestamp")
        
        missing = c()
        for (i in 1:nrow(joined)) {
          # i = 1
          if ((abs(difftime(joined$blast_timestamp[i], joined$extra_timestamp[i], units = "secs")[[1]]) > 5)) {
            missing <- append(missing, i)
          }
        }
        missing
        
        joined[missing, 256:280] <- NA
        
        dup1 = which(duplicated(joined$extra_timestamp))
        omit = c()
        for (val in dup1) {
          # val = 18
          val = as.numeric(val)
          if (is.na(joined$Equipment[val])) {
            next
          }
          first_diff = as.numeric(difftime(joined$blast_timestamp[val-1], joined$extra_timestamp[val-1], units = "secs"))
          second_diff = as.numeric(difftime(joined$blast_timestamp[val], joined$extra_timestamp[val], units = "secs"))
          if (timestamp_diff == 0 & abs(first_diff) < abs(second_diff)) {
            omit = append(val, omit)
          } else if (timestamp_diff == 0 & abs(first_diff) > abs(second_diff)) {
            omit = append(val-1, omit)
          } else if (timestamp_diff != 0 & (timestamp_diff*first_diff) < 0) {
            omit = append(val-1, omit)
          } else if (timestamp_diff != 0 & (timestamp_diff*first_diff) >= 0) {
            omit = append(val, omit)
          } else {
            omit = append(val-1, omit)
            
          }
        }
        # omit
        joined[omit, 256:280] <- NA
        suppressMessages(joined_all <- bind_rows(joined_all, joined)) 
      }
    }
    if (nrow(joined_all) == 0) {
      return(print("no paired swings"))
    } else {
      joined_all <- joined_all %>%
        mutate(
          middle_of_field = ifelse(between(Direction, -15, 15), TRUE, FALSE),
          good_angle = ifelse(between(Angle, 5, 35), TRUE, FALSE)
        )
      
      if (!(any(duplicated(joined_all$extra_timestamp %>% na.omit())))) {
        blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
        dbWriteTable(blastDb, value = joined_all, row.names = FALSE, name = paste0(name, "_joinedLive"), overwrite = T)
        # dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive")
        # dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive", append = T)
        # Disconnect to clean up the connection to the database.
        dbDisconnect(blastDb)
      } else {
        paste0("address duplicates for ", name)
      }
    }
    
  }
  
  write.player.joined.bp <- function(name) {
    
    # name = "rusiecki"
    tmName = athletes[which(athletes$blast_name == name), "tm_name"]
    
    # Get table player_swings from db
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    table_name = paste0(name, "_swings")
    load_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_query)
    rs = dbSendQuery(blastDb, load_query)
    player_swings <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    # Get table tmBP from db
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    table_name = "tmBP"
    load_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_query)
    rs = dbSendQuery(blastDb, load_query)
    tmData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    joined_all = data.frame()
    for (date in unique(tmData$Date, na.rm=T)) {
      # date = "2022-09-15"
      if (TRUE %in% grepl(date, unique(player_swings$formatted_timestamp)) & (tmName %in% filter(tmData, Date == date)$Batter)) {
        
        tmDay <- tmData %>% filter(Batter == tmName, Date == date)
        day_swings <- player_swings %>% filter(grepl(date, formatted_timestamp))
        tmDay <- tmBP.timestamp(tmData = tmDay, day_swings = day_swings, tmName = tmName, date = date)
        
        joined <- nearestTime(tmDay, day_swings, "blast_timestamp", "formatted_timestamp")
        
        missing = c()
        for (i in 1:nrow(joined)) {
          # i = 1
          if ((abs(difftime(joined$blast_timestamp[i], joined$extra_timestamp[i], units = "secs")[[1]]) > 5)) {
            missing <- append(missing, i)
          }
        }
        # missing
        
        joined[missing, 176:193] <- NA
        
        diffs = c()
        for (i in 1:nrow(joined)) {
          diffs = append(as.numeric(difftime(joined$blast_timestamp[i], joined$extra_timestamp[i], units = "secs")), diffs)
        }
        diffs
        getmode <- function(v) {
          uniqv <- unique(v)
          uniqv[which.max(tabulate(match(v, uniqv)))]
        }
        if (length(diffs) > 3) {
          timestamp_diff = as.numeric(getmode(diffs %>% na.omit()))
        } else {
          timestamp_diff = as.numeric(median(diffs, na.rm = T))
        }
        
        dup1 = which(duplicated(joined$extra_timestamp))
        omit = c()
        for (val in dup1) {
          # val = 18
          val = as.numeric(val)
          if (is.na(joined$Equipment[val])) {
            next
          }
          first_diff = as.numeric(difftime(joined$blast_timestamp[val-1], joined$extra_timestamp[val-1], units = "secs"))
          second_diff = as.numeric(difftime(joined$blast_timestamp[val], joined$extra_timestamp[val], units = "secs"))
          if (timestamp_diff == 0 & abs(first_diff) < abs(second_diff)) {
            omit = append(val, omit)
          } else if (timestamp_diff == 0 & abs(first_diff) > abs(second_diff)) {
            omit = append(val-1, omit)
          } else if (timestamp_diff != 0 & (timestamp_diff*first_diff) < 0) {
            omit = append(val-1, omit)
          } else if (timestamp_diff != 0 & (timestamp_diff*first_diff) >= 0) {
            omit = append(val, omit)
          } else {
            omit = append(val-1, omit)
            
          }
        }
        # omit
        joined[omit, 176:193] <- NA
        
        # joined_all <- bind_rows(joined_all, joined)
        suppressMessages(joined_all <- bind_rows(joined_all, joined))
      }
    }
    
    if (nrow(joined_all) == 0) {
      return(print("no paired swings"))
    } else {
      joined_all <- joined_all %>%
        mutate(
          middle_of_field = ifelse(between(Direction, -15, 15), TRUE, FALSE),
          good_angle = ifelse(between(Angle, 5, 35), TRUE, FALSE)
        )
      
      if (!(any(duplicated(joined_all$extra_timestamp %>% na.omit())))) {
        blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
        dbWriteTable(blastDb, value = joined_all, row.names = FALSE, name = paste0(name, "_joinedBP"), overwrite = T)
        # dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive")
        # dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive", append = T)
        # Disconnect to clean up the connection to the database.
        dbDisconnect(blastDb)
      } else {
        paste0("address duplicates for ", name)
      }
    }
    
  }
  
}

## Write Trackman data to db (BP and Live)
# Read in corresponding TM data
sep9 <- read.csv("~/Desktop/UNC Baseball /Blast/TM/Sep9-Game.csv") %>% select(-c(HomeTeamForeignID, AwayTeamForeignID))
sep12 <- read.csv("~/Desktop/UNC Baseball /Blast/TM/Sep12-Game.csv") %>% select(-c(HomeTeamForeignID, AwayTeamForeignID))
sep13 <- read.csv("~/Desktop/UNC Baseball /Blast/TM/Sep13-Game.csv") %>% select(-c(HomeTeamForeignID, AwayTeamForeignID))
tmDataLive <- bind_rows(sep9, sep12, sep13)
# unique(tmData$Date)
tmDataLive$Date <- ifelse(tmDataLive$Date == "", NA, tmDataLive$Date)

# write into db
write.live(tmDataLive)
write.bp(tmDataBP)

date_range = "September 5 - 30"
csv_list = list.files(paste0("~/Desktop/UNC Baseball /Blast/", date_range))
blast_names = (athletes %>% filter(file_name %in% csv_list))$blast_name

## Write swings, joinedLive, and joinedBP for all athletes ##
for (athlete in blast_names) {
  # athlete = "cook"
  print(athlete)
  suppressMessages(write.player.swings(athlete))
  print("swings done")
  suppressMessages(write.player.joined.live(athlete))
  print("live done")
  supppressMessages(write.player.joined.bp(athlete))
  print("BP done")
}

# Bind all hitters' joined BP data for Blast research group to use
for (athlete in blast_names) {
  # athlete = "cook"
  print(athlete)
  
  # load all of table query
  table_name = paste0(athlete, "_joinedBP")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  print(load_all_query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  player_joinedBP <- dbFetch(rs)
  # Clear the result
  dbClearResult(rs)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  dbWriteTable(blastDb, value = player_joinedBP, row.names = FALSE, name = "fullteam_joinedBP", append = T)
  # dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive")
  # dbWriteTable(blastDb, value = tmData, row.names = FALSE, name = "tmLive", append = T)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  
}

# load all of table query
table_name = paste0("fullteam_joinedBP")
load_all_query <- paste0("SELECT * FROM ", table_name, ";")
print(load_all_query)
blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
rs = dbSendQuery(blastDb, load_all_query)
fullteam_joinedBP <- dbFetch(rs)
# Clear the result
dbClearResult(rs)
# Disconnect to clean up the connection to the database.
dbDisconnect(blastDb)

# fullteam_joinedBP <- fullteam_joinedBP %>% filter(!(is.na(Equipment)))
# write.csv(fullteam_joinedBP, "Blast BP (Sep 5-30).csv")

