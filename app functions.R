
userpassword <- "Remy2022"

recent.session.summary <- function(name) {
  
  # name = "horvath"
  # load all of table query
  table_name = paste0(name, "_joinedBP")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  # print(load_all_query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  # Clear the result
  dbClearResult(rs)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  data1 <- loadedData
 
  # load all of table query
  table_name = paste0(name, "_swings")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  # print(load_all_query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  # Clear the result
  dbClearResult(rs)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  data2 <- loadedData

  date = as.character(max(as.Date(unique(data2$tmDate))))
  summary_session1 <- data1 %>% filter(Date...2 == date) %>% summarise(
    # avg_plane_score = median(`Plane Score`, na.rm=T),
    # avg_connection_score = median(`Connection Score`, na.rm=T),
    # avg_rotation_score = median(`Rotation Score`, na.rm=T),
    # avg_bat_speed = median(`Bat Speed (mph)`, na.rm=T),
    # avg_hand_speed = median(`Peak Hand Speed (mph)`, na.rm=T),
    # avg_timetocontact= median(`Time to Contact (sec)`, na.rm=T),
    # avg_attack_angle = median(`Attack Angle (deg)`, na.rm=T),
    ev90th = round(quantile(ExitSpeed, .9, na.rm = T), 1),
    middle_pct = round((sum(middle_of_field, na.rm = T)/n())*100, 1),
    good_angle_pct = round((sum(good_angle, na.rm=T)/n())*100, 1)
  )
  colnames(summary_session1) <- c(
    # "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)", 
    # "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)", 
    "90th percentile Exit Velo", "Gap 2 Gap %", "Quality Launch Angle %")
  summary_season1 <- data1 %>% filter(Date...2 != date) %>% summarise(
    # avg_plane_score = median(`Plane Score`, na.rm=T),
    # avg_connection_score = median(`Connection Score`, na.rm=T),
    # avg_rotation_score = median(`Rotation Score`, na.rm=T),
    # avg_bat_speed = median(`Bat Speed (mph)`, na.rm=T),
    # avg_hand_speed = median(`Peak Hand Speed (mph)`, na.rm=T),
    # avg_timetocontact= median(`Time to Contact (sec)`, na.rm=T),
    # avg_attack_angle = median(`Attack Angle (deg)`, na.rm=T),
    ev90th = round(quantile(ExitSpeed, .9, na.rm = T), 1),
    middle_pct = round((sum(middle_of_field, na.rm = T)/n())*100, 1),
    good_angle_pct = round((sum(good_angle, na.rm=T)/n())*100, 1)
  )
  colnames(summary_season1) <- c(
    # "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)", 
    #                             "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)", 
    "90th percentile Exit Velo", "Gap 2 Gap %", "Quality Launch Angle %")
  df=bind_rows(summary_session1, summary_season1)
  rownames(df) <- c("Most Recent Session", "Season")
  
  summary_session2 <- data2 %>% filter(tmDate == date) %>% summarise(
    n = n(),
    avg_plane_score = round(median(`Plane Score`, na.rm=T), 1),
    avg_connection_score = round(median(`Connection Score`, na.rm=T), 1),
    avg_rotation_score = round(median(`Rotation Score`, na.rm=T), 1),
    avg_bat_speed = round(median(`Bat Speed (mph)`, na.rm=T), 1),
    avg_hand_speed = round(median(`Peak Hand Speed (mph)`, na.rm=T), 1),
    avg_timetocontact= round(median(`Time to Contact (sec)`, na.rm=T), 2),
    avg_attack_angle = round(median(`Attack Angle (deg)`, na.rm=T), 1)
  )
  colnames(summary_session2) <- c(
    "# Swings", "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)",
    "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)"
  )
  summary_season2 <- data2 %>% filter(tmDate != date) %>% summarise(
    n = n(),
    avg_plane_score = round(median(`Plane Score`, na.rm=T), 1),
    avg_connection_score = round(median(`Connection Score`, na.rm=T), 1),
    avg_rotation_score = round(median(`Rotation Score`, na.rm=T), 1),
    avg_bat_speed = round(median(`Bat Speed (mph)`, na.rm=T), 1),
    avg_hand_speed = round(median(`Peak Hand Speed (mph)`, na.rm=T), 1),
    avg_timetocontact= round(median(`Time to Contact (sec)`, na.rm=T), 2),
    avg_attack_angle = round(median(`Attack Angle (deg)`, na.rm=T), 1)
    
  )
  colnames(summary_season2) <- c(
    "# Swings", "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)",
    "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)"
  )
  
  df2=bind_rows(summary_session2, summary_season2)
  rownames(df2) <- c("Most Recent Session", "Season")
  
  df_final = merge(df2, df, by=0, all=T)
  df_final = df_final[, 2:12] %>% arrange(`# Swings`)
  
  df_final[is.na(df_final)] <- ""
  # df_final[is.nan(df_final)] <- ""
  
  rownames(df_final) <- c("Most Recent Session", "Season")
  
  colnames(df_final) <- c(
    "# Swings", "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)",
    "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)", "90th percentile Exit Velo", "Gap 2 Gap %", "Quality Launch Angle %"
  )
  
  return(df_final)
}

best.swings <- function(name, metric) {
  
  # metric = "Plane Score (20-80)"
  
  # name = "horvath"
  # load all of table query
  table_name = paste0(name, "_joinedBP")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  # print(load_all_query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  # Clear the result
  dbClearResult(rs)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  data <- loadedData
  
  df <- data %>% select(`Plane Score`, `Connection Score`, `Rotation Score`,`Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Time to Contact (sec)`)
  colnames(df) <- c("Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)", 
                                 "Hand Speed (mph)", "Time to Contact (sec)")
  df$`Video Link` <- "Link Here"
  df[, 1:6] <- lapply(df[, 1:6], as.numeric)
  if (metric %in% c("Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)", 
                    "Hand Speed (mph)")) {
    ret <- df %>% select(`metric`, `Video Link`)
    return(ret[order(-ret[,`metric`]), ])
  }
  if (metric == "Time to Contact (sec)") {
    ret <- df %>% select(`metric`, `Video Link`)
    return(ret[order(ret[,`metric`]), ])
  }
}

worst.swings <- function(name, metric) {
  
  # metric = "Plane Score (20-80)"
  
  # name = "horvath"
  # load all of table query
  table_name = paste0(name, "_joinedBP")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  # print(load_all_query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  # Clear the result
  dbClearResult(rs)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  data <- loadedData
  
  df <- data %>% select(`Plane Score`, `Connection Score`, `Rotation Score`,`Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Time to Contact (sec)`)
  colnames(df) <- c("Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)", 
                    "Hand Speed (mph)", "Time to Contact (sec)")
  df$`Video Link` <- "Link Here"
  df[, 1:6] <- lapply(df[, 1:6], as.numeric)
  if (metric %in% c("Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)", 
                    "Hand Speed (mph)")) {
    ret <- df %>% select(`metric`, `Video Link`)
    return(ret[order(ret[,`metric`]), ])
  }
  if (metric == "Time to Contact (sec)") {
    ret <- df %>% select(`metric`, `Video Link`)
    return(ret[order(-ret[,`metric`]), ])
  }
  
}

session.summary <- function(name, date) {
  
  # name = "horvath"
  # load all of table query
  table_name = paste0(name, "_joinedBP")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  # print(load_all_query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  # Clear the result
  dbClearResult(rs)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  data1 <- loadedData
  
  # load all of table query
  table_name = paste0(name, "_swings")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  # print(load_all_query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  # Clear the result
  dbClearResult(rs)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  data2 <- loadedData
  
  summary_session1 <- data1 %>% filter(Date...2 == date) %>% summarise(
    # avg_plane_score = median(`Plane Score`, na.rm=T),
    # avg_connection_score = median(`Connection Score`, na.rm=T),
    # avg_rotation_score = median(`Rotation Score`, na.rm=T),
    # avg_bat_speed = median(`Bat Speed (mph)`, na.rm=T),
    # avg_hand_speed = median(`Peak Hand Speed (mph)`, na.rm=T),
    # avg_timetocontact= median(`Time to Contact (sec)`, na.rm=T),
    # avg_attack_angle = median(`Attack Angle (deg)`, na.rm=T),
    ev90th = round(quantile(ExitSpeed, .9, na.rm = T), 1),
    middle_pct = round((sum(middle_of_field, na.rm = T)/n())*100, 1),
    good_angle_pct = round((sum(good_angle, na.rm=T)/n())*100, 1)
  )
  colnames(summary_session1) <- c(
    # "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)", 
                                 # "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)", 
                                 "90th percentile Exit Velo", "Gap 2 Gap %", "Quality Launch Angle %")
  summary_season1 <- data1 %>% filter(Date...2 != date) %>% summarise(
    # avg_plane_score = median(`Plane Score`, na.rm=T),
    # avg_connection_score = median(`Connection Score`, na.rm=T),
    # avg_rotation_score = median(`Rotation Score`, na.rm=T),
    # avg_bat_speed = median(`Bat Speed (mph)`, na.rm=T),
    # avg_hand_speed = median(`Peak Hand Speed (mph)`, na.rm=T),
    # avg_timetocontact= median(`Time to Contact (sec)`, na.rm=T),
    # avg_attack_angle = median(`Attack Angle (deg)`, na.rm=T),
    ev90th = round(quantile(ExitSpeed, .9, na.rm = T), 1),
    middle_pct = round((sum(middle_of_field, na.rm = T)/n())*100, 1),
    good_angle_pct = round((sum(good_angle, na.rm=T)/n())*100, 1)
  )
  colnames(summary_season1) <- c(
    # "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)", 
    #                             "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)", 
                                "90th percentile Exit Velo", "Gap 2 Gap %", "Quality Launch Angle %")
  df=bind_rows(summary_session1, summary_season1)
  rownames(df) <- c("Session", "Season")
  
  summary_session2 <- data2 %>% filter(tmDate == date) %>% summarise(
    n = n(),
    avg_plane_score = median(`Plane Score`, na.rm=T),
    avg_connection_score = median(`Connection Score`, na.rm=T),
    avg_rotation_score = median(`Rotation Score`, na.rm=T),
    avg_bat_speed = median(`Bat Speed (mph)`, na.rm=T),
    avg_hand_speed = median(`Peak Hand Speed (mph)`, na.rm=T),
    avg_timetocontact= median(`Time to Contact (sec)`, na.rm=T),
    avg_attack_angle = median(`Attack Angle (deg)`, na.rm=T)
  )
  colnames(summary_session2) <- c(
    "# Swings", "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)",
    "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)")
  summary_season2 <- data2 %>% filter(tmDate != date) %>% summarise(
    n = n(),
    avg_plane_score = median(`Plane Score`, na.rm=T),
    avg_connection_score = median(`Connection Score`, na.rm=T),
    avg_rotation_score = median(`Rotation Score`, na.rm=T),
    avg_bat_speed = median(`Bat Speed (mph)`, na.rm=T),
    avg_hand_speed = median(`Peak Hand Speed (mph)`, na.rm=T),
    avg_timetocontact= median(`Time to Contact (sec)`, na.rm=T),
    avg_attack_angle = median(`Attack Angle (deg)`, na.rm=T)
    
  )
  colnames(summary_season2) <- c(
    "# Swings", "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)",
                                "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)"
    )
  df2=bind_rows(summary_session2, summary_season2)
  rownames(df2) <- c("Session", "Season")
  
  df_final = merge(df2, df, by=0, all=T)
  df_final = df_final[, 2:12] %>% arrange(`# Swings`)
  
  df_final[is.na(df_final)] <- ""
  # df_final[is.nan(df_final)] <- ""
  
  rownames(df_final) <- c("Session", "Season")
  
  colnames(df_final) <- c(
    "# Swings", "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)",
    "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)", "90th percentile Exit Velo", "Gap 2 Gap %", "Quality Launch Angle %"
  )
  
  return(df_final)
  
}

notebook.table <- function(name) {
  
  # name = "alvarez"
  # date = "2022-09-14"
  
  # load all of table query
  table_name = paste0(name, "_joinedBP")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  # print(load_all_query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  # Clear the result
  dbClearResult(rs)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  data <- loadedData

    table <- data %>% select(id, Date...2, `Plane Score`, `Connection Score`, `Rotation Score`, `Bat Speed (mph)`, `Peak Hand Speed (mph)`,
                                                          `Time to Contact (sec)`, `Attack Angle (deg)`, ExitSpeed, middle_of_field, good_angle, Note1) %>%
      filter(!(is.na(`Bat Speed (mph)`)))
    
    table$`Video Link` <- "Link Here"
    table$ExitSpeed <- round(table$ExitSpeed, 1)
    
    table[is.na(table)] <- ""
    
    table$middle_of_field = ifelse(table$middle_of_field == 1, "Yes", 
                                   ifelse(table$middle_of_field == 0, "No", ""))
    table$good_angle = ifelse(table$good_angle == 1, "Yes", 
                              ifelse(table$good_angle == 0, "No", ""))
    
    table <- table[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 13)]
    
    colnames(table) <- c("Swing ID", "Date", "Plane Score (20-80)", "Connection Score (20-80)", "Rotation Score (20-80)", "Bat Speed (mph)", 
                         "Hand Speed (mph)", "Time to Contact (sec)", "Attack Angle (deg)", 
                         "Exit Velo", "Gap 2 Gap", "Quality Launch Angle", "Video Link", "Note")
    
    return(table)
  
}

notebook.test <- function(name) {
  
  # name = "alvarez"
  # date = "2022-09-14"
  
  # load all of table query
  table_name = paste0(name, "_joinedBP")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  # print(load_all_query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  # Clear the result
  dbClearResult(rs)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  data <- loadedData
  
  return(data)
  
}

# data = notebook.add.table(name = "blaser", date = date)

get.notes <- function(name) {
  
  table_name = paste0(name, "_notes")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  return(loadedData)
  
}

recent.notes <- function(name) {
  
  df = data.frame(get.notes(name)) %>% arrange(desc(id))
  df %>% select(-id) %>% rename("Swing ID" = "Swing.ID")
  
}

recent.mechanics <- function(name) {
  
  df = data.frame(get.mechanics(name)) %>% arrange(desc(id))
  df %>% select(-id)
  
}

add.swing.note <- function(id, note, name, date) {
  
    # note = "test live"
    # id = 58
    query <- paste0("UPDATE `blast`.`", name, "_joinedBP` SET `Note1` = '", note, "' WHERE (`id` = ", id, ");")
    # print(query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, query)
    dbClearResult(rs)
    dbDisconnect(blastDb)
    
    query <- paste0("INSERT INTO `", name, "_notes` (`Date`,`Type`,`Swing ID`,`Note`) VALUES ('", date, "','Swing','", id, "','", note, "');")
    # print(query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, query)
    dbClearResult(rs)
    dbDisconnect(blastDb)
  
  
}

add.session.note <- function(note, name, date) {
    
    query <- paste0("INSERT INTO `", name, "_notes` (`Date`,`Type`,`Note`) VALUES ('", date, "','Session','", note, "');")
    # print(query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, query)
    dbClearResult(rs)
    dbDisconnect(blastDb)
  
  
}

# add.swing.note(33, "", "blaser", "2022-09-13")

edit.note <- function (id, name, note) {
  
  query <- paste0("UPDATE `blast`.`", name, "_joinedBP` SET `Note1` = '", note, "' WHERE (`id` = ", id, ");")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
  query <- paste0("UPDATE `blast`.`", name, "_notes` SET `Note` = '", note, "' WHERE (`Swing ID` = ", id, ");")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
}

delete.note <- function (id1, id2, name) {

    table_name = paste0(name, "_notes")
    query <- paste0("DELETE FROM `", table_name, "` WHERE `id` = ", id1, ";")
    # print(query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, query)
    dbClearResult(rs)
    dbDisconnect(blastDb)
    
    query <- paste0("UPDATE `blast`.`", name, "_joinedBP` SET `Note1` = NULL WHERE (`id` = ", id2, ");")
    # print(query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, query)
    dbClearResult(rs)
    dbDisconnect(blastDb)

}

# delete.note(10, "alvarez")

log.mechanics <- function(name, date, desc, reas) {
  
  query <- paste0("INSERT INTO `", name, "_mechanics` (`Date`,`Description`,`Reasoning`) VALUES ('", date, "','", desc, "','", reas, "');")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
}

get.mechanics <- function(name) {
  
  table_name = paste0(name, "_mechanics")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  return(loadedData)
  
}

edit.mechanics <- function (id, name, desc, reas) {
  
  query <- paste0("UPDATE `blast`.`", name, "_mechanics` SET `Description` = '", desc, "' WHERE (`id` = ", id, ");")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
  query <- paste0("UPDATE `blast`.`", name, "_mechanics` SET `Reasoning` = '", reas, "' WHERE (`id` = ", id, ");")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
}

delete.mechanics <- function (id, name) {
  
  table_name = paste0(name, "_mechanics")
  query <- paste0("DELETE FROM `", table_name, "` WHERE `id` = ", id, ";")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
}


