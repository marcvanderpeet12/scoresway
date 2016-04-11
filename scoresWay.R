library(httr)
library(RCurl)
library(XML)

getSeasonData <- function(compID, season, scrapeNew = FALSE){
  #Loading data from .csv
  fileName <- paste0("seasonData_", compID, "_", gsub("/", "-", season, fixed = T), ".csv")
  fileName <- paste0("data/seasons/", fileName)
  if(scrapeNew == FALSE){
    if(file.exists(fileName)){
      retTable <- read.csv(fileName, colClasses = "character")[, -1]
      return(retTable)}}
  
  #Season page
  tryCount <- 0
  repeat{
    tryCount <- tryCount + 1
    if(tryCount == 10){
      return(stop(" Couldn't connect to the database."))}
    url <- paste0(
      "http://www.scoresway.com/?sport=soccer&page=competition&id=", compID,
      "&view=matches")
    cat("\n")
    cat(url)
    cat("\n")
    con <- getURL(url)
    tre <- htmlParse(con)
    
    #Season ID
    seaNode <- getNodeSet(tre, '//select[@name="season_id"]/option')
    seaVals <- xmlSApply(seaNode, xmlValue, trim = T)
    seaNode <- seaNode[[which(seaVals == season)]]
    sID <- gsub(
      "?sport=soccer&page=season&id=", "", fixed = T,
      xmlGetAttr(seaNode, "value"))
    
    if(class(sID) != 'try-error'){
      break}}
  
  #Round page
  tryCount <- 0
  repeat{
    tryCount <- tryCount + 1
    if(tryCount == 10){
      return(stop(" Couldn't connect to the database."))}
    url <- paste0(
      "http://www.scoresway.com/?sport=soccer&page=season&id=", sID)
    
    cat("\n")
    cat(url)
    cat("\n")
    con <- getURL(url)
    tre <- htmlParse(con)
    
    #Round ID
    rndNode <- getNodeSet(tre, '//select[@name="round_id"]//option')
    if(is.null(rndNode)){
      rndNode <- getNodeSet(tre, '//div[@class="nav clearfix"]/div')
      rID <- try(xmlGetAttr(rndNode[[1]], 'data-round_id'))
    }else{
      rID <- xmlSApply(rndNode, xmlGetAttr, "value")
      rID <- gsub("?sport=soccer&page=round&id=", "", rID, fixed = T)}
    
    if(class(rID) != 'try-error'){
      break}}
  
  #Result page
  getRound <- function(rID, sID, compID){
    w <- 0
    breakFlag <- F
    retTable <- c()
    repeat{
      w <- w + 1
      getID <- function(w){
        url <- paste0(
          "http://www.scoresway.com/b/block.competition_matches.fixtures_results?gameweek=", w,
          "&round_id=", rID,
          "&season_id=", sID,
          "&competition_id=", compID,
          "&sport=soccer&localization_id=www")
        cat("\n")
        cat(url)
        cat("\n")
        con <- getURL(url)
        tre <- htmlParse(con)
        
        endNote <- try(xmlValue(getNodeSet(tre, '//div[@class="notice fully-padded"]')[[1]], trim = T), silent = T)
        if(endNote == 'No matches available.'){
          return("STOP")}
        
        tab <- readHTMLTable(tre)[[1]][, 1:5]
        tab <- tab[complete.cases(tab), ]
        
        #Match IDs
        mtcNode <- getNodeSet(tre, '//tr')
        mID <- unlist(xmlSApply(mtcNode, xmlGetAttr, "id"))
        mID <- unlist(sapply(mID, strsplit, "match-", fixed = T))
        mID <- mID[seq(2, length(mID), 2)]			
        tab <- cbind(tab, "ID" = mID)
        
        #Team IDs
        tidNode <- list(
          getNodeSet(tre, '//td[@class="team team-a "]/a'),
          getNodeSet(tre, '//td[@class="team team-b "]/a'),
          getNodeSet(tre, '//td[@class="team team-a strong"]/a'),
          getNodeSet(tre, '//td[@class="team team-b strong"]/a'))
        tidNode <- unlist(tidNode)
        
        tID <- unique(gsub("?sport=soccer&page=team&id=", "", sapply(tidNode, xmlGetAttr, "href"), fixed = T))
        names(tID) <- unique(sapply(tidNode, xmlValue, trim = T))
        tID <- cbind(
          cbind(tID[as.character(tab[, "Home team"])]),
          cbind(tID[as.character(tab[, "Away team"])]))
        
        tab <- cbind(tab, tID)
        colnames(tab) <- c("Day", "Date", "Home Team", "Score.Time", "Away.Team", "Match.ID", "Home.Team.ID", "Away.Team.ID")
        return(tab)}
      
      tryCount <- 0
      repeat{
        tryCount <- tryCount + 1
        if(tryCount == 10){
          return(stop(" Data not available."))}
        # wTab <- try(getID(w), silent = T)
        wTab <- try(getID(w))
        
        if(class(wTab) != 'try-error'){
          if(class(wTab) == 'character'){
            breakFlag <- TRUE}
          break}}
      
      if(breakFlag){
        break}
      
      if(length(intersect(wTab[, "Match.ID"], retTable[, "Match.ID"])) > 0){
        break}
      
      retTable <- rbind(retTable, wTab)}
    rownames(retTable) <- NULL
    retTable <- retTable[!duplicated(retTable), ]
    return(retTable)}
  
  endTable <- c()
  for(i in seq_along(rID)){
    rndTable <- getRound(rID = rID[i], sID = sID, compID = compID)
    endTable <- rbind(endTable, rndTable)}
  
  #Saving times to .rds
  sTimes <- strsplit(as.character(endTable[, "Score.Time"]), " : ", fixed = T)
  sTimes <- sapply(sTimes, strsplit, " : ", fixed = T)
  timeRows <- sapply(sTimes, length) == 2
  sTimes <- sTimes[timeRows]
  sTimes <- sapply(sTimes, paste, collapse = ":")
  nmVect <- as.character(endTable[timeRows, "Match.ID"])
  
  for(i in seq_along(sTimes)){
    tName <- paste0("data/times/", nmVect[i], "_time.rds")
    saveRDS(sTimes[i], tName)}
  
  #Getting remaining times
  mTimes <- c()
  print("Getting match times...")
  for(i in 1:nrow(endTable)){
    print(paste0(i, "/", nrow(endTable)))
    mID <- as.character(endTable[i, "Match.ID"])
    mTimes[i] <- getTime(mID)}
  
  endTable <- cbind(endTable,
                    "Time" = mTimes,
                    "Competition.ID" = rep(compID, nrow(endTable)))
  
  #Saving data to .csv
  write.csv(endTable, fileName)
  return(endTable)}

getMatchIDs <- function(compID, startDate, endDate){
  #Possible date formats: "%d-%m-%Y", "%Y-%m-%d" and "%Y/%m/%d"
  dateRange <- c(startDate, endDate)
  
  dRange <- as.Date(dateRange)
  
  if(!dRange[1] > as.Date("1900-01-01")){
    dRange <- as.Date(dateRange, format = "%d-%m-%Y")
  }
  yRange <- as.numeric(unique(format(dRange, "%Y")))
  yRange <- seq(yRange[1]-1, yRange[2]+1, 1)
  
  sRange <- c()
  for(i in 1:(length(yRange)-1)){
    sRange[i] <- paste0(yRange[i], "/", yRange[i+1])}
  
  endData <- list()
  for(i in seq_along(sRange)){
    seaData <- try(getSeasonData(compID, sRange[i]), silent = T)
    if(class(seaData) == 'try-error'){
      break}
    endData[[i]] <- seaData}
  return(endData)
  
  vDates <- endData[, "Date"]
  for(i in seq_along(vDates)){
    vD <- strsplit(vDates[[i]], "/", fixed = T)[[1]]
    vDates[i] <- paste0("20", vD[3], "-", vD[2], "-", vD[1])}
  vDates <- as.Date(vDates)
  
  endData <- endData[vDates >= dRange[1] & vDates <= dRange[2], ]
  return(endData[, "ID"])}

getMatchInfo <- function(matchID){
  fileName <- paste0("data/matches/", matchID, ".rds")
  if(file.exists(fileName)){
    print(fileName)
    retTable <- readRDS(fileName)
    return(retTable)}
  
  tryCount <- 0
  repeat{
    tryCount <- tryCount + 1
    if(tryCount == 10){
      return(stop(" Couldn't connect to the database."))}
    
    url <- paste0("http://www.scoresway.com/?sport=soccer&page=match&id=", matchID)
    cat("\n")
    cat(url)
    cat("\n\n")
    con <- getURL(url)
    tre <- htmlParse(con)
    
    #Team ID's
    hID <- try(gsub(
      "?sport=soccer&page=team&id=", "",
      xmlGetAttr(getNodeSet(tre, '//div[@class="container left"]/h3/a')[[1]], "href"),
      fixed = T), silent = T)
    
    if(class(hID) != 'try-error'){
      break}}
  
  
  aID <- gsub(
    "?sport=soccer&page=team&id=", "",
    xmlGetAttr(getNodeSet(tre, '//div[@class="container right"]/h3/a')[[1]], "href"),
    fixed = T)
  
  #Competition ID
  cNode <- getNodeSet(tre, '//ul[@class="tree level-1 expanded"]//a')[[1]]
  cID <- gsub(
    "?sport=soccer&page=competition&id=", "",
    xmlGetAttr(cNode, "href"),
    fixed = T)
  
  #Kickoff time
  kTime <- xmlValue(getNodeSet(tre, '//div[@class="details clearfix"]//span[@data-format="HH:MM"]')[[1]], trim = T)
  saveRDS(kTime, paste0("data/times/", matchID, "_time.rds"))
  
  getPlayerDetails <- function(pitch){
    retTable <- c()
    for(i in seq_along(mainNode)){
      pNode <- mainNode[[i]]
      
      #Position
      POS <- xmlGetAttr(pNode, "class")
      if(grepl("referee", POS)){
        next}
      if(grepl("sub", POS)){
        next}
      if(grepl("coach", POS)){
        next}
      
      #Turf
      POS <- strsplit(POS, " ", fixed = T)[[1]]
      TRF <- POS[length(POS)]
      POS <- POS[-length(POS)]
      POS <- POS[-1]
      POS <- paste(POS, collapse = " ")
      
      #Name
      NAM <- xmlValue(getNodeSet(pNode, './/div[@class="playername"]')[[1]], trim = T)
      
      #ID
      PID <- xmlGetAttr(getNodeSet(pNode, './/div[@class="playername"]/a')[[1]], "href")
      PID <- gsub("?sport=soccer&page=player&id=", "", PID, fixed = T)
      
      #Events
      MIN <- 0
      MOT <- 90
      eNode <- getNodeSet(pNode, './/div[@class="events"]/span')
      if(length(eNode) != 0){
        sCls <- xmlSApply(eNode, xmlGetAttr, "class")
        if("si_so" %in% sCls){
          eVal <- sapply(xmlChildren(xmlParent(eNode[[1]])), xmlValue, trim = T)
          MOT <- eVal[which(grepl('for', eVal)) - 1]
          MOT <- gsub("'", "", MOT, fixed = T)}}
      
      res <- cbind(
        "Turf" = TRF,
        "Name" = NAM,
        "Player ID" = PID,
        "Position" = POS,
        "Minute In" = MIN,
        "Minute Out" = MOT)
      
      retTable <- rbind(retTable, res)}
    rownames(retTable) <- NULL
    return(retTable)}
  
  mainNode <- getNodeSet(tre, '//div[@class="pitch_view_container"]/div/div/div')
  if(!is.null(mainNode)){
    retTable <- getPlayerDetails(mainNode)
  }else{
    mainNode <- getNodeSet(tre, '//div[@class="lineups"]//li')
    retTable <- c()
    for(i in seq_along(mainNode)){
      pNode <- getNodeSet(mainNode[[i]], './/span[@class="player"]')[[1]]
      nNode <- getNodeSet(pNode, './/span[@class="name"]/a')[[1]]
      NAM <- xmlValue(nNode, trim = T)
      PID <- gsub("?sport=soccer&page=player&id=", "", xmlGetAttr(nNode, 'href'), fixed = T)
      MOT <- try(getNodeSet(pNode, './/span[@class="name"]//span[@class="evt-icon evt-icon-SO"]')[[1]], silent = T)
      if(class(MOT) == 'try-error'){
        MOT <- 90}
      
      res <- cbind(
        "Name" = NAM,
        "Player.ID" = PID,
        "Position" = NA,
        "Minute.In" = 0,
        "Minute.Out" = MOT)
      retTable <- rbind(retTable, res)}
    retTable <- cbind(
      "Turf" = rep(c("home", "away"), each = 11),
      retTable)}
  
  TID <- rep(NA, nrow(retTable))
  TID[retTable[, "Turf"] == "home"] <- hID
  TID[retTable[, "Turf"] == "away"] <- aID
  
  MID <- rep(matchID, nrow(retTable))
  CID <- rep(cID, nrow(retTable))
  
  retTable <- cbind(
    "Match.ID" = MID,
    "Competition.ID" = CID,
    retTable,
    "Team.ID" = TID)
  saveRDS(retTable, fileName)
  return(retTable)}

getMatchData <- function(compID, startDate, endDate){
  mIDs <- getMatchIDs(compID, startDate, endDate)
  
  retTable <- c()
  for(i in seq_along(mIDs)){
    mTable <- getMatchInfo(mIDs[i])
    retTable <- rbind(retTable, mTable)}
  rownames(retTable) <- NULL
  return(retTable)}

getTime <- function(matchID){
  fileName <- paste0("data/times/", matchID, "_time.rds")
  if(file.exists(fileName)){
    mTime <- readRDS(fileName)
  }else{
    x <- try(getMatchInfo(matchID))
    if(class(x) != 'try-error'){
      mTime <- getTime(matchID)
    }else{
      return(stop(" Couldn't connect to the database."))}}
  return(mTime)}


getSeasonData(7, "2003/2004", T)
getSeasonData(7, "2004/2005", T)
getSeasonData(7, "2005/2006", T)
getSeasonData(7, "2006/2007", T)
getSeasonData(7, "2007/2008", T)
getSeasonData(7, "2008/2009", T)
getSeasonData(7, "2009/2010", T)
getSeasonData(7, "2010/2011", T)






