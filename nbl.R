# Script TODOs
# * Check the Mins for each team is a multiple of 5 (5 players on court) %% Modulus may be useful
# * Add a column to teamsummary for possessions per 40 mins to cater for OT games. table(teamsummary$Min) shows a majority are 199, 55 games under 199 mins 
# * Look at quarter by quarter stats, are some teams better starters than others. See homeqtr and awayqtr variables already created.
# * Add time and travel between games metrics
# *  Use the odds/lines/totals in teamsummary to create a punting data set that includes whether the fave/dog wins ATS, SU
#     whether the total is over or under, whether the win is 1-10 or 11+
# * start uploading this script into github as a backup

# Script Data Set Errors
#- The result field is not being calculated correctly for OT games. Game id 336790 has Sydney result as 0 when they won the game in overtime over Adelaide. Ensure this field is using the final scores and not the regulation scores.
#   Need to confirm that the location of the home and away teams are consistent in RealGM when scraping, rather than just assuming the home and away teams are always in the same location.
# - Game 336735 has no regulation scores for Sydney/NZ  
# - Minutes totals aren't a multiple of 5 (5 players ont he court)
# - Perth Wildcats have been based at Perth Arena since 2012. Challenge Stadium was the home court from 2002-2012. May be incorrect on the Real GM Box scores
# - SEM Phoenix don't have a home court listed (should be Melbourne Arena)
# - 29 games have regulation scores of 0-0, but have scores listed for the final. Only one of these games (183872) was an OT game.
# - Some data in the RealGM box scores differs from NBL. As a result, fantasy scores sometimes vary. 
#    An example is Dario Hunt having 10 rebounds v 9 rebounds in game 336729 vs Illawarra. As a result, he gets a Double Double bonus according the RealGM, but only had 9 according to NBL.

library(stringr)
library(xml2)
library(rvest)
library(tidyr)
library(dplyr)
library(googlesheets) 
library(xlsx)

  # Define file locations
  teamfile <- "/Users/Clint/Desktop/Sports Modelling/NBL/teamsummary.csv"
  playersfile <- "/Users/Clint/Desktop/Sports Modelling/NBL/players.csv"
  oddsfile <- "/Users/Clint/Desktop/Sports Modelling/NBL/odds.xlsx"
  puntfile <- "/Users/Clint/Desktop/Sports Modelling/NBL/punting.csv"
  
  # Scraping the URLs for the box scores for the listed season. The year given is the year that season's playoffs were in.
  season <- "2020"
  
  # Define the league to be scraped - "NBL Blitz" or "Australian NBL"
  league <- "Australian NBL"
  
  # returns the odds of the games to join to the team summary for each game.
  oddsURL <- "http://www.aussportsbetting.com/historical_data/nbl.xlsx"
  
  download.file(oddsURL, destfile = oddsfile, method="curl")
  odds <- read.xlsx(oddsfile, "Data", startRow = 2, header=TRUE, detectDates=TRUE, stringsAsFactors = FALSE)
  # Replace NA with N for Play Off Games
  odds$Play.Off.Game.[is.na(odds$Play.Off.Game.)] = 'N'
  odds$Over.Time.[is.na(odds$Over.Time.)] = 'N'

  homeodds <- subset(odds, select = c(`Date`, `Home.Team`, `Home.Odds.Close`, `Away.Odds.Close`, 
                                    `Home.Line.Close`, `Total.Score.Close`, `Play.Off.Game.`, `Over.Time.`))

  awayodds <- subset(odds, select = c(`Date`, `Away.Team`, `Away.Odds.Close`, `Home.Odds.Close`, 
                                    `Away.Line.Close`, `Total.Score.Close`, `Play.Off.Game.`, `Over.Time.`))

  namescols <- c("date", "team", "teamOdds", "oppOdds", "teamLine", "totalLine", "playoffGame", "ot" )
  names(awayodds) <- namescols 
  names(homeodds) <- namescols 
  homeawayodds <- rbind(homeodds, awayodds)

  # Schedule link (change the season on the end)
  #https://basketball.realgm.com/international/league/5/Australian-NBL/team/355/Perth/schedule/2019
  #Roster Link (Change the season on the end)
  #https://basketball.realgm.com/international/league/5/Australian-NBL/team/88/South-East-Melbourne/rosters/2020
  
  nblteams <- c("https://basketball.realgm.com/international/league/5/Australian-NBL/team/355/Perth",
  "https://basketball.realgm.com/international/league/5/Australian-NBL/team/555/Sydney",
  "https://basketball.realgm.com/international/league/5/Australian-NBL/team/25/Adelaide",
  "https://basketball.realgm.com/international/league/5/Australian-NBL/team/3/Cairns",
  "https://basketball.realgm.com/international/league/5/Australian-NBL/team/310/Illawarra",
  "https://basketball.realgm.com/international/league/5/Australian-NBL/team/648/Melbourne",
  "https://basketball.realgm.com/international/league/5/Australian-NBL/team/407/New-Zealand",
  "https://basketball.realgm.com/international/league/5/Australian-NBL/team/159/Brisbane",
  "https://basketball.realgm.com/international/league/5/Australian-NBL/team/88/South-East-Melbourne")
  
  # Initialise players and team summary before they are assigned below
  players <- NULL
  teamsummary <- NULL    
  
  # Column names starting with a number (such as 3PM and 3PA) get prepended with an X without the check.names=FALSE 
  # players 2012-2019.csv for stats at the start of the 2019-20 season
  # players.csv for current stats including games already this season
  players <- read.csv(playersfile, stringsAsFactors = FALSE, as.is=TRUE, check.names=FALSE)
  teamsummary <- read.csv(teamfile, stringsAsFactors = FALSE, as.is=TRUE, check.names=FALSE)
  players$Date <- as.Date(players$Date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
                         optional = FALSE)
  teamsummary$date <- as.Date(teamsummary$date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
                             optional = FALSE)
  
 for(team in 1:length(nblteams))
 {
   url <- paste(nblteams[team], "/schedule/", season, sep="")
   html <- paste(readLines(url), collapse="\n")
   
   matched <- str_match_all(html, "<a href=\"/international/boxscore(.*?)\"")
   links <- matched[[1]]
   
   boxscores <- links[,1]
   boxscores<-str_replace_all(boxscores, "<a href=\"", "https://basketball.realgm.com")
   boxscores<-str_replace_all(boxscores, "\"", "")
   
 for(i in 1:length(boxscores))
 {
  # Boxscore url
  bsurl <- boxscores[i]
  
  message (i, "/", length(boxscores), " ", bsurl)
  
  game<-read_html(bsurl, encoding = "UTF-8")
  gametables<-html_table(game, fill = TRUE)
  
  # Calculate game header data (home team, away team, date, venue and game number)
  # Use Selector Gadget to find the CSS for the parts of the page you want to scrape
  # https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/

  # Retreive the h2 headings on the page
  headings <-  html_text(html_nodes(game, 'h2'))

  # Store the hometeam and away team names from the headings
  header <- data.frame (awayteam=headings[2], hometeam=headings[3], stringsAsFactors=FALSE)
  # Add the date and convert to date format https://www.r-bloggers.com/date-formats-in-r/
  header$date <- as.Date(html_text(html_nodes(game, 'h2 + p')), "%B%d,%Y")
  header$season <- season
  header$url <- bsurl
  # Return the last component of the bsurl string. To return other parts of the url as strings
  # increase the last number to return the teams and date from the URL.
  header$gameid <- tail(strsplit(bsurl, '/')[[1]], 1)
  header$league <- html_text(html_nodes(game, 'p'))[12]
  
  # Find the rows that match the gameid
  existinggames <- which(teamsummary$gameid == header$gameid )
  gameexists <- length(existinggames) > 0
  gameodds <- FALSE
  # If the game exists, check whether we have the game odds or if the value is NA
  if (gameexists)
  {
    # If the odds value is NA, then there are not odds assigned. 
    # We add the ! to reverse the value, as this flag indicates whether the odds exist for that game.
    gameodds <- !((is.na(teamsummary[existinggames,]$teamOdds)))[1]
  }
  
  # If the game is the league and either
  # 1) Game id doesnt exist in teamsummary
  # 2) Game id does exist, but doesn't have odds values yet.
  # then proceed with loading the game. 
  # Otherwise move on to next game
  #       ((!header$gameid %in% teamsummary$gameid)) || ((header$gameid %in% teamsummary$gameid)  && gameodds) )
  
  if(header$league == league )
  {
    if(!gameexists || (gameexists && !gameodds))
    {
      if(gameexists && !gameodds)
      {
        message("Game exists, but we don't have odds assigned")
        # In this instance, check if we now have odds available. 
        # If so, update the teamsummary with the odds (potentially via deleting existing rows for this game and reloading)
        # If not, nothing to be gained by processing again, just skip.
        
        # temporarily just remove these games and reload them.
        players <- players[!(players$Gameid == header$gameid),]
        teamsummary <- teamsummary[!(teamsummary$gameid == header$gameid),]
      }
      
    # Add the venue
    venue <-  html_text(html_nodes(game, 'p:nth-child(4)') [[1]])
    # separate into Location and Venue (location inside brackets)
    # trimming white space around the sub strings
    header$venue <- str_trim(strsplit(venue, "[()]")[[1]][1], side="both")
    header$location <- str_trim(strsplit(venue, '[()]')[[1]][2], side="both")
  
    # Now add the header data to the relevant game tables.
    home <- gametables[[5]]  
    away <- gametables[[4]]
    
    # clean up boxscore activities
    # Add header data to each row, Team, opponent, venue, data, game number
    # Convert minutes into a decimal number. 
    # Seperate FG, FT and 3P attempts and made into seperate columns
    # strip the bottom two rows (team totals and percentages) and store in a seperate team table
    # along with the gametables[[1]], [[2]] and [[3]]\
    # Allow for OT games in gametables[[1]]
  
    # Populate the scores and results table
    homeqtr <- gametables[[1]][2,]
    awayqtr <- gametables[[1]][1,]
    
    awayreg <- as.numeric (awayqtr[2] + awayqtr[3] + awayqtr[4] + awayqtr[5])
    homereg <- as.numeric (homeqtr[2] + homeqtr[3] + homeqtr[4] + homeqtr[5]) 
    
    homefinal <- as.numeric(gametables[[1]][2,ncol(homeqtr)])
    awayfinal <- as.numeric(gametables[[1]][1,ncol(awayqtr)])
    
    # Margin of victory and result
    homemov <- homefinal - awayfinal
    awaymov <- awayfinal - homefinal
    
    homeresult <- if( homemov > 0) 1 else 0
    awayresult <- if( awaymov > 0) 1 else 0
    
    #store team percentages
    team_pct <- rbind(away[nrow(away),], home[nrow(home),])
    team_pct <- subset(team_pct, select = c(`FGM-A`, `3PM-A`, `FTM-A`))
    #remove % sign and make them numeric
    team_pct$`FGM-A` <- as.numeric(str_replace(team_pct$`FGM-A`, "%", ""))
    team_pct$`FTM-A` <- as.numeric(str_replace(team_pct$`FTM-A`, "%", ""))
    team_pct$`3PM-A` <- as.numeric(str_replace(team_pct$`3PM-A`, "%", ""))
    
    away$FGM <-  as.numeric((str_split_fixed(away$`FGM-A`, "-", 2))[,1])
    away$FGA <-  as.numeric((str_split_fixed(away$`FGM-A`, "-", 2))[,2])
    away$`3PM` <-  as.numeric((str_split_fixed(away$`3PM-A`, "-", 2))[,1])
    away$`3PA` <-  as.numeric((str_split_fixed(away$`3PM-A`, "-", 2))[,2])
    away$`FTM` <-  as.numeric((str_split_fixed(away$`FTM-A`, "-", 2))[,1])
    away$`FTA` <-  as.numeric((str_split_fixed(away$`FTM-A`, "-", 2))[,2])
    
    home$FGM <-  as.numeric((str_split_fixed(home$`FGM-A`, "-", 2))[,1])
    home$FGA <-  as.numeric((str_split_fixed(home$`FGM-A`, "-", 2))[,2])
    home$`3PM` <-  as.numeric((str_split_fixed(home$`3PM-A`, "-", 2))[,1])
    home$`3PA` <-  as.numeric((str_split_fixed(home$`3PM-A`, "-", 2))[,2])
    home$`FTM` <-  as.numeric((str_split_fixed(home$`FTM-A`, "-", 2))[,1])
    home$`FTA` <-  as.numeric((str_split_fixed(home$`FTM-A`, "-", 2))[,2])
  
    # Copy team totals over to a separate data frame before they are removed.
    team_tot <- rbind(away[nrow(away)-1,], home[nrow(home)-1,])
    team_tot <- team_tot[-c(1,2,3,4, 6, 7, 8)]
      
    away$Gameid <- header$gameid
    away$Date <- header$date
    away$Season <- season
    away$Team <- header$awayteam
    away$Opp <- header$hometeam
    away$Venue <- header$venue
    away$Location <- header$location
    away$HA <- "Away"
    away$result <- awayresult
    away$mov <- awaymov
    
    # Convert minutes from mm:ss format into a decimal number of minutes. 
    away$Min <- sapply(strsplit(away$Min,":"),
                       function(x) {
                         x <- as.numeric(x)
                         x[1]+x[2]/60
                       }
    )
  
    home$Gameid <- header$gameid
    home$Date <- header$date
    home$Season <- season
    home$Team <- header$hometeam
    home$Opp <- header$awayteam
    home$Venue <- header$venue
    home$Location <- header$location
    home$HA <- "Home"
    home$result <- homeresult
    home$mov <- homemov
    
    # Convert minutes from mm:ss format into a decimal number of minutes. 
    home$Min <- sapply(strsplit(home$Min,":"),
           function(x) {
             x <- as.numeric(x)
             x[1]+x[2]/60
           }
    )
    
    # Remove the bottom two rows for the team totals/percentages once they have been
    # copied over to the team data set
    away <- head(away, -2)
    home <- head(home, -2)
    
    # Remove the old FGA-M, 3PM-A and FTM-A columns from before the values were split.
    home <- subset(home, select = -c(`FGM-A`, `3PM-A`, `FTM-A`))
    away <- subset(away, select = -c(`FGM-A`, `3PM-A`, `FTM-A`))
    
    # Join the home and away players together and select the relevent columns
    homeaway <- rbind(home, away)
    homeaway <- subset(homeaway, select = c(`Season`, `Date`, `Team` , `Opp` , `Venue`, `HA`, 
                                           `Player` ,`Status`,	`Pos`,	`Min` ,	`FIC` ,	`Off` ,	
                                           `Def`,	`Reb`,	`Ast`,	`PF`,	`STL`,	`TO`,	`BLK`,	
                                           `PTS`,`FGM` ,`FGA`,`3PM`,`3PA`,`FTM`,`FTA`, `Gameid`, `result`, `mov`))
  
    # number of main cateogories in double figures (to calculate double doubles and triple doubls)
    homeaway$doubles <- (homeaway$PTS > 9) + (homeaway$Reb > 9) + (homeaway$Ast >9) + (homeaway$STL > 9) + (homeaway$BLK > 9)
    
    # calculate NBL Fantasy score based off the fantasy scoring at https://fantasy.nbl.com.au/classic/#help/game-guidelines
    homeaway$fantasy <- homeaway$PTS + (homeaway$Off * 3) + (homeaway$Def * 2) + (homeaway$Ast * 3) +
                        (homeaway$STL *4) + (homeaway$BLK *4) + (homeaway$`3PM`) + (homeaway$FTM - homeaway$FTA) +
                         ceiling((homeaway$FGM - homeaway$FGA)/2) + (homeaway$TO * -2 )  + 
                        ((homeaway$doubles == 2)*10) + ((homeaway$doubles > 2)*20) + (homeaway$result * 2)
  
    # Bind this subset with the existing players subset
    players <- rbind(players, homeaway)
  
    #**************** Populate the Team Totals Table ********************
    hometeam <- data.frame(gameid=header$gameid, season=header$season, date=header$date, team=header$hometeam,
                           opponent=header$awayteam, venue=header$venue, homeaway='Home', 
                           result=homeresult, reg=homereg, oppreg=awayreg, final=homefinal, oppfinal=awayfinal, mov=(homefinal-awayfinal))
    
    awayteam <- data.frame(gameid=header$gameid, season=header$season, date=header$date, team=header$awayteam,
                           opponent=header$hometeam, venue=header$venue, homeaway='Away', 
                           result=awayresult, reg=awayreg, oppreg=homereg, final=awayfinal, oppfinal=homefinal, mov=(awayfinal-homefinal) )
    
    hometeam$Poss <- gametables[[2]][2,2]
    hometeam$OppPoss <- gametables[[2]][1,2]
    hometeam$OffRtg <- gametables[[2]][2,3]
    hometeam$DefRtg <- gametables[[2]][2,4]
  
    awayteam$Poss <- gametables[[2]][1,2]
    awayteam$OppPoss <- gametables[[2]][2,2]
    awayteam$OffRtg <- gametables[[2]][1,3]
    awayteam$DefRtg <- gametables[[2]][1,4]
      
    fourfactors <- data.frame(gametables[[3]][2,], gametables[[3]][1,])
    #drop the team names from four factors
    fourfactors <- subset(fourfactors, select = -c(1,6))
    names(fourfactors) <- c('eFG%','TO%','OR%','FTR','OppeFG%','OppTO%','OppOR%','OppFTR')
    
    awayfourfactors<- data.frame(gametables[[3]][1,], gametables[[3]][2,])
    #drop the team names from four factors
    awayfourfactors <- awayfourfactors[-c(1,6)]
    names(awayfourfactors) <- names(fourfactors) 
    
    # Add the four factors to hometeam and away team 
    hometeam <- cbind(hometeam, fourfactors)
    awayteam <- cbind(awayteam, awayfourfactors)
    
    hometeam$'FG%' <- team_pct[2,'FGM-A']
    hometeam$'OppFG%' <- team_pct[1,'FGM-A']
    hometeam$'3PT%' <- team_pct[2,'3PM-A']
    hometeam$'Opp3PT%' <- team_pct[1,'3PM-A']
    hometeam$'FT%' <- team_pct[2,'FTM-A']
    hometeam$'OppFT%' <- team_pct[1,'FTM-A']
    
    awayteam$'FG%' <- team_pct[1,'FGM-A']
    awayteam$'OppFG%' <- team_pct[2,'FGM-A']
    awayteam$'3PT%' <- team_pct[1,'3PM-A']
    awayteam$'Opp3PT%' <- team_pct[2,'3PM-A']
    awayteam$'FT%' <- team_pct[1,'FTM-A']
    awayteam$'OppFT%' <- team_pct[2,'FTM-A']
    
    cols <- colnames(team_tot)
    # Create a set of column names with Opp at the front of them
    oppcols <- paste("Opp", cols, sep="")
    
    # Combine the home and away team totals for the home team into one row
    team_tot_comb <- cbind(team_tot[2,], team_tot[1,])
    # Add the names with the Opp column names
    names(team_tot_comb) <- cbind(cols, oppcols)
    # Remove OppMins as the number of minutes will be the same for both teams.
    team_tot_comb <- subset(team_tot_comb, select = -c(OppMin))
    # Add team_total_comb columns to hometeam
    hometeam <- cbind(hometeam, team_tot_comb)
    
    # Combine the home and away team totals for the away team in one row
    team_tot_comb <- cbind(team_tot[1,], team_tot[2,])
    # Add the names with the Opp column names
    names(team_tot_comb) <- cbind(cols, oppcols)
    # Remove OppMins as the number of minutes will be the same for both teams.
    team_tot_comb <- subset(team_tot_comb, select = -c(OppMin))
    # Add team_total_comb columns to away team
    awayteam <- cbind(awayteam, team_tot_comb)
    
    awayteam <- merge(awayteam, homeawayodds,  by.x=c("date","team"), by.y=c("date","team"), all.x = TRUE)
    hometeam <- merge(hometeam, homeawayodds,  by.x=c("date","team"), by.y=c("date","team"), all.x = TRUE)
    
    teamsummary <- rbind(teamsummary, awayteam, hometeam)
    }# end of if !gameexists or gamesexists with no odds
    else{ message("skipped - game exists with odds already")} # end of else
  } # end of if header$gameid exists in teamsummary
  else{ message("skipped - not the same league: ", header$league)} # end of else
  } # end of boxscore loop
 } # end of team loop
 
  #calculations completed, cleanup working data frames
  #remove(awayteam)
  #remove(hometeam)
  #remove(team_tot_comb)
  #remove(team_pct)
  #remove(team_tot)
  #remove(fourfactors)
  #remove(awayfourfactors)
  
  #reorder the players columns
  players <- subset(players, select = c(`Season`, `Date`, `Team` , `Opp` , `Venue`, `HA`, 
                                        `Player` ,`Status`,	`Pos`,	`Min` ,	`FIC` ,	`Off` ,	
                                        `Def`,	`Reb`,	`Ast`,	`PF`,	`STL`,	`TO`,	`BLK`,	
                                        `PTS`,`FGM` ,`FGA`,`3PM`,`3PA`,`FTM`,`FTA`, `Gameid`, 
                                        `result`, `mov`, `doubles`, `fantasy`))
 
  # sort the tables with most recent games first
  players <- players[order(players$Date, players$Gameid, players$Team, players$Status, players$Min, decreasing = TRUE),]
  teamsummary <- teamsummary[order(teamsummary$date, teamsummary$gameid, teamsummary$homeaway, decreasing = TRUE),]
  
  # Add history from previous seasons and bind with current teams/players
  #histplayers <- read.csv("/Users/Clint/Desktop/Sports Modelling/NBL/players10-1.csv", stringsAsFactors = FALSE, as.is=TRUE)
  #histteams <- read.csv("/Users/Clint/Desktop/Sports Modelling/NBL/teamsummary.csv", stringsAsFactors = FALSE, as.is=TRUE)      
  #names(histplayers) <- names(players)
  #players <- rbind(players, histplayers)
  #names(histteams) <- names(teamsummary)
  #teamsummary <- rbind(histteams, teamsummary)
 
  # need to keep commented as will add the odds to rows that already contain the odds.
  #teamsummary <- merge(teamsummary,homeawayodds, by.x=c("date","team"), by.y=c("date","team"))

  # Patch to include result and mov in prior player entries ***********
  #players$doubles <- (players$PTS > 9) + (players$Reb > 9) + (players$Ast >9) + (players$STL > 9) + (players$BLK > 9)
  #players <- merge(players, teamsummary[c("gameid", "team", "result", "final", "oppfinal")], by.x=c("Gameid", "Team"), by.y=c("gameid", "team"))
  #players$mov <- players$final - players$oppfinal
  #players$final <- NULL
  #players$oppfinal <- NULL
  #players$fantasy <- (players$PTS + (players$Off * 3) + (players$Def * 2) + (players$Ast * 3) +
  #       (players$STL *4) + (players$BLK * 4) + (players$`3PM`) + (players$FTM - players$FTA) +
  #         ceiling((players$FGM - players$FGA)/2)  + (players$TO * -2 ) +
  #       ((players$doubles == 2)*10) + ((players$doubles > 2)*20) + (players$result * 2))
  # **************************************************************************************
  
  #lowmins <- subset(teamsummary, teamsummary$Min < 199)
 # teamsummary$Min
  
  #--------- Punting ----------------------
  punting <- subset(teamsummary, select = c( `gameid`, `season`, `date`, `team` , `opponent` , `venue`, `homeaway`, `reg`, `oppreg`, `final`, `oppfinal`,
                                             `result`, `mov`, `teamOdds`, `oppOdds`, `teamLine`, `totalLine`, `playoffGame`, `ot`))
  
  punting$totalreg <- punting$reg + punting$oppreg 
  punting$totalfinal <- punting$final + punting$oppfinal
  #punting$ou <- if (punting$totalfinal > punting$totalLine) 'Over' else if (punting$totalfinal < punting$totalLine) {'Under'}
  
  #--------- End of Punting ---------------
  
  
 write.csv(teamsummary, teamfile, row.names=FALSE)
 write.csv(players, playersfile, row.names=FALSE)
 write.csv(punting, puntfile, row.names=FALSE)
 
 gs_upload(teamfile, "NBLTeamSummary", verbose = TRUE, overwrite = TRUE)
 gs_upload(playersfile, "NBLPlayers", verbose = TRUE, overwrite = TRUE)
 gs_upload(puntfile, "NBLPunting", verbose = TRUE, overwrite = TRUE)
 
