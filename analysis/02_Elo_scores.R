###################################################################################################
#' Survey main dataset and Elo scores computation
#'
#' This script compute the Elo scores 
#'
#' Produces :
#'          - matches_all.csv
#'          - elo_scores.csv
#'          - Poster_24.jpg
#'          - Poster_all.jpg
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/05/16 first created, major update 2024/03/05
##################################################################################################

rm(list=ls())

#Import the data from the sql ddb & save ----
#no need to run again the ddb_user and ddb_password are in a txt file in the R folder but not 
#available on github 

  source(here::here("R","ddb_codes.R"))

  mysqlconnection = RMySQL::dbConnect(RMySQL::MySQL(),
                              dbname='tfy1nmntqcxcnebn',
                              host='l3855uft9zao23e2.cbetxkdyhwsb.us-east-1.rds.amazonaws.com',
                              port=3306,
                              user=ddb_user,
                              password=ddb_password)
  sql_query_en <- "SELECT id, judge_id, challenger_1, challenger_2, winner, start_time, end_time, pool_number, survey_id
                FROM tfy1nmntqcxcnebn.answer
                WHERE survey_id = 15"
  sql_query_fr <- "SELECT id, judge_id, challenger_1, challenger_2, winner, start_time, end_time, pool_number, survey_id
                FROM tfy1nmntqcxcnebn.answer
                WHERE survey_id = 18"
  
  raw_matches_en <-DBI::dbGetQuery(mysqlconnection, sql_query_en)
  raw_matches_fr <-DBI::dbGetQuery(mysqlconnection, sql_query_fr)

  write.csv2(raw_matches_en,here::here("data", "BIG_FILES","survey","raw_matches_en.csv"))
  write.csv2(raw_matches_fr,here::here("data", "BIG_FILES","survey","raw_matches_fr.csv"))
  
#----

#Compute Elo scores (run only once)----
#Compute the Elo scores with only with the respondent who answered the profile survey 
#and with no color perception issues
#XXXX respondents answered the survey; after removing the color blind and the the respondent who 
#did not answered the profile survey we ended up with XXXX respondents
  
  data_respondents <- read.csv2(here::here("data","data_respondents.csv"))
  
  #get the raw_matches from English and French survey and bind 
  
  raw_matches_en <- read.csv2(here::here("data", "BIG_FILES","survey","raw_matches_en.csv"))
  raw_matches_fr <- read.csv2(here::here("data", "BIG_FILES","survey","raw_matches_fr.csv"))

  raw_matches <- rbind(raw_matches_en,raw_matches_fr)
  
  #remove the respondent who did not answered the google form 
    sum(!unique(raw_matches$judge_id)%in%data_respondents$ID_Judge)
    #305 respondent made the first section of the survey (image matches) 
    #but did not answered the survey (they need to be removed from the elo computation)
    raw_matches <- raw_matches[raw_matches$judge_id%in%data_respondents$ID_Judge,]
  
  #remove the color blind
    raw_matches <- raw_matches[raw_matches$judge_id%in%data_respondents$ID_Judge[data_respondents$Color_blind%in%"No"],]
    
  #prepare the winner columns
    raw_matches$challenger_1 <- as.character(raw_matches$challenger_1)
    raw_matches$challenger_2 <- as.character(raw_matches$challenger_2)
    raw_matches$winner <- as.character(raw_matches$winner)
    names(raw_matches)[names(raw_matches) == 'winner'] <- 'Winner'

  ##create the full matches data file (2xmatches)

    matches_1 <- raw_matches
    matches_2 <- raw_matches
    names(matches_2)[names(matches_2) == 'challenger_1'] <- 'temp'
    names(matches_2)[names(matches_2) == 'challenger_2'] <- 'challenger_1'
    names(matches_2)[names(matches_2) == 'temp'] <- 'challenger_2'
    matches_all <- rbind(matches_1,matches_2)
    matches_all$outcome <- as.numeric(matches_all$challenger_1==matches_all$Winner)
    
    ### will be also used afterward to test the effect of the respondent profils 
      write.csv2(matches_all,here::here("data","matches_all.csv"),row.names = F)
    
  ##Compute Elo scores

    eloruns=1000
    mc.cores=parallel::detectCores()

    matches_all$Loser <- NA
    matches_all$Loser[matches_all$outcome==1]=matches_all$challenger_2[matches_all$outcome==1]
    matches_all$Loser[matches_all$outcome==0]=matches_all$challenger_1[matches_all$outcome==0]
      
    #calculate the mean number of matches per images (645)
      
      images_list <- unique(matches_all$challenger_1)
      
      match_dat <- do.call(rbind,pbmcapply::pbmclapply(images_list, function(id){
        data.frame(images=id,nb_matche=dim(matches_all[matches_all$challenger_1==id,])[1])
      },mc.cores = mc.cores))
      
      meanmatchs <- round(mean(match_dat$nb_matche),2) 
      
    #computing Elos
      
      res_elo    <- EloChoice::elochoice(winner = matches_all$Winner, loser = matches_all$Loser,
                                         startvalue = 1500, runs = eloruns)
      elo_scores <- cbind.data.frame(Idmages=names(EloChoice::ratings(res_elo, show="mean",drawplot = F)),
                                 mean=EloChoice::ratings(res_elo, show="mean",drawplot = F),
                                 var=EloChoice::ratings(res_elo, show="var",drawplot = T))
      rm(res_elo)
      elo_scores[,3] <- sqrt(elo_scores[,3])
      colnames(elo_scores) <- c("Id_images","Elo_mean_all","Elo_sd_all")
      
      write.csv2(elo_scores,here::here("results","elo_scores.csv"),row.names = F)
      write.csv2(elo_scores,here::here("figures_tables",paste0("table_elos_",length(unique(matches_all$judge_id)),"_respondents",".csv")),row.names = F)
      
      elo_deep_cora <- elo_scores[1:2]
      write.table(elo_deep_cora,here::here("results","elo_deep_cora.txt"),row.names = F,col.names = F,quote = FALSE,sep=",")
#----
    
#Posters ----
    
    source(here::here('R','functions.R'))
    elo_scores <- read.csv2(here::here("results","elo_scores.csv"))

    #do the poster for all images 
      order_id<-elo_scores[order(elo_scores$Elo_mean_all,decreasing = TRUE),"Id_images"]
      outputImageFileName <- here::here("figures_tables","Poster_all_Elos.jpg")
      poster(path_photo <- here::here("data","BIG_FILES","images","jpg_survey"),
             tab_image <-  paste0(order_id,".jpg"),
             nb_row <- 15,
             nb_col <- 20,
             outputImageFileName <- outputImageFileName,
             sizered=1
      )
      
    #do the poster for 24 
      order_id<-elo_scores[order(elo_scores$Elo_mean_all,decreasing = TRUE),"Id_images"]
      im_24 <- order_id[c(1:6,seq(14,255,round(254/12)),254:300)]
      outputImageFileName <- here::here("figures_tables","Poster_24_Elos.jpg")
      poster(path_photo <- here::here("data","BIG_FILES","images","jpg_survey"),
             tab_image <-  paste0(im_24,".jpg"),
             nb_row <- 4,
             nb_col <- 6,
             outputImageFileName <- outputImageFileName,
             sizered=1
      )
#----

  
  
 




