###################################################################################################
#' Respondents profiles & effects 
#'
#'This script ; 
#'  - dl and format the raw data for the respondents
#'  - plots the respondents profiles 
#'  - test the effects of the respondents profiles 
#'
#'Produces : 
#'         - data_respondents.csv with the formatted respondents profiles 
#'         - Fig_respondents on the distribution of respondents profiles
#'         - table_S1.csv contain the results of the glmer test on the respondents profiles
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/05/16 first created, major update 2024/03/05
##################################################################################################

rm(list=ls(all=TRUE))
library(ggplot2) 
source(here::here("R",'functions.R'))

#Get the raw respondent data and compare with the raw_matches----
#No need to run this one again, it has been done already and the respondents profils 
#have been saved in data_respondents.csv

  ##get the file on google sheet (you need to login to your gmail acount to get the raw sheets)
    googlesheets4::gs4_auth(email = "YOURGMAILADRESSHERE")
    data_resp_fr <- as.data.frame(googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1yKae4t3-CKVVJFqrYhIW2bHKz7Ouh7igWqcBKaoSi_A/edit?usp=sharing',
                                                            col_types = "Dcicccccc"))
    data_resp_en <- as.data.frame(googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1SspNwBAGfOa-c-T7iOdvfXjO1AzEgWXADASoZLAX1ZU/edit?usp=sharing',
                                                            col_types = "Dcicccccc"))
    write.csv2(data_resp_fr,here::here("data","Esthe_Coraux.csv"))
    write.csv2(data_resp_en,here::here("data","Beautiful_Corals.csv"))
  
  ##clean and add column survey 
    data_resp_fr$Survey <- "FR"
    data_resp_en$Survey <- "EN"
    
    colnames(data_resp_en) <- c("Time","Gender","Age","Country","Education","Experience_diving","Knowledge_corals","Color_blind","ID_Judge","Survey")
    colnames(data_resp_fr) <- c("Time","Gender","Age","Country","Education","Experience_diving","Knowledge_corals","Color_blind","ID_Judge","Survey")
  
    ####there was a bug in the ID of the french judges need to remove "ID_JUGE" at the beginning 
      data_resp_fr$ID_Judge <- gsub("ID_JUGE","",data_resp_fr$ID_Judge)
  
  ##translate the french questionnaire and harmonize both terminologies 
    data_resp_fr$Gender[data_resp_fr$Gender%in%"Femme"] <- "Female"
    data_resp_fr$Gender[data_resp_fr$Gender%in%"Homme"] <- "Male"
    data_resp_fr$Gender[data_resp_fr$Gender%in%"Autre"] <- "Other"
    
    data_resp_en$Education[data_resp_en$Education%in%"None"] <- "Secondary"
    data_resp_en$Education[data_resp_en$Education%in%"Professional certificate"] <- "Secondary"
    data_resp_en$Education[data_resp_en$Education%in%"High school graduate"] <- "High school"
    data_resp_en$Education[data_resp_en$Education%in%"Associate's and/or Bachelor's degree"] <- "Bachelor"
    data_resp_en$Education[data_resp_en$Education%in%"Master's degree"] <- "Master"
    data_resp_en$Education[data_resp_en$Education%in%"Doctoral or Professional degree"] <- "PhD"
    
    data_resp_fr$Education[data_resp_fr$Education%in%"Aucun"] <- "Secondary"
    data_resp_fr$Education[data_resp_fr$Education%in%"Certificat d'étude"] <- "Secondary"
    data_resp_fr$Education[data_resp_fr$Education%in%"Baccalauréat, Certificat professionnel (CAP, BEP)"] <- "High school"
    data_resp_fr$Education[data_resp_fr$Education%in%"Bac+3"] <- "Bachelor"
    data_resp_fr$Education[data_resp_fr$Education%in%"Bac+5"] <- "Master"
    data_resp_fr$Education[data_resp_fr$Education%in%"Bac+8 (doctorat)"] <- "PhD"
    
    data_resp_en$Experience_diving[data_resp_en$Experience_diving%in%"Both diving and snorkeling"] <- "Diving"
    data_resp_en$Experience_diving[data_resp_en$Experience_diving%in%"Only diving"] <- "Diving"
    data_resp_en$Experience_diving[data_resp_en$Experience_diving%in%"Only snorkeling"] <- "Snorkeling"
    data_resp_en$Experience_diving[data_resp_en$Experience_diving%in%"Never"] <-"None"
    
    data_resp_fr$Experience_diving[data_resp_fr$Experience_diving%in%"Les deux"] <- "Diving"
    data_resp_fr$Experience_diving[data_resp_fr$Experience_diving%in%"Plongée bouteille"] <- "Diving"
    data_resp_fr$Experience_diving[data_resp_fr$Experience_diving%in%"Plongée masque et tuba"] <- "Snorkeling"
    data_resp_fr$Experience_diving[data_resp_fr$Experience_diving%in%"Non"] <-"None"
  
    data_resp_fr$Knowledge_corals[data_resp_fr$Knowledge_corals%in%"Aucune"] <- "None"
    data_resp_fr$Knowledge_corals[data_resp_fr$Knowledge_corals%in%"Faible"] <- "Low"
    data_resp_fr$Knowledge_corals[data_resp_fr$Knowledge_corals%in%"Moyenne"] <- "Average"
    data_resp_fr$Knowledge_corals[data_resp_fr$Knowledge_corals%in%"Bonne"] <-"Good"
    data_resp_fr$Knowledge_corals[data_resp_fr$Knowledge_corals%in%"Excellente"] <-"Excellent"
    
    data_resp_en$Knowledge_corals[data_resp_en$Knowledge_corals%in%"Poor"] <- "None"
    data_resp_en$Knowledge_corals[data_resp_en$Knowledge_corals%in%"Low"] <- "Low"
    data_resp_en$Knowledge_corals[data_resp_en$Knowledge_corals%in%"Average"] <- "Average"
    data_resp_en$Knowledge_corals[data_resp_en$Knowledge_corals%in%"Good"] <-"Good"
    data_resp_en$Knowledge_corals[data_resp_en$Knowledge_corals%in%"Excellent"] <-"Excellent"
    
    data_resp_fr$Color_blind[data_resp_fr$Color_blind%in%"Non"] <-"No"
    data_resp_fr$Color_blind[data_resp_fr$Color_blind%in%"Oui"] <-"Yes"
    
  ##Bind them both 
    
    data_respondents <- rbind(data_resp_en,data_resp_fr)
    
  ##Age class
    
    data_respondents$Age <- as.numeric(data_respondents$Age)
    
    data_respondents$Age_class <- ifelse(data_respondents$Age<=12, "0_12",
                                         ifelse(data_respondents$Age>12 & data_respondents$Age<=18, "13_18",
                                            ifelse(data_respondents$Age>18 & data_respondents$Age<=30, "19_30",
                                                ifelse(data_respondents$Age>30 & data_respondents$Age<60, "31_59","60_100"))))
    data_respondents$Age_class <- as.factor(data_respondents$Age_class)
    data_respondents$Age_class <- as.ordered(data_respondents$Age_class)
    
  ##Save 
    
    #select the variable to keep and in which order 

    var_keep <- c("Time","ID_Judge","Gender","Age","Age_class","Country","Education","Experience_diving",
                  "Knowledge_corals","Color_blind","Survey")
    
    data_respondents <- data_respondents[,var_keep]
    
    #keep only the responses < 2023-09-30
      data_respondents <- data_respondents[as.Date(data_respondents$Time)< as.Date("2023-09-30"),]
    
    #respondant_clean <- respondant_clean[!respondant_clean$gender%in%"unspecified",]
    #respondant_clean$gender[respondant_clean$gender=="female"]="Female"
    #respondant_clean$gender[respondant_clean$gender=="male"]="Male"
    #respondant_clean$gender <- factor(respondant_clean$gender, levels=c("Female", "Male"))
    
    write.csv2(data_respondents,here::here('data','data_respondents.csv'),row.names = F)

#----

#PLOT the respondent profiles  Fig_respondents -----
library(forcats)
    
respondents <- read.csv2(here::here("data","data_respondents.csv"))

textsize=4.5
axissize=15
ticks=10
hjust=-0.2
perct=0.2

  ##time serie
    # times <- as.data.frame(table(respondents$Time))
    # colnames(times) <- c("date","respondents")
    # times$date <- as.Date(times$date)
    # ts <- ggplot(data=times, aes(x = date, y = respondents)) +
    #   geom_col(aes(show.legend = FALSE),fill="#74BDD6")+
    #   theme_bw()+
    #   theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))
    # ggsave(file=here::here("figures_tables","Fig_time.tiff"), ts,width = 12, height = 8, dpi = 200, units = "cm", device='tiff')

  ##gender 
  ##https://cran.r-project.org/web/packages/treemapify/vignettes/introduction-to-treemapify.html  
    data_sub <- table(respondents$Gender)
    data_sub <- data.frame(Gender=names(data_sub),value=as.vector(data_sub))
    data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
    data_sub$per <- paste(data_sub$per,"%")
    
    data_sub$Gender <- as.factor(data_sub$Gender)
    data_sub$Gender <- factor(data_sub$Gender,levels=c("Male","Female","Other"))
    
    Gender <- ggplot(data_sub, aes(value,Gender)) +
      geom_col(aes(fill=Gender),show.legend = FALSE) + 
      theme_bw()+
      theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
      xlim(0,max(data_sub[,2])+perct*max(data_sub[,2]))+
      xlab("# individuals")+ ylab("Gender")+
      geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)
  
  ##Color_blind 
    data_sub <- table(respondents$Color_blind)
    data_sub <- data.frame(Color_blind=names(data_sub),value=as.vector(data_sub))
    data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
    data_sub$per <- paste(data_sub$per,"%")
    data_sub$Color_blind <- as.factor(data_sub$Color_blind)
    data_sub$Color_blind <- factor(data_sub$Color_blind, levels=c("Yes", "No"))
    
    Color_blind <- ggplot(data_sub, aes(value,Color_blind)) +
      geom_col(aes(fill=Color_blind),show.legend = FALSE) + 
      theme_bw()+
      theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
      xlim(0,max(data_sub[,2])+perct*max(data_sub[,2]))+
      xlab("# individuals")+ ylab("Color_blind")+
      geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

  ##Age 
    data_sub <- respondents
    data_sub$Gender <- as.factor(data_sub$Gender)
    data_sub$Gender <- factor(data_sub$Gender,levels=c("Male","Female","Other"))
    
    xmax <- max(table(cut(data_sub$Age, breaks=seq(0,100,5), right = FALSE)))+100
    
    Age <- ggplot(data_sub, aes(y=Age, color=Gender, fill=Gender)) +
      geom_histogram(alpha=0.6, binwidth = 5)+
      scale_y_continuous(breaks = seq(0,100,10), labels = seq(0,100, 10))+
      theme_bw()+
      theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
      xlim(0,xmax)+
      xlab("# individuals")+
      theme(legend.position = c(0.75, 0.75),legend.title=element_text(size=axissize), 
            legend.text=element_text(size=ticks))
    
  ##Age_class
    data_sub <- table(respondents$Age_class)
    data_sub <- data.frame(Age_class=names(data_sub),value=as.vector(data_sub))
    data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
    data_sub$per <- paste(data_sub$per,"%")
    data_sub$Age_class <- as.factor(data_sub$Age_class)
    data_sub$Age_class <- factor(data_sub$Age_class, levels=c("0_12", "13_18", "19_30","31_59","60_100"))
    
    Age_class <- ggplot(data_sub, aes(value,Age_class)) +
      geom_col(aes(fill=Age_class),show.legend = FALSE) + 
      xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
      theme_bw()+
      theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
      xlab("# individuals")+ 
      ylab("Age_class")+ 
      geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)
    
  ##Education
    data_sub <- table(respondents$Education)
    data_sub <- data.frame(Education=names(data_sub),value=as.vector(data_sub))
    data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
    data_sub$per <- paste(data_sub$per,"%")
    data_sub$Education <- as.factor(data_sub$Education)
    data_sub$Education <- factor(data_sub$Education, levels=c("Secondary", "High school", "Bachelor","Master","PhD"))
  
    Education <- ggplot(data_sub, aes(value,Education)) +
      geom_col(aes(fill=Education),show.legend = FALSE) + 
      xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
      theme_bw()+
      theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
      xlab("# individuals")+ 
      ylab("Education")+ 
      geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

  ##Experience_diving  
    data_sub <- table(respondents$Experience_diving)
    data_sub <- data.frame(Experience_diving=names(data_sub),value=as.vector(data_sub))
    data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
    data_sub$per <- paste(data_sub$per,"%")
    data_sub$Experience_diving <- as.factor(data_sub$Experience_diving)
    data_sub$Experience_diving <- factor(data_sub$Experience_diving, levels=c("None", "Snorkeling", "Diving"))
    
    Experience_diving <- ggplot(data_sub, aes(value,Experience_diving)) +
      geom_col(aes(fill=Experience_diving),show.legend = FALSE) + 
      xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
      theme_bw()+
      theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
      xlab("# individuals")+ 
      ylab("Experience_diving")+ 
      geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)
  
  ##Knowledge_corals  
  
    data_sub <- table(respondents$Knowledge_corals)
    data_sub <- data.frame(Knowledge_corals=names(data_sub),value=as.vector(data_sub))
    data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
    data_sub$per <- paste(data_sub$per,"%")
    data_sub$Knowledge_corals <- as.factor(data_sub$Knowledge_corals)
    data_sub$Knowledge_corals <- factor(data_sub$Knowledge_corals, levels=c("None", "Low", "Average","Good","Excellent"))
  
    Knowledge_corals <- ggplot(data_sub, aes(value,Knowledge_corals)) +
      geom_col(aes(fill=Knowledge_corals),show.legend = FALSE) + 
      xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
      theme_bw()+
      theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
      xlab("# individuals")+ 
      ylab("Knowledge_corals")+ 
      geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)
    
  ##Countries
    
  respondents$Country[respondents$Country%in%"French Southern Territories"] <- "TAAF"

  data_sub <- table(respondents$Country)
  data_sub <- data_sub[order(data_sub,decreasing = T)]
  data_sub <- data.frame(Country=names(data_sub),value=as.vector(data_sub))
  data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
  data_sub$per <- paste(data_sub$per,"%")
  data_sub$Country <- as.factor(data_sub$Country)
  data_sub <- data_sub[1:8,]
  data_sub$Country <- factor(data_sub$Country, levels=rev(as.character(data_sub$Country)))

  Country <- ggplot(data_sub, aes(value,Country)) +
    geom_col(aes(fill=Country),show.legend = FALSE) + 
    xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
    theme_bw()+
    theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
    xlab("# individuals")+ 
    ylab("Country")+ 
    geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)
  
    
  ##Save_the plot 
    
    library(gridExtra)
    g <- arrangeGrob(Gender,Color_blind,Age,Age_class,Education,Experience_diving,Knowledge_corals,Country, ncol=3) #generates g
    ggsave(file=here::here("figures_tables","Fig_respondents.tiff"), g,width = 30, height = 30, dpi = 200, units = "cm", device='tiff') 

#----
    
#TEST the respondent effects on matches outcomes GLMER Table S1----
#produce the Table SA 
    
    matches_all <- read.csv2(here::here("data","matches_all.csv"))
    respondents <- read.csv2(here::here("data","data_respondents.csv"))
    
    #keep only the respondent which are not color blind 
    
    respondents <- respondents[respondents$Color_blind%in%"No",]
    
    #create the country categorie others 
    
    sort(table(respondents$Country))
    
    sum(respondents$Country%in%c("Australia","United States","United Kingdom","Indonesia","France"))/nrow(respondents)
    
    respondents$Country[!respondents$Country%in%c("Australia","United States","United Kingdom","Indonesia","France")]="Others"
   
    #respondents <- read.csv2(here::here("data","respondant_clean.csv"))
    respondents <- rutils::rename_col(respondents,"ID_Judge","judge_id")

    list_var <- c("Gender","Age_class","Country","Education","Experience_diving","Knowledge_corals")
  
    respondents_sub <- respondents[,c("judge_id",list_var)]
    respondents_sub <- respondents_sub[complete.cases(respondents_sub),]
  
  ##Keep only the judges for which we have intel in data_judges
    matches_all <- matches_all[matches_all$judge_id %in% unique(respondents_sub$judge_id),]
  
  ##Merge with the data_judges and save 
  
    data_matches_judge <- merge(matches_all, respondents_sub, by = "judge_id",all.x = T,all.y=F)

  ##Variables that will be included in the test 

    data_matches_judge$Gender <- as.factor(data_matches_judge$Gender)
    data_matches_judge$Age_class <- as.factor(data_matches_judge$Age_class)
    data_matches_judge$Education <- as.factor(data_matches_judge$Education)
    data_matches_judge$Experience_diving <- as.factor(data_matches_judge$Experience_diving)
    data_matches_judge$Knowledge_corals <- as.factor(data_matches_judge$Knowledge_corals)
    data_matches_judge$Country <- as.factor(data_matches_judge$Country)
    
  #run the test
    whatvar=c("Gender","Age_class","Education","Experience_diving","Knowledge_corals","Country")
    RhpcBLASctl::blas_set_num_threads(10) # set the total number of proc used by the BLAS (within the glmer function of the lme4 package) make sure to keep some proc free :) 
    test_model <- lme4::glmer(as.formula(paste("outcome ~ ",paste(whatvar,collapse = "+"),"+ (1 | challenger_1)")), family = binomial,
                              data = data_matches_judge, na.action = na.fail)
    restest <- car::Anova(test_model)
    
    output <- data.frame(restest)
    output <- cbind.data.frame(variables=rownames(output),output)
    output$signi <- unlist(lapply(output$Pr..Chisq., signi))
    output
  #save the table 
    
    write.csv2(output,here::here("figures_tables","table_S1.csv"),row.names = F)

    #TEST the two-way interaction models
    
    # Loop over all pairs of variables to create two-way interaction models
    for (i in 1:(length(whatvar) - 1)) {
      for (j in (i + 1):length(whatvar)) {
        
        # Create interaction term
        interaction_term <- paste(whatvar[i], "*", whatvar[j])
        formula_str <- paste("outcome ~", interaction_term, "+ (1 | challenger_1)")
        
        # Fit the model
        test_model <- lme4::glmer(
          as.formula(formula_str),
          family = binomial,
          data = data_matches_judge,
          na.action = na.fail
        )
        
        # Perform Anova on the fitted model
        restest <- car::Anova(test_model)
        
        # Create a data frame for this specific interaction and add the interaction term
        output <- data.frame(restest)
        output <- cbind.data.frame(variables = rownames(output), output)
        output$interaction <- interaction_term  # Add interaction as a new column
        output$signi <- unlist(lapply(output$`Pr(>Chisq)`, signi))
        
        output_all <- rbind(output_all, output)
      }
    }
    
    # Save the output
    write.csv2(output_all, here::here("figures_tables", "table_S1_bis.csv"), row.names = FALSE)    
#----

    
    
