library(twitteR)

# data required for setting up twitter access:
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

###################################################################################################
#CURRENT GOVERNMENT:

# files found on kildarestreet.com
senators <- read_excel("senators.xls") # list of Irish senators
TDs <- read_excel("TDs.xlsx")          # list of Irish TDs

###################################################################################################
# FEMALE SENATORS:
# No twitter acocunts: Marie-Louise O'Donnell [4], Jennifer Murnane O'Connor [10]

women_sen <- subset(senators, senators$Title == 'Ms') #extract women
women_sen <- women_sen[-c(4,10),] #remove senators without twitter acounts
women_sen[,c(2,3,6,7,8)] <- NULL  #unesecary columns
women_sen[8,3]<-'Independent'     #missing data

#twitter handles:
twitter_name <- c('conwaywalsh', 'MulherinFG','hopkins_maura', 'senatormbyrne', 'mairedev',
                  'SenLynnRuane', 'LorrCliff', 'SenJoanFreeman', 'ivanabacik', 'GraceOSllvn',
                  'gabmcfadden', 'frances_black', 'senatornoone', 'cardagh', 'aliceeire')


women_sen <- cbind(women_sen, twitter_name) # add twitter handles to data set
women_sen$twitter_name <- as.character(women_sen$twitter_name)

sen_handles <- (women_sen$twitter_name)  # list of handles
sen_handles <- as.character(sen_handles) # make handles of character type
sen_users <- lookupUsers(sen_handles)    # get twitter user information


###################################################################################################
# FEMALE TDs:
# No twitter acocunts: Catherine Byrne [4]

#TDS:
women_TDs <- subset(TDs, TDs$Title == 'Ms') #extract women
women_TDs <- women_TDs[-c(4),] #remove TDs without twitter acounts
women_TDs[,c(2,5,6,7)] <- NULL #remove unecessary columns

#twitter handles:
twitter_name_TD <- c('AnneRabbitte', 'bridsmithTD', 'carolno98273399', 'cathmartingreen', 
                     'catherinegalway', 'CathMurphyTD', 'ClareDalyTD', 'DMitchellTD', 
                     'Fiona_Kildare', 'FitzgeraldFrncs', 'HHumphreysFG', 'HMcEntee',
                     '1Hildegarde', 'ImeldaMunster', 'JanOSullivanTD', 'joanburton',
                     'JoanCollinsTD', 'josephamadigan', 'campaignforkate', 'KZapponeTD',
                     'Kathleensf1', 'lichamber','loreillysf','MarcellaCK','MurphyOMahonyTD',
                     'MariaBaileyFG','mary_butler_','MaryLouMcDonald','mitchelloconnor',
                     'MOSullivanTD','NiamhSmythTD','ReginaDo','RoisinShortall','RuthCoppingerTD')

#add twitter handles to data frame:
women_TDs <- cbind(women_TDs, twitter_name_TD) # add twitter name to data set
women_TDs$twitter_name_TD <- as.character(women_TDs$twitter_name_TD)

TD_handles <- (women_TDs$twitter_name)  # list of handles
TD_handles <- as.character(TD_handles)  # make handles of type character
TD_users <- lookupUsers(TD_handles)     # get twitter user information


###################################################################################################
#SENATORS AND TDS

#combine data for both senators and TDs
women_gov <-data.frame(name = c(women_sen$Name, women_TDs$Name), 
                       twitter_name = c(women_sen$twitter_name, women_TDs$twitter_name_TD),
                       Party = c(women_sen$PartyElected, women_TDs$PartyElected))

N <- nrow(women_gov)

#clean data:
women_gov$Party <- as.character(women_gov$Party)
women_gov[c(22,32),4] <-'Independent'
women_gov[c(17,49),4] <-'AAA-PBP'
women_gov$Party <- as.factor(women_gov$Party) #return party to a factor variable:

women_gov <- cbind(intercept = rep(1,N), women_gov) #intercept for MEclustnet function

# Role covariate:
Role <- c(rep('Senator',15), rep('TD', 34))

# Age covariate:
Age <- c(48, 46, 33, 50, 45, 33, 36, 60, 49, 56, 50, 57, 41, 35, 42,
         47, 56, 39, 45, 60, 64, 49, 41, 52, 67, 54, 31, 40, 50, 67,
         69, 56, 47, 38, 64, 36, 31, 38, 55, 48, 42, 51, 48, 58, 66,
         39, 47, 63, 50)

# Location covariate:
Location <- rep(NA,N)

Location[c(1:2,37)] <- "Mayo"
Location[3] <- "Roscommon"
Location[c(4,30)] <- "Limerick"
Location[c(5:9,12,14,17,22,23,25,31:35,38,41,43:45,48,49)] <- "Dublin"
Location[c(10,13,42)] <- "Waterford" 
Location[11] <- "Westmeath"
Location[c(15,16,20,28)] <- "Galway"
Location[c(18,39)] <- "Offaly" 
Location[c(19,26)] <- "Monaghan"
Location[c(21,24)] <- "Kildare"
Location[c(27,47)] <- "Meath"
Location[29] <- "Louth"
Location[36] <- "Kilkenny"
Location[40] <- "Cork"
Location[46] <- "Cavan"


#University Covariate:
University <- rep(NA,N)

University[c(1,3,13,16,26,37)]<-"NUI Galway"
University[c(2,15,19,25,31,35,38,39,41,45,48,49)] <- "UCD"
University[c(4,21,22,27,47)] <- "DCU"
University[c(5,23,24,29)] <- "DIT"
University[c(6,9,14,20,30,33,43)] <- "TCD"
University[c(7)] <- "UCC"
University[c(8,10)] <- "Open University"
University[c(11)] <- "Athlone IT"
University[c(12)] <- "All Halows College"
University[c(17)] <- "Tallaght IT"
University[c(18,44)] <- "Mary Immaculate College"
University[c(28)] <- "St. Pats Dublin"
University[c(32)] <- "None"
University[c(34)] <-"University of Brighton"
University[c(36)] <-"American College Dublin"
University[c(40)] <-"Cork IT"
University[c(42)] <-"Waterford IT"
University[c(46)] <-"Dún Laoghaire Institute"

women_gov <- cbind(women_gov, Role, Age, Location, University)


######## New collapsed levels for Location, Univeristy and Party #################

#Collapse levels for locations:
women_gov$Location[c(1:3,11,37,15,16,20,28,21,24,27,47,29,46)] <- "North"
women_gov$Location[c(4,10,13,42,30,18,39,19,26,36,40)] <- "South"
women_gov$Location[c(5:9,12,14,17,22,23,25,31:35,38,41,43:45,48,49)] <- "Dublin"

#collapse levels for univeristy:
women_gov$University <-"Other"
women_gov$University[c(2,15,19,25,31,35,38,39,41,45,48,49)] <- "UCD"

#collaps levels for party:
Party2 <- rep(0,N)
for(i in 1:nrow(women_gov)){
  if(women_gov$Party[i] != 'Fine Gael' & women_gov$Party[i] != 'Fianna Fáil'){
    Party2[i] <- 'Other'
  }else{
    Party2[i] <- as.character(women_gov$Party[i])
  }
}

women_gov <- cbind(women_gov,Party2)

# Add initials of each politician:

women_gov$initials <- gsub("[^A-Z]*([A-Z])[^A-Z]*", "\\1", women_gov$name)

# Add political party colours:

women_gov$colors <- NA
for(i in 1:49){
  if(women_gov$Party[i] == "AAA-PBP"){
    women_gov$colors[i] <- 'darkorange'
  }
  else if(women_gov$Party[i] == "Fianna Fáil"){
    women_gov$colors[i] <- '#66BB66'
  }
  else if(women_gov$Party[i] == "Fine Gael"){
    women_gov$colors[i] <- 'dodgerblue3'
  }
  else if(women_gov$Party[i] == "Green Party" ){
    women_gov$colors[i] <- 'chartreuse'
  }
  else if(women_gov$Party[i] == "Independent"){
    women_gov$colors[i] <- 'skyblue3'
  }
  else if(women_gov$Party[i] ==  "Labour"){
    women_gov$colors[i] <- '#B22222'
  }
  else if(women_gov$Party[i] == "Sinn Féin"){
    women_gov$colors[i] <- 'darkgreen'
  }
  else{
    women_gov$colors[i] <- 'darkviolet'
  }
}

#save female politicans covariate information:
write.table(women_gov,"women_gov.txt",sep="\t",row.names=FALSE)


################# Twitter Adjaceny Matrix ##################################

#get twitter user information for each politican:
women_users <- lookupUsers(c(sen_handles, TD_handles)) 

women_adj <- matrix(0, N, N) # N x N matrix of zeros

#NOTE: can only collect friends informations on 15 politicians every 15 minutes.
#      The indexes fo i need to be updated as 1:15, 16:31, 32:47, 47:49 each time
#      the loop is run.

for(i in 1:N){ 
  res <- women_users[[i]]$getFriends()
  df <- rbindlist(lapply(res, as.data.frame))
  friends <- as.data.frame(df$screenName)
  colnames(friends) <- 'following'
  women_adj[i,] <- ifelse(women_gov$twitter_name %in% friends$following, 1, 0)
}

### save female politicians adjacency matrix:
write.table(women_adj, file="women_adj.txt", row.names=FALSE, col.names=FALSE)




