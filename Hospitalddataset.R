##Hist of the Outcome file
plotinfo <- function (){
  df1<- read.csv("C:/Users/Usuario/Desktop/Coursera/R Programming/TPS/Programming Assignment 3/outcome-of-care-measures.csv",colClasses = "character",sep = ",")
  df1[,11]<- as.numeric(df1[,11])
  hist(df1[,11])
}

best<- function (state,outcome){
  
  #Read the file
  dataoutcome<- read.csv("C:/Users/Usuario/Desktop/Coursera/R Programming/TPS/Programming Assignment 3/outcome-of-care-measures.csv", sep = ",")
  
  #Corroborate if the information delivered by the user its correct!!
  #1) First check State
  state_flag<- FALSE;
  states <- levels(dataoutcome[, 7])[dataoutcome[, 7]]
  for(i in 1:length(states)){
    if(states[i] == state){
      state_flag<- TRUE
    }
  }
  if(state_flag == FALSE){
    stop("The State is not correct")
  }

  #2) Check "outcome"
  if (!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia"))){
    stop("The information delivered for outcome is not correct")
  }
  
  # set the column with the outcome
  if (outcome == "heart attack"){
    columna<-11
  }else if (outcome == "heart failure"){
    columna<-17
  }else{
    columna<-23
  }
  
  #Select the specific data
  dataoutcome[,columna]<-suppressWarnings(as.numeric(levels(dataoutcome[,columna])[dataoutcome[,columna]]))
  dataoutcome[,2]<- as.character(dataoutcome[,2])
  statedata <- dataoutcome[grep(state,dataoutcome$State),]
  orderdata <- statedata[order(statedata[,columna],statedata[,2], na.last = NA),]
  
  #Return hospital name in that state with the given rank 30-day death rate
  return(orderdata[1,2])
  
}

rankhospital <- function(state,outcome,num = "best"){
  #Read the file
  dataoutcome<- read.csv("C:/Users/Usuario/Desktop/Coursera/R Programming/TPS/Programming Assignment 3/outcome-of-care-measures.csv", sep = ",")
  
  #Corroborate if the information delivered by the user its correct!!
  #1) First check State
  state_flag<- FALSE;
  states <- levels(dataoutcome[, 7])[dataoutcome[, 7]]
  for(i in 1:length(states)){
    if(states[i] == state){
      state_flag<- TRUE
    }
  }
  if(state_flag == FALSE){
    stop("The State is not correct")
  }
  
  #2) Check "outcome"
  if (!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia"))){
    stop("The information delivered for outcome is not correct")
  }
  
  # set the column with the outcome
  if (outcome == "heart attack"){
    columna<-11
  }else if (outcome == "heart failure"){
    columna<-17
  }else{
    columna<-23
  }
  
  #Select the specific data
  dataoutcome[,columna]<-suppressWarnings(as.numeric(levels(dataoutcome[,columna])[dataoutcome[,columna]]))
  dataoutcome[,2]<- as.character(dataoutcome[,2])
  statedata <- dataoutcome[grep(state,dataoutcome$State),]
  orderdata <- statedata[order(statedata[,columna],statedata[,2], na.last = NA),]
  
  #Present the information recarding the info into "num" variable.
  if (num == "best"){
    return(orderdata[1,2])
  }else if (num== "worst"){
    return(orderdata[nrow(orderdata),2])
  }else{
    return(orderdata[num,2])
  }
}

rankall<- function(outcome,num= "best"){
  
  #Read outcome data
  dataoutcome <- read.csv("C:/Users/Usuario/Desktop/Coursera/R Programming/TPS/Programming Assignment 3/outcome-of-care-measures.csv")
  
  #Check outcome data
  if (!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia"))){
    stop("The information delivered for outcome is not correct")
  }
  
  # set the column with the outcome
  if (outcome == "heart attack"){
    columna<-11
  }else if (outcome == "heart failure"){
    columna<-17
  }else{
    columna<-23
  }
  
  dataoutcome[,columna]<-suppressWarnings(as.numeric(levels(dataoutcome[,columna])[dataoutcome[,columna]]))
  dataoutcome[,2]<- as.character(dataoutcome[,2])
  
  #Create a vector to output the information!!
  output <- vector()
  states <- levels(dataoutcome[,7])
  
  for (i in 1:length(states)) {
    statedata <- dataoutcome[grep(states[i],dataoutcome$State),]
    orderdata <- statedata[order(statedata[,columna],statedata[,2], na.last = NA),]
    if (num == "best"){
      hospital <- orderdata[1,2]
    }else if (num== "worst"){
      hospital <- orderdata[nrow(orderdata),2]
    }else{
      hospital <- orderdata[num,2]
    }
    ## add elements to the output vector
    output <- append(output,c(hospital,states[i]))
  }
  #Return a data frame with the info of the hospitals names and the state names
  output <- as.data.frame(matrix(output,length(states),2,byrow = TRUE))
  colnames(output)<- c("Hospital","State")
  return(output)
}