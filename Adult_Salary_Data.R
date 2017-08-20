

########  Adult Salary data  #########

## Reinier Kuschke
## 18/08/2017
## 

adult <- read.csv('adult_sal.csv')

head(adult)

library(dplyr)
adult <- select(adult,-X)

#Call head, structure and summary of data

head(adult)

str(adult)

summary(adult)

#Clean data


## Employer Type
table(adult$type_employer)

unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

  
  adult$type_employer <- sapply(adult$type_employer,unemp)
  
  table(adult$type_employer)
  
  group_emp <- function(job){
    if (job=='Local-gov' | job=='State-gov'){
      return('SL-gov')
    }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
      return('self-emp')
    }else{
      return(job)
    }
  }
  
  adult$type_employer <- sapply(adult$type_employer,group_emp)
  
  table(adult$type_employer)
  
## Marital Status
  
  table(adult$marital)
  group_marital <- function(mar){
    mar <- as.character(mar)
    
    ### Not-Married
    if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
      return('Not-Married')
      
      ### Never-Married   
    }else if(mar=='Never-married'){
      return(mar)
      
      ### Married
    }else{
      return('Married')
    }
  }
  
  adult$marital <- sapply(adult$marital,group_marital)
  table(adult$marital)
  
## Country
  
  table(adult$country)
  levels(adult$country)
  
  ### Group Countries
  
  Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
            'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')
  
  North.America <- c('Canada','United-States','Puerto-Rico' )
  
  Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
              'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')
  
  Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                               'El-Salvador','Guatemala','Haiti','Honduras',
                               'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                               'Jamaica','Trinadad&Tobago')
  Other <- c('South')
  
  group_country <- function(ctry){
    if (ctry %in% Asia){
      return('Asia')
    }else if (ctry %in% North.America){
      return('North.America')
    }else if (ctry %in% Europe){
      return('Europe')
    }else if (ctry %in% Latin.and.South.America){
      return('Latin.and.South.America')
    }else{
      return('Other')      
    }
  }
  
  adult$country <- sapply(adult$country,group_country)
  
  table(adult$country)
    
  ##
  
  str(adult)
  
  adult$type_employer <- sapply(adult$type_employer,factor)
  adult$country <- sapply(adult$country,factor)
  adult$marital <- sapply(adult$marital,factor)
  
  str(adult)
  
  
  
# MISSING DATA
  
  ## Using AMELIA
  
  library(Amelia)

  
  adult[adult == '?'] <- NA
  
  
  table(adult$type_employer)
  
## Change to factors
  
  adult$type_employer <- sapply(adult$type_employer,factor)
  adult$country <- sapply(adult$country,factor)
  adult$marital <- sapply(adult$marital,factor)
  adult$occupation <- sapply(adult$occupation,factor)
  
  ## Create Missingness Map
  
  missmap(adult)
  
  ###  Clean Missingness Map
  
  missmap(adult,y.at=c(1),y.labels = c(''),col=c('red','green'))
  
  adult <- na.omit(adult)
  
  str(adult)
  
  missmap(adult,y.at=c(1),y.labels = c(''),col=c('red','green'))
  
## EXPLORITORY DATA ANAYSIS
  
  # Check structure of data frame
  
  str(adult)
  
  # Call ggplot2 and dplyr
  
  library(ggplot2)
  library(dplyr)
  
  # Histogram: Ages coloured by income
  
  ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()
  
  # Histogram: Hrs per week 
  
  ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()
  
  # Rename "Country" to "Region"(Better reflection of factor levels)
  
  names(adult)[names(adult)=="country"] <- "region"
  
  # Check Structure of dataframe 
  
  str(adult)
  
  # Barplot: Region fill colour scale by income
  
    ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ### Building Model
  
  # Check head of data frame
  
  head(adult)
  
  ##Train test split
  
  # Import caTools
  
  library(caTools)
  
  # Set RNG seed @ 123
  
  set.seed(123) 
  
  # Split up the sample
  sample <- sample.split(adult$income, SplitRatio = 0.70)
  
  # Training Data
  train = subset(adult, sample == TRUE)
  
  # Testing Data
  test = subset(adult, sample == FALSE)
  
  # LM MODEL
  
  model = glm(income ~ ., family = binomial(logit), data = train)
  
  # Check Summary
  
  summary(model)
  
  # Create new model
  
  new.step.model <- step(model)
  
  
# Check new model summary
  
  summary(new.step.model)
    
  #Create Confusion matrix
  
  test$predicted.income = predict(model, newdata=test, type="response")
  
  table(test$income, test$predicted.income > 0.5)
  
  # Check for model accuracy
  
  (6372+1423)/(6372+1423+548+872)
  
  #recall
  
  6732/(6372+548)
  
  #precision
  
  6732/(6372+872)
  