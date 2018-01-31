library(stringr)
library(dplyr)
library(knitr)
library(kableExtra)
library(car)

ds <- read.csv("multipleChoiceResponses.csv", stringsAsFactors = F)
cr <- read.csv("conversionRates.csv", stringsAsFactors = F)
sch <- read.csv("schema.csv", stringsAsFactors = F)

dim_ds <- dim(ds)
dim_cr <- dim(cr)
dim_sch <- dim(sch)
dimension <- data.frame(dim_ds, dim_cr, dim_sch, row.names = c("Rows", "Columns"))

# Transform blank cells into NA cells
is.na(ds) <- ds == ""

# Converting salaries
ds$CompensationAmount <- ds$CompensationAmount %>% 
  str_replace_all(",","") %>% 
  as.numeric()

# Next, we are going to join the ds table and the cr table together, using the CompensationCurrency as key. 
# At the same time, the CompensationAmount can be converted to US dollars using the exchangeRate column of the cr table, and stored as a new column named Salary. 
# This will be our response variable, in which the linear regression model is trained to predict.

cr <- rename(cr, CompensationCurrency = originCountry)
ds <- ds %>% 
  left_join(cr, by = "CompensationCurrency") %>% 
  mutate(Salary = CompensationAmount*exchangeRate) %>% 
  mutate(Salary = round(Salary))


# In the following step, we remove any data where the Salary column is NA. 
# These are the cases where the respondents did not respond to the CompensationAmount or CompensationCurrency questions, or the CompensationCurrency entered was not in the exchangeRate column.
ds <- filter(ds, !is.na(ds$Salary))

# Next we are going to remove few columns in the ds table. 
# Since we have completed the currency conversion to create the Salary column, we can remove the CompensationAmount and CompensationCurrency columns. 
# During the model selection process, the model may pick up these two variables since they are highly correlated to the Salary variable. Likewise, the exchangeRate column is removed.
remove_col <- which(names(ds) %in% c(names(cr), "CompensationAmount"))
ds <- ds[,-remove_col]

# Potential Explanatory Variables
all_shown <- c()
i <- 0
for (question in names(ds)){
  if (question %in% sch$Column){
    row <- which(sch$Column == question)
    if (sch[row, "Asked"] == "All"){
      i <- i + 1
      all_shown[i] <- question
    }

  }
}
length(all_shown)


# This function takes the name of the survey question and the scheme data, and return the row of the question
# question = a string vector containing the question names from the sch$Column
findRow <- function(question){
  row <- which(sch$Column == question)
  return(sch[row,])
}

# This function takes the name of the survey question and the survey data, and returns the levels of answer to the question.
# question = a string vector containing the question names
findLvl <- function(question){
  vec <- which(names(ds)==question)
  vec <- ds[,vec]
  temp <- levels(as.factor(vec))
  return(temp)
}

# Use findRow function to extract information from the schema table for only those questions shown to all respondent.
exp_vars <- all_shown %>% 
  sapply(findRow) %>% 
  t() %>% 
  data.frame(row.names=NULL)
# Apply findLvl function to extract from the survey data the levels of answer to each question.
all_levels <- sapply(all_shown, findLvl)
# Create a new column, counting the number of levels for each question.
exp_vars$Num.Levels <- sapply(all_levels, length)
# Create a new column, collecting all levels for the question.
exp_vars$Levels <- sapply(all_levels, paste, collapse="~")
# Remove the "Asked" redundant column.
exp_vars <- exp_vars[,-which(names(exp_vars)=="Asked")]
head(sort(exp_vars$Num.Levels, decreasing = T))

EXP_VARS <- exp_vars

# This function takes a question that has check-box type answers and return the avaiable box selections 
# question = a string vector containing the question names from the exp_vars$Column
findSelections <- function(question){
  temp <- which(EXP_VARS$Column == question) %>% 
    EXP_VARS[.,"Levels"] %>% 
    str_split("~") %>% 
    unlist() %>% 
    str_split(",") %>% 
    unlist() %>% 
    unique()
  return(temp)
}



# This function takes a question that has check-box type answer and use 
# the available box selections to create features (columns), in which each
# column contains boolean value indicating if the respondent checked the box.
# The function returns a data.frame object.
# question = a string vector containing the question names
createFeatures <- function(question){
  base_lvls <- findSelections(question)
  col <- which(names(ds)==question)
  vec <- ds[,col]
  temp <- lapply(vec, str_detect, base_lvls)
  temp <- data.frame(matrix(unlist(temp), nrow=length(temp), byrow=T))
  names(temp) <- base_lvls %>% 
    str_replace_all("[[:punct:]]| ", "") %>% 
    paste(question, ., sep="_")
  return(temp)
}

createFeaturesForEstimation <- function(question, ds){
  base_lvls <- findSelections(question)
  col <- which(names(ds)==question)
  vec <- ds[,col]
  temp <- lapply(vec, str_detect, base_lvls)
  temp <- data.frame(matrix(unlist(temp), nrow=length(temp), byrow=T))
  names(temp) <- base_lvls %>% 
    str_replace_all("[[:punct:]]| ", "") %>% 
    paste(question, ., sep="_")
  return(temp)
}


#We will now identify the check-box type survey questions, create the new columns, and use cbind to include them in the ds table.

checkbox_questions <- c("LearningPlatformSelect", "MLTechniquesSelect", 
                        "PastJobTitlesSelect", "BlogsPodcastsNewslettersSelect",
                        "MLSkillsSelect", "PublicDatasetsSelect")
new_columns <- lapply(checkbox_questions, createFeatures)
ds <- cbind(ds, new_columns)

#We also want to update the exp_vars table to include these new variables.

# Create pattern to be used as parameter for the str_extract function
pattern <- checkbox_questions %>% 
  paste("_[[:print:]]*", sep="") %>% 
  paste(collapse="|")
# Use str_extract to detect and extract the names of new columns in ds
new_features <- unlist(str_extract_all(names(ds), pattern))
# This function finds the full question given the name of the survey question of the new features
# question = a string vector containing the question names
findQsn <- function(question){
  strings <- str_extract(question, "\\w*_")
  strings <- str_replace(strings, "_", "")
  temp <- findRow(strings)
  return(temp$Question)
}
# Construct the data frame
new_features <- data.frame(Column = new_features, 
                           Question = sapply(new_features, findQsn), 
                           Num.Levels = rep(2, length(new_features)), 
                           Levels = rep("TRUE|FALSE", length(new_features)))
# Combine with the exp_vars
exp_vars <- rbind(exp_vars, new_features)


#There are two variables that we will need to remove from the exp_vars table - LearningCategory, and LearningPlatformUsefulness. 
#The answers to these questions are respondents’ opinions and will not be helpful for the model.

# Identify the questions related to "LearningCategory" and "LearningPlatformUsefulness"
lcpu_questions <- exp_vars$Column %>% 
  sapply(str_extract, "LearningCategory\\w*|LearningPlatformUsefulness\\w*") %>% 
  .[!is.na(.)]
remove_features <- c(checkbox_questions, lcpu_questions,
                     "MLToolNextYearSelect", "MLMethodNextYearSelect")
exp_vars <- exp_vars[!(exp_vars$Column %in% remove_features),]
row.names(exp_vars) <- c(1:length(exp_vars$Column))

#If the IQR*1.5 criteria is used to remove the outliers, we may be removing too much data. Here, we opt to take out 1% of data from the top and bottom of the data respectively.

one_percent <- round(0.01 * dim(ds)[1])
bottom <- max(sort(ds$Salary)[1:one_percent])
top <- min(sort(ds$Salary, decreasing = T)[1:one_percent])
ds <- filter(ds, Salary > bottom & Salary < top)
summary(ds$Salary)


# Below, two functions are created to transform a variable using Box Cox Transformation, and to convert a transformed variable back to its original form.

# Perform BoxCoxTransformation
bct <- function(vec, lambda = 0.4) return((vec^lambda-1)/lambda)

# Perform inverse of BoxCoxTransformation
invbct <- function(vec, lambda = 0.4) return((vec*lambda+1)^(1/lambda))
#The Salary variable can now be transformed.

ds$Salary <- bct(ds$Salary)
summary(ds$Salary)

# This functions returns the adjusted R square for the linear model
# question = a string vector or list containing the question names
calAdjRsqr <- function(question){
  formula <- question %>% 
    paste(collapse="+") %>% 
    paste("Salary", "~", .) %>% 
    as.formula()
  ds$Salary[is.infinite(ds$Salary)] <- NA
  fit <- lm(formula, data = ds)
  return(summary(fit)$adj.r.squared)
}

# forward selection:
# This functions calculates the adj R squares of the models resulted from adding a variable
# explain = a string vector or list containing the question names referencing the explainatory variables in the model
# add_explain = a string vector or list containing the explainatory variables to be added to the model
# pos = starting position of the search in the add_explain vector
forwardSearch <- function(explain, add_explain, pos=1){
  if (pos > length(add_explain)) return(c())
  temp <- c(calAdjRsqr(c(explain, add_explain[pos])), forwardSearch(explain, add_explain, pos+1))
  return(temp)
}

## perform forward selection: (dauert lange, rausnehmen, da nur einmal benoetigt, stattdessen var_add manuell definieren:
#var_add <- c()
#vec_arsquare <- forwardSearch(var_add, var_pool)
#max_arsquare <- max(vec_arsquare)
#new_arsquare_fs <- c()
#current_max <- 0
#while(max_arsquare > current_max){
#  current_max <- max_arsquare
#  pos <- which(vec_arsquare == max_arsquare)[1]
#  var_add <- c(var_add, var_pool[pos])
#  var_pool <- var_pool[!var_pool %in% var_add]
#  new_arsquare_fs <- c(new_arsquare_fs, max_arsquare)
#  vec_arsquare <- forwardSearch(var_add, var_pool)
#  max_arsquare <- max(vec_arsquare)
#}

var_add <- c('Country','Tenure','Age','LearningPlatformSelect_CollegeUniversity','MajorSelect','EmploymentStatus','MLTechniquesSelect_DecisionTreesRandomForests','ParentsEducation','MLSkillsSelect_NaturalLanguageProcessing','GenderSelect','PastJobTitlesSelect_SoftwareDeveloperSoftwareEngineer','PastJobTitlesSelect_Researcher','PastJobTitlesSelect_DataScientist','LearningPlatformSelect_Companyinternalcommunity','MLTechniquesSelect_LogisticRegression','LearningPlatformSelect_YouTubeVideos','LearningPlatformSelect_Blogs','BlogsPodcastsNewslettersSelect_RBloggersBlogAggregator','PastJobTitlesSelect_Engineer','BlogsPodcastsNewslettersSelect_TalkingMachinesPodcast','PastJobTitlesSelect_Programmer','MLSkillsSelect_SurvivalAnalysis','BlogsPodcastsNewslettersSelect_BecomingaDataScientistPodcast','LearningPlatformSelect_Arxiv','PastJobTitlesSelect_BusinessAnalyst','MLTechniquesSelect_EnsembleMethods','MLSkillsSelect_SpeechRecognition','PublicDatasetsSelect_UniversityNonprofitresearchgroupwebsites','PastJobTitlesSelect_Other','LearningPlatformSelect_Tutoringmentoring','LearningPlatformSelect_Onlinecourses','MLSkillsSelect_AdversarialLearning','BlogsPodcastsNewslettersSelect_SirajRavalYouTubeChannel','BlogsPodcastsNewslettersSelect_LinearDigressionsPodcast','PastJobTitlesSelect_DataMiner','LearningPlatformSelect_Tradebook','PastJobTitlesSelect_ComputerScientist','PastJobTitlesSelect_Ihaventstartedworkingyet','PastJobTitlesSelect_PredictiveModeler','MLTechniquesSelect_NeuralNetworksGANs','MLTechniquesSelect_DecisionTreesGradientBoostedMachines','LearningPlatformSelect_Conferences','MLSkillsSelect_TimeSeries','MLTechniquesSelect_BayesianTechniques','MLTechniquesSelect_HiddenMarkovModelsHMMs')

# fit model:
fit_fs <- var_add%>% 
  paste(collapse="+") %>% 
  paste("Salary", ., sep="~") %>% 
  lm(data=ds, subset=(!is.infinite(Salary)))

questions <- unique(exp_vars$Question[exp_vars$Column %in% var_add])
