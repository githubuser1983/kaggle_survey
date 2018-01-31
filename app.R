library(shiny)
library(markdown)
library(randomForest)

# For Data Cleaning
library(tidyverse)
library(rlang)
library(stringr)
library(car)

# For text analysis and word clouds
library(tm)
library(SnowballC)
library(wordcloud)



# Import multiple choice data
rawMCData <- read.csv('multipleChoiceResponses.csv', stringsAsFactors = TRUE, header = TRUE)

# Import freeform responses
rawFFData <- read.csv('freeformResponses.csv', stringsAsFactors = FALSE, header = TRUE)

# Import the actual questions asked
schema <- read.csv('schema.csv', stringsAsFactors = FALSE, header = TRUE)

# Last, I need to import the currency conversion rates for use later. 
conversionRates <- read.csv('conversionRates.csv', header = TRUE)

cleanMCData <- rawMCData
cleanFFData <- rawFFData


#has_compensation <- cleanMCData %>%
#    filter(CompensationAmount > 0) %>% # only get salaries of > 0
#    mutate(CleanedCompensationAmount = str_replace_all(CompensationAmount,"[[:punct:]]", "")) %>%
#    mutate(CleanedCompensationAmount = as.numeric(CleanedCompensationAmount)) 

#print(summary(has_compensation))
#print(head(has_compensation))

#rf <- randomForest(CleanedCompensationAmount ~ . , data = has_compensation)

### Function for single choice questions {#function-chooseOne}
# A function to analyze questions where you choose only one answer
chooseOne <- function(question, filteredData = cleanMCData){
  
  return( filteredData %>% 
    # Remove any rows where the respondent didn't answer the question
    filter(!UQ(sym(question)) == "") %>% 
    # Group by the responses to the question
    group_by_(question) %>% 
    # Count how many respondents selected each option
    summarise(count = n()) %>% 
    # Calculate what percent of respondents selected each option
    mutate(percent = (count / sum(count)) * 100) %>% 
    # Arrange the counts in descending order
    arrange(desc(count))
   )
}

### Function for multi choice questions {#function-chooseMultiple}
# A function to analyze questions where you choose multiple answers
chooseMultiple <- function(question, filteredData = cleanMCData){

  filteredData %>% 
    # Remove any rows where the respondent didn't answer the question
    filter(!UQ(sym(question)) == "") %>%
    # Remove all columns except question
    select(question) %>% 
    # Add a column with the initial number of respondents to question
    mutate(totalCount = n()) %>% 
    # Split multiple answers apart at the comma, but ignore commas inside parentheses
    mutate(selections = strsplit(as.character(UQ(sym(question))), 
                                 '\\([^)]+,(*SKIP)(*FAIL)|,\\s*', perl = TRUE)) %>%
    # Split answers are now nested, need to unnest them
    unnest(selections) %>% 
    # Group by the selected responses to the question
    group_by(selections) %>% 
   # Count how many respondents selected each option
    summarise(totalCount = max(totalCount),
              count = n()) %>% 
    # Calculate what percent of respondents selected each option
    mutate(percent = (count / totalCount) * 100) %>% 
    # Arrange the counts in descending order
    arrange(desc(count))
  
}


source("salaryEstimation.R")


server <- function(input, output) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })

  output$summary <- renderPrint({
    summary(cars)
  })

  output$genderTable <- DT::renderDataTable({
    DT::datatable(chooseOne("GenderSelect"))
  })

  cleanMCData$Age <- as.numeric(as.character(cleanMCData$Age))

   age <- chooseOne("Age") %>% 
      # Remove values < 1 year
      filter(!Age < 1)

  output$ageAnalysisTable <- DT::renderDataTable({
    DT::datatable(age)
  })

  ageHist <- cleanMCData %>% 
  # Remove any rows where the respondent didn't answer the question
  filter(!Age == "") %>% 
  select(Age)

  output$ageAnalysisPlot <- renderPlot({ ggplot(ageHist, aes(x = Age)) + 
   geom_histogram(binwidth = 2) + 
   xlab("Age (years)") + 
   ylab("Number of Respondents")
  })


   output$ageCountryPlot <- renderPlot({ 
     ageCountryHist <- cleanMCData %>%
     filter(!Age == "") %>%
     filter( Country == reactive({input$country})() ) %>%
     select(Age)

   ggplot(ageCountryHist, aes(x = Age)) + 
   geom_histogram(binwidth = 2) + 
   xlab(paste("Age (years) (",reactive({input$country})(), ") ")) + 
   ylab("Number of Respondents")
  })

   output$ageGenderPlot <- renderPlot({ 
     ageGenderHist <- cleanMCData %>%
     filter(!Age == "") %>%
     filter( GenderSelect == reactive({input$gender})() ) %>%
     select(Age)

   ggplot(ageGenderHist, aes(x = Age)) + 
   geom_histogram(binwidth = 2) + 
   xlab(paste("Age (years) (",reactive({input$gender})(), ") ")) + 
   ylab("Number of Respondents")
  })

  residence <- chooseOne("Country")

  residenceFilter <- residence %>% 
  filter(count >= 100)

  output$countryPlot <- renderPlot( {
     ggplot(residenceFilter, aes(x = reorder(Country, -count), y = count)) + 
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust = 1)) + labs(x = "Country") + 
   ylab("Number of Respondents")
  })

 output$currentResidenceTable <- DT::renderDataTable({
    DT::datatable(residence)
  })

  
# Create a corpus
jobTitles <- Corpus(VectorSource(cleanFFData$CurrentJobTitleFreeForm))

# Convert to plain text document
jobTitles <- tm_map(jobTitles, PlainTextDocument)

# Remove numbers and punctuation, just in case
jobTitles <- tm_map(jobTitles, removeNumbers)
jobTitles <- tm_map(jobTitles, removePunctuation)

# Make all jobTitles lowercase
jobTitles <- tm_map(jobTitles, content_transformer(tolower))

# Remove non job title words
jobTitles <- tm_map(jobTitles, removeWords, c("and"))

# Generate the wordcloud
output$wordcloudPlot <- renderPlot( {
  wordcloud(jobTitles, 
          scale = c(5,0.2), 
          max.words = reactive({input$max})(), 
          min.freq = reactive({input$freq})(),
          random.order = FALSE, 
          use.r.layout = TRUE, 
          colors = brewer.pal(6, "Blues")[c(4,5,6,7,8,9)])
})

# salary estimation:
   GenderSelect <- reactive({input$GenderSelect})
   Country <- reactive({input$Country })
   Age <- reactive({input$Age })
   EmploymentStatus <- reactive({input$EmploymentStatus })
   MajorSelect <- reactive({input$MajorSelect })
   Tenure <- reactive({input$Tenure })
   ParentsEducation <- reactive({input$ParentsEducation })
   LearningPlatformSelect <- reactive({input$LearningPlatformSelect })
   MLTechniquesSelect <- reactive({input$MLTechniquesSelect })
   PastJobTitlesSelect <- reactive({input$PastJobTitlesSelect })
   BlogsPodcastsNewslettersSelect <- reactive({input$BlogsPodcastsNewslettersSelect })
   MLSkillsSelect <- reactive({input$MLSkillsSelect })
   PublicDatasetsSelect <- reactive({input$PublicDatasetsSelect })

   output$SalaryEstimation <- renderText({
   x <- data.frame( "GenderSelect" = factor(GenderSelect()),
                    "Country" = factor(Country()),
                    "Age" = Age(),
                    "EmploymentStatus" = factor(EmploymentStatus()),
		    "Tenure" = factor(Tenure()),
		    "MajorSelect" = factor(MajorSelect()),
                    "ParentsEducation" = factor(ParentsEducation())
         )
      print( x )
      d <- data.frame("LearningPlatformSelect" = ifelse( is.null(LearningPlatformSelect()), "", LearningPlatformSelect()) )
      new_columns <- createFeaturesForEstimation( "LearningPlatformSelect", d)
      x <- cbind(x, new_columns)
      d <- data.frame("LearningPlatformSelect" = ifelse( is.null(LearningPlatformSelect()), "", LearningPlatformSelect()) )
      new_columns <- createFeaturesForEstimation( "LearningPlatformSelect", d)
      x <- cbind(x, new_columns)
      d <- data.frame("MLTechniquesSelect" = ifelse( is.null(MLTechniquesSelect()), "", MLTechniquesSelect()) )
      new_columns <- createFeaturesForEstimation( "MLTechniquesSelect", d)
      x <- cbind(x, new_columns)
      d <- data.frame("PastJobTitlesSelect" = ifelse( is.null(PastJobTitlesSelect()), "", PastJobTitlesSelect()) )
      new_columns <- createFeaturesForEstimation( "PastJobTitlesSelect", d)
      x <- cbind(x, new_columns)
      d <- data.frame("BlogsPodcastsNewslettersSelect" = ifelse( is.null(BlogsPodcastsNewslettersSelect()), "", BlogsPodcastsNewslettersSelect()) )
      new_columns <- createFeaturesForEstimation( "BlogsPodcastsNewslettersSelect", d)
      x <- cbind(x, new_columns)
      d <- data.frame("MLSkillsSelect" = ifelse( is.null(MLSkillsSelect()), "", MLSkillsSelect()) )
      new_columns <- createFeaturesForEstimation( "MLSkillsSelect", d)
      x <- cbind(x, new_columns)
      d <- data.frame("PublicDatasetsSelect" = ifelse( is.null(PublicDatasetsSelect()), "", PublicDatasetsSelect()) )
      new_columns <- createFeaturesForEstimation( "PublicDatasetsSelect", d)
      x <- cbind(x, new_columns)
      print( x )
      salary <- round(invbct(predict(fit_fs,newdata=x)),0)
      salary
   })


}


ui <- navbarPage("Kaggle Data Science Survey 2017", theme="background.css",windowTitle="Statistik Beratung - Orges Leka | Kaggle Survey",
  tabPanel("Introduction",
     fluidRow(
        column(6,
          includeMarkdown("introduction.md")
        ),
        column(6,
          img(class="img-polaroid",
            src="data-science.jpg",style = "margin:10px; width:100%; height: 'auto'; opacity: 0.75;")
        )
      ) 
  ),
  navbarMenu("Demographics",
    tabPanel("Gender",
      
      fluidRow(
        column(3,
          includeMarkdown("gender.md")
        ),
        column(6,
          DT::dataTableOutput("genderTable")
        )
      )
    ),
    tabPanel("Current Residence",
       fluidRow(
        column(1,
          includeMarkdown("current_residence.md")
        ),
        column(6,
          DT::dataTableOutput("currentResidenceTable")
        ),
        column(5,
           plotOutput("countryPlot")
        )
      )
    ),
   tabPanel("Age Analysis",      
        fluidRow(
        column(1,
          includeMarkdown("age_analysis.md")
        ),
        column(6,
          DT::dataTableOutput("ageAnalysisTable")
        ),
        column(5, 
          plotOutput("ageAnalysisPlot")
        )
      )
    ),
    tabPanel("Age by Country",
         headerPanel('Age by Country'),
       sidebarPanel(
        selectInput('country', 'Choose Country', sort(unique(cleanMCData$Country)), selected="Germany")
        
     ),
     mainPanel(
    plotOutput('ageCountryPlot')
     )
    ),
  tabPanel("Age by Gender",
         headerPanel('Age by Gender'),
       sidebarPanel(
        selectInput('gender', 'Choose Gender', sort(unique(cleanMCData$GenderSelect)), selected="Male")
        
     ),
     mainPanel(
    plotOutput('ageGenderPlot')
     )
   )
 ),
  tabPanel("Job Title",
    fluidPage(
  # Application title
  titlePanel("Job Title - Word Cloud"),

  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),

    # Show Word Cloud
    mainPanel(
      plotOutput("wordcloudPlot")
    )
  )
  )
  ),
  tabPanel("Salary Estimation",

     fluidPage(
  # App title ----
  titlePanel("Data Scientist Salary Estimation"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(


      
    selectInput('GenderSelect', questions[1],all_levels$GenderSelect),
    selectInput('Country', questions[2],all_levels$Country),
    numericInput("Age", label = questions[3], value = 25),
    selectInput("EmploymentStatus", questions[4], all_levels$EmploymentStatus),
    selectInput("MajorSelect", questions[5], all_levels$MajorSelect),
    selectInput("Tenure", questions[6], all_levels$Tenure),
    selectInput("ParentsEducation", questions[7], all_levels$ParentsEducation),
    checkboxGroupInput("LearningPlatformSelect", label = questions[8],choices = findSelections("LearningPlatformSelect")[c(1,2,3,4,5,7,11,16,17,18)]),
    checkboxGroupInput("MLTechniquesSelect", label = questions[9], choices = findSelections("MLTechniquesSelect")[c(1,2,3,4,7,8,11)]),
    checkboxGroupInput("PastJobTitlesSelect", label = questions[10], choices = findSelections("PastJobTitlesSelect")[c(1,2,4,5,7,16,14,10,11,12,13)]),
    checkboxGroupInput("BlogsPodcastsNewslettersSelect", label = questions[11], choices = findSelections("BlogPodcastsNewslettersSelect"))[c(1,14,8,16,9)],
    checkboxGroupInput("MLSkillsSelect", label = questions[12], choices = findSelections("MLSkillsSelect")[c(1,4,8,10,11)]),
    checkboxGroupInput("PublicDatasetsSelect", label = questions[13], choices = findSelections("PublicDatasetsSelect")[c(6)])
   ),

    # Main panel for displaying outputs ----
    mainPanel(

     h3("Estimated Salary (USD)")

      # Output: 
      ,verbatimTextOutput("SalaryEstimation")
      
      )
    )
  )
    
  ),
  hr(),
  print(tags$a(href="http://www.orges-leka.de", "Statistik Beratung - Orges Leka"))
)
shinyApp(ui, server)
