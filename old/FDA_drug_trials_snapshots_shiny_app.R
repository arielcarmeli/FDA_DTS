# Shiny App to Explore FDA Drug Trials Snapshots 2015-2019 Data Set
# Author: Ariel Carmeli (ariel_carmeli@hms.harvard.edu)

library(shiny)
library(tidyverse)
library(lubridate)
library(dplyr)

# install.packages("scales")
library(scales) # for problem 1.2a. Need to make sure scales is installed!

fda_approvals <- read.csv('FDA_Drug_Trials_Snapshots_2015-19.csv')

# Change class type of select variables to aid in data processing and visualization
fda_approvals$Enrollment <- as.numeric(as.character(fda_approvals$Enrollment))
fda_approvals$Therapeutic_Area <- as.character(fda_approvals$Therapeutic_Area)
fda_approvals$Brand_Name <- as.character(fda_approvals$Brand_Name)
fda_approvals$United_States <- as.numeric(as.character(fda_approvals$United_States))

# Add a column to group trial size
fda_approvals <- fda_approvals %>% 
  mutate( Enrollment_bucket = case_when(
    Enrollment < 100 ~ "a. 1-99 patients", 
    Enrollment >= 100 & Enrollment < 200 ~ "b. 100-199 patients", 
    Enrollment >= 200 & Enrollment < 300 ~ "c. 200-299 patients",
    Enrollment >= 300 & Enrollment < 500 ~ "d. 300-499 patients",
    Enrollment >= 500 & Enrollment < 1000 ~ "e. 500-999 patients",
    Enrollment >= 1000 & Enrollment < 2000 ~ "f. 1000-1999 patients",
    Enrollment >= 2000  ~ "g. Over 2000 patients"
  ), .after = Enrollment)

# Add a column for non-hispanic 
fda_approvals <- fda_approvals %>% mutate(Non_Hispanic = 100 - Hispanic, .after = Hispanic)

# Create longer version, for plotting
fda_approvals_long <- pivot_longer(fda_approvals, cols = Women:Age_80_or_older, names_to = "Demographic", values_to = "Percentage")

fda_approvals_long <- fda_approvals_long %>% pivot_longer(cols = Sex_comparison:Age_comparison, names_to = "Comparison", values_to = "Compared")

# Change class type of variable in long
fda_approvals_long$Percentage <- as.numeric(as.character(fda_approvals_long$Percentage))

ui <- fluidPage(
    
    titlePanel("FDA Drug Trials Snapshots - Data Explorer"),

    tabsetPanel( # Problem 2.1 - add tab set to the window
        tabPanel("All 2015-2019 FDA Approvals", # Problem 2.1 - add Product Details tab
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        "race",
                        "Race",
                        #choices=str_to_title(unique(fda_approvals_long$Demographic)), 
                        choices= c("Asian", "Black", "White", "Other"),
                        selected = c("Asian", "Black", "White", "Other"),
                        multiple=T
                    ),
                    
                    selectInput(
                        "ethnicity",
                        "Ethnicity",
                        #choices=str_to_title(unique(fda_approvals_long$Demographic)), 
                        choices= c("Hispanic", "Non_Hispanic"),
                        #selected = c("Hispanic", "Non_Hispanic"),
                        multiple=T
                    ),
                    
                    sliderInput(
                      "participation",
                      "Filter participation by",
                      min=0,
                      max=100,
                      value=c(0, 100)
                    ),
                    
                    radioButtons( 
                        "is_TA_Stratified", 
                        "Stratify by Therapeutic Area?", 
                        choiceNames=list( "Yes", "No" ), 
                        choiceValues=list(TRUE,FALSE),
                        selected=FALSE
                    ),
                    
                    selectInput(
                        "year",
                        "Year(s)",
                        #choices=unique(fda_approvals$Approval_Year),
                        choices = c(2015, 2016, 2017, 2018, 2019),
                        selected = c(2015, 2016, 2017, 2018, 2019),
                        multiple=T
                    ),
                    
                    radioButtons( 
                      "is_Year_labelled", 
                      "Label by Year?", 
                      choiceNames=list( "Yes", "No" ), 
                      choiceValues=list(TRUE,FALSE),
                      selected=FALSE
                    )#,
                    
                    , width = 3
                    
                    #textOutput("summaryText")
                ),
        
                mainPanel(
                    
                   h2("How consistent is diversity in FDA approvals?"),
                   plotOutput("individualPlot"),
                   
                   h2("Count (Density) of trial participation"),
                   plotOutput("participationCountPlot"),
                   
                   h2("Approval Details"),
                   DT::dataTableOutput("approvalsTable"),
                )
            )
        ), 
        tabPanel("By Therapeutic Area & Disease",
             sidebarLayout(
                 sidebarPanel(
                    
                    selectInput(
                        "race_TA_page",
                        "Race",
                        #choices=str_to_title(unique(fda_approvals_long$Demographic)), 
                        choices= c("Asian", "Black", "White", "Other"),
                        selected = c("Asian", "Black", "White", "Other"),
                        multiple=T
                    ), 
                    
                    selectInput(
                        "ethnicity_TA_page",
                        "Ethnicity",
                        #choices=str_to_title(unique(fda_approvals_long$Demographic)), 
                        choices= c("Hispanic", "Non_Hispanic"),
                        #selected = c("Hispanic", "Non_Hispanic"),
                        multiple=T
                    ),
                   
                    selectInput(
                         "therapeutic_area",
                         "Therapeutic Area",
                         choices=sort(unique(fda_approvals$Therapeutic_Area)),
                         selected = "Oncology",
                         multiple=FALSE
                     ),

                     radioButtons( 
                       "is_Disease_Stratified", 
                       "Stratify by Disease?", 
                       choiceNames=list( "Yes", "No" ), 
                       choiceValues=list(TRUE,FALSE),
                       selected=FALSE
                     ),
                     
                 ), 
                 mainPanel(
                     h2("How consistent is diversity in FDA approvals in ", input$therapeutic_area, "?"),
                     plotOutput("TA_individualPlot"),
                     
                     h2("Approval Details"),
                     DT::dataTableOutput("TA_approvalsTable"),
                 )
            )
        )
    )
)

server <- function(input, output) {
    
    print( "Loading data ..." )
    
    # lazy loading, first access will be slow
    #data <- neiss_2008_2018
    fda_approvals <- read.csv('FDA_Drug_Trials_Snapshots_2015-19.csv')

    print( "Data loaded!" )
    print( dim(fda_approvals) )

    print( "Processing data ..." )

    # Change class type of select variables to aid in data processing and visualization
    fda_approvals$Enrollment <- as.numeric(as.character(fda_approvals$Enrollment))
    fda_approvals$Therapeutic_Area <- as.character(fda_approvals$Therapeutic_Area)
    fda_approvals$Brand_Name <- as.character(fda_approvals$Brand_Name)
    fda_approvals$United_States <- as.numeric(as.character(fda_approvals$United_States))
    
    # Add a column to group trial size
    fda_approvals <- fda_approvals %>% 
      mutate( Enrollment_bucket = case_when(
        Enrollment < 100 ~ "a. 1-99 patients", 
        Enrollment >= 100 & Enrollment < 200 ~ "b. 100-199 patients", 
        Enrollment >= 200 & Enrollment < 300 ~ "c. 200-299 patients",
        Enrollment >= 300 & Enrollment < 500 ~ "d. 300-499 patients",
        Enrollment >= 500 & Enrollment < 1000 ~ "e. 500-999 patients",
        Enrollment >= 1000 & Enrollment < 2000 ~ "f. 1000-1999 patients",
        Enrollment >= 2000  ~ "g. Over 2000 patients"
      ), .after = Enrollment)
    
    # Add a column for non-hispanic 
    fda_approvals <- fda_approvals %>% mutate(Non_Hispanic = 100 - Hispanic, .after = Hispanic)
    
    # Create longer version, for plotting
    fda_approvals_long <- pivot_longer(fda_approvals, cols = Women:Age_80_or_older, names_to = "Demographic", values_to = "Percentage")
    
    fda_approvals_long <- fda_approvals_long %>% pivot_longer(cols = Sex_comparison:Age_comparison, names_to = "Comparison", values_to = "Compared")
    
    # Change class type of variable in long
    fda_approvals_long$Percentage <- as.numeric(as.character(fda_approvals_long$Percentage))
    
    # add columns to the tibble
    #data <- data %>% 
    #    mutate(Week = as.factor(week(ymd(Treatment_Date)))) %>%
    #    mutate(Weekday = as.factor(weekdays(ymd(Treatment_Date)))) 

    print( "Data processed!" )
    #print( dim(data) )
    
    # reactive expression to filter selected approvals
    approvals <- reactive({
        selection <- fda_approvals_long %>%
            select(Brand_Name, Therapeutic_Area, Indication, Enrollment, Demographic, Percentage, Approval_Year, Enrollment_bucket) %>%
            filter(Percentage != "NA") %>% 
            unique()
            #filter(Product_1 == input$product) %>%
            #filter(stringr::str_detect(Narrative_1, toupper(input$narrative_filter)))

        if ( !is.null( input$race ) | !is.null( input$ethnicity ) ) {
          # Problem 1.3.1 - wrap "Body_Part" by stringr function str_to_title to match the input format
          # Problem 1.3.2 - change "Body_Part in filter to `Body Part` to account for updated column name
          #selection <- selection %>% filter( str_to_title(`Body Part`) %in% input$demographic )
          selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity )
        }
        
        if ( !is.null( input$year ) ) {
          # Problem 1.3.1 - wrap "Body_Part" by stringr function str_to_title to match the input format
          # Problem 1.3.2 - change "Body_Part in filter to `Body Part` to account for updated column name
          #selection <- selection %>% filter( str_to_title(`Body Part`) %in% input$demographic )
          selection <- selection %>% filter( Approval_Year %in% input$year )
        }
        
        # SOLUTION GROUP EXERCISE
        #selection <- selection %>%
        #    filter(Age >= input$age[1] ) %>%
        #    filter(Age <= input$age[2] )
        
        # Problem 1.3.2 - update these 2 column name strings to replace "_" with a space (" ")
        #colnames(selection)[which(names(selection) == "Age_Group")] <- "Age Group"
        #colnames(selection)[which(names(selection) == "Body_Part")] <- "Body Part"
        
        # SOLUTION GROUP EXERCISE
        # only apply body part filter if user made a selection
        # do not filter if no body part was selected
        #if ( !is.null( input$bodypart ) ) {
            # Problem 1.3.1 - wrap "Body_Part" by stringr function str_to_title to match the input format
            # Problem 1.3.2 - change "Body_Part in filter to `Body Part` to account for updated column name
        #    selection <- selection %>% filter( str_to_title(`Body Part`) %in% input$bodypart ) 
        #}
        
        # Problem 1.1 - add filter if year(s) were selected. do nothing if no year was selected
        #if ( !is.null( input$year ) ) {
        #    selection <- selection %>% filter( Year %in% input$year )
        #}
        
        # Problem 1.2.c to order the weekdays in order. I do this by specifying the levels of the Weekday values
        #if ( input$group_by == "Weekday" ){
            
        #    selection <- selection %>% mutate(Weekday = factor(Weekday, 
        #                                             levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
        #                                                        "Thursday", "Friday", "Saturday"),
        #                                             ordered = TRUE))
        #}
        
        selection
    })
    
    # reactive expression for the count chart
    approvals_count <- reactive({
      selection <- fda_approvals_long %>% 
        select(-Comparison) %>% 
        select(-Compared) %>% 
        filter(Percentage != "NA")

      if ( !is.null( input$race ) | !is.null( input$ethnicity ) ) {
        # Problem 1.3.1 - wrap "Body_Part" by stringr function str_to_title to match the input format
        # Problem 1.3.2 - change "Body_Part in filter to `Body Part` to account for updated column name
        #selection <- selection %>% filter( str_to_title(`Body Part`) %in% input$demographic )
        selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity )
      }
      
      if ( !is.null( input$year ) ) {
        # Problem 1.3.1 - wrap "Body_Part" by stringr function str_to_title to match the input format
        # Problem 1.3.2 - change "Body_Part in filter to `Body Part` to account for updated column name
        #selection <- selection %>% filter( str_to_title(`Body Part`) %in% input$demographic )
        selection <- selection %>% filter( Approval_Year %in% input$year )
      }
      
      #selection$Approval_Year <- as.character(selection$Approval_Year)
      
      selection <- selection %>% 
        group_by(Brand_Name, Demographic, Percentage, Approval_Year) %>% 
        unique() %>% 
        summarize(Count = n()) %>% 
        group_by(Demographic, Percentage, Approval_Year) %>% 
        summarize(Count = sum(Count))
      
      selection
    })

    
    # Reactive expression to filter selected approvals on the TA/disease tab 
    approvals_TA <- reactive({
      selection <- fda_approvals_long %>%
        select(Brand_Name, Therapeutic_Area, Disease, Indication, Enrollment, Demographic, Percentage, Approval_Year, Enrollment_bucket) %>%
        filter(Percentage != "NA") %>% 
        unique()

      #if ( !is.null( input$demographic_TA_page ) ) {
        # Problem 1.3.1 - wrap "Body_Part" by stringr function str_to_title to match the input format
        # Problem 1.3.2 - change "Body_Part in filter to `Body Part` to account for updated column name
        #selection <- selection %>% filter( str_to_title(`Body Part`) %in% input$demographic )
      #  selection <- selection %>% filter( Demographic %in% input$demographic_TA_page )
      #}
      
      if ( !is.null( input$race_TA_page ) | !is.null( input$ethnicity_TA_page ) ) {
        # Problem 1.3.1 - wrap "Body_Part" by stringr function str_to_title to match the input format
        # Problem 1.3.2 - change "Body_Part in filter to `Body Part` to account for updated column name
        #selection <- selection %>% filter( str_to_title(`Body Part`) %in% input$demographic )
        selection <- selection %>% filter( Demographic %in% input$race_TA_page | Demographic %in% input$ethnicity_TA_page )
      }
      
      if ( !is.null( input$therapeutic_area ) ) {
        selection <- selection %>% filter( Therapeutic_Area %in% input$therapeutic_area )
      }
      
      selection
      
    })
    
    # Problem 2.2.2.1 - Create reactive expression to filter on selected products and aggregate injuries by product and year
    #injuries_prod_year <- reactive({
    #    selection <- data %>% 
    #        filter(Product_1 %in% input$product_comparison) %>% 
    #        select(Product_1, Year) %>% 
    #        group_by(Product_1, Year) %>% 
    #        summarize(Count = n())
        
        # Problem 2.2.5.2 - Perform normalization if user requests
    #    if ( input$normalize ){
    #        selection <- selection %>% 
    #            mutate(Count = rescale(Count, c(0,1)))
    #    }
        
    #    selection
    #})
    
    # Problem 2.2.2.2 - Create reactive expression to filter on selected products and aggregate injuries by product and body_part
    #injuries_prod_bdypt <- reactive({
    #    selection <- data %>% 
    #        filter(Product_1 %in% input$product_comparison) %>% 
    #        select(Product_1, Body_Part) %>% 
    #        group_by(Product_1, Body_Part) %>% 
    #        summarize(Count = n())
        
        # Problem 2.2.5.2 - Perform normalization if user requests
    #    if ( input$normalize ){
    #        selection <- selection %>% 
    #            mutate(Count = rescale(Count, c(0,1)))
    #    }
        
    #    selection
    #})
    
    output$individualPlot <- renderPlot({
        
        plot <- approvals() %>% 
          filter(Percentage >= input$participation[1]) %>% 
          filter(Percentage <= input$participation[2]) %>% 
          ggplot(aes(Demographic, Percentage)) +
          #geom_jitter(width = 0.2, aes(colour=Demographic)) + 
          theme(legend.position = "top", legend.title = element_blank()) +
          scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
          ggtitle("Demographic participation in clinical trials for FDA approvals")
        
        if ( input$is_TA_Stratified ) {
            plot <- plot + 
              facet_wrap(~Therapeutic_Area) + 
              scale_y_continuous(breaks = round(seq(min(0), max(100), by = 25),1))
        }
        
        if ( input$is_Year_labelled ) {
          plot <- plot + geom_jitter(width = 0.2, aes(colour=Approval_Year))
        }
        else{
          plot <- plot + geom_jitter(width = 0.2, aes(colour=Demographic))
        }

          #ggplot(approvals()) +
            #xlab( "Demographic" ) +
            #xlab( input$group_by ) +
            #ylab( "Cases" ) +
            #filter(Percentage > -1) %>%
            #theme(axis.text.x = element_text(angle=45, hjust=1)) + # Problem 1.2b - rotate x-axis labels
            #scale_y_continuous(breaks=pretty_breaks()) # Problem 1.2a - ensure y-axis labels always integers
        
        #if ( input$isSexStratified ) {
        #        plot <- plot + 
        #            geom_bar(aes(x=.data[[input$group_by]], fill=as.factor(Sex) ), position="dodge") + 
        #            ggtitle( paste0( "Injuries by ", input$group_by, " and sex" ) )    
            
        #} else {
        #    plot <- plot + 
        #        geom_bar(aes(x=.data[[input$group_by]] )) +
        #        ggtitle( paste0( "Injuries by ", input$group_by ) )
        #}

        # SOLUTION GROUP EXERCISE
        #if ( input$isDiagnosisStratified ) {
        #    plot <- plot + facet_wrap(as.factor(approvals()$Diagnosis))
        #}
        
        plot
    })#, height = 800, width = 1200)

    output$participationCountPlot <- renderPlot({
      
      if ( input$is_Year_labelled ) {
        # line plot - split by year
        plot <- approvals_count() %>% 
          filter(Percentage >= input$participation[1]) %>% 
          filter(Percentage <= input$participation[2]) %>% 
          ggplot(aes(Percentage, y=Count, width=1, group = Approval_Year)) +
          geom_line(aes(colour = Approval_Year), stat = "identity") +
          xlab("Percent participation in trials for FDA approval") +
          ylab("Number of FDA approvals") +
          ggtitle("Number of FDA approvals by participation in trial") +
          facet_wrap(~Demographic, ncol=1)
        
      }
      else {
        # bar chart - for all years
        plot <- approvals_count() %>% 
          group_by(Demographic, Percentage) %>% 
          summarize(Count = sum(Count)) %>% 
          filter(Percentage >= input$participation[1]) %>% 
          filter(Percentage <= input$participation[2]) %>% 
          ggplot(aes(Percentage, y=Count, width=1)) +
          geom_bar(stat = "identity", position = 'dodge') +
          geom_text(aes(label=Count), position = position_dodge(0.9), vjust=-0.3, size=4) +
          xlab("Percent participation in trials for FDA approval") +
          ylab("Number of FDA approvals") +
          ggtitle("Number of FDA approvals by participation in trial") +
          facet_wrap(~Demographic, ncol=1)
      }
      
      plot
    })
    
    output$approvalsTable <- DT::renderDataTable(
        approvals() %>% 
          select(-Enrollment_bucket) %>% 
          pivot_wider(names_from = Demographic, values_from = Percentage), 
          options=list(pageLength=10)

    )
    
    output$TA_individualPlot <- renderPlot({
      
      plot <- approvals_TA() %>% 
        ggplot(aes(Demographic, Percentage)) + 
        geom_jitter(width = 0.2, aes(colour=Demographic)) + 
        theme(legend.position = "top", legend.title = element_blank()) +
        scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
        ggtitle("Demographic participation in clinical trials for FDA approvals")
      
      if ( input$is_Disease_Stratified ) {
        plot <- plot + 
          facet_wrap(~Disease) + 
          scale_y_continuous(breaks = round(seq(min(0), max(100), by = 10),1))
      }
      
      plot
    })
    
    output$TA_approvalsTable <- DT::renderDataTable(
      approvals_TA() %>% 
        select(-Enrollment_bucket) %>%
        pivot_wider(names_from = Demographic, values_from = Percentage), options=list(pageLength=10)
    )
    
    #output$summaryText <- renderText(
    #    paste0( "Selected: ", dim( cases() )[1], "/", dim( data )[1], " cases" )
    #)
    
    # Problem 2.2.3 - Implement line chart to compare injuries by year
    #output$injuries_by_year_plot <- renderPlot({
    #    plot <- ggplot(injuries_prod_year()) +
    #        geom_line(aes(x=Year, y=Count, linetype=Product_1, color=Product_1), linetype="solid", stat="identity") + 
    #        xlab( "Year" ) +
    #        ylab( "Cases" ) +
    #        theme(axis.text.x = element_text(angle=45, hjust=1), 
    #              legend.position = "bottom", legend.direction = "vertical") +
    #        scale_x_continuous(breaks=pretty_breaks())
        
    #    plot
        
    #})
    
    # Problem 2.2.4 - Implement line chart to compare injuries by body part
    #output$injuries_prod_bdypt <- renderPlot({
    #    plot <- ggplot(injuries_prod_bdypt()) +
    #        geom_bar(aes(x=Body_Part, y=Count, fill=Product_1), stat="identity", position="dodge") + 
    #        xlab( "Year" ) +
    #        ylab( "Cases" ) +
    #        theme(axis.text.x = element_text(angle=45, hjust=1), 
    #              legend.position = "bottom", legend.direction = "vertical")
            
        
    #    plot
        
    #})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
