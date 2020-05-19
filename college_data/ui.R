#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("/Users/matthewgentile/Documents/GitHub/colgate_admissions_visualizations/create_dataframe_functions.R")

library(shiny)
library(rvest)
library(stringr)
library(tidyverse)
library(shinythemes)

shinyUI(fluidPage(
      theme = shinytheme("superhero"),
  
  # Application title
  title = "College Admission Data Visualizations",
  
  
  plotOutput("mainPlot"),
  
  hr(),
  h2("Instructions:"),
  h4("1) Select schools you would like to include in the comparison plot. If typing in non-Patriot League schools, make sure to spell the proper school 
name correctly, use proper capitalization, and separate multiple schools with a comma."),
  h4("2) Press the Update Selected Schools! button."),
  h4("3) Select the admissions metric you would like to compare. If no data is present for a school in the plot, then that school does not report inforation for the
     selected metric"),
  h4("4) Press the Compare! button and wait for visualization to appear."),
  hr(), 
  
  fluidRow(
        column(3,
               h4("Select Comparison Metric"),
               selectInput("metric",
                           "Select Admission Metric to Compare:",
                           c(
                                 "Total Applicants" = 2,
                                 "Total Admitted" = 3,
                                 "Acceptance Rate" = 4,
                                 "Acceptance Rate - Male" = 5,
                                 "Acceptance Rate - Female" = 6,    
                                 "Total Enrolled" = 7,
                                 "% of Admitted that Enrolled" = 8,
                                 "Early Decision Accepatance Rate" = 9,
                                 "Early Decision Applicants" = 10,
                                 "Average High School GPA" =11,
                                 "SAT Math (middle 50% range)" = 12,
                                 "SAT EBRW Average" = 14,
                                 "SAT EBRW (middle 50% range)" = 15,
                                 "ACT Average" = 17,
                                 "ACT (middle 50% range)" = 18
                                 
                           )
               ),
               actionButton("create_plot", "Compare!", style='padding:12.5px; font-size:150%')
        ),
        
        column(4, offset = 1,
               checkboxGroupInput("colleges",
                                  "Select Patriot League Schools to Compare:",
                                  c(
                                        "American University" = "American University",
                                        "Boston University" = "Boston University",
                                        "Bucknell University" = "Bucknell University",
                                        "Colgate University" = "Colgate University",
                                        "College of the Holy Cross" =  "College of the Holy Cross",
                                        "Lafayette College" = "Lafayette College",
                                        "Lehigh University" =  "Lehigh University",
                                        "Loyola University Maryland" = "Loyola University Maryland"
                                  )
               ),
               textInput("text_college", "Add Common Application School(s) To Selection (separated by comma):")
               
               #actionButton("add_school", "Add School to Selection!")
              
        ),
        column(4,
               
               actionButton("make_list", "Update Selected Schools!",style='padding:22.5px; font-size:150%')
               
        )
  )
)
)
       
        
        
        
        
        
        
                
  

