#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("/Users/matthewgentile/Documents/GitHub/colgate_admissions_visualizations/create_dataframe_functions.R")
library(shiny)
library(ggplot2)
library(ggalt)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
      
      
      observeEvent(input$make_list,{
            current_schools <- input$colleges
            
           
            
            new_school <- input$text_college
            
            if (new_school != ""){
                  list_url <- "https://www.collegedata.com/en/prepare-and-apply/common-application-guide/"
                  list_html <- read_html(list_url)
                  common_app_schools <- list_html %>%
                        html_nodes(".t-title__details") %>%
                        html_text() %>%
                        str_remove_all("\r\n") %>%
                        trimws()
                  
                  inputted_schools <- unlist(strsplit(new_school, ", "))
                                             
                  if(check_school(inputted_schools, common_app_schools)){

                        current_schools <- c(current_schools, inputted_schools)
                        
                  } else{
                        
                        showNotification("Please make sure to enter the proper school name with correct capitlization (e.g. Williams College, Duke University)",
                                         type = "error")
                        
                  }
            }
               
               
            
            
            
            
            
            #print(current_schools)
            
            
            current_schools <- unlist(strsplit(current_schools, ", "))
            
            
            
            
            
            
            observeEvent(input$create_plot, {
                  y <- input$metric
                  y <- as.numeric(y)
                  
                  ############
                  #print(current_schools)
                  
                  url_p1 <- "https://www.collegedata.com/college/"
                  url_p2 <- "/?tab=profile-admission-tab"
                  
                  school_list <- current_schools   
                  
                  url_list <- make_urls(url_p1,school_list,url_p2)
                  num_schools <- length(school_list)
                  
                  info_df_list <- list()
                  for (i in 1:num_schools){
                        #print(i)
                        school_tibble <- build_info_char(url_list[[i]]) %>%
                              build_info_df(school_list[[i]]) 
                        
                        info_df_list[[i]] <- school_tibble
                        
                        
                  }
                  
                  `%notin%` <- Negate(`%in%`)
                  
                  if (y %notin% c(12,13,15,16,18,19)){
                        
                        
                        plot_vals <- c()
                        for (i in 1:num_schools){
                              plot_vals[i] <- info_df_list[[i]][[y]]
                        }
                        
                        
                        
                        plot_df <- tibble(school = school_list, val = plot_vals)
                        
                        
                        output$mainPlot <- renderPlot({
                              
                              ggplot(data=plot_df, aes(x=school, y=val)) +
                                    geom_bar(stat="identity", fill="steelblue")+
                                    geom_text(aes(label=val), vjust=1.6, color="white", size=3.5)+
                                    theme_minimal()+
                                    ggtitle("College Comparison")+
                                    theme(plot.title = element_text(size = 30, face = "bold"))+
                                    xlab("School Name") +
                                    ylab("Comparison Metric Value")
                              
                        })
                  }
                  
                  else{
                        plot_valsH <- c()
                        plot_valsL <- c()
                        
                        for (i in 1:num_schools){
                              
                              
                              
                              plot_valsH[i] <- info_df_list[[i]][[y]]
                              plot_valsL[i] <- info_df_list[[i]][[y+1]]
                        }
                        
                        
                        plot_df <- tibble(school = school_list, valH = plot_valsH, valL = plot_valsL)
                        
                        output$mainPlot <- renderPlot({     
                              ggplot(data = plot_df, aes(y = school, x = valH, xend = valL, size = 5)) +
                                    geom_dumbbell(color="steel blue", 
                                                  size_x=10, 
                                                  size_xend = 10)+
                                    geom_text(color="white", size=4.5,
                                              aes(x=valL, label=valL))+
                                    geom_text(aes(x=valH, label=valH), 
                                              color="white", size=4.5)+
                                    coord_flip() +
                                    theme_minimal()+
                                    theme(plot.title = element_text(size = 30, face = "bold"))+
                                    ggtitle("College Comparison") +
                                    ylab("School Name") +
                                    xlab("Comparison Metric Value")
                        })
                        
                  }
                  
                  
            })
            
            
      })
      
})
