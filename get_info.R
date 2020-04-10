
# 
# Patriot League Schools
# 
# American University
# Boston University
# Bucknell University
# Colgate University
# College of Holy Cross 
# Laffayette College 
# Lehigh University
# Loyola University - Maryland
# 
# 
# Desired Stats
# 
# median SAT
# median ACT
# 25th percentile SAT
# 75th percential SAT
# 25th percentile ACT
# 75th percentile ACT
# Total Applicants
# Total Admitted
# Total Enrollment
# Acceptance Rate



################################################################################################
# Patriot League Schools college data urls


# American University
# Boston University
# Bucknell University
# Colgate University
# College of the Holy Cross 
# Laffayette College 
# Lehigh University
# Loyola University Maryland


#https://www.collegedata.com/college/Lehigh-University/?tab=profile-admission-tab

################################################################################################

install.packages("rvest")
install.packages("stringr")
install.packages("tidyverse")

library(rvest)
library(stringr)
library(tidyverse)


################################################################################################

url_p1 <- "https:////www.collegedata.com//college//"

pat_league_schools <- c("American University",
                        "Boston University",
                        "Bucknell University",
                        "Colgate University",
                        "College of the Holy Cross",
                        "Laffayette College", 
                        "Lehigh University",
                        "Loyola University Maryland")

url_p2 <- "//?tab=profile-admission-tab"


make_urls <- function(p1,school_list,p2){
      
      school_list_new <- str_replace_all(school_list, " ", "-")
      url_list <- list()
      num_schools <- length(school_list)
      
      for (i in 1:num_schools){
            
            url_list[i] <- paste0(url_p1, school_list_new[i], url_p2)
      }
      
      
      return(url_list)
}

url_list <- make_urls(url_p1, pat_league_schools, url_p2)







