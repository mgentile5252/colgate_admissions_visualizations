
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

url_p1 <- "https://www.collegedata.com/college/"

pat_league_schools <- c("American University",
                        "Boston University",
                        "Bucknell University",
                        "Colgate University",
                        "College of the Holy Cross",
                        "Lafayette College", 
                        "Lehigh University",
                        "Loyola University Maryland")

url_p2 <- "/?tab=profile-admission-tab"


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



# USE RVEST TO SCRAPE ADMISSIONS INFO FROM URLS

# # do for lehigh first then build functions around it 
# 
# 
# lehigh_url <- unlist(url_list[7])
# lehigh_html <- read_html(lehigh_url)
# lehigh_info <- lehigh_html %>%
#       html_nodes(".card-body") %>%
#       html_text()%>%
#       .[9] %>%
#       str_split("\n")
# 
# lehigh_info <- unlist(lehigh_info)
# lehigh_info <- str_split(lehigh_info, "      ")






build_info_char <- function(school_url){
      
      school_html <- read_html(school_url)
      
      school_info_char <- school_html %>%
            html_nodes(".card-body") %>%
            html_text()%>%
            .[9] %>%
            str_split("\n") %>%
            unlist() %>%
            str_split("      ")
      
      
      return(school_info_char)
}


build_info_df <- function(school_name, info_char){
      
      
      apps_char <- unlist(str_split(info_char[[3]], " "))[3]
      num_apps <- as.numeric(gsub("[^0-9.-]", "", apps_char))
      
      admitted_char <- unlist(str_split(info_char[[9]], " "))[4]
      num_admitted <- as.numeric(gsub("[^0-9.-]", "", admitted_char))
      
      accept_per_char <- unlist(str_split(info_char[[3]], " "))[1]
      accept_per <- as.numeric(gsub("[^0-9.-]", "", accept_per_char))
      
      accept_per_male_char <- unlist(str_split(info_char[[7]], " "))[1]
      accept_per_male <- as.numeric(gsub("[^0-9.-]", "", accept_per_male_char))
      
      accept_per_female_char <- unlist(str_split(info_char[[5]], " "))[1]
      accept_per_female <- as.numeric(gsub("[^0-9.-]", "", accept_per_female_char))
      
      enrolled_char <- unlist(str_split(info_char[[9]], " "))[1]
      num_enrolled <- as.numeric(gsub("[^0-9.-]", "", enrolled_char))
      
      enrolled_per_char <- unlist(str_split(info_char[[9]], " "))[2]
      enrolled_per <-  as.numeric(gsub("[^0-9.-]", "", enrolled_per_char))
      
      early_accept_per_char <- unlist(str_split(info_char[[15]], " "))[1]
      early_accept_per <-  as.numeric(gsub("[^0-9.-]", "", early_accept_per_char))
      
      early_apps_char <- unlist(str_split(info_char[[15]], " "))[3]
      early_apps <-  as.numeric(gsub("[^0-9.-]", "", early_apps_char))
      
      avg_gpa_char <- unlist(str_split(info_char[[26]], " "))[1]
      avg_gpa <- ifelse(avg_gpa_char == "Not", NA, as.numeric(gsub("[^0-9.-]", "", avg_gpa_char)))
      
      math_sat_range_char <- unlist(str_split(info_char[[41]], " "))[1]
      math_sat_range_list <- unlist(str_split(math_sat_range_char, "-"))
      math_sat_high <- math_sat_range_list[2]
      math_sat_low <- math_sat_range_list[1]
      
      ebrw_sat_avg_char <- unlist(str_split(info_char[[55]], " "))[1]
      ebrw_sat_avg <- as.numeric(gsub("[^0-9.-]", "", ebrw_sat_avg_char))
      
      ebrw_sat_range_char <- unlist(str_split(info_char[[55]], " "))[3]
      ebrw_sat_range_list <- unlist(str_split(ebrw_sat_range_char, "-"))
      ebrw_sat_high <- ebrw_sat_range_list[2]
      ebrw_sat_low <- ebrw_sat_range_list[1]
      
      
      act_avg_char <- unlist(str_split(info_char[[70]], " "))[1]
      act_avg <- as.numeric(gsub("[^0-9.-]", "", act_avg_char))
      
      act_range_char <- unlist(str_split(info_char[[70]], " "))[3]
      act_range_list <- unlist(str_split(act_range_char, "-"))
      act_high <- act_range_list[2]
      act_low <- act_range_list[1]
      
      
      info_df <- tibble(
            school = school_name,
            total_apps = num_apps,
            total_admitted = num_admitted,
            acceptance_rate = accept_per,
            male_acceptance_rate = accept_per_male,
            female_acceptance_rate = accept_per_female,
            total_enrolled = num_enrolled,
            Percentage_of_accepted_students_that_enrolled = enrolled_per,
            ED_acceptance_rate = early_accept_per,
            ED_applicants = early_apps,
            Avg_High_School_GPA = avg_gpa,
            SAT_math_range_high = math_sat_high,
            SAT_math_range_low = math_sat_low,
            SAT_ebrw_avg = ebrw_sat_avg,
            SAT_ebrw_range_high = ebrw_sat_high,
            SAT_ebrw_range_low = ebrw_sat_low,
            ACT_avg = act_avg,
            ACT_range_high = act_high,
            ACT_range_low = act_low
            
      )
      
      
      
      return(info_df)
}



# rows for df
# total applicants X
# total admitted X
# acceptance rate X
# male acceptance rate X
# female acceptance rate X
# total enrolled X
# % of admitted that enrolled X
# early decision acceptance rate X
# early decision applicants X
# average GPA of enrolled X
# SAT math range of middle 50% X
# SAT EBRW average X
# SAT EBRW range of middle 50% X
# ACT average X
# ACT range of middle 50% X







################################################################################################
# lafayette_url <- unlist(url_list[6])
# 
# lafayette_html <- read_html(lafayette_url)
# 
# 
# lafayette_info <- lafayette_html %>%
#       html_nodes(".card-body") %>%
#       html_text() %>%
#       .[9]%>%
#       str_split("\n")
# 
# lafayette_info <-  unlist(lafayette_info)
# lafayette_info <- str_split(lafayette_info, "      ")
# 
# lehigh_info <- lehigh_html %>%
#       html_nodes(".card-body") %>%
#       html_text()%>%
#       .[9] %>%
#       str_split("\n")
# 
# lehigh_info <- unlist(lehigh_info)
# 
# lehigh_info <- str_split(lehigh_info, "      ")
# 



# test with american univeristy


american_url <- unlist(url_list[1])
american_info_char <- build_info_char(american_url)
american_info_df <- build_info_df(american_info_char)

# build rest of data frames

boston_url <- unlist(url_list[2])
boston_info_char <- build_info_char(boston_url)
boston_info_df <- build_info_df(boston_info_char)



bucknell_url <- unlist(url_list[3])
bucknell_info_char <- build_info_char(bucknell_url)
bucknell_info_df <- build_info_df(bucknell_info_char)




colgate_url <- unlist(url_list[4])
colgate_info_char <- build_info_char(colgate_url)
colgate_info_df <- build_info_df(colgate_info_char)




holycross_url <- unlist(url_list[5])
holycross_info_char <- build_info_char(holycross_url)
holycross_info_df <- build_info_df(holycross_info_char)




lafayette_url <- unlist(url_list[6])
lafayette_info_char <- build_info_char(lafayette_url)
lafayette_info_df <- build_info_df(lafayette_info_char)



lehigh_url <- unlist(url_list[7])
lehigh_info_char <- build_info_char(lehigh_url)
lehigh_info_df <- build_info_df(lehigh_info_char)



lehigh_url <- unlist(url_list[7])
lehigh_info_char <- build_info_char(lehigh_url)
lehigh_info_df <- build_info_df(lehigh_info_char)



loyola_url <- unlist(url_list[7])
loyola_info_char <- build_info_char(loyola_url)
loyola_info_df <- build_info_df(loyola_info_char)




prac_char <- build_info_char(prac_list[[1]])
prac_df <- build_info_df(prac_char,pat_league_schools[1])



