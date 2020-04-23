

################################################################################################
################################################################################################

#url_p1 <- "https://www.collegedata.com/college/"

#url_p2 <- "/?tab=profile-admission-tab"

make_urls <- function(p1,school_list,p2){
      
      school_list_new <- str_replace_all(school_list, " ", "-")
      url_list <- list()
      num_schools <- length(school_list)
      
      for (i in 1:num_schools){
            
            url_list[i] <- paste0(url_p1, school_list_new[i], url_p2)
      }
      
      
      return(url_list)
}


################################################################################################
################################################################################################


build_info_char <- function(school_url){
      
      school_html <- read_html(school_url)
      
      school_info_char <- school_html %>%
            html_nodes(".card-body") %>%
            html_text()%>%
            #.[9] %>%
            .[4] %>%
            str_split("\n") %>%
            unlist()
            #str_split("      ")
      
      
      return(school_info_char)
}


################################################################################################
################################################################################################


build_info_df <- function(info_char, school_name){
      
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


################################################################################################
################################################################################################

# example usage: 

# create list of schools: Williams College, Amherst College, Tufts University

# make list of urls:
# url_list <- make_urls(p1, school_list, p2)


# college






