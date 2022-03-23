# Function for cleaning the data. Parts 2-7. E.g. if you want to clean raw_part5 you need to type clean_data(raw_part5)
clean_data <- function(df) {
  # Removes the first two rows. They contain some metadata/ labels we don't need. 
  
  df <- df[-c(1:2), ]
  
  # Rename columns
  
  df <- df %>% dplyr::rename("age" = Q14, gender = "Q15", year_born = "Q18", gender_elaborated = "Q15_3_TEXT", id = "ResponseId") %>% 
    
    # Create "response" column containing the participant's chance rating.  
    
    unite(response, nonSev, severe, sep = "") %>% 
    
    # Creates column showing the variation of the scenario the participant was shown
    
    unite(whisper, "c", "n", sep = "") %>%
    
    # Changes "severity" to "condition"
    dplyr::rename("condition" = "severity") %>% 
    
    # Recode severity and gender   
    
    mutate(condition = dplyr::recode(condition, "2" = "severe", "1" = "nonSevere"),
           gender = dplyr::recode(gender, "1" = "male", "2" = "female", "3" = "other", "4" = "prefer not to say")) %>% 
    # Selects columns  
    
    select(id, age, year_born, gender, gender_elaborated, condition ,response, whisper) 
  
  # changes variables to numeric/ character. 
  df$age <- as.numeric(df$age)
  df$year_born <- as.numeric(df$year_born)
  df$response <- as.character(df$response)
  df$whisper <- as.character(df$whisper)
  
  # Creates attention check
  
  df <- df %>% mutate(attention_check = 2022 - year_born - age,
  )  %>% ungroup() 
  
  return(df)  
}



# Returns the participants who failed attention check. 
exclusion <- function(df){
  df <- df %>% filter(attention_check < 0| attention_check > 1 ) 
  return(df)
  
}


# Excludes participants who failed the attention check.  
apply_exclusion <- function (df) {
  df <-  df %>% filter(age >= 18, age <=100) %>% filter(attention_check == 0| attention_check == 1)
  return(df)
}




# Returns the proportions of participants in each condition. 
condition_proportion <- function(df) {
  condition_proportion <- df %>% group_by(condition) %>% summarise(n = n())  %>% ungroup() %>% print()
  condition_proportion <- condition_proportion %>% mutate(qualtrics_number = max(condition_proportion$n)-n)
  return(condition_proportion)
}



proportion <- function(df, col){
  col <-  enquo(col); 
  df <- df %>% 
    group_by(!!col, condition) %>% 
    summarise(n = n()) 
  
  nonSevere <- df %>% filter(condition == "nonSevere")
  severe <- df %>% filter(condition == "severe")
  
  nonSevere <- nonSevere %>% mutate(qualtrics_number = max(nonSevere$n)-n,
                                    final_dummy_number = max(nonSevere$n),
                                    severity = "(1)"
  ) %>% ungroup()
  
  severe <- severe %>% mutate(qualtrics_number = max(severe$n)-n,
                              final_dummy_number = max(severe$n),
                              severity = "(2)") %>% ungroup()
  
  
  
  return(list(nonSevere_1 = nonSevere, severe_2 = severe))
  
}


final_tidy <- function(df){
  df %>% mutate(response_type = ifelse((part %% 2) == 0, "verbal", "numeric")) %>% select(id, part, age, year_born, gender, gender_elaborated, condition, response_type, response, whisper, attention_check)
}


###

# Removes the first two rows. They contain some metadata/ labels we don't need. 
clean_data_p1 <- function(df){
df <- df[-c(1, 2), ]

# Rename columns

df <- df %>% dplyr::rename("age" = Q1, gender = "Q2", year_born = "Q18", gender_elaborated = "Q2_3_TEXT", id = "ResponseId") %>% 
  
  # Create "response" column containing all the answers. 
  
  unite(response, nonSevere,severe, sep = "") %>% 
  
  # Recode severity and gender   
  
  mutate(condition = dplyr::recode(severity, "2" = "severe", "1" = "nonSevere"),
         gender = dplyr::recode(gender, "1" = "male", "2" = "female", "3" = "other", "4" = "prefer not to say"),
         whisper = "000") %>%
  
  select(id, age, year_born, gender, gender_elaborated, condition ,response, whisper) %>% 
  
  
  # changes variables to numeric/ character. 
  
  mutate(age = as.numeric(age),
         year_born = as.numeric(year_born),
         response = as.character(response),
         whisper = as.character(whisper))

# creates attention check

df <-   df %>% group_by(id) %>% mutate(attention_check = 2022 - year_born - age)  %>% ungroup()
}



# Graph

graph_numeric <- function(df){

  # Summary statistics
  summary <- df %>% group_by(time, severity) %>% dplyr::summarise(mean = mean(response),
                                                                  median = median(response),
                                                                  sd = sd(response),
                                                                  se = sd(response)/ sqrt(n()),
                                                                  lowerCI = mean - (se*1.96),
                                                                  upperCI = mean + (se*1.96),
                                                                  max = max(response),
                                                                  min = min(response)) %>% tibble()
  
  # Graph creation
  
  pd <- position_dodge(0.1) # move them .05 to the left and right
  
  ggplot(summary, aes(x = time, 
                      y = median,
                      group = severity, 
                      colour = as.factor(severity))) +
    geom_line(stat= "identity") +
    labs(color = "Severity") +
    xlab("Time") +
    ylab("Numerical interpretation of\nthe verbal probability expression (%)") +
    geom_errorbar(aes(ymin=min, ymax=max, colour=condition),
                  width = 0.2,colour="black",  position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    scale_color_manual(labels = c("non-severe", "severe"),
                       values = c("blue", "red"))+
    theme(legend.position = c(.9,.1))
}


graph_verbal <- function(df){
  # Graph
  # Summary statistics
  summary <- df %>% na.omit() %>% group_by(time, severity) %>% dplyr::summarise(mean = mean(response),
                                                                                sd = sd(response),
                                                                                se = sd(response)/ sqrt(n()),
                                                                                lowerCI = mean - (se*1.96),
                                                                                upperCI = mean + (se*1.96)) %>% tibble()
  
  # Graph creation
  
  pd <- position_dodge(0.1) # move them .05 to the left and right
  
  ggplot(summary, aes(x = time, 
                      y = mean,
                      group = severity, 
                      colour = as.factor(severity))) +
    geom_line(stat= "identity") +
    labs(color = "Severity") +
    xlab("Time") +
    ylab("Verbal interpretation of\nthe numeric probability expression") +
    geom_errorbar(aes(ymin=lowerCI, ymax=upperCI, colour=condition),
                  width = 0.2,colour="black",  position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
    scale_y_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) +
    scale_color_manual(labels = c("non-severe", "severe"),
                       values = c("blue", "red"))+
    theme(legend.position = c(.9,.1))
}


tidy_combined_df <- function(df){
  df <- df %>%  mutate(part = as.factor(part), condition = as.factor(condition)) %>% rename("severity" = "condition", "time" = "part")
  return(df)
}



verbal_tidy <- function(df){
  df %>% na.omit() %>% filter(response_type == "verbal") %>% mutate(response = dplyr::recode(response, "Very Low" = 1, "Low" = 2, "Medium" = 3, "High" = 4)) 
}


