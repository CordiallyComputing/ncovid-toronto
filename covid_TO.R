setwd(file.path("C:", "Users", "Stanley", "Desktop", "Learning R", "Covid Project", "Data"))  #Change to folder containing Dataset.txt

#setwd()   #Change to folder containing Dataset.txt
covid_TO <- read.table("Dataset.txt", header=TRUE, sep="\t")

library(tidyverse)
library(ggthemes)

##################################################################################################################
##display()
#displays and plots cases vs column category entered
display <- function(name, num_intervals=4){
  index <- 0
  
  if(name == "New.cases"){
    print("Semantic.Error: Function prints category vs new cases daily.")
  }else if(name %in% names(covid_TO)){
      #Arranges column category from largest to smallest then indexes new cases according to ordered category
    cases_ordered <- covid_TO %>% pull(New.cases)%>% .[order(pull(covid_TO, name), decreasing=TRUE)]
    
    #Summary (assumes data is normally distributed to be significant)
    normal_summary <- summarize(covid_TO, Average = mean(covid_TO[[name]]), Standard_Dev = sd(covid_TO[[name]]))
    
    #To add column interval of length num_intervals to covid_TO dataset
    interval <- covid_TO %>% arrange(Mar) %>% pull("Mar") %>% cut(num_intervals, labels = 1:num_intervals)
    covid_TO_with_interval <- covid_TO %>% arrange(Mar) %>% mutate(interval)
    
    
    #Plots column of "name" vs New.cases
    x_plot <- covid_TO[[name]]
    cases_vs_name <- covid_TO_with_interval %>% ggplot(aes(x = x_plot, y = New.cases, color=interval)) +
      geom_point(size = 3) +
      xlab(name) + 
      ylab("New cases daily") +
      ggtitle(sprintf("Daily Cases vs %s", name)) +
      scale_y_continuous(trans = "log2") +
      scale_color_discrete(name = "Time Interval") +
      theme_economist_white()
    
    
    #Prints
    print(sprintf("Daily cases ordered by decreasing %s", name))
    print(cases_ordered)
    print("")
    print(sprintf("%s Summary (if normal)", name))
    print(normal_summary)
    print(cases_vs_name)
    
  }else{
    print("ERROR: Invalid Input. Please input a valid category!")
  }
}

##################################################################################################################
##new_table_by_cases()
#creates new table ordered by new cases daily
new_table_by_cases <- function(approach = 3){
  
  case_index = order(covid_TO$New.cases, decreasing = TRUE)
  
  if(approach == 1){#Manual Approach; only includes avg stats
    covid_TO_by_case_1 <- data.frame(Cases = covid_TO$New.cases[case_index],
                               avg.Temp = covid_TO$Temp.Avg[case_index],
                               avg.DewPoint = covid_TO$Dew.Point.Avg[case_index],
                               avg.Humidity = covid_TO$Humid.Avg[case_index],
                               Wind.Speed.Avg = covid_TO$Wind.Speed.Avg[case_index])
  }else if(approach == 2){#Simplified Approach
    index <- 1
    covid_TO_by_case_2 <- data.frame(Cases = as.vector(covid_TO$New.cases[case_index]))
  
    for(column in covid_TO){
      column_name <- names(covid_TO[index])
      
      if(column_name != "New.cases"){
        covid_TO_by_case_2[column_name] <- column[case_index]
      }
      index = index + 1
    }
  }else if(approach == 3){#Dplyr approach
    covid_TO_by_case_3 <- covid_TO %>% arrange(desc(New.cases))
  }
  
  switch(approach,covid_TO_by_case_1,covid_TO_by_case_2,covid_TO_by_case_3)
}

##################################################################################################################
##is_normal()
#plots qq plot to see data is approximately normal
is_normal <- function(category="New.cases", approach=1){
  covid_data <- array(covid_TO[[category]])
  
  avg <- mean(covid_data)
  sigma <- sd(covid_data)

  centered_data <- sweep(covid_data, 1, avg, "-")
  normalized_data <- sweep(centered_data, 1, sigma, "/")
  
  if(approach == 1){
    quantiled_normalized_data <- quantile(normalized_data, seq(0.01, 0.99, 0.01))
    theoretical_quant <- qnorm(seq(0.01, 0.99, 0.01))
  
    plot(theoretical_quant, quantiled_normalized_data, xlab="Theoretical Quantiles", ylab=sprintf("Actual Quantiles for %s", category), main="QQ Plot") 
    abline(0, 1)
  } else if (approach == 2){
    covid_TO %>%
      ggplot(aes(sample=normalized_data)) +
      geom_qq() +
      geom_abline() +
      ylab(sprintf("Normalized %s", category))
  } else{
    print("ERROR: Invalid approach argument!")
  }
}

##################################################################################################################







