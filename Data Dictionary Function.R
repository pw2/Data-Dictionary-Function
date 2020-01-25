
### Data Dictionary Function ###

data_dict <- function(data, print_table = "No"){
  
  # packages
  suppressPackageStartupMessages(suppressWarnings(require(tidyverse)))
  suppressPackageStartupMessages(suppressWarnings(require(psych)))
  suppressPackageStartupMessages(suppressWarnings(require(ggpubr)))
  suppressPackageStartupMessages(suppressWarnings(require(gridExtra)))
  
  # get rid of scientific notation
  options(scipen = 999)
  
  # Get variable info and NAs
  var_info <- data.frame(Variable = names(data),
                      VariableType = sapply(data, class),
                      MissingValues = sapply(data, function(y) 
                        sum(length(which(is.na(y))))),
                      row.names = NULL)
  
  # get descriptive stats
  desc_stats <- data.frame(Variable = names(data), 
                           describe(data)[c(2:5, 13, 8:10)], 
                           row.names = NULL)
  
  # Create the data dictionary
  d_dict <- merge(var_info, desc_stats, by = "Variable")
  d_dict <- d_dict %>% mutate_at(vars("mean":"range"), .fun = round, 2)

  # NA's for summary stats of variables not of class numeric or integer
   d_dict <- d_dict %>%
    mutate(mean = ifelse(VariableType == "numeric" | VariableType == "integer", mean, ""),
           sd = ifelse(VariableType == "numeric" | VariableType == "integer", sd, ""),
           median = ifelse(VariableType == "numeric" | VariableType == "integer", median, ""),
           se = ifelse(VariableType == "numeric" | VariableType == "integer", se, ""),
           min = ifelse(VariableType == "numeric" | VariableType == "integer", min, ""),
           max = ifelse(VariableType == "numeric" | VariableType == "integer", max, ""),
           range = ifelse(VariableType == "numeric" | VariableType == "integer", range, ""))
   # return the result
   if(print_table == "No"){
     
     return(d_dict)
     
   } else {
     #create table if table = TRUE
     d_dict <- ggtexttable(d_dict,
                           rows = NULL)
     
     return(d_dict)
     
   }
}



## create fake data

Names <- c("Sal", "John", "Jeff", "Karl", "Ben")
HomeTown <- c("CLE", "NYC", "CHI", "DEN", "SEA")
var1 <- rnorm(n = length(Names), mean = 10, sd = 2)
var2 <- rnorm(n = length(Names), mean = 300, sd = 150)
var3 <- rnorm(n = length(Names), mean = 1000, sd = 350)
var4 <- c(6, 7, NA, 3, NA)

df <- data.frame(Names, HomeTown, var1, var2, var3, var4)
df

# Data dictionary without table
data_dict(df, print_table = "No")

# Data dictionary with table
data_dict(df, print_table = "Yes")

### Try the data dictionary on a larger data set

library(Lahman)

# without table
data_dict(Batting, print_table = "No")

# with table
data_dict(Batting, print_table = "Yes")
