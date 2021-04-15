#' An MSA Explore Function
#'
#' This function allows you to run an exploratory MSA
#' @param data .csv file with variables in columns and observations in rows
#' @keywords msa
#' @export
#' @examples
#' msa_explore()

### Exploratory MSA ###


msa_explore <- function(data){

  # Load libraries

  library(mokken)
  library(poLCA)
  library(semTools)
  library(tidyverse)

  # Prepare Data

  data <- Filter(function(x) !(all(x=="")), data)
  data <- na.omit(data)
  data_mat <- as.matrix(data)

  # Run Mokken Scaling

  scales <- as.data.frame(aisp(data_mat, search = "ga", lowerbound = .3))

  scales$var <- rownames(scales)
  scales <- scales[scales$`0.3` > 0,]

  scales <- scales %>%
    group_by(`0.3`) %>%
    filter(n() > 2) %>%
    ungroup()

  list_of_dataframes <- list()

  for(i in 1:max(scales$`0.3`)){
    list_of_dataframes[[i]] <- data[scales$var[scales$`0.3` == i]]
    names(list_of_dataframes)[i] <- as.character(i)
  }

  results <- lapply(list_of_dataframes, msa_standard)

  return(results)

}
