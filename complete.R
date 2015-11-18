complete <- function(directory, id = 1:332) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     nobs<-numeric(length(id))
     iCount <- 1
     
     for(i_filecounter in id){
          
          c_filename <- paste(formatC(i_filecounter, 
                                      width = 3, flag="0"), "csv", sep=".")
          c_path <- paste(directory, c_filename, sep ="/")
          
          df_data <- read.csv(c_path, header = TRUE)
               
          temp <- complete.cases(df_data)
          df_complete <- df_data[temp, ]
          
          nobs[iCount] <- length(df_complete[[TRUE]])
          iCount <- iCount + 1
          
          
     }
     ## Return a data frame of the form:
     ## id nobs
     ## 1  117
     ## 2  1041
     ## ...
     ## where 'id' is the monitor ID number and 'nobs' is the
     ## number of complete cases
     m_Observations <- data.frame(id, nobs)
     m_Observations
}