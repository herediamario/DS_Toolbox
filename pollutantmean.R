pollutantmean <- function(directory, pollutant, id = 1:332) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     
     ## 'pollutant' is a character vector of length 1 indicating
     ## the name of the pollutant for which we will calculate the
     ## mean; either "sulfate" or "nitrate".
     
     
     ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     n_mean <- numeric(length(id))
     i_count <-1
     nSum <- 0
     nCount <- 0
     for(i_filecounter in id){
          #print(i_filecounter)
          c_filename <- paste(formatC(i_filecounter, 
                                      width = 3, flag="0"), "csv", sep=".")
          c_path <- paste(directory, c_filename, sep ="/")
          #print(c_filename)
          df_data <- read.csv(c_path, header = TRUE)
          #n_mean[i_count] <- mean(df_data[[pollutant]], na.rm = TRUE)
          #print(n_mean[i_count])
          
          
          #temp <- complete.cases(df_data)
          #print(df_data[temp, ])
          df_pollutant_table <- df_data[pollutant]
          df_good_data <- df_pollutant_table[!is.na(df_pollutant_table)]
          print(length(df_good_data))
          #print(sum(df_good_data))
          #n_mean[i_count] <- mean(df_good_data, na.rm = TRUE)
          nSum<- nSum+ sum(df_good_data)
          nCount <- nCount + length(df_good_data)
          print(n_mean[i_count])
          i_count<- i_count+1
          
     }
     
     ## Return the mean of the pollutant across all monitors list
     ## in the 'id' vector (ignoring NA values)
     ## NOTE: Do not round the result!
     n_mean <- nSum/nCount
     print(paste("----"))
     mean(n_mean)
     #print(n_mean)
     
}