corr <- function(directory, threshold = 0) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'threshold' is a numeric vector of length 1 indicating the
     ## number of completely observed observations (on all
     ## variables) required to compute the correlation between
     ## nitrate and sulfate; the default is 0
     nfiles<- 1:length(dir(directory))
     
     iCount <- 1
     nCorr <- numeric(0)
     
     for(i_filecounter in nfiles){
          
          c_filename <- paste(formatC(i_filecounter, 
                                      width = 3, flag="0"), "csv", sep=".")
          c_path <- paste(directory, c_filename, sep ="/")
          
          df_data <- read.csv(c_path, header = TRUE)
          
          temp <- complete.cases(df_data)
          
          df_good_data <- df_data[temp, ]
          nobs <- length(df_good_data[[TRUE]])
          #print(nobs)
          if(nobs > threshold) {
               df_sulfate <- df_good_data["sulfate"]
               df_nitrate <- df_good_data["nitrate"]
               #print(names(df_nitrate))
               nCorr[iCount] <- cor(df_sulfate, df_nitrate)
               #print(nCorr[iCount])
               iCount <- iCount + 1
               
               
          }
               
          
          #
          #print(nCorr)
     }
     ## Return a numeric vector of correlations
     ## NOTE: Do not round the result!
     nCorr
}