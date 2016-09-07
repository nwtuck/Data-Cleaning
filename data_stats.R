# inspects a data set and scaffolds out a framework for data cleaning and associated test methods
# provides sumary statistics of each column in the data set for easy reference
# populates a trello board to keep track of who is working on what columns
# output code framework comtaining methods stubs and glue code to clean and test the data set

# read data (csv file)
##read in data
raw_data <- read.csv(file.choose(), stringsAsFactors = FALSE)

# loop through each column 
for (thisCol in 1:length(colnames(raw_data))){
  columnData <- raw_data[, thisCol]
  columnRows <- length(columnData)
  # name of column
  writeLines(paste0('Column: ', colnames(raw_data)[thisCol]))
  
  # number of non-null rows
  nonNullRows <- sum(!is.na(columnData))
  writeLines(paste0('Non-null rows: ', nonNullRows, ', ', nonNullRows/columnRows*100, '%'))
  # number of null rowsh
  nullRows <- sum(is.na(columnData))
  writeLines(paste0('Null rows: ', nullRows, ', ', nullRows/columnRows*100, '%'))
  
  # numeric rows
  columnNumeric <- columnData[!is.na(as.numeric(columnData))]
  # number of numeric rows
  numericRows <- length(columnNumeric)
  writeLines(paste0('Numeric rows: ', numericRows, ', ', numericRows/columnRows*100, '%'))
  if (length(columnNumeric) > 0){
    # max of numeric rows
    writeLines(paste0("Max of numeric rows: ", max(columnNumeric)))
    # min of numeric rows
    writeLines(paste0("Min of numeric rows: ", min(columnNumeric)))
    # mean of numeric rows
    writeLines(paste0("Mean of numeric rows: ", mean(columnNumeric)))
  }

  # non-numeric rows
  columnNonNumeric <- columnData[is.na(as.numeric(columnData))]
  # number of non-numeric rows
  nonNumericRows <- length(columnNonNumeric)
  writeLines(paste0('Non numeric rows: ', nonNumericRows, ', ', nonNumericRows/columnRows*100, '%'))
  if (length(columnNonNumeric) > 0){
    # max length of non-numeric rows
    writeLines(paste0("Max length of non-numeric rows: ", max(nchar(columnNonNumeric))))
    # mean length of non-numeric rows
    writeLines(paste0("Mean length of non-numeric rows: ", mean(nchar(columnNonNumeric))))
  }
  
  columnFactor <- sort(table(as.factor(columnData)), decreasing = TRUE)
  # most frequent values
  writeLines('Most frequent values: ')
  mostFreqValues <- head(columnFactor, 5)
  for (i in 1:length(mostFreqValues)){
    writeLines(paste0(rownames(mostFreqValues)[i], ': ', mostFreqValues[i]))
  }

  writeLines('\n')
}




