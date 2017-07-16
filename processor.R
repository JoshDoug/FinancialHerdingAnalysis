# Program to process and analyse financial data for herding

# Read in CSV Data
iss.data <- read.csv("InstitutionSecuritiesDataset.csv", header = TRUE) # This is a data frame

# Set up data

## Consolidate institutions into unique list - useful for looping through
institutions <- factor(iss.data$InstitutionID)
institutionLevels <- levels(institutions)

## Consolidate security tickers into unique list
securities <- factor(iss.data$SecurityTicker)
securityTickerLevels <- levels(securities)

## Get dates can be converted into date format if needed
dates <- factor(iss.data$ReportDate)
dateLevels <- levels(dates)
quarterDates <- dateLevels # Currently just leave as Strings
# quarterDates <- as.Date(dateLevels, "%Y%m%d") # Convert into date objects - might cause more issues actually
# dateFormat <- format(quarterDates, "%Y%b%d") # Not used, just an example of formatting a date

# Create a template data.frame to hold B and S for each security per institution - then add together after
securityTemplateBS <- data.frame(B = rep(0, times = length(quarterDates)), S = rep(0, times = length(quarterDates)), quarter = quarterDates)
securityTemplateBS <- securityTemplateBS[-1,] # Remove first row as it is unused for B and S

# This will form the main loop which will work through each security and delegate specific parts to other functions
for(i in securityTickerLevels) {
  print(i)
}

# Okay lets do a test run with UNP and then integrate the rest of them using above loop
# Grab all rows for the current security and put in a data frame subset
# Loop through Institutions, grab subset the current institution and work out B & S for each year & store in vector
# add to prior vector using R's cool vector math
# After B & S are gathered for the security can then feed into the AF formula
# After the AF formula is done can feed into herding formula

# Grab all rows from the data frame where security ticker equals UNP - similar to an SQL query
indices <- which(iss.data$SecurityTicker == "UNP")
dataSubset <- iss.data[indices, ]
dataSubset

securityInstanceIndices <- which(dataSubset$InstitutionID == "000W5D-E")
securityInstance <- dataSubset[securityInstanceIndices, ]
securityInstance

for(i in institutionLevels) {
  print(i)
  # which(iss.data$InstitutionID == i & iss.data$SecurityTicker == "UNP") # might not work well
}

securityBS <- walkSecurityInstance(securityInstance, securityTemplateBS)
securityBS

# Get B for a security of an institution
walkSecurityInstance <- function(securityInstance, securityTemplateBS) {
  securityBS <- securityTemplateBS
  trimmedInstance <- securityInstance[, 3:4] # could be trimmed earlier, not really necessary to trim at all
  print(trimmedInstance)
  
  yearCompare <- trimmedInstance[1,1] # holds the value of the prior hear to compare against, is reset at end of loop to the latest year
  firstDate <- trimmedInstance[1,"ReportDate"] # holds the first date
  
  rowIndex <- NA
  # Get first indiced of date to use in template for setting B or S
  # rowIndex is the current date +1 in the template, which may be a bit confusing
  # alternative is set 0 or not add 1 and then increment at start of loop
  if(firstDate == "20061231") {
    print("First date!") # This is the first possible date in the dataset, perhaps not good to hardcode... TODO - remove hardcoded date
    rowIndex <- 1
  } else {
    print("Security data starts after 2006")
    rowIndex <- which(securityTemplateBS$quarter == firstDate) + 1
  }
  
  # Walk through rows, compare to last year, if change then alter depending on the date
  for(i in 2:nrow(trimmedInstance)) {
    if (yearCompare == trimmedInstance[i,1]) {
      print("No change")
    } else if (trimmedInstance[i,1] > yearCompare) {
      print("Increase so +B")
      securityBS[rowIndex, 1] <- 1
    } else if (trimmedInstance[i,1] < yearCompare) {
      print("Decrease so +S")
      securityBS[rowIndex, 2] <- 1
    }
    yearCompare <- trimmedInstance[i, 1]
    print(i)
    rowIndex <- rowIndex + 1
  }
  securityBS
}

