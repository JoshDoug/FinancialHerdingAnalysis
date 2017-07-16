# Program to process and analyse financial data for herding

## Read in CSV Data
iss.data <- read.csv("InstitutionSecuritiesDataset.csv", header = TRUE) # This is a data frame
str(iss.csv) # Show structure (str)

# Consolidate institutions into unique list - useful for looping through
institutions <- factor(iss.data$InstitutionID)
institutionLevels <- levels(institutions)

# Consolidate security tickers into unique list
securities <- factor(iss.data$SecurityTicker)
securityTickerLevels <- levels(securities)

# Get dates and convert into date format - not necessary but may be useful
dates <- factor(iss.data$ReportDate)
dateLevels <- levels(dates)
quarterDates <- as.Date(dateLevels, "%Y%m%d") # Convert into date objects

dateFormat <- format(quarterDates, "%Y%b%d") # Not used, just an example of formating a date

# Create a template data.frame to hold B or S for each security per institution - then add together after
# Currently need a data.frame for B and another for S...could consolidate into a single data.frame?
securityTemplateBS <- data.frame(B = rep(0, times = length(quarterDates)), S = rep(0, times = length(quarterDates)), quarter = quarterDates)
# securityChangeHold[[4,1]] # Test to access data in 4th row of first column
# securityChangeHold[[4,1]] <- 1 # Test to alter field in 4th row of first column
securityTemplateBS

# Quick test to see if new object is created or reference (new object! so altering it doesn't alter the original)
testSecTemplate <- securityTemplateBS
# testSecTemplate[5,1] <- 5 # Change row 5 column 1 field value to 5
testSecTemplate[5,] # Return row 5, all 3 columns - don't need to set to return the row
testSecTemplate

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

for(i in institutionLevels) {
  print(i)
  # which(iss.data$InstitutionID == i & iss.data$SecurityTicker == "UNP") # might not work well
}



