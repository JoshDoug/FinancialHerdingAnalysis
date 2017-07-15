# Program to process and analyse financial data for herding

## Read in CSV Data
iss.csv <- read.csv("InstitutionSecuritiesDataset.csv", header = TRUE)
str(iss.csv) # Show structure (str)

# Consolidate institutions into unique list - useful for looping through
institutions <- factor(iss.csv$InstitutionID)
institutionLevels <- levels(institutions)

# Consolidate security tickers into unique list
securities <- factor(iss.csv$SecurityTicker)
securityTickerLevels <- levels(securities)

# Get dates and convert into date format - not necessary but may be useful
dates <- factor(iss.csv$ReportDate)
dateLevels <- levels(dates)
quarterDates <- as.Date(dateLevels, "%Y%m%d") # Convert into date objects

dateFormat <- format(quarterDates, "%Y%b%d") # Not used, just an example of formating a date

# Create a template data.frame to hold B or S for each security per institution - then add together after
# Currently need a data.frame for B and another for S...could consolidate into a single data.frame?
securityTemplateBS <- data.frame(B = rep(0, times = length(dateTest)), S = rep(0, times = length(dateTest)), quarter = dateTest)
# securityChangeHold[[4,1]] # Test to access data in 4th row of first column
# securityChangeHold[[4,1]] <- 1 # Test to alter field in 4th row of first column
securityTemplateBS

# Quick test to see if new object is created or reference (new object! so altering it doesn't alter the original)
testSecTemplate <- securityTemplateBS
# testSecTemplate[5,1] <- 5 # Change row 5 column 1 field value to 5
testSecTemplate

for(i in institutionLevels) {
  print(i)
}

for(i in securityTickerLevels) {
  print(i)
}

# Okay lets do a test run with UNP and then loop the rest of them
# Loop through Institutions, grab from the iss.csv or table or whatever the rows
# that equal the current institution and security, then create a vector/matrix and populate with B and S values
# Store it? hm.. but then do the same again for each institution and add together,
# maybe add at the end of each loop?

