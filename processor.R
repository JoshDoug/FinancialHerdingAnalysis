# Program to process and analyse financial data for herding

# The general structure of this program is data is read in and initial data structures are created at the start of the script
# then functions which are used to process the data are at the end, while the main loops that walk through the data are in the middle

## Would be interesting to see if results change if securities were restricted to a minimum amount of rows and/or institutions?

# Set up data
iss.data <- read.csv("InstitutionSecuritiesDataset.csv", header = TRUE) # Read in CSV Data into data frame
institutionLevels <- levels(factor(iss.data$InstitutionID)) # Consolidate institutions into list and remove duplicates
securityTickerLevels <- levels(factor(iss.data$SecurityTicker)) # Consolidate securities into list and remove duplicates
quarterDates <- levels(factor(iss.data$ReportDate)) # Holds dates of each quarter

# Create a template data.frame to hold B and S for each security per institution and additional columns as the data is processed, remove first row
securityTemplate <- data.frame(B = rep(0, times = length(quarterDates)), S = rep(0, times = length(quarterDates)), quarter = quarterDates)[-1,]

# Create empty list to hold every security
security.list <- list(TEMP = securityTemplate) # Set up empty List
security.increment <- 1 # Set counter to use for list - might be able to use the for loop temp var i as a counter instead?

# The main loop which walks through each security and parses the data for B and S which can then be used to calculate everything
for(i in securityTickerLevels) {
  #print(i) # Print current security, useful for debugging conflicts etc
  indices <- which(iss.data$SecurityTicker == i)
  dataSubset <- iss.data[indices, ]
  tempSecurityBS <- getSecurityBS(securityTemplate, dataSubset) # Get B and S for each quarter for a security
  tempSecurityBS <- calculateBSandN(tempSecurityBS) # Calculate N for each quarter for a security
  
  # Set up security
  security.list[[security.increment]] <- tempSecurityBS # Add security to list
  names(security.list)[security.increment] <- i # Add name of security to list
  
  security.increment <- security.increment + 1
}

security.list # This now holds B, S, and N for every security so p can now be calculated for each quarter
p.quarters <- calculateP(security.list) # Calculates p for each quarter
p.quarters # Now have p for each quarter and the actual calculations can begin

# Walk through each security and calculate AF and H for each quarter
for(name in names(security.list)) {
  security <- security.list[[name]]
  security <- calculateAFandH(security, p.quarters)
  security.list[[name]] <- security
}

# Set up H template
h.quarters <- rep(0, times = length(p.quarters$p))

# Get total of H for each quarter
for(name in names(security.list)) {
  security <- security.list[[name]]
  print(security$H)
  security$H[is.nan(security$H)] = 0
  h.quarters <- h.quarters + security$H
}

# Get average of H for each quarter
h.quarters <- h.quarters / length(securityTickerLevels)
h.quarters

######### Start Testing Code

str(p.list[["UNP"]])
p.list[["UNP"]]

# Grab all rows from the data frame where security ticker equals UNP - similar to an SQL query - just for testing currently
indices <- which(iss.data$SecurityTicker == "K")
indices <- which(iss.data$SecurityTicker == "CCO" & iss.data$InstitutionID == "07KRX4-E")
dataSubset <- iss.data[indices, ]
dataSubset

fix <- getSecurityBS(securityTemplateBS, dataSubset)

#options(max.print=1000000)

fixIndices <- which(dataSubset$InstitutionID == "00BQTS-E")
fixSubset  <- dataSubset[fixIndices, ]
fixSubset <- fixSubset1
fixSubset <- fixSubset[order(fixSubset$ReportDate), ] # Reorder by date - just in case the dates are out of order!
fixSubset
help(order)

# dbinom takes the arguments x (k), size or x (N), prob (p)
p.list[["UNP"]]
# Test results for 1st relevant quarter of UNP, 2007-03-31
# B = 2, S = 0, BS = 1, N = 2, p = 0.1133196
dbinom(0, 0, 0.113)

abs(0/2 - 0.1133196)
help(dbinom)

for(p.quarter in p.quarters) {
  print(p.quarter)
  #str(p.quarter)
}

testAFH <- p.list[["UNP"]]
testAFH <- calculateAFandH(testAFH, p.quarters)
testAFH

length(testAFH$B)

######### End Testing Code

# Calculate AF
## This is calculated per security per quarter, so a single security will have an AF for each quarter which will then be used to calculate H
calculateAFandH <- function(securityBS, p.quarters) {
  securityBS$P <- unlist(p.quarters$p)
  securityBS$AF <- unlist(rep(0, times = length(p.quarters$p)))
  securityBS$H <- unlist(rep(0, times = length(p.quarters$p)))

  for(i in 1:nrow(p.quarters)) {
    #print(securityBS[i,"P"])
    AF <- calculateAFQuarter(securityBS[i, "N"], securityBS[i, "P"])
    securityBS[i, "AF"] <- AF
    print(AF)
    
    H <- calculateHQuarter(securityBS[i, "BS"], securityBS[i, "P"], AF)
    securityBS[i, "H"] <- H
  }
  return(securityBS)
}

calculateHQuarter <- function(BS, p, AF) {
  H <- abs(BS - p) - AF
  return(H)
}

calculateAFQuarter <- function(N, p) {
  AF <- 0
  for(k in 0:N) {
    AF <- AF + (dbinom(k, N, p) * abs((k/N) - p))
  }
  return(AF)
}

# Calculate P
calculateP <- function(p.list) {
  # Hold p for each quarter...
  p.quarters <- data.frame(p = rep(0, times = length(quarterDates)), quarter = quarterDates)
  p.quarters <- p.quarters[-1,] # Remove first row
  
  for(name in names(p.list)) {
    print(name)
    print(p.list[[name]])
    
    security <- p.list[[name]]
    p.quarters$p <- p.quarters$p + security$BS
  }
  
  p.quarters$p <- p.quarters$p / length(securityTickerLevels)
  return(p.quarters)
}

# Calculate B/B+S and N for each quarter of a security and add the rows to the data frame
calculateBSandN <- function(securityBS) {
  securityBS$BS <- unlist(securityBS$B / securityBS$B + securityBS$S) # Calculate B/B+S
  securityBS$N <- unlist(securityBS$B + securityBS$S) # Calculate N = B + S
  securityBS$BS[is.nan(securityBS$BS)] = 0 # Set any NaNs to 0
  return(securityBS)
}

# Get B and S for a security
getSecurityBS <- function(securityTemplateBS, dataSubset) {
  securityTotalBS <- securityTemplateBS
  for(i in institutionLevels) {
    #print(i)
    securityInstanceIndices <- which(dataSubset$InstitutionID == i) # Get all indices of a security for a particular security
    if(length(securityInstanceIndices) != 0) { # Check that we actually got any indices - or this will throw an error
      print(i) # Print current institution, useful for 'debugging' conflicts etc
      securityInstance <- dataSubset[securityInstanceIndices, ] # Get the rows back and then pass to the walkSecurity function to get it processed
      securityTempBS <- walkSecurityInstance(securityInstance, securityTemplateBS) # Get B and S for each quarter of a security for a specific institution
      securityTotalBS$B <- securityTotalBS$B + securityTempBS$B # In R you can just add two vectors together and it will add each equivalent value
      securityTotalBS$S <- securityTotalBS$S + securityTempBS$S # So this just adds the number for B and S for each corresponding quarter together
    }
  }
  return(securityTotalBS)
}

# Get B and S for a security of a single institution
walkSecurityInstance <- function(securityInstance, securityTemplateBS) {
  securityBS <- securityTemplateBS # Create new object copy of template - don't want to alter original template
  trimmedInstance <- securityInstance[, 3:4] # could be trimmed earlier, not *really* necessary to trim at all
  trimmedInstance <- trimmedInstance[order(trimmedInstance$ReportDate), ]

  yearCompare <- trimmedInstance[1, "SharesHeld"] # holds the value of the prior year to compare against, is reset at end of loop to the latest year
  firstDate <- trimmedInstance[1, "ReportDate"] # holds the first date
  
  rowIndex <- NA
  # Get first index of date to use in template for setting B or S
  # rowIndex is the current date +1 in the template, which may be a bit confusing
  # alternative is set 0 or not add 1 and then increment at start of loop
  if(firstDate == quarterDates[1]) {
    # This is the first possible date in the dataset - relies on quarterDates vector being global
    rowIndex <- 1
  } else {
    # The first row for this dataset starts after the first possible date for a dataset
    rowIndex <- which(securityTemplateBS$quarter == firstDate) + 1
  }
  
  if(nrow(trimmedInstance) > 1) { # Ignore any data subset that is 1 row or less
    # Walk through rows, compare to last year, if change then alter depending on the date
    for(i in 2:nrow(trimmedInstance)) {
      if (yearCompare == trimmedInstance[i,1]) {
        # No change, do nothing
      } else if (trimmedInstance[i,1] > yearCompare) {
        securityBS[rowIndex, 1] <- 1 # Shares held increased, so +B
      } else if (trimmedInstance[i,1] < yearCompare) {
        securityBS[rowIndex, 2] <- 1 # Shares held decreased, so +S
      }
      yearCompare <- trimmedInstance[i, 1]
      rowIndex <- rowIndex + 1
    }
  } # Else do nothing and the blank template gets returned, which is fine
  return(securityBS)
}

