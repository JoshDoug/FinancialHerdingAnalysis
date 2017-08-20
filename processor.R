# Program to process and analyse financial data for herding

# The general structure of this program is data is read in and create initial data structures at the start of the script
# then functions which are used to process the data are at the end, while the main loops that walk through the data are in the middle
# with some test code after

######### Set up data
#iss.data <- read.csv("InstitutionSecuritiesDataset.csv", header = TRUE) # Read in CSV Data into data frame
iss.data <- read.csv("data/AllInstitutions.csv", header = TRUE) # Read in CSV Data into data frame
institutionLevels <- levels(factor(iss.data$InstitutionID)) # Consolidate institutions into list and remove duplicates
securityTickerLevels <- levels(factor(iss.data$SecurityTicker)) # Consolidate securities into list and remove duplicates
quarterDates <- as.numeric(levels(factor(iss.data$ReportDate))) # Holds dates of each quarter

# Create a template data.frame to hold B and S for each security per institution and additional columns as the data is processed, remove first row
securityTemplate <- data.frame(B = rep(0, times = length(quarterDates)), S = rep(0, times = length(quarterDates)), quarter = quarterDates)[-1,]

# Create empty list to hold every security, in a name -> data.frame format
security.list <- list(TEMP = securityTemplate) # Set up start of empty List which will be overwritten
security.increment <- 1 # Set counter to use for list
######### Finish reading in and setting up data structures

######### Start of main program logic
# Main loop - walks through each security and parses the data for B and S, and calculates B/B+S and N
for(i in securityTickerLevels) {
  indices <- which(iss.data$SecurityTicker == i) # Get row numbers for current security
  dataSubset <- iss.data[indices, ] # Get rows for current security
  
  tempSecurityBS <- getSecurityBS(securityTemplate, dataSubset) # Get B and S for each quarter for a security
  tempSecurityBS <- calculateBSandN(tempSecurityBS) # Calculate N for each quarter for a security
  
  # Set up security
  security.list[[security.increment]] <- tempSecurityBS # Add security to list
  names(security.list)[security.increment] <- i # Add name of security to list
  
  security.increment <- security.increment + 1
}

p.quarters <- calculateP(security.list) # Calculates p for each quarter

#security.list # This now holds B, S, and N for every security
p.quarters # Holds p for each quarter

# Walk through each security and calculate AF and H for each quarter
for(name in names(security.list)) {
  security <- security.list[[name]]
  security <- calculateAFandH(security, p.quarters)
  security.list[[name]] <- security
}

# Get Standard Deviation for each quarter
## - either need to get a vector of h values for each quarter where a security is active (ie not Nan or 0)
## - or loop through each security again and manually work out with simple own version of standard deviation formula...
security.h.quarters <- data.frame(matrix(ncol = length(quarterDates[-1]), nrow = 0))
colnames(security.h.quarters) <- quarterDates[-1]

security.increment <- 1 # Set counter to use for list
for(name in names(security.list)) {
  security.h.quarters[security.increment,] <- security.list[[name]]$H
  security.increment <- security.increment + 1
}

# Set any 0 values to NA so that they can be averaged by ignoring 0 values and thus only take into account active securities
security.h.quarters[security.h.quarters == 0] <- NA
sd.quarters <- data.frame(quarter = quarterDates, hAvg = rep(0, times = length(quarterDates)), std = rep(0, times = length(quarterDates)), se = rep(0, times = length(quarterDates)), activeTotal = rep(0, times = length(quarterDates)))[-1,]
sd.quarters$hAvg <- colMeans(security.h.quarters, na.rm = TRUE)

for(name in names(security.h.quarters)) {
  hValues <- security.h.quarters[,name]
  sd.quarters[sd.quarters$quarter == name, "std"] <- sd(hValues, na.rm = TRUE)
  sd.quarters[sd.quarters$quarter == name, "se"] <- sd(hValues, na.rm = TRUE) / sqrt(length(hValues[!is.na(hValues)]))
  sd.quarters[sd.quarters$quarter == name, "activeTotal"] <- length(hValues[!is.na(hValues)])
}

sd.quarters$p <- p.quarters$p # Add P values so quarter data is in a single place
sd.quarters # Hold all the potentially useful values and info
p.quarters

######### End of main program logic

######### Sample graphing code

round.h <- round(h.quarters, 8) # 1 way of making results more human readable
sprintf("%f", h.quarters) # 2nd way of making results human readable
quarters <- as.Date(as.character(p.quarters$quarter), "%Y%m%d") # Make date values

p.quarters

plot(quarters, p.quarters$p, ylim= c(0,1))
plot(quarters, round.h, ylim= c(-0.005,0.015))


######### Functions
# Calculate AF
## This is calculated per security per quarter, so a single security will have an AF for each quarter which will then be used to calculate H
calculateAFandH <- function(securityBS, p.quarters) {
  securityBS$P <- unlist(p.quarters$p)
  securityBS$AF <- unlist(rep(0, times = length(p.quarters$p)))
  securityBS$H <- unlist(rep(0, times = length(p.quarters$p)))

  for(i in 1:nrow(p.quarters)) {
    if(securityBS[i, "N"] > 0) { # Only calculate AF and H if N is greater than 0, if N is not greater than 0 then they remain at the default of 0
      AF <- calculateAFQuarter(securityBS[i, "N"], securityBS[i, "P"])
      securityBS[i, "AF"] <- AF
      #print(c("AF: ", AF))
      
      H <- calculateHQuarter(securityBS[i, "BS"], securityBS[i, "P"], AF)
      securityBS[i, "H"] <- H
    }
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
    AF <- AF + (dbinom(k, N, p) * abs((k/N) - p)) # dbinom takes the arguments x (k), size (N), prob (p)
  }
  return(AF)
}

# Calculate P
calculateP <- function(p.list) {
  # Data structure to hold p for each quarter, first row is removed as it is not applicable
  p.quarters <- data.frame(p = rep(0, times = length(quarterDates)), activeSecurities = rep(0, times = length(quarterDates)), quarter = quarterDates)[-1,]

  for(name in names(p.list)) {
    security <- p.list[[name]]
    p.quarters$p <- p.quarters$p + security$BS
    p.quarters$activeSecurities <- p.quarters$activeSecurities + ifelse(security$N > 0, 1, 0)
  }
  print(p.quarters$p)
  p.quarters$p <- p.quarters$p / p.quarters$activeSecurities
  return(p.quarters)
}

# Calculate B/B+S and N for each quarter of a security and add the rows to the data frame
calculateBSandN <- function(securityBS) {
  securityBS$BS <- unlist(securityBS$B / (securityBS$B + securityBS$S)) # Calculate B/B+S - should this be rounded to 2 or 3 decimal places?
  securityBS$N <- unlist(securityBS$B + securityBS$S) # Calculate N = B + S
  securityBS$BS[is.nan(securityBS$BS)] = 0 # Set any NaNs to 0 - where B and S are both 0
  return(securityBS)
}

# Get B and S totals for a security
getSecurityBS <- function(securityTemplateBS, dataSubset) {
  securityTotalBS <- securityTemplateBS # Create copy of template, to avoid overriding contents of template
  for(i in institutionLevels) { # Add each B and S for each quarter of a security, cumulatively per institution
    securityInstanceIndices <- which(dataSubset$InstitutionID == i) # Get all indices (row numbers) of a security for a particular security
    if(length(securityInstanceIndices) != 0) { # Check that we actually got any indices - or this will throw an error
      securityInstance <- dataSubset[securityInstanceIndices, ] # Get the relevant rows
      securityTempBS <- walkSecurityInstance(securityInstance, securityTemplateBS) # Get B and S for each quarter of a security for a specific institution
      securityTotalBS$B <- securityTotalBS$B + securityTempBS$B # Add B data to cumulative B total for security
      securityTotalBS$S <- securityTotalBS$S + securityTempBS$S # Add S data to cumulative S total for security
    }
  }
  return(securityTotalBS)
}

# Get B and S for a security of a single institution
walkSecurityInstance <- function(securityInstance, securityTemplateBS) {
  securityBS <- securityTemplateBS # Create new object copy of template - don't want to alter original template
  trimmedInstance <- securityInstance[, 3:4] # Remove columns holding Institution and Security names
  trimmedInstance <- trimmedInstance[order(trimmedInstance$ReportDate), ] # Order by date
  
  if(nrow(trimmedInstance) > 1) { # Ignore any data subset that is 1 row or less
    
    yearCompare <- trimmedInstance[1, "SharesHeld"] # Holds the no. of shares held of the prior quarter, updated after each quarter to prior quarter
    firstDate <- trimmedInstance[1, "ReportDate"] # Holds the first date for the shares held by an Institution for a particular Security
    
    rowIndex <- NA # Needs to be set to a value outside of the if-else statement
    # Get first index of date to use in template for setting B or S
    # rowIndex is the current date +1 in the template - because the rowIndex will be compared against the previous quarter
    # The check against quarterDates[1] is because the first quarter could be a quarter earlier than in the template,
    # which the first quarter was removed from. So 1 is actually equivalent to row 0 + 1, so the + 1 for the if is implicit
    if(firstDate == quarterDates[1]) {
      rowIndex <- 1 # First possible quarter - relies on quarterDates vector being global
    } else {
      # The first row for this dataset starts after the first possible date for a dataset
      rowIndex <- which(securityTemplateBS$quarter == firstDate) + 1
    }

    # Starts from 2 because the shares held for row 1 is already in yearCompare to compare against
    for(i in 2:nrow(trimmedInstance)) {
      if (yearCompare == trimmedInstance[i,1]) {
        # No change, do nothing - 0 is the default value, so doesn't need to be set to 0
      } else if (trimmedInstance[i,1] > yearCompare) {
        securityBS[which(securityTemplateBS$quarter == trimmedInstance[i, "ReportDate"]), 1] <- 1 # Shares held increased, so +B
      } else if (trimmedInstance[i,1] < yearCompare) {
        securityBS[which(securityTemplateBS$quarter == trimmedInstance[i, "ReportDate"]), 2] <- 1 # Shares held decreased, so +S
      }
      yearCompare <- trimmedInstance[i, 1]
    }
  } # Else do nothing and the blank template gets returned, which is fine
  return(securityBS)
}
######### End of functions

######### Start Testing Code

security.list[["K"]] # Security results to test against

## Test B/B+S calculations
indices <- which(iss.data$SecurityTicker == "UNP") # Grab a security
#indices <- which(iss.data$SecurityTicker == "UNP" & iss.data$InstitutionID == "07KRX4-E")
dataSubset <- iss.data[indices, ]
dataSubset
testSecurityUNP <- getSecurityBS(securityTemplate, dataSubset)
testSecurityUNP <- calculateBSandN(testSecurityUNP)
testSecurityUNP # Optionally can round B/B+S value using round: round(result, 2) where 2 is for choosing 2 decimal points

security.list[["UNP"]]

## Test totalling only active securities per quarter
test.security.active <- security.list[["UNP"]]
test.total <- ifelse(test.security.active$N > 0, 1, 0)
test.total

p.quarters

######### End Testing Code