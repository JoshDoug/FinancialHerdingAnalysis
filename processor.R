# Program to process and analyse financial data for herding

iss.csv <- read.csv("InstitutionSecuritiesDataset.csv", header = TRUE)
str(iss.csv)

shares.freq <- table(iss.csv$SharesHeld)
dates.freq <- table(iss.csv$ReportDate)

institutions <- factor(iss.csv$InstitutionID)
institutionLevels <- levels(institutions)

securities <- factor(iss.csv$SecurityTicker)
securityTickerLevels <- levels(securities)

for(i in institutionLevels) {
  print(i)
}

for(i in securityTickerLevels) {
  print(i)
}