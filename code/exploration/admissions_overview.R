# Library ====
library(bigrquery)
library(DBI)
library(dplyr)
library(data.table)
library(lubridate)
library(glue)

# Connection to BigQuery ====
Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()
con <- dbConnect(
    bigrquery::bigquery(),
    project = "bgse-dsc",
    dataset = "MIMIC3_V1_4",
    billing = billing
)
dbListTables(con)

# Data Download ====
admissions <- tbl(con, "ADMISSIONS")
admissions <- as.data.table(admissions)
#View(admissions)
transfers <- tbl(con, "TRANSFERS")
transfers <- as.data.table(transfers)
#View(transfers)

# Stats ====
## When do admissions happen?
# overall
admissions[, hist(hour(ADMITTIME))]
# by weekday
admissions[, admitweekday := weekdays(ADMITTIME)] # create new column weekday
admissions[, hour(ADMITTIME), by = admitweekday]
par(mfrow=c(4,2))
dd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
for(i in dd) {
    hist(admissions[admitweekday == i, hour(ADMITTIME)], main=paste0("Admissions on ", i), 
         xlab = "Time of the Day", col="coral", border = "white", 
         xlim=c(0,25), ylim= c(0,1300))
}
plot.new()
par(mfrow=c(1,1))

## How long do patients stay? 
# overall distribution
cols <- c("darkgrey","seashell", "darkorange", "darkolivegreen1", "khaki1", "mistyrose", 
          "royalblue1","lightpink")
admissions[, duration := as.numeric(as.duration(DISCHTIME - ADMITTIME), 
                                    "hours")] # create new column length of stay
plot(density(admissions[duration < 1000, duration]), main = "Length of Stay in Hours", 
     lwd=5, col=cols[1])

for(i in 1:7) {
    lines(density(admissions[admitweekday == dd[i] & duration < 1000, duration]), lwd=2, col=cols[i+1])
}
legend("topright", col=cols, legend= c("Overall", dd), lwd=3, cex = .7, bty="n",lty=1)

