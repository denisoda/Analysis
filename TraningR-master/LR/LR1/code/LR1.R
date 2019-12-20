#устновка пакетов
install.packages(c("plyr", "ggplot2", "ggthemes", "knitr",
                   "highcharter", "stringi", "sp", "rmarkdown")) 
#получение спаска установленных пакетов
packages <- dir(.libPaths())

#создание списка нужных нам пакетов
pack <- c("cluster", "dplyr", "ggplot2", "ggthemes", "knitr",
          "highcharter", "stringi", "sp", "lubridate", "plyr",
          "rmarkdown", "stats", "tidyr", "xts")

#проверка есть ли нужные нам пакеты в списке
pack %in% packages

#сохранение пакетов в txt файл
write.table(packages, "docs/packages.txt")

dat <- read.csv("data/List of species EuroBirdwatch-2014 Belarus .csv",
                header = TRUE, encoding = "UTF-8")

dat$individualsPerObservation = dat$individualsNumber / dat$observationsNumber

dat$speciesShare = dat$individualNumber / sum(dat$observationsNumber)

sort(dat$individualsNumber, decreasing = TRUE)

dat1 <- dat[order(dat$individualsNumber, decreasing = TRUE),]

write.csv(dat1, file = "data/List of species EuroBirdwatch-2014 Belarus_updated.csv")

NewTable <- data.frame(SumIndNum = sum(dat$individualsNumber))

NewTable$MedNum = median(dat$observationsNumber)

PerZnach <- subset(dat, dat$individualsNumber > 0 & dat$individualsNumber < 11)
VroZnach <- subset(dat, dat$individualsNumber > 10 & dat$individualsNumber < 51)
TreZnach <- subset(dat, dat$individualsNumber > 50 & dat$individualsNumber < 101)
CheZnach <- subset(dat, dat$individualsNumber > 100 & dat$individualsNumber < 501)
FivZnach <- subset(dat, dat$individualsNumber > 500 & dat$individualsNumber < 1000)
SixZnach <- subset(dat, dat$individualsNumber > 1000)

MaxVal <- max(dat$observationsNumber, na.rm = FALSE)
MaxBird <- subset(dat$speciesRus, dat$observationsNumber == MaxVal)