# delete environment R sebelumnya
rm(list=ls())

# set nama lokasi tinjauan
nama_pos <- "Nanjung"

# input data
library(readr)
data_hujan <- read_csv("data_hujan_jam2an_csv.csv", 
                                  col_types = cols(date_time = col_datetime(format = "%m/%d/%Y %H:%M")))



# rekap data
summary(data_hujan)
library(psych)
describe(data_hujan$P)

library(extremeStat)
dlf <- distLfit(data_hujan$P)
plotLfit(dlf,legend = T,col = "cyan4")
plotLfit(dlf, cdf=TRUE)

#filter data. pilih nilai diatas 0.1 mm (minimum BMKG)
data_filter <- subset(data_hujan,data_hujan[,2] >= 0.1)
summary(data_filter)
describe(data_filter$P)

dlf <- distLfit(data_filter$P)
plotLfit(dlf,legend = T,col = "grey", main = "Grafik PDF Hujan Jam-jaman", xlab="Hujan (mm)",nbest = 6)
plotLfit(dlf, cdf=TRUE)
dle <- distLextreme(dlf=dlf, RPs=c(2,10,100), gpd=F)
plotLextreme(dle)
plotLextreme(dle, nbest=6, log=TRUE,ylab="Debit (m^3/dtk)",xlab="Periode Kala Ulang (Tahun)",
             main=c("Analisa Frekuensi",nama_pos),legargs = list(cex=0.6,bg="transparent"))
