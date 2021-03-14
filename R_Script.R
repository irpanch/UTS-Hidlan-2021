# delete environment R sebelumnya
rm(data_debit_eto)

# set nama lokasi tinjauan
nama_pos <- "Nanjung"

# input data
library(readr)
data_hujan <- read_csv("data_hujan_jam2an_csv.csv", 
                                  col_types = cols(date_time = col_datetime(format = "%m/%d/%Y %H:%M")))

data_debit_eto <- read_csv("data_debit_eto_csv.csv")
data_debit_eto$Tanggal <- as.Date(data_debit_eto$Tanggal,format="%d-%b-%y")

# filter data. pilih nilai diatas 0.1 mm (minimum BMKG)
data_filter <- subset(data_hujan,data_hujan[,2] >= 0.1)

# buat grafik CDF dan PDF
library(extremeStat) 
# Untuk data hujan
dlf_P <- distLfit(data_filter$P)
plotLfit(dlf_P,legend = T,col = "cadetblue1", main = "Grafik PDF Hujan Jam-jaman", xlab="Hujan (mm)",nbest = 6)
plotLfit(dlf_P, cdf=TRUE, main = "Grafik CDF Hujan Jam-jaman", xlab="Hujan (mm)")

# Untuk data debit
dlf_Debit <- distLfit(data_debit_eto$Debit)
plotLfit(dlf_Debit,legend = T,col = "cornflowerblue", main = "Grafik PDF Debit Harian", xlab="Debit (m^3/dtk)",nbest = 6)
plotLfit(dlf_Debit, cdf=TRUE, main = "Grafik CDF Debit Harian", xlab="Debit (m^3/dtk)")

# Untuk data eto
dlf_Eto <- distLfit(data_debit_eto$Eto)
plotLfit(dlf_Eto,legend = T,col = "aquamarine", main = "Grafik PDF Evapotranspirasi Potensial Harian", xlab="Eto (mm)",nbest = 6)
plotLfit(dlf_Eto, cdf=TRUE, main = "Grafik CDF Evapotranspirasi Potensial Harian", xlab="Eto (mm)")



dle <- distLextreme(dlf=dlf, RPs=c(2,10,100), gpd=F)
plotLextreme(dle)
plotLextreme(dle, nbest=6, log=TRUE,ylab="Debit (m^3/dtk)",xlab="Periode Kala Ulang (Tahun)",
             main=c("Analisa Frekuensi",nama_pos),legargs = list(cex=0.6,bg="transparent"))
