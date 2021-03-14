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


# cari FDC, exceedance probabilty 80%, 90%, dan 95%
sort_hujan <- sort(data_filter$P,decreasing = T)
sort_debit <- sort(data_debit_eto$Debit,decreasing = T)
sort_eto <- sort(data_debit_eto$Eto,decreasing = T)

## buat data frame dimana kolom x adalah prosentase dan kolom y adalah variabel
df_hujan <- data.frame(x=100/length(sort_hujan)*1:length(sort_hujan),y=sort_hujan)
df_debit<- data.frame(x=100/length(sort_debit)*1:length(sort_debit),y=sort_debit)
df_eto<- data.frame(x=100/length(sort_eto)*1:length(sort_eto),y=sort_eto)


## plot
plot(x = df_hujan$x, y = df_hujan$y, type = "l", log = "y",ylab="Hujan (mm)",
     xlab="Exceedance Probabilty (%)",main="Flow Duration Curve - Hujan Jam-jaman")
grid()
plot(x = df_debit$x, y = df_debit$y, type = "l", log = "y",ylab="Debit (m^3/dtk)",
     xlab="Exceedance Probabilty (%)",main="Flow Duration Curve - Debit Harian")
grid()
plot(x = df_eto$x, y = df_eto$y, type = "l", log = "y",ylab="Eto (mm)",
     xlab="Exceedance Probabilty (%)",main="Flow Duration Curve - Eto Harian")
grid()

## cari Q80, Q90, dan Q95%
### data hujan
x_h=df_hujan$x
y_h=df_hujan$y
percentage=c(80,90,95)
ep_hujan=c(y_h[which.min(abs(x_h - 80))],
         y_h[which.min(abs(x_h - 90))],y_h[which.min(abs(x_h - 95))])
duration.dataframe.h<-cbind(percentage,ep_hujan)
colnames(duration.dataframe.h)=c("%","Hujan (mm)")
duration.dataframe.h

### data debit
x_d=df_debit$x
y_d=df_debit$y
ep_debit=c(y_d[which.min(abs(x_d - 80))],
           y_d[which.min(abs(x_d - 90))],y_d[which.min(abs(x_d - 95))])
duration.dataframe.d<-cbind(percentage,ep_debit)
colnames(duration.dataframe.d)=c("%","Debit (m^3/dtk)")
duration.dataframe.d

### data eto
x_e=df_eto$x
y_e=df_eto$y
ep_debit=c(y_e[which.min(abs(x_e - 80))],
           y_e[which.min(abs(x_e - 90))],y_e[which.min(abs(x_e - 95))])
duration.dataframe.e<-cbind(percentage,ep_debit)
colnames(duration.dataframe.e)=c("%","Eto (mm)")
duration.dataframe.e


# cari debit banjir 
dle <- distLextreme(dlf=dlf, RPs=c(2,10,100), gpd=F)
plotLextreme(dle)
plotLextreme(dle, nbest=6, log=TRUE,ylab="Debit (m^3/dtk)",xlab="Periode Kala Ulang (Tahun)",
             main=c("Analisa Frekuensi",nama_pos),legargs = list(cex=0.6,bg="transparent"))
