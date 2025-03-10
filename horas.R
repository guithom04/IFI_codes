setwd("U:/Macro/Projeções/PIB/Hiato do produto/SS/Ajuste Sazonal")
library("xlsx")
library("ggplot2")
library("graphics")
library("seasonal")
library("readxl")
library("writexl")


Series <- ts(read_excel("horas_original.xlsx",sheet=1) [,-1] ,start=c(1996,1),freq=4)
horas_sa<-seas(Series[,1])
plot(horas_sa)
write_xlsx(list(Series =as.data.frame(horas_sa)), "horas_sa.xlsx")

