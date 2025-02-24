setwd("U:/Macro/Projeções/Dados R")



library(GetBCBData)
library(dplyr)
library(rJava)
library(xlsx)
library(meedr)
library(zoo)
library(lubridate)
library(tidyverse)
library(rbcb)
library(readxl)
library(writexl)



# Anfavea

dados_sgs_automotivo <- GetBCBData::gbcbd_get_series(
  id = c("anfavea_prodtotal"=1373, "anfavea_vendastotal"=1378, "fenabrave_vendastotal"=7384, "anfavea_prodtotal_sa"=28527, "anfavea_vendastotal_sa"=28528, "fenabrave_vendas_auto"=7384 ),  
  first.date = "31/01/1993",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_automotivo)

write_xlsx(list(Series = as.data.frame(dados_sgs_automotivo)),"dados_sgs_automotivo.xlsx")

# Abras

dados_sgs_abras <- GetBCBData::gbcbd_get_series(
  id = c("abras"=28549),  
  first.date = "31/01/2000",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_abras)
write.xlsx(dados_sgs_abras,"dados_sgs_abras.xlsx",sheetName="Series",row.names=F)

# PIB mensal

dados_sgs_pib <- GetBCBData::gbcbd_get_series(
  id = c("pib_mensal"=4380),  
  first.date = "31/01/1990",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_pib)
write_xlsx(list(Series = as.data.frame(dados_sgs_pib)),"dados_sgs_pib.xlsx")

# inflação (IGP´s, IPCA, núcleos)

dados_sgs_ipca <- GetBCBData::gbcbd_get_series(
  id = c("igpdi"=190, "igpm"=189, "salario_m"=1619, "inpc"=188, "ipca"=433,  "ipca_livres"=11428, "ipca_ndr"=10841, "ipca_semidr"=10842, "ipca_dur"=10843, "ipca_serv"=10844, "ipca_monitorados"=4449, "nucleo_dp"=16122, "nucleo_ex1"=16121, "nucleo_ex0"=11427,  "nucleo_ma"=11426, "nucleo_ms"=4466, "nucleo_ex2"=27838, " nucleo_ex3"= 27839, "nucleo_p55"= 28750, "difusao"=21379, "ipca_comerc"= 4447,"ipca_ncomerc"=4448, "ipca_ind"= 27863,   "ipca_alimdomic"= 27864,    "ipca_alimebeb"= 1635, "ipca_hab"= 1636, "ipca_artigresid"= 1637, "ipca_vest"= 1638, "ipca_transp"= 1639, "ipca_comunic"= 1640, "ipca_saude"= 1641,  "ipca_desppess"= 1642, "ipca_educ"= 1643, "ipa_m"= 7450, "ipc_m"= 7453, "incc_m"= 7456),
  first.date = "29/02/1944",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_ipca)

#write.xlsx(dados_sgs_ipca,"dados_sgs_ipca.xlsx",sheetName="Series",row.names=F)
write_xlsx(list(Series = as.data.frame(dados_sgs_ipca)),"dados_sgs_ipca.xlsx")

# commodities (IC)

dados_sgs_comm <- GetBCBData::gbcbd_get_series(
  id = c("ic_br"=27574,  "ic_agro"=27575, "ic_metal"=27576, "ic_ener"=27577, "ic_dolar"=29042),
  first.date = "01/01/2002",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_comm)
write_xlsx(dados_sgs_comm,"dados_sgs_comm.xlsx")

# renda disponível

dados_sgs_renda_disp <- GetBCBData::gbcbd_get_series(
  id = c("renda_disp"=29023,  "renda_disp_restrita"=29024, "renda_disp_deflac"=29025, "renda_disp_restrita_deflac"=29026, "renda_disp_restrita_deflac_sa"=29028),
  first.date = "01/03/2003",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_renda_disp)
#write_xlsx(dados_sgs_renda_disp,"dados_sgs_renda_disp.xlsx")
write_xlsx(list(Series = as.data.frame(dados_sgs_renda_disp)),"dados_sgs_renda_disp.xlsx")

# IBC-Br

dados_sgs_ibc <- GetBCBData::gbcbd_get_series(
  id = c("ibc"=24363,  "ibc_sa"=24364),
  first.date = "01/01/2003",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_ibc)
write_xlsx(dados_sgs_ibc,"dados_sgs_ibc.xlsx")

# cambio mensal

dados_sgs_cambio <- GetBCBData::gbcbd_get_series(
  id = c("cambio_final"=3696,  "cambio_medio"=3698, "cambio_real_efet_ipca"=11752),
  first.date = "31/01/1953",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_cambio)
write_xlsx(list(Series = as.data.frame(dados_sgs_cambio)),"dados_sgs_cambio.xlsx")

# cambio diário (ptax)

dados_sgs_cambio_d <- GetBCBData::gbcbd_get_series(
  id = c("cambio_d"=1),
  first.date = "28/11/1984",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_cambio_d)
write_xlsx(list(Series = as.data.frame(dados_sgs_cambio_d)),"dados_sgs_cambio_d.xlsx")

# selic

dados_sgs_selic <- GetBCBData::gbcbd_get_series(
  id = c("selic_meta"=432),
  first.date = "05/03/1999",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_selic)
write_xlsx(list(Series = as.data.frame(dados_sgs_selic)), "dados_sgs_selic.xlsx")

# novo Caged

dados_sgs_caged <- GetBCBData::gbcbd_get_series(
  id = c("caged_total"=28763, "caged_total_sa"=28784, "caged_agro"=28764, "caged_transf"=28766,"caged_constr"=28770, "caged_com"=28771, "caged_serv"=28772, "caged_agro_sa"=28785, "caged_transf_sa"=28787,"caged_constr_sa"=28791, "caged_com_sa"=28792, "caged_serv_sa"=28793 ), 
  first.date = "01/01/1992",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_caged)
#write.xlsx(dados_sgs_caged,"dados_sgs_caged.xlsx",sheetName="Series",row.names=F)
write_xlsx(list(Series = as.data.frame(dados_sgs_caged)),"dados_sgs_caged.xlsx")

# Nuci

dados_sgs_nuci <- GetBCBData::gbcbd_get_series(
  id = c( "Nuci"= 24352, "Nuci_sa"= 28561),
  first.date = "01/01/2001",
  last.date = Sys.Date(),
  format.data = "wide"  
)
tail(dados_sgs_nuci)

write_xlsx(list(Series = as.data.frame(dados_sgs_nuci)),"dados_sgs_nuci.xlsx")

# Dados do Boletim Focus

indic <- c("IPCA", "IGP-M")
start_date <- "2021-01-01"
x <- get_monthly_market_expectations(start_date = start_date, `$top` = 20)


IPCA <- rbcb::get_series(c(IPCA = 433),
                         start_date = "2004-01-01",
                         end_date = "2017-12-01",
                         as = "ts")
monthplot(IPCA, main = "IPCA agrupado em meses")

ipca <- meedr::get_monthly(
  indicator      = "IPCA",
  first_date     = Sys.Date()-2*365,
  reference_date = format(Sys.Date(), "%m/%Y"),
  be_quiet       = TRUE
)

head(ipca, 5)
write.xlsx(ipca,"ipca_focus.xlsx",sheetName="Series")

# Quarterly market expectations for GDP indicator
pib<- meedr::get_quarterly(
  indicator      = "PIB Total",
  first_date     = "2021-01-01",
  reference_date = paste0(lubridate::quarter(Sys.Date()), "/", lubridate::year(Sys.Date())),
  be_quiet       = TRUE
)
head(pib, 5)

