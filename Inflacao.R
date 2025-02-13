# Import using base R
glossario <- read.csv("U:/Pastas pessoais/LucasM/Downloads/glossario_bcb_final.csv",
                      sep = ";",
                      header = TRUE, 
                      stringsAsFactors = FALSE)

# View the first few rows of the data
head(glossario)
View(glossario)

# pegar dados ipca
# livres e administrados
livres = rbcb::get_series(code = c(livres = "11428"))
administrados = rbcb::get_series(code = c(administrados = "4449"))

# núcleos
nucleo_mass <- rbcb::get_series(code = c(nucleo_mass = "11426"))
nucleo_macs <- rbcb::get_series(code = c(nucleo_macs = "4466"))
núcleo_EX0 <- rbcb::get_series(code = c(núcleo_EX0 = "11427"))
núcleo_EX1 <- rbcb::get_series(code = c(núcleo_EX1 = "16121"))
nucleo_dp <- rbcb::get_series(code = c(nucleo_dp = "16122"))

# difusão
difusao <- rbcb::get_series(code = c(difusao = "21379"))

tail(difusao)

# Merge progressivo com merge() do R base
dados_merge <- merge(livres, administrados, by = "date", all = TRUE)
dados_merge <- merge(dados_merge, nucleo_mass, by = "date", all = TRUE)
dados_merge <- merge(dados_merge, nucleo_macs, by = "date", all = TRUE)
dados_merge <- merge(dados_merge, núcleo_EX0, by = "date", all = TRUE)
dados_merge <- merge(dados_merge, núcleo_EX1, by = "date", all = TRUE)
dados_merge <- merge(dados_merge, nucleo_dp, by = "date", all = TRUE)
dados_merge <- merge(dados_merge, difusao, by = "date", all = TRUE)

# Visualiza as primeiras linhas do dataset unificado
head(dados_merge)
tail(dados_merge)


write.csv(dados_merge, file = "merge_inflacao.csv")



