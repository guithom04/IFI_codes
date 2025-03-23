ipca.measures <- function(){
  IPCA_12_Meses <- R_BC_Direct(13522)
  IPCA_Administrados <- R_BC_Direct(4449)
  IPCA_Administrados <- IPCA_Administrados %>%
    mutate(SGS_4449 = as.numeric(SGS_4449)) %>%
    arrange(data) %>%
    mutate(IPCA_adm_12m = (1 + SGS_4449 / 100)) %>%
    mutate(IPCA_adm_12m = zoo::rollapply(IPCA_adm_12m, 12, prod, align = "right", fill = NA)) %>%
    mutate(IPCA_adm_12m = round((IPCA_adm_12m - 1) * 100, 2)) %>%
    select(data, IPCA_adm_12m)
  IPCA_Livres <- R_BC_Direct(11428)
  IPCA_Livres <- IPCA_Livres %>%
    mutate(SGS_11428 = as.numeric(SGS_11428)) %>%
    arrange(data) %>%
    mutate(IPCA_Livres_12m = (1 + SGS_11428 / 100)) %>%
    mutate(IPCA_Livres_12m = zoo::rollapply(IPCA_Livres_12m, 12, prod, align = "right", fill = NA)) %>%
    mutate(IPCA_Livres_12m = round((IPCA_Livres_12m - 1) * 100, 2)) %>%
    select(data, IPCA_Livres_12m)
  merged_ipca <- reduce(list(IPCA_12_Meses,
                             IPCA_Administrados,
                             IPCA_Livres),
                        full_join, by = "data")
  colnames(merged_ipca) <- c("data","ipca","ipca_adm","ipca_livres")
}
