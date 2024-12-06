################################################################################
### PACOTES NECESSÁRIOS
################################################################################

options(scipen=999)
library(tidyverse)
library(readxl)
library(janitor)
library(usethis)

usethis::create_github_token()
usethis::edit_r_environ()
usethis::git_sitrep()

usethis::use_git()
usethis::use_github()

################################################################################
### LEITURA DOS BANCOS DE DADOS 
################################################################################

dados_SIGA1 <- read_excel("Dados/6616f24e-6a34-4942-823b-68a3b7038fa2.xlsx") %>% 
  janitor::clean_names()

dados_SIGA2 <- read_excel("Dados/707458a9-4809-441d-9b30-84ef7f65009b.xlsx") %>% 
  janitor::clean_names() 

Emendas_Parlamentares_Novo <- read_excel("Dados/Emendas-Parlamentares Novo.xlsx") %>% 
  janitor::clean_names()

Emendas_planilha_mãe <- read_excel("Dados/Emendas - planilha mãe.xlsx") %>% 
  janitor::clean_names()

################################################################################
### MERGE DOS DADOS
################################################################################

dados_MERGE1 <-  dados_SIGA1 %>% 
  dplyr::left_join(dados_SIGA2, by = "emenda_numero_ano") %>% 
  dplyr::filter(empenhado != 0) %>% 
  dplyr::select(favorecido_do_pagamento_cpf_cnpj, favorecido_do_pagamento_municipio_uf, favorecido_do_pagamento_uf, 
                emenda_numero_ano, empenhado)


dados_MERGE2 <- dados_MERGE1 %>% 
  #dplyr::select(favorecido_do_pagamento_cpf_cnpj, empenhado) %>% 
  dplyr::left_join(Emendas_Parlamentares_Novo %>% 
                     dplyr::mutate(cnpj_do_favorecido = base::as.character(cnpj_do_favorecido)),
                   by = c("favorecido_do_pagamento_cpf_cnpj" = "cnpj_do_favorecido")) %>% 
  dplyr::filter(ano == "2024") %>% 
  dplyr::filter(nome_emenda == "Emenda Individual" & transferencia_especial == "Sim") %>%
  dplyr::select(favorecido_do_pagamento_cpf_cnpj, favorecido_do_pagamento_municipio_uf, favorecido_do_pagamento_uf, 
                codigo_siafi,  codigo_ibge, nome_ente,
                emenda_numero_ano, empenhado, nome_emenda, transferencia_especial) %>% 
  #dplyr::distinct() %>% 
  dplyr::mutate(Regiao = dplyr::case_when(favorecido_do_pagamento_uf == "PR" |
                                          favorecido_do_pagamento_uf == "SC" |
                                          favorecido_do_pagamento_uf == "RS" ~ "R1",
                                        
                                          favorecido_do_pagamento_uf == "ES" |
                                          favorecido_do_pagamento_uf == "GO" |
                                          favorecido_do_pagamento_uf == "MG" |
                                          favorecido_do_pagamento_uf == "MS" |
                                          favorecido_do_pagamento_uf == "PR" |
                                          favorecido_do_pagamento_uf == "RJ" |
                                          favorecido_do_pagamento_uf == "SP" ~ "R2",
                                          
                                          favorecido_do_pagamento_uf == "AM" |
                                          favorecido_do_pagamento_uf == "AP" |
                                          favorecido_do_pagamento_uf == "CE" |
                                          favorecido_do_pagamento_uf == "DF" |
                                          favorecido_do_pagamento_uf == "GO" |
                                          favorecido_do_pagamento_uf == "MA" |
                                          favorecido_do_pagamento_uf == "MT" |
                                          favorecido_do_pagamento_uf == "PA" |
                                          favorecido_do_pagamento_uf == "PB" |
                                          favorecido_do_pagamento_uf == "RI" |
                                          favorecido_do_pagamento_uf == "RO" |
                                          favorecido_do_pagamento_uf == "AC" |
                                          favorecido_do_pagamento_uf == "PI" |
                                          favorecido_do_pagamento_uf == "RR" |
                                          favorecido_do_pagamento_uf == "TO" ~ "R3",
                                          
                                          favorecido_do_pagamento_uf == "AL" |
                                          favorecido_do_pagamento_uf == "BA" |
                                          favorecido_do_pagamento_uf == "CE" |
                                          favorecido_do_pagamento_uf == "MG" |
                                          favorecido_do_pagamento_uf == "PB" |
                                          favorecido_do_pagamento_uf == "PE" |
                                          favorecido_do_pagamento_uf == "RN" |
                                          favorecido_do_pagamento_uf == "SE" ~ "R4"
                                        )) %>% 
  dplyr::distinct() %>% 

  dplyr::group_by(nome_ente) %>% 
  dplyr::mutate(soma_ente = sum(empenhado)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(favorecido_do_pagamento_uf, emenda_numero_ano) %>% 
  dplyr::mutate(soma_Estado = sum(empenhado)) %>% 
  dplyr::ungroup()

################################################################################
### EXPORTANDO EM EXCEL
################################################################################

writexl::write_xlsx(dados_MERGE2,
                    "C:/Users/fernanda.kelly/Desktop/Projetos R/consultoria_Gabriela/consultoria_Gab/Dados/dados_MERGE2_0412.xlsx")

writexl::write_xlsx(a,
                    "C:/Users/fernanda.kelly/Desktop/Projetos R/consultoria_Gabriela/consultoria_Gab/Dados/baseDF_0412.xlsx")

################################################################################
### ATIVIDADE DAS REGIONAIS
################################################################################

Emendas_planilha_mãe_SOMA <- Emendas_planilha_mãe %>% 
  dplyr::filter(nome_emenda == "Emenda Individual" & transferencia_especial == "Sim") %>% 
  dplyr::group_by(uf) %>% 
  dplyr::mutate(soma_2 = base::sum(valor_2)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(codigo_siafi) %>% 
  dplyr::mutate(soma_4 = base::sum(valor_2)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(comercial) %>% 
  dplyr::mutate(soma_R = base::sum(valor_2)) %>% 
  dplyr::ungroup()
################################################################################
### EXPORTANDO EM EXCEL
################################################################################

writexl::write_xlsx(Emendas_planilha_mãe_SOMA,
                    "C:/Users/fernanda.kelly/Desktop/Projetos R/Maturidade Tecnológica São Paulo/MaturidadeTecnologica_SP/Dados/Emendas_planilha_mãe_SOMA_2111.xlsx")

################################################################################