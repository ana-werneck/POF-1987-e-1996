# Instalar os pacotes
pacotes <- c(
  "dplyr", 
  "matrixStats", 
  "modi", 
  "ggplot2", 
  "openxlsx", 
  "stringr", 
  "Hmisc", 
  "extrafont",
  "tidyr"
)

for(pacote in pacotes) {
  if (!requireNamespace(pacote, quietly = TRUE)) {
    install.packages(pacote)
  }
}


library(dplyr)
library(matrixStats)
library(modi)
library(ggplot2)
library(openxlsx)
library(stringr)
library(Hmisc)
library(extrafont)
library(tidyr)


# Adicionando as bases
DOMICILIO_1996 <- readRDS("domicilio_1996.rds")
MORADOR_1996 <- readRDS("morador_1996.rds")
DESPESA_90DIAS_1996 <- readRDS("despesa_90dias_1996.rds")
DESPESA_6MESES_1996 <- readRDS("despesa_6meses_1996.rds")
BENSDURAVEIS_1996 <- readRDS("bensduraveis_1996.rds")
OUTRASDESPESAS_1996 <- readRDS("outrasdespesas_1996.rds")
SERV_DOMS_1996 <- readRDS("serv_doms_1996.rds")
CADERNETA_COLETIVA_1996 <- readRDS("caderneta_coletiva_1996.rds")
DESPESA_INDIVIDUAL_1996 <- readRDS("despesa_individual_1996.rds")
DESP_VEICULOS_1996 <- readRDS("desp_veiculos_1996.rds")
RENDIMENTOS_1996 <- readRDS("rendimentos_1996.rds")
OUTROS_RECEBIMENTOS_1996 <- readRDS("outros_recebimentos_1996.rds")
############

# Criando ID
DOMICILIO_1996 <- DOMICILIO_1996 %>%
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

MORADOR_1996 <- MORADOR_1996 %>%
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

DESPESA_90DIAS_1996 <- DESPESA_90DIAS_1996 %>% 
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

DESPESA_6MESES_1996 <- DESPESA_6MESES_1996 %>% 
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

BENSDURAVEIS_1996 <- BENSDURAVEIS_1996 %>% 
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

OUTRASDESPESAS_1996 <- OUTRASDESPESAS_1996 %>% 
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

SERV_DOMS_1996 <- SERV_DOMS_1996 %>% 
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

CADERNETA_COLETIVA_1996 <- CADERNETA_COLETIVA_1996 %>% 
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

DESPESA_INDIVIDUAL_1996 <- DESPESA_INDIVIDUAL_1996 %>% 
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

DESP_VEICULOS_1996 <- DESP_VEICULOS_1996 %>% 
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

RENDIMENTOS_1996 <- RENDIMENTOS_1996 %>% 
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))

OUTROS_RECEBIMENTOS_1996 <- OUTROS_RECEBIMENTOS_1996 %>% 
  mutate(ID = paste(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""))


### CHECAGEM DE GASTOS COM ALUGUEL
domicilios_alugados <- DOMICILIO_1996 %>%
  filter(COND_OCUP_DOM == 5) %>%
  select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM) %>%
  distinct()

recebimentos_filtrados <- DESPESA_6MESES_1996 %>%
  semi_join(domicilios_alugados, by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM"))

table(recebimentos_filtrados$COD_ITEM)

### ADICIONANDO COLUNAS DE RENDIMENTO 

RENDIMENTOS_1996c_rendimento <- RENDIMENTOS_1996 %>%
  group_by(ID) %>%
  mutate(
    RENDIMENTO_BRUTO_UC = sum(VALOR_1_ANUALIZACAO_RECEBIDO, na.rm = TRUE),
    TOTAL_DEDUCOES = sum(
      VALOR_2_ANUALIZACAO_IMPOSTO_DE_RENDA,
      VALOR_3_ANUALIZACAO_PREVIDENCIA_PUBLICA,
      VALOR_4_ANUALIZACAO_OUTRAS_DEDUCOES,
      na.rm = TRUE
    ),
    RENDIMENTO_LIQ_UC = RENDIMENTO_BRUTO_UC - TOTAL_DEDUCOES
  ) %>%
  ungroup()


RENDIMENTOS_1996_agrupado <- RENDIMENTOS_1996c_rendimento %>% 
  group_by(ID) %>% 
  summarise(RENDIMENTO_BRUTO_UC = min(RENDIMENTO_BRUTO_UC), RENDIMENTO_LIQ_UC = min(RENDIMENTO_LIQ_UC))

### Adicionar rendimento a outras bases

# DESPESA 90 DIAS - somente UC
DESPESA_90DIAS_1996c_rendimento <- DESPESA_90DIAS_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado %>%
      select(ID, RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC),
    by = "ID", relationship = "many-to-many"
  )

# DESPESA 6 MESES - somente UC
DESPESA_6MESES_1996c_rendimento <- DESPESA_6MESES_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado %>%
      select(ID, RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC),
    by = "ID", relationship = "many-to-many"
  )

#OUTRAS DESPESAS - SOMENTE UC
OUTRASDESPESAS_1996c_rendimento <- OUTRASDESPESAS_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado %>%
      select(ID, RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC),
    by = "ID", relationship = "many-to-many"
  )


#SERVIÇOS DOMÉSTICOS - SOMENTE UC
SERV_DOMS_1996c_rendimento <- SERV_DOMS_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado %>%
      select(ID, RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC),
    by = "ID", relationship = "many-to-many"
  )


# CADERNETA_COLETIVA

CADERNETA_COLETIVA_1996c_rendimento <- CADERNETA_COLETIVA_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado %>%
      select(ID, RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC),
    by = "ID", relationship = "many-to-many"
  )


#DESPESA INDIVIDUAL - SOMENTE UC

DESPESA_INDIVIDUAL_1996c_rendimento <- DESPESA_INDIVIDUAL_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado %>%
      select(ID, RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC),
    by = "ID", relationship = "many-to-many"
  )

#DESPESA COM VEÍCULOS - SOMENTE UC

DESP_VEICULOS_1996c_rendimento <- DESP_VEICULOS_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado %>%
      select(ID, RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC),
    by = "ID", relationship = "many-to-many"
  )

#OUTROS RECEBIMENTOS - SOMENTE UC

OUTROS_RECEBIMENTOS_1996c_rendimento <- OUTROS_RECEBIMENTOS_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado %>%
      select(ID, RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC),
    by = "ID", relationship = "many-to-many"
  )



###### RENDIMENTO PER CAPITA

# Base DOMICILIO_1996 - pessoas por UC = Qtde de moradore sno domicílio / Qtde de UC's
pessoas_por_uc <- MORADOR_1996 %>%
  group_by(ID) %>%
  summarise(PESSOAS_POR_UC = max(as.numeric(COD_INFORMANTE), na.rm = TRUE))

# Função para adicionar renda per capita
adicionar_renda_pc_uc <- function(base) {
  base %>%
    left_join(pessoas_por_uc, by = "ID") %>%
    mutate(RENDIMENTO_BRUTO_PC = RENDIMENTO_BRUTO_UC / PESSOAS_POR_UC,
           RENDIMENTO_LIQ_PC = RENDIMENTO_LIQ_UC / PESSOAS_POR_UC)
}

DESPESA_90DIAS_1996c_rendimentopc <- adicionar_renda_pc_uc(DESPESA_90DIAS_1996c_rendimento)
DESPESA_6MESES_1996c_rendimentopc <- adicionar_renda_pc_uc(DESPESA_6MESES_1996c_rendimento)
OUTRASDESPESAS_1996c_rendimentopc <- adicionar_renda_pc_uc(OUTRASDESPESAS_1996c_rendimento)
SERV_DOMS_1996c_rendimentopc <- adicionar_renda_pc_uc(SERV_DOMS_1996c_rendimento)
CADERNETA_COLETIVA_1996c_rendimentopc <- adicionar_renda_pc_uc(CADERNETA_COLETIVA_1996c_rendimento)
DESPESA_INDIVIDUAL_1996c_rendimentopc <- adicionar_renda_pc_uc(DESPESA_INDIVIDUAL_1996c_rendimento)
DESP_VEICULOS_1996c_rendimentopc <- adicionar_renda_pc_uc(DESP_VEICULOS_1996c_rendimento)
RENDIMENTOS_1996c_rendimentopc <- adicionar_renda_pc_uc(RENDIMENTOS_1996c_rendimento)
OUTROS_RECEBIMENTOS_1996c_rendimentopc <- adicionar_renda_pc_uc(OUTROS_RECEBIMENTOS_1996c_rendimento)

###########

# Somando as despesas por UC em cada base

DESPESA_90DIAS_1996c_despesapc <- DESPESA_90DIAS_1996c_rendimentopc %>%
  group_by(ID) %>%
  summarise(
    DESPESA_PC_90DIAS = sum(VALOR_DEFL_ANUALIZADO, na.rm = TRUE) / min(PESSOAS_POR_UC),
    RENDIMENTO_BRUTO_PC_90DIAS = min(RENDIMENTO_BRUTO_PC),
    RENDIMENTO_LIQ_PC_90DIAS = min(RENDIMENTO_LIQ_PC),
    FATOR_EXPANSAO1_90DIAS = min(FATOR_EXPANSAO1)
  ) 

DESPESA_6MESES_1996c_despesapc <- DESPESA_6MESES_1996c_rendimentopc %>%
  group_by(ID) %>%
  summarise(
    DESPESA_PC_6MESES = sum(VALOR_DEFL_ANUALIZADO, na.rm = TRUE) / min(PESSOAS_POR_UC),
    RENDIMENTO_BRUTO_PC_6MESES = min(RENDIMENTO_BRUTO_PC),
    RENDIMENTO_LIQ_PC_6MESES = min(RENDIMENTO_LIQ_PC),
    FATOR_EXPANSAO1_6MESES = min(FATOR_EXPANSAO1)
  ) 

OUTRASDESPESAS_1996c_despesapc <- OUTRASDESPESAS_1996c_rendimentopc %>%
  group_by(ID) %>%
  summarise(
    DESPESA_PC_OUTRAS = sum(VALOR_DEFL_ANUALIZADO, na.rm = TRUE) / min(PESSOAS_POR_UC),
    RENDIMENTO_BRUTO_PC_OUTRAS = min(RENDIMENTO_BRUTO_PC),
    RENDIMENTO_LIQ_PC_OUTRAS = min(RENDIMENTO_LIQ_PC),
    FATOR_EXPANSAO1_OUTRAS = min(FATOR_EXPANSAO1)
  ) 

SERV_DOMS_1996c_despesapc <- SERV_DOMS_1996c_rendimentopc %>%
  group_by(ID) %>%
  summarise(
    DESPESA_PC_SERV_DOMS = sum(VALOR_ANUALIZADO, na.rm = TRUE) / min(PESSOAS_POR_UC),
    RENDIMENTO_BRUTO_PC_SERV_DOMS = min(RENDIMENTO_BRUTO_PC),
    RENDIMENTO_LIQ_PC_SERV_DOMS = min(RENDIMENTO_LIQ_PC),
    FATOR_EXPANSAO1_SERV_DOMS = min(FATOR_EXPANSAO1)
  ) 

CADERNETA_COLETIVA_1996c_despesapc <- CADERNETA_COLETIVA_1996c_rendimentopc %>%
  group_by(ID) %>%
  summarise(
    DESPESA_PC_CADERNETA_COLETIVA = sum(VALOR_ANUALIZADO, na.rm = TRUE) / min(PESSOAS_POR_UC),
    RENDIMENTO_BRUTO_PC_CADERNETA_COLETIVA = min(RENDIMENTO_BRUTO_PC),
    RENDIMENTO_LIQ_PC_CADERNETA_COLETIVA = min(RENDIMENTO_LIQ_PC),
    FATOR_EXPANSAO1_CADERNETA_COLETIVA = min(FATOR_EXPANSAO1)
  ) 

DESPESA_INDIVIDUAL_1996c_despesapc <- DESPESA_INDIVIDUAL_1996c_rendimentopc %>%
  group_by(ID) %>%
  summarise(
    DESPESA_PC_INDIVIDUAL = sum(VALOR_ANUALIZADO, na.rm = TRUE) / min(PESSOAS_POR_UC),
    RENDIMENTO_BRUTO_PC = first(RENDIMENTO_BRUTO_PC),
    RENDIMENTO_LIQ_PC = min(RENDIMENTO_LIQ_PC),
    FATOR_EXPANSAO1 = min(FATOR_EXPANSAO1),
    RENDIMENTO_BRUTO_UC = first(RENDIMENTO_BRUTO_UC),
    RENDIMENTO_LIQ_UC = first(RENDIMENTO_BRUTO_UC)
  )

DESP_VEICULOS_1996c_despesapc <- DESP_VEICULOS_1996c_rendimentopc %>%
  group_by(ID) %>%
  summarise(
    DESPESA_PC_VEICULOS = sum(VALOR_ANUALIZADO, na.rm = TRUE) / min(PESSOAS_POR_UC),
    RENDIMENTO_BRUTO_PC_VEICULOS = first(RENDIMENTO_BRUTO_PC),
    RENDIMENTO_LIQ_PC_VEICULOS = min(RENDIMENTO_LIQ_PC),
    FATOR_EXPANSAO1_VEICULOS = min(FATOR_EXPANSAO1)
  ) 



saveRDS(DOMICILIO_1996, "DOMICILIO_1996.rds")
saveRDS(MORADOR_1996, "MORADOR_1996.rds")
saveRDS(DESPESA_90DIAS_1996c_despesapc, "DESPESA_90DIAS_1996.rds")
saveRDS(DESPESA_6MESES_1996c_despesapc, "DESPESA_6MESES_1996.rds")
saveRDS(BENSDURAVEIS_1996, "BENSDURAVEIS_1996.rds")
saveRDS(OUTRASDESPESAS_1996c_despesapc, "OUTRASDESPESAS_1996.rds")
saveRDS(SERV_DOMS_1996c_despesapc, "SERV_DOMS_1996.rds")
saveRDS(CADERNETA_COLETIVA_1996c_despesapc, "CADERNETA_COLETIVA_1996.rds")
saveRDS(DESPESA_INDIVIDUAL_1996c_despesapc, "DESPESA_INDIVIDUAL_1996.rds")
saveRDS(DESP_VEICULOS_1996c_despesapc, "DESP_VEICULOS_1996.rds")
saveRDS(RENDIMENTOS_1996c_rendimentopc, "RENDIMENTOS_1996.rds")
saveRDS(OUTROS_RECEBIMENTOS_1996c_rendimentopc, "OUTROS_RECEBIMENTOS_1996.rds")


############



DESPESAS_TOTAIS <- DESPESA_INDIVIDUAL_1996c_despesapc %>% 
  left_join(
    DESPESA_90DIAS_1996c_despesapc,
    by = "ID"
  ) %>%
  left_join(
    DESPESA_6MESES_1996c_despesapc,
    by = "ID"
  ) %>% 
  left_join(
    OUTRASDESPESAS_1996c_despesapc,
    by = "ID"
  ) %>% 
  left_join(
    SERV_DOMS_1996c_despesapc,
    by = "ID"
  ) %>% 
  left_join(
    CADERNETA_COLETIVA_1996c_despesapc,
    by = "ID"
  ) %>% 
  left_join(
    DESP_VEICULOS_1996c_despesapc,
    by = "ID"
  ) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
  select(ID, FATOR_EXPANSAO1, RENDIMENTO_BRUTO_UC, RENDIMENTO_BRUTO_PC, RENDIMENTO_LIQ_UC, RENDIMENTO_LIQ_PC, 
         DESPESA_PC_INDIVIDUAL, DESPESA_PC_90DIAS, DESPESA_PC_6MESES, DESPESA_PC_OUTRAS,
         DESPESA_PC_SERV_DOMS, DESPESA_PC_CADERNETA_COLETIVA, DESPESA_PC_VEICULOS)

DESPESAS_TOTAIS <- DESPESAS_TOTAIS %>% 
  mutate(DESPESA_TOTAL_PC = DESPESA_PC_INDIVIDUAL + DESPESA_PC_90DIAS + DESPESA_PC_6MESES + DESPESA_PC_OUTRAS +
           DESPESA_PC_SERV_DOMS + DESPESA_PC_CADERNETA_COLETIVA + DESPESA_PC_VEICULOS)

despesa_total <- DESPESAS_TOTAIS %>% 
  mutate(PERCENTIL_RENDA = ntile(RENDIMENTO_BRUTO_PC,100))


## ÍNDICE DE GINI
GINI_RENDA <- ineq::Gini(DESPESAS_TOTAIS$RENDIMENTO_BRUTO_PC)
GINI_DESPESA <- ineq::Gini(DESPESAS_TOTAIS$DESPESA_TOTAL_PC)

cat("Índice de Gini - Renda per capita:", round(GINI_RENDA, 4), "\n")
cat("Índice de Gini - Despesa total:", round(GINI_DESPESA, 4), "\n")

#DISTRIBUICAO_PERCENTIL <- DESPESAS_TOTAIS %>%
#  group_by(PERCENTIL_RENDA) %>%
#  summarise(
#    RENDA_MEDIA_FAMILIAR = weighted.mean(RENDIMENTO_UC, FATOR_EXPANSAO1, na.rm = TRUE)/100,
#    RENDA_MEDIA_PC = weighted.mean(RENDIMENTO_PC, FATOR_EXPANSAO1, na.rm = TRUE)/100,
#    DESPESA_MEDIA_PC = weighted.mean(DESPESA_PC_TOTAL, FATOR_EXPANSAO1, na.rm = TRUE)/100
#  )

#DISTRIBUICAO_UF <- DESPESAS_TOTAIS %>%
#  group_by(COD_UF) %>%
#  summarise(
#    RENDA_MEDIA_FAMILIAR = weighted.mean(RENDIMENTO_UC, FATOR_EXPANSAO1, na.rm = TRUE)/100,
#    RENDA_MEDIA_PC = weighted.mean(RENDIMENTO_PC, FATOR_EXPANSAO1, na.rm = TRUE)/100,
#    DESPESA_MEDIA_PC = weighted.mean(DESPESA_PC_TOTAL, FATOR_EXPANSAO1, na.rm = TRUE)/100
#  )


#View(DISTRIBUICAO_UF)

saveRDS(despesa_total, "DESPESA_TOTAL_1996_ID.rds")
