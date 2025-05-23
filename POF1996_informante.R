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
  )


### Adicionar rendimento a outras bases
RENDIMENTOS_1996_agrupado <- RENDIMENTOS_1996c_rendimento %>% 
  group_by(ID) %>% 
  summarise(RENDIMENTO_BRUTO_UC = min(RENDIMENTO_BRUTO_UC), 
            TOTAL_DEDUCOES = min(TOTAL_DEDUCOES), 
            RENDIMENTO_LIQ_UC = min(RENDIMENTO_LIQ_UC))

RENDIMENTOS_1996_agrupado_percentil <- RENDIMENTOS_1996_agrupado %>% 
  mutate(PERCENTIL_RENDA = ntile(RENDIMENTO_BRUTO_UC, 100))


# DESPESA 90 DIAS - somente UC
DESPESA_90DIAS_1996c_rendimento <- DESPESA_90DIAS_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado_percentil %>%
      select(ID, RENDIMENTO_BRUTO_UC, TOTAL_DEDUCOES, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA),
    by = "ID", relationship = "many-to-many"
  )

# DESPESA 6 MESES - somente UC
DESPESA_6MESES_1996c_rendimento <- DESPESA_6MESES_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado_percentil %>%
      select(ID, RENDIMENTO_BRUTO_UC, TOTAL_DEDUCOES, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA),
    by = "ID", relationship = "many-to-many"
  )

#OUTRAS DESPESAS - SOMENTE UC
OUTRASDESPESAS_1996c_rendimento <- OUTRASDESPESAS_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado_percentil %>%
      select(ID, RENDIMENTO_BRUTO_UC, TOTAL_DEDUCOES, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA),
    by = "ID", relationship = "many-to-many"
  )


#SERVIÇOS DOMÉSTICOS - SOMENTE UC
SERV_DOMS_1996c_rendimento <- SERV_DOMS_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado_percentil %>%
      select(ID, RENDIMENTO_BRUTO_UC, TOTAL_DEDUCOES, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA),
    by = "ID", relationship = "many-to-many"
  )


# CADERNETA_COLETIVA

CADERNETA_COLETIVA_1996c_rendimento <- CADERNETA_COLETIVA_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado_percentil %>%
      select(ID, RENDIMENTO_BRUTO_UC, TOTAL_DEDUCOES, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA),
    by = "ID", relationship = "many-to-many"
  )


#DESPESA INDIVIDUAL - SOMENTE UC

DESPESA_INDIVIDUAL_1996c_rendimento <- DESPESA_INDIVIDUAL_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado_percentil %>%
      select(ID, RENDIMENTO_BRUTO_UC, TOTAL_DEDUCOES, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA),
    by = "ID", relationship = "many-to-many"
  )

#DESPESA COM VEÍCULOS - SOMENTE UC

DESP_VEICULOS_1996c_rendimento <- DESP_VEICULOS_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado_percentil %>%
      select(ID, RENDIMENTO_BRUTO_UC, TOTAL_DEDUCOES, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA),
    by = "ID", relationship = "many-to-many"
  )

#OUTROS RECEBIMENTOS - SOMENTE UC

OUTROS_RECEBIMENTOS_1996c_rendimento <- OUTROS_RECEBIMENTOS_1996 %>%
  left_join(
    RENDIMENTOS_1996_agrupado_percentil %>%
      select(ID, RENDIMENTO_BRUTO_UC, TOTAL_DEDUCOES, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA),
    by = "ID", relationship = "many-to-many"
  )


# Base DOMICILIO_1996 - pessoas por UC = Qtde de moradore sno domicílio / Qtde de UC's
pessoas_por_uc <- MORADOR_1996 %>%
  group_by(ID) %>%
  summarise(TAMANHO_FAMILIA = max(as.numeric(COD_INFORMANTE), na.rm = TRUE))

# Função para adicionar renda per capita
adicionar_renda_pc_uc <- function(base) {
  base %>%
    left_join(pessoas_por_uc, by = "ID") %>%
    mutate(RENDIMENTO_BRUTO_PC = RENDIMENTO_BRUTO_UC / TAMANHO_FAMILIA,
           RENDIMENTO_LIQ_PC = RENDIMENTO_LIQ_UC / TAMANHO_FAMILIA)
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

DESPESA_90DIAS_1996c_despesa <- DESPESA_90DIAS_1996c_rendimentopc %>%
  group_by(ID, COD_ITEM) %>%
  summarise(
    DESPESA_UC = sum(VALOR_DEFL_ANUALIZADO, na.rm = TRUE),
    RENDIMENTO_BRUTO_UC = min(RENDIMENTO_BRUTO_UC),
    RENDIMENTO_LIQ_UC = min(RENDIMENTO_LIQ_UC),
    FATOR_EXPANSAO = min(FATOR_EXPANSAO1),
    TAMANHO_FAMILIA = min(TAMANHO_FAMILIA),
    TIPO_REGISTRO = min(TIPO_REGISTRO),
    PERCENTIL_RENDA = min(PERCENTIL_RENDA)) %>% 
  mutate(COD_INFORMANTE = "00") %>% 
  select(ID, COD_INFORMANTE, COD_ITEM, TIPO_REGISTRO,TAMANHO_FAMILIA, FATOR_EXPANSAO, DESPESA_UC, 
         RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA)

DESPESA_6MESES_1996c_despesa <- DESPESA_6MESES_1996c_rendimentopc %>%
  group_by(ID, COD_ITEM) %>%
  summarise(
    DESPESA_UC = sum(VALOR_DEFL_ANUALIZADO, na.rm = TRUE),
    RENDIMENTO_BRUTO_UC = min(RENDIMENTO_BRUTO_UC),
    RENDIMENTO_LIQ_UC = min(RENDIMENTO_LIQ_UC),
    FATOR_EXPANSAO = min(FATOR_EXPANSAO1),
    TAMANHO_FAMILIA = min(TAMANHO_FAMILIA),
    TIPO_REGISTRO = min(TIPO_REGISTRO),
    PERCENTIL_RENDA = min(PERCENTIL_RENDA)) %>% 
  mutate(COD_INFORMANTE = "00") %>% 
  select(ID, COD_INFORMANTE, COD_ITEM, TIPO_REGISTRO,TAMANHO_FAMILIA, FATOR_EXPANSAO, DESPESA_UC, 
         RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA)

OUTRASDESPESAS_1996c_despesa <- OUTRASDESPESAS_1996c_rendimentopc %>%
  group_by(ID, COD_ITEM) %>%
  summarise(
    DESPESA_UC = sum(VALOR_DEFL_ANUALIZADO, na.rm = TRUE),
    RENDIMENTO_BRUTO_UC = min(RENDIMENTO_BRUTO_UC),
    RENDIMENTO_LIQ_UC = min(RENDIMENTO_LIQ_UC),
    FATOR_EXPANSAO = min(FATOR_EXPANSAO1),
    TAMANHO_FAMILIA = min(TAMANHO_FAMILIA),
    TIPO_REGISTRO = min(TIPO_REGISTRO),
    PERCENTIL_RENDA = min(PERCENTIL_RENDA)) %>% 
  mutate(COD_INFORMANTE = "00") %>% 
  select(ID, COD_INFORMANTE, COD_ITEM, TIPO_REGISTRO,TAMANHO_FAMILIA, FATOR_EXPANSAO, DESPESA_UC, 
         RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA)

SERV_DOMS_1996c_despesa <- SERV_DOMS_1996c_rendimentopc %>%
  group_by(ID, COD_ITEM) %>%
  summarise(
    DESPESA_UC = sum(VALOR_ANUALIZADO, na.rm = TRUE),
    RENDIMENTO_BRUTO_UC = min(RENDIMENTO_BRUTO_UC),
    RENDIMENTO_LIQ_UC = min(RENDIMENTO_LIQ_UC),
    FATOR_EXPANSAO = min(FATOR_EXPANSAO1),
    TAMANHO_FAMILIA = min(TAMANHO_FAMILIA),
    TIPO_REGISTRO = min(TIPO_REGISTRO),
    PERCENTIL_RENDA = min(PERCENTIL_RENDA)) %>% 
  mutate(COD_INFORMANTE = "00") %>% 
  select(ID, COD_INFORMANTE, COD_ITEM, TIPO_REGISTRO,TAMANHO_FAMILIA, FATOR_EXPANSAO, DESPESA_UC, 
         RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA)

CADERNETA_COLETIVA_1996c_despesa <- CADERNETA_COLETIVA_1996c_rendimentopc %>%
  group_by(ID, COD_INFORMANTE, COD_ITEM) %>%
  summarise(
    DESPESA_UC = sum(VALOR_ANUALIZADO, na.rm = TRUE),
    RENDIMENTO_BRUTO_UC = min(RENDIMENTO_BRUTO_UC),
    RENDIMENTO_LIQ_UC = min(RENDIMENTO_LIQ_UC),
    FATOR_EXPANSAO = min(FATOR_EXPANSAO1),
    TAMANHO_FAMILIA = min(TAMANHO_FAMILIA),
    TIPO_REGISTRO = min(TIPO_REGISTRO),
    PERCENTIL_RENDA = min(PERCENTIL_RENDA)) %>% 
  select(ID, COD_INFORMANTE, COD_ITEM, TIPO_REGISTRO,TAMANHO_FAMILIA, FATOR_EXPANSAO, DESPESA_UC, 
         RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA)

DESPESA_INDIVIDUAL_1996c_despesa <- DESPESA_INDIVIDUAL_1996c_rendimentopc %>%
  group_by(ID, COD_INFORMANTE, COD_ITEM) %>%
  summarise(
    DESPESA_UC = sum(VALOR_ANUALIZADO, na.rm = TRUE),
    RENDIMENTO_BRUTO_UC = min(RENDIMENTO_BRUTO_UC),
    RENDIMENTO_LIQ_UC = min(RENDIMENTO_LIQ_UC),
    FATOR_EXPANSAO = min(FATOR_EXPANSAO1),
    TAMANHO_FAMILIA = min(TAMANHO_FAMILIA),
    TIPO_REGISTRO = min(TIPO_REGISTRO),
    PERCENTIL_RENDA = min(PERCENTIL_RENDA)) %>% 
  select(ID, COD_INFORMANTE, COD_ITEM, TIPO_REGISTRO,TAMANHO_FAMILIA, FATOR_EXPANSAO, DESPESA_UC, 
         RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA)


DESP_VEICULOS_1996c_despesa <- DESP_VEICULOS_1996c_rendimentopc %>%
  group_by(ID, COD_INFORMANTE, COD_ITEM) %>%
  summarise(
    DESPESA_UC = sum(VALOR_ANUALIZADO, na.rm = TRUE),
    RENDIMENTO_BRUTO_UC = min(RENDIMENTO_BRUTO_UC),
    RENDIMENTO_LIQ_UC = min(RENDIMENTO_LIQ_UC),
    FATOR_EXPANSAO = min(FATOR_EXPANSAO1),
    TAMANHO_FAMILIA = min(TAMANHO_FAMILIA),
    TIPO_REGISTRO = min(TIPO_REGISTRO),
    PERCENTIL_RENDA = min(PERCENTIL_RENDA)) %>% 
  select(ID, COD_INFORMANTE, COD_ITEM, TIPO_REGISTRO,TAMANHO_FAMILIA, FATOR_EXPANSAO, DESPESA_UC, 
         RENDIMENTO_BRUTO_UC, RENDIMENTO_LIQ_UC, PERCENTIL_RENDA)



saveRDS(DOMICILIO_1996, "DOMICILIO_1996.rds")
saveRDS(MORADOR_1996, "MORADOR_1996.rds")
saveRDS(DESPESA_90DIAS_1996c_despesa, "DESPESA_90DIAS_1996.rds")
saveRDS(DESPESA_6MESES_1996c_despesa, "DESPESA_6MESES_1996.rds")
saveRDS(BENSDURAVEIS_1996, "BENSDURAVEIS_1996.rds")
saveRDS(OUTRASDESPESAS_1996c_despesa, "OUTRASDESPESAS_1996.rds")
saveRDS(SERV_DOMS_1996c_despesa, "SERV_DOMS_1996.rds")
saveRDS(CADERNETA_COLETIVA_1996c_despesa, "CADERNETA_COLETIVA_1996.rds")
saveRDS(DESPESA_INDIVIDUAL_1996c_despesa, "DESPESA_INDIVIDUAL_1996.rds")
saveRDS(DESP_VEICULOS_1996c_despesa, "DESP_VEICULOS_1996.rds")
saveRDS(RENDIMENTOS_1996c_rendimentopc, "RENDIMENTOS_1996.rds")
saveRDS(OUTROS_RECEBIMENTOS_1996c_rendimentopc, "OUTROS_RECEBIMENTOS_1996.rds")


############



DESPESAS_TOTAIS <- rbind(DESPESA_90DIAS_1996c_despesa,DESPESA_6MESES_1996c_despesa,OUTRASDESPESAS_1996c_despesa,
                         SERV_DOMS_1996c_despesa,CADERNETA_COLETIVA_1996c_despesa,DESPESA_INDIVIDUAL_1996c_despesa,
                         DESP_VEICULOS_1996c_despesa)

View(DESPESAS_TOTAIS)

## ÍNDICE DE GINI
GINI_RENDA <- ineq::Gini(DESPESAS_TOTAIS$RENDIMENTO_BRUTO_UC)
GINI_DESPESA <- ineq::Gini(DESPESAS_TOTAIS$DESPESA_UC)

cat("Índice de Gini - Renda per capita:", round(GINI_RENDA, 4), "\n")
cat("Índice de Gini - Despesa total:", round(GINI_DESPESA, 4), "\n")

DISTRIBUICAO_PERCENTIL <- DESPESAS_TOTAIS %>%
  group_by(PERCENTIL_RENDA) %>%
  summarise(
    RENDA_MEDIA_FAMILIAR = weighted.mean(RENDIMENTO_BRUTO_UC, FATOR_EXPANSAO, na.rm = TRUE)/100,
    RENDA_MEDIA_PC = weighted.mean((RENDIMENTO_BRUTO_UC / TAMANHO_FAMILIA), FATOR_EXPANSAO, na.rm = TRUE)/100,
    DESPESA_MEDIA_PC = weighted.mean((DESPESA_UC / TAMANHO_FAMILIA), FATOR_EXPANSAO, na.rm = TRUE)/100
  )

View(DISTRIBUICAO_PERCENTIL)
DISTRIBUICAO_UF <- DESPESAS_TOTAIS %>%
  group_by(UF = substr(ID, 1, 2)) %>%
  summarise(
    RENDA_MEDIA_FAMILIAR = weighted.mean(RENDIMENTO_BRUTO_UC, FATOR_EXPANSAO, na.rm = TRUE)/100,
    RENDA_MEDIA_PC = weighted.mean((RENDIMENTO_BRUTO_UC / TAMANHO_FAMILIA), FATOR_EXPANSAO, na.rm = TRUE)/100,
    DESPESA_MEDIA_PC = weighted.mean((DESPESA_UC / TAMANHO_FAMILIA), FATOR_EXPANSAO, na.rm = TRUE)/100
  )


View(DISTRIBUICAO_UF)

saveRDS(DESPESAS_TOTAIS, "DESPESA_TOTAL_1996_INFORMANTE.rds")
