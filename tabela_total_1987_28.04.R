library(tidyverse)

############# TRAZENDO OS CINcO DFs ############# 


tabela_total_um <- readRDS("tabela_total_um_1987.rds")

tabela_total_dois <- readRDS("tabela_total_dois_1987.rds")

tabela_total_tres <- readRDS("tabela_total_tres_1987.rds")

tabela_total_quatro <- readRDS("tabela_total_quatro_1987.rds")

tabela_total_cinco <- readRDS("tabela_total_cinco_1987.rds")

############# TRATANDO OS DADOS E RENOMEANDO QUATRO E CINCO ############# 

tabela_total_um <- as.data.frame(lapply(tabela_total_um, as.numeric)) 
tabela_total_dois <- as.data.frame(lapply(tabela_total_dois, as.numeric))
tabela_total_tres <- as.data.frame(lapply(tabela_total_tres, as.numeric))
tabela_total_quatro <- as.data.frame(lapply(tabela_total_quatro, as.numeric)) # PARA DESPESAS GERAIS #
tabela_total_cinco <- as.data.frame(lapply(tabela_total_cinco, as.numeric)) # PARA DESPESAS COM BENS DURAVEIS #

ver_duplicadas <- tabela_total_quatro[duplicated(tabela_total_quatro), ]

tabela_total_quatro <- tabela_total_quatro %>%
  rename(
    TIPO_DE_DOMICILIO = V20,
    AREA_GEOGRAFICA = V60,
    PERIODO_REAL = V70,
    NUMERO_DE_DOMICILIO = V100,
    UNIDADE_DE_CONSUMO = V130,
    INFORMANTE = V140,
    QUESTIONARIO = V150,
    AUXILIAR_1 = V170,
    AUXILIAR_2 = V180,
    ESTRATO_DA_AMOSTRA = V190,
    PESO_AREA_METROPOLITANA = V210,
    PESO_MUNICIPIO_CAPITAL = V220,
    
    CODIGO_DO_ITEM = V4030,
    FATOR_DE_ANUALIZACAO = V4110,
    LOCAL_DE_COMPRA_POF_3 = V4120,
    ESPECIE_DO_SERVICO_DOMESTICO = V4130,
    VALOR_DEFLACIONADO_DO_ITEM = V4170,
    RELACAO_COM_O_CHEFE = V4180,
    CONDICAO_DE_PRESENCA = V4190,
    SEXO = V4200,
    IDADE_CALCULADA = V4210,  
    FREQUENCIA_A_ESCOLA = V4220,
    NIVEL_DE_INSTRUCAO = V4230,
    DESPESA_MENSAL_FAMILIAR = V4240,
    ITEM_TABULACAO_DESPESA_1 = V4250,
    ITEM_TABULACAO_DESPESA_2 = V4260,
    ITEM_TABULACAO_DESPESA_3 = V4270,
    ITEM_TABULACAO_DESPESA_4 = V4280,
    LOCAL_DE_COMPRA_TABELA = V4290,
    FONTE_DE_RENDIMENTO = V4300,
    FONTE_DE_RENDIMENTO_DEDUCOES = V4310
  )

tabela_total_cinco <- tabela_total_cinco %>%
  rename(
    TIPO_DE_DOMICILIO = V20,
    AREA_GEOGRAFICA = V60,
    PERIODO_REAL = V70,
    NUMERO_DE_DOMICILIO = V100,
    UNIDADE_DE_CONSUMO = V130,
    INFORMANTE = V140,
    QUESTIONARIO = V150,
    AUXILIAR_1 = V170,
    AUXILIAR_2 = V180,
    ESTRATO_DA_AMOSTRA = V190,
    PESO_AREA_METROPOLITANA = V210,
    PESO_MUNICIPIO_CAPITAL = V220,
    
    CODIGO_DO_ITEM = V5030,
    NUMERO_CARTOES_DE_CREDITO = V5100,
    MES_DE_AQUISICAO = V5110,
    MES_UNIDADE_DE_CONSUMO_POF2 = V5120,
    FATOR_DE_ANUALIZACAO = V5130,
    NUMERO_DE_CHEQUES_ESPECIAIS = V5140,
    ANO_DE_AQUISICAO = V5150,
    FORMA_DE_AQUISICAO_DO_BEM = V5190,
    ESTADO_DO_BEM = V5200,
    VALOR_DO_ITEM_DEFLACIONADO = V5240,
    TOTAL_AQUISICAO_DEFLACIONADO = V5250,
    RELACAO_COM_O_CHEFE = V5260,
    CONDICAO_DE_PRESENCA = V5270,
    SEXO = V5280,
    IDADE_CALCULADA = V5290,
    FREQUENTA_ESCOLA = V5300,
    NIVEL_DE_INSTRUCAO = V5310,
    DESPESA_MENSAL_FAMILIAR = V5320,
    ITEM_TABULACAO_DESPESA_1 = V5330,
    ITEM_TABULACAO_DESPESA_2 = V5340
  )

###### ADICIONANDO RENDA FAMILIAR NAS TABELAS DE DESPESA (QUATRO E CINCO) ###### 

tabela_total_dois_subset <- tabela_total_dois %>% select(V60, V100, V130, V2260, V2160) %>% 
  rename(
    AREA_GEOGRAFICA = V60,
    NUMERO_DE_DOMICILIO = V100,
    UNIDADE_DE_CONSUMO = V130,
    RENDA_FAMILIAR = V2260,
    N_FAMILIA = V2160) %>%
  group_by(AREA_GEOGRAFICA, NUMERO_DE_DOMICILIO, UNIDADE_DE_CONSUMO) %>%
  summarise(
    RENDA_FAMILIAR = first(RENDA_FAMILIAR),  # ou sum(), depende do seu caso
    N_FAMILIA = first(N_FAMILIA),
    .groups = "drop"
  )

tabela_total_quatro_c_renda <- tabela_total_quatro %>% 
  left_join(
    tabela_total_dois_subset, 
    by = c("AREA_GEOGRAFICA", "NUMERO_DE_DOMICILIO", "UNIDADE_DE_CONSUMO"), 
    relationship = "many-to-many") %>% 
    mutate(RENDA_PC = RENDA_FAMILIAR/N_FAMILIA)

df_quatro <- tabela_total_quatro_c_renda %>%
  mutate(QUADRO_ITEM = substr(CODIGO_DO_ITEM, 1, 2)) %>%
  mutate(RENDIMENTOS_DEDUCOES = if_else(QUADRO_ITEM %in% c("53", "54", "55", "56"), 1, 0)) %>% # indica se o item contempla rendimento ou deducacao
  mutate(DECIL_RENDA_PC = ntile(RENDA_PC, 10))  

tabela_total_cinco_c_renda <- tabela_total_cinco %>% 
  left_join(
    tabela_total_dois_subset, 
    by = c("AREA_GEOGRAFICA", "NUMERO_DE_DOMICILIO", "UNIDADE_DE_CONSUMO"), 
    relationship = "many-to-many") %>% 
  mutate(RENDA_PC = RENDA_FAMILIAR/N_FAMILIA)

df_cinco <- tabela_total_cinco_c_renda %>%
  mutate(QUADRO_ITEM = substr(CODIGO_DO_ITEM, 1, 2)) %>%
  mutate(DECIL_RENDA_PC = ntile(RENDA_PC, 10))  

df_cinco %>% group_by(QUADRO_ITEM) %>% summarise(VALOR_DO_ITEM_DEFLACIONADO = sum(VALOR_DO_ITEM_DEFLACIONADO))

View(df_cinco %>% filter(QUADRO_ITEM == 21))

### RENDA MÉDIA E QUANTIDADE DE MORADORES POR REGIÃO ### 

tabela_estat <- df_quatro %>% group_by(AREA_GEOGRAFICA, NUMERO_DE_DOMICILIO, UNIDADE_DE_CONSUMO) %>% summarise(RENDA_FAMILIAR = max(RENDA_FAMILIAR), peso = min(PESO_AREA_METROPOLITANA), decil = min(DECIL_RENDA_PC)) 

estat_RMs <- tabela_estat %>% group_by(AREA_GEOGRAFICA) %>% summarise(quantidade_familias = sum(peso), renda_media = weighted.mean(RENDA_FAMILIAR, peso)/100)

estat_decis <- tabela_estat %>% group_by(decil) %>% summarise(quantidade_familias = sum(peso), renda_media = weighted.mean(RENDA_FAMILIAR, peso)/100)


# A RENDA FAMILIAR FOI DIVIDIDA POR CEM POIS O NUMERO CONTA COM DUAS CASAS DECIMAIS

### DESPESA POR DECIL ###

despesa_uc <- df_quatro %>% filter(RENDIMENTOS_DEDUCOES == 0) %>%
  group_by(AREA_GEOGRAFICA, NUMERO_DE_DOMICILIO, UNIDADE_DE_CONSUMO) %>%
  summarise(
    DESPESA_TOTAL_UC = sum(DESPESA_MENSAL_FAMILIAR, na.rm = TRUE),
    peso = min(PESO_AREA_METROPOLITANA),
    .groups = "drop"
  )

despesa_uc_com_decil <- despesa_uc %>%
  left_join(
    df_quatro %>%
      group_by(AREA_GEOGRAFICA, NUMERO_DE_DOMICILIO, UNIDADE_DE_CONSUMO) %>%
      summarise(
        RENDA_FAMILIAR = min(RENDA_FAMILIAR),
        DECIL_RENDA_PC = min(DECIL_RENDA_PC),
        .groups = "drop"
      ),
    by = c("AREA_GEOGRAFICA", "NUMERO_DE_DOMICILIO", "UNIDADE_DE_CONSUMO")
  )


# Média da despesa por decil
despesa_media_por_decil <- despesa_uc_com_decil %>%
  group_by(DECIL_RENDA_PC) %>%
  summarise(
    DESPESA_MEDIA = weighted.mean(DESPESA_TOTAL_UC, peso,na.rm = TRUE)/100,
    RENDA_MÉDIA = weighted.mean(RENDA_FAMILIAR, peso, na.rm = TRUE)/100
  )

# Média da despesa por RM

despesa_media_por_RM <- despesa_uc_com_decil %>%
  group_by(AREA_GEOGRAFICA) %>%
  summarise(
    DESPESA_MEDIA = weighted.mean(DESPESA_TOTAL_UC, peso,na.rm = TRUE)/100,
    RENDA_MÉDIA = weighted.mean(RENDA_FAMILIAR, peso, na.rm = TRUE)/100
  )


