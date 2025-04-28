##### PRIMEIRA PARTE CONSISTE NA COMPOSIÇÃO DA BASE DE DADOS 
#### TRATAMENTO COMEÇA A PARTIR DA LINHA 686

#arquivos <- c(
#  "~/Downloads/dadospof1996/BA4x.txt",
#  "~/Downloads/dadospof1996/CE4x.txt",
#  "~/Downloads/dadospof1996/DF4x.txt",
#  "~/Downloads/dadospof1996/GO4x.txt",
#  "~/Downloads/dadospof1996/MG4x.txt",
#  "~/Downloads/dadospof1996/PA4x.txt",
#  "~/Downloads/dadospof1996/PE4x.txt",
#  "~/Downloads/dadospof1996/PR4x.txt",
#  "~/Downloads/dadospof1996/RJ4x.txt",
#  "~/Downloads/dadospof1996/RS4x.txt",
#  "~/Downloads/dadospof1996/SP4x.txt"
#)


#arquivos <- c(
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/BA4x.txt",
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/CE4x.txt",
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/DF4x.txt",
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/GO4x.txt",
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/MG4x.txt",
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/PA4x.txt",
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/PE4x.txt",
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/PR4x.txt",
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/RJ4x.txt",
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/RS4x.txt",
#  "C:/Users/ana.werneck/Downloads/pof1996/Dados/SP4x.txt"
#)

# Inicializar lista para armazenar as linhas por tipo
listas_separadas <- vector("list", 12) 
names(listas_separadas) <- sprintf("%02d", 1:12)

for (arquivo in arquivos) {
  cat("Processando arquivo:", arquivo, "\n")  
  linhas <- readLines(arquivo, warn = FALSE)  
  
  for (linha in linhas) {
    if (nchar(linha) >= 2) {  
      tipo_registro <- substr(linha, 1, 2)  
      
      tipo_numero <- suppressWarnings(as.numeric(tipo_registro))
      if (!is.na(tipo_numero) && tipo_numero >= 1 && tipo_numero <= 12) {
        listas_separadas[[sprintf("%02d", tipo_numero)]] <- c(listas_separadas[[sprintf("%02d", tipo_numero)]], linha)
      } else {
        cat("Linha ignorada (tipo inválido):", linha, "\n")  
      }
    } else {
      cat("Linha ignorada (muito curta):", linha, "\n")  
    }
  }
}

# Verificar listas separadas
str(listas_separadas)  

View(listas_separadas)

####

#####tipo 1 - DOMICILIOS
posicoes_iniciais <- c(1, 3, 5, 7, 10, 11, 13, 14, 16, 18, 19, 21, 23, 24, 25, 26, 27, 28, 29, 31, 33, 39)
posicoes_finais <- c(posicoes_iniciais[-1] - 1, 44) 
nomes_variaveis <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ",
                     "DV_SEQ", "NUM_DOM", "NUM_UC", "PERIODO_REAL_COLETA", "QTDE_MORADORES", 
                     "TIPO_DOM", "QTDE_COMODOS", "COMODOS_DORMITORIO", "TIPO_ABAST_AGUA", 
                     "TIPO_ESCOAD_SANITARIO", "COND_OCUP_DOM", "CONTRATO_DOCUMENTADO", 
                     "PERIOCICDADE_DO_REAJUSTE", "TEMPO_MORADIA", "NUMERO_DA_UNIDADE_DE_CONSUMO", 
                     "ESTRATO_DA_CLASSIFICACAO_DE_RENDA_DO_SETOR", "FATOR_DE_EXPANSAO_DA_AREA", 
                     "FATOR_DE_EXPANSAO_DO_MUNICIPIO_CAPITAL")

separar_por_posicoes <- function(linha, posicoes_iniciais, posicoes_finais, nomes_variaveis) {
  resultado <- list()
  for (i in seq_along(nomes_variaveis)) {
    resultado[[nomes_variaveis[i]]] <- substr(linha, posicoes_iniciais[i], posicoes_finais[i])
  }
  return(resultado)
}

tipo01 <- listas_separadas[["01"]]

tipo01_separado <- lapply(tipo01, separar_por_posicoes, posicoes_iniciais, posicoes_finais, nomes_variaveis)

DOMICILIO_1996 <- do.call(rbind, lapply(tipo01_separado, as.data.frame))

#Salvando arquivo
write.table(DOMICILIO_1996, "tipo01_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)

View(DOMICILIO_1996)

#####tipo2- MORADORES
posicoes_iniciais <- c(1, 3, 5, 7, 10, 11, 13, 14, 16, 18, 19, 20, 21, 22, 24, 25, 26, 29, 30, 32, 33)
posicoes_finais <- c(posicoes_iniciais[-1] - 1, 34) 
nomes_variaveis <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", 
                     "DV_SEQ", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "NUM_QUADRO",
                     "COD_REL_PESSOA_REF", "COD_CONDICAO_PRESENCA", "SEXO_PESSOA", 
                     "FREQ_ESCOLA", "NIVEL_INSTRUCAO", "ORCAMENTO_DESPESA", 
                     "ORCAMENTO_RENDIMENTO", "IDADE_CALCULADA", "TEM_CARTAO_CREDITO", 
                     "QUANTOS_CARTOES_DE_CREDITO", "TEM_CHEQUE_ESPECIAL", 
                     "QTDE_CHEQUES_ESPECIAIS")

separar_por_posicoes <- function(linha, posicoes_iniciais, posicoes_finais, nomes_variaveis) {
  resultado <- list()
  for (i in seq_along(nomes_variaveis)) {
    resultado[[nomes_variaveis[i]]] <- substr(linha, posicoes_iniciais[i], posicoes_finais[i])
  }
  return(resultado)
}

tipo02 <- listas_separadas[["02"]]

tipo02_separado <- lapply(tipo02, separar_por_posicoes, posicoes_iniciais, posicoes_finais, nomes_variaveis)

MORADOR_1996 <- do.call(rbind, lapply(tipo02_separado, as.data.frame))

#Salvando arquivo
write.table(MORADOR_1996, "tipo02_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)

View(MORADOR_1996)


#####tipo3 - Despesas de 90 dias do questionário de despesa coletiva (POF 2)
posicoes_iniciais <- c(1, 3, 5, 7, 10, 11, 13, 14, 18, 19, 23, 33, 35, 39, 43, 43, 46, 56, 66, 81)
posicoes_finais <- c(posicoes_iniciais[-1] - 1, 95) 
nomes_variaveis <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ",
                     "DV_SEQ", "NUM_DOM", "NUM_UC", "COD_ITEM", "DV_DO_CODIGO_DA_DESPESA", 
                     "QTD_CONSUMIDA_ORIGINAL", "VALOR_DO_ITEM", "COD_LOCAL_COMPRA", 
                     "DEFLATOR", "CODIGO_CRITICA", "COD_IMPUTACAO", "FATOR_ANUALIZACAO", 
                     "VALOR_DEFLACIONADO", "VALOR_DEFL_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

separar_por_posicoes <- function(linha, posicoes_iniciais, posicoes_finais, nomes_variaveis) {
  resultado <- list()
  for (i in seq_along(nomes_variaveis)) {
    resultado[[nomes_variaveis[i]]] <- substr(linha, posicoes_iniciais[i], posicoes_finais[i])
  }
  return(resultado)
}

tipo03 <- listas_separadas[["03"]]

tipo03_separado <- lapply(tipo03, separar_por_posicoes, posicoes_iniciais, posicoes_finais, nomes_variaveis)

DESPESA_90DIAS_1996 <- do.call(rbind, lapply(tipo03_separado, as.data.frame))

###ajuste da base

# Criando coluna NUM_QUADRO que define a categoria do produto
DESPESA_90DIAS_1996$NUM_QUADRO <- substr(DESPESA_90DIAS_1996$COD_ITEM, 1, 2)

# Converter colunas numéricas
colunas_a_converter <- c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                         "VALOR_DEFL_ANUALIZADO", "VALOR_EXPANDIDO_1", 
                         "VALOR_EXPANDIDO_2")
for (coluna in colunas_a_converter) {
  DESPESA_90DIAS_1996[[coluna]] <- as.numeric(DESPESA_90DIAS_1996[[coluna]])
}

# Calcular o FATOR_EXPANSAO1 (evitar divisão por 0)
DESPESA_90DIAS_1996$FATOR_EXPANSAO1 <- DESPESA_90DIAS_1996$VALOR_EXPANDIDO_1 / 
  DESPESA_90DIAS_1996$VALOR_DEFL_ANUALIZADO

# Acrescentar valores decimais
colunas_para_dividir <- c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                          "VALOR_DEFL_ANUALIZADO", "VALOR_EXPANDIDO_1", 
                          "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_dividir) {
  DESPESA_90DIAS_1996[[coluna]] <- DESPESA_90DIAS_1996[[coluna]] / 100
}

# Visualizar o dataframe
View(DESPESA_90DIAS_1996)

write.table(DESPESA_90DIAS_1996, "tipo03_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)

####

######tipo4 - Despesas de 6 meses do questionário de despesa coletiva (POF 2)
posicoes_iniciais <- c(1, 3, 5, 7, 10, 11, 13, 14, 18, 19, 29, 31, 32, 36, 38, 40, 43, 53, 63, 78)
posicoes_finais <- c(posicoes_iniciais[-1] - 1, 92)  
nomes_variaveis <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", 
                     "COD_ITEM", "DV_CODIGO_DESPESA", "VALOR_DO_ITEM", "MES_GASTO", "QTD_MESES", 
                     "DEFLATOR", "CODIGO_CRITICA", "COD_IMPUTACAO", "FATOR_ANUALIZACAO", "FATOR_DEFLACIONADO", 
                     "VALOR_DEFL_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

separar_por_posicoes <- function(linha, posicoes_iniciais, posicoes_finais, nomes_variaveis) {
  resultado <- list()
  for (i in seq_along(nomes_variaveis)) {
    resultado[[nomes_variaveis[i]]] <- substr(linha, posicoes_iniciais[i], posicoes_finais[i])
  }
  return(resultado)
}

tipo04 <- listas_separadas[["04"]]

tipo04_separado <- lapply(tipo04, separar_por_posicoes, posicoes_iniciais, posicoes_finais, nomes_variaveis)

DESPESA_6MESES_1996 <- do.call(rbind, lapply(tipo04_separado, as.data.frame))


#####base com ajustes

# Criando coluna NUM_QUADRO que define a categoria do produto
DESPESA_6MESES_1996$NUM_QUADRO <- substr(DESPESA_6MESES_1996$COD_ITEM, 1, 2)


# Converter colunas numéricas
colunas_a_converter <- c("VALOR_DO_ITEM", "DEFLATOR", "FATOR_DEFLACIONADO", 
                         "VALOR_DEFL_ANUALIZADO", "VALOR_EXPANDIDO_1", 
                         "VALOR_EXPANDIDO_2")

for (coluna in colunas_a_converter) {
  DESPESA_6MESES_1996[[coluna]] <- as.numeric(DESPESA_6MESES_1996[[coluna]])
}

# Calculando os fatores de expansão
DESPESA_6MESES_1996$FATOR_EXPANSAO1 <- DESPESA_6MESES_1996$VALOR_EXPANDIDO_1 / DESPESA_6MESES_1996$VALOR_DEFL_ANUALIZADO
DESPESA_6MESES_1996$FATOR_EXPANSAO2 <- DESPESA_6MESES_1996$VALOR_EXPANDIDO_2 / DESPESA_6MESES_1996$VALOR_DEFL_ANUALIZADO


# Acrescentar valores decimais
colunas_para_dividir <- c("VALOR_DO_ITEM", "DEFLATOR", "FATOR_DEFLACIONADO", 
                          "VALOR_DEFL_ANUALIZADO", "VALOR_EXPANDIDO_1", 
                          "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_dividir) {
  DESPESA_6MESES_1996[[coluna]] <- DESPESA_6MESES_1996[[coluna]] / 100
}

View(DESPESA_6MESES_1996)

# Salvando em um arquivo
write.table(DESPESA_6MESES_1996, "tipo04_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)


#####

###tipo 5 - Inventário de bens duráveis do questionário de despesa coletiva (POF 2)
posicoes_iniciais <- c(1, 3, 5, 7, 10, 11, 13, 14, 18, 19, 20, 22, 24, 25, 27)
posicoes_finais <- c(posicoes_iniciais[-1] - 1, 28)  
nomes_variaveis <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", "DV_SEQ", 
                     "NUM_DOM", "NUM_UC", "COD_ITEM", "DV_CODIGO_DESPESA", "ESTADO_ULTIMA_AQUISICAO", 
                     "QUANTIDADE_BEM", "ANO_ULTIMA_AQUISICAO", "FORMA_ULTIMA_AQUISICAO", "CODIGO_CRITICA", 
                     "COD_IMPUTACAO")

separar_por_posicoes <- function(linha, posicoes_iniciais, posicoes_finais, nomes_variaveis) {
  resultado <- list()
  for (i in seq_along(nomes_variaveis)) {
    resultado[[nomes_variaveis[i]]] <- substr(linha, posicoes_iniciais[i], posicoes_finais[i])
  }
  return(resultado)
}

tipo05 <- listas_separadas[["05"]]

tipo05_separado <- lapply(tipo05, separar_por_posicoes, posicoes_iniciais, posicoes_finais, nomes_variaveis)

BENSDURAVEIS_1996 <- do.call(rbind, lapply(tipo05_separado, as.data.frame))

#####base com ajustes

# Criando coluna NUM_QUADRO que define a categoria do produto
BENSDURAVEIS_1996$NUM_QUADRO <- substr(BENSDURAVEIS_1996$COD_ITEM, 1, 2)

# Salvando o arquivo ajustado
write.table(BENSDURAVEIS_1996, "tipo05_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# Exibindo as primeiras linhas
View(BENSDURAVEIS_1996)


######


#####tipo 6 - Outras despesas do questionário de despesa coletiva (POF 2)
posicoes_iniciais <- c(1, 3, 5, 7, 10, 11, 13, 14, 18, 19, 20, 22, 24, 25, 35, 37, 41, 43, 45, 48, 58, 68, 83)
posicoes_finais <- c(posicoes_iniciais[-1] - 1, 97) 
nomes_variaveis <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", 
                     "COD_ITEM", "DV_CODIGO_DESPESA", "COD_ESTADO_AQUISICAO", "MES_AQUISICAO", "ANO_AQUISICAO", 
                     "COD_FORMA_OBTENCAO", "VALOR_DO_ITEM", "COD_LOCAL_COMPRA", "DEFLATOR", 
                     "CODIGO_CRITICA", "COD_IMPUTACAO", "FATOR_ANUALIZACAO", "VALOR_DEFLACIONADO", 
                     "VALOR_DEFL_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

separar_por_posicoes <- function(linha, posicoes_iniciais, posicoes_finais, nomes_variaveis) {
  resultado <- list()
  for (i in seq_along(nomes_variaveis)) {
    resultado[[nomes_variaveis[i]]] <- substr(linha, posicoes_iniciais[i], posicoes_finais[i])
  }
  return(resultado)
}

tipo06 <- listas_separadas[["06"]]

tipo06_separado <- lapply(tipo06, separar_por_posicoes, posicoes_iniciais, posicoes_finais, nomes_variaveis)

OUTRASDESPESAS_1996 <- do.call(rbind, lapply(tipo06_separado, as.data.frame))

###


###base com ajustes
# Definições de posição

# Ajuste para separar NUM_QUADRO
OUTRASDESPESAS_1996$NUM_QUADRO <- substr(OUTRASDESPESAS_1996$COD_ITEM, 1, 2)

# Conversão para numérico (evitando erros com strings vazias)
colunas_para_converter <- c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                          "VALOR_DEFL_ANUALIZADO", "VALOR_EXPANDIDO_1", 
                          "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_converter) {
  OUTRASDESPESAS_1996[[coluna]] <- as.numeric(OUTRASDESPESAS_1996[[coluna]])
}

# Calculando os fatores de expansão
OUTRASDESPESAS_1996$FATOR_EXPANSAO1 <- OUTRASDESPESAS_1996$VALOR_EXPANDIDO_1 / OUTRASDESPESAS_1996$VALOR_DEFL_ANUALIZADO
OUTRASDESPESAS_1996$FATOR_EXPANSAO2 <- OUTRASDESPESAS_1996$VALOR_EXPANDIDO_2 / OUTRASDESPESAS_1996$VALOR_DEFL_ANUALIZADO

# Acrescentar valores decimais
colunas_para_dividir <- c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                          "VALOR_DEFL_ANUALIZADO", "VALOR_EXPANDIDO_1", 
                          "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_dividir) {
  OUTRASDESPESAS_1996[[coluna]] <- OUTRASDESPESAS_1996[[coluna]] / 100
}


# Salvando o arquivo ajustado
write.table(OUTRASDESPESAS_1996, "tipo06_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# Exibindo as primeiras linhas
View(OUTRASDESPESAS_1996)

####


# tipo 07 - Despesaas com serviços domésticos do questionário de despesa coletiva (POF 2)
### tipo 7 possui um problema. O tamanho da lista é 10 caracteres menor do que o que aponta o dicionário,
### isso resulta na omissão de caracteres na variável valor expandido 2. INSS
### A solução que encontramos foi calcular esse valor com base no dicionário

posicoes_iniciais_07 <- c(1, 3, 5, 7, 10, 11, 13, 14, 18, 19, 21, 23, 24, 34, 44, 
                          46, 47, 51, 54, 64, 74, 76, 78, 88, 103, 118, 128, 143)
posicoes_finais_07 <- c(posicoes_iniciais_07[-1] - 1, 157)  

nomes_variaveis_07 <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", 
                        "COD_ITEM", "DV_CODIGO_DESPESA", "CODIGO_CRITICA", "COD_IMPUTACAO", "COD_ESPECIE", 
                        "VALOR_DO_ITEM", "VALOR_INSS", "MES_GASTO", "NUM_MESES", "DEFLATOR", 
                        "FATOR_ANUALIZACAO", "VALOR_DEFLACIONADO", "VALOR_1_DEFLACIONADO", "CODIGO_CRITICA_INSS", 
                        "COD_IMPUTACAO_INSS", "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2", 
                        "VALOR_DEFL_ANUAL_INSS", "VALOR_EXPANDIDO_1_INSS", "VALOR_EXPANDIDO_2_INSS")

separar_por_posicoes <- function(linha, posicoes_iniciais, posicoes_finais, nomes_variaveis) {
  resultado <- list()
  for (i in seq_along(nomes_variaveis)) {
    resultado[[nomes_variaveis[i]]] <- substr(linha, posicoes_iniciais[i], posicoes_finais[i])
  }
  return(resultado)
}

tipo07 <- listas_separadas[["07"]]

tipo07_separado <- lapply(tipo07, separar_por_posicoes, posicoes_iniciais_07, posicoes_finais_07, nomes_variaveis_07)

SERV_DOMS_1996 <- do.call(rbind, lapply(tipo07_separado, as.data.frame))

###base ajustada

# Ajuste para separar NUM_QUADRO e COD_ITEM
SERV_DOMS_1996$NUM_QUADRO <- substr(SERV_DOMS_1996$COD_ITEM, 1, 2)

# Conversão para numérico (evitando erros com strings vazias)
colunas_para_converter <- c("VALOR_DO_ITEM","VALOR_INSS", "DEFLATOR", "VALOR_DEFLACIONADO", 
                            "VALOR_1_DEFLACIONADO", "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", 
                            "VALOR_EXPANDIDO_2", "VALOR_DEFL_ANUAL_INSS", "VALOR_EXPANDIDO_1_INSS", 
                            "VALOR_EXPANDIDO_2_INSS")

for (coluna in colunas_para_converter) {
  SERV_DOMS_1996[[coluna]] <- as.numeric(SERV_DOMS_1996[[coluna]])
}

# Criando fatores de expansão apenas se as colunas necessárias existirem
SERV_DOMS_1996$FATOR_EXPANSAO1 <- SERV_DOMS_1996$VALOR_EXPANDIDO_1 / SERV_DOMS_1996$VALOR_ANUALIZADO
SERV_DOMS_1996$FATOR_EXPANSAO2 <- SERV_DOMS_1996$VALOR_EXPANDIDO_2 / SERV_DOMS_1996$VALOR_ANUALIZADO

#Calculando a última coluna com dígitos faltantes
SERV_DOMS_1996$VALOR_EXPANDIDO_2_INSS <- SERV_DOMS_1996$FATOR_EXPANSAO2 * SERV_DOMS_1996$VALOR_DEFL_ANUAL_INSS

# Acrescentar valores decimais - coluna com caractere a menos fica com uma casa decimal a menos
colunas_para_dividir <- c("VALOR_DO_ITEM","VALOR_INSS", "DEFLATOR", "VALOR_DEFLACIONADO", 
                          "VALOR_1_DEFLACIONADO", "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", 
                          "VALOR_EXPANDIDO_2", "VALOR_DEFL_ANUAL_INSS", "VALOR_EXPANDIDO_1_INSS", "VALOR_EXPANDIDO_2_INSS")

for (coluna in colunas_para_dividir) {
  SERV_DOMS_1996[[coluna]] <- SERV_DOMS_1996[[coluna]] / 100
}

# Salvando o arquivo ajustado
write.table(SERV_DOMS_1996, "tipo07_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# Exibindo as primeiras linhas
View(SERV_DOMS_1996)

###




# tipo 08 - Caderneta de despesas (alimentação, higiene e limpeza) (POF 3)
posicoes_iniciais_08 <- c(1, 3, 5, 7, 10, 11, 13, 14, 16, 18, 22, 23, 25, 27, 32, 35, 45, 48, 52, 55, 65, 75, 90)

posicoes_finais_08 <- c(posicoes_iniciais_08[-1] - 1, 104)

nomes_variaveis_08 <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", 
                        "COD_INFORMANTE", "NUM_QUADRO", "COD_ITEM", "DV_CODIGO_DESPESA", "CODIGO_CRITICA", "COD_IMPUTACAO", 
                        "QUANT_ITEM", "COD_UNIDADE_MEDIDA", "VALOR_DO_ITEM", "COD_LOCAL_COMPRA", "DEFLATOR", 
                        "FATOR_ANUALIZACAO", "VALOR_DEFLACIONADO", "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", 
                        "VALOR_EXPANDIDO_2")

tipo08 <- listas_separadas[["08"]]

tipo08_separado <- lapply(tipo08, separar_por_posicoes, posicoes_iniciais_08, posicoes_finais_08, nomes_variaveis_08)

CADERNETA_COLETIVA_1996 <- do.call(rbind, lapply(tipo08_separado, as.data.frame))


###base ajustada

# Conversão para numérico (evitando erros com strings vazias)
colunas_para_converter <- c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                            "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_converter) {
  CADERNETA_COLETIVA_1996[[coluna]] <- as.numeric(CADERNETA_COLETIVA_1996[[coluna]])
}


# Criando fatores de expansão, garantindo que divisões por zero resultem em 0, não NA
CADERNETA_COLETIVA_1996$FATOR_EXPANSAO1 <- CADERNETA_COLETIVA_1996$VALOR_EXPANDIDO_1 / CADERNETA_COLETIVA_1996$VALOR_ANUALIZADO
CADERNETA_COLETIVA_1996$FATOR_EXPANSAO2 <- CADERNETA_COLETIVA_1996$VALOR_EXPANDIDO_2 / CADERNETA_COLETIVA_1996$VALOR_ANUALIZADO

# Acrescentar valores decimais
colunas_para_dividir <- c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                          "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_dividir) {
  CADERNETA_COLETIVA_1996[[coluna]] <- CADERNETA_COLETIVA_1996[[coluna]] / 100
}

View(CADERNETA_COLETIVA_1996)

# Salvando o arquivo ajustado
write.table(CADERNETA_COLETIVA_1996, "tipo08_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)


###


# tipo 09 - Despesas do questionário de despesas individual (POF 4)
posicoes_iniciais_09 <- c(1, 3, 5, 7, 10, 11, 13, 14, 16, 20, 21, 31, 33, 35, 39, 41, 43, 46, 56, 66, 81)
posicoes_finais_09 <- c(posicoes_iniciais_09[-1] - 1, 95)
nomes_variaveis_09 <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "COD_ITEM", 
                        "DV_CODIGO_DESPESA", "VALOR_DO_ITEM", "COD_LOCAL_COMPRA", "MES_GASTO", "DEFLATOR", "CODIGO_CRITICA", 
                        "COD_IMPUTACAO", "FATOR_ANUALIZACAO", "VALOR_DEFLACIONADO", "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

tipo09 <- listas_separadas[["09"]]

tipo09_separado <- lapply(tipo09, separar_por_posicoes, posicoes_iniciais_09, posicoes_finais_09, nomes_variaveis_09)

DESPESA_INDIVIDUAL_1996 <- do.call(rbind, lapply(tipo09_separado, as.data.frame))

# Ajustando colunas
DESPESA_INDIVIDUAL_1996$NUM_QUADRO <- substr(DESPESA_INDIVIDUAL_1996$COD_ITEM, 1, 2)

# Convertendo colunas para numérico
colunas_para_converter <- c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                            "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_converter) {
  DESPESA_INDIVIDUAL_1996[[coluna]] <- as.numeric(DESPESA_INDIVIDUAL_1996[[coluna]])
}

# Calculando os fatores de expansão
DESPESA_INDIVIDUAL_1996$FATOR_EXPANSAO1 <- DESPESA_INDIVIDUAL_1996$VALOR_EXPANDIDO_1 / DESPESA_INDIVIDUAL_1996$VALOR_ANUALIZADO
DESPESA_INDIVIDUAL_1996$FATOR_EXPANSAO2 <- DESPESA_INDIVIDUAL_1996$VALOR_EXPANDIDO_2 / DESPESA_INDIVIDUAL_1996$VALOR_ANUALIZADO

# Acrescentar valores decimais
colunas_para_dividir <- c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                          "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_dividir) {
  DESPESA_INDIVIDUAL_1996[[coluna]] <- as.numeric(DESPESA_INDIVIDUAL_1996[[coluna]])
  DESPESA_INDIVIDUAL_1996[[coluna]] <- DESPESA_INDIVIDUAL_1996[[coluna]] / 100
}

View(DESPESA_INDIVIDUAL_1996)

#Salvando arquivo
write.table(DESPESA_INDIVIDUAL_1996, "tipo09_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)


###


# tipo 10 - Despesas com veículo individual (POF4)
posicoes_iniciais_10 <- c(1, 3, 5, 7, 10, 11, 13, 14, 16, 20, 21, 22, 24, 26, 27, 37, 39, 43, 45, 47, 50, 60, 70, 85)
posicoes_finais_10 <- c(posicoes_iniciais_10[-1] - 1, 99)
nomes_variaveis_10 <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", 
                       "COD_INFORMANTE", "COD_ITEM", "DV_CODIGO_DESPESA", "COD_ESTADO_AQUISICAO", "MES_AQUISICAO", 
                       "ANO_AQUISICAO", "COD_FORMA_OBTENCAO", "VALOR_DO_ITEM", "COD_LOCAL_COMPRA", 
                       "DEFLATOR", "CODIGO_CRITICA", "COD_IMPUTACAO", "FATOR_ANUALIZACAO", "VALOR_DEFLACIONADO", 
                       "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

tipo10 <- listas_separadas[["10"]]

tipo10_separado <- lapply(tipo10, separar_por_posicoes, posicoes_iniciais_10, posicoes_finais_10, nomes_variaveis_10)

DESP_VEICULOS_1996 <- do.call(rbind, lapply(tipo10_separado, as.data.frame))

### base ajustada

# Ajuste para separar NUM_QUADRO e COD_ITEM
DESP_VEICULOS_1996$NUM_QUADRO <- substr(DESP_VEICULOS_1996$COD_ITEM, 1, 2)

# Conversão para numérico (evitando erros com strings vazias)
colunas_para_converter <-c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                           "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_converter) {
  DESP_VEICULOS_1996[[coluna]] <- as.numeric(DESP_VEICULOS_1996[[coluna]])
}

# Criando fatores de expansão, garantindo que divisões por zero resultem em 0, não NA
DESP_VEICULOS_1996$FATOR_EXPANSAO1 <- DESP_VEICULOS_1996$VALOR_EXPANDIDO_1 / DESP_VEICULOS_1996$VALOR_ANUALIZADO
DESP_VEICULOS_1996$FATOR_EXPANSAO2 <- DESP_VEICULOS_1996$VALOR_EXPANDIDO_2 / DESP_VEICULOS_1996$VALOR_ANUALIZADO


# Acrescentar valores decimais
colunas_para_dividir <- c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                          "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_dividir) {
  DESP_VEICULOS_1996[[coluna]] <- as.numeric(DESP_VEICULOS_1996[[coluna]])
  DESP_VEICULOS_1996[[coluna]] <- DESP_VEICULOS_1996[[coluna]] / 100
}

View(DESP_VEICULOS_1996)

# Salvando o arquivo ajustado
write.table(DESP_VEICULOS_1996, "tipo10_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)



###


# tipo 11 - Rendimento e deduções individual (POF 5)
posicoes_iniciais_11 <- c(1, 3, 5, 7, 10, 11, 13, 14, 16, 20, 21, 23, 24, 34, 44, 54, 64, 68, 70, 72, 75, 85, 95, 110, 125, 135, 145, 160, 175, 185, 195, 210, 225, 235, 245,260)
posicoes_finais_11 <- c(posicoes_iniciais_11[-1] - 1, 274)
nomes_variaveis_11 <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "COD_ITEM",
                        "DV_CODIGO_DESPESA", "MES_GASTO", "NUM_MESES", "ULTIMO_RECEBIMENTO", "VALOR_IR", "VALOR_PREVIDENCIA_PUBLICA", "VALOR_OUTRAS_DEDUCOES",
                        "DEFLATOR", "CODIGO_CRITICA", "COD_IMPUTACAO", "FATOR_ANUALIZACAO", "VALOR_1_DEFLACIONADO_RECEBIDO", "VALOR_1_ANUALIZACAO_RECEBIDO",
                        "VALOR_1_EXPANDIDO_1_RECEBIMENTO", "VALOR_1_EXPANDIDO_2_RECEBIMENTO", "VALOR_2_DEFLACIONADO_IMPOSTO_DE_RENDA", "VALOR_2_ANUALIZACAO_IMPOSTO_DE_RENDA",
                        "VALOR_2_EXPANDIDO_1_IMPOSTO_DE_RENDA", "VALOR_2_EXPANDIDO_2_IMPOSTO_DE_RENDA", "VALOR_3_DEFLACIONADO_PREVIDENCIA_PUBLICA",
                        "VALOR_3_ANUALIZACAO_PREVIDENCIA_PUBLICA", "VALOR_3_EXPANDIDO_1_PREVIDENCIA_PUBLICA", "VALOR_3_EXPANDIDO_2_PREVIDENCIA_PUBLICA",
                        "VALOR_4_DEFLACIONADO_OUTRAS_DEDUCOES", "VALOR_4_ANUALIZACAO_OUTRAS_DEDUCOES", "VALOR_4_EXPANDIDO_1_OUTRAS_DEDUCOES",
                        "VALOR_4_EXPANDIDO_2_OUTRAS_DEDUCOES")

tipo11 <- listas_separadas[["11"]]
tipo11_separado <- lapply(tipo11, separar_por_posicoes, posicoes_iniciais_11, posicoes_finais_11, nomes_variaveis_11)
RENDIMENTOS_1996 <- do.call(rbind, lapply(tipo11_separado, as.data.frame))

# Convertendo as variáveis para numérico
colunas_para_converter <-c("ULTIMO_RECEBIMENTO", "VALOR_IR", "VALOR_PREVIDENCIA_PUBLICA", 
                           "VALOR_OUTRAS_DEDUCOES", "DEFLATOR","VALOR_1_DEFLACIONADO_RECEBIDO", 
                           "VALOR_1_ANUALIZACAO_RECEBIDO", "VALOR_1_EXPANDIDO_1_RECEBIMENTO", 
                           "VALOR_1_EXPANDIDO_2_RECEBIMENTO", "VALOR_2_DEFLACIONADO_IMPOSTO_DE_RENDA", 
                           "VALOR_2_ANUALIZACAO_IMPOSTO_DE_RENDA", "VALOR_2_EXPANDIDO_1_IMPOSTO_DE_RENDA",
                           "VALOR_2_EXPANDIDO_2_IMPOSTO_DE_RENDA", "VALOR_3_DEFLACIONADO_PREVIDENCIA_PUBLICA",
                           "VALOR_3_ANUALIZACAO_PREVIDENCIA_PUBLICA", "VALOR_3_EXPANDIDO_1_PREVIDENCIA_PUBLICA", 
                           "VALOR_3_EXPANDIDO_2_PREVIDENCIA_PUBLICA","VALOR_4_DEFLACIONADO_OUTRAS_DEDUCOES", 
                           "VALOR_4_ANUALIZACAO_OUTRAS_DEDUCOES", "VALOR_4_EXPANDIDO_1_OUTRAS_DEDUCOES",
                           "VALOR_4_EXPANDIDO_2_OUTRAS_DEDUCOES")

for (coluna in colunas_para_converter) {
  RENDIMENTOS_1996[[coluna]] <- as.numeric(RENDIMENTOS_1996[[coluna]])
}

# Calculando FATOR_EXPANSAO1 e FATOR_EXPANSAO2
RENDIMENTOS_1996$FATOR_EXPANSAO1 <- RENDIMENTOS_1996$VALOR_1_EXPANDIDO_1_RECEBIMENTO / RENDIMENTOS_1996$VALOR_1_ANUALIZACAO_RECEBIDO
RENDIMENTOS_1996$FATOR_EXPANSAO2 <- RENDIMENTOS_1996$VALOR_1_EXPANDIDO_2_RECEBIMENTO / RENDIMENTOS_1996$VALOR_1_ANUALIZACAO_RECEBIDO

# Acrescentar valores decimais
colunas_para_dividir <- c("ULTIMO_RECEBIMENTO", "VALOR_IR", "VALOR_PREVIDENCIA_PUBLICA", 
                          "VALOR_OUTRAS_DEDUCOES", "DEFLATOR","VALOR_1_DEFLACIONADO_RECEBIDO", 
                          "VALOR_1_ANUALIZACAO_RECEBIDO", "VALOR_1_EXPANDIDO_1_RECEBIMENTO", 
                          "VALOR_1_EXPANDIDO_2_RECEBIMENTO", "VALOR_2_DEFLACIONADO_IMPOSTO_DE_RENDA", 
                          "VALOR_2_ANUALIZACAO_IMPOSTO_DE_RENDA", "VALOR_2_EXPANDIDO_1_IMPOSTO_DE_RENDA",
                          "VALOR_2_EXPANDIDO_2_IMPOSTO_DE_RENDA", "VALOR_3_DEFLACIONADO_PREVIDENCIA_PUBLICA",
                          "VALOR_3_ANUALIZACAO_PREVIDENCIA_PUBLICA", "VALOR_3_EXPANDIDO_1_PREVIDENCIA_PUBLICA", 
                          "VALOR_3_EXPANDIDO_2_PREVIDENCIA_PUBLICA","VALOR_4_DEFLACIONADO_OUTRAS_DEDUCOES", 
                          "VALOR_4_ANUALIZACAO_OUTRAS_DEDUCOES", "VALOR_4_EXPANDIDO_1_OUTRAS_DEDUCOES")

colunas_para_dividir_10 <- c("VALOR_4_EXPANDIDO_2_OUTRAS_DEDUCOES")

for (coluna in colunas_para_dividir) {
  RENDIMENTOS_1996[[coluna]] <- RENDIMENTOS_1996[[coluna]] / 100
}

for (coluna in colunas_para_dividir_10) {
  RENDIMENTOS_1996[[coluna]] <- RENDIMENTOS_1996[[coluna]] / 10
}

View(RENDIMENTOS_1996)

# Salvando o arquivo
write.table(RENDIMENTOS_1996, "tipo11_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)


###



# tipo 12 - Outros recebimentos e movimentação financeira individual (POF 5)
posicoes_iniciais_12 <- c(1, 3, 5, 7, 10, 11, 13, 14, 16, 20, 21, 23, 33, 37, 39, 41, 44, 54, 64, 79)
posicoes_finais_12 <- c(posicoes_iniciais_12[-1] - 1, 93)
nomes_variaveis_12 <- c("TIPO_REGISTRO", "REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", 
                        "COD_INFORMANTE", "COD_ITEM", "DV_CODIGO_DESPESA", "MES_GASTO", "VALOR_DO_ITEM", 
                        "DEFLATOR", "CODIGO_CRITICA", "CODIGO_IMPUTACAO", "FATOR_ANUALIZACAO", 
                        "VALOR_DEFLACIONADO", "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

tipo12 <- listas_separadas[["12"]]
tipo12_separado <- lapply(tipo12, separar_por_posicoes, posicoes_iniciais_12, posicoes_finais_12, nomes_variaveis_12)
OUTROS_RECEBIMENTOS_1996 <- do.call(rbind, lapply(tipo12_separado, as.data.frame))

### Ajustando base de dados
# Conversão para numérico 
colunas_para_converter <- c("VALOR_DO_ITEM", "DEFLATOR", "FATOR_ANUALIZACAO", 
                            "VALOR_DEFLACIONADO", "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_converter) {
  OUTROS_RECEBIMENTOS_1996[[coluna]] <- as.numeric(OUTROS_RECEBIMENTOS_1996[[coluna]])
}

# Calculando FATOR_EXPANSAO1 e FATOR_EXPANSAO2
OUTROS_RECEBIMENTOS_1996$FATOR_EXPANSAO1 <- OUTROS_RECEBIMENTOS_1996$VALOR_EXPANDIDO_1 / OUTROS_RECEBIMENTOS_1996$VALOR_ANUALIZADO
OUTROS_RECEBIMENTOS_1996$FATOR_EXPANSAO2 <- OUTROS_RECEBIMENTOS_1996$VALOR_EXPANDIDO_2 / OUTROS_RECEBIMENTOS_1996$VALOR_ANUALIZADO


# Acrescentar valores decimais
colunas_para_dividir <- c("VALOR_DO_ITEM", "DEFLATOR", "VALOR_DEFLACIONADO", 
                          "VALOR_ANUALIZADO", "VALOR_EXPANDIDO_1", "VALOR_EXPANDIDO_2")

for (coluna in colunas_para_dividir) {
  OUTROS_RECEBIMENTOS_1996[[coluna]] <- OUTROS_RECEBIMENTOS_1996[[coluna]] / 100
}

View(OUTROS_RECEBIMENTOS_1996)


# Salvando o arquivo ajustado
write.table(OUTROS_RECEBIMENTOS_1996, "tipo12_separado.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# Exibindo as primeiras linhas
View(OUTROS_RECEBIMENTOS_1996)

#salvando as bases em rds 
saveRDS(DOMICILIO_1996, "DOMICILIO_1996.rds")
saveRDS(MORADOR_1996, "MORADOR_1996.rds")
saveRDS(DESPESA_90DIAS_1996, "DESPESA_90DIAS_1996.rds")
saveRDS(DESPESA_6MESES_1996, "DESPESA_6MESES_1996.rds")
saveRDS(BENSDURAVEIS_1996, "BENSDURAVEIS_1996.rds")
saveRDS(OUTRASDESPESAS_1996, "OUTRASDESPESAS_1996.rds")
saveRDS(SERV_DOMS_1996, "SERV_DOMS_1996.rds")
saveRDS(CADERNETA_COLETIVA_1996, "CADERNETA_COLETIVA_1996.rds")
saveRDS(DESPESA_INDIVIDUAL_1996, "DESPESA_INDIVIDUAL_1996.rds")
saveRDS(DESP_VEICULOS_1996, "DESP_VEICULOS_1996.rds")
saveRDS(RENDIMENTOS_1996, "RENDIMENTOS_1996.rds")
saveRDS(OUTROS_RECEBIMENTOS_1996, "OUTROS_RECEBIMENTOS_1996.rds")

############
DOMICILIO_1996 <- readRDS("DOMICILIO_1996.rds")
MORADOR_1996 <- readRDS("MORADOR_1996.rds")
DESPESA_90DIAS_1996 <- readRDS("DESPESA_90DIAS_1996.rds")
DESPESA_6MESES_1996 <- readRDS("DESPESA_6MESES_1996.rds")
BENSDURAVEIS_1996 <- readRDS("BENSDURAVEIS_1996.rds")
OUTRASDESPESAS_1996 <- readRDS("OUTRASDESPESAS_1996.rds")
SERV_DOMS_1996 <- readRDS("SERV_DOMS_1996.rds")
CADERNETA_COLETIVA_1996 <- readRDS("CADERNETA_COLETIVA_1996.rds")
DESPESA_INDIVIDUAL_1996 <- readRDS("DESPESA_INDIVIDUAL_1996.rds")
DESP_VEICULOS_1996 <- readRDS("DESP_VEICULOS_1996.rds")
RENDIMENTOS_1996 <- readRDS("RENDIMENTOS_1996.rds")
OUTROS_RECEBIMENTOS_1996 <- readRDS("OUTROS_RECEBIMENTOS_1996.rds")
############

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




font_import()
y
loadfonts()



#####PLANILHAS DE RENDIMENTOS#####
#Presença de valores em duplicado. Parece haver valores duplicados, mesma pessoa, mesmo tipo de emprego, 
#mesmo valor de rendimento bruto, mesmo número de meses recebidos. Como não tenho informação da fonte pagadora, não
#eliminei, mas tem muita chance de ser reporte em dobro

duplicatas <- RENDIMENTOS_1996 %>%
  group_by(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, COD_ITEM ) %>%
  filter(n() == 1) %>% select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, COD_ITEM, VALOR_1_DEFLACIONADO_RECEBIDO,
                             , NUM_MESES, FATOR_ANUALIZACAO, VALOR_1_ANUALIZACAO_RECEBIDO, NUM_MESES)
View(duplicatas)

### ADICIONANDO COLUNAS DE RENDIMENTO BRUTO

RENDIMENTOS_1996 <- RENDIMENTOS_1996 %>%
  group_by(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, COD_ITEM) %>%
  mutate(RENDIMENTO_BRUTO_ITEM = mean(VALOR_1_ANUALIZACAO_RECEBIDO, na.rm = TRUE)) %>%
  ungroup()

RENDIMENTOS_1996 <- RENDIMENTOS_1996 %>%
  group_by(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE) %>%
  mutate(RENDIMENTO_BRUTO_INFORMANTE = sum(RENDIMENTO_BRUTO_ITEM, na.rm = TRUE)) %>%
  ungroup()

RENDIMENTOS_1996 <- RENDIMENTOS_1996 %>%
  group_by(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC) %>%
  mutate(RENDIMENTO_BRUTO_UC = sum(RENDIMENTO_BRUTO_INFORMANTE, na.rm = TRUE)) %>%
  ungroup()


View(RENDIMENTOS_1996)

### Importar para outras bases

# DESPESA 90 DIAS - somente UC
DESPESA_90DIAS_1996 <- DESPESA_90DIAS_1996 %>%
  left_join(
    RENDIMENTOS_1996 %>%
      select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, RENDIMENTO_BRUTO_UC),
    by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC"), relationship = "many-to-many"
  )

DESPESA_90DIAS_1996 <- DESPESA_90DIAS_1996 %>%
  mutate(DECIL_RENDIMENTO_BRUTO_UC = ntile(RENDIMENTO_BRUTO_UC, 10))

View(DESPESA_90DIAS_1996)

# DESPESA 6 MESES - somente UC
DESPESA_6MESES_1996 <- DESPESA_6MESES_1996 %>%
  left_join(
    RENDIMENTOS_1996 %>%
      select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, RENDIMENTO_BRUTO_UC),
    by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC"), relationship = "many-to-many"
  )

DESPESA_6MESES_1996 <- DESPESA_6MESES_1996 %>%
  mutate(DECIL_RENDIMENTO_BRUTO_UC = ntile(RENDIMENTO_BRUTO_UC, 10))


View(DESPESA_6MESES_1996)

#OUTRAS DESPESAS - SOMENTE UC
OUTRASDESPESAS_1996 <- OUTRASDESPESAS_1996 %>%
  left_join(
    RENDIMENTOS_1996 %>%
      select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, RENDIMENTO_BRUTO_UC),
    by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC"), relationship = "many-to-many"
  )

OUTRASDESPESAS_1996 <- OUTRASDESPESAS_1996 %>%
  mutate(DECIL_RENDIMENTO_BRUTO_UC = ntile(RENDIMENTO_BRUTO_UC, 10))

View(OUTRASDESPESAS_1996)

#SERVIÇOS DOMÉSTICOS - SOMENTE UC
SERV_DOMS_1996 <- SERV_DOMS_1996 %>%
  left_join(
    RENDIMENTOS_1996 %>%
      select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, RENDIMENTO_BRUTO_UC),
    by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC"), relationship = "many-to-many"
  )

SERV_DOMS_1996 <- SERV_DOMS_1996 %>%
  mutate(DECIL_RENDIMENTO_BRUTO_UC = ntile(RENDIMENTO_BRUTO_UC, 10))

View(SERV_DOMS_1996)

# CADERNETA_COLETIVA
#CADERNETA_COLETIVA_1996 <- CADERNETA_COLETIVA_1996 %>% 
#  rename(COD_INFORMANTE = NUM_INFORMANTE)

CADERNETA_COLETIVA_1996 <- CADERNETA_COLETIVA_1996 %>%
  left_join(
    RENDIMENTOS_1996 %>%
      select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, RENDIMENTO_BRUTO_INFORMANTE),
    by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", "COD_INFORMANTE"), relationship = "many-to-many"
  )

CADERNETA_COLETIVA_1996 <- CADERNETA_COLETIVA_1996 %>%
  left_join(
    RENDIMENTOS_1996 %>%
      select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, RENDIMENTO_BRUTO_UC),
    by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC"), relationship = "many-to-many"
  )

CADERNETA_COLETIVA_1996 <- CADERNETA_COLETIVA_1996 %>%
  mutate(DECIL_RENDIMENTO_BRUTO_UC = ntile(RENDIMENTO_BRUTO_UC, 10))

View(CADERNETA_COLETIVA_1996)

#DESPESA INDIVIDUAL - SOMENTE UC
#DESPESA_INDIVIDUAL_1996 <- DESPESA_INDIVIDUAL_1996 %>% 
#  rename(COD_INFORMANTE = NUM_INFORMANTE)

DESPESA_INDIVIDUAL_1996 <- DESPESA_INDIVIDUAL_1996 %>%
  left_join(
    RENDIMENTOS_1996 %>%
      select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, RENDIMENTO_BRUTO_INFORMANTE),
    by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", "COD_INFORMANTE"), relationship = "many-to-many"
  )

DESPESA_INDIVIDUAL_1996 <- DESPESA_INDIVIDUAL_1996 %>%
  left_join(
    RENDIMENTOS_1996 %>%
      select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, RENDIMENTO_BRUTO_UC),
    by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC"), relationship = "many-to-many"
  )

DESPESA_INDIVIDUAL_1996 <- DESPESA_INDIVIDUAL_1996 %>%
  mutate(DECIL_RENDIMENTO_BRUTO_UC = ntile(RENDIMENTO_BRUTO_UC, 10))

View(DESPESA_INDIVIDUAL_1996)

#DESPESA COM VEÍCULOS - SOMENTE UC
#DESP_VEICULOS_1996 <- DESP_VEICULOS_1996 %>% 
#  rename(COD_INFORMANTE = NUM_INFORMANTE)

DESP_VEICULOS_1996 <- DESP_VEICULOS_1996 %>%
  left_join(
    RENDIMENTOS_1996 %>%
      select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, RENDIMENTO_BRUTO_INFORMANTE),
    by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC", "COD_INFORMANTE"), relationship = "many-to-many"
  )

DESP_VEICULOS_1996 <- DESP_VEICULOS_1996 %>%
  left_join(
    RENDIMENTOS_1996 %>%
      select(COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, RENDIMENTO_BRUTO_UC),
    by = c("COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM", "NUM_UC"), relationship = "many-to-many"
  )

DESP_VEICULOS_1996 <- DESP_VEICULOS_1996 %>%
  mutate(DECIL_RENDIMENTO_BRUTO_UC = ntile(RENDIMENTO_BRUTO_UC, 10))

View(DESP_VEICULOS_1996)

#salvando as bases em rds 
saveRDS(DOMICILIO_1996, "DOMICILIO_1996.rds")
saveRDS(MORADOR_1996, "MORADOR_1996.rds")
saveRDS(DESPESA_90DIAS_1996, "DESPESA_90DIAS_1996.rds")
saveRDS(DESPESA_6MESES_1996, "DESPESA_6MESES_1996.rds")
saveRDS(BENSDURAVEIS_1996, "BENSDURAVEIS_1996.rds")
saveRDS(OUTRASDESPESAS_1996, "OUTRASDESPESAS_1996.rds")
saveRDS(SERV_DOMS_1996, "SERV_DOMS_1996.rds")
saveRDS(CADERNETA_COLETIVA_1996, "CADERNETA_COLETIVA_1996.rds")
saveRDS(DESPESA_INDIVIDUAL_1996, "DESPESA_INDIVIDUAL_1996.rds")
saveRDS(DESP_VEICULOS_1996, "DESP_VEICULOS_1996.rds")
saveRDS(RENDIMENTOS_1996, "RENDIMENTOS_1996.rds")
saveRDS(OUTROS_RECEBIMENTOS_1996, "OUTROS_RECEBIMENTOS_1996.rds")



###### RENDIMENTO PER CAPITA
library(dplyr)

# Base DOMICILIO_1996
base_uc <- DOMICILIO_1996 %>%
  mutate(PESSOAS_POR_UC = as.numeric(QTDE_MORADORES) / as.numeric(NUMERO_DA_UNIDADE_DE_CONSUMO)) %>%
  select(REGIAO_METROPOLITANA, COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, PESSOAS_POR_UC)

adicionar_renda_pc_uc <- function(base) {
  base %>%
    left_join(base_uc, by = c("REGIAO_METROPOLITANA", "COD_UF", "NUM_SEQ", "DV_SEQ", "NUM_DOM")) %>%
    mutate(RENDIMENTO_PC_UC = RENDIMENTO_BRUTO_UC / PESSOAS_POR_UC)
}

DESPESA_90DIAS_1996 <- adicionar_renda_pc_uc(DESPESA_90DIAS_1996)
DESPESA_6MESES_1996 <- adicionar_renda_pc_uc(DESPESA_6MESES_1996)
OUTRASDESPESAS_1996 <- adicionar_renda_pc_uc(OUTRASDESPESAS_1996)
SERV_DOMS_1996 <- adicionar_renda_pc_uc(SERV_DOMS_1996)
CADERNETA_COLETIVA_1996 <- adicionar_renda_pc_uc(CADERNETA_COLETIVA_1996)
DESPESA_INDIVIDUAL_1996 <- adicionar_renda_pc_uc(DESPESA_INDIVIDUAL_1996)
DESP_VEICULOS_1996 <- adicionar_renda_pc_uc(DESP_VEICULOS_1996)
RENDIMENTOS_1996 <- adicionar_renda_pc_uc(RENDIMENTOS_1996)


decil_renda_pc_uc <- function(base) {
  
  # Checar se RENDIMENTO_PC_UC existe
  if(!("RENDIMENTO_PC_UC" %in% names(base))) {
    stop("A coluna RENDIMENTO_PC_UC não foi encontrada na base!")
  }
  
  # Criar uma chave única para UC
  base <- base %>%
    mutate(
      chave_uc = paste(REGIAO_METROPOLITANA, COD_UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = "_")
    )
  
  # Resumir para uma linha por UC
  ucs_unicas <- base %>%
    distinct(chave_uc, RENDIMENTO_PC_UC) %>%
    mutate(DECIL_RENDIMENTO_PC_UC = ntile(RENDIMENTO_PC_UC, 10))
  
  # Voltar o decil para a base original
  base_final <- base %>%
    left_join(
      ucs_unicas %>% select(chave_uc, DECIL_RENDIMENTO_PC_UC),
      by = "chave_uc"
    )
  
  return(base_final)
}


DESPESA_90DIAS_1996 <- decil_renda_pc_uc(DESPESA_90DIAS_1996)
DESPESA_6MESES_1996 <- decil_renda_pc_uc(DESPESA_6MESES_1996)
OUTRASDESPESAS_1996 <- decil_renda_pc_uc(OUTRASDESPESAS_1996)
SERV_DOMS_1996 <- decil_renda_pc_uc(SERV_DOMS_1996)
CADERNETA_COLETIVA_1996 <- decil_renda_pc_uc(CADERNETA_COLETIVA_1996)
DESPESA_INDIVIDUAL_1996 <- decil_renda_pc_uc(DESPESA_INDIVIDUAL_1996)
DESP_VEICULOS_1996 <- decil_renda_pc_uc(DESP_VEICULOS_1996)
RENDIMENTOS_1996 <- decil_renda_pc_uc(RENDIMENTOS_1996)

saveRDS(DOMICILIO_1996, "DOMICILIO_1996.rds")
saveRDS(MORADOR_1996, "MORADOR_1996.rds")
saveRDS(DESPESA_90DIAS_1996, "DESPESA_90DIAS_1996.rds")
saveRDS(DESPESA_6MESES_1996, "DESPESA_6MESES_1996.rds")
saveRDS(BENSDURAVEIS_1996, "BENSDURAVEIS_1996.rds")
saveRDS(OUTRASDESPESAS_1996, "OUTRASDESPESAS_1996.rds")
saveRDS(SERV_DOMS_1996, "SERV_DOMS_1996.rds")
saveRDS(CADERNETA_COLETIVA_1996, "CADERNETA_COLETIVA_1996.rds")
saveRDS(DESPESA_INDIVIDUAL_1996, "DESPESA_INDIVIDUAL_1996.rds")
saveRDS(DESP_VEICULOS_1996, "DESP_VEICULOS_1996.rds")
saveRDS(RENDIMENTOS_1996, "RENDIMENTOS_1996.rds")
saveRDS(OUTROS_RECEBIMENTOS_1996, "OUTROS_RECEBIMENTOS_1996.rds")

# Lista das colunas a serem convertidas
colunas_para_converter <- c(
  "VALOR_REND_DEFL_ANUAL", 
  "VALOR_IR_DEFL_ANUAL", 
  "VALOR_PREVID_DEFL_ANUAL", 
  "VALOR_OUTRAS_DEDUCOES_DEFL_ANUAL"
)

# Converter as colunas para numérico
RENDIMENTOS_1996[colunas_para_converter] <- lapply(RENDIMENTOS_1996[colunas_para_converter], function(x) as.numeric(x))


RENDIMENTOS_1996 <- RENDIMENTOS_1996 %>%
  mutate(
    REND_TRAB_MENSAL = VALOR_REND_DEFL_ANUAL  / 12, 
    DED_IR_MENSAL = VALOR_IR_DEFL_ANUAL / 12,
    DED_PREV_MENSAL = VALOR_PREVID_DEFL_ANUAL / 12,
    DED_OUTRAS_MENSAL = VALOR_OUTRAS_DEDUCOES_DEFL_ANUAL / 12
  ) %>%
  mutate(
    REND_TRAB_MENSAL = ifelse(is.na(REND_TRAB_MENSAL), 0, REND_TRAB_MENSAL), 
    NUM_SEQ = str_pad(NUM_SEQ, width = 3, side = "left", pad = "0"), 
    COD_UPA = paste0(COD_UF, NUM_SEQ, DV_SEQ), 
    PESO_FINAL = FATOR_EXPANSAO2
  )

colnames(RENDIMENTOS_1996)

RENDIMENTOS_1996_LIMPA <- select(RENDIMENTOS_1996, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, PESO_FINAL, 
                                                                  REND_TRAB_MENSAL, DED_IR_MENSAL, 
                                                                  DED_PREV_MENSAL, DED_OUTRAS_MENSAL)
                                 
#RENDIMENTOS_1996_LIMPA <- select(RENDIMENTOS_1996, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, PESO_FINAL, 
#                                 REND_TRAB_MENSAL, DED_IR_MENSAL, 
#                                 DED_PREV_MENSAL, DED_OUTRAS_MENSAL) %>% rename(COD_INFORMANTE = COD_INFORMANTE)

saveRDS(RENDIMENTOS_1996, "RENDIMENTOS_1996.rds")
saveRDS(RENDIMENTOS_1996_LIMPA, "RENDIMENTOS_1996_LIMPA.rds")   

#Quantas pessoas tiveram renda do trabalho?
n_pessoas <- RENDIMENTOS_1996_LIMPA %>%
  distinct(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, PESO_FINAL)
sum(n_pessoas$PESO_FINAL)
#[1] 14694296

#média de renda empregador
RENDIMENTOS_2002_LIMPA %>%
  filter(POSICAO_OCUPACAO == 06) %>%
  summarise(media_ponderada = weighted.mean(REND_TRAB_MENSAL, PESO_FINAL, na.rm = TRUE)) %>%
  pull(media_ponderada)
# [1] 2022.611


#Média de renda conta própria
#Média de renda empregado público e privado
  #Falta a variável POSICAO_OCUPACAO na POF de 1996


#Calculando a renda líquida do trab
RENDIMENTOS_1996_LIMPA <- RENDIMENTOS_1996_LIMPA %>%
  mutate(
    REND_TRAB_MENSAL_LIQ = REND_TRAB_MENSAL - DED_IR_MENSAL - DED_PREV_MENSAL - DED_OUTRAS_MENSAL) 
saveRDS(RENDIMENTOS_1996_LIMPA, "RENDIMENTOS_1996_LIMPA.rds")

RENDIMENTOS_1996_LIMPA1 <- RENDIMENTOS_1996_LIMPA %>%
  select(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, REND_TRAB_MENSAL, REND_TRAB_MENSAL_LIQ) %>%
  dplyr::group_by(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE) %>%
  dplyr::summarize(
    soma_RENDA_TRAB = sum(REND_TRAB_MENSAL, na.rm = TRUE),
    soma_RENDA_TRAB_LIQ = sum(REND_TRAB_MENSAL_LIQ, na.rm = TRUE),
    .groups = "drop"
  )

#Incluindo a renda do trab na planilha de MORADOR
##as linhas de rendimento do trabalho ainda estão por morador, não per capita por domicilio

MORADOR_1996 <- MORADOR_1996 %>%
  mutate(COD_UPA = paste0(COD_UF, NUM_SEQ, DV_SEQ))


MORADOR_1996 <- left_join(MORADOR_1996, RENDIMENTOS_1996_LIMPA1, by = c("COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE"))

MORADOR_1996 <- MORADOR_1996 %>%
  mutate_all(~replace_na(., 0))

saveRDS(MORADOR_1996, "MORADOR_1996.rds")

###
#Avaliando o montante dos programas de transferência de renda, alugueis e aposentadorias
  #Falta a variável VALOR_DEDUCAO_DEFLACAO_ANUAL

#Número de pessoas que recebiam algum tipo de transferência do governo
  #OUTROS_RENDIMENTOS_1996_trasnf (?)
  #PESO_FINAL = FATOR_EXPANSÃO2
    #n_pessoas <- OUTROS_RENDIMENTOS_1996 %>%
    #  distinct(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, PESO_FINAL)
    #sum(n_pessoas$PESO_FINAL)

    #weighted.mean(x=OUTROS_RENDIMENTOS_2002_transf$OUTROS_REND,
                  #W=OUTROS_RENDIMENTOS_2002_transf$PESO_FINAL)

#Avaliando outras rendas "perenes"(criando instrumento para proxy de despesa total)
OUTROS_RENDIMENTOS_2002 <- OUTROS_RENDIMENTOS_2002 %>%
  mutate(OUTROS_REND = (VALOR_REND_DEFLACAO_ANUAL/ 12),
         NUM_SEQ = str_pad(NUM_SEQ, width = 3, side = "left", pad = "0"), 
         COD_UPA = paste0(COD_UF, NUM_SEQ, DV_SEQ)) %>%
  filter(NUM_QUADRO == 53) %>%
  filter(COD_ITEM %in% c("3501", "3502", "3503", "3504", "3505", "3506", 
                         "3601", "3602", "3701", "3801", "4101","4201")) 

