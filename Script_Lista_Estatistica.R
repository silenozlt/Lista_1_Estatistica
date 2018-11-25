#LISTA DE EXERCICIOS 

require(dplyr)
require(ggplot2)
# Definindo o diretorio
setwd("/Users/cassio/Desktop/Lista_1_Estatistica")

#CARREGANDO ARQUIVO CBO2002_Familia.csv
familia <- read.table("CBO2002_Familia.csv", sep = ";", header = TRUE,na.strings = '')
View(familia)

#CARREGANDO DADOS RAIS 2014
rais_2014 <- read.csv("dados_rais_2014.txt", sep = "\t", header = TRUE,na.strings = '')
View(rais_2014)

rais_fama <- data.frame(rais_2014$ano, rais_2014$regiao, rais_frama$sg_estado, rais_2014$id_cbo, rais_2014$renda_mensal_total,rais_2014$total_empregos, rais_2014$total_estabelecimentos)
View(rais_frama)

#CARREGANDO DADOS GRANDE GRUPO
GrandeGrupo <- read.table("CBO2002_Grande_Grupo.txt", sep = "\t",
                    header = T ,na.strings = '', encoding = "latin1")
head(GrandeGrupo)
tail(GrandeGrupo)
View(GrandeGrupo)
#####################################################################################


#tentativa de merge
#tabela_uniao <- merge(GrandeGrupo, novo_grande_grupo, by = "Codigo")

#PERGUNTA 2 Insira em seu data frame que cont ́em os dados da RAIS as descri ̧c ̃oes de Fam ́ılia e Grandes Grupos. Dica: Para criar o Grande Grupo, utilize a fun ̧ca ̃o substr.
#QUEBRANDO A FAMILIA
Codigo <- substring(familia$codigo, 1,1) 
Codigo
descricao <- (familia$descricao)
descricao
id_cbo <- (familia$codigo)
id_cbo

#NOVA FAMILIA 
novo_grande_grupo <- data.frame(Codigo,descricao,id_cbo)

View(novo_grande_grupo)


rais_completo <- merge(rais_2014, novo_grande_grupo, by = "id_cbo")

View(rais_completo)
View(familia)
#####################################################################################
#PERGUNTA 3 -Calcule a Renda Mensal Total e o número total de empregos por estado, por ocupacao e por grande grupo.
#POR ESTADO
questao3 <- aggregate(cbind( rais_completo$renda_mensal_total, rais_completo$total_empregos)
,by = list(rais_completo$sg_estado), FUN = "sum" )
print(questao3)

#POR GRANDE GRUPO
questao3a <- aggregate(cbind( rais_completo$renda_mensal_total, rais_completo$total_empregos)
                      ,by = list(rais_completo$descricao), FUN = "sum" )
View(questao3a)

#POR OCUPACAO
questao3b <- aggregate(cbind( rais_completo$renda_mensal_total, rais_completo$total_empregos)
                      ,by = list(rais_completo$id_cbo),  FUN = "sum")
View(questao3b)

#####################################################################################
# PERGUNTA 4 - ORDENANDO POR ESTADO
estado <- rais_completo$sg_estado
empregos <- rais_completo$total_empregos


data_questao <- data_frame(estado, empregos)

questao4 <- data_questao[order(data_questao$empregos, decreasing = TRUE),]
View(questao4)
#####################################################################################
#Qual ocupa ̧ca ̃o possui o maior nu ́mero de empregos no Brasil?









estado_funcionarios <- 
questao4 <- rais_completo[order(rais_completo$sg_estado),]
View(questao4)
order(rais_completo, decreasing = TRUE)

questao4 <- rais_completo[order(rais_completo$sg_estado),]
View(questao4)

subset(rais_completo$sg_estado, rais_completo$ano > 2000, rais_completo$total_empregos,)

# PERGUNTA 5 - Qual ocupacao possui o maior número de empregos no Brasil?
questao5 <- aggregate(cbind( rais_completo$id_cbo, rais_completo$total_empregos)
                      ,by = list(rais_completo$id_cbo), FUN = "sum" )
View(questao5)


#EXEMPLO SUBSET
x <- subset(rais_completo, id_cbo == 1112, select = c(id_cbo, total_empregos))

#6. Qual ocupa ̧ca ̃o possui maior remunera ̧ca ̃o m ́edia? E a menor?

questao6 <- questao6[
  order(
    questao6$renda_mensal_total
    ,decreasing = TRUE
  )
]


#CARREGANDO CONJUNTO DE DADOS INFERT
(data("infert"))
View(infert)

#SOMANDO TOTAL DE ABORTOS INDUZIDOS
sum(infert$induced)

#SOMANDO TOTAL DE ABORTOS INDUZIDOS
sum(infert$spontaneous)


require(gmodels)

# Tabela cruzada
table(infert$education,infert$induced) # Tabela cruzada de frequ?ncia absoluta

table(infert$education,infert$spontaneous) # Tabela cruzada de frequ?ncia absoluta

#TABELA CRUZADA
CrossTable(x=infert$education,y=infert$induced,format="SAS")
?CrossTable


