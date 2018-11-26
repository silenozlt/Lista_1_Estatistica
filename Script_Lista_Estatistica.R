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
# PERGUNTA 5 - Qual ocupacao possui o maior número de empregos no Brasil?
questao5 <- aggregate(cbind( rais_completo$id_cbo, rais_completo$total_empregos)
                      ,by = list(rais_completo$id_cbo), FUN = "sum" )
View(questao5)

#EXEMPLO SUBSET
x <- subset(rais_completo, id_cbo == 1112, select = c(id_cbo, total_empregos))
#####################################################################################
#6. Qual ocupa ̧ca ̃o possui maior remunera ̧ca ̃o m ́edia? E a menor?
questao6 <- aggregate(cbind( rais_completo$id_cbo, rais_completo$renda_mensal_total)
                      ,by = list(rais_completo$id_cbo), FUN = "sum" )
View(questao6)

renda_media <- questao6$V2 / questao6$V1
View(renda_media)


#####################################################################################
# PERGUNTA - 7. Quantas ocupa ̧co ̃espossuemremunera ̧ca ̃om ́ediamenorqueR$2.000,00 reais?
questao7 <- aggregate(
   cbind( remuneracao_media = (rais_completo$renda_mensal_total /rais_completo$total_empregos))
   ,by = list(ocupacao = rais_completo$descricao), FUN = "median" )
View(questao7)

questao7 <- subset(questao7, remuneracao_media < 2000 ,select = ocupacao)

print(nrow(questao7))

#####################################################################################
#PERGUNTA - 8 Dentre os t ́ecnicos de n ́ıvel m ́edio de Minas Gerais, qual ocupa ̧ca ̃o pos- sui maior remunera ̧ca ̃o m ́edia?
questao8_data <- subset(rais_completo, sg_estado == "MG" & Codigo ==3, select = c(descricao, renda_mensal_total, total_empregos) )
View(questao8_data)

questao8 <- aggregate(
  cbind( questao8_data = (questao8_data$renda_mensal_total /questao8_data$total_empregos))
  ,by = list(ocupacao = questao8_data$descricao), FUN = "median" )

print(questao8)

which.max(questao8$questao8_data)

#####################################################################################
# PERGUNTA - 9 Fa ̧ca um gr ́afico de barras representando o nu ́mero de empregos por Grande Grupo
questao9 <- aggregate(cbind(rais_completo$descricao ,rais_completo$total_empregos)
  ,by = list(ocupacao = rais_completo$descricao), FUN = "sum" )
View(questao9)

pie(questao9$V2,
    labels = paste(questao9$ocupacao," (",round(prop,3)*100,"%)", sep=""),col=c(1,3))
plot(questao8$V1, questao9$V2)




#####################################################################################
#CARREGANDO CONJUNTO DE DADOS INFERT
(data("infert"))
View(infert)
# 1. Calcule o nu ́mero total de abortos (induzidos e espontˆaneos)

#SOMANDO TOTAL DE ABORTOS INDUZIDOS
sum(infert$induced)

#SOMANDO TOTAL DE ABORTOS INDUZIDOS
sum(infert$spontaneous)

#####################################################################################
#2. Construa uma tabela de frequˆencia cruzada entre o nu ́mero total de abortos e a escolaridade das mulheres.


replicate1 <- infert[1:82,]
replicate2 <- infert[83:164,]
replicate3 <- infert[165:246,]

CASOS <- c(sum(replicate1$induced),sum(replicate1$spontaneous))
Controle1 <- c(sum(replicate2$induced),sum(replicate2$spontaneous))
Controle2 <- c(sum(replicate3$induced),sum(replicate3$spontaneous))

repl <- data.frame(CASOS, Controle1, Controle2)
barplot(as.matrix(repl), main= "CASOS DE ABORTO", ylab = "Induced + Spontaneous",space=0.3, cex.axis=0.8, col= cm.colors(2))
legend("topright", c("INDUZIDO", "ESPONTANEO"), fill=cm.colors(2))




require(gmodels)

# Tabela cruzada
table(infert$education,infert$induced) # Tabela cruzada de frequ?ncia absoluta

table(infert$education,infert$spontaneous) # Tabela cruzada de frequ?ncia absoluta

#TABELA CRUZADA
CrossTable(x=infert$education,y=infert$induced,format="SAS")
?CrossTable


