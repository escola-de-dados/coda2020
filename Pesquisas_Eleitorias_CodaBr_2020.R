#Alguns Atalhos

#CTRL+ENTER: roda a linha selecionada no script. O atalho mais utilizado.
#ALT+-: (<-) sinal de atribuição.
#CTRL+SHIFT+M: (%>%) operador pipe.
#CTRL+1: altera cursor para o script.
#CTRL+2: altera cursor para o console.
#ALT+SHIFT+K: janela com todos os atalhos disponíveis.


install.packages("dplyr") # Pacote de manipulação de dados
install.packages("tidyverse") # 
install.packages("agricolae") # correlação
install.packages("ggplot2") #gráficos dispersão
install.packages("qcc")  #Graf. Pareto

library(dplyr)
library(tidyverse)
library(agricolae)
library(ggplot2)
library(qcc)

# Criando diretório de trabalho

dir.create("C:/Users/Data 5/Desktop/CodaBr/Arquivos",recursive=T)
setwd("C:/Users/Data 5/Desktop/CodaBr/Arquivos")


# lendo e tratando os dados de repositótio 
# baixando arquivo da internet:


download.file(url="http://agencia.tse.jus.br/estatistica/sead/odsele/pesquisa_eleitoral/pesquisa_eleitoral_2020.zip",
              destfile="pesquisa_eleitoral_2020.zip",cacheOK=F)

# Descompactando arquivo

unzip(zipfile="pesquisa_eleitoral_2020.zip",exdir = getwd())

# Prompt de comand digitar cd C:\Users\Data 5\Desktop\CodaBr\Arquivos
# copy *.csv pesquisa_eleitoral_2020_BRASIL.csv


#lendo a tabela descompactada

Base_pesquisa_eleitoral<-read.csv2("pesquisa_eleitoral_2020_BRASIL.csv",header = T)


## nomes das variáveis
names(Base_pesquisa_eleitoral)

View(head(Base_pesquisa_eleitoral,10)) # Visualizando as primeiras 10 obs.

## exlcuir duplicidade de head
Base_pesquisa_eleitoral <- subset(Base_pesquisa_eleitoral, Base_pesquisa_eleitoral$DT_GERACAO != "DT_GERACAO") 

## Altera var char para numerica
Base_pesquisa_eleitoral$QT_ENTREVISTADOS<-as.numeric(Base_pesquisa_eleitoral$QT_ENTREVISTADOS)

## Altera var char para valor
Base_pesquisa_eleitoral$VR_PESQUISA <- as.numeric(gsub (",",".",Base_pesquisa_eleitoral$VR_PESQUISA))


#data de inicio da primeira pesquisa
min((Base_pesquisa_eleitoral[,"DT_INICIO_PESQUISA"]))

#data de inicio da última pesquisa
max((Base_pesquisa_eleitoral[,"DT_INICIO_PESQUISA"]))


table(Base_pesquisa_eleitoral$DS_CARGOS) # Frequência por cargo

prop.table(table(Base_pesquisa_eleitoral$DS_CARGOS)) #  percentual

table(Base_pesquisa_eleitoral$DS_ORIGEM_RECURSO) # Frequência Origem Recursos

prop.table(table(Base_pesquisa_eleitoral$DS_ORIGEM_RECURSO)) # percentual

#Qtd de Empresas distintas envolvidas por Município

Qtd_Empresas <- Base_pesquisa_eleitoral %>% 
  group_by(NM_UE,SG_UE,SG_UF) %>% 
  summarise(QTD = n_distinct(NR_CNPJ_EMPRESA))

View(Qtd_Empresas)

#Ver Gráfico Histograma

G_hist<-hist(Qtd_Empresas$QTD,  
     main = "Qtd. de Empresas distintas envolvidas em pesquisa por município", 
     xlab = "Qtd. de Empresas", ylab = "Freq. Absoluta", 
     col = c("blue"), 
     labels = TRUE)

#Ver Gráfico Pareto
Pareto <- table(Qtd_Empresas$QTD)
G_Pareto <- pareto.chart(Pareto)

# Municipios com + de 3 empresas atuando
MUN_COM_3EM<-Qtd_Empresas %>% 
  filter(QTD>3)
View(MUN_COM_3EM)

# Concentracao de Municipios com + de 3 empresas atuando por Estado
TAB_MUN_COM_3EM<-prop.table(table(MUN_COM_3EM$SG_UF))*100


#Box-Plot
#Existe diferença no Valor da Pesquisa paga com origem de recursos distintas?
#Selecionando mesmo município e cargo

#Box-Plot Valor Pesquisa
Base_pesquisa_eleitoral %>%
    filter(DS_CARGOS=="Prefeito",NM_UE=="SÃO PAULO") %>%
    ggplot(aes(reorder(DS_ORIGEM_RECURSO,VR_PESQUISA),VR_PESQUISA))+
    geom_boxplot()

#Box-Plot Qtd Entrevistados
Base_pesquisa_eleitoral %>%
  filter(DS_CARGOS=="Prefeito",NM_UE=="SÃO PAULO") %>%
  ggplot(aes(reorder(DS_ORIGEM_RECURSO,QT_ENTREVISTADOS),QT_ENTREVISTADOS))+
  geom_boxplot()
  
#Filtra Origem de Recursos Fundo Partidário
Fundo_Pardidario <- Base_pesquisa_eleitoral %>%
  filter(DS_CARGOS=="Prefeito",DS_ORIGEM_RECURSO=="Fundo Partidário")

View(head(Fundo_Pardidario,10)) # Visualizando as primeiras 10 obs.


#Box-Plot Ordenado
Base_pesquisa_eleitoral %>%
  filter(DS_CARGOS=="Prefeito",NM_UE=="RIO BRANCO") %>%
  ggplot(aes(reorder(DS_ORIGEM_RECURSO,VR_PESQUISA),VR_PESQUISA))+
  geom_boxplot()

#Box-Plot Colorido
Base_pesquisa_eleitoral %>%
  filter(DS_CARGOS=="Prefeito",NM_UE=="RIO BRANCO",SG_UF=="AC") %>%
  ggplot(aes(reorder(DS_ORIGEM_RECURSO,VR_PESQUISA),VR_PESQUISA))+
  geom_boxplot(color="red",fill=NA)


#Box-Plot Comparando Cidades
Base_pesquisa_eleitoral %>%
  filter(DS_CARGOS=="Prefeito",NM_UE %in% c("RIO DE JANEIRO","SÃO PAULO")) %>%
  ggplot(aes(reorder(NM_UE,VR_PESQUISA),VR_PESQUISA, fill=NM_UE))+
  geom_boxplot()


#Outra possibilidade de leitura dos valores 
RP<-(Base_pesquisa_eleitoral %>% 
          filter(NM_UE=="RIO BRANCO",DS_CARGOS=="Prefeito",DS_ORIGEM_RECURSO=="Recursos Próprios"))

Nulo<-(Base_pesquisa_eleitoral %>% 
                      filter(NM_UE=="RIO BRANCO",DS_CARGOS=="Prefeito",DS_ORIGEM_RECURSO=="#NULO#"))

FP<-(Base_pesquisa_eleitoral %>% 
         filter(NM_UE=="RIO BRANCO",DS_CARGOS=="Prefeito",DS_ORIGEM_RECURSO=="Fundo Partidário"))

summary(RP$VR_PESQUISA)
summary(Nulo$VR_PESQUISA)
summary(FP$VR_PESQUISA)
  }        

#Relação entre Valor Pesquisa e Qtd de Entrevistados

Base_pesquisa_eleitoral %>% 
  filter(NM_UE=="SÃO PAULO",DS_CARGOS=="Prefeito")%>%
  ggplot(aes(QT_ENTREVISTADOS,VR_PESQUISA)) + 
  geom_point()

#Relação entre Valor Pesquisa e Qtd de Entrevistados / correlação
#função já existente no R
cor(Base_pesquisa_SP$VR_PESQUISA,Base_pesquisa_SP$QT_ENTREVISTADOS, method = "spearman")

#agricolare adicional p valor (significancia)
#quanto maior qtd de entrevistados mais barata a pesquisa
correlation(Base_pesquisa_SP$QT_ENTREVISTADOS,Base_pesquisa_SP$VR_PESQUISA)

#p valor maior que 0,05 não significativo

#Valor Pesquisa é dependente da Qtd de entrevistados / regressão 
lm1= lm(VR_PESQUISA ~ QT_ENTREVISTADOS,  data=Base_pesquisa_SP)
summary(lm1)

#Bo Intercept 
#B1 angulação da reta



#https://receita.economia.gov.br/orientacao/tributaria/cadastros/cadastro-nacional-de-pessoas-juridicas-cnpj/dados-publicos-cnpj
#https://github.com/turicas/socios-brasil/
#https://brasil.io/dataset/socios-brasil/files/





