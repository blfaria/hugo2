library(ggplot2)
library(dplyr)
library(rasterVis)
library(gridExtra)


getwd()
setwd("C:/Desertificacao/processando/")
ava = read.csv("Avaliação de Desempenho Docente (4).csv")
str(ava)


##subsetting
for (name in unique(ava$Nome.do.professor)){
  print(name)
  # separa na variavel atual apenas a informaçao que vai usar
  atual = subset(ava,ava$Nome.do.professor==name)
  
  
  #################
  #################
  assiduidade = mean(atual$X2...Assiduidade)
  pontualidade  = mean(atual$X1..Pontualidade)
  organizacao = mean(atual$X3..OrganizaÃ.Ã.o.e.disciplina)
  relacionamento = mean(atual$X4..Relacionamento)
  interesse  = mean(atual$X5..Interesse)
  metodologia  = mean(atual$X6..Metodologia)
  avaliacao  = mean(atual$X7..AvaliaÃ.Ã.o)
  desenvolvimento  = mean(atual$X8..Desenvolvimento.profissional)
  compromisso = mean(atual$X9..Compromisso.institucional)
  etica = mean(atual$X10..Ã.tica)
  
  #colm = colMeans(etica)
  #nomes = x$Nome.do.professor.
  
  x = c("MEDIA GERAL","assiduidade","avaliacao","compromisso","desenvolvimento","etica","interesse","metodologia","organizacao","pontualidade","relacionamento")
  # para dar o valor certo precisa ser um vetor
  media = c(assiduidade,pontualidade,organizacao,relacionamento,interesse,metodologia,avaliacao,desenvolvimento,compromisso,etica)
  y.mean = mean(media)
  y = c(y.mean,assiduidade,avaliacao,compromisso,desenvolvimento,etica,interesse,metodologia,organizacao,pontualidade,relacionamento)

  df <- data.frame(x,y)
  ##exibir pela ordem em que a variavel aparece no vetor
  df$x <- factor(df$x, as.character(df$x))
  
  #################
  #################
  
  #### faz o grafico
  p <- ggplot(df, aes(x, y, fill = x)) + 
    geom_bar(stat = "identity",position = 'dodge') + 
    # texto sobre as barras
    geom_text(aes(label=format(round(y, 2), nsmall = 2)), data=df, position=position_dodge(width=0.9), vjust=-0.25) 
  
  #### coloca labels no grafico, como titulo e nomes dos eixos
  p = p + xlab("group") +ylab("Medias das notas") +
    labs(fill="Criterios") + ggtitle(paste(name," "," Observacoes: ", nrow(atual))) +ylim(0,5) #ylim faz com que todos os graficos gerados mostrem ate o valor 5
  
  # sequencia que faz a gravaçao
  p
  ggsave(paste(name,"sobreposto","_", nrow(atual),"_Observacoes","180.jpg"),width = 275, height = 170,units = "mm", dpi = 180)

}