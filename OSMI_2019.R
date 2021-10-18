###############################################################################
# Instalacao e Carregamento de Todos os Pacotes
###############################################################################

pacotes <- c("tidyverse","factoextra","C50","gmodels","rpart.plot","neuralnet",
             "rattle","DataExplorer","caret","GGally","kernlab","pROC","Metrics",
             "OneR","tm","wordcloud","e1071","caretEnsemble","plotly","ggrepel",
             "sjPlot","knitr","kableExtra","FactoMineR","cabootcrs",
             "PerformanceAnalytics", "SDaA")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

library(dplyr)
library(tidyverse) #pacote para manipulacao de dados
library(factoextra) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(DataExplorer)
library(caret)
library(psych)
library(ggplot2)
library(ggraph)
library(see)
library(correlation)
library(Rcpp)
library(grid)
library(GGally)
library(reshape2)
library(C50)
library(gmodels)
library(rpart)
library(rpart.plot)
library(rattle)
library(neuralnet)
library(kernlab)
library(caretEnsemble)
library(pROC)
library(Metrics)
library(OneR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(scales)

###############################################################################
#Carregamento e Processamento dos dados   OSMI 2019
###############################################################################

#importando base de dados
osmi19 <- read.csv("OSMI 2019 Mental Health in Tech Survey Results.csv", stringsAsFactors = TRUE)

a = osmi19

#nomes das variaveis
names(a)

#averiguando nome e tipo das variaveis
glimpse(a)

#explorando dataset
head(a)

#explorando estrutura do banco de dados
plot_intro(a)

#analise exploratoria
str(a)

#renomeando variaveis para melhor compreendimento
a <- rename(a, 
                 Age = What.is.your.age.,
                 Gender = What.is.your.gender.,
                 Country = What.country.do.you..live..in.,
                 State_Country_EUA = What.US.state.or.territory.do.you..live..in.,
                 Race = What.is.your.race.,
                 Self_Employed =  X.Are.you.self.employed..,
                 Family_Historic = Do.you.have.a.family.history.of.mental.illness.,
                 Interfere_Work = How.has.it.affected.your.career.,
                 Freq_Interfere_Work = If.you.have.a.mental.health.disorder..how.often.do.you.feel.that.it.interferes.with.your.work..when.being.treated.effectively..,
                 Mental_Disturb = Do.you..currently..have.a.mental.health.disorder.,
                 Mental_Disturb_Previous = Have.you.ever.been..diagnosed..with.a.mental.health.disorder.,
                 Seek_Treatment = Have.you.ever.sought.treatment.for.a.mental.health.disorder.from.a.mental.health.professional.,
                 Tech_Ind_Preview_Health_Care_Options = Were.you.aware.of.the.options.for.mental.health.care.provided.by.your.previous.employers.,
                 Tech_Ind_Provide_Health_Care = Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.,
                 Aware_Help_Online_Local = Do.you.know.local.or.online.resources.to.seek.help.for.a.mental.health.issue.,
                 Aware_Health_Care_Options = Do.you.know.the.options.for.mental.health.care.available.under.your.employer.provided.health.coverage.,
                 Wellness_Program = Has.your.employer.ever.formally.discussed.mental.health..for.example..as.part.of.a.wellness.campaign.or.other.official.communication..,
                 Help_Learn_Health_Care = Does.your.employer.offer.resources.to.learn.more.about.mental.health.disorders.and.options.for.seeking.help.,
                 ID_Protected = Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.provided.by.your.employer.,
                 Leave_Emg = If.a.mental.health.issue.prompted.you.to.request.a.medical.leave.from.work..how.easy.or.difficult.would.it.be.to.ask.for.that.leave.,
                 Importance_Health_Physic = Overall..how.much.importance.does.your.employer.place.on.physical.health.,
                 Importance_Health_Mental = Overall..how.much.importance.does.your.employer.place.on.mental.health.,
                 Productivity_Interfere_Affected = Do.you.believe.your.productivity.is.ever.affected.by.a.mental.health.issue.,
                 Level_Productivity_Affected = If.yes..what.percentage.of.your.work.time..time.performing.primary.or.secondary.job.functions..is.affected.by.a.mental.health.issue.,
                 Dlg_Supervisor = Would.you.have.been.willing.to.discuss.your.mental.health.with.your.direct.supervisor.s..,
                 Dlg_Coworker = Would.you.have.been.willing.to.discuss.your.mental.health.with.your.coworkers.at.previous.employers.,
                 Int_Health_Physic = Would.you.be.willing.to.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview.,
                 Int_Health_Mental = Would.you.bring.up.your..mental..health.with.a.potential.employer.in.an.interview.,
                 Tech_Ind_Suport = Overall..how.well.do.you.think.the.tech.industry.supports.employees.with.mental.health.issues.,
                 Coments = If.there.is.anything.else.you.would.like.to.tell.us.that.has.not.been.covered.by.the.survey.questions..please.use.this.space.to.do.so.,
                 Num_Workers_Ind = How.many.employees.does.your.company.or.organization.have.,
                 Working_Tech_Ind = Is.your.employer.primarily.a.tech.company.organization.,
                 )

#Selecionando as variáveis mais fundamentais para o estudo
a <- select(a,
                 Age,
                 Gender,
                 Country,
                 #State_Country_EUA,
                 #Race,
                 Self_Employed,
                 Family_Historic,
                 #Interfere_Work,
                 Freq_Interfere_Work,
                 Mental_Disturb,
                 Mental_Disturb_Previous,
                 Seek_Treatment,
                 Tech_Ind_Preview_Health_Care_Options,
                 Tech_Ind_Provide_Health_Care,
                 #Aware_Help_Online_Local,
                 Aware_Health_Care_Options,
                 Wellness_Program,
                 Help_Learn_Health_Care,
                 ID_Protected,
                 Leave_Emg,
                 Importance_Health_Physic,
                 Importance_Health_Mental,
                 Productivity_Interfere_Affected,
                 Level_Productivity_Affected,
                 Dlg_Supervisor,
                 Dlg_Coworker,
                 Int_Health_Physic,
                 Int_Health_Mental,
                 Tech_Ind_Suport,
                 #Coments,
                 Num_Workers_Ind,
                 Working_Tech_Ind
                 )

#resumo do dataset
summary(a)

#contagem de missing values por coluna
colSums(is.na(a))

#transformando espaços em branco em missing values
a[a == ""] <- NA

#Econtrei uma idade = 1, vou substituir este valor pela mediana, pois foi apenas
#um valor, substituindo consigo manter a integridade 100% da coluna
a$Age[a$Age < 10] <- median(a$Age)

#Padronizando observacoes para algo mais identificavel
#1
a$Self_Employed[a$Self_Employed == FALSE] <- "Not Autonomus"
a$Self_Employed[a$Self_Employed == TRUE] <- "Autonomus"
a$Self_Employed <- as.factor(a$Self_Employed)
class(a$Self_Employed)

#padronizando a observacao do sexo
Male <- c("Male ", "Mail", "maile","Cis Man", "Malr", "Man", "Male", "male", "M", "cis male", "m", "Male-ish", "Mal", "Male (CIS)", "Cis Male", "Make", "Male", "msle")
Female <- c("Female ","Female","femail","woman","Female","Female (cis)","cis-female/femme", "Cis Female", "Trans woman","female","F","Woman","f","Femake", "Trans-female", "Female (trans)")
Non_Binary <- c("Genderqueer","ostensibly male, unsure what that really means","p","A little about you","queer","Neuter", "queer/she/they","something kinda male?","non-binary","Nah","All","Enby","fluid","Androgyne","Agender","Guy (-ish) ^_^","male leaning androgynous")

a$Gender <- as.factor(ifelse(a$Gender %in% Male,"male",
                                  ifelse(a$Gender %in% Female,"female","Non binary")))

#Visualizando a base de dados
a %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#atualizando a estrutura do banco de dados
plot_intro(a)

#Separando o dataframe para trabalhadores autonomos e não autonomos por conta do questionario
#autonomos
b  <- as.data.frame(filter(a, Self_Employed == "Autonomus"))
b  <- select(b, -Tech_Ind_Provide_Health_Care,-Aware_Health_Care_Options,
            -Wellness_Program,-Help_Learn_Health_Care,-ID_Protected,-Importance_Health_Physic,
            -Importance_Health_Mental,-Num_Workers_Ind, -Leave_Emg, -Working_Tech_Ind)

#Visualizando a base de dados
b %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

plot_intro(b)

#nao autonomos
c <- as.data.frame(filter(a, Self_Employed == "Not Autonomus"))
c <- select(c, -Productivity_Interfere_Affected, -Level_Productivity_Affected)

#Visualizando a base de dados
c %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

plot_intro(c)


#histograma da idade
g1 <- ggplot(a, aes(x = Age), na.rm = TRUE) +
  geom_histogram(binwidth = 2.5, bins = 50, fill = "grey", color = "grey50") +
  scale_x_continuous(name = "Idade", breaks = seq(15,70,5),limits = c(15,70)) +
  scale_y_continuous(name = "Contagem") +
  labs(title = "Histograma da idade",
       subtitle = "dos respondentes",
       #x = "Genero",
       #y = "Quantidade",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  theme_classic()

#genero
g2 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Gender), color = "grey50", fill = "grey") +
  geom_text(aes(x = Gender, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Distribuicao de Genero",
       subtitle = "dos respondentes",
       x = "Genero",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#country
g3 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Country), color = "grey50", fill = "grey") +
  geom_text(aes(x = Country, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Distribuicao de Nacionalidade",
       subtitle = "dos respondentes",
       x = "Pais",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#empregado
g4 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Self_Employed), color = "grey50", fill = "grey") +
  geom_text(aes(x = Self_Employed, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Trabalhadores autonomos",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#historico familiar de disturbio
g5 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Family_Historic), color = "grey50", fill = "grey") +
  geom_text(aes(x = Family_Historic, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Historico familiar de disturbio mental",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#frequencia de interferencia no trabalho
g6 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Freq_Interfere_Work), color = "grey50", fill = "grey") +
  geom_text(aes(x = Freq_Interfere_Work, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Frequencia que o distúrbio ocorre, interferindo no trabalho",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#disturbio mental
g7 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Mental_Disturb), color = "grey50", fill = "grey") +
  geom_text(aes(x = Mental_Disturb, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Trabalhadores que suspeitam ter algum disturbio mental",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#disturbio mental prévio (Somente quem disse 'Sim' sobre ter doenca mental)
g8 <- ggplot(filter(a, Mental_Disturb == "Yes", na.rm = TRUE)) +
  geom_bar(aes(x = Mental_Disturb_Previous), color = "grey50", fill = "grey") +
  geom_text(aes(x = Mental_Disturb_Previous, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Trabalhadores que possuem disturbio mental, com antecedente",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#buscou tratamento
g9 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Seek_Treatment), color = "grey50", fill = "grey") +
  geom_text(aes(x = Seek_Treatment, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Trabalhadores que já buscaram tratamento",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#conhecimento prévio das opções de tratamento disponíveis pela Tech Industry
g10 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Tech_Ind_Preview_Health_Care_Options), color = "grey50", fill = "grey") +
  geom_text(aes(x = Tech_Ind_Preview_Health_Care_Options, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Conhecimento prévio de opções de tratamento",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#conhecimento se a Tech Industry disponibiliza opções de tratamento
g11 <- ggplot(c, na.rm = TRUE) +
  geom_bar(aes(x = Tech_Ind_Provide_Health_Care), color = "grey50", fill = "grey") +
  geom_text(aes(x = Tech_Ind_Provide_Health_Care, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Conhecimento prévio se a Tech industry possui opções de tratamento",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#conhecimento sobre as opções de tratamento disponíveis pela Tech industry
g12 <- ggplot(c, na.rm = TRUE) +
  geom_bar(aes(x = Aware_Health_Care_Options), color = "grey50", fill = "grey") +
  geom_text(aes(x = Aware_Health_Care_Options, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Conhecimento prévio de opções de tratamento",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#Tech industry disponibiliza programas formais de tratamento
g13 <- ggplot(c, na.rm = TRUE) +
  geom_bar(aes(x = Wellness_Program), color = "grey50", fill = "grey") +
  geom_text(aes(x = Wellness_Program, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Disponibilização de programas formais de tratamento",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#Tech industry disponibiliza recursos para que você se aprofunde mais no tema
g14 <- ggplot(c, na.rm = TRUE) +
  geom_bar(aes(x = Help_Learn_Health_Care), color = "grey50", fill = "grey") +
  geom_text(aes(x = Help_Learn_Health_Care, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Disponibilização de recursos para aprofundamento em tratamento mental",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#Identidade protegida
g15 <- ggplot(c, na.rm = TRUE) +
  geom_bar(aes(x = ID_Protected), color = "grey50", fill = "grey") +
  geom_text(aes(x = ID_Protected, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Confianca na protecao da identidade caso necessite de apoio de tratamento",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#saida emergencial
g16 <- ggplot(c, na.rm = TRUE) +
  geom_bar(aes(x = Leave_Emg), color = "grey50", fill = "grey") +
  geom_text(aes(x = Leave_Emg, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Qual a facilidade para deixar o trabalho momentaneamente por motivo de saude mental",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#valor atribuido da industria ao cuidado fisico
g17 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Importance_Health_Physic), color = "grey50", fill = "grey") +
  geom_text(aes(x = Importance_Health_Physic, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Industria se preocupa com o cuidado físico do colaborador",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#valor atribuido da industria ao cuidado mental
g18 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Importance_Health_Mental), color = "grey50", fill = "grey") +
  geom_text(aes(x = Importance_Health_Mental, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Industria se preocupa com o cuidado mental do colaborador",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#Produtividade afetada do colaborador
g19 <- ggplot(b, na.rm = TRUE) +
  geom_bar(aes(x = Productivity_Interfere_Affected), color = "grey50", fill = "grey") +
  geom_text(aes(x = Productivity_Interfere_Affected, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Produtividade afetada devido ao disturbio",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#Nivel da produtividade afetada
g20 <- ggplot(filter(b, Productivity_Interfere_Affected == "Yes", na.rm = TRUE)) +
  geom_bar(aes(x = Level_Productivity_Affected), color = "grey50", fill = "grey") +
  geom_text(aes(x = Level_Productivity_Affected, label = ..count..), stat = "count", vjust = -0.5) +
  labs(title = "Nível de produtividade afetada devido ao disturbio",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  #coord_flip() +
  theme_classic()

#Dialogo com supervisor sobre disturbio mental
g21 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Dlg_Supervisor), color = "grey50", fill = "grey") +
  geom_text(aes(x = Dlg_Supervisor, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Você falaria com seu supervisor sobre tratamento mental",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#Dialogo com colega de trabalho sobre disturbio mental
g22 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Dlg_Coworker), color = "grey50", fill = "grey") +
  geom_text(aes(x = Dlg_Coworker, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Você falaria com seu colega de trabalho sobre tratamento mental",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#dialogo num entrevista de trabalho sobre preocupacao fisica
g23 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Int_Health_Physic), color = "grey50", fill = "grey") +
  geom_text(aes(x = Int_Health_Physic, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Você falaria numa entrevista de trabalho sobre preocupacoes fisicas",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#dialogo num entrevista de trabalho sobre preocupacao mental
g24 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Int_Health_Physic), color = "grey50", fill = "grey") +
  geom_text(aes(x = Int_Health_Physic, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Você falaria numa entrevista de trabalho sobre preocupacoes fisicas",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#na sua opiniao qual o nivel do suporte que a Tech industry fornece 
g25 <- ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Tech_Ind_Suport), color = "grey50", fill = "grey") +
  geom_text(aes(x = Tech_Ind_Suport, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Nivel de suporte dado pela Tech industry",
       subtitle = "dos respondentes",
       x = "Nivel",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#numero de colaboradores onde atua
g26 <- ggplot(c, na.rm = TRUE) +
  geom_bar(aes(x = reorder(Num_Workers_Ind)), color = "grey50", fill = "grey") +
  geom_text(aes(x = Num_Workers_Ind, label = ..count..), stat = "count", vjust = -0.5) +
  labs(title = "Numero de colaboradores onde atua",
       subtitle = "dos respondentes",
       x = "Intervalo",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  #coord_flip() +
  theme_classic()

#atuantes de Tech industry
g27 <- ggplot(c, na.rm = TRUE) +
  geom_bar(aes(x = Working_Tech_Ind), color = "grey50", fill = "grey") +
  geom_text(aes(x = Working_Tech_Ind, label = ..count..), stat = "count", hjust = -0.5) +
  labs(title = "Atuantes de Tech industry",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

#arranjando todos os graficos
grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,nrow=3) 
grid.arrange(g10,g11,g12,g13,g14,g15,g16,g17,g18,nrow=3)
grid.arrange(g19,g20,g21,g22,g23,g24,g25,g26,g27,nrow=3)

#analises especificas
#1-para casos de disturbio mental, qual a proporcao de casos previos
ge1 <- ggplot(filter(a, Mental_Disturb == "Yes"), na.rm = TRUE) +
  geom_bar(aes(x = Mental_Disturb, fill = Mental_Disturb_Previous), position = "fill") +
  labs(title = "Grau de reincidencia do disturbio",
       subtitle = "dos respondentes",
       x = "Distribuição",
       y = "Percentual",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  #coord_flip() +
  theme_classic()

#1.1-autonomos
ggplot(filter(b, Mental_Disturb == "Yes"), na.rm = TRUE) +
  geom_bar(aes(x = Mental_Disturb, fill = Mental_Disturb_Previous), position = "fill") +
  labs(title = "Grau de reincidencia do disturbio",
       subtitle = "dos respondentes",
       x = "Distribuição",
       y = "Percentual",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  #coord_flip() +
  theme_classic()

#1.2-nao autonomos
ggplot(filter(c, Mental_Disturb == "Yes"), na.rm = TRUE) +
  geom_bar(aes(x = Mental_Disturb, fill = Mental_Disturb_Previous), position = "fill") +
  labs(title = "Grau de reincidencia do disturbio",
       subtitle = "dos respondentes",
       x = "Distribuição",
       y = "Percentual",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  #coord_flip() +
  theme_classic()

#2-para casos onde a produtividade foi afetada, segue a proporcao dos afetamentos
#Valido somente para os nao autonomos
ge2 <- ggplot(filter(a, Productivity_Interfere_Affected == "Yes"), na.rm = TRUE) +
  geom_bar(aes(x = Productivity_Interfere_Affected, fill = Level_Productivity_Affected), position = "fill") +
  labs(title = "Nivel do impacto causado pelo disturbio mental",
       subtitle = "dos respondentes",
       x = "Distribuição",
       y = "Percentual",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  #coord_flip() +
  theme_classic()

#2.1-autonomos **INVESTIGAR MELHOR**
ggplot(filter(a, Self_Employed == TRUE, Productivity_Interfere_Affected == "Yes"), na.rm = TRUE) +
  geom_bar(aes(x = Productivity_Interfere_Affected, fill = Level_Productivity_Affected), position = "fill") +
  labs(title = "Nivel do impacto causado pelo disturbio mental",
       subtitle = "dos respondentes",
       x = "Distribuição",
       y = "Percentual",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  #coord_flip() +
  theme_classic()


#3-medias dos conceitos numericos
Imp_Physic <- summary(c$Importance_Health_Physic)
Imp_Mental <- summary(c$Importance_Health_Mental)
Ind_Sup <- summary(a$Tech_Ind_Suport)
idad <- summary(a$Age)


#4-importancia para saude fisica e mental
ge3 <- tibble(Imp_Physic,Imp_Mental,Ind_Sup,idad)
ge3 <- as.data.frame(ge3)
rownames(ge3) <- c('Min','1st Qu','Median','Mean', '3rd Qu', 'Max')
ge3

#5- Quem diz não ter disturbio, nao registra frequencia de ocorrência?
ggplot(a, na.rm = TRUE) +
  geom_bar(aes(x = Mental_Disturb, fill = Freq_Interfere_Work)) +
  labs(title = "Trabalhadores que suspeitam ter algum disturbio mental",
       subtitle = "dos respondentes",
       x = "Categoria",
       y = "Contagem",
       caption = "Fonte: OSMI 2019 Mental Health in Tech Survey Results") +
  coord_flip() +
  theme_classic()

a19 = a
a19["Ano"] <- 2019

# Até aqui ta tudo bem, podemos começar os cluster!


###############################################################################
#ANALISE DE CORRELACAO E COLINEARIDADE
###############################################################################

#Verificando a estrutura do dataset
str(a)

#transformando idade em inteiro
a$Age <- as.integer(a$Age)

#Observando a dispersao das relacoes (NUMERIC)
a.numeric <- select(a, Age, Importance_Health_Physic, 
                               Importance_Health_Mental, Tech_Ind_Suport)

#A função chart.Correlation() do pacote PerformanceAnalytics apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation((a.numeric), histogram = TRUE)

#Para utilizar o modelo neural é necessário converter o dataset para numerico
#Apos transformar o dataset para numerico, nossas variaveis categoricas viraram dummy
#Conservando a base, e trabalhando com variável
#1-autonomos
b1 <- b

for (i in 1:ncol(b1)){
  if(is.factor(b1[,i] )){
    b1[,i] <- as.numeric(b1[,i])
  }
}

b1 <- round(cor(b1),2)

b1 <- melt(b1)
ggplot(data = b1, aes(Var2, Var1, fill = value, na.rm = TRUE)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "Dark Orange", high = "Dark Blue", mid = "Green", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Grau de correlação") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed()+xlab("")+ylab("")+ggtitle("Correlação de Pearson")

#2-nao autonomos
c1 = c

for (i in 1:ncol(c1)){
  if(is.factor(c1[,i] )){
    c1[,i] <- as.numeric(c1[,i])
  }
}

c1 <- round(cor(c1),2)

c1 <- melt(c1)
ggplot(data = c1, aes(Var2, Var1, fill = value, na.rm = TRUE)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "Dark Orange", high = "Dark Blue", mid = "Green", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Grau de correlação") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed()+xlab("")+ylab("")+ggtitle("Correlação de Pearson")

#observando correlacoes duas a duas interessantes individualmente
#1-Desconforto de falar sobre saude mental X Nacionalidade
a %>%
count(Int_Health_Mental, Country) %>%
  ggplot(mapping = aes(x = Int_Health_Mental, y = Country)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradient2(low="#FDE725FF", mid="#21908CFF", high="#440154FF")

#2-Desconforto de falar sobre saude mental X Desconforto de falar sobre saude fisica
a %>%
count(Int_Health_Mental, Int_Health_Physic) %>%
  ggplot(mapping = aes(x = Int_Health_Mental, y = Int_Health_Physic)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradient2(low="#FDE725FF", mid="#21908CFF", high="#440154FF")

#3-Apoio da Tech industry X Trabalhador autonomo
a %>%
  count(Tech_Ind_Suport, Self_Employed) %>%
  ggplot(mapping = aes(x = Tech_Ind_Suport, y = Self_Employed)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradient2(low="#FDE725FF", mid="#21908CFF", high="#440154FF")

###############################################################################
# ANALISE DE CORRECACAO MULTIPLA
###############################################################################
# Não há como se estabelecer umas única tabela de contingência que abranja todas
# as variáveis presentes na base de dados. Assim, poderíamos utilizar a função
# table() ou a função sjt.xtab() para analisarmos as variáveis duas a duas:

# Mental x Genero
sjt.xtab(var.row = a$Mental_Disturb, 
         var.col = a$Gender,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Mental x Modalidade que trabalha
sjt.xtab(var.row = a$Mental_Disturb, 
         var.col = a$Self_Employed,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Mental x Experiencia prévia com cuidados mentais
sjt.xtab(var.row = a$Mental_Disturb, 
         var.col = a$Tech_Ind_Preview_Health_Care_Options,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Mental x Receio Entrevista
sjt.xtab(var.row = a$Mental_Disturb, 
         var.col = a$Int_Health_Mental,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Mental x Conversa com colega de trabalho
sjt.xtab(var.row = a$Mental_Disturb, 
         var.col = a$Dlg_Coworker,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Mental x Conversa com supervisor
sjt.xtab(var.row = a$Mental_Disturb, 
         var.col = a$Dlg_Supervisor,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Mental x Frequencia que acontece
sjt.xtab(var.row = a$Mental_Disturb, 
         var.col = a$Freq_Interfere_Work,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

#-------------------------------------------------------------------------------

# Modalidade de trabalho x Genero
sjt.xtab(var.row = a$Self_Employed, 
         var.col = a$Gender,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Modalidade de trabalho x Experiencia prévia com cuidados mentais
sjt.xtab(var.row = a$Self_Employed, 
         var.col = a$Tech_Ind_Preview_Health_Care_Options,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Experiencia prévia com cuidados mentais x Genero
sjt.xtab(var.row = a$Tech_Ind_Preview_Health_Care_Options, 
         var.col = a$Gender,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Experiencia prévia com cuidados mentais x Receio Entrevista
sjt.xtab(var.row = a$Tech_Ind_Preview_Health_Care_Options, 
         var.col = a$Int_Health_Mental,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Receio Entrevista x Genero
sjt.xtab(var.row = a$Int_Health_Mental, 
         var.col = a$Gender,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Receio Entrevista x Modalidade de trabalho
sjt.xtab(var.row = a$Int_Health_Mental, 
         var.col = a$Self_Employed,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Conversa supervisor x Receio entrevista
sjt.xtab(var.row = a$Dlg_Supervisor, 
         var.col = a$Int_Health_Mental,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Conversa supervisor x Experiencia prévia com cuidados mentais
sjt.xtab(var.row = a$Dlg_Supervisor, 
         var.col = a$Tech_Ind_Preview_Health_Care_Options,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Conversa supervisor x Modalidade de trabalho
sjt.xtab(var.row = a$Dlg_Supervisor, 
         var.col = a$Self_Employed,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Conversa supervisor x Genero
sjt.xtab(var.row = a$Dlg_Supervisor, 
         var.col = a$Gender,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Conversa colega de trabalho x Conversa supervisor
sjt.xtab(var.row = a$Dlg_Coworker, 
         var.col = a$Dlg_Supervisor,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Conversa colega de trabalho x Receio entrevista
sjt.xtab(var.row = a$Dlg_Coworker, 
         var.col = a$Int_Health_Mental,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Conversa colega de trabalho x Experiencia prévia com cuidados mentais
sjt.xtab(var.row = a$Dlg_Coworker, 
         var.col = a$Tech_Ind_Preview_Health_Care_Options,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Conversa colega de trabalho x Modalidade de trabalho
sjt.xtab(var.row = a$Dlg_Coworker, 
         var.col = a$Self_Employed,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Conversa colega de trabalho x Genero
sjt.xtab(var.row = a$Dlg_Coworker, 
         var.col = a$Gender,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Estudo do Qui-Quadrado de cada tabela de contingências

#1- Mental x Gender
tab1 <- table(a$Mental_Disturb,
              a$Gender)

tab1

#2- Mental x Modalidade trabalho
tab2 <- table(a$Mental_Disturb,
              a$Self_Employed)
tab2

#3- Perfil x Aplicação
tab3 <- table(a$Mental_Disturb,
              a$Tech_Ind_Preview_Health_Care_Options)

#4- Perfil x Aplicação
tab4 <- table(a$Mental_Disturb,
              a$Int_Health_Mental)

#5- Perfil x Aplicação
tab5 <- table(a$Mental_Disturb,
              a$Dlg_Coworker)

#6- Perfil x Aplicação
tab6 <- table(a$Mental_Disturb,
              a$Dlg_Supervisor)

#7- Perfil x Aplicação
tab7 <- table(a$Mental_Disturb,
              a$Freq_Interfere_Work)

#8- Perfil x Aplicação
tab8 <- table(a$Self_Employed,
              a$Gender)

#9- Perfil x Aplicação
tab9 <- table(a$Self_Employed,
              a$Tech_Ind_Preview_Health_Care_Options)

#10- Perfil x Aplicação
tab10 <- table(a$Tech_Ind_Preview_Health_Care_Options,
              a$Gender)

#11- Perfil x Aplicação
tab11 <- table(a$Tech_Ind_Preview_Health_Care_Options,
              a$Int_Health_Mental)

#12- Perfil x Aplicação
tab12 <- table(a$Int_Health_Mental,
              a$Gender)

#13- Perfil x Aplicação
tab13 <- table(a$Int_Health_Mental,
              a$Self_Employed)

#14- Perfil x Aplicação
tab14 <- table(a$Dlg_Supervisor,
              a$Int_Health_Mental)

#15- Perfil x Aplicação
tab15 <- table(a$Dlg_Supervisor,
               a$Tech_Ind_Preview_Health_Care_Options)

#16- Perfil x Aplicação
tab16 <- table(a$Dlg_Supervisor,
               a$Self_Employed)

#17- Perfil x Aplicação
tab17 <- table(a$Dlg_Supervisor,
               a$Gender)

#18- Perfil x Aplicação
tab18 <- table(a$Dlg_Coworker,
               a$Dlg_Supervisor)

#19- Perfil x Aplicação
tab19 <- table(a$Dlg_Coworker,
               a$Int_Health_Mental)

#20- Perfil x Aplicação
tab20 <- table(a$Dlg_Coworker,
               a$Tech_Ind_Preview_Health_Care_Options)

#21- Perfil x Aplicação
tab21 <- table(a$Dlg_Coworker,
               a$Self_Employed)

#22- Perfil x Aplicação
tab22 <- table(a$Dlg_Coworker,
               a$Gender)

#Matriz Binária e Matriz de Burt
#Padronizando um dataset somente com as variaveis de interesse para as matrizes
a.std <- as.data.frame(select(a,
                -Age, -Country, -Family_Historic, -Mental_Disturb_Previous, 
                -Seek_Treatment, -Tech_Ind_Provide_Health_Care,
                -Aware_Health_Care_Options, -Wellness_Program, -Help_Learn_Health_Care,
                -ID_Protected, -Leave_Emg, -Importance_Health_Physic, 
                -Productivity_Interfere_Affected, -Level_Productivity_Affected, 
                -Int_Health_Physic, -Tech_Ind_Suport, -Importance_Health_Mental,
                -Num_Workers_Ind, -Working_Tech_Ind,-Self_Employed))

str(a.std)
colSums(is.na(a.std))

#para o modelo rodar é necessario remover os missing values do dataset
a.std <- as.data.frame(a.std[!is.na(a.std$Dlg_Coworker),])
summary(a.std)

#Para se estabelecer uma matriz binária:
matriz_binaria <- getindicator(Xinput = a.std)
matriz_binaria 

CA(matriz_binaria)

# Para a matriz de Burt:
matriz_burt <- getBurt(Xinput = a.std)
matriz_burt

CA(matriz_burt)

# Rodando a ACM
##############################################################################
#MODELOS PREDITIVOS
##############################################################################






