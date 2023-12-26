
rm(list = ls())
#dev.off(dev.list()["RStudioGD"])
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos = "http://cran.us.r-project.org")
  sapply(pkg, require, character.only = TRUE)
}

ipak(c("rstudioapi")) 

ipak(c("readxl", "tidyverse", "data.table", "janitor", "sf", "openxlsx","RColorBrewer", "dplyr","spdep", 
       "ggplot2","classInt","cowplot", "googleway", "ggrepel", "readstata13","gghighlight","ggpubr",
       "ggspatial", "tidytext","stringr","tm",'tidyr',"wordcloud2","textdata","syuzhet","tidyselect","reshape2",
       "data.table","wordcloud", "sf", "rnaturalearth","rtweet","academictwitteR",
       "lubridate","rvest","RSelenium","sp","rgdal","fixest","aod", "stargazer"))


path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


# raw=read.xlsx("Base_R.xlsx")
# 
# names(raw)
# 
# table(raw$Barrio)
# 
# raw$eleccion1=raw$`Escoja.3.opciones.,.ordenándolas.de.tal.manera.que.1.sea.la.más.importante.y.3.la.menos.importante..1`
# raw$eleccion2=raw$`Escoja.3.opciones.,.ordenándolas.de.tal.manera.que.1.sea.la.más.importante.y.3.la.menos.importante..2`
# raw$eleccion3=raw$`Escoja.3.opciones.,.ordenándolas.de.tal.manera.que.1.sea.la.más.importante.y.3.la.menos.importante..3`
# 
# table(raw$`Escoja.3.opciones.,.ordenándolas.de.tal.manera.que.1.sea.la.más.importante.y.3.la.menos.importante..1`)
# 
# table(raw$`Escoja.3.opciones.,.ordenándolas.de.tal.manera.que.1.sea.la.más.importante.y.3.la.menos.importante..2`)
# 
# table(raw$`Escoja.3.opciones.,.ordenándolas.de.tal.manera.que.1.sea.la.más.importante.y.3.la.menos.importante..3`)
# 
# table(raw$eleccion1)
# raw$eleccion1=factor(raw$eleccion1)
# 
# raw$gusto1=0
# raw$gusto1[raw$eleccion1==" Cara"]=1
# raw$gusto1[raw$eleccion1==" Cuerpo"]=1
# raw$gusto1[raw$eleccion1==" Edad"]=1
# 
# 
# raw$gusto2=0
# raw$gusto2[raw$eleccion1==" Cara"]=1
# raw$gusto2[raw$eleccion1==" Cuerpo"]=1
# raw$gusto2[raw$eleccion1==" Edad"]=1
# raw$gusto2[raw$eleccion2==" Cara"]=1
# raw$gusto2[raw$eleccion2==" Cuerpo"]=1
# raw$gusto2[raw$eleccion2==" Edad"]=1
# 
# 
# 
# raw$gusto3=0
# raw$gusto3[raw$eleccion1==" Cara"]=1
# raw$gusto3[raw$eleccion1==" Cuerpo"]=1
# raw$gusto3[raw$eleccion1==" Edad"]=1
# raw$gusto3[raw$eleccion1=="Conexiones en comun"]=2
# raw$gusto3[raw$eleccion1=="Institucion educativa"]=2
# raw$gusto3[raw$eleccion1=="Lugar de trabajo"]=2
# 
# 
# raw$gusto4=0
# raw$gusto4[raw$eleccion1==" Cara"]=1
# raw$gusto4[raw$eleccion1==" Cuerpo"]=1
# raw$gusto4[raw$eleccion1==" Edad"]=1
# raw$gusto4[raw$eleccion1=="Conexiones en comun"]=2
# raw$gusto4[raw$eleccion1=="Institucion educativa"]=2
# raw$gusto4[raw$eleccion1=="Lugar de trabajo"]=2
# raw$gusto4[raw$eleccion2==" Cara"]=1
# raw$gusto4[raw$eleccion2==" Cuerpo"]=1
# raw$gusto4[raw$eleccion2==" Edad"]=1
# raw$gusto4[raw$eleccion2=="Conexiones en comun"]=2
# raw$gusto4[raw$eleccion2=="Institucion educativa"]=2
# raw$gusto4[raw$eleccion2=="Lugar de trabajo"]=2
# 
# 
# 


# 
# 
# summary(raw)
# table(raw$`3..¿Con.qué.finalidad.ha.buscado.hacer.match.en.Tinder?`)
# raw$finalidadsexo=0
# raw$finalidadsexo[raw$`3..¿Con.qué.finalidad.ha.buscado.hacer.match.en.Tinder?`=="Buscar un encuentro sexual"]=1
# 
# table(raw$finalidadsexo)
# 
# table(raw$Indique.su.genero)
# 
# raw$genero=factor(raw$Indique.su.genero)
# 
# raw$genero=factor(raw$Indique.su.genero)
# 
# raw$educacion=factor(raw$`¿Cuál.es.tu.nivel.educativo?(finalizado.o.en.curso)`)
# table(raw$educacion)
# 
# raw$edad=as.numeric(paste(raw$`¿Su.edad.es?`))
# 
# raw1=subset(raw,is.na(eleccion1)==F)
# write.csv(raw1,"tinder_transformados.csv")

raw=read.csv("tinder_transformados.csv")
names(raw)
table(raw$Match.con)

raw$gusto1=0
raw$gusto1[raw$eleccion1==" Cara"]=1
raw$gusto1[raw$eleccion1==" Cuerpo"]=1

table(raw$gusto1)
table(raw$eleccion1)

raw$gusto2=0
raw$gusto2[raw$eleccion1=="Contexto(fondo/escenario de la foto)"]=1
raw$gusto2[raw$eleccion1=="Institucion educativa"]=1
raw$gusto2[raw$eleccion1=="Lugar de trabajo"]=1
raw$gusto2[raw$eleccion2=="Contexto(fondo/escenario de la foto)"]=1
raw$gusto2[raw$eleccion2=="Institucion educativa"]=1
raw$gusto2[raw$eleccion2=="Lugar de trabajo"]=1
raw$gusto2[raw$eleccion3=="Contexto(fondo/escenario de la foto)"]=1
raw$gusto2[raw$eleccion3=="Institucion educativa"]=1
raw$gusto2[raw$eleccion3=="Lugar de trabajo"]=1


table(raw$gusto2)

raw$gusto3=0
raw$gusto3[raw$eleccion1==" Pose"]=1
raw$gusto3[raw$eleccion1=="Descripcion personal"]=1
raw$gusto3[raw$eleccion2=="Descripcion personal"]=1
raw$gusto3[raw$eleccion2==" Pose"]=1
raw$gusto3[raw$eleccion3=="Descripcion personal"]=1
raw$gusto3[raw$eleccion3==" Pose"]=1


table(raw$gusto3)


 

names(raw)
summary(raw)
#raw$etnia=raw$X.Con.que.grupo.étnico.se.identifica.
#table(raw$X.Cuál.es.tu.estado.civil.)
raw$ecivil=raw$�.Cu�.l.es.tu.estado.civil.
raw$soltero=0
raw$soltero[raw$�.Cu�.l.es.tu.estado.civil.=="Soltero"]=1
raw$gender=0
raw$gender[raw$genero=="Femenino"]=1
names(raw)

form11=formula(gusto1 ~ factor(genero) + factor(educacion)+ factor(Agerank))
form12=formula(gusto1 ~ factor(genero) + factor(educacion)+ factor(Agerank) +soltero)
form13=formula(gusto1 ~ factor(genero) + factor(educacion)+ factor(Agerank) +soltero+finalidad)

form21=formula(gusto2 ~ factor(genero) + factor(educacion)+ factor(Agerank))
form22=formula(gusto2 ~ factor(genero) + factor(educacion)+ factor(Agerank) +soltero)
form23=formula(gusto2 ~ factor(genero) + factor(educacion)+ factor(Agerank)+soltero +finalidad)

form31=formula(gusto3 ~ factor(genero) + factor(educacion)+ factor(Agerank))
form32=formula(gusto3 ~ factor(genero) + factor(educacion)+ factor(Agerank) +soltero)
form33=formula(gusto3 ~ factor(genero) + factor(educacion)+ factor(Agerank)+soltero +finalidad)


mylogit11 <- glm(form11, data = raw, family = "binomial")
mylogit21 <- glm(form21, data = raw, family = "binomial")
mylogit12 <- glm(form12, data = raw, family = "binomial")
mylogit22 <- glm(form22, data = raw, family = "binomial")
mylogit13 <- glm(form13, data = raw, family = "binomial")
mylogit23 <- glm(form23, data = raw, family = "binomial")
mylogit31 <- glm(form31, data = raw, family = "binomial")
mylogit32 <- glm(form32, data = raw, family = "binomial")
mylogit33 <- glm(form33, data = raw, family = "binomial")


stargazer(mylogit11,mylogit12,mylogit13, type="text")
stargazer(mylogit21,mylogit22,mylogit23, type="text")
stargazer(mylogit31,mylogit32,mylogit33, type="text")

stargazer(mylogit11,mylogit12,mylogit13, type="html", out="tablelogit1.html")
stargazer(mylogit21,mylogit22,mylogit23, type="html", out="tablelogit2.html")
stargazer(mylogit31,mylogit32,mylogit33, type="html", out="tablelogit3.html")



# x <- model.matrix(gender + education+ edad + soltero, raw)
# mylogit1 <- glm(form1, data = raw, family = "binomial")
# mylogit2 <- glm(form2, data = raw, family = "binomial")
# mylogit3 <- glm(form3, data = raw, family = "binomial")
# 
# 
# stargazer(mylogit1,mylogit2,mylogit3,type="text")
# 
# stargazer(mylogit1,mylogit2,mylogit3,type="html",out="table2logit.html")

stargazer(raw,type="html",out="tabledescriptivef.html")


summary(mylogit2)

## CIs using profiled log-likelihood
confint(mylogit13)

## CIs using standard errors
confint.default(mylogit13)

wald.test(b = coef(mylogit23), Sigma = vcov(mylogit13), Terms = 2:4)

## odds ratios only
exp(coef(mylogit))


## odds ratios and 95% CI
confint1=exp(cbind(OR = coef(mylogit13), confint(mylogit13)))
confint2=exp(cbind(OR = coef(mylogit23), confint(mylogit23)))
confint3=exp(cbind(OR = coef(mylogit33), confint(mylogit33)))

write.csv(confint1,"confint13.csv")
write.csv(confint2,"confint23.csv")
write.csv(confint3,"confint33.csv")
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

## view data frame
newdata1

newdata1$rankP <- predict(mylogit13, newdata = newdata1, type = "response")
newdata1
