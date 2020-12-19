#Libraries
library(WRS2)
library(ggplot2)
library(nplr)
library(plotly)
library(tidyverse)
library(dplyr)
library(arrow)
library(ggplot2)
library(nplr)
library(dplyr)
library(purrr)
library(stringr)
library(R.utils)



#Upload table with number of cases per each infectious disease
tabla<-read_parquet(file.choose(),h=TRUE)

#Upload table with death tall data
mortalidad<-read_parquet(file.choose())

#Grouping by infectious disease
malaria<-subset(tabla,ENFERMEDAD=="MALARIA")
chikungunya<-subset(tabla,ENFERMEDAD=="CHIKUNGUNYA")
ira<-subset(tabla,ENFERMEDAD=="IRA")
leishmaniasis<-subset(tabla,ENFERMEDAD=="LEISHMANIASIS")
chagas<-subset(tabla,ENFERMEDAD=="CHAGAS")
zika<-subset(tabla,ENFERMEDAD=="ZIKA")

# Homocedasticity analysis
bartlett.test(TOTAL_MES~DEPARTAMENTO,data=malaria)
bartlett.test(TOTAL_MES~DEPARTAMENTO,data=zika)
bartlett.test(TOTAL_MES~DEPARTAMENTO,data=chikungunya)
bartlett.test(TOTAL_MES~DEPARTAMENTO,data=chagas)
bartlett.test(TOTAL_MES~DEPARTAMENTO,data=ira)
bartlett.test(TOTAL_MES~DEPARTAMENTO,data=leishmaniasis)


#Data Transformation
tabla$Ln1<-tabla$TOTAL_MES+1
tabla$Ln<-log(tabla$Ln1)

tabla$Ln2<-asin(tabla$Ln1/1000)
tabla$Ln2

bartlett.test(Ln~DEPARTAMENTO,data=malaria)
bartlett.test(Ln~DEPARTAMENTO,data=zika)
bartlett.test(Ln~DEPARTAMENTO,data=chikungunya)
bartlett.test(Ln~DEPARTAMENTO,data=chagas)
bartlett.test(Ln~DEPARTAMENTO,data=ira)
bartlett.test(Ln~DEPARTAMENTO,data=leishmaniasis)

bartlett.test(Ln2~DEPARTAMENTO,data=malaria)
bartlett.test(Ln2~DEPARTAMENTO,data=zika)
bartlett.test(Ln2~DEPARTAMENTO,data=chikungunya)
bartlett.test(Ln2~DEPARTAMENTO,data=chagas)
bartlett.test(Ln2~DEPARTAMENTO,data=ira)
bartlett.test(Ln2~DEPARTAMENTO,data=leishmaniasis
              
              #Two way robust ANOVA analysis for all data and subgroups for number of cases for each infectious disease
              mavo<-t2way(TOTAL_MES~DEPARTAMENTO*IPS_1M_P_AÑO,data=malaria)
              cavo<-t2way(TOTAL_MES~DEPARTAMENTO*IPS_1M_P_AÑO,data=chikungunya)
              iavo<-t2way(TOTAL_MES~DEPARTAMENTO*IPS_1M_P_AÑO,data=ira)
              lavo<-t2way(TOTAL_MES~DEPARTAMENTO*IPS_1M_P_AÑO,data=leishmaniasis)
              c2avo<-t2way(TOTAL_MES~DEPARTAMENTO*IPS_1M_P_AÑO,data=chagas)
              zavo<-t2way(TOTAL_MES~DEPARTAMENTO*IPS_1M_P_AÑO,data=zika)
              tavo<-t2way(TOTAL_MES~DEPARTAMENTO*IPS_1M_P_AÑO,data=tabla)
              
              summary(mavo)
              summary(cavo)
              summary(iavo)
              summary(lavo)
              summary(c2avo)
              summary(zavo)
              summary(tavo)
              
              
              #Two way Robust ANOVA for death toll     
              
              #Change NaN for 0 for ANOVA analysis
              df2<-mortalidad %>% replace(is.na(.), 0)
              VCTaov<-t2way(TOTAL_MUERTES_VECTORES~DEPARTAMENTO*IPS_1M,data=df2)    
              Iaov<-aov(TOTAL_MUERTES_IRA~DEPARTAMENTO*IPS_1M,data=df2)
              
              #Tukey Test
              TukeyHSD(VCTaov)
              
              #Heatmap creation
              
              #Upload Matrix with p-value from Tukey Test
              matrixMV<-read.table(file.choose())
              
              departamentos<-c("Amazonas","Antioquia","Arauca","Atlantico","Bogota","Bolivar","Boyaca","Caldas","Caqueta","Casanare","Cauca","Cesar","Choco","Cundinamarca","Cordoba","Guania","Guaviare","Huila","Guajira","Madgalena","Meta","Nariño","Norte de Santander","Putumayo","Quindio","Risaralda","San Andres y Providencia","Santander","Sucre","Tolima","Valle del Cauca","Vaupes","Vichada")
              dimnames(matrixMV)<-list(departamentos,departamentos)
              matrixMV
              
              matrixMV%>% 
                as.data.frame() %>%
                rownames_to_column("departamentos") %>%
                pivot_longer(-c(departamentos), names_to = "Departamentos", values_to = "p_value")%>%
                ggplot(aes(x=departamentos, y=Departamentos, fill=p_value)) +   
                geom_raster() +
                scale_fill_distiller(palette = "Spectral")+
                labs(title="Mean differences for Death Toll caused by infectious diseases transmitted by vectors ",y="Departments",x="Departments")+
                theme(plot.title=element_text(hjust=0.5,face="bold"),axis.text.x=element_text(angle=90,vjust=0.5))
              
              
              
              
              
              