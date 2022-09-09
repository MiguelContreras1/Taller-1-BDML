# PROBLEM SET 1 
#PUNTO 1
#Paquetes a usar
install.packages("pacman")
install.packages("rvest")
install.packages("dplyr")
install.packages("tibble")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("tidyverse")
install.packages("boot")
install.packages("caret")

library(caret)
library(dplyr)
library(boot)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(tibble)
library(dplyr)
library(pacman)
library(rvest)

#IMPORTAR DATOS
    #Lectura de URL
    url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html URL chunk 1"
    browseURL(url)
    #Lectura de cada chunck
    url_base <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")

    #loop para unir las bases
    df <- data.frame()
    for (url in url_base) {
      print(url)
      temp <- read_html(url) %>%
        html_table()
      temp <- as.data.frame(temp[[1]])
      df <- rbind(df, temp)
    }
    head(df)
    
#LIMPIEZA DE LA BASE
    #Restringirla solo para mayores de 18 años y que sean empleados 
    base=subset(df, df$age>17) #age variable edad
    base=subset(base, base$ocu==1)  #ocu variable ocupados
    
    #FIltro de variables 
    base2 <- select(base,college, maxEducLevel, age, estrato1, sex, regSalud, cotPension, 
                    ingtot, sizeFirm, microEmpresa, oficio, hoursWorkActualSecondJob, 
                    hoursWorkUsual, informal, relab )

#PRIMER PUNTO 

    #Revisión de missings de variables por individuo
    sum(is.na(base2$estrato1)) #0
    sum(is.na(base2$maxEducLevel)) #1
    sum(is.na(base2$college)) #0
    sum(is.na (base2$regSalud)) #1420
    sum(is.na(base2$cotPension )) #0
    
    #Revisión de missings de variables de trabajo
    sum(is.na(base2$sizeFirm)) #0
    sum(is.na(base2$microempresa)) #0
    sum(is.na(base2$oficio)) #0
    sum(is.na(base2$hoursWorkActualSecondJob))#15980
    sum(is.na(base2$hoursWorkUsual))#0
    sum(is.na(base2$informal)) #0
    sum(is.na(base2$relab))#0
    
    #Revisión de missings de variables candidatas para Ingreso 
    sum(is.na(base2$age)) #0
    sum(is.na(base2$sex)) #0
    sum(is.na(base2$p6500)) #4535
    sum(is.na(base2$ingtot)) #0
    sum(is.na(base2$maxEducLevel)) #1
    sum(is.na(base2$y_total_m)) #0

    #Reemplazo de los missings 
    
    base2$maxEducLevel = ifelse(is.na(base2$maxEducLevel)==T,1,base2$maxEducLevel) #reemplazar missings de educación
    base2$regSalud = ifelse(is.na(base2$regSalud)==T,0,base2$regSalud) #reemplazar missings de regimen de salud
    base2$hoursWorkActualSecondJob = ifelse(is.na(base2$hoursWorkActualSecondJob)==T,0,base2$hoursWorkActualSecondJob) #reemplazar losmissings de horas trabajadas segundo trabajo

    #Análisis descriptivo 
        #CRUCE DE VARIABLES 
    table(base2$regSalud, base2$maxEducLevel)
    table(base2$age, base2$regSalud)
    table(base2$regSalud, base2$sex)
    table(base2$regSalud, base2$cotPension)
    table(base2$regSalud, base2$maxEducLevel)
    table(base2$hoursWorkActualSecondJob, base2$sex)
    table(base2$hoursWorkActualSecondJob, base2$age)
    table(base2$hoursWorkActualSecondJob, base2$maxEducLevel)

        #Tabla estadísticas básicas para variables continuas
    ingreso <- (as.data.frame(summary(base2))) ; ingreso
    output <- capture.output(ingreso, file=NULL, append =FALSE)
    output_ad <-as.data.frame(output) #convertir summary en tabla
    write.table(x = output_ad, file = "summary.xlsx", sep = " ", 
                row.names = FALSE, col.names = TRUE)
        # Media de ingresos por grupos
    # por sexo
    a <- base2 %>% group_by(sex) %>% summarize(mean(base$ingtot,na.rm = T));a
    # por edad
    b <- base2 %>% group_by(age) %>% summarize(mean(ingtot,na.rm = T));b 
    # por estrato
    c <- base2 %>% group_by(estrato1) %>% summarize(mean(ingtot,na.rm = T));c
    
         #varianzas variables continuas
    base2 %>% var()
    var(base2$ingtot) "7.158984e+12"
    var(base2$age) " 181.7871"
    var(base2$hoursWorkActualSecondJob)"79.56467"
    var(base2$hoursWorkUsual)"241.5766"
    var(base2$sex)"0.249116"

          #diferencia de medias ingreso entre edades <57> y sexo
    d_medias_sex<- t.test (base2$ingtot ~ base2$sex ) ;d_medias_sex
    d_medias_age<- t.test (base2$ingtot ~ base2$age ) ;d_medias_age
    
          #Gráficos de estadísticas desciptivas
    #dif medias
    Grafico_dmedias <- boxplot(base2$ingtot ~ base2$sex, col= "gray", xlab ="sexo", ylab = "ingreso total")
    stargazer(type="text", title ="difmean gen", TRUE) #exportarlo 
    #histograma ingreso
    gp <-  ggplot() + geom_histogram(data = base2,aes(x=ingtot));gp
    #Scatter ingreso-edad
    gp2<- ggplot()+geom_point(data=base2, aes(y=ingtot, x=age));gp2
    #histograma edad
    gp3 <- ggplot() + geom_histogram(data = base2, aes(x=age));gp3
    #scatter ingreso-sex
    gp4<- ggplot()+geom_point(data=base2, aes(y=ingtot, x=age))+ "división sex"
    +ylab("Cantidad") + xlab("Sexo") + ggtitle("Cantidad de personas segun el sexo")+ 
      scale_x_discrete(limit = c("Hombre", "Mujer"))
   
    #grafico de dispersion del ingreso promedio por sexo
    grafico1 <- plot(a, main = "ingreso promedio por sexo", xlab = "sexo", ylab = "Ingreso promedio", pch = 21,  bg = "yellow", col = "red", cex = 1, lwd = 2)
    ggsave(plot= grafico1 , file = "views/Grafico22.jpeg") # puedes agregar los temas predeterminados para mejorar la apariencia dle grafico

    #Grafica de ingreso promedio por edad
    grafico2 <- plot(b,type="h",main = "ingreso promedio por edad", xlab = "Edad", ylab = "Ingreso promedio", col = "Darkblue",lwd=2, ylim=c(0,9000000),xlim=c(15,85))
    ggsave(plot= grafico2 , file = "views/Grafico33.jpeg") 

    #tabla de correlación
    correlacion <- (as.data.frame(cor(base2))) ; correlacion
    output_corr <- capture.output(correlacion, file=NULL, append =FALSE)
    output_corr <-as.data.frame(output_corr) #convertir summary en tabla
    write.table(x = output_corr, file = "summary.xlsx", sep = " ", 
                row.names = FALSE, col.names = TRUE)

#SEGUNDO PUNTO
    
    # crear variable "edad2"
    edad2 <- as.data.frame((base2$age^2))
    base2 <- cbind(base2, edad2)
    names(base2)[names(base2)=='(base2$age^2)']<- 'age2'
    
    # correr regresión 
    regresion1 <- lm(ingtot ~ age+age2, data= base2); regresion1
    summary(regresion1)
    
    # ver coeficiente de la regresión
    lm_summary = as.data.frame(summary(regresion1)$coefficients)
    
    #Intervalos de confianza de la predicción
    y_predicho<- predict(regresion1)
    base2 <- cbind(base2, y_predicho)
    confint(regresion1)

    #Errores bootstrap
    set.seed(12345)
    n<- length(base2$ingtot)
    R<-1000
    eta_mod1<- rep(0,R)
    eta_mod2<-rep(0,R)
    eta_mod3<-rep(0,R)
    y_predichos<- matrix(NA,n,R)
    for (i in 1:R){
      db_sample<- sample_frac(base2,size=1, replace=TRUE)
      f<- lm(ingtot~age+age2, db_sample)
      coefs<- f$coefficients
      
      eta_mod1[i]<- coefs[1]
      eta_mod2[i]<- coefs[2]
      eta_mod3[i]<- coefs[3]
      
    } 
    for (i in 1:R){
      columnas<- eta_mod1[i]+eta_mod2[i]*base2$age+eta_mod3[i]*base2$age2
      y_predichos[,i]<-columnas
    }
    ee<-rowSds(y_predichos)
    df2<-cbind(base2,ee)
    IC_bajo=base2$y_predicho-1.96*ee
    IC_alto=base2$y_predicho+1.96*ee
    
    base2<- cbind(base2,IC_alto, IC_bajo)

    #Gráfico y predicho modelo 1 con IC por bootstrap
    ggplot(base2, aes(age, y_predicho)) + geom_point() +                                
      geom_line(color = "dark green", size = 2) +
      geom_ribbon(aes(ymin=IC_bajo, ymax=IC_alto), alpha=0.1, fill = "green", 
                  color = "black", linetype = "dotted")
    #Hallar peak age 
    peak_y_predicho=(which.max(base2$y_predicho)) #2161715
    peak_age=(base2$age[113])#57
    base2[113,] "college maxEducLevel age estrato1 sex regSalud cotPension ingtot sizeFirm microEmpresa oficio
                 1            6  57        2   0        1          1 838333        5            0     55
              hoursWorkActualSecondJob hoursWorkUsual informal relab age2 y_predicho IC_alto IC_bajo
                             NA             48          0        1   3249    2161715 2260888 2062541"

#TERCER PUNTO

    #Estimación incondicional modelo de género
    #Hallar logaritmo del ingreso
    ingtot2 = base2$ingtot
    ingtot2<-ifelse((ingtot2)==0,1,ingtot2) #reemplazar los ingreso= 0 por 1
    log_ingtot<- log(ingtot2)
    base2<- cbind(base2,log_ingtot)
    #Estimar modelo
    regresion2<- lm(log_ingtot~sex, data=base2)
    lm_summary2=as.data.frame(summary(regresion2)$coefficients)

    #Estimación incondicional modelo age-género
    regresion3<- lm(log_ingtot~sex+age+age2, data=base2)
    lm_summary3=as.data.frame(summary(regresion3)$coefficients)
    y_predicho3<- predict(regresion3)
    base2<- cbind(base2, y_predicho3)
    sqrt(var(y_predicho3))#0.2793598
    IC_bajo3= base2$y_predicho3-1.96*0.2793598
    IC_alto3= base2$y_predicho3+1.96*0.2793598
    base2<- cbind(base2, IC_alto3)
    base2<- cbind(base2, IC_bajo3)

    #Gráfico con modelo y=age+age2+sex divido por género por CI normal
    ggplot(base2, aes(age, y_predicho3)) + geom_point() +                                
      geom_line(color = "dark green", size = 2) +
      geom_ribbon(aes(ymin=IC_bajo3, ymax=IC_alto3), alpha=0.1, fill = "green", 
                  color = "black", linetype = "dotted")+facet_grid(sex~.)
      #peak age hombres 
      base_hombres2= subset(base2, base$sex==1)
      peak_hombre2<-(which.max(base_hombres$y_predicho3))#16
      peak_age_hombre2=(base_hombres$age[16])#43
      as.integer(max(base_hombres$ingtot))#85.833.333
      base_hombres2[16,] 
      
      #peak age mujeres 
      base_mujeres2= subset(base2, base$sex==0)
      peak_mujeres2<-(which.max(base_mujeres$y_predicho3))#23
      peak_age_mujer2=(base_mujeres$age[23])#43
      as.integer(max(base_mujeres$ingtot))#40.000.000
      base_mujeres2[23,]"    college maxEducLevel age estrato1 sex regSalud cotPension  ingtot sizeFirm
             0            7  43        3   0        2          1 3150000        5
           microEmpresa oficio hoursWorkActualSecondJob hoursWorkUsual informal relab age2
                0     13                       NA             30        0     2 1849
          y_predicho IC_alto IC_bajo log_ingtot y_predicho2 holdout IC_alto2 IC_bajo2
           2004672 2074797 1934547   14.96291    13.54926   FALSE 14.07188 13.40277
          y_predicho3 IC_alto3  IC_bajo3
          13.73732     14.28487 13.18978"
      
      #peak age hombres con modelo de age 
      base_hombres= subset(base2, base$sex==1)
      peak_hombre<-(which.max(base_hombres$y_predicho))#141
      peak_age_hombre=(base_hombres$age[141])#57
      as.integer(max(base_hombres$ingtot))#85.833.333
      #peak age mujeres con modelo age 
      base_mujeres= subset(base2, base$sex==0)
      peak_mujeres<-(which.max(base_mujeres$y_predicho))#57
      peak_age_mujer=(base_mujeres$age[57])#57
      as.integer(max(base_mujeres$ingtot))#40.000.000
      
      #Gráfico con modelo y=age+age2+sex divido por género por CI normal
      ggplot(base2, aes(age, y_predicho)) + geom_point() +                                
        geom_line(color = "dark green", size = 2) +
        geom_ribbon(aes(ymin=IC_bajo3, ymax=IC_alto3), alpha=0.1, fill = "green", 
                    color = "black", linetype = "dotted")+facet_grid(sex~.)
      
      #Errores bootstrap modelo y~age+age2+sex
      eta_mod11<-rep(0,R)
      eta_mod22<-rep(0,R)
      eta_mod33<-rep(0,R)
      eta_mod44<-rep(0,R)
      
      set.seed(12345)
      n<- length(base2$log_ingtot)
      R<-1000
      y_predichos2<- matrix(NA,n,R)
      for (i in 1:R){
        db_sample<- sample_frac(base2,size=1, replace=TRUE)
        f2<- lm(log_ingtot~sex+age+age2, db_sample)
        coefs2<- f2$coefficients
        
        eta_mod11[i]<- coefs2[1]
        eta_mod22[i]<- coefs2[2]
        eta_mod33[i]<- coefs2[3]
        eta_mod44[i]<- coefs2[4]
        
      } 
      for (i in 1:R){
        columnas2<- eta_mod11[i]+eta_mod22[i]*base2$sex+eta_mod33*base2$age+eta_mod44*base2$age2
        y_predichos2[,i]<-columnas2
      }
      ee2<-rowSds(y_predichos2)
      df22<-cbind(base2,ee2)
      IC_bajo2=base2$y_predicho3-1.96*ee2
      IC_alto2=base2$y_predicho3+1.96*ee2
      
      base2<- cbind(base2,IC_alto2, IC_bajo2)
      
      #Gráfico con modelo y=age+age2+sex divido por género por bootstrap
      ggplot(base2, aes(age, y_predicho3)) + geom_point() +                                
        geom_line(color = "dark green", size = 2) +
        geom_ribbon(aes(ymin=IC_bajo2, ymax=IC_alto2), alpha=0.1, fill = "green", 
                    color = "black", linetype = "dotted")+facet_grid(sex~.)

      #peak age hombres modelo y=age+age2+sex por bootstrap
      base_hombres2= subset(base2, base$sex==1)
      peak_hombre2<-(which.max(base_hombres$y_predicho3))#16
      peak_age_hombre2=(base_hombres$age[16])#43
      as.integer(max(base_hombres$ingtot))#85.833.333
      
      #peak age mujeres modelo y=age+age2+sex por bootstrap
      base_mujeres2= subset(base2, base$sex==0)
      peak_mujeres2<-(which.max(base_mujeres$y_predicho3))#23
      peak_age_mujer2=(base_mujeres$age[23])#43
      as.integer(max(base_mujeres$ingtot))#40.000.000


      #regresion condicional
      #denominar dummies como factores
      base2$maxEducLevel<-as.factor(base2$maxEducLevel)
      base2$estrato1<-as.factor(base2$estrato1)
      base2$regSalud <-as.factor(base2$regSalud)
      base2$cotPension<-as.factor(base2$cotPension)
      base2$sizeFirm<-as.factor(base2$sizeFirm)
      base2$oficio<-as.factor(base2$ofici)
      base2$informal<-as.factor(base2$informal)
      base2$relab<-as.factor(base2$relab)
      
      #estimación modelo condicional
      regresion4<- lm(log_ingtot~sex+maxEducLevel+age+age2+estrato1+regSalud+cotPension
                      +sizeFirm+oficio+hoursWorkActualSecondJob+hoursWorkUsual+informal
                      +relab, data=base2)
      lm_summary4=as.data.frame(summary(regresion4$coefficients))
      stargazer(regresion4,regresion3, regresion2, regresion1,type="text")
      
      #FWL
      base2<-base2 %>% mutate(res_y_a=lm(log_ingtot~maxEducLevel+age+age2+estrato1+regSalud
                                         +cotPension+sizeFirm+oficio+hoursWorkActualSecondJob
                                         +hoursWorkUsual+informal+relab,base2)$residuals, #Residuals con ingreso
                              res_s_a=lm(sex~sex+maxEducLevel+age+age2+estrato1+regSalud
                                         +cotPension+sizeFirm+oficio+hoursWorkActualSecondJob
                                         +hoursWorkUsual+informal+relab,base2)$residuals, #Residuals con sexo
      )
      
      regresion5<-lm(res_y_a~res_s_a-1,base2)
      
      #boostrap FWL
      
      set.seed(12345)
      eta.fn<-function(base2,i){
        coef(lm(res_y_a~res_s_a-1,base2, subset = i))
      }
      replicas<- boot(data = base2, statistic = eta.fn, R = 1000)

#PUNTO 4 
    #división de muestra
    set.seed(12345) #sets a seed
    base2 <- base2 %>%
      mutate(holdout= as.logical(1:nrow(base2) %in%
                                   sample(nrow(base2), nrow(base2)*.2))
      )
    test<-base2[base2$holdout==T,]
    train<-base2[base2$holdout==F,]
    
    
#Especificaciones 

    #1
    especificacion1= lm(ingtot~age+age2, data=train) 
    summary(especificacion1)
    #MSE especificación 1
    test$especificacion1<-predict(especificacion1,newdata = test)
    with(test,mean((ingtot-especificacion1)^2)) #5.766696e+12
    
    #2  
    especificacion2= lm(log_ingtot~sex, data=train) 
    summary(especificacion2)
    test$especificacion2<-predict(especificacion2,newdata = test)
    #MSE especificación 2
    with(test,mean((log_ingtot-especificacion2)^2)) #3.86366 MSE 
    
    #3 
    especificacion3 <-lm(log_ingtot~age+age2+sex+sex:age,data=train)
    test$especificacion3<-predict(especificacion3,newdata = test)
    #MSE especificación 3
    with(test,mean((log_ingtot-especificacion3)^2)) #3.794644 MSE 
    
    #4
    especificacion4 <-lm(log_ingtot~sex+maxEducLevel+age+age2+estrato1+regSalud+cotPension+sizeFirm+oficio+
                           hoursWorkActualSecondJob+hoursWorkUsual+informal+relab,data=train)
    test$especificacion4<-predict(especificacion4,newdata = test)
    #MSE especificación 4
    with(test,mean((log_ingtot-especificacion4)^2)) #MSE 1.119276
    
    #5
    especificacion5 <-lm(log_ingtot~sex+maxEducLevel+age+age2+estrato1+regSalud+cotPension+
                           sizeFirm+oficio+hoursWorkActualSecondJob+hoursWorkUsual+
                           informal+relab+ sex:maxEducLevel+ sex:age+sex:oficio+sex:informal,data=train)
    test$especificacion5<-predict(especificacion5,newdata = test)
    #MSE especificación 5
    with(test,mean((log_ingtot-especificacion5)^2))#MSE 1.12227
    
    #6
    especificacion6 <-lm(log_ingtot~sex+maxEducLevel+age+age2+estrato1+regSalud+cotPension+
                           sizeFirm+oficio+hoursWorkActualSecondJob+informal+
                           relab+ sex:maxEducLevel +sex:age+sex:informal+
                           hoursWorkUsual: maxEducLevel+ hoursWorkUsual,data=train)
    test$especificacion6<-predict(especificacion6,newdata = test)
    #MSE especificación 6
    with(test,mean((log_ingtot-especificacion6)^2)) #MSE 1.117673
    
    #7
    especificacion7 <-lm(log_ingtot~sex+age+age2+estrato1+regSalud+cotPension+
                           sizeFirm+oficio+hoursWorkActualSecondJob+hoursWorkUsual+informal+
                           relab+sex:oficio+sex:regSalud+sex:informal+ sex:oficio+
                           age:sex+hoursWorkUsual:maxEducLevel+hoursWorkUsual:relab+
                           hoursWorkUsual:informal+regSalud:cotPension+oficio:relab,data=train)
    
    test$especificacion7<-predict(especificacion7,newdata = test)
    #MSE especificación 7
    with(test,mean((log_ingtot-especificacion7)^2)) #MSE 1.175428
    
    #8
    especificacion8 <-lm(log_ingtot~sex+maxEducLevel+age+age2+estrato1+regSalud+cotPension+
                           sizeFirm+oficio+hoursWorkActualSecondJob+hoursWorkUsual+informal+
                           relab + hoursWorkActualSecondJob:maxEducLevel+
                           hoursWorkActualSecondJob:informal+ hoursWorkActualSecondJob:oficio+
                           hoursWorkActualSecondJob:relab,data=train)
    
    test$especificacion8<-predict(especificacion8,newdata = test)
    #MSE especificación 8
    with(test,mean((log_ingtot-especificacion8)^2)) #MSE 2.942313
    
    #9
    especificacion9 <-lm(log_ingtot~sex+maxEducLevel+age+age2+estrato1+regSalud+cotPension+
                           sizeFirm+oficio+hoursWorkActualSecondJob+informal+
                           relab+ poly(maxEducLevel,2) + hoursWorkActualSecondJob:maxEducLevel+
                           hoursWorkActualSecondJob:informal+ hoursWorkActualSecondJob:oficio+
                           hoursWorkActualSecondJob:relab + poly(hoursWorkUsual,2) + 
                           cotPension:regSalud,data=train)
    test$especificacion9<-predict(especificacion9,newdata = test)
    #MSE especificación 9
    with(test,mean((log_ingtot-especificacion9)^2)) #MSE 2.921648 
    


    #Obs que el modelo no predijo
    #modelo ganador estimado en test
    especificacion6_test<-lm(log_ingtot~sex+maxEducLevel+age+age2+estrato1+regSalud+cotPension+
                                  sizeFirm+oficio+hoursWorkActualSecondJob+informal+
                                  relab+ sex:maxEducLevel +sex:age+sex:informal+hoursWorkUsual+
                                  hoursWorkUsual: maxEducLevel,data=test)
    #errores predichos modelo ganador
    predichos_modelo6<- predict(especificacion6_test)
    errores<- test$log_ingtot-predichos_modelo6
    
    plot1<- ggplot(data=test, aes(log_ingtot, predichos_modelo6))+
      geom_point()+geom_smooth(color="firebrick")+
      theme_bw()

    #Influencia estadística
    lm.inf <- lm.influence(especificacion6_test)
    hat<-lm.inf$hat
    influecia<-(predichos_modelo6)/(1+hat)
    hist(influecia)
    summary(influecia) #     Min.  1st Qu.  Median    Mean   3rd Qu.    Max. 
                       #    0.5296 13.0051  13.5971  13.3210 14.0030  15.9767 
    sd(influecia)      #    Desviación estándar 1.710525 

    #gráfico de distribucion de influencia 
    test<- cbind(test, influecia)
    plot2<- ggplot(data=test, aes(log_ingtot, influecia))+
      geom_point()+
      theme_bw()



    #LOOV
    CV_especificacion6 <- train(log_ingtot ~ age+age2+sex,
                    # model to fit
                    data = base2,
                    trControl = trainControl(method = "cv", number = 5), method = "lm")

    CV_especificacion7<- train(log_ingtot ~ age+age2+sex,
                                # model to fit
                                data = base2,
                                trControl = trainControl(method = "cv", number = 5), method = "lm")
    
    # fit a simple regression
model2
"Linear Regression 
        
        16542 samples
            3 predictor
        
        No pre-processing
        Resampling: Cross-Validated (5 fold) 
        Summary of sample sizes: 13234, 13233, 13234, 13234, 13233 
        Resampling results:
        
          RMSE      Rsquared   MAE      
          1.936076  0.0201174  0.8491334
        
        Tuning parameter 'intercept' was held constant at a value of TRUE"



# Fit the model at the mean 
ggplot(dat, aes(y = base2$log_ingtot, x = age)) +
  geom_point() +
  geom_line(data = mean_obs2, aes(x = temp, y = y_hat, 
                                  color = "with controls"), size = 1) +
  stat_smooth(formula = 'y ~ x+ x2', method = lm, se = FALSE, 
              aes(color = "without controls"), 
              size = 1) +
  theme_bw() +
  labs(x = "Edad", 
       y = "Logaritmo del ingreso total",
       title = "Valores predichos de la semielasticidad del ingreso respecto a la edad") +
  scale_color_manual(name = "Model", values = c("red", "blue"))



#Temas pendientes

#como exportar
#varianza




