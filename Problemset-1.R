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
        install.packages("matrixStats") 
        library(matrixStats)
        library(rowSds)
        library(boot)
        library(tidyverse)
        library(stargazer)
        library(ggplot2)
        library(tibble)
        library(dplyr)
        library(pacman)
        library(rvest, tidyverse)
        #
    #IMPORTAR DATOS
      #Lectura de URL
        url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html URL chunk 1"
        browseURL(url)
      
      url_base <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")
      print(url_base)
        #[1] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
        #[2] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html"
        #[3] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html"
        #[4] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html"
        #[5] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html"
        #[6] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html"
        #[7] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html"
        #[8] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html"
        #[9] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html"
        #[10] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html"
        
      
      #loop para unir la base en df
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
      #Restringirlo solo para age>=18 y empleado 
        base=subset(df, df$age>17)
        base=subset(base, base$ocu==1) #¿ocu (2mil +) o p6240? con ocu se tienen personas ocupadas no remuneradas
        #lista <- as.list(c(base$college, base$Educlevel, base$age, base$estrato1, base$sex, base$regSalud, base$cotPension, base$ingtot, base$sizeFirm, base$microempresa, base$oficio, base$hoursWorkActualSecondJob, base$hoursWorkUsual, base$informal, base$relab))
        #lista2 <- as.list(c('college', 'Educlevel', 'age', 'estrato1', 'sex', 'regSalud', 'cotPension', 'ingtot', 'sizeFirm','microempresa', 'oficio', 'hoursWorkActualSecondJob', 'hoursWorkUsual', 'informal', 'relab'))
        #lista3 <- c(college, maxEducLevel, age, estrato1, sex, regSalud, cotPension, ingtot, sizeFirm, microempresa, oficio, hoursWorkActualSecondJob, hoursWorkUsual, informal, relab)
       
      #FIltro de variables 
        base2 <- select(base,college, maxEducLevel, age, estrato1, sex, regSalud, cotPension, ingtot, sizeFirm, microEmpresa, oficio, hoursWorkActualSecondJob, hoursWorkUsual, informal, relab )
        base2=base2[-c(1),]
        
        #Caracteristicas persona
            #college
            #Educlevel 
            #age
            #estrato1
            #sex
            #regSalud
            #cotPension
        #Variables ingreso 
            #p6585S2a2 Subsidio de transporte
            #p6630S1a1 Prima de servicios (12 meses)
            #p7510S6a1 cesantías (12 meses)
            #p6630S2a1 prima navidad (12 meses)
            #p6500 ingreso antes de descuentos cuánto ganó el mes pasado en este empleo 
            #p6585S3 Subsidios 
            #impa ingreso monetario de la primera actividad antes de imputación
            #isa ingreso monetario de la segunda actividad antes de imputación
            #ingtotes ingreso total imputado
            #ingtot ingreso total
            #ingtotob Ingreso total observado
            #y_salary_m 
            #y_ingLab_m
            #y_total_m
            #y_total_m_ha
            #p6500 ingreso antes de descuentos cuánto ganó el mes pasado en este empleo 
            #p6585S3 Subsidios 
            #p6590S1 
            #p6585S2a2 Subsidio de transporte
            #p6240 actividad qué ocupó la mayor parte del tiempo la semana pasada
          
        #Características trabajo/empresa
            #sizeFirm
            #microempresa
            #oficio (qué hace)
            #hoursWorkActualSecondJob
            #hoursWorkUsual
            #informal
            #relab
        
        #mPIEZA DE MISSIINGS VALUES  
        
          #individuo
          sum(is.na(base2$estrato1)) #0
          sum(is.na(base2$maxEducLevel)) #1
          sum(is.na(base2$college)) #0
          sum(is.na (base2$regSalud)) #1420
          sum(is.na(base2$cotPension )) #0
          
          #trabajo
          sum(is.na(base2$sizeFirm)) #0
          sum(is.na(base2$microempresa)) #0
          sum(is.na(base2$oficio)) #0
          sum(is.na(base2$hoursWorkActualSecondJob))#15980
          sum(is.na(base2$hoursWorkUsual))#0
          sum(is.na(base2$informal)) #0
          sum(is.na(base2$relab))#0
         
          #Ingreso 
          sum(is.na(base2$age)) #0
          sum(is.na(base2$sex)) #0
          sum(is.na(base2$p6500)) #4535
          sum(is.na(base2$ingtot)) #0
          sum(is.na(base2$maxEducLevel)) #1
          sum(is.na(base2$y_total_m)) #0
          
          #Eliminarlos /reemplazarlos 
          base2 = subset(x = base2, subset = is.na(maxEducLevel)==FALSE) #eliminarlo
          base2$regSalud = ifelse(is.na(base2$regSalud)==T,0,base2$regSalud) #reemplazarlos
          base2$hoursWorkActualSecondJob = ifelse(is.na(base2$hoursWorkActualSecondJob)==T,0,base2$hoursWorkActualSecondJob) #reemplazarlos
      
        #CRUCES DE VARIABLES 
          table(base2$regSalud, base2$maxEducLevel)
            "           1    3    4    5    6    7
                   1   43  362  884 1104 3876 6080
                   2    0    2   12   14  103  290
                   3   70  313  484  517  753  214"
          table(base2$age, base2$regSalud)
          " EDAD   1   2   3  #1 CONTRIBUTIVO 2 ESPECIAL 3 SUBSIDIADO 
              18  86   4  29
              19 161   6  52
              20 228   3  41
              21 252   2  39
              22 264   7  44
              23 343   4  47
              24 376   5  44
              25 389   9  48
              26 392   3  39
              27 385   5  59
              28 376  12  38
              29 388   8  48
              30 354  12  44
              31 327  14  40
              32 344  24  38
              33 306  19  47
              34 328  13  50
              35 326  18  42
              36 332   9  52
              37 296   9  53
              38 308  18  61
              39 284   3  47
              40 306  11  45
              41 291   8  30
              42 250   8  45
              43 252  10  50
              44 258   5  69
              45 232   8  44
              46 230   6  48
              47 251  11  48
              48 236  10  59
              49 231   4  53
              50 227  12  60
              51 227  13  41
              52 209   9  57
              53 225   7  55
              54 218   9  54
              55 215  11  50
              56 202   8  56
              57 171   6  42
              58 173   7  49
              59 143   7  36
              60 130   6  41
              61 150   7  39
              62  86   6  40
              63  93   4  34
              64  77   4  30
              65  73   2  30
              66  61   2  20
              67  48   3  20
              68  41   2   8
              69  30   1  16
              70  29   0  12
              71  22   4  11
              72  24   1  19
              73  20   0   8
              74   7   0   7
              75   9   0   5
              76  13   0   3
              77  10   0   4
              78   3   0   5
              79   5   0   2
              80  10   1   0
              81   1   0   1
              82   2   0   0
              83   2   0   1
              84   1   0   1
              85   2   0   0
              86   3   0   0
              87   2   1   1
              90   1   0   0
              91   1   0   0
              93   1   0   0
              94   1   0   0
            > "
          table(base2$regSalud, base2$sex)
        " SALUD   0    1   #SEXO
              1 5912 6438
              2  189  232
              3 1173 1178"
          table(base2$regSalud, base2$cotPension)
          "      1    2    3
              1 9039 2992  319  
              2  289   72   60
              3   61 2290    0 "
          table(base2$regSalud, base2$maxEducLevel)
          "      1    3    4    5    6    7  #1 Ninguna #2 preescolar #3PI #4 PC #5 SI #6 SC #7terciaria
            1   43  362  884 1104 3876 6080   #Proporciones 
            2    0    2   12   14  103  290
            3   70  313  484  517  753  214"
          table(base2$hoursWorkActualSecondJob, base2$sex)
        "horas  0  1  #sexo
            1   9  7
            2  19 16
            3  15 13
            4  21 24
            5  22 13
            6  29 17
            7   5  4
            8  35 24
            9   5  3
            10 27 24
            11  2  0
            12 18 15
            13  0  1
            14 13 10
            15 13 17
            16  6  9
            18  9  7
            20 21 22
            21  2  2
            23  1  0
            24  2 12
            25  2  3
            26  0  1
            28  4  4
            30  7  5
            33  2  0
            34  0  1
            35  2  0
            36  4  5
            40  1  2
            42  2  0
            48  0  1
            50  2  0"
          table(base2$hoursWorkActualSecondJob, base2$age) #gráfico
          table(base2$hoursWorkActualSecondJob, base2$maxEducLevel)
      
          "    1  3  4  5  6  7
          1   0  0  0  3  4  9
          2   0  0  2  5  9 19
          3   1  0  1  2  7 17
          4   1  0  3  1  8 32
          5   0  1  2  3  9 20
          6   0  1  3  3 14 25
          7   0  2  0  0  4  3
          8   0  5  6  4  7 37
          9   0  1  0  0  3  4
          10  0  2  2  8 18 21
          11  0  0  0  1  0  1
          12  0  0  4  4  6 19
          13  0  0  0  0  0  1
          14  2  2  0  2  5 12
          15  0  3  0  2  6 19
          16  0  0  0  1  4 10
          18  1  0  1  5  5  4
          20  0  0  5  2  6 30
          21  0  0  1  1  1  1
          23  0  1  0  0  0  0
          24  0  0  3  1  5  5
          25  0  0  0  0  0  5
          26  0  0  0  0  0  1
          28  0  0  0  0  6  2
          30  0  0  1  2  1  8
          33  0  0  0  0  0  2
          34  0  0  1  0  0  0
          35  0  0  0  0  1  1
          36  0  0  1  0  3  5
          40  0  0  0  0  1  2
          42  0  1  0  0  0  1
          48  0  0  0  0  0  1
          50  0  0  0  0  1  1"
          
           #Análisis descriptivo
          
          ingreso <- (as.data.frame(summary(base2))) ; ingreso
          output <- capture.output(ingreso, file=NULL, append =FALSE)
          output_ad <-as.data.frame(output) #convertir summary en tabla
          write.table(x = output_ad, file = "summary.xlsx", sep = " ", 
                      row.names = FALSE, col.names = TRUE)
          
          
          #outliers
          
          rp.outlier(base2[base2$ingtot=="2 Pints", "attractiveness"])
          is.numeric(base2$ingtot)
          
          
          #diferencia de medias
          
          dmedias<- t.test (base2$ingtot ~ base2$sex ) ;dmedias
          Grafico_dmedias <- boxplot(base2$ingtot ~ base2$sex, col= "gray", xlab ="sexo", ylab = "ingreso total")
          #diferencia de medias para edades >50 y <50 
          stargazer(type="text", title ="difmean gen", TRUE) #exportarlo 
          
          gp <-  ggplot() + geom_histogram(data = base2,aes(x=ingtot));gp
          gp2<- ggplot()+geom_point(data=base2, aes(y=ingtot, x=age));gp2
          g<- plot(base2$age, base2$ingtot)
          
          grafico1 <- ggplot() + geom_histogram(data = base2, aes(x=age));grafico1
          
            ylab("Cantidad") + xlab("Sexo") + ggtitle("Cantidad de personas segun el sexo")+ 
            scale_x_discrete(limit = c("Hombre", "Mujer"))
          
          #varianzas
          lapply(base2[])
          
          base2 %>% var()
          var(base2$ingtot)
          "7.158984e+12"
          var(base2$age)
          " 181.7871"
          var(base2$hoursWorkActualSecondJob)
          "79.56467"
          var(base2$hoursWorkUsual)
          "241.5766"
          
          ## Media de ingresos
          # por sexo
          a <- base2 %>% group_by(sex) %>% summarize(mean(base$ingtot,na.rm = T));a
          "0  1769379
           1  1769379"
          # por edad
          b <- base2 %>% group_by(age) %>% summarize(mean(ingtot,na.rm = T));b 
          "      18                   697774.
                 19                   793232.
                 20                   822194.
                 21                   960762.
                 22                  1035891.
                 23                  1115837.
                 24                  1213093.
                 25                  1412683.
                 26                  1485837.
                 27                  1414338."
          # por estrato
          c <- base2 %>% group_by(estrato1) %>% summarize(mean(ingtot,na.rm = T));c
                 "1                   926645.
                  2                  1131979.
                  3                  1636072.
                  4                  3866322.
                  5                  5622865.
                  6                  9076656."
         
           #graficas
          
          #grafico de dispersi?n del ingreso promedio por sexo
          grafico1 <- plot(a, main = "ingreso promedio por sexo", xlab = "sexo", ylab = "Ingreso promedio", pch = 21,  bg = "yellow", col = "red", cex = 1, lwd = 2)
          ggsave(plot= grafico1 , file = "views/Grafico22.jpeg") # puedes agregar los temas predeterminados para mejorar la apariencia dle grafico
          
          #Grafica de ingreso promedio por edad
          grafico2 <- plot(b,type="h",main = "ingreso promedio por edad", xlab = "Edad", ylab = "Ingreso promedio", col = "Darkblue",lwd=2, ylim=c(0,9000000),xlim=c(15,85))
          ggsave(plot= grafico2 , file = "views/Grafico33.jpeg") # puedes agregar los temas predeterminados para mejorar la apariencia dle grafico



#PUNTO 2 
          
          # crear variable "edad2"
          edad2 <- as.data.frame((base2$age^2))
          base2 <- cbind(base2, edad2)
          names(base2)[names(base2)=='(base2$age^2)']<- 'age2'
          
          # correr regresión 
          regresion1 <- lm(ingtot ~ age+age2, data= base2); regresion1
          summary(regresion1)
          
                "Residuals:
                  Min       1Q   Median       3Q      Max 
                -2161715 -1080907  -546251    55041 83828662 
                
                Coefficients:
                             Estimate Std. Error     t    value Pr(>|t|)    
                (Intercept) -436662.9   178347.2  -2.448     0.0144 *  
                  age         91143.5     8886.4  10.256    < 2e-16 ***
                  age2         -799.3      102.9  -7.771    8.24e-15 ***
                  ---
                  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                
                Residual standard error: 2653000 on 16539 degrees of freedom
                Multiple R-squared:  0.01716,	Adjusted R-squared:  0.01704 
                F-statistic: 144.4 on 2 and 16539 DF,  p-value: < 2.2e-16"
          
          # ver coeficiente de la regresión
          lm_summary = as.data.frame(summary(regresion1)$coefficients)
          
          #Intervalos de confianza de la predicción
          y_predicho<- predict(regresion1)
          base2 <- cbind(base2, y_predicho)
          confint(regresion1)
                        "     2.5 %    97.5 %
              (Intercept) -786242.586  -87083.27
              age           73725.129  108561.79
              age2          -1000.862  -597.66"
          
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
              
              #Gráfico
              ggplot(base2, aes(age, y_predicho)) + geom_point() +                                
                geom_line(color = "dark green", size = 2) +
                geom_ribbon(aes(ymin=IC_bajo, ymax=IC_alto), alpha=0.1, fill = "green", 
                            color = "black", linetype = "dotted")
              
              peak_y_predicho=(which.max(base2$y_predicho)) #2161715
              peak_age=(base2$age[113])#57
              base2[113,]
              "college maxEducLevel age estrato1 sex regSalud cotPension ingtot sizeFirm microEmpresa oficio
                 1            6  57        2   0        1          1 838333        5            0     55
              hoursWorkActualSecondJob hoursWorkUsual informal relab age2 y_predicho IC_alto IC_bajo
                             NA             48          0        1   3249    2161715 2260888 2062541"
              

#3ER PUNTO
       
    #Estimación incondicional 
      regresion2<- lm(ingtot~sex, data=base2)
      lm_summary2<-summary(regresion2)
      lm_summary2
      
        "Residuals:
           Min       1Q   Median       3Q      Max 
      -1915848 -1015848  -673293   -27816 83917486 
      
      Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)  1604223      30294  52.955  < 2e-16 ***
      sex           311625      41612   7.489  7.3e-14 ***
      ---
      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
      
      Residual standard error: 2671000 on 16540 degrees of freedom
      Multiple R-squared:  0.003379,	Adjusted R-squared:  0.003319 
      F-statistic: 56.08 on 1 and 16540 DF,  p-value: 7.302e-14
"
      #Errores estandar 
      #Errores bootstrap
      set.seed(12345)
      n<- length(base2$ingtot)
      R<-1000
      eta_mod11<- rep(0,R)
      eta_mod22<-rep(0,R)
      eta_mod33<-rep(0,R)
      y_predichos2<- matrix(NA,n,R)
      for (i in 1:R){
        db_sample<- sample_frac(base2,size=1, replace=TRUE)
        f2<- lm(ingtot~sex, db_sample)
        coefs2<- f2$coefficients
        
        eta_mod11[i]<- coefs2[1]
        eta_mod22[i]<- coefs2[2]

      } 
      for (i in 1:R){
        columnas2<- eta_mod11[i]+eta_mod22[i]*base2$sex
        y_predichos2[,i]<-columnas2
      }
      ee2<-rowSds(y_predichos)
      df22<-cbind(base2,ee)
      IC_bajo2=base2$y_predicho-1.96*ee
      IC_alto2=base2$y_predicho+1.96*ee
      
      base2<- cbind(base2,IC_alto2, IC_bajo2)
      
      #Gráfico
      ggplot(base2, aes(age, y_predicho)) + geom_point() +                                
        geom_line(color = "dark green", size = 2) +
        geom_ribbon(aes(ymin=IC_bajo, ymax=IC_alto), alpha=0.1, fill = "green", 
                    color = "black", linetype = "dotted")
      
      
    #tabla de correlación
    correlacion <- (as.data.frame(cor(base2))) ; correlacion
    output_corr <- capture.output(correlacion, file=NULL, append =FALSE)
    output_corr <-as.data.frame(output_corr) #convertir summary en tabla
    write.table(x = output_corr, file = "summary.xlsx", sep = " ", 
                row.names = FALSE, col.names = TRUE)
    
    #preguntas
    
    #como exportar
    #outliers
    #varianza
    #missing salud
    
    
