# Regresion-logistica-multiple

#Cargar datos

    datos <-read.csv2( "D:/Especialización Estadistica/Semestre II/Seminario de Investigación/Tesis/BD/Bd filtradas/Se ha hecho la           mamografia/hecho_mamografia.csv", header=T)

#Definir variable respuesta-dicótomica

    hecho_mamografia <- datos$q938r
    levels(datos$q938r)
    table(hecho_mamografia)

#Definir modalidad de referencia

    hecho_mamografia <- ifelse(hecho_mamografia == "Si", 1, 0)
    hecho_mamografia <- factor(hecho_mamografia)
    table(hecho_mamografia)

#Definir variables independientes

    edad <- datos$Edadcategorizada
    etnia <- datos$QH08
    niv_educativo <- datos$nniveled
    estado_civil <- datos$QH14
    afil_salud <- datos$QH38
    act_economica <- datos$trabajo
    ind_riqueza <- datos$QHWLTHI5
    zona <- datos$zona
    región <- datos$QHREGION

#Generar marco de datos con las variables que conformaran la RLM

    datos.bin <- data.frame(hecho_mamografia,edad,etnia,niv_educativo,estado_civil,afil_salud,act_economica,ind_riqueza,zona,región)
    head(datos.bin)

#Verificar que el tipo de clase de la variable sea factor

    sapply(datos.bin, class)

#Función glm para ajustar un modelo lineal generalizado

    args(glm)
    function(formula, family=gaussian, data, weights, subset,
         na.action, star=NULL, etastart, mustart, offset, control=list(...),
         model= TRUE, method="glm.fit", x=FALSE, y=TRUE, contrasts=NULL,
         ...)
    NULL

#Modelo hecho_mamografia + edad categorizada
#Definir modalidad de referencia

    datos.bin$edad <- relevel(datos.bin$edad, ref = "40 a 49")
    levels(datos.bin$edad)
    contrasts(datos.bin$edad)

#Generar el modelo con la variable dependiente versus edad

    modelo.1 <- glm(hecho_mamografia ~ datos.bin$edad, data=datos.bin, family=binomial)
    summary(modelo.1)

#Calculo de los OR de las estimaciónes del modelo.1
   
    exp(coef(modelo.1))
#Calculo de los intervalos de confianza para los parametros del modelo.1

    exp(confint(modelo.1, level = 0.95))

#Modelo hecho_mamografia + etnia

    #Definir categoria de referencia
    datos.bin$etnia <- relevel(datos.bin$etnia, ref = "Ninguna de las anteriores")
    levels(datos.bin$etnia)
    #Modelo
    modelo.2 <- glm(hecho_mamografia ~ etnia, data=datos.bin, family=binomial)
    summary(modelo.2)
    #Calculo de los OR de las estimaciónes del modelo.2
    exp(coef(modelo.2))
    #Calculo de los intervalos de confianza para los parametros del modelo.2
    exp(confint(modelo.2, level = 0.95))

#Modelo hecho_mamografia + nivel educativo

    #Definir categoria de referencia
    niv_educativo <- relevel(niv_educativo , ref = "Ninguno")
    levels(niv_educativo)
    table(niv_educativo)
    #Modelo 
    modelo.3 <- glm(hecho_mamografia ~ niv_educativo, data=datos.bin, family=binomial)
    summary(modelo.3)
    #Calculo de los OR de las estimaciónes del modelo.3
    exp(coef(modelo.3))
    #Calculo de los intervalos de confianza para los parametros del modelo.3
    exp(confint(modelo.3, level = 0.95))

#Modelo hecho_mamografia + estado civil

    #Definir categoria de referencia
    datos.bin$estado_civil <- relevel(datos.bin$estado_civil, ref = "Soltera(o)")
    levels(datos.bin$estado_civil)
    #Modelo
    modelo.4 <- glm(hecho_mamografia ~ estado_civil, data=datos.bin, family=binomial)
    summary(modelo.4)
    #Calculo de los OR de las estimaciónes del modelo.4
    exp(coef(modelo.4))
    #Calculo de los intervalos de confianza para los parametros del modelo.4
    exp(confint(modelo.4, level = 0.95))

#Modelo hecho_mamografia + afiliación a la seguridad social en salud

    #Definir categoria de referencia
    datos.bin$afil_salud <- relevel(datos.bin$afil_salud, ref = "No está afiliado(a)")
    levels(datos.bin$afil_salud)
    #Modelo
    modelo.5 <- glm(hecho_mamografia ~ afil_salud, data=datos.bin, family=binomial)
    summary(modelo.5)
    #Calculo de los OR de las estimaciónes del modelo.5
    exp(coef(modelo.5))
    #Calculo de los intervalos de confianza para los parametros del modelo.5
    exp(confint(modelo.5, level = 0.95))

#Modelo hecho_mamografia + actividad economica

    #Definir categoria de referencia
    datos.bin$act_economica <- relevel(datos.bin$act_economica, ref = "No trabaja actualmente")
    levels(datos.bin$act_economica)
    #Modelo
    modelo.6 <- glm(hecho_mamografia ~ act_economica, data=datos.bin, family=binomial)
    summary(modelo.6)
    #Calculo de los OR de las estimaciónes del modelo.6
    exp(coef(modelo.6))
    #Calculo de los intervalos de confianza para los parametros del modelo.6
    exp(confint(modelo.6, level = 0.95))

#Modelo hecho_mamografia + indice de riqueza

    #Definir categoria de referencia
    datos.bin$ind_riqueza <- relevel(datos.bin$ind_riqueza, ref = "Más bajo")
    levels(datos.bin$ind_riqueza)
    #Modelo
    modelo.7 <- glm(hecho_mamografia ~ ind_riqueza, data=datos.bin, family=binomial)
    summary(modelo.7)
    #Calculo de los OR de las estimaciónes del modelo.7
    exp(coef(modelo.7))
    #Calculo de los intervalos de confianza para los parametros del modelo.7
    exp(confint(modelo.7, level = 0.95))

#Modelo hecho_mamografia + zona

    #Definir categoria de referencia
    datos.bin$zona <- relevel(datos.bin$zona, ref = "Rural")
    levels(datos.bin$zona)
    #Modelo
    modelo.8 <- glm(hecho_mamografia ~ zona, data=datos.bin, family=binomial)
    summary(modelo.8)
    #Calculo de los OR de las estimaciónes del modelo.8
    exp(coef(modelo.8))
    #Calculo de los intervalos de confianza para los parametros del modelo.8
    exp(confint(modelo.8, level = 0.95))

#Modelo hecho_mamografia + región

    #Definir categoria de referencia
    datos.bin$región <- relevel(datos.bin$región, ref = "Territorios Nacionales")
    levels(datos.bin$región)
    #Modelo
    modelo.9 <- glm(hecho_mamografia ~ región, data=datos.bin, family=binomial)
    summary(modelo.9)
    #Calculo de los OR de las estimaciónes del modelo.9
    exp(coef(modelo.9))
    #Calculo de los intervalos de confianza para los parametros del modelo.9
    exp(confint(modelo.9, level = 0.95))

#Modelo hecho mamografia + todas las variables

    modelo.10 <- glm(hecho_mamografia ~edad+etnia+niv_educativo+estado_civil+afil_salud+act_economica+ind_riqueza+zona+región,               data=datos.bin, family=binomial)
    summary(modelo.10)
    #Calculo de los OR de las estimaciónes del modelo.10
    exp(coef(modelo.10))
    #Calculo de los intervalos de confianza para los parametros del modelo.10   
    exp(confint(modelo.10, level = 0.95))

#Selección automatica de variables mediante procedimiento stepwise

    #Paquete necesario
    library(MASS)
    modelo.full <- glm(hecho_mamografia ~ edad+etnia+niv_educativo+estado_civil+afil_salud+act_economica+ind_riqueza+zona+región, data =     datos.bin,
                   family = binomial)
    modelo.inicial <- glm(hecho_mamografia~ 1, data = datos.bin, family = binomial)
    modelo.stp <- stepAIC(modelo.inicial, scope = list(upper = modelo.full), direction = "both")
    summary(modelo.stp)
    #Calculo de los OR de las estimaciónes del modelo.stp
    exp(coef(modelo.stp))
    #Calculo de los intervalos de confianza para los parametros del modelo.stp
    exp(confint(modelo.stp, level = 0.95))

#Devianza del modelo modelo.stp

    LR <- modelo.stp$null.deviance-modelo.stp$deviance
    N <- sum(weights(modelo.stp))
    (RsqCN <- 1- exp(-LR/N))

#Asignar 1 a los individuos cuya probabilidad estimada sea mayor o igual que 0.5, para cada uno de los modelos

    table(datos.bin$hecho_mamografia)
    prediccion <- ifelse(fitted.values(modelo.1) >= 0.5, 1, 0)
    table(prediccion)
    prediccion <- ifelse(fitted.values(modelo.2) >= 0.5, 1, 0)
    table(prediccion)
    prediccion <- ifelse(fitted.values(modelo.3) >= 0.5, 1, 0)
    table(prediccion)
    prediccion <- ifelse(fitted.values(modelo.4) >= 0.5, 1, 0)
    table(prediccion)
    prediccion <- ifelse(fitted.values(modelo.5) >= 0.5, 1, 0)
    table(prediccion)
    prediccion <- ifelse(fitted.values(modelo.6) >= 0.5, 1, 0)
    table(prediccion)
    prediccion <- ifelse(fitted.values(modelo.7) >= 0.5, 1, 0)
    table(prediccion)
    prediccion <- ifelse(fitted.values(modelo.8) >= 0.5, 1, 0)
    table(prediccion)
    prediccion <- ifelse(fitted.values(modelo.9) >= 0.5, 1, 0)
    table(prediccion)
    prediccion <- ifelse(fitted.values(modelo.10) >= 0.5, 1, 0)
    table(prediccion)
    prediccion <- ifelse(fitted.values(modelo.stp) >= 0.5, 1, 0)
    table(prediccion)

#Generar tabla de clasificación

    table(datos.bin$hecho_mamografia, prediccion)
    tabla.clasif <- table(datos.bin$hecho_mamografia, prediccion)
    tcc <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)
    tcc

#Calcula los valores de verdaderos positivos, verdaderos negativos, falsos positivos y falsos negativos para diferentes puntos de corte.

    pred <- prediction(fitted.values(modelo.stp), datos.bin$hecho_mamografia)
    perf1 <- performance(pred, measure = "acc")
# el punto de corte que maximiza 'acc' es

    (posicion.max <- sapply(perf1@y.values, which.max))
    (punto.corte <- sapply(perf1@x.values, "[", posicion.max))
    plot(perf1, col = "darkred")
# Añadimos una línea horizontal al valor de 0.8
    abline(h = 0.8, lty = 2)
# Añadimos recta con el punto de corte que maximiza la tasa de clasificaciones correctas
    abline(v = punto.corte, lty = 2)

#Genera el valor bajo la curva de ROC(Ajuste del modelo)

    AUC <- performance(pred, "auc")
    AUC@y.name
## [1] "Area under the ROC curve"
    AUC@y.values

#Genera grafico con el area bajo la curva de ROC

    perf2 <- performance(pred, "tpr", "fpr")
    plot(perf2, colorize = TRUE,xlab="Razón de verdaderos positivos", ylab="Razón de falsos positivos",cex=0.5) # mostramos colores         según el punto de corte
# Añadimos la recta y=x que sería la correspondiente al peor clasificador
    abline(a = 0, b = 1)
# Añadimos el valor del área bajo la curva
    text(0.4, 0.6, paste(AUC@y.name, "\n", round(unlist(AUC@y.values), 3)), cex = 0.7)


#Generar comparación de modelos a traves de prediccion de los modelos ajustados

    pred.modelo.stp <- prediction(fitted.values(modelo.stp), datos.bin$hecho_mamografia)
    perf.modelo.stp <- performance(pred.modelo.stp, "tpr", "fpr")
    plot(perf.modelo.stp, col = "red", lty = 2,xlab="Razón de verdaderos positivos", ylab="Razón de falsos positivos") # modelo.stp sin     las variables significativas
    pred.modelo.10 <- prediction(fitted.values(modelo.10), datos.bin$hecho_mamografia)
    perf.modelo.10 <- performance(pred.modelo.10, "tpr", "fpr")
    plot(perf.modelo.10, col = "blue", lty = 3, add = TRUE) # modelo con todas las variables
    abline(a = 0, b = 1)
    legend("bottomright", c("R.L Modelo con todas las variables","R.L Modelo método Stepwise"),
       col = c("red", "darkblue"), lty = 2:3, cex = 0.7)

#Graficos de diagnostico de los residuales

    par(mfrow = c(2, 2))
    plot(modelo.stp, cex = 0.5)

#Prueba de multicolinealidad de las variables predictoras

    #Paquete necesario
    library(car)
    vif(modelo.stp)

#Calcular el valor máximo de cook
 
    cook <-cooks.distance(modelo.stp)
    max(cook)
