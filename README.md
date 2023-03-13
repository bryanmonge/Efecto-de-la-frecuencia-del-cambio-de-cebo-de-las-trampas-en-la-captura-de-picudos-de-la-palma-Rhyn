  + Datos

```{r}
library(readxl)
```

```{r}
base = read_excel("base.xlsx")
base$Trampa = as.factor(base$Trampa)
base$Lote = as.factor(base$Lote)
base$Tratamiento = as.factor(base$Frecuencia)
base$Semana = as.factor(base$Semana)
base$Tasa = base$Capturas / base$Frecuencia
str(base)

#save(base, file = "trampas.Rdata")
load("trampas.Rdata")
str(base)
```
  
  + Contextualizacion del problema
  
  Interesa conocer el efecto que tiene la frecuencia de cambio de las trampas sobre la tasa de captura de Rhynchophorus palmarum.
  
  En este caso la variable respuesta es la tasa de captura de Rhynchophorus palmarum, la cual es un numero relativo que representa el conteo de insectos capturados sobre la cantidad de dias que estuvo la trampa puesta. Esto se puede expresar como:
  
  $$T_j=\frac{Y_j}{n_j}$$
  
  Donde $T_j$ es la tasa de captura observada en una trampa y una cantidad de dias especifica, entonces $Y_j$ es cantidad de insectos capturados y $n_j$ es la cantidad de dias que estuvo puesta la trampa. Como n es fijo en la unidad de observacion, entonces la variable aleatoria Y tiene esperanza:
  
  $$E[T_j]=\frac{E[Y_j]}{n_j}$$
  
  
  Para analizar este efecto se hara uso de un diseño de medidas repetidas el cual se presenta a continuacion:
  
  $$log(E[Y_j])=\beta_0+\tau_j+\beta_{0,i}+\alpha_r+log(dias)$$
  
  donde:
  
  $\tau_j$: Efecto de la frecuencia de cambio (fijo)
  $\beta_{0,i}$: Aumento o disminucion en la tasa de captura de la i-esima palma en la frecuenta de cambio de 7 dias. (aleatorio)
  $\alpha_r$: Efecto del r-esimo lote
  
  Este es un modelo de conteos donde log(dias) tiene como coeficiente 1, dado que ese coeficiente no se quiere estimar.
  
  
  + Tasa promedio de captura de Rhynchophorus palmarum en cada lote, segun frecuencia de cambio de la trampa
  
```{r}
tapply(base$Tasa, list(base$Frecuencia, base$Lote), mean)
```
  
  
  + Grafico de la tasa de cambio
  
```{r}
library(lattice)
```
  
  
```{r}
# Grafico de la tasa de cambio segun frecuencia de dias
xyplot(Tasa ~ Tratamiento, type = c("r", "p"), xlab = "Frecuencia de cambio", ylab = "Tasa de captura", base)

# Grafico de la tasa de cambio segun frecuencia de dias y lote
xyplot(Tasa ~ Tratamiento|Lote, type = c("r", "p"), xlab = "Frecuencia de cambio", ylab = "Tasa de captura", base)
```
  
  Respuesta:
  
    Se observa que conforme avanza la frecuencia de cambio, la tasa de captura de Rhynchophorus palmarum disminuye para los tres lotes, siendo esta relacion negativa en los tres casos.
  
    Cuando se combinan los tres lotes, se observa que conforme aumenta la frecuencia de dias disminuye la tasa de captura de Rhynchophorus palmarum para todos los lotes.
    

--------------------------------------------------------------------------------
  
  + Estimacion del modelo inicial (*Poisson*)
  
```{r}
library(lme4)
```
  
  
```{r}
modmi = glmer(Capturas ~ offset(log(Frecuencia)) + Tratamiento + (1|Lote) + (1|Trampa), family = "poisson", base)
```
  
  
  Se obtienen los residuales de Pearson con la función `residuals` usando `type = "pearson"` y se  calcula el parámetro de dispersión. 
  
  a. Residual de Pearson
  
  $$r_p=\frac{y_i+\bar{y}_i}{\sqrt{\bar{y}_i}}$$
  
```{r}
rp = residuals(modmi, type = "pearson")
```
  
  
  b. Parametro de dispersion
  
  $$\hat{\phi}=\frac{\sum{r_p^2}}{n-p}$$
  
  donde n es el numero de observaciones en la muestra, y p es el numero de parametros estimados en el modelo.
  
```{r}
p = length(summary(modmi)$coefficients[,1])
n = nrow(base)
phi = sum(rp^2)/(n - p)
round(phi, 3)
```
  
  Entonces
  
  $H0:E[Y|X]=V[Y|X]$
  $H1:E[Y|X]\neq V[Y|X]$
  
  Se obtiene un parametro de disperion de $\hat{\phi}=1.81>1$ entonces no se cumpe el supuesto de que la varianza condicional es igual a la media estimada condicional, es decir, se demuestra la sospecha de que existe sobredispersion. 
  
  + Grafico para observar la relacion entre residuales al cuadrado contra valores ajustados

  
```{r}
# Valores ajustados
fit = predict(modmi, type = "response")

# Residuales
r = residuals(modmi, type = "response")
```
  
  
  + Prueba de hipotesis de equidispersion
  
```{r}
overdisp_fun <- function(modmi) {
  
  rdf <- df.residual(modmi)
  rp <- residuals(modmi, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
  
}

overdisp_fun(modmi)
```
  
  
  Respuesta:
  
    Se rechaza la H0 de equidispersion, es decir, se prefiere usar un modelo que tome en cuenta la sobredispersion en los datos, el cual seria la BN.
  
  
  Se grafican los residuales al cuadrado contra los valores ajustados. Puede poner ambos en logaritmo para visualizar mejor. Se agrega la función identidad para ver que tranto se parecen las medias y las varianzas estimadas.
  
```{r}
plot(log(r^2) ~ log(fit), xlab = expression(log(hat(lambda))), ylab = expression(log(r^2))); abline(0,1, col = 2)
```
  
  Respuesta:
  
    En este grafico podemos observar que hay equidispersion por lo que se procede a utilizar el modelo Poisson. No obstante, tomanto en cuenta la prueba y el parametro de dispersion, es engañoso usar la Poisson o la BN, por lo que se contrastaran los resultados de ambos modelos segun las tasas de captura obtenidas en cada lote segun la frecuencia de cambio del cebo de las trampas.
  
  
  
--------------------------------------------------------------------------------  
  
  *Poisson*
  
  La componente aleatoria es:
  
  $$y|F\backsim Poisson(\lambda)$$
  
  entonces la esperanza matematica es:
  
  $$E[Y|F]=Var[Y|F]=\lambda$$
  

  
  Se estima los parámetros de los efectos aleatorios y fijos del modelo Poisson.
  
```{r}
summary(modmi)
```
  
  + Se prueba la hiposis nula de que hay un efecto de la frecuencia de cambio de las trampas sobre la tasa de captura de Rhynchophorus palmarum
  
  $H0:\tau_j=0$
  
```{r}
drop1(modmi, test = "Chisq")
```
  
  Respuesta:
  
    Con una significancia del 5%, hay suficiente evidencia estadistica para rechazar la H0 de que hay un efecto de la frecuencia de cambio de las trampas sobre la tasa de captura de Rhynchophorus palmarum. Es decir, en al menos uno de las frecuencias, existe diferencias.

  + Comparaciones (*Usando restriccion de tratamiento de referencia*)
  
  1. $H0:\tau_7=\tau_14$
  2. $H0:\tau_7=\tau_21$
  3. $H0:\tau_7=\tau_42$
  4. $H0:\tau_{14}=\tau_{21}$
  5. $H0:\tau_{14}=\tau_{42}$
  6. $H0:\tau_{21}=\tau_{42}$
  
```{r}
# Estimacion de tasas de captura promedio
b = summary(modmi)$coef[,1]
f7 = c(1, 0, 0, 0)
f14 = c(1, 1, 0, 0)
f21 = c(1, 0, 1, 0)
f42 = c(1, 0, 0, 1)

# Estimacion de tasa promedio para cada frecuencia
exp(f7%*%b)
exp(f14%*%b)
exp(f21%*%b)
exp(f42%*%b)

# Estimacion de diferencias de tasas de captura segun la frecuencia de cambio
f7_14 = f7 - f14
f7_21 = f7 - f21
f7_42 = f7 - f42

f14_21 = f14 - f21
f14_42 = f14 - f42

f21_42 = f21 - f42

contr = cbind(f7_14, f7_21, f7_42, f14_21, f14_42, f21_42)

ee = sqrt(diag(t(contr)%*%vcov(modmi)%*%contr))
eta = t(contr)%*%b
qt = eta/ee

gl = n - p # PREGUNTAR

p = ptukey(qt*sqrt(2), 4, gl, lower.tail = F)
p < 0.05
```
  
  Respuesta:
  
    Se observan diferencias en la tasa de captura en todos los tratamientos, menos cuando se compara el efecto de la frecuencia de cambio de 7 dias contra la de 14 sobre la tasa de captura de Rhynchophorus palmarum. 
  
  + Intervalos de confianza
    
```{r}
t = qt(1-0.05/5, gl)
ICI = exp(eta[-1]-t*ee[-1])
ICS = exp(eta[-1]+t*ee[-1])
round(ICI, 2)
round(ICS, 2)
```
    

  Respuesta:
      
    1. Con un 95% de confianza se espera que la tasa de captura cuando la frecuencia de cambio de las trampas es 7 dias sea al menos un 34% mayor en comparacion a cuando la frecuencia de cambio de la trampa es 21 dias.


    2. Con un 95% de confianza se espera que la tasa de captura cuando la frecuencia de cambio de las trampas es 7 dias sea al menos un 84% mayor en comparacion a cuando la frecuencia de cambio de la trampa es 42 dias.
    
    3. Con un 95% de confianza se espera que la tasa de captura cuando la frecuencia de cambio de las trampas es 14 dias sea al menos un 11% mayor en comparacion a cuando la frecuencia de cambio de la trampa es 21 dias.
    
    4. Con un 95% de confianza se espera que la tasa de captura cuando la frecuencia de cambio de las trampas es 14 dias sea al menos un 52% mayor en comparacion a cuando la frecuencia de cambio de la trampa es 42 dias.
    
    5. Con un 95% de confianza se espera que la tasa de captura cuando la frecuencia de cambio de las trampas es 21 dias sea al menos un 11% mayor en comparacion a cuando la frecuencia de cambio de la trampa es 42 dias.
    
    
   - Estimacion de las varianzas de los efectos aleatorios (trampa y lote)
  
```{r}
summary(modmi)

vars = c(0.2856, 0.2530)
vartot = sum(vars)
vartot

# Porcentajes de varianza
round(vars/vartot*100, 1)
```
  
  Respuesta:
  
    El efecto de la palma tiene una variancia de 0.2856 que representa un 53% de la variancia total, la cual es menor que la variancia del lote (0.2530) la cual representa un 47%.  
  
  
  
  + Parte grafica para el analisis
  
```{r}
library(ggplot2)
```
  
  

```{r}

ggplot(base, aes(x = Lote, y = Tasa, fill = Tratamiento)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot() +
  theme_classic()+
  scale_fill_brewer(palette = "Dark2")+theme(legend.position = "bottom")+
  labs(y = "Tasa de Captura")

tapply(base$Tasa,list(base$Lote,base$Tratamiento),mean)
tasa.capturas = as.vector(round(tapply(base$Tasa,list(base$Lote,base$Tratamiento),mean),2))
Frecuencia = rep(c("7 días", "14 días", "21 días", "42 días"), each = 3)
Lotes = rep(c("Lote 32", "Lote 33", "Lote 34"), times = 4)
datos = data.frame(tasa.capturas,Frecuencia,Lotes)


datos$tasa.capturas = as.numeric(datos$tasa.capturas)
datos$Frecuencia = as.factor(datos$Frecuencia)
datos$Lotes = as.factor(datos$Lotes)

ggplot(datos, aes(x = Lotes, y = tasa.capturas, fill = factor(Frecuencia, levels = c("7 días","14 días","21 días","42 días")))) +
  geom_bar(position="dodge", stat="identity") + 
  theme_classic()  +theme(legend.position = "bottom")+
  labs(fill=" ",y="Promedio de Tasa de Captura") + 
  scale_fill_brewer(palette = "Set3")+
  geom_text(aes(label=tasa.capturas), vjust=1.6, color="black",position = position_dodge(0.9), size=4.0)
```
  
  
