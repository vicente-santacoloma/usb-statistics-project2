###############################################################################
##                                  PROYECTO 2                               ##
###############################################################################
## Integrantes: Fatima Querales     07-41388                                 ##
##              Vicente Santacoloma 08-11044                                 ##
############################################################################### 

###############################################################################
##                                  PARTE 1                                  ##
###############################################################################

summary(swiss) 
boxplot(swiss) # Diagrama de Boxplot

Fertility        = swiss[,1]
Agriculture      = swiss[,2]
Examination      = swiss[,3]
Education        = swiss[,4]
Catholic         = swiss[,5]
Infant.Mortality = swiss[,6]

# Fertility
jpeg('Fertility.jpeg')
par(mfrow=c(1,2))
hist(Fertility)
boxplot(Fertility)
dev.off()

# Agriculture
jpeg('Agriculture.jpeg')
par(mfrow=c(1,2))
hist(Agriculture)
boxplot(Agriculture)
dev.off()

# Examination
jpeg('Examination.jpeg')
par(mfrow=c(1,2))
hist(Examination)
boxplot(Examination)
dev.off()

# Education
jpeg('Education.jpeg')
par(mfrow=c(1,2))
hist(Education)
boxplot(Education)
dev.off()

# Catholic
jpeg('Catholic.jpeg')
par(mfrow=c(1,2))
hist(Catholic)
boxplot(Catholic)
dev.off()

# Infant Mortality
jpeg('Infant.Mortality.jpeg')
par(mfrow=c(1,2))
hist(Infant.Mortality)
boxplot(Infant.Mortality)
dev.off()

###############################################################################
##                                  PARTE 2                                  ##
###############################################################################

pairs(swiss)   # Diagrama de Dispersion
cor(swiss)     # Matriz de Correlacion

###############################################################################
##                                  PARTE 3                                  ##
###############################################################################

# FALTA

###############################################################################
##                                  PARTE 4                                  ##
###############################################################################

# FALTA

###############################################################################
##                                  PARTE 5                                  ##
###############################################################################

# FALTA

###############################################################################
##                                  PARTE 6                                  ##
###############################################################################

# Funcion para pruebas de hipotesis de diferencias de medias para muestra       
# pequenas. 
#
# Devuelve:
# true si se rechaza la hipotesis nula. 
# false si no se puede rechazar la hipotesis nula.
#
# Parametros:
# x1 = datos del grupo 1
# x2 = datos del grupo 2
# m1 = media del grupo 1
# m2 = media del grupo 2
# s1 = desviacion estandar del grupo 1 (puede ser la real o aproximada segun el caso)
# s2 = desviacion estandar del grupo 2 (puede ser la real o aproximada segun el caso)
# n1 = tamano del grupo 1
# n2 = tamano del grupo 2
# tipo de prueba: 1 => Ha: Z > Zalfa, 2 => Ha: Z < Zalfa, 3 => Ha: |Z| > Zalfa
# varConocida: 1 si las varianzas son conocidas. 0 si no lo son.
# alpha: nivel de significancia (0.05 por defecto)
# u0: valor del lado derecho de la prueba de hipotesis nula. Por defecto se confideran
# iguales las dos medias
PHDiferenciaMedias <- function(x1,x2,m1,m2,s1,s2,n1,n2,tipo,varConocida,alpha = 0.05,u0=0)
{
  if(n1 <= 0 || n2 <= 0 || !(1 <= tipo & tipo <=3) || !(varConocida != 0 || varConocida != 1)) {
    cat("Datos Invalidos")
    return()
  }
  if(n1 >= 30 | n2 >= 30) {
    cat("No Aplicable a la Hipotesis de Diferencia de Medias para Muestras Pequenas")
    return()
  }
  if(varConocida == 0) {
    varTest = var.test(x1,x2)
    F = unique(unlist(varTest$statistic))
    p.valor = unique(unlist(varTest$p.value)) 
    intConf = unique(unlist(varTest$conf.int))
    varIguales = 0
    if(F <= intConf[1] || intConf[1] >= F)
      varIguales = 0
    else if(p.valor <= 0.05 || p.valor >= 0.7 || p.valor >= alpha)
      varIguales = 1

    if(varIguales == 1) {
      Z = m1-m2-u0
      Z = Z/sqrt((((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))*((1/n1)+(1/n2)))
      p.valor = pt(Z,n1+n2-2,lower.tail=FALSE)
      if(tipo == 3) {
	alpha = alpha/2
        p.valor = p.valor*2
      }
      RR = qt(alpha,n1+n2-2,lower.tail=FALSE)
    }
    else {
      Z = m1-m2-u0
      Z = Z/sqrt(s1^2/n1+s2^2/n2)
      v = (s1^2/n1+s2^2/n2)^2
      v = v/((s1^2/n1)^2*(1/(n1-1))+(s2^2/n2)^2*(1/(n2-1)))
      v = v-2
      p.valor = pt(Z,v,lower.tail=FALSE)
      if(tipo == 3) {
	alpha = alpha/2
        p.valor = p.valor*2
      }
      RR = qt(alpha,v,lower.tail=FALSE)
    }
  }
  else {
    Z = m1-m2-u0
    Z = Z/sqrt((s1^2/n1)+(s2^2/n2))
    p.valor = pnorm(Z, mean = 0, sd = 1, lower.tail = FALSE)
    if(tipo == 3) {
      alpha = alpha/2
      p.valor = p.valor*2
    }
    RR = qnorm(alpha, mean = 0, sd = 1, lower.tail = FALSE)
  }
  if(tipo == 3)
    print(c('Region de Rechazo: ',-RR,RR))
  else if(tipo == 1)
    print(c('Region de Rechazo: ', -RR))
  else
    print(c('Region de Rechazo: ',  RR))
  print(c('Estadistico Z: ',Z))
  print(c('p-valor: ',p.valor))
  abs(Z) > abs(RR)
}

x1 = Fertility[Education >= 9.6]
x2 = Fertility[Education <  9.6]
m1 = mean(x1)
m2 = mean(x2)
n1 = length(x1)
n2 = length(x2)
s1 = sd(x1)
s2 = sd(x2)
tipo = 1
alpha = 0.025
varConocida = 0
b = PHDiferenciaMedias(x1,x2,m1,m2,s1,s2,n1,n2,tipo,varConocida,alpha)

###############################################################################
##                                  PARTE 7                                  ##
###############################################################################

# Construyendo la tabla de distribucion de frecuencias para Infant.Mortality

n = length(Infant.Mortality)
k = sqrt(n)
k = round(k)
l = max(Infant.Mortality) - min(Infant.Mortality)
l = l / k
f = matrix(nrow = k, ncol = 1)

class = matrix(nrow = k, ncol = 2)
class[1,1] = min(Infant.Mortality)
class[1,2] = class[1,1] + l

for (i in 2:k) {
  class[i,1] = class[i-1,2]
  class[i,2] = class[i,1] + l
}

for (i in 1:k) {
  c = 0
  for(j in 1:n) {  
    if (class[i,1] <= Infant.Mortality[j] && Infant.Mortality[j] < class[i,2]) {
      c = c + 1
    }
  }
  f[i] = c
}

# Pruebas de Bondad de Ajuste

# Ho: Los datos de Infant.Mortality se ajustan a una distribucion normal
# Ha: Los datos de Infant.Mortality no se ajustan a una distribucion normal

alpha = 0.05
r = 3
m = matrix(nrow = k, ncol = 1)
for (i in 1:k) {
  m[i] = class[i,1] + (l/2)
}

mean = sum(f * m)/n

variance = (sum(f * m^2) - ((sum(f * m))^2)/n) / (n-1)
#variance = sum(f * (m-mean)^2)/n
sd = sqrt(variance)

p = pnorm(class[,2],mean,sd) - pnorm(class[,1],mean,sd)

X2.obs = sum((f - n * p)^2 / (n * p))
X2.alpha = qchisq(1 - alpha,k-r)

# Calculando el p-valor:
p.value = 1 - pchisq(X2.obs,k-r)

# Obtenemos X2.obs = 7.345114 y X2 k−r;α = 9.487729
# Entonces, la region de rechazo es RR = (9.487729 , ∞)
# Como X2 !∈ RR, en base a la evidencia no podemos rechazar la hipotesis
# nula de que los datos se ajustan a una distribucion normal

###############################################################################
##                                  PARTE 8                                  ##
###############################################################################

q1 = quantile(Education,.25)
q2 = quantile(Education,.50)
q3 = quantile(Education,.75)

G1 = Infant.Mortality[Education < q1]
G2 = Infant.Mortality[q1 <= Education & Education < q2]
G3 = Infant.Mortality[q2 <= Education & Education < q3]
G4 = Infant.Mortality[Education >= q3]
n1 = length(G1)
n2 = length(G2)
n3 = length(G3)
n4 = length(G4)

Infant.Mortality = c(G1,G2,G3,G4)
Group = factor(rep(LETTERS[1:4],c(n1,n2,n3,n4)))
Infant.Mortality.df = data.frame(Group,Infant.Mortality)
aov.Infant.Mortality = aov(Infant.Mortality ~ Group, Infant.Mortality.df)
summary = summary(aov.Infant.Mortality)

# 1. 3
# 2. 43
# 3. 23.4
# 4. 366.8
# 5. 7.813
# 6. 8.531
# 7. 0.916
# 8. 0.441

# FALTA reponder

