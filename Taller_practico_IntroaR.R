##Clase Introducción a R

##Contenidos
##1.- Consideraciones previas
##2.- Importar bases de datos
##3.- Exploración de los datos
##4.- Análisis de datos: gráficos bivariados y modelo de regresión

##1.-

##En primer lugar, si queremos establecer comentarios en nuestro editor de sintaxis (es decir, donde estamos 
#ubicados ahora), basta con colocar signo gato "#". De lo contrario, el software leerá lo escrito como código.
#Es muy útil para efectos de ir registrando lo que vamos haciendo y en caso de consultas futuras entender bien lo que se hizo

##Correr código, varias opciones: (i) seleccionar código a utilizar y luego apretar boton run. 
#(ii) colocarse en línea de código que se desea a utilizar y apretar ctr + enter

##Establecer directorio de trabajo: Debemos señalarle al programa el lugar o más precisamente la ruta donde
## queremos alojar el trabajo que estamos haciendo en el software. Se puede hacer manualmente en sessión->set wd->choose directory
##Opción recomendada: con código, basta usar la función setdwd(), y al interior del parentesis colocar ruta local donde queremos 
#nuestro directorio de trabajo. Veamoslo.

setwd("D:/Docencia/Introducción a R/Introduccion-a-lenguaje-R") ##mi ruta local

##Luego, podemos consultar donde estamos ubicados actualmente con getwd() para corroborar que esté todo ok.

getwd()

##Instalación y carga de paquetes, dos métodos: (1) el clásico install.packages y luego library, o (2) opción más eficiente
# con función p_load, del paquete pacman. Entonces, instalar pacman si no lo han hecho

install.packages("pacman")

##Y ahora colocamos todos los paquetes que utilizaremos en nuestros análisis

pacman::p_load("haven", 
               "readxl",
               "tidyverse",
               "car",
               "sjmisc",
               "summarytools",
               "stargazer",
               "corrplot", 
               "texreg")

##¿cómo saber para qué sirve cada paquete?



##Antes de cargar las bases de datos, recomiendo limpiar entorno de trabajo y evitar notación científica, tal como hemos
# visto en las capsulas 

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

##2.- Cargar/importar bases de datos que tienen diferentes formatos 

##Leer una base formato .dta (stata)
padres<- read_dta("input/data/original/simce2m2017_cpad_publica.dta")

##Leer una base formato .sav (SPSS)
datos<- read_spss("input/data/original/OCDE_GENERO.sav")

##Leer una base en formato xlsx (Excel)
elsoc <- read_xlsx("input/data/original/DatosELSOC2.xlsx") 

##Leer una base en formato R, la que usaremos en el taller
load("input/data/original/proc.RData")

##Empezamos a explorar la BBDD

##Dimensiones de la base de datos: Cuantas filas (observaciones) y cuántas columnas (variables)

dim(proc) #dimensiones. Entonces, la consola nos señala que tenemos 169.824 observaciones y 92 variables 

##Ahora pidamosle los nombres de las variables 

names(proc)

##Deberíamos consultar libro de códigos de la base de datos, para saber qué significa cada una de ellas. 
##En este ejemplo, seleccionaremos "rbd", Ingresos del hogar, nivel educacional de los padres, y puntaje SIMCE Mate.

#De acuerdo con ello, seleccionamos variables

proc_s=proc %>% select(rbd.x, cpad_p06 ,cpad_p07, cpad_p10, 
                     ptje_mate2m_alu) %>% as.data.frame()

## El operador "%>%" (se genera con ctr + shift + m) es para concatenar operaciones en una misma línea de código, es decir aplicar varias funciones de 
#Manera continuada o seguida. En nuestro ejemplo, creamos un nuevo objeto (base de datos) a partir del objeto denominado proc, 
# y luego usando la función select escogemos las variables con las cuales trabajaremos de aquí en adelante. Por último, 
#le pedimos que guarde nuestro nuevo objeto como base de datos (en inglés data frame)

##3.-Exploración y manipulación de la base de datos 

str(proc_s)

##Podemos pedirle lo mismo, pero para alguna columna en específico

class(proc_s$ptje_mate2m_alu)

## El ooperador $ se usa en el contexto del llamado a algún objeto que hayamos creados. En este caso, nuestro objeto es
# una BBDD, de modo que el "$" señala que vaya y aplique determinada función a algun elemento (variable) 
#en particular del objeto creado

##Descriptivos

summary(proc_s)

##de alguna variable en particular 

summary(proc_s$ptje_mate2m_alu)

##tabla de frecuencias de nivel educaiconal del padre, donde cada categoría representa los años estudiados

table(proc_s$cpad_p06)

## Ver descriptivos y frecuencia al mismo tiempo: Nuestra función ya conocida dfSummary 

view(dfSummary(proc_s, headings=FALSE))

## Si nos fijamos, hay valores extraños en nuestras variables de interés que corresponden a NS/NR denotados por 99 y 0

frq(proc_s$cpad_p10)

##Recodificar. Cada valor es un tramo y nosotros le imputaremos el valor "medio" del tramo  

proc_s$cpad_p10<- recode(proc_s$cpad_p10, 
                       "c(99, 0)=NA; 1=50000; 2=150000; 3=250000; 4=350000; 5=450000; 6=550000; 
                       7=700000; 8=900000; 9=1100000; 10=1300000; 
                       11=1500000; 12=1700000; 13=1900000; 14=2100000; 15=3000000")

##le pedimos denuevo tabla de frecuencia para chequear cambios 
frq(proc_s$cpad_p10)

##Nos conviene renombrar variable por temas de identificación, usamos función rename 

proc_s<- proc_s %>% rename("ingresos"=cpad_p10) 

##Hacemos recodificación para nivel educacional del padre y de la madre (están como dos variables diferentes)


frq(proc_s$cpad_p07)

frq(proc_s$cpad_p06)

proc_s$cpad_p07= recode(proc_s$cpad_p07, "c(21,99)=0")

proc_s$cpad_p06= recode(proc_s$cpad_p06, "c(21,99)=0")


## Creamos nueva variable que será el resultado del valor maximo del nivel ed. alcanzado por mapadres

proc_s$educacion<- ifelse(proc_s$cpad_p07>proc_s$cpad_p06, proc_s$cpad_p07, proc_s$cpad_p06)

frq(proc_s$educacion)

proc_s$educacion<- recode(proc_s$educacion, "0=NA")

##Eliminemos perdidos

proc_s<-na.omit(proc_s)

##Una función típica para crear nuevas variables es mutate

##Nuestra base de datos contiene el colegio al que pertenece cada alumno (rbd), de modo que podemos calcular estadísticas para cada colegio

##Por ejemplo, podemos crear una nueva variable que tenga el promedio de los ingresos y de niv. ed.  de cada colegio

proc_s= proc_s %>%
  group_by(rbd.x) %>%
  mutate(m_ingresos = mean(ingresos)) %>%
  mutate(sd_ingresos = sd(ingresos)) %>%
  mutate(mean_educacion=mean(educacion))
  
##Otra función útil: Filter 

proc_f<- proc_s %>% filter(educacion<=15)

frq(proc_f$educacion)

##Concatenando funciones con %>%  

proc_f_s<- proc_s %>% filter(educacion<=15) %>% select(ingresos, educacion, ptje_mate2m_alu)

## Correlaciones previas al análisis de regresión 

cormat= proc_s %>% select(ingresos, educacion, ptje_mate2m_alu) %>% cor()
round(cormat, digits=2)
corrplot.mixed(cormat)

##Una simple regresión lineal múltiple con dos VI sin estandarizar 

reg<- lm(ptje_mate2m_alu ~ log(ingresos) + educacion, data= proc_s)
screenreg(reg)

## Guardamos nuestra BBDD 

save(proc_s, file="input/data/proc/datos.rdata")

