
#install.packages('readxl','ggplot2','stringr','RSQLite')

setwd("E:/Downloads/ADRES-Pruebatecnica")

#importar librerias
library('readxl')
library('dplyr')
library('campfin')
library('ggplot2')
library('stringr')
library('RSQLite')

#importar Prestadores.xls
Prestadores <- read_excel("Prestadores - Copy.xlsx")
View(Prestadores)

#importar Municipios.xlsx
Municipios <- read_excel("Municipios.xlsx")
View(Municipios)

### Limpiar datos ###

######################
######################
##### Municipios #####

##Limpiar Municipios
#Eliminar caracteres especiales
Municipios['Municipio'] <- Municipios['Municipio'] %>% 
  mutate(across(everything(), ~gsub("[[:punct:]]", "", .x))) %>%
  print(n=Inf)
#Normalizar mayus/minusculas
Municipios['Municipio'] <- Municipios['Municipio'] %>%
  mutate(across(everything(), ~str_normal(.x))) %>%
  print(n=Inf)

##Limpiar Deptos
Municipios['Departamento'] <- Municipios['Departamento'] %>% 
  mutate(across(everything(), ~gsub("[[:punct:]]", "", .x))) %>%
  print(n=Inf)
#Normalizar
Municipios['Departamento'] <- Municipios['Departamento'] %>%
  mutate(across(everything(), ~str_normal(.x))) %>%
  print(n=Inf)



##################################
######### Prestadores ############
##################################

Prestadores <- Prestadores %>% 
  mutate(across(everything(), ~str_normal(.x)))

head(Prestadores)

#Modificar nombres de las columnas
colnames(Prestadores) <- str_to_title(colnames(Prestadores))
colnames(Prestadores)[colnames(Prestadores) %in%
  c('Depa_nombre', 'Muni_nombre')] <- c("Departamento", "Municipio")
colnames(Prestadores)

## Extraer cuales deptos están en un dataframe y no en el otro
setdiff(Municipios['Departamento'], Prestadores['Departamento'])
setdiff(Prestadores['Departamento'], Municipios['Departamento'])
## Result:
## Barranquilla, Buenaventura, Cali, Cartagena, Santa Marta
## son 'deptos' en dataframe prestadores,
## Bogotá DC y San Andrés tienen nombres diferentes
##
##SE DEBEN CAMBIAR NOMBRES DE LOS DEPTOS 'Cali', 'Barranquilla', etc.
##

#Fix data
Prestadores[Prestadores$Departamento == 'BARRANQUILLA',]['Departamento'] <- 'ATLÁNTICO'
Prestadores[Prestadores$Departamento == 'BUENAVENTURA',]['Departamento'] <- 'VALLE DEL CAUCA'
Prestadores[Prestadores$Departamento == 'CALI',]['Departamento'] <- 'VALLE DEL CAUCA'
Prestadores[Prestadores$Departamento == 'CARTAGENA',]['Departamento'] <- 'BOLÍVAR'
Prestadores[Prestadores$Departamento == 'SANTA MARTA',]['Departamento'] <- 'MAGDALENA'
Municipios[Municipios$Departamento == 'BOGOTÁ D C',]['Departamento'] <- 'BOGOTÁ DC'
Municipios[Municipios$Departamento == 'SAN ANDRÉS',]['Departamento'] <- 'SAN ANDRÉS Y PROVIDENCIA'


setdiff(Municipios['Departamento'], Prestadores['Departamento'])
setdiff(Prestadores['Departamento'], Municipios['Departamento'])
#return 0


## Municipios con prestadores y sin registro
setdiff(Prestadores['Municipio'], Municipios['Municipio'])
## retorna 20 municipios.
## Municipios con prestadores activos
## que no aparecen en la lista de municipios
## 
## Se tienen:
## GUACHENÉ
## SAN LUIS DE PALENQUE
## SINCÉ
## TUMACO
## 
## los demás valores que aparecen en setdiff están en la lista
## con diferencias de ortografía.

## Corregir errores
Prestadores['Municipio'][Prestadores['Municipio'] == 'GUATAPE',] <- 'GUATAPÉ'
Prestadores['Municipio'][Prestadores['Municipio'] == 'SONSON',] <- 'SONSÓN'
Prestadores['Municipio'][Prestadores['Municipio'] == 'BOGOTÁ',] <- 'BOGOTÁ DC'
Municipios['Municipio'][Municipios['Municipio'] == 'BOGOTÁ D C',] <- 'BOGOTÁ DC'
Prestadores['Municipio'][Prestadores['Municipio'] == 'TURBANA',] <- 'TURBANÁ'
Prestadores['Municipio'][Prestadores['Municipio'] == 'PAEZ',] <- 'PÁEZ'
Prestadores['Municipio'][Prestadores['Municipio'] == 'DIBULLA',] <- 'DIBULA'
Prestadores['Municipio'][Prestadores['Municipio'] == 'CHIBOLO',] <- 'CHIVOLO'
Prestadores['Municipio'][Prestadores['Municipio'] == 'PUEBLOVIEJO',] <- 'PUEBLO VIEJO'
Prestadores['Municipio'][Prestadores['Municipio'] == 'ACACÍAS',] <- 'ACACIAS'
Prestadores['Municipio'][Prestadores['Municipio'] == 'VISTAHERMOSA',] <- 'VISTA HERMOSA'
Prestadores['Municipio'][Prestadores['Municipio'] == 'MAGÜI',] <- 'MAGÜÍ'
Prestadores['Municipio'][Prestadores['Municipio'] == 'VALLE DEL GUAMUEZ',] <- 'VALLE DE GUAMEZ'
Prestadores['Municipio'][Prestadores['Municipio'] == 'CALARCA',] <- 'CALARCÁ'
Prestadores['Municipio'][Prestadores['Municipio'] == 'CHIMA',] <- 'CHIMÁ'
Municipios['Municipio'][Municipios['Municipio'] == 'CARMEN DE APICALA',] <- 'CARMEN DE APICALÁ'
Prestadores['Municipio'][Prestadores['Municipio'] == 'RIOBLANCO',] <- 'RIO BLANCO'

## correr setdiff() de nuevo para confirmar
setdiff(Prestadores['Municipio'], Municipios['Municipio'])
## resultado debe ser los 4 municipios mencionados antes



#############################
## Datos limpios, crear DB ##
#############################


#crear database en SQLite
conn <- dbConnect(RSQLite::SQLite(), 'PruebaDB.db')
dbListTables(conn)
dbWriteTable(conn, 't_Municipios', Municipios)
dbWriteTable(conn, 't_Prestadores', Prestadores)

dbListTables(conn)

### Municipios sin prestador registrado
setdiff(Municipios['Municipio'], Prestadores['Municipio']) %>% print(n=Inf)
#SQL
dbGetQuery(conn, "select distinct Municipio from t_Municipios where Municipio not in (select Municipio from t_Prestadores)")
#ambos retornan 122 elementos
###




#Observaciones limpias
dbGetQuery(conn, "select Departamento, count(Departamento) from t_Municipios group by Departamento") 
dbGetQuery(conn, "select Municipio from t_Municipios limit 20")


####################################
#######       Queries       ########
####################################
####################################

# Numero de prestadores por depto #
dbGetQuery(conn, "select Departamento, count(*) as n_prest from t_Prestadores group by Departamento")

#save query as table
n_prestadores_por_depto <- dbGetQuery(conn, "select Departamento, count(*) as n_prest from t_Prestadores group by Departamento")

#Plot
ggplot(Prestadores, aes(y=reorder(Departamento, Departamento, function(x)-length(x)))) +
     geom_bar(fill='red') +  labs(y='Depto')



###################################
# Numero de IPS, Hospitales, etc. #
# en cada region                  #
dbGetQuery(conn, "select Clpr_nombre, count(*) from t_Prestadores group by Clpr_nombre")

n_prestadores <- Prestadores %>%
  count(Clpr_nombre) %>% print(n=Inf)

## clpr_codigo = 1 -> IPS
## clpr_codigo = 2 -> Profesional Indep.
## clpr_codigo = 3 -> Transp. Especial de Pacientes
## clpr_codigo = 4 -> Objeto Social diferente a Prestación de Serv. de Salud

## IPS
IPS_por_depto <- dbGetQuery(conn, "select Departamento, count(*) as n_IPS from t_Prestadores where Clpr_codigo == 1 group by Departamento")

IPS_por_depto

## Profesional Independiente
PI_por_depto <- dbGetQuery(conn, "select Departamento, count(*) as n_PI from t_Prestadores where Clpr_codigo == 2 group by Departamento")

PI_por_depto


## Transp. Especial
TE_por_depto <- dbGetQuery(conn, "select Departamento, count(*) as n_TE from t_Prestadores where Clpr_codigo == 3 group by Departamento")

TE_por_depto


## Obj. Social diferente a prestadores de Salud
OS_dif_por_depto <- dbGetQuery(conn, "select Departamento, count(*) as n_OS_dif from t_Prestadores where Clpr_codigo == 4 group by Departamento")

OS_dif_por_depto


##Plot
ggplot(IPS_por_depto, aes(y=reorder(Departamento, desc(n_IPS)), x=n_IPS)) + geom_col()
ggplot(PI_por_depto, aes(y=reorder(Departamento, desc(n_PI)), x=n_PI)) + geom_col()

## Merge dataframes
prestadores_por_depto <- list(IPS_por_depto, PI_por_depto, TE_por_depto, OS_dif_por_depto)
prestadores_por_depto <- Reduce(function(x,y) merge(x,y, all=TRUE), prestadores_por_depto)


###############################
###############################
#### Calculate per capita #####

#Poblacion por depto

Poblaciones <- dbGetQuery(conn, "select m.Departamento, sum(m.Poblacion) from t_Municipios as m group by m.Departamento")
Poblaciones
#per capita
prestadores_per_capita <- prestadores_por_depto[,2:5]/Poblaciones[[2]]
prestadores_per_capita <- data.frame(Poblaciones['Departamento'],prestadores_per_capita)
##Plot
ggplot(prestadores_per_capita, aes(y=reorder(Departamento, desc(n_IPS)), x=n_IPS)) + geom_col(fill='blue') +
  labs(x='Numero de IPS per capita', y='Departamento', title = 'IPS per capita segun\n departamento')
ggplot(prestadores_per_capita, aes(y=reorder(Departamento, desc(n_PI)), x=n_PI))  + geom_col(fill='blue') +
  labs(x='Numero de PI per capita', y='Departamento', title = 'PI per capita segun\n departamento')


##Calculate number of IPS or PI relative to population number
## by municipio

## IPS
IPS_por_mun <- dbGetQuery(conn, "select Municipio, Departamento, count(*) as n_IPS from t_Prestadores where Clpr_codigo == 1 group by Municipio, Departamento")

IPS_por_mun

## Profesional Independiente
PI_por_mun <- dbGetQuery(conn, "select Municipio, count(*) as n_PI from t_Prestadores where Clpr_codigo == 2 group by Municipio")

PI_por_mun


## Transp. Especial
TE_por_mun <- dbGetQuery(conn, "select Municipio, count(*) as n_TE from t_Prestadores where Clpr_codigo == 3 group by Municipio")

TE_por_mun


## Obj. Social diferente a prestadores de Salud
OS_dif_por_mun <- dbGetQuery(conn, "select Municipio, count(*) as n_OS_dif from t_Prestadores where Clpr_codigo == 4 group by Municipio")

OS_dif_por_mun


##Plot
# 10 municipios con mas IPS
IPS_por_mun %>% arrange(desc(n_IPS)) %>% slice(1:10) %>%
ggplot(., aes(y=Municipio, x=n_IPS)) + geom_col()

# 10 municipios con mas PI
PI_por_mun %>% arrange(desc(n_PI)) %>% slice(1:10) %>%
  ggplot(., aes(y=Municipio, x=n_PI)) + geom_col()

## Municipios sin prestadores se encontraron al final de la
## limpieza de datos
# query:
dbGetQuery(conn, "select distinct Municipio from t_Municipios where Municipio not in (select Municipio from t_Prestadores)")
# retorna 122 municipios
# se puede observar la distribución de estos
mun_sin_prest <- dbGetQuery(conn, "select distinct count(m.Municipio) as n_mun, m.Departamento, sq.n_mun_total from t_Municipios as m join (select Departamento, count(Municipio) as n_mun_total from t_Municipios group by Departamento) as sq on sq.Departamento = m.Departamento where m.Municipio not in (select Municipio from t_Prestadores) group by m.Departamento")
mun_sin_prest %>%
  ggplot(., aes(y=reorder(Departamento,desc(n_mun)), x=n_mun)) + geom_col(fill='red') +
  labs(x='Numero de municipios sin prestadores', y='Departamento', title='Numero de municipios sin\n prestador por departamento') +
  theme(plot.title = element_text(size=10), axis.text = element_text(size=9))
# y en porcentajes
mun_sin_prest['n_per_cent'] <- mun_sin_prest['n_mun']/mun_sin_prest['n_mun_total']
mun_sin_prest %>%
  ggplot(., aes(y=reorder(Departamento,desc(n_per_cent)), x=n_per_cent)) + geom_col(fill='red') +
  labs(x='Porcentaje de municipios sin prestadores', y='Departamento', title='Porcentaje de municipios sin prestador\n por departamento') +
  theme(plot.title = element_text(size = 10, hjust = 0.5), axis.text = element_text(size=9)) + 
  scale_x_continuous(labels = scales::percent)


# Municipios con un único prestador

dbGetQuery(conn, "select Municipio, Clpr_nombre from t_Prestadores group by Municipio having count(Municipio) = 1")
# se observa que la mayoría son municipios con una IPS



# municipios con mayor/menor poblacion
dbGetQuery(conn, "select m.Municipio, m.Poblacion, count(p.Municipio) as n_prestadores from t_Municipios as m, t_Prestadores as p where m.Municipio = p.Municipio group by m.Municipio order by Poblacion desc limit 10")
dbGetQuery(conn, "select m.Municipio, m.Departamento, m.Poblacion, count(p.Municipio) as n_prestadores from t_Municipios as m, t_Prestadores as p where (m.Municipio = p.Municipio) & (m.Departamento = p.Departamento) group by m.Municipio order by Poblacion asc limit 100")
# el query no es veloz, sin embargo muestra
# los 100 municipios con menor población en Colombia
# que poseen al menos 1 proveedor de salud, así como 
# el número de proveedores que posee
dbGetQuery(conn, "select Municipio, Clpr_nombre, Nombre_prestador from t_Prestadores where Municipio=?", params= 'ALBANIA')

#ejemplo de tres municipios con el mismo nombre
dbGetQuery(conn, "select * from t_Municipios where Municipio=?", params = "LA VICTORIA")



#################################
######### Fechas ################
#################################
  
# Fecha de radicacion
Radicacion <- dbGetQuery(conn, "select Fecha_radicacion, count(Fecha_radicacion) as count from t_Prestadores group by Fecha_radicacion")
#fechas mas comunes
dbGetQuery(conn, "select Fecha_radicacion, count(Fecha_radicacion) from t_Prestadores group by Fecha_radicacion order by count(Fecha_radicacion) desc limit 15")

Radicacion['Fecha_radicacion'] <- Radicacion['Fecha_radicacion'] %>%
  mutate(Fecha_radicacion = as.Date(Fecha_radicacion, '%Y%m%d'))


ggplot(Radicacion, aes(x=Fecha_radicacion, y=count)) + geom_line() + labs(x='Fecha radicación', y='Cantidad de entradas', title='Frecuencia de fechas de radicación') +
  theme(plot.title = element_text(size = 9, hjust = 0.5), axis.text = element_text(size=8))

#Fecha de vencimiento
Vencimiento <- dbGetQuery(conn, "select Fecha_vencimiento, count(Fecha_vencimiento) as count from t_Prestadores group by Fecha_vencimiento")
#fechas mas comunes
dbGetQuery(conn, "select Fecha_vencimiento, count(Fecha_vencimiento) from t_Prestadores group by Fecha_vencimiento order by count(Fecha_vencimiento) desc limit 15")

Vencimiento['Fecha_vencimiento'] <- Vencimiento['Fecha_vencimiento'] %>%
  mutate(Fecha_vencimiento = as.Date(Fecha_vencimiento, '%Y%m%d'))

ggplot(Vencimiento, aes(x=Fecha_vencimiento, y=count)) + geom_line() + labs(x='Fecha vencimiento', y='Cantidad de entradas', title='Frecuencia de fechas de vencimiento') +
  theme(plot.title = element_text(size = 9, hjust = 0.5), axis.text = element_text(size=8))

