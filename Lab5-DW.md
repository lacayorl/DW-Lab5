Lab5 DW
================
Roberto Lacayo
3/10/2021

# Parte 1: Predecir el siguiente Eclipse Solar

Fecha del ultimo ecilpse solar, 21 de agosto del 2017 a las 18:26:40 El
siguiente eclipse se dará e un saros y un Saros equivale a 223 Synodic
Months 1 Synodic Month equivale a 29 días con 12 horas, con 44 minutos y
3 segundos

``` r
Fecha_ultimo_eclipse <- ymd_hms("2017-august-21-18:26:40")
Fecha_ultimo_eclipse
```

    ## [1] "2017-08-21 18:26:40 UTC"

``` r
synodic_month <- ddays(29 ) + dhours(12) + dminutes(44) + dseconds(3)
synodic_month
```

    ## [1] "2551443s (~4.22 weeks)"

``` r
saros <- 223*synodic_month
saros
```

    ## [1] "568971789s (~18.03 years)"

``` r
Fecha_siguiente_eclipse <- Fecha_ultimo_eclipse + saros
Fecha_siguiente_eclipse
```

    ## [1] "2035-09-02 02:09:49 UTC"

# Parte 2: Agrupaciones y Operaciones con fechas

``` r
data$tipo_num <- str_detect(data$`Fecha Creación`, "-")
data$Fecha_C <- ifelse(data$tipo_num == FALSE,
                       yes=as.Date(as.numeric(data$`Fecha Creación`)),
                       no=dmy(data$`Fecha Creación`))
```

    ## Warning in as.Date(as.numeric(data$`Fecha Creación`)): NAs introducidos por
    ## coerción

    ## Warning: 104237 failed to parse.

``` r
data$Fecha_C <- as.Date(data$Fecha_C)
data$Hora_C <- format(data$`Hora Creación`, format ="%H:%M:%S")
data$Hora_F <- format(data$`Hora Final`, format ="%H:%M:%S")
data$Mes <- month(data$Fecha_C)
data$Dia <- weekdays(data$Fecha_C)
```

## 1. ¿En qué meses existe una mayor cantidad de llamadas por código?

``` r
llamadasXcodigo_mes <- data %>% 
  filter(Call == 1) %>% 
  group_by(Mes, Cod) %>%
  summarize(LLamadas = n())
```

    ## `summarise()` has grouped output by 'Mes'. You can override using the `.groups` argument.

``` r
llamadasXcodigo_mes <- llamadasXcodigo_mes[order(llamadasXcodigo_mes$LLamadas, 
                                                 decreasing = TRUE),]
llamadasXcodigo_mes
```

    ## # A tibble: 12 x 3
    ## # Groups:   Mes [12]
    ##      Mes Cod                          LLamadas
    ##    <dbl> <chr>                           <int>
    ##  1     3 Actualización de Información      497
    ##  2     7 Actualización de Información      496
    ##  3     5 Actualización de Información      494
    ##  4    11 Actualización de Información      493
    ##  5    10 Actualización de Información      487
    ##  6    12 Actualización de Información      478
    ##  7     8 Actualización de Información      474
    ##  8     6 Actualización de Información      471
    ##  9     1 Actualización de Información      465
    ## 10     9 Actualización de Información      465
    ## 11     4 Actualización de Información      462
    ## 12     2 Actualización de Información      443

Podemos ver que el codigo con más llamadas es el de Actualización de
Información y el mes con mayor cantidad de llamadas es marzo, algo
interesante es que las llamadas aumentan en los meses en que se termina
algun tipo de ciclo, como los trimestres o semestres del año.

## 2. ¿Qué día de la semana es el más ocupado?

``` r
Dia_mas_ocupado <- data %>%
  group_by(Dia) %>% 
  summarize(LLamadas = n())
Dia_mas_ocupado <- Dia_mas_ocupado[order(Dia_mas_ocupado$LLamadas,
                                         decreasing = TRUE),]
Dia_mas_ocupado
```

    ## # A tibble: 7 x 2
    ##   Dia       LLamadas
    ##   <chr>        <int>
    ## 1 viernes      39083
    ## 2 lunes        38339
    ## 3 jueves       38179
    ## 4 martes       37726
    ## 5 domingo      36921
    ## 6 sábado       36873
    ## 7 miércoles    36604

El día más ocupado es el viernes con 39,083 llamadas o eventos que se
atienden

## 3. ¿Qué mes es el más ocupado?

``` r
Mes_mas_ocupado <- data %>%
  group_by(Mes) %>% 
  summarize(LLamadas = n())
Mes_mas_ocupado <- Mes_mas_ocupado[order(Mes_mas_ocupado$LLamadas,
                                         decreasing = TRUE),]
Mes_mas_ocupado
```

    ## # A tibble: 12 x 2
    ##      Mes LLamadas
    ##    <dbl>    <int>
    ##  1     3    22708
    ##  2    10    22601
    ##  3     5    22525
    ##  4     7    22514
    ##  5     1    22425
    ##  6     8    22316
    ##  7    12    22151
    ##  8     9    21891
    ##  9    11    21681
    ## 10     4    21611
    ## 11     6    21370
    ## 12     2    19932

El mes más ocupado es marzo y al igual que en la primera pregunta
podemos notar que casi son los mismos meses

## 4. ¿Existe una concentración o estacionalidad en la cantidad de llamadas?

``` r
ggplot(llamadasXcodigo_mes, aes(x=Mes, y=LLamadas, group = 1)) +
  geom_line(color = "blue")
```

![](Lab5-DW_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Podemos notar que nuestro supuesto en las preguntas anteriores no está
tan alejado de la realidad y existen subidas en los cuatro trimestres
del año, por lo que tambien podemos decir que hay cierta estacionalidad

## 5. ¿Cuántos minutos dura la llamada promedio?

``` r
data$Duracion <- difftime((data$`Hora Final`),(data$`Hora Creación`),units = "mins")
LLamadas <- data %>%
  filter(Call == 1)%>%
  select((Duracion)) 
mean(LLamadas$Duracion)
```

    ## Time difference of 14.5579 mins

En promedio las llamadas duran 14 minutos con 56 segundos

## 6. Realice una tabla de frecuencias con el tiempo de llamada.

``` r
Frecuancia_llamadas <- as.data.frame(table(LLamadas))
names(Frecuancia_llamadas)[1] <- "Duracion llamada (min)"
names(Frecuancia_llamadas)[2] <- "Cantidad de llamadas"
Frecuancia_llamadas
```

    ##    Duracion llamada (min) Cantidad de llamadas
    ## 1                       0                  221
    ## 2                       1                  211
    ## 3                       2                  173
    ## 4                       3                  195
    ## 5                       4                  193
    ## 6                       5                  184
    ## 7                       6                  194
    ## 8                       7                  197
    ## 9                       8                  212
    ## 10                      9                  166
    ## 11                     10                  190
    ## 12                     11                  197
    ## 13                     12                  169
    ## 14                     13                  163
    ## 15                     14                  203
    ## 16                     15                  188
    ## 17                     16                  181
    ## 18                     17                  178
    ## 19                     18                  186
    ## 20                     19                  190
    ## 21                     20                  179
    ## 22                     21                  205
    ## 23                     22                  175
    ## 24                     23                  192
    ## 25                     24                  186
    ## 26                     25                  174
    ## 27                     26                  157
    ## 28                     27                  173
    ## 29                     28                  158
    ## 30                     29                  171
    ## 31                     30                  164

# Parte 3: Signo Zodiacal

## Ingresar su fecha de nacimiento en formato Y-M-D

``` r
signo_zodiacal <- function(BD){
  
  BD2 <- ymd(BD)
  mes <- month(BD2)
  dia <- day(BD2)
  
  if(mes == 1){
    if(dia < 20){
      signo <- "capricornio"
    }else
      signo <- "acuario"
  }else{
    if(mes == 2){
      if(dia < 19){
        signo <- "acuario"
      }else{
        signo <- "piscis"
      }
    }else{
      if(mes == 3){
        if(dia < 21){
          signo <- "piscis"
        }else{
          signo <- "aries"
        }
      }else{
        if(mes == 4){
          if(dia < 20){
            signo <- "aries"
          }else{
            signo <- "tauro"
          }
        }else{
          if(mes == 5){
            if(dia < 21){
              signo <- "tauro"
            }else{
              signo <- "geminis"
            }
          }else{
            if(mes == 6){
              if(dia < 21){
                signo <- "geminis"
              }else{
                signo <- "cancer"
              }
            }else{
              if(mes == 7){
                if(dia < 23){
                  signo <- "cancer"
                }else{
                  signo <- "leo"
                }
              }else{
                if(mes == 8){
                  if(dia < 23){
                    signo <- "leo"
                  }else{
                    signo <- "virgo"
                  }
                }else{
                  if(mes == 9){
                    if(dia < 23){
                      signo <- "virgo"
                    }else{
                      signo <- "libra"
                    }
                  }else{
                    if(mes == 10){
                      if(dia < 23){
                        signo <- "libra"
                      }else{
                        signo <- "escorpio"
                      }
                    }else{
                      if(mes == 11){
                        if(dia < 22){
                          signo <- "escorpio"
                        }else{
                          signo <- "sagitario"
                        }
                      }else{
                        if(mes == 12){
                          if(dia < 22){
                            signo <- "sagitario"
                          }else{
                            signo <- "capricornio"
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(signo)
  
}

signo_zodiacal("2000-august-9")
```

    ## [1] "leo"

# Parte 4: Flights

## 1. Genere 4 nuevas columnas para cada variable con formato fecha y hora.

``` r
Hora_buena <-  function(Y, M, D, H) {
  make_datetime(Y, M, D, H %/% 100, H %% 100)
}

flights$dep_time_new <- Hora_buena(flights$year,
                                   flights$month,
                                   flights$day,
                                   flights$dep_time)
flights$sched_dep_time_new <- Hora_buena(flights$year,
                                         flights$month,
                                         flights$day,
                                         flights$sched_dep_time)
flights$arr_time_new <- Hora_buena(flights$year,
                                   flights$month,
                                   flights$day,
                                   flights$arr_time)
flights$sched_arr_time_new <- Hora_buena(flights$year,
                                   flights$month,
                                   flights$day,
                                   flights$sched_arr_time)

flights %>% select(sched_dep_time_new,sched_arr_time_new,dep_time_new,arr_time_new)
```

    ## # A tibble: 336,776 x 4
    ##    sched_dep_time_new  sched_arr_time_new  dep_time_new       
    ##    <dttm>              <dttm>              <dttm>             
    ##  1 2013-01-01 05:15:00 2013-01-01 08:19:00 2013-01-01 05:17:00
    ##  2 2013-01-01 05:29:00 2013-01-01 08:30:00 2013-01-01 05:33:00
    ##  3 2013-01-01 05:40:00 2013-01-01 08:50:00 2013-01-01 05:42:00
    ##  4 2013-01-01 05:45:00 2013-01-01 10:22:00 2013-01-01 05:44:00
    ##  5 2013-01-01 06:00:00 2013-01-01 08:37:00 2013-01-01 05:54:00
    ##  6 2013-01-01 05:58:00 2013-01-01 07:28:00 2013-01-01 05:54:00
    ##  7 2013-01-01 06:00:00 2013-01-01 08:54:00 2013-01-01 05:55:00
    ##  8 2013-01-01 06:00:00 2013-01-01 07:23:00 2013-01-01 05:57:00
    ##  9 2013-01-01 06:00:00 2013-01-01 08:46:00 2013-01-01 05:57:00
    ## 10 2013-01-01 06:00:00 2013-01-01 07:45:00 2013-01-01 05:58:00
    ## # ... with 336,766 more rows, and 1 more variable: arr_time_new <dttm>

## 2. Encuentre el delay total que existe en cada vuelo.

El delay total se puede encontrar sumando el delay de la salida y el
delay de la entrada.

``` r
flights$Total_Delay <- flights$dep_delay + flights$arr_delay
flights %>% select(origin,dest,Total_Delay)
```

    ## # A tibble: 336,776 x 3
    ##    origin dest  Total_Delay
    ##    <chr>  <chr>       <dbl>
    ##  1 EWR    IAH            13
    ##  2 LGA    IAH            24
    ##  3 JFK    MIA            35
    ##  4 JFK    BQN           -19
    ##  5 LGA    ATL           -31
    ##  6 EWR    ORD             8
    ##  7 EWR    FLL            14
    ##  8 LGA    IAD           -17
    ##  9 JFK    MCO           -11
    ## 10 LGA    ORD             6
    ## # ... with 336,766 more rows
