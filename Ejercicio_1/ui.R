

library(shiny)
library(ggplot2)

# Cargamos los datos desde el repositorio github del módulo 
dataset <- get(load(url("https://github.com/pedroconcejero/master_UNED/blob/master/datos_4510_vehiculos_2016.rda?raw=true")))

#Una pequeña modificacion en el tipo 
dataset$Tipo[dataset$FuelType == "Electricity / Petrol"] <- "Hibrido Gasolina"
dataset$Tipo[dataset$FuelType == "Electricity / Diesel"] <- "Hibrido Diesel"
dataset$Tipo[dataset$FuelType == "Diesel Electric"] <- "Hibrido Diesel"
dataset$Tipo[dataset$FuelType == "Petrol Electric"] <- "Hibrido Gasolina"
dataset$Tipo[dataset$FuelType == "Petrol Hybrid"] <- "Hibrido Gasolina"

# Y una clarificacion en las cajas de cambio. Las semiautomaticas se consideran automaticas
dataset$Transm="Manual"
dataset$Transm[grep("A",dataset$Transmission,ignore.case = TRUE)]<-"Automatico"

# distinguimos variables "a nivel de intervalo" ("continuas" para ggplot)
nums <- sapply(dataset, is.numeric)
continuas <- names(dataset)[nums]

# y variables "categóricas" ("discretas" para ggplot)
cats <- sapply(dataset, is.character)
categoricas <- names(dataset)[cats]

shinyUI(
  navbarPage("Shiny Visualización Avanzada",
             tabPanel("Descripción del trabajo",
                      fluidPage(
                        h1("Ejercicio Visualización Avanzada", align = "center"),
                        h2("Realizado por Victor M. del Canto Godino", align = "center"),
                        br(br()),
                        tags$ul(
                          tags$li("Se realiza un estudio basado en los datos propuestos de modelos de 
                          vehiculos disponibles en Reino Unido en 2016 ."),
                          br(),
                          tags$li("La segunda pestaña plantea un gráfico interactivo que explora
                                  medidas de 4500 automóviles en venta en el mercado de Reino Unido en 2016"),
                          br(),
                          tags$li("La tercera pestaña 'Transmision' realiza una exploracion por la relacion entre
                                  la oferta de modelos y el tipo de transmision"),
                          br(),
                          tags$li("La cuarta pestaña 'Emisiones' buscamos relaciones entre los diversos tipos de motores,
                                  las cilindradas y las diferentes emisiones y sus limitaciones legales y fiscales"),
                          br(),
                          tags$li("La quinta y ultima pestaña profundiza entre la dependencia del coste con las tecnologias
                                  de propulsion y su capacidad, ademas de hacer una valoracion sobre el consumo"),
                          br(),
                          tags$li("Fuera de este estudio se queda el tema de los ruidos por tener una normativa muy rigida 
                                  y limitativa y sobre todo por estar desvinculada su relacion con los propulsores, 
                                  en cualquiera de sus caracteristicas (cilindrada,tecnologia), al influir tambien el 
                                  aislamiento acustico del habitaculo, de lo cual no tenemos datos"),
                          br(),
                          tags$li("En las pestañas especificas se extraen interesantes conclusiones acerca de cada tema")
                        )

                        )),
             tabPanel("Scatterplot consumo y más",
                      sidebarPanel(
                        
                        selectInput('x', 'Elige variable para eje X', continuas, continuas[[1]]),
                        selectInput('y', 'Elige variable para eje Y', continuas, continuas[[8]]),
                        selectInput('color', 'Color', c('None', 'Tipo')),
                        
                        checkboxInput('lm', 'Línea de Regresión'),
                        checkboxInput('smooth', 'Suavizado LOESS'),
                        
                        selectInput('facet_row', 'Elige variable para facetas por filas', c(None='.', categoricas))
                      ),
                      
                      mainPanel(
                        plotOutput('plot1',
                                   height=500)
                        
                      )
             ),
              tabPanel("Transmision",
                       fluidPage(
                         fluidRow(
                           column(12,
                                  h2("ESTUDIO SOBRE LA TRANSMISION",align="center"),
                                  br(),
                                  fluidRow(
                                    column(1),
                                    column(4,
                                           br(br()),
                                           p("Para empezar este epigrafe veremos primero todos los tipos de 
                                      transmision que tenemos en los datos. Segun la documentacion tenemos un 
                                             total de 25 tipos de transmision en funcion de los grados de 
                                             automatizacion, secuencialidad, traccion a las ruedas, cantidad de marchas, etc.."),
                                           br(),
                                           p("Por operatividad vamos a reducirlas unicamente a 2: Manual y automatica. 
                                             Con el criterio sencillo de que toda la que tenga algun tipo de automatizacion 
                                             se considera automatica y el resto manual. "),
                                           br(),
                                           p("Para ello con los codigos facilitados en la documentacion hacemos una discriminacion
                                             entre los codigos que tienen una 'A' y los que no"),
                                           br(),
                                           p("Es importante destacar que el estudio se realiza sobre el total de modelos ofertados en 
                                              el mercado, no puede extrapolarse ningun razonamiento al total de vehiculos real")
                                           ),
                                    column(2),
                                    column(5,
                                           fluidRow(
                                             tags$embed(src="Pdf/transmision.pdf",width=500,height=700)
                                           )
                                           )
                                    ),
                                  br()
                                  )
                           ),
                         fluidRow(
                           h4("A continuacion reproducimos una diferenciacion en los modelos por marca y tipo de transmision 
                              unicamente a titulo informativo",align="center"),
                           br(),
                           plotOutput("plot2",width="100%",height = 500),
                           br(),
                           column(4,
                             br(br()),
                             h4("PUEDE ESCOGER UNA MARCA PARA VER EN DETALLE",align="center"),
                             br(),
                             selectInput("marca",
                                         label="ESCOJA UNA MARCA",choices = dataset$Manufacturer)
                           ),
                           column(4,
                                  plotOutput("plot16")
                                  ),
                           column(4,
                                  plotOutput("plot17"))
                         ),
                         fluidRow(
                           br(),
                           h4("Aqui esta representado el total de vehiculos. Puede comprobarse visualmente que 
                              los modelos de transmision manual representan casi el doble que los de automatica",align="center"),
                           br(),
                           plotOutput("plot3",height = 500),
                           br(br())
                         ),
                         fluidRow(
                           column(6,
                                  h4("Representacion del grafico de cajas y bigotes",align="center"),
                                  br(),
                                  p("Aqui aparece una de las caracteristicas principales que encontramos al estudiar 
                                  la transmision de los vehiculos. La relacion entre cilindrada y transmision desvela que 
                                  el rango intercuartílico de los modelos con transmision automatica comienza justo cuando termina el 
                                    de los modelos de transmision manual"),
                                  br(),
                                  p("Tambien se puede apreciar que la mediana Q2 en los modelos de transmision automatica esta junto 
                                  al cuartil Q1 con lo que tenemos en el entorno de la cilindrada 2000 cc. un 25% del total de modelos de transmision automatica"),
                                  br(),
                                  p("Por debajo de esa misma cilindrada se concentra el 75% de los modelos de transmision manual. 
                                  En este grupo de vehiculos se aprecia una mayor simetria y una menor dispersion"),
                                  br(),
                                  p("Los valores atipicos se reparten casi por igual en ambos grupos, en vehiculos de alta cilindrada"),
                                  br(br())
                           
                           ),
                           column(6,
                                  plotOutput("plot4",height = 500)
                           )
                         )
                           
                         
                       )
                        
              ),
             tabPanel("Emisiones",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("ESTUDIO SOBRE LAS EMISIONES",align="center"),
                                 br(),
                                 fluidRow(
                                   column(5,
                                          tags$embed(src="Pdf/EmisionEuro6.pdf",width=500,height=700)
                                          ),
                                   column(2,
                                          p("En primer lugar vamos a analizar la documentacion adjunta a los datos donde queda 
                                       expuestos los limites marcados por la normativa Euro6. "),
                                          br(),
                                          p("En la nomenclatura tenemos CO (monoxido de carbono), THC (hidrocarburos totales),
                                       NMHC(monoxidos de carbono no metanico), NOx (varios oxidos de nitrogeno), PM (particulas)"),
                                          br(),
                                          p("En la pagina de la izquierda tenemos los valores maximos admitidos en funcion del tipo de motor 
                                            termico (puro o hibrido) que incorpore el vehiculo. Vemos que hay ciertos parametros no exigidos para ciertos tipos"),
                                          br(),
                                          p("En la pagina de la derecha tenemos las tasas repercutidas en cada modelo segun 
                                            sea las emisiones de dioxido de carbono (CO2)")
                                          ),
                                   column(5,
                                          tags$embed(src="Pdf/EmisionCO2.pdf",width=500,height=700)
                                          ),
                                   fluidRow(br())
                                   
                                 )
                          ),
                                 fluidRow(
                                   br(),
                                   p("No se ha apreciado que exista una relacion lineal entre los parametros de emisiones con limitaciones y la 
                                     cilindrada de los modelos. Para verlo propiamente se recomienda ir a la pestaña 'Scatterplot consumo y mas'"),
                                   br(),
                                   p("A continuacion observamos unos graficos de caja y bigotes donde se pueden apreciar ciertas caracteristicas reseñables.",align="center"),
                                   br()
                                 ),
                          
                                 fluidRow(
                                   column(4,
                                          p("Las emisiones de CO son bastante superiores en los modelos de gasolina que los de diesel, lo que era esperado. 
                                            Esta es una de los razones por la que se fomenta el uso del diesel hace unos años. La mediana del gasolina esta 
                                            por encima del cuantil Q3 en el diesel. Sin embargo cuando entramos a valorar los hibridos da un vuelco la situacion, 
                                            aun a pesar de las pocas observaciones,sobre todo en hibrido diesel")
                                          ),
                                   column(4, 
                                          p("Las emisiones de oxidos del nitrogeno NOx son sobre todo emitidas por los diesel ,aunque segun se han ido mejorando en 
                                            eficiencia y emisiones de CO2 los motores de gasolina han aumentado las correspondientes a este epigrafe. Por curiosidad 
                                            ninguna diesel supera el limite marcado legalmente(80), pero si hay 3 gasolinas que superan el limite legal(60)"),   
                                          p("CHRYSLER JEEP-Jeep Renegade, MY2016  INFINITI-Q30 Euro6, 2016   RENAULT-Twingo Euro6, 2015")
                                          ),
                                   column(4,
                                          p("Las particulas son territorio diesel.Parecen estar concentradas la mayoria entre 0  y 0,5, pero hay muchos valores atipicos. 
                                            Extrañamente todas las observaciones en gasolina estan sin datos (NA) salvo 4 modelos KIA" )
                                          ),
                                    br(br())
                                 ),
                          fluidRow(
                            column(4,
                                   plotOutput("plot5",height = 500)
                                   ),
                            column(4,
                                   plotOutput("plot6",height = 500)
                                   ),
                            column(4,
                                   plotOutput("plot9",height = 500)
                                   ),
                            br()
                          ),
                         hr(),
                         fluidRow(
                           column(4,
                                  p("A destacar en este grafico unicamente las pocas observaciones que aparecen en diesel, unicamente 29 y la desaparicion de los hibridos
                                    diesel. No existen limites para esta tecnologia porque fundamentalmente no los produce")
                                  ),
                           column(4,
                                  p("Estas emisiones solo se producen en diesel. Estan concentradas de forma bastante simetrica en torno a su mediana que es aproximadamente de 80. 
                                    El limite esta en 170 .Existen algunos valores atipicos pero el mayor esta en 163")
                                  ),
                           column(4,
                                  p("No soy capaz de apreciar caracteristicas apreciables en este grafico salvo que hay cierta dispersion, pero parece que hay ciertas relaciones que estudiaremos en el epigrafe siguiente")
                                  ),
                           br(br())
                         ),
                         fluidRow(
                           column(4,
                                  plotOutput("plot7",height = 500)
                                  ),
                           column(4,
                                  plotOutput("plot8",height = 500)
                                  ),
                           column(4,
                                  plotOutput("plot10",height= 500)
                                  )
                         ),
                         fluidRow(
                           hr(),
                           h4("Vamos a estudiar detenidamente las relaciones de los vehiculos y el CO2."),
                           p("En primer lugar observar una relacion directa entre cilindrada y emisiones CO2, 
                             salvo en los hibridos diesel, pero se debe a las pocas observaciones de que disponemos")
                         ),

                         
                         plotOutput("plot11"),
                         fluidRow(
                           column(4,
                                  br(),
                                  p("Como se puede apreciar en la documentacion existe una penalizacion fiscal para los 
                                    vehiculos en funcion de los valores de emision de CO2. Para eso establece unos intervalos. 
                                    He dividido los datos en esos mismos intervalos y los he separado segun el tipo de motor. 
                                    Estos son los histogramas con los valores en cada banda"),
                                  p("Se puede observar la agrupacion de la mayor parte de los modelos diesel en las bandas de 
                                    menor penalizacion, y sin embargo los modelos de gasolina estan mas repartidos")
                                  ),
                           column(8,
                                  plotOutput("plot19")
                                  )
                         )
                        )
                      )
              ),
             tabPanel("Coste",
                       fluidPage(
                         fluidRow(
                           plotOutput("plot12",height = 500),
                           hr()
                         ),
                         fluidRow(
                           column(width=6,
                                  
                                    h2("RELACION COSTE-CILINDRADA",align="center"),
                                    p("Escogemos como predictor para el coste de carburante en  12000 millas la cilindrada del vehiculo. 
                                      En principio eso nos deja fuera a los vehiculos electricos, por lo que este epigrafe lo desdoblamos en dos:", align="center"),
                                    h3("1 - Estudio sobre los vehiculos con al menos un  motor termico "),
                                    p("En el grafico superior se aprecia claramente una relacion lineal entre el gasto en energia y la cilindrada. 
                                      En la documentacion adjunta a los datos tenemos otras claves interesantes. 
                                      El coste se calcula con la formula: ")
                                  ,
                                  fluidRow(
                                    tags$img(src="Images/Coste.jpg", width = "650px", height = "325px")
                                  )
                                    ),
                           column(width=6, 
                                    plotOutput("plot13",height = 600)
                                  )       
                                  ),
                         hr(),
                         fluidRow(
                           column(width=12,
                                    p("En el grafico sobre estas lineas vemos junto todas las rectas extraidas del modelo de regresion lineal, juntas sin los puntos de las observaciones. 
                                      Como el precio de la gasolina y el diesel es practicamente identico la diferencia entre las lineas se puede achacar a las diferencias de consumos. 
                                      Ese mismo aspecto es aplicable a los hibridos con los diferentes carburantes, con la salvedad de que la parte de consumo electrico es mucho mas barata. 
                                      Asimismo es apreciable una mayor pendiente en la linea de los motores de gasolina achacable a su superior consumo. "),
                                    hr(),
                                    h3("2 - Estudio sobre los vehiculos electricos ")
                                  
                            ),
                           hr(),
                         fluidRow(
                           plotOutput("plot14"),
                           br(),br(),br(),br(),br(),br()   
                         ),fluidRow(
                           column(6,
                                  p("Puede apreciarse en el grafico superior una relacion lineal entre la potencia electrica y la autonomia
                                    en los vehiculos con motor electrico puro. En los sistemas hibridos esta limitada la autonomia puesto que 
                                    por su configuracion la parte electrica de propulsion solos se utiliza en velocidades reducidas y cuando 
                                    se aproxima el fin de la carga o se aumenta la velocidad se activa el motor termico convencional "),
                                  p("Sin embargo el grafico que tenemos en la parte derecha podria hacenos pensar que hemos encontrado 
                                    una relacion muy directa entre la potencia de los motores electricos y el coste en 12000 millas.
                                    Y que la dispersion que existe en los modelos hibridos se debe precisamente a su doble condicion. 
                                    Sin embargo esto esconde una pequeña trampa y es que sí existe una relacion directa puesto que el calculo 
                                    del coste se basa en una formula proporcional a la potencia como figura en la documentacion de los datos")
                                  ,
                                  tags$img(src="Images/Coste2.jpg", width = "650px", height = "325px")
                                  )
                            ,column(6,
                                    plotOutput("plot15")
                                    ),
                           fluidRow()
                            ),
                         hr(),
                         br(),
                         h3("EL CONSUMO",align="center"),
                         fluidRow(
                           column(12,
                                  br(br()),
                           fluidRow(
                             column(4,
                                    br(),
                                    p("En el grafico de la derecha vemos los consumos urbanos y extraurbanos separados y por tipos de motor."),
                                    p("Lo primero que se aprecia es la relacion directa con la cilindrada."), 
                                    p("En segundo lugar que los consumos son mayores en la medida urbana en todos los tipos."),
                                    p("Y en tercer lugar que el incremento de cilindrada supone proporcionalmente un mayor consumo urbano que extraurbano (pendientes de las rectas)."),
                                    p("Ademas queda claro el ahorro en consumo en los diesel respecto a los gasolina en todos los tramos")
                                    ),
                             column(8,
                                    plotOutput("plot18")
                                    )
                           )
                         )
                         )
                           )
                        
                       )
                       )
                       )
)
