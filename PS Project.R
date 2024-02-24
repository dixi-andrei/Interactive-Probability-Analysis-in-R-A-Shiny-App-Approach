# a.
frepcomgen <- function(m, n) {
  return(matrix(0, nrow = m, ncol = n))
}

m <- 2
n <- 3

# freqcom ca sa creem tabelul gol iar dupa ii vom pune valoriile
repartitie_comuna <- frepcomgen(m, n)
print(repartitie_comuna)
# aici vom avea afisat repartitia goala cu 0 peste tot

# b.
fcompleprecom <- function(freq_table, completions) {
  for (completion in completions) {
    freq_table[completion$x, completion$y] <- completion$value
  }
  return(freq_table)
}


completions <- list(
  list(x=1, y=1, value=0.1),
  list(x=1, y=2, value=0.2),
  list(x=2, y=1, value=0.3),
  list(x=2, y=3, value=0.4)
)

# completam repartitia cu valoriile pe care le-am pus in lista
repartitie_completa <- compleprecom(repartitie_comuna, completions)
print(repartitie_completa)


# c.
frepmarginal <- function(repartitie_comuna) {
  # repartitia marginala pentru X se calculeaza adunand coloanele 
  repartitia_marginala_X <- apply(repartitie_comuna, 1, sum, na.rm = TRUE)
  
  # repartitia marginala pentru Y se calculeaza adunand randurile
  repartitia_marginala_Y <- apply(repartitie_comuna, 2, sum, na.rm = TRUE)
  
  # functia returneaza o lista cu ambele repartitii marginale
  return(list(repartitia_marginala_X = repartitia_marginala_X,
              repartitia_marginala_Y = repartitia_marginala_Y))
}

repartitii_marginale <- frepmarginal(repartitie_completa)

# afisam repartitile marginale 
print(repartitii_marginale$repartitia_marginala_X)
print(repartitii_marginale$repartitia_marginala_Y)


#d.
fpropcov <- function(repartitie_comuna, a, b, c, d) {
  
  X <- row(repartitie_comuna)  # X este indexul randului
  Y <- col(repartitie_comuna)  # Y este indexul coloanei
  
  E_X <- sum(X * repartitie_comuna)
  E_Y <- sum(Y * repartitie_comuna)
  
  # calculam E[X] si E[Y]
  
  # scriem Z si T
  Z <- a * X + b * Y
  T <- c * X + d * Y
  
  # calculam E[Z], E[T] si E[ZT]
  E_Z <- sum(Z * repartitie_comuna)
  E_T <- sum(T * repartitie_comuna)
  E_ZT <- sum(Z * T * repartitie_comuna)
  
  # calculam covarianta Cov(Z,T)
  cov_ZT <- E_ZT - E_Z * E_T
  
  return(cov_ZT)
}

# dam valori coeficientilor a,b,c,d 
a <- 2
b <- 3
c <- 2
d <- 1

# apelam functia cu repartitia de la punctul b si afisam covarianta
covarianta <- fpropcov(repartitie_completa, a, b, c, d)
print(covarianta)

#e.
fPcond <- function(repartitie_comuna, x, y) {
  # repartitia marginala pentru Y
  P_Y <- sum(repartitie_comuna[, y])
  
  # obtinem probabilitatea comuna pentru perechea (x, y)
  P_X_and_Y <- repartitie_comuna[x, y]
  
  # calculam probabilitatea conditionata folosind formula si o returnam
  P_X_given_Y <- P_X_and_Y / P_Y
  return(P_X_given_Y)
}

# dam valori lui x si y pentru care vrem sa calculam probabilitatea,apelam functia si afisam probabilitatea
x_val <- 2
y_val <- 1 

prob_cond <- fPcond(repartitie_completa, x_val, y_val)
print(prob_cond)

#f.
fPcomun <- function(repartitie_comuna, x, y) {
  # accesam direct probabilitatea comuna pentru perechea (x, y)
  P_XY <- repartitie_comuna[x, y]
  return(P_XY)
}

# dam valori lui x si y pentru care vrem sa calculam probabilitatea legata de perechea (x,y)
x_val <- 2
y_val <- 1 

prob_comuna <- fPcomun(repartitie_completa, x_val, y_val)
print(prob_comuna)

#g.
# calcul Cov(5X+9, 3Y-2)
cov_calcul <- function(repartitie_comuna) {
  X <- row(repartitie_comuna)  
  Y <- col(repartitie_comuna)  
  
  #folosim Z si T pentru a calcula valorile E[5X+9] si E[3Y-2]
  Z <- 5 * X + 9
  T <- 3 * Y - 2
  
  E_Z <- sum(Z * repartitie_comuna)
  E_T <- sum(T * repartitie_comuna)
  
  # calculam E[ZT]
  E_ZT <- sum(Z * T * repartitie_comuna)
  
  # calculam covarianta Cov(Z,T)
  cov_ZT <- E_ZT - E_Z * E_T
  
  return(cov_ZT)
}

covariantaZT <- cov_calcul(repartitie_completa)
print(covariantaZT)

# calcul P(0<X<0.8|Y>0.3)
P_X_between_0_and_08_given_Y_greater_03 <- function(repartitie_comuna) {
  total_prob <- sum(repartitie_comuna[repartitie_comuna > 0.3]) # suma totala a probabilităților unde Y > 0.3
  prob_X_between_0_and_08_and_Y_greater_03 <- sum(repartitie_comuna[repartitie_comuna > 0.3 & row(repartitie_comuna) <= 0.8])
  return(prob_X_between_0_and_08_and_Y_greater_03 / total_prob)
}

calculPX <- P_X_between_0_and_08_given_Y_greater_03(repartitie_completa)
print(calculPX)

# Calcul P(X>0.2 & Y<1.7)
P_X_greater_02_and_Y_less_17 <- function(repartitie_comuna) {
  # calculam probabilitatea comuna 
  return(sum(repartitie_comuna[row(repartitie_comuna) > 0.2 & col(repartitie_comuna) < 1.7]))
}

calculPX2 <-P_X_greater_02_and_Y_less_17(repartitie_completa)
print(calculPX2)

# calculul Cov(5X - 3Y)
Cov_5X_minus_3Y <- function(repartitie_comuna) {
  X <- row(repartitie_comuna)  
  Y <- col(repartitie_comuna) 
  
  # E[5X] si E[-3Y]
  E_5X <- sum(5 * X * repartitie_comuna)
  E_minus_3Y <- sum(-3 * Y * repartitie_comuna)
  
  # E[5X * -3Y]
  E_5X_minus_3Y <- sum(5 * X * -3 * Y * repartitie_comuna, na.rm = TRUE)
  
  # calculul covariantei folosind formula 
  cov_5X_minus_3Y <- E_5X_minus_3Y - (E_5X * E_minus_3Y)
  
  return(cov_5X_minus_3Y)
}

calculCov <- Cov_5X_minus_3Y(repartitie_completa)
print(calculCov)


#h.
fverind <- function(repartitie_comuna) {
  # calculam repartitile marginale pentru x si y
  repartitia_marginala_X <- apply(repartitie_comuna, 1, sum)
  repartitia_marginala_Y <- apply(repartitie_comuna, 2, sum)
  
  # verificam conditia de independenta pentru fiecare pereche (x,y)
  for (x in seq_along(repartitia_marginala_X)) {
    for (y in seq_along(repartitia_marginala_Y)) {
      if (repartitie_comuna[x, y] != repartitia_marginala_X[x] * repartitia_marginala_Y[y]) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

fvernecor <- function(repartitie_comuna) {
  # calculam valorile asteptate si covarianta
  X <- row(repartitie_comuna)
  Y <- col(repartitie_comuna)
  E_X <- sum(X * repartitie_comuna)
  E_Y <- sum(Y * repartitie_comuna)
  E_XY <- sum(X * Y * repartitie_comuna)
  
  cov_XY <- E_XY - E_X * E_Y
  
  #verificam daca valoarea absoluta a covariantei este atat de mica incat sa poata fi considerata 0
  return(abs(cov_XY) < .Machine$double.eps^0.5)
}


# apelam si afisam functile 
independente <- fverind(repartitie_completa)
necorelate <- fvernecor(repartitie_completa)

print(independente)
print(necorelate)



#instalam pachetele necesare pentru punctul 2

install.packages("plotly")
install.packages("shiny")
install.packages("ggplot2") # pachet pentru creare de grafice si vizualizari de date
install.packages("animate")
install.packages("gganimate")
library(shiny)
library(plotly)
library(ggplot2)
library(animate)
library(gganimate)

#2.

#b
library(shiny)
library(plotly)

# definim interfata 
ui <- fluidPage(
  titlePanel("Interpretarea Geometrica a Integralei Duble"),
  sidebarLayout(
    sidebarPanel(
      textInput("functie", "Introduceți functia f(x, y):", "sin(x) * cos(y)"),
      sliderInput("range_x", "Alegeti intervalul pentru x:", min = -10, max = 10, value = c(-3, 3)),
      sliderInput("range_y", "Alegeti intervalul pentru y:", min = -10, max = 10, value = c(-3, 3))
    ),
    mainPanel(
      plotlyOutput("plot3D")
    )
  )
)

# definim serverul
server <- function(input, output) {
  output$plot3D <- renderPlotly({
    f <- function(x, y) {
      eval(parse(text = input$functie))
    }
    # generarea valorilor pentru x si y in functie de intervalele specificate
    x_vals <- seq(input$range_x[1], input$range_x[2], length.out = 50)
    y_vals <- seq(input$range_y[1], input$range_y[2], length.out = 50)
    # calculul valorilor functiei pentru toate valorile (x,y)
    z_vals <- outer(x_vals, y_vals, f)
    
    plot_ly(x = ~x_vals, y = ~y_vals, z = ~z_vals, type = "surface")
  })
}

shinyApp(ui = ui, server = server)


#c)
library(shiny)

# definim interfata
ui <- fluidPage(
  titlePanel("Verificare Densitate de Probabilitate"),
  sidebarLayout(
    sidebarPanel(
      textInput("functie", "Introduceți funcția f(x, y):", "x^2 + y^2"),
      numericInput("lower_x", "Limita inferioara pentru x:", -1),
      numericInput("upper_x", "Limita superioara pentru x:", 1),
      numericInput("lower_y", "Limita inferioara pentru y:", -1),
      numericInput("upper_y", "Limita superioara pentru y:", 1),
      actionButton("verifica", "Verifica")
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

# definim serverul
server <- function(input, output) {
  observeEvent(input$verifica, {
    # functie care evalueaza expresia data cu gestionarea erorilor
    safeEval <- function(str) {
      tryCatch({
        eval(parse(text = str))
      }, error = function(e) NA)
    }
    # vectorizam functia pentru a o face aplicabila la vectori de intrare
    f <- Vectorize(function(x, y) safeEval(input$functie))
    # verificam daca functia este pozitiva pe tot domeniul
    is_positive <- all(outer(seq(input$lower_x, input$upper_x, length.out = 100), seq(input$lower_y, input$upper_y, length.out = 100), f) >= 0)
    # calculul integralei duble 
    integral <- tryCatch({
      integrate(function(x) {
        integrate(function(y) f(x, y), input$lower_y, input$upper_y)$value
      }, input$lower_x, input$upper_x)$value
    }, error = function(e) NA)
    
    # verificam daca functia respecta conditiile unei densitati de probabilitate
    if (!is.na(integral) && is_positive && abs(integral - 1) < 1e-5) {
      output$result <- renderText("Funcția este o densitate de probabilitate.")
    } else {
      output$result <- renderText("Funcția nu este o densitate de probabilitate.")
    }
  })
}

shinyApp(ui = ui, server = server)

#d)
library(shiny)
library(ggplot2)

# definim interfata
ui <- fluidPage(
  titlePanel("Variabile Aleatoare Continue"),
  sidebarLayout(
    sidebarPanel(
      textInput("f_expr", "Introduceti functia de densitate de probabilitate f(x, y):", "x^2 + y^2"),
      numericInput("x_min", "Limita minima pentru x:", 0),
      numericInput("x_max", "Limita maxima pentru x:", 1),
      numericInput("y_min", "Limita minima pentru y:", 0),
      numericInput("y_max", "Limita maxima pentru y:", 1),
      checkboxInput("bidimensional", "Variabila bidimensionala", TRUE),
      actionButton("gen_button", "Genereaza Variabila Aleatoare")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# definim serverul
server <- function(input, output) {
  observeEvent(input$gen_button, {
    f_expr <- parse(text = input$f_expr) # parseaza expresia functiei
    f <- function(x, y) { eval(f_expr) } # creeaza functia f
    
    output$plot <- renderPlot({
      if (input$bidimensional) {
        # genereaza o variabila aleatoare bidimensionala
        x_vals <- runif(1000, input$x_min, input$x_max)
        y_vals <- runif(1000, input$y_min, input$y_max)
        densitati <- mapply(f, x_vals, y_vals)
        df <- data.frame(x = x_vals, y = y_vals, densitate = densitati)
        
        # afiseaza un grafic pentru variabila bidimensionala
        ggplot(df, aes(x = x, y = y, color = densitate)) + geom_point() + theme_minimal()
      } else {
        # genereaza o variabila aleatoare unidimensionala
        x_vals <- seq(input$x_min, input$x_max, length.out = 1000)
        densitati <- sapply(x_vals, function(x) f(x, 0)) # presupunand ca y este intotdeauna 0 pentru cazul unidimensional
        df <- data.frame(x = x_vals, densitate = densitati)
        
        # afișeaza un grafic pentru variabila unidimensionala
        ggplot(df, aes(x = x, y = densitate)) + geom_line() + theme_minimal()
      }
    })
    
  })
}

shinyApp(ui = ui, server = server)


#e.

library(shiny)
library(ggplot2)
library(animate)

# definim interfata 
ui <- fluidPage(
  titlePanel("Variabile Aleatoare Continue Bidimensionale"),
  sidebarLayout(
    sidebarPanel(
      textInput("f_expr", "Introduceti functia de densitate de probabilitate f(x, y):", "x^2 + y^2"),
      numericInput("x_min", "Limita minima pentru x:", 0),
      numericInput("x_max", "Limita maxima pentru x:", 1),
      numericInput("y_min", "Limita minima pentru y:", 0),
      numericInput("y_max", "Limita maxima pentru y:", 1),
      actionButton("gen_button", "Genereaza Variabile Aleatoare"),
      downloadButton("download_data", "Descarca Date")
    ),
    mainPanel(
      plotOutput("plot"),
      plotOutput("marginal_x_plot"),
      plotOutput("marginal_y_plot")
    )
  )
)

# definim serverul
server <- function(input, output, session) {
  observeEvent(input$gen_button, {
    f_expr <- parse(text = input$f_expr) # parseaza expresia funcției
    f <- function(x, y) { eval(f_expr) } # creeaza functia f
    
    # genereaza o variabila aleatoare bidimensionala
    x_vals <- runif(1000, input$x_min, input$x_max)
    y_vals <- runif(1000, input$y_min, input$y_max)
    densitati <- mapply(f, x_vals, y_vals)
    df <- data.frame(x = x_vals, y = y_vals, densitate = densitati)
    
    # afiseaza un scatter plot pentru variabila bidimensionala
    p <- ggplot(df, aes(x = x, y = y, color = densitate)) +
      geom_point() +
      theme_minimal()
    
    # calculeaza densitatile marginale pentru x și y
    densitate_marginala_X <- function(x) {
      integrate(function(y) f(x, y), lower = input$y_min, upper = input$y_max)$value
    }
    densitate_marginala_Y <- function(y) {
      integrate(function(x) f(x, y), lower = input$x_min, upper = input$x_max)$value
    }
    
    # afișeaza densitatile marginale pe grafic
    marginal_x_plot <- ggplot(data.frame(x = seq(input$x_min, input$x_max, length.out = 100)), aes(x)) +
      stat_function(fun = densitate_marginala_X, geom = "line", color = "blue") +
      theme_minimal()
    marginal_y_plot <- ggplot(data.frame(y = seq(input$y_min, input$y_max, length.out = 100)), aes(y)) +
      stat_function(fun = densitate_marginala_Y, geom = "line", color = "red") +
      theme_minimal()
    
    # salveaza datele intr-un fisier CSV
    output$download_data <- downloadHandler(
      filename = function() { "variabile_aleatoare.csv" },
      content = function(file) {
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # afișeaza graficele și densitatile marginale
    output$plot <- renderPlot({ p })
    output$marginal_x_plot <- renderPlot({ marginal_x_plot })
    output$marginal_y_plot <- renderPlot({ marginal_y_plot })
  })
}

shinyApp(ui = ui, server = server)


#f)
library(shiny)
library(ggplot2)
library(animate)

# definim interfata
ui <- fluidPage(
  titlePanel("Reprezentarea Grafica a Densitatii si Functiei de Repartitie"),
  sidebarLayout(
    sidebarPanel(
      # selecteaza tipul de variabila aleatoare (unidimensionala sau bidimensionala)
      radioButtons("var_type", "Tip Variabila Aleatoare:", choices = c("Unidimensionala", "Bidimensionala")),
      
      # definim parametrii repartitiei
      sliderInput("param1", "Parametru 1:", min = 0, max = 10, value = 5),
      sliderInput("param2", "Parametru 2:", min = 0, max = 10, value = 5),
      
      # buton pentru generarea graficelor
      actionButton("gen_button", "Genereaza Grafice"),
      
      # optiune pentru animatie
      checkboxInput("animate_check", "Animatie", value = FALSE)
    ),
    mainPanel(
      plotOutput("density_plot"),
      plotOutput("cdf_plot")
    )
  )
)

# definim serverul
server <- function(input, output, session) {
  observeEvent(input$gen_button, {
    # functia pentru generarea densitatii și functiei de repartitie în functie de parametri
    generate_plots <- function(param1, param2) {
      
      # exemplu de generare de grafice pentru densitate si functie de repartitie
      density_plot <- ggplot(data = data.frame(x = seq(0, 10, length.out = 100)), aes(x)) +
        geom_density(aes(fill = "Densitate")) +
        theme_minimal()
      
      cdf_plot <- ggplot(data = data.frame(x = seq(0, 10, length.out = 100)), aes(x)) +
        stat_ecdf(aes(fill = "Functie de Repartitie")) +
        theme_minimal()
      
      return(list(density_plot, cdf_plot))
    }
    # parametrii selectati de utilizator
    param1_val <- input$param1
    param2_val <- input$param2
    
    # genereaza graficele
    plots <- generate_plots(param1_val, param2_val)
    
    output$density_plot <- renderPlot({ plots[[1]] })
    output$cdf_plot <- renderPlot({ plots[[2]] })
  })
  
  # animare
  observeEvent(input$animate_check, {
    if (input$animate_check) {
      # definim o secventa de valori pentru parametri
      param_values <- seq(0, 10, by = 0.1)
      
      # creeaza o lista de grafice pentru animatie
      animation_plots <- lapply(param_values, function(param) {
        generate_plots(param, param)  # puteti ajusta parametrii aici
      })
      
      # creeaza animatia și afiseaza-o în interfata Shiny
      animation <- gganimate::gganimate(plot_grid(plotlist = animation_plots))
      output$density_plot <- renderPlot({ animation })
      output$cdf_plot <- renderPlot({ NULL })  # sterge functia de repartitie pentru animatie
    }
  })
}


shinyApp(ui = ui, server = server)


#h

library(shiny)

# definim interfata
ui <- fluidPage(
  titlePanel("Calculul Mediei si Dispersiei pentru g(X)"),
  sidebarLayout(
    sidebarPanel(
      textInput("f_x", "Introduceti functia de densitate f(x):", "dnorm(x)"),
      textInput("g_x", "Introduceti functia g(x):", "x^2"),
      numericInput("lower_limit", "Limita inferioara:", -Inf),
      numericInput("upper_limit", "Limita superioara:", Inf),
      actionButton("calc_button", "Calculeaza")
    ),
    mainPanel(
      textOutput("media"),
      textOutput("dispersie")
    )
  )
)

# definim serverul
server <- function(input, output) {
  # rendereaza media 
  output$media <- renderText({
    # verifica daca limitele de integrare sunt specificate corect
    if (is.na(input$lower_limit) || is.na(input$upper_limit)) {
      return("Limitele de integrare nu sunt specificate corect.")
    }
    # parsarea si evaluarea functiilor
    f_x <- eval(parse(text = paste("function(x){", input$f_x, "}")))
    g_x_function <- eval(parse(text = paste("function(x){", input$g_x, "}")))
    
    # calculul mediei 
    media <- integrate(function(x) g_x_function(x) * f_x(x), input$lower_limit, input$upper_limit)$value
    return(paste("Media: ", media))
  })
  
  output$dispersie <- renderText({
    if (is.na(input$lower_limit) || is.na(input$upper_limit)) {
      return("Limitele de integrare nu sunt specificate corect.")
    }
    
    f_x <- eval(parse(text = paste("function(x){", input$f_x, "}")))
    g_x_function <- eval(parse(text = paste("function(x){", input$g_x, "}")))
    
    # calcul dispersiei
    media <- integrate(function(x) g_x_function(x) * f_x(x), input$lower_limit, input$upper_limit)$value
    dispersie <- integrate(function(x) (g_x_function(x) - media)^2 * f_x(x), input$lower_limit, input$upper_limit)$value
    return(paste("Dispersia: ", dispersie))
  })
}

shinyApp(ui = ui, server = server)


#i
library(shiny)

# definim interfata
ui <- fluidPage(
  titlePanel("Calculul Probabilitatilor pentru Variabile Aleatoare"),
  sidebarLayout(
    sidebarPanel(
      textInput("f_densitate_input", "Funcția de densitate f(x) (sau f(x, y) pentru bidimensional):", "dnorm(x)"),
      numericInput("interval_inf", "Limita inferioara a intervalului:", -Inf),
      numericInput("interval_sup", "Limita superioara a intervalului:", Inf),
      checkboxInput("bidimensional_check", "Variabila aleatoare bidimensionala", FALSE),
      actionButton("calc_button", "Calculeaza Probabilitatea")
    ),
    mainPanel(
      textOutput("probabilitate_output")
    )
  )
)

# functie pentru calculul probabilitatii pentru variabile aleatoare
P <- function(f_densitate, interval_inf, interval_sup, bidimensional = FALSE, f_densitate_y = NULL) {
  if (is.na(interval_inf) || is.na(interval_sup)) {
    return("Intervalul de integrare nu este valid. Te rugam sa introduci limite valide.")
  }
  
  if (!is.finite(interval_inf) || !is.finite(interval_sup)) {
    return("Intervalul de integrare trebuie sa fie o valoare finita. Te rugam sa introduci limite valide.")
  }
  
  if (!bidimensional) {
    # pentru variabile aleatoare unidimensionale
    probabilitate <- integrate(f_densitate, interval_inf, interval_sup)$value
  } else {
    # pentru variabile aleatoare bidimensionale
    probabilitate <- integrate(function(x) {
      integrate(function(y) f_densitate(x, y), interval_inf, interval_sup)$value
    }, interval_inf, interval_sup)$value
  }
  return(probabilitate)
}

# definim serverul
server <- function(input, output) {
  output$probabilitate_output <- renderText({
    if (input$bidimensional_check) {
      # pentru cazul bidimensional
      f_densitate <- try(eval(parse(text = input$f_densitate_input)), silent = TRUE)
      if (inherits(f_densitate, "function")) {
        probabilitate <- P(f_densitate, input$interval_inf, input$interval_sup, bidimensional = TRUE)
      } else {
        probabilitate <- "Expresia introdusa nu este o functie valida."
      }
    } else {
      # pentru cazul unidimensional
      f_densitate <- try(eval(parse(text = paste("function(x){", input$f_densitate_input, "}"))), silent = TRUE)
      if (inherits(f_densitate, "function")) {
        probabilitate <- P(f_densitate, input$interval_inf, input$interval_sup)
      } else {
        probabilitate <- "Expresia introdusa nu este o functie valida."
      }
    }
    paste("Probabilitatea calculata: ", probabilitate)
  })
}
# ruleaza aplicatia
shinyApp(ui = ui, server = server)

