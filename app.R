# Load packages ----------------------------------------------------------------
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Define UI --------------------------------------------------------------------
ui <- tagList(
  includeCSS(path = "www/css/styles.css"), 
  tags$div(
    tags$div(
      class = "app_title", 
      titlePanel(
        title = "Centrala Gränsvärdessatsen & Samplingfördelning - Kriminologiska Institutionen", 
        windowTitle = "Centrala Gränsvärdessatsen"
      ),
    ),
    br(),
    fluidPage(
      # Title ----
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            # Select distribution ----
            radioButtons("dist", "Populationsfördelning:",
                         c("Normal" = "rnorm",
                           "Positiv snedfördelning" = "rlnorm",
                           "Negativ snedfördelning" = "rbeta"),
                         # "Uniform" = "runif"), Uniform distribution is disabled. Remove comment to restore
                         selected = "rnorm"),
            
            # Distribution parameters / features ----
            uiOutput("mu"),
            uiOutput("sd"),
            uiOutput("minmax"),
            uiOutput("skew"),
            
            # Select sample size ----
            sliderInput("n",
                        "Stickprovsstorlek:", 
                        value = 30,
                        min = 2,
                        max = 500),
            
            # Number of samples ----
            sliderInput("k",
                        "Antal stickprov:",
                        value = 200,
                        min = 10,
                        max = 1000)
          ),
        ),
        
        mainPanel(
          tabsetPanel(
            type = "tabs",
            # First tab ----
            tabPanel(
              title = "Populationsfördelning",
              fluidRow(
                column(width = 12,
                       # Population plot ----
                       plotOutput("pop.dist", height = "500px"),
                       textOutput("pop.descr"),
                       br()
                )
              ),
            ),
            # Second tab ----
            tabPanel(
              title = "Stickprov",
              # Sample plots ----
              br(),
              plotOutput("sample.dist", height = "600px"),
              #  Number of samples text ----
              div(textOutput("num.samples")),
              br()
            ),
            # Third tab ----
            tabPanel(
              title = "Stickprovsfördelning",
              
              fluidRow(
                column(width = 7,
                       br(), br(),
                       # CLT description ----
                       div(textOutput("CLT.descr"), align = "justify")),
                column(width = 5,
                       br(),
                       # Population plot ----
                       plotOutput("pop.dist.two", width = "85%", height = "200px"))
              ),
              
              fluidRow(
                column(width = 12,
                       br(),
                       # Sampling plot ----
                       plotOutput("sampling.dist"),
                       # Sampling description ----
                       div(textOutput("sampling.descr", inline = TRUE), align = "center"))
              )
            )
          )
        )
      )
    )
  ),
  tags$footer(
    tags$div(
      
      class = "footer_container", 
      
      includeHTML(path = "www/html/footer.html")
    )
  )
)

# Define server function --------------------------------------------
seed <- as.numeric(Sys.time())

server <- function(input, output, session) {
  
  # Mean slider for Normal distribution ----
  output$mu = renderUI(
    {
      if (input$dist == "rnorm")
      {
        sliderInput("mu",
                    "Medelvärde:",
                    value = 0,
                    min = -40,
                    max = 50)
      }
    })
  
  # SD slider for Normal distribution ----
  output$sd = renderUI(
    {
      if (input$dist == "rnorm")
      {
        sliderInput("sd",
                    "Standardavvikelse:",
                    value = 20,
                    min = 1,
                    max = 30)
      }
    })
  
  # Minmax slider for Uniform distribution ----
  output$minmax = renderUI(
    {
      
      if (input$dist == "runif")
      {
        sliderInput("minmax",
                    "Nedre och övre gränser",
                    value = c(5, 15),
                    min = 0,
                    max = 20)
      }
    })
  
  # Making sure range for uniform distribution != 0 ----
  observeEvent(input$minmax, {
    
    req(input$minmax)
    
    if (input$minmax[1] == input$minmax[2]){
      if (input$minmax[1] == 0){
        updateSliderInput(session, "minmax", value = c(0, 1))
      } else if (input$minmax[2] == 20){
        updateSliderInput(session, "minmax", value = c(19, 20))
      } else {
        updateSliderInput(session, "minmax", value = c(input$minmax[2], input$minmax[2] + 1))
      }
    }
  })
  
  # skew slider for rlnorm and rbeta ----
  output$skew = renderUI(
    {
      
      if (input$dist == "rlnorm" | input$dist == "rbeta"){
        selectInput(inputId = "skew",
                    label = "Skew:",
                    choices = c("Låg snedfördelning" = "low",
                                "Medium snedfördelning" = "med",
                                "Hög snedfördelning" = "high"),
                    selected = "low")
      }
    })
  
  # generating random samples ----
  rand_draw <- function(dist, n, mu, sd, min, max, skew){
    
    vals = NULL
    
    if (dist == "rbeta"){
      req(skew)
      if (skew == "low"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=2))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=1.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=1)) 
      }
    }
    
    else if (dist == "rnorm"){
      req(mu, sd)
      vals = do.call(dist, list(n=n, mean=mu, sd=sd))
    }
    
    else if (dist == "rlnorm"){
      req(skew)
      if (skew == "low"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=.25))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=1))
      }
    }
    
    else if (dist == "runif"){
      req(min, max)
      vals = do.call(dist, list(n=n, min=min, max=max))
    }
    return(vals)
  }
  
  rep_rand_draw = repeatable(rand_draw)
  
  # Defining some reactive variables to use later ----
  parent = reactive({
    
    n_sample = 1e5
    
    return(rep_rand_draw(input$dist, n_sample, input$mu, input$sd,
                         input$minmax[1], input$minmax[2], input$skew))
  })
  
  samples = reactive({
    
    pop = parent()
    n = input$n
    k = input$k
    
    return(replicate(k, sample(pop, n, replace=TRUE)))
  })
  
  u_min = reactive({
    req(input$minmax)
    return(input$minmax[1])
  })
  
  u_max = reactive({
    req(input$minmax)
    return(input$minmax[2])
  })
  
  # plot 1 a) ----
  
  output$pop.dist = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "Populationsfördelning: Normal",
                      rlnorm = "Populationsfördelning: Positiv snedfördelning",
                      rbeta = "Populationsfördelning: Negativ snedfördelning",
                      runif = "Populationsfördelning: Uniform")
    
    pop = parent()
    
    m_pop =  round(mean(pop), 2)
    sd_pop = round(sd(pop), 2)
    
    pop = tibble(samples = pop)
    pdens = density(pop$samples)
    
    x_range = max(pop$samples) - min(pop$samples)
    y_pos = max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm"){
      
      req(input$mu)
      mu = input$mu
      
      x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 20,
                     max(100, max(pop$samples)) - 20)
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white", fill = "#595959") +
        stat_density(geom="line", color = "#595959", size = 1) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, x = "", y = "") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("medelvärde", "=", bquote(.(m_pop)),
                               "\n", "standardavvikelse", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_light(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      
    } else if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        "  " # this is to temporarily prevent graph from displaying while 
        # observeEvent is fixing the range.
      } else {
        
        x_pos = max(pop$samples) - 0.1*x_range
        
        ggplot(data = pop, aes(x = samples, y = after_stat(density))) +
          geom_histogram(bins = 45, color = "white", fill = "#595959") +
          stat_density(geom = "line", color = "#595959", size = 1) +
          scale_y_continuous(expand = expansion(mult = c(0, .3))) +
          labs(title = distname, x = "", y = "") +
          annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                   label = paste("medelvärde", "=", bquote(.(m_pop)),
                                 "\n", "standardavvikelse", "=", bquote(.(sd_pop))),
                   color = "black", size = 5) +
          theme_light(base_size = 19) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank())}
      
    } else if (input$dist == "rlnorm"){
      
      x_pos = max(pop$samples) - 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white", fill = "#595959") +
        stat_density(geom = "line", color = "#595959", size = 1) +
        labs(title = distname, x = "", y = "") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("medelvärde", "=", bquote(.(m_pop)), 
                               "\n", "standardavvikelse", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_light(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      
    } else if (input$dist == "rbeta"){
      
      x_pos = min(pop$samples) + 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white", fill = "#595959") +
        stat_density(geom = "line", color = "#595959", size = 1) +
        labs(title = distname, x = "", y = "") +
        annotate("text", x = x_pos, y = y_pos, 
                 label = paste("medelvärde", "=", bquote(.(m_pop)), 
                               "\n", "standardavvikelse", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_light(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      
    }
  })
  
  # plot 1 b) ----
  # this is the population plot in the third tab. it is plot 1 a) with a few
  # changes to make it fit the smaller pace in the tab. (apparently not possible to use the
  # same output id in different tabs.)
  
  output$pop.dist.two = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "Populationsfördelning: Normal",
                      rlnorm = "Populationsfördelning: Positiv snedfördelning",
                      rbeta = "Populationsfördelning: Negativ snedfördelning",
                      runif = "Populationsfördelning: Uniform")
    
    pop = parent()
    
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    
    pop = tibble(samples = pop)
    pdens = density(pop$samples)
    
    x_range = max(pop$samples) - min(pop$samples)
    y_pos = max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm"){
      
      req(input$mu)
      mu = input$mu
      
      x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 40,
                     max(100, max(pop$samples)) - 40)
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white", fill = "#595959") +
        stat_density(geom="line", color = "#595959", size = 1) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("medelvärde", "=", bquote(.(m_pop)),
                               "\n", "standardavvikelse", "=", bquote(.(sd_pop))),
                 color = "black", size = 3) +
        theme_light(base_size = 10) +
        ylab("") +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      
    } else if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        " "
      } else {
        
        x_pos = max(pop$samples) - 0.15*x_range
        
        ggplot(data = pop, aes(x = samples, y = after_stat(density))) +
          geom_histogram(bins = 45, color = "white", fill = "#595959") +
          stat_density(geom = "line", color = "#595959", size = 1) +
          scale_y_continuous(expand = expansion(mult = c(0, .3))) +
          labs(title = distname, x = "x") +
          annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                   label = paste("medelvärde", "=", bquote(.(m_pop)),
                                 "\n", "standardavvikelse", "=", bquote(.(sd_pop))),
                   color = "black", size = 3) +
          theme_light(base_size = 10) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank())}
      
    } else if (input$dist == "rlnorm"){
      
      x_pos = max(pop$samples) - 0.15*x_range
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white", fill = "#595959") +
        stat_density(geom = "line", color = "#595959", size = 1) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("medelvärde", "=", bquote(.(m_pop)), 
                               "\n", "standardavvikelse", "=", bquote(.(sd_pop))),
                 color = "black", size = 3) +
        theme_light(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      
    } else if (input$dist == "rbeta"){
      
      x_pos = min(pop$samples) + 0.15*x_range
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white", fill = "#595959") +
        stat_density(geom = "line", color = "#595959", size = 1) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos, 
                 label = paste("medelvärde", "=", bquote(.(m_pop)), 
                               "\n", "standardavvikelse", "=", bquote(.(sd_pop))),
                 color = "black", size = 3) +
        theme_light(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      
    }
  })
  
  # plot 2 ----
  output$sample.dist = renderPlot({
    
    y = samples()
    x = samples() %>% as_tibble()
    
    plots = list(rep(NA, 8))
    
    for (i in 1:8){
      
      mean = round(mean(y[,i]), 2)
      sd = round(sd(y[,i]), 2)
      
      x_range = max(y[,i]) - min(y[,i])
      pdens = density(y[,i])
      
      x_pos = ifelse(input$dist == "rbeta", min(y[,i]) + 0.2*x_range, 
                     max(y[,i]) - 0.2*x_range)
      
      plots[[i]] = ggplot(x, aes_string(x = paste0("V", i))) +
        geom_dotplot(alpha = 0.8, dotsize = 0.7) +
        labs(title = paste("Stickprov", i), x = "", y = "") +
        theme_light(base_size = 13) +
        annotate("text", x = x_pos, y = 1.8,
                 label = paste("medelvärde", "=", bquote(.(mean)),
                               "\n", "standardavvikelse", "=", bquote(.(sd))),
                 color = "black", size = 3) +
        scale_y_continuous(limits = c(0,2), breaks = NULL) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
    }
    
    grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
                 plots[[6]], plots[[7]], plots[[8]], ncol = 4)
  })
  
  
  
  # text for sample plots ----
  output$num.samples = renderText({
    
    k = input$k
    paste0("... och så fortsätter det till stickprov ",k,". Klicka på fliken Stickprovsfördelning för at se hur det skulle se ut om vi tog ut
           medelvärdena från samtliga ", k, " stickprov och la de i en figur och beräknade medelvärdet för medelvärdena samt spridningen som medelvärdena uppvisar.")
    
  })
  
  # plot 3 ----
  output$sampling.dist = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "normalfördelad population",
                      rlnorm  = "positiv snedfördelad population",
                      rbeta = "negativ snedfördeladpopulation",
                      runif = "uniform population")
    
    n = input$n
    k = input$k
    
    pop = parent()
    
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    
    ndist = tibble(means = colMeans(samples()))
    
    m_samp =  round(mean(ndist$means),2)
    sd_samp = round(sd(ndist$means),2)
    
    ndens = density(ndist$means)
    nhist = hist(ndist$means, plot=FALSE)
    
    x_range = max(ndist$means) - min(ndist$means)
    
    y_pos = max(ndens$y) - 0.1*max(ndens$y)
    x_pos = ifelse(m_samp > 0, min(ndist$means) + 0.1*x_range, 
                   max(ndist$means) - 0.1*x_range)
    
    p = ggplot(data = ndist, aes(x = means, y = after_stat(density))) +
      geom_histogram(bins = 20, color = "white", fill = "#ea9999") +
      stat_density(geom = "line", color = "#ea9999", size = 1) +
      labs(title = paste("Stickprovsfördelning"),
           x = "Stickprovsmedelvärde",
           y = "") +
      annotate("text", x = x_pos, y = y_pos,
               label = paste("medelvärdet av samtliga medelvärden", "=", bquote(.(m_samp)),
                             "\n", "standardfelet", "=", bquote(.(sd_samp))),
               color = "black", size = 5) +
      theme_light(base_size = 19) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
    if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        " "
      } else {
        p
      }
    } else {
      p
    }
  })
  
  # description for population and sampling distribution plot ----
  # description for population ----
  output$pop.descr = renderText({
    
    pop = parent()
    
    distname = switch(input$dist,
                      rnorm = "normalfördelad",
                      rlnorm  = "positivt snedfördelad",
                      rbeta = "negativt snedfördelad",
                      runif = "uniform")
    
    m_pop =  round(mean(pop),2)
    s_pop = round(sd(pop),2)
    
    k = input$k
    n = input$n
    
    se=round(s_pop/sqrt(n),2)
    
    paste0("Ovanstående är en population vars fördelning är ", distname,  " med ett medelvärde på ", m_pop, " och en standardavvikelse
          på ", s_pop, ". Klicka på fliken Stickprov för att se hur fördelningen för enskilda stickprov skulle se ut om vi drog ", k, 
          " stickprov från ovanstående population där varje enskilda stickprov innehåller ", n, " observationer.")
  })
  
  output$sampling.descr = renderText({
    
    distname = switch(input$dist,
                      rnorm = "normalfördelad population",
                      rlnorm  = "positivt snedfördelad population",
                      rbeta = "negativt snedfördelad population",
                      runif = "uniform population")
    
    k = input$k
    n = input$n
    paste("Fördelningen av medelvärden från ", k, "slumpmässiga stickprov,
          som varje enskilt innehåller", n, " observationer
          från en", distname)
  })
  
  # description for CLT ----
  output$CLT.descr = renderText({
    
    pop = parent()
    m_pop =  round(mean(pop),2)
    s_pop = round(sd(pop),2)
    
    n = input$n
    se=round(s_pop/sqrt(n),2)
    
    paste0("Centrala gränsvärdessatsen säger att om vi upprepade gånger beräknar medelvärdet 
           av ett stort antal antal slumpmässiga observationer så kommer fördelningen av dessa medelvärden (den så kallade samplingfördelningen)
           att uppvisa en normalfördelning - nästa oavsett hur populationens ursprungliga fördelning ser ut. 
           Samplingfördelningens medelvärde bör vara nära populationens medelvärde (", m_pop, ") och standardfelet bör vara nära populationens 
           standardavvikelse delat med roten ur stickprovsstorleken (", s_pop, " / roten ur ",n, " = ", se,"). Standardfelet är likt standardavvikelsen
           en beskrivning av spridningen. Standardfel beskriver dock den genomsnittliga spridningen av medelvärdena. 
           Här nedan är en figur över vår stickprovsfördelning. Uppe till höger är populationsfördelningen.")
  })
}
# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)