library(shiny)
library(gamlss)
library(extraDistr)


shinyServer(function(input, output, session) {
  my.data <- reactive({
    if (input$input_type == "csv") {
      MyData <<- input$avalidatedCustomFile
      if (is.null(MyData))
        return(NULL)
      return(data.frame(read.csv(
        MyData$datapath, sep = input$separator, header = TRUE
      )))
    } else {
      switch(
        input$terserah_samean,
        "CD4" = CD4,
        "LGAclaims" = LGAclaims,
        "alveolar" = alveolar,
        "computer" = computer,
        "iris" = iris,
        "rock" = rock,
        "mtcars" = mtcars,
        "orange" = Orange,
        "trees" = trees
      )
    }
  })
  
  output$hist.DashBinomial <- renderPlot({
    N <- input$input.n_binomial
    u <- input$input.u_binomial
    v <- input$input.v_binomial
    p <- u / (u + v)
    x <- 0:N
    y <- dbinom(x, N, p)
    barplot(t(matrix(c(y), ncol = 2)), beside = TRUE, col = c("blue"))
    legend("topright",
           c("Binomial"),
           col = c("blue"),
           pch = 15)
  })
  
  
  output$hist.DashBeta <- renderPlot({
    x <- seq(0, 1, by = 0.02)
    y <- dbeta(x, input$input.s1_beta, input$input.s2_beta)
    barplot(t(matrix(c(y))), beside = TRUE, col = c("blue"))
    legend("topright",
           c("Distribusi Beta"),
           col = c("blue"),
           pch = 15)
  })
  
  
  output$data_table.sum_Ringkasan <- renderPrint({
    set.seed(100)
    #summary(my.data())
    print(input$avalidatedCustomFile)
  })
  
  output$output.var_dash_beta <- renderPrint({
    set.seed(100)
    x <- seq(0, 1, by = 0.02)
    y <- dbeta(x, input$input.s1_beta, input$input.s2_beta)
    round(var(y), 6)
  })
  output$output.sd_dash_beta <- renderPrint({
    set.seed(100)
    x <- seq(0, 1, by = 0.02)
    y <- dbeta(x, input$input.s1_beta, input$input.s2_beta)
    round(sd(y), 6)
  })
  
  
  output$output.var_dash_binomial <- renderPrint({
    set.seed(100)
    N <- input$input.n_binomial
    u <- input$input.u_binomial
    v <- input$input.v_binomial
    p <- u / (u + v)
    x <- 0:N
    y <- dbinom(x, N, p)
    round(var(y), 6)
  })
  
  output$output.sd_dash_binomial <- renderPrint({
    set.seed(100)
    N <- input$input.n_binomial
    u <- input$input.u_binomial
    v <- input$input.v_binomial
    p <- u / (u + v)
    x <- 0:N
    y <- dbinom(x, N, p)
    round(sd(y), 6)
  })
  
  output$output.sum_dash_binomial <- renderPrint({
    set.seed(100)
    N <- input$input.n_binomial
    u <- input$input.u_binomial
    v <- input$input.v_binomial
    p <- u / (u + v)
    x <- 0:N
    y <- dbinom(x, N, p)
    summary(y)
  })
  
  # Untuk Grafik Normal Baku pada Dashboard
  # output$hist.dash_normal_baku <- renderPlot({
  #   set.seed(100)
  #   n <- input$input.n_dash_normal_baku
  #   min_x <- input$input.min_x_dash_normal_baku
  #   max_x <- input$input.max_x_dash_normal_baku
  #   data <- rnorm(n)
  #   hist(
  #     data,
  #     xlim = c(min_x, max_x),
  #     freq = F,
  #     main = "Normal Baku N(0,1)",
  #     col = 'lightgreen'
  #   )
  #   lines(density(data), col='red')
  # })
  #untuk grafik beta-binomial
  output$hist.Dashbeta_binomial <- renderPlot({
    N <- input$input.n_beta_binomial
    u <- input$input.u_beta_binomial
    v <- input$input.v_beta_binomial
    p <- u / (u + v)
    x <- 0:N
    y <- dbinom(x, N, p)
    yy <- dbbinom(x,
                  N,
                  alpha = u,
                  beta = v,
                  log = FALSE)
    barplot(t(matrix(c(y, yy), ncol = 2)),
            beside = TRUE,
            col = c("blue", "red"))
    legend(
      "topright",
      c("Distribusi Binomial", "Distribusi Beta-Binomial"),
      col = c("blue", "red"),
      pch = 15
    )
  })
  #set summary beta binomialnya
  output$output.sum_dash_beta_binomial <- renderPrint({
    set.seed(561662)
    N <- input$input.n_beta_binomial
    u <- input$input.u_beta_binomial
    v <- input$input.v_beta_binomial
    rr <- rbbinom(1000, N, u, v)
    summary(rr)
  })
  
  output$output.sum_dash_binomial_bawah <- renderPrint({
    set.seed(561662)
    u <- input$input.u_beta_binomial
    v <- input$input.v_beta_binomial
    N <- input$input.n_beta_binomial
    p <- u / (u + v)
    r <- rbinom(1000, N, p)
    summary(r)
  })
  
  #set varians beta binomialnya
  output$output.var_dash_beta_binomial <- renderPrint({
    set.seed(561662)
    u <- input$input.u_beta_binomial
    v <- input$input.v_beta_binomial
    N <- input$input.n_beta_binomial
    rr <- rbbinom(1000, N, u, v)
    round(var(rr), 4)
  })
  #set varians beta binomialnya
  output$output.var_dash_binomial_bawah <- renderPrint({
    set.seed(561662)
    u <- input$input.u_beta_binomial
    v <- input$input.v_beta_binomial
    p <- u / (u + v)
    N <- input$input.n_beta_binomial
    r <- rbinom(1000, N, p)
    round(var(r), 4)
  })
  #set SD beta binomial dkk
  output$output.sd_dash_beta_binomial <- renderPrint({
    set.seed(561662)
    u <- input$input.u_beta_binomial
    v <- input$input.v_beta_binomial
    N <- input$input.n_beta_binomial
    rr <- rbbinom(1000, N, u, v)
    round(sd(rr), 4)
  })
  output$output.sd_dash_binomial_bawah <- renderPrint({
    set.seed(561662)
    u <- input$input.u_beta_binomial
    v <- input$input.v_beta_binomial
    p <- u / (u + v)
    N <- input$input.n_beta_binomial
    r <- rbinom(1000, N, p)
    round(sd(r), 4)
  })
  
  # Untuk Grafik Normal pada Dashboard
  output$hist.dash_normal <- renderPlot({
    set.seed(100)
    n <- input$input.n_dash_normal
    mu <- input$input.mu_dash_normal
    sigma <- input$input.sigma_dash_normal
    min_x <- input$input.min_x_dash_normal
    max_x <- input$input.max_x_dash_normal
    data <- rnorm(n, mu, sigma)
    hist(
      data,
      xlim = c(min_x, max_x),
      freq = F,
      main = "Normal N(mu,sigma)",
      col = 'lightgreen'
    )
    lines(density(data), col = 'red')
  })
  
  output$output.sum_dash_normal <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_normal
    mu <- input$input.mu_dash_normal
    sigma <- input$input.sigma_dash_normal
    data <- rnorm(n, mu, sigma)
    summary(data)
  })
  
  output$output.var_dash_normal <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_normal
    mu <- input$input.mu_dash_normal
    sigma <- input$input.sigma_dash_normal
    data <- rnorm(n, mu, sigma)
    round(var(data), 4)
  })
  
  # Untuk Grafik Normal-Normal pada Dashboard
  output$hist.dash_normal_normal <- renderPlot({
    set.seed(100)
    n <- input$input.n_dash_normal_normal
    
    mu_0 <- input$input.mu_0_dash_normal_normal
    sigma_0 <- input$input.sigma_0_dash_normal_normal
    mu <- rnorm(n, mu_0, sigma_0)
    
    min_x <- input$input.min_x_dash_normal_normal
    max_x <- input$input.max_x_dash_normal_normal
    
    sigma <- input$input.sigma_dash_normal_normal
    
    data <- rnorm(n, mu, sigma)
    hist(
      data,
      xlim = c(min_x, max_x),
      freq = F,
      main = "Normal-Normal N(mu,sigma), dengan mu ~ N(mu_0, sigma_0)",
      col = 'lightgreen'
    )
    lines(density(data), col = 'red')
  })
  
  output$output.sum_dash_normal_normal <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_normal_normal
    
    mu_0 <- input$input.mu_0_dash_normal_normal
    sigma_0 <- input$input.sigma_0_dash_normal_normal
    mu <- rnorm(n, mu_0, sigma_0)
    
    sigma <- input$input.sigma_dash_normal_normal
    
    data <- rnorm(n, mu, sigma)
    summary(data)
  })
  
  output$output.var_dash_normal_normal <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_normal_normal
    
    mu_0 <- input$input.mu_0_dash_normal_normal
    sigma_0 <- input$input.sigma_0_dash_normal_normal
    mu <- rnorm(n, mu_0, sigma_0)
    
    sigma <- input$input.sigma_dash_normal_normal
    
    data <- rnorm(n, mu, sigma)
    round(var(data), 4)
  })
  
  # Perbandingan 3 Distribusi
  output$hist.dash_banding_prior <- renderPlot({
    set.seed(100)
    n <- input$input.n_dash_banding
    
    mu_0 <- input$input.mu_0_dash_banding
    sigma_0 <- input$input.sigma_0_dash_banding
    
    min_x <- input$input.min_x_dash_banding
    max_x <- input$input.max_x_dash_banding
    
    data <- rnorm(n, mu_0, sigma_0)
    hist(
      data,
      xlim = c(min_x, max_x),
      freq = F,
      main = "Normal Prior N(mu_0, sigma_0)",
      col = 'lightgreen'
    )
    lines(density(data), col = 'red')
  })
  
  output$output.sum_dash_banding_prior <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_banding
    mu <- input$input.mu_0_dash_banding
    sigma <- input$input.sigma_0_dash_banding
    data <- rnorm(n, mu, sigma)
    summary(data)
  })
  
  output$output.var_dash_banding_prior <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_banding
    mu <- input$input.mu_0_dash_banding
    sigma <- input$input.sigma_0_dash_banding
    data <- rnorm(n, mu, sigma)
    round(var(data), 4)
  })
  
  output$hist.dash_banding_normal <- renderPlot({
    set.seed(100)
    n <- input$input.n_dash_banding
    
    mu <- input$input.mu_dash_banding
    sigma <- input$input.sigma_dash_banding
    
    min_x <- input$input.min_x_dash_banding
    max_x <- input$input.max_x_dash_banding
    
    data <- rnorm(n, mu, sigma)
    hist(
      data,
      xlim = c(min_x, max_x),
      freq = F,
      main = "Normal N(mu, sigma)",
      col = 'lightgreen'
    )
    lines(density(data), col = 'red')
  })
  
  output$output.sum_dash_banding_normal <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_banding
    mu <- input$input.mu_dash_banding
    sigma <- input$input.sigma_dash_banding
    data <- rnorm(n, mu, sigma)
    summary(data)
  })
  
  output$output.var_dash_banding_normal <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_banding
    mu <- input$input.mu_dash_banding
    sigma <- input$input.sigma_dash_banding
    data <- rnorm(n, mu, sigma)
    round(var(data), 4)
  })
  
  output$hist.dash_banding_normal_normal <- renderPlot({
    set.seed(100)
    n <- input$input.n_dash_banding
    
    mu_0 <- input$input.mu_0_dash_banding
    sigma_0 <- input$input.sigma_0_dash_banding
    mu <- rnorm(n, mu_0, sigma_0)
    
    min_x <- input$input.min_x_dash_banding
    max_x <- input$input.max_x_dash_banding
    
    sigma <- input$input.sigma_dash_banding
    
    data <- rnorm(n, mu, sigma)
    hist(
      data,
      xlim = c(min_x, max_x),
      freq = F,
      main = "Normal-Normal N(mu,sigma), \ndengan mu ~ N(mu_0, sigma_0)",
      col = 'lightgreen'
    )
    lines(density(data), col = 'red')
  })
  
  output$output.sum_dash_banding_normal_normal <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_banding
    
    mu_0 <- input$input.mu_0_dash_banding
    sigma_0 <- input$input.sigma_0_dash_banding
    mu <- rnorm(n, mu_0, sigma_0)
    
    sigma <- input$input.sigma_dash_banding
    
    data <- rnorm(n, mu, sigma)
    summary(data)
  })
  
  output$output.var_dash_banding_normal_normal <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_banding
    
    mu_0 <- input$input.mu_0_dash_banding
    sigma_0 <- input$input.sigma_0_dash_banding
    mu <- rnorm(n, mu_0, sigma_0)
    
    sigma <- input$input.sigma_dash_banding
    
    data <- rnorm(n, mu, sigma)
    round(var(data), 4)
  })
  
  # Untuk Distribusi NOF
  output$hist.dash_nof <- renderPlot({
    set.seed(100)
    n <- input$input.n_dash_nof
    mu <- input$input.mu_dash_nof
    sigma <- input$input.sigma_dash_nof
    v <- input$input.v_dash_nof
    min_x <- input$input.min_x_dash_nof
    max_x <- input$input.max_x_dash_nof
    
    data <- rNOF(n, mu, sigma, v)
    
    hist(
      data,
      xlim = c(min_x, max_x),
      freq = F,
      main = "Normal Family NOF(mu,sigma,nu)",
      col = 'lightgreen'
    )
    lines(density(data), col = 'red')
  })
  
  output$output.sum_dash_nof <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_nof
    mu <- input$input.mu_dash_nof
    sigma <- input$input.sigma_dash_nof
    v <- input$input.v_dash_nof
    data <- rNOF(n, mu, sigma, v)
    summary(data)
  })
  
  output$output.var_dash_nof <- renderPrint({
    set.seed(100)
    n <- input$input.n_dash_nof
    mu <- input$input.mu_dash_nof
    sigma <- input$input.sigma_dash_nof
    v <- input$input.v_dash_nof
    data <- rNOF(n, mu, sigma, v)
    round(var(data), 4)
  })
})