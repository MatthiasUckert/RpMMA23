library(tidyverse)
library(patchwork)
library(shiny)
library(shinyWidgets)
options(shiny.autoreload = TRUE)
source("../1_code/functions/f-02_business_case.R", encoding = "UTF-8")
source("../1_code/functions/v-02_business_case_default_values.R")
source("../1_code/functions/f-02_shiny_business_case.R", encoding = "UTF-8")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      slider_sidebar()
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Input Functions",
          br(),
          radioGroupButtons(
            inputId = "in_fun", 
            choices = c("Variable Cost", "Fixed Cost", "Price")
          ),
          plotOutput(outputId = "pO_input")
        ),
        tabPanel(
          title = "Contribution Margin",
          h3("Volume Scenarions"),
          splitLayout(
            numericInput(inputId = "v1", label = NULL, value = 2500),
            numericInput(inputId = "v2", label = NULL, value = 5000),
            numericInput(inputId = "v3", label = NULL, value = 7500),
            numericInput(inputId = "v4", label = NULL, value = 10000)
          ),
          htmlOutput(outputId = "cm_table")
        ),
        tabPanel(
          title = "Sensitivity Analysis",
          br(),
          radioGroupButtons(
            inputId = "sens", 
            choices = c("Volume", "Price", "Variable Cost", "Fixed Cost", "Discount")
            ),
          plotOutput(outputId = "pO_sensitivity"),
        ),
        tabPanel(
          title = "Monte Carlo",
          splitLayout(
            numericInput(inputId = "vsd", label = "SD (Volume)", value = .def_vsd),
            numericInput(inputId = "psd", label = "SD (Price)", value = .def_psd),
            numericInput(inputId = "cvsd", label = "SD (Variable Cost)", value = .def_cvsd),
            numericInput(inputId = "cfsd", label = "SD (Fixed Cost)", value = .def_cfsd),
            numericInput(inputId = "casd", label = "SD (Company Cost)", value = .def_casd),
          ),
          splitLayout(
            numericInput(inputId = "vbsd", label = "SD (Base Volume)", value = .def_vbsd),
            numericInput(inputId = "vcsd", label = "SD (Cap Volume)", value = .def_vcsd),
            numericInput(inputId = "vdsd", label = "SD (Volume Discount)", value = .def_vdsd),
            numericInput(inputId = "dsd", label = "SD (Discount)", value = .def_dsd),
            numericInput(inputId = "vmsd", label = "SD (Multiplier)", value = .def_vmsd)
          ),
          plotOutput(outputId = "pO_monte"),
          htmlOutput(outputId = "hO_monte")
        ),
        tabPanel(
          title = "Amortization",
          splitLayout(
            numericInput(inputId = "years", label = "Number of Years", value = 15, step = 1),
            numericInput(inputId = "invest", label = "Initial Investment", value = 1e6, step = 100000),
            numericInput(inputId = "dcf", label = "Discount Rate", value = .08, step = .001),
          ),
          plotOutput(outputId = "pO_amortization")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  output$pO_input <- renderPlot({
    if (input$in_fun == "Variable Cost") {
      tab_vc <- get_input_table(
        .cv = input$cv, .v = input$v, .vb = input$vb,
        .vc = input$vc, .vd = input$vd, .d = input$d
      ) %>%
        dplyr::select(-v) %>%
        tidyr::expand_grid(v = 500:10000)

      tab_vc <- tab_vc %>%
        mutate(
          discount = fnc_var_disc(.v = v, .vb = vb, .vc = vc, .vd = vd, .d = d),
          var_unit = fnc_var_unit(.cv = cv, .v = v, .vb = vb, .vc = vc, .vd = vd, .d = d),
          var_cost = fnc_var_cost(.cv = cv, .v = v, .vb = vb, .vc = vc, .vd = vd, .d = d)
        )

      gg_vc_unit <- plot_points(tab_vc, v, var_unit, NULL, NULL, "Unit Variable Cost", .size = 16)
      gg_vc_tot <- plot_points(tab_vc, v, var_cost, NULL, NULL, "Total Variable Cost", .size = 16)
      gg_vc_unit + gg_vc_tot

    } else if (input$in_fun == "Fixed Cost") {
      tab_cf <- get_input_table(
        .cf = input$cf, .v = input$v, .vm = input$vm
        ) %>%
        dplyr::select(-v) %>%
        tidyr::expand_grid(v = 500:10000)
      tab_cf <- tab_cf %>%
        mutate(
          multiple = fnc_fix_mult(.v = v, .vm = vm),
          var_unit = fnc_fix_unit(.cf = cf, .v = v, .vm = vm),
          var_cost = fnc_fix_cost(.cf = cf, .v = v, .vm = vm)
        )

      gg_cf_unit <- plot_points(tab_cf, v, var_unit, NULL, NULL, "Unit Fixed Cost", .size = 16)
      gg_cf_tot <- plot_points(tab_cf, v, var_cost, NULL, NULL, "Total Fixed Cost", .size = 16)
      gg_cf_unit + gg_cf_tot
    } else if (input$in_fun == "Price") {
      tab_rv <- get_input_table(
        .p = input$p, .v = input$v
        ) %>%
        dplyr::select(-v) %>%
        tidyr::expand_grid(v = 500:10000)
      tab_rv <- tab_rv %>%
        mutate(
          sigmoid = fnc_rev_sigmoid(.v = v),
          price = fnc_rev_unit(.p = p, .v = v),
          revenue = fnc_rev(.p = p, .v = v)
        )

      gg_rv_unit <- plot_points(tab_rv, v, price, NULL, NULL, "Price", .size = 16)
      gg_rv_tot  <- plot_points(tab_rv, v, revenue, NULL, NULL, "Revenue", .size = 16)
      gg_rv_unit + gg_rv_tot
    }
  })

  output$cm_table <- function() {
    a <- get_input_table(
      .v = input$v1, .p = input$p, .cv = input$cv, .cf = input$cf,
      .vb = input$vb, .vc = input$vc, .vd = input$vd, .d = input$d,
      .vm = input$vm, .ca = input$ca
    ) %>%
      get_profit() %>%
      get_cm_table() %>%
      rename(`# 2,500` = value, `% 2,500` = perc)
    
    b <- get_input_table(
      .v = input$v2, .p = input$p, .cv = input$cv, .cf = input$cf,
      .vb = input$vb, .vc = input$vc, .vd = input$vd, .d = input$d,
      .vm = input$vm, .ca = input$ca
    ) %>%
      get_profit() %>%
      get_cm_table() %>%
      rename(`# 5,000` = value, `% 5,000` = perc)
    
    c <- get_input_table(
      .v = input$v3, .p = input$p, .cv = input$cv, .cf = input$cf,
      .vb = input$vb, .vc = input$vc, .vd = input$vd, .d = input$d,
      .vm = input$vm, .ca = input$ca
    ) %>%
      get_profit() %>%
      get_cm_table() %>%
      rename(`# 7,500` = value, `% 7,500` = perc)
    
    d <- get_input_table(
      .v = input$v4, .p = input$p, .cv = input$cv, .cf = input$cf,
      .vb = input$vb, .vc = input$vc, .vd = input$vd, .d = input$d,
      .vm = input$vm, .ca = input$ca
    ) %>%
      get_profit() %>%
      get_cm_table() %>%
      rename(`# 10,000` = value, `% 10,000` = perc)
    
    a %>%
      left_join(b, by = "name") %>%
      left_join(c, by = "name") %>%
      left_join(d, by = "name") %>%
      kableExtra::kbl(align = "lcccccc", col.names = NULL) %>%
      kableExtra::kable_paper(html_font = "Times New Roman", lightable_options = "hover") %>%
      kableExtra::column_spec(column = c(1, 3, 5, 7), border_right = TRUE) %>%
      kableExtra::row_spec(row = c(1, 3, 5, 7), bold = TRUE) %>%
      kableExtra::add_header_above(
        c(" " = 1, "Volume 1" = 2, "Volume 2" = 2, "Volume 3" = 2, "Volume 4" = 2)
      ) %>%
      kableExtra::kable_styling(full_width = TRUE, bootstrap_options = "hover")
  } 
  
  output$pO_sensitivity <- renderPlot({
    tab_ <- get_input_table(
      .v = input$v, .p = input$p, .cv = input$cv, .cf = input$cf,
      .vb = input$vb, .vc = input$vc, .vd = input$vd, .d = input$d,
      .vm = input$vm, .ca = input$ca
    ) 

    if (input$sens == "Volume") {
      tab_ %>%
        dplyr::select(-v) %>%
        tidyr::expand_grid(v = seq(500L, 15000L, 100L)) %>%
        get_profit() %>%
        plot_profits(v, profit, .xlab = "Volume", "Profit", "Profit (Volume)", .size = 16)
    } else if (input$sens == "Price") {
      tab_ %>%
        dplyr::select(-p) %>%
        tidyr::expand_grid(p = seq(500L, 1250L, 10L))  %>%
        get_profit() %>%
        plot_profits(p, profit, .xlab = "Price", "Profit", "Profit (Price)", .size = 16)
    } else if (input$sens == "Variable Cost") {
      tab_ %>%
        dplyr::select(-cv) %>%
        tidyr::expand_grid(cv = seq(200L, 750L, 10L))  %>%
        get_profit() %>%
        plot_profits(cv, profit, .xlab = "Variable Cost", "Profit", "Profit (Variable Cost)", .size = 16)
    } else if (input$sens == "Fixed Cost") {
      tab_ %>%
        dplyr::select(-cf) %>%
        tidyr::expand_grid(cf = seq(40000L, 75000L, 1000L))  %>%
        get_profit() %>%
        plot_profits(cf, profit, .xlab = "Fixed Cost", "Profit", "Profit (Fixed Cost)", .size = 16)
    } else if (input$sens == "Discount") {
      tab_ %>%
        dplyr::select(-d) %>%
        tidyr::expand_grid(d = seq(.01, .075, .001))  %>%
        get_profit() %>%
        plot_profits(d, profit, .xlab = "Discount", "Profit", "Profit (Discount)", .size = 16)
    }


  })
  
  rv_monte_ <- reactive({
    set.seed(123)
    .n = 100000
    tab_ <- get_input_table(
      .v = rnorm(n = .n, mean = input$v, sd = input$vsd), 
      .p = rnorm(n = .n, mean = input$p, sd = input$psd), 
      .cv = rnorm(n = .n, mean = input$cv, sd = input$cvsd), 
      .cf = rnorm(n = .n, mean = input$cf, sd = input$cfsd),
      .vb = rnorm(n = .n, mean = input$vb, sd = input$vbsd), 
      .vc = rnorm(n = .n, mean = input$vc, sd = input$vcsd), 
      .vd = rnorm(n = .n, mean = input$vd, sd = input$vdsd), 
      .d  = rnorm(n = .n, mean = input$d, sd = input$dsd),
      .vm = rnorm(n = .n, mean = input$vm, sd = input$vmsd), 
      .ca = rnorm(n = .n, mean = input$ca, sd = input$casd)
    ) %>% get_profit() 
  })
  
  output$pO_monte <- renderPlot({
    tab_ <- rv_monte_()
    plot_monte_carlo(tab_, .size = 16)

  })
  
  output$hO_monte <- function() {
    paste0(
      strong("Profit > 0: "), scales::percent(sum(rv_monte_()$profit > 0) / nrow(rv_monte_()), accuracy = .01), "<br>",
      strong("Profit < 0: "), scales::percent(sum(rv_monte_()$profit < 0) / nrow(rv_monte_()), accuracy = .01), "<br>",
      strong("Mean Profit: "), scales::comma(mean(rv_monte_()$profit)), "<br>",
      strong("Median Profit: "), scales::comma(median(rv_monte_()$profit)), "<br>",
      strong("Minimum Profit: "), scales::comma(min(rv_monte_()$profit)), "<br>",
      strong("Maximum Profit: "), scales::comma(max(rv_monte_()$profit)), "<br>"
    )
  }
  
  output$pO_amortization <- renderPlot({
    tab_ <- get_input_table(
      .v = input$v, .p = input$p, .cv = input$cv, .cf = input$cf,
      .vb = input$vb, .vc = input$vc, .vd = input$vd, .d = input$d,
      .vm = input$vm, .ca = input$ca
    ) 
    tab_ <- get_amortization(tab_, input$years, .invest = input$invest, .discount = input$dcf)

    plot_amortization(tab_, 16)
  })
}

shinyApp(ui, server)