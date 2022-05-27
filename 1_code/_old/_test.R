

# Variables ---------------------------------------------------------------
inv_hcb <- 400000
inv_mcb <- 150000
fc_firm <-  30000
rrr     <- .08

vol_hcb <- 400
vol_mcb <- 250
price_hcb <- 650
price_mcb <- 450

# Tables ------------------------------------------------------------------
.tab <- tab_data <- read.xlsx("0_data/business_case_2.xlsx", 1) %>%
  clean_names() %>%
  filter(!is.na(eur))
.rand = TRUE
get_cm <- function(.tab, .rand = FALSE) {
  if (.rand) {
    tab_ <- .tab %>%
      dplyr::rowwise() %>%
      dplyr::mutate(eur = dplyr::case_when(
        min < 0 & max < 0 ~ runif(1, max, min),
        min > 0 & max > 0 ~ runif(1, min, max),
      )) %>% suppressWarnings() %>%
      dplyr::ungroup()
  } else {
    tab_ <- .tab
  }
  
  tab_ %>%
    dplyr::group_by(type, product) %>%
    dplyr::summarise(eur = sum(eur), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = type, values_from = eur) %>%
    dplyr::transmute(
      product = product,
      Revenue = Price * Volume,
      `Variable Cost` = Variable * Volume,
      `Contribution Margin I` = Revenue + `Variable Cost`,
      `Product Fix Cost` = FixProduct,
      `Contribution Margin II` = `Contribution Margin I` - `Product Fix Cost`,
      `Company Fix Cost` = FixCompany,
    ) %>% 
    tidyr::pivot_longer(Revenue:`Company Fix Cost`)  %>%
    tidyr::pivot_wider(names_from = product) %>%
    dplyr::mutate(ALL = dplyr::if_else(is.na(ALL), rowSums(.[, -(1:2)]), ALL)) %>%
    janitor::adorn_totals(name = "Profit") %>%
    tibble::as_tibble()
}


plan("multisession")
lst_cm <- furrr::future_map_dfr(
  .x = 1:1000, 
  .f = ~ get_cm(tab_data, TRUE), 
  .id = "run", 
  .options = furrr_options(seed = TRUE), 
  .progress = TRUE
  )
plan("default")

tab_cm <- lst_cm %>%
  pivot_longer(!matches("^run$|^name$"), names_to = "product", values_to = "val")

tab_ <- tab_cm %>%
  filter(name == "Contribution Margin I") 
gg_hist <- tab_ %>%
  ggplot(aes(val)) +
  # geom_histogram(aes(fill = ifelse(val > 0,"green","red")), bins = 100) +
  geom_histogram(aes(fill = ifelse(val > 0,"green","red"), alpha = .5), bins = nrow(tab_) / 10) +
  scale_fill_manual(values = c("green", "red")) + 
  facet_wrap(~product) + 
  ggthemes::theme_tufte() + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme(legend.position = "none") + 
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = percent)

ggplotly(gg_hist)




# Create data
my_variable=rnorm(2000, 0 , 10)

# Calculate histogram, but do not draw it
my_hist=hist(my_variable , breaks=40  , plot=F)

# Color vector
my_color= ifelse(my_hist$breaks < -10, rgb(0.2,0.8,0.5,0.5) , ifelse (my_hist$breaks >=10, "purple", rgb(0.2,0.2,0.2,0.2) ))

# Final plot
plot(my_hist, col=my_color , border=F , main="" , xlab="value of the variable", xlim=c(-40,40) )

tab_vc <- read.xlsx("0_data/input_data.xlsx", 1) %>%
  group_by(Product) %>%
  summarise(Cost = sum(Cost))
tab_fc <- read.xlsx("0_data/input_data.xlsx", 2)  %>%
  group_by(Product) %>%
  summarise(Cost = sum(Cost))

get_revenue <- function(.p1, .v1, .p2, .v2) {
  tibble(
    Company = .p1 * .v1 + .p2 * .v2,
    HCB = .p1 * .v1,
    MCB = .p2 * .v2
  )
}
.price <- c(hcb = 650, mcb = 400, lcb = 200)
.volume <- c(hcb = 250, mcb = 500, lcb = 800)
.varc <- c(hcb = 500, mcb = 300)
.fixc <- c(hcb = 12000, mcb = 75000)
.comc <- 50000

get_cm <- function(.price, .volume, .varc, .fixc, .comc) {
  
  tibble(
    price = list(.price),
    volume = list(.volume),
    varc = list(.varc),
    fixc = list(.fixc)
  ) %>% unnest_longer(everything()) %>%
    select(id = price_id, price, volume, varc, fixc) %>%
    transmute(
      id = id,
      Revenue = price * volume,
      `Variable Cost` = -varc * volume,
      `Contribution Margin I` = Revenue + `Variable Cost`,
      `Product Fix Cost` = -fixc,
      `Contribution Margin II` = `Contribution Margin I` - `Product Fix Cost`
    ) %>% 
    pivot_longer(Revenue:`Contribution Margin II`) %>%
    pivot_wider(names_from = id) %>%
    mutate(total = rowSums(.[, -1])) %>%
    bind_rows(tibble(name = "Company Fix Cost", total = .comc)) %>%
    janitor::adorn_totals(name = "Profit") %>%
    as_tibble()

}

.lst <- list(p = c(hcb = 650, mcb = 450, lcb = 250), v = c(hcb = 400, mcb = 600))
get_revenue <- function(.lst) {
  purrr::transpose(.lst) %>%
    tibble::enframe() %>%
    tidyr::unnest_wider(value) %>%
    dplyr::mutate(revenue = p * v) %>%
    dplyr::select(name, revenue) %>%
    tidyr::pivot_wider(names_from = name, values_from = revenue) %>%
    dplyr::mutate(total = rowSums(.[1, ]))
}

get_var_cost <- function(.c1, .v1, .c2, .v2) {
  tibble(
    Company = .c1 * .v1 + .c2 * .v2,
    HCB = .c1 * .v1,
    MCB = .c2 * .v2
  )
}
  
get_cm1 <- function(.revenue, .var_cost) {
  
}

get_revenue(price_hcb, vol_hcb, price_mcb, vol_mcb)
get_var_cost(tab_vc$Cost[1], vol_hcb, tab_vc$Cost[2], vol_mcb)
