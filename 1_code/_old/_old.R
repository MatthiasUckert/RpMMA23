
# Contribution Margin I
```{r}
tab_volume <- tibble(
  volume = seq(250, 10000, 5),
  var_cost = 500,
  base_var = 500,
  fix_cost = 50000,
  base_fix = 750,
  price = 750,
  cmp_cost = 10000
) %>%
  mutate(
    var_cost = fnc_var_cost(var_cost, base_var, volume),
    fix_cost = fnc_fix_cost(fix_cost, base_fix, volume),
    revenue  = fnc_revenue(price, volume)
  ) %>%
  mutate(
    cm1 = revenue - var_cost,
    cm2 = cm1 - fix_cost,
    profit = cm2 - cmp_cost
  )


monte_carlo <- function(.volume, .var_cost, .base_var, .fix_cost, .base_fix,
                        .price, .cmp_cost, .n) {
  tibble(
    volume   = runif(.n, .volume[1], .volume[2]),
    var_cost = runif(.n, .var_cost[1], .var_cost[2]),
    base_var = runif(.n, .base_var[1], .base_var[2]),
    fix_cost = runif(.n, .fix_cost[1], .fix_cost[2]),
    base_fix = runif(.n, .base_fix[1], .base_fix[2]),
    price    = runif(.n, .price[1], .price[2]),
    cmp_cost = runif(.n, .cmp_cost[1], .cmp_cost[2])
  ) %>%
    mutate(
      var_cost = fnc_var_cost(var_cost, base_var, volume),
      fix_cost = fnc_fix_cost(fix_cost, base_fix, volume),
      revenue  = fnc_revenue(price, volume)
    ) %>%
    mutate(
      cm1 = revenue - var_cost,
      cm2 = cm1 - fix_cost,
      profit = cm2 - cmp_cost
    )
}


sensitivity <- function(.volume, .var_cost, .base_var, .fix_cost, .base_fix,
                        .price, .cmp_cost, .n, .col) {
  tibble(
    base = seq(.01, 1.99, .01),
    volume = mean(runif(.n, 250, 1000)),
    var_cost = mean(runif(.n, 250, 600)),
    base_var = mean(runif(.n, 400, 600)),
    fix_cost = mean(runif(.n, 25000, 100000)),
    base_fix = mean(runif(.n, 750, 1250)),
    price = mean(runif(.n, 500, 1250)),
    cmp_cost = mean(runif(.n, 10000, 10000))
  ) %>%
    mutate({{ .col }} := {{ .col }} * base) %>%
    mutate(
      var_cost = fnc_var_cost(var_cost, base_var, volume),
      fix_cost = fnc_fix_cost(fix_cost, base_fix, volume),
      revenue  = fnc_revenue(price, volume)
    ) %>%
    mutate(
      cm1 = revenue - var_cost,
      cm2 = cm1 - fix_cost,
      profit = cm2 - cmp_cost
    ) %>%
    mutate(
      change = profit / .$profit[.$base == 1] - 1
    )
}

tab_monte_carlo <- monte_carlo(
  .volume = c(500, 10000),
  .var_cost = c(250, 500),
  .base_var = c(500, 500),
  .fix_cost = c(5000, 25000),
  .base_fix = c(1000, 1000),
  .price = c(500, 1500),
  .cmp_cost = c(15000, 15000), 
  .n = 100000
) 

tab_sensitvity <- sensitivity(
  .volume = c(500, 10000),
  .var_cost = c(250, 500),
  .base_var = c(500, 500),
  .fix_cost = c(5000, 25000),
  .base_fix = c(1000, 1000),
  .price = c(500, 1500),
  .cmp_cost = c(15000, 15000), 
  .n = 100000,
  .col = volume
)

gg_ <- tab_sensitvity %>%
  ggplot(aes(base, change)) + 
  geom_col(fill = "grey")

plotly::ggplotly(gg_, width = 750)
```


```{r}
tab_volume <- tibble(
  base = seq(.01, 1.99, .01),
  volume   = mean(runif(100000, 250, 1000)),
  var_cost = mean(runif(100000, 250, 600)),
  base_var = mean(runif(100000, 400, 600)) ,
  fix_cost = mean(runif(100000, 25000, 100000)) ,
  base_fix = mean(runif(100000, 750, 1250)),
  price    = mean(runif(100000, 500, 1250)) * base,
  cmp_cost = mean(runif(100000, 10000, 10000))
) %>%
  mutate(
    var_cost = fnc_var_cost(var_cost, base_var, volume),
    fix_cost = fnc_fix_cost(fix_cost, base_fix, volume),
    revenue  = fnc_revenue(price, volume)
  ) %>%
  mutate(
    cm1 = revenue - var_cost,
    cm2 = cm1 - fix_cost,
    profit = cm2 - cmp_cost
  ) %>%
  mutate(
    change = profit / .$profit[.$base == 1] - 1
  )

tab_volume %>%
  ggplot(aes(base, change)) + 
  geom_col()

```


```{r}
gg_cm1 <- plot_points(tab_cm[250:2500, ], volume, cm1, "Volume", "Unit Contribution Margin I") +
  scale_y_continuous(labels = comma) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = tab_cm$volume[which(tab_cm$cm1 < 0)[1]], linetype = "dashed")
gg_cm2 <- plot_points(tab_cm[250:2500, ], volume, cm2, "Volume", "Unit Contribution Margin II") +
  scale_y_continuous(labels = comma) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = tab_cm$volume[which(tab_cm$cm2 < 0)[1]], linetype = "dashed")

plotly::ggplotly(gg_cm1, width = 750)
plotly::ggplotly(gg_cm2, width = 750)
```


```{r include=FALSE}
.tab <- tab_data <- read.xlsx("0_data/business_case_2.xlsx", 1) %>%
  clean_names() %>%
  filter(!is.na(eur)) %>%
  as_tibble() %>%
  mutate(position = if_else(is.na(position), type, position))
```

```{r include=FALSE}
tab_monte_carlo <- randomize_input(tab_data, .n = 1e4)
tab_cm <- get_cm(tab_monte_carlo)
```

```{r}
make_density_plot(tab_cm, "Contribution Margin I")
make_density_plot(tab_cm, "Contribution Margin II")
make_density_plot(tab_cm, "Profit")
```


```{r}
tab_sensitivity <- randomize_input(tab_data, .type = "sensitivity") %>%
  mutate(run = if_else(!is.na(sensitivity), paste(sensitivity, position, product, sep = "-"), NA_character_)) %>%
  fill(run)

tab_cm <- get_cm(tab_sensitivity) %>%
  filter(name == "Profit") %>%
  separate(run, c("sens", "tmp1", "tmp2"), convert = TRUE, sep = "-") %>%
  unite(change, tmp2, tmp1, sep = "-") %>%
  mutate(change = gsub("ALL-", "", change)) %>%
  group_by(change) %>%
  mutate(base = if_else(sens == 1, ALL, NA_real_)) %>%
  fill(base, .direction = "downup") %>%
  ungroup() %>%
  mutate(profit = (ALL - base) / base) %>%
  filter(between(sens, .1, 1.9))

unique(tab_cm$change)
```


```{r}
tab_cm %>%
  filter(change == "MCB-Volume") %>%
  ggplot(aes(x = sens, y = profit)) + 
  geom_col(aes(fill = ifelse(profit < 0, "red", "green"))) + 
  scale_fill_manual(values = c("blue", "darkblue")) + 
  facet_wrap(~ change) + 
  scale_x_continuous(name = "Change in Input", labels = percent) + 
  scale_y_continuous(name = "Change in Profit", labels = percent) + 
  geom_vline(xintercept = 1, linetype = "dashed") + 
  ggthemes::theme_tufte() + 
  ggthemes::geom_rangeframe() +
  theme(legend.position = "none")


```

