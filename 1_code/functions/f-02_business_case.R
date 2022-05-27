# Variable Cost -----------------------------------------------------------
fnc_var_disc <- function(.v, .vb, .vc, .vd, .d) {
  vu_ <- dplyr::case_when(
    .v < .vb ~ .vb,
    .v > .vc ~ .vc,
    TRUE ~ .v
  )
  floor((vu_ - .vb) / .vd) * .d
}

fnc_var_unit <- function(.cv, .v, .vb, .vc, .vd, .d) {
  disc_ <- fnc_var_disc(.v, .vb, .vc, .vd, .d)
  .cv * (1 - disc_)
}

fnc_var_cost <- function(.cv, .v, .vb, .vc, .vd, .d) {
  .v * fnc_var_unit(.cv, .v, .vb, .vc, .vd, .d)
}


# Fixed Cost --------------------------------------------------------------
fnc_fix_mult <- function(.v, .vm) {
  ceiling(.v / .vm)
}

fnc_fix_cost <- function(.cf, .v, .vm) {
  .cf * fnc_fix_mult(.v, .vm)
}

fnc_fix_unit <- function(.cf, .v, .vm) {
  (.cf * fnc_fix_mult(.v, .vm)) / .v
}


# Price -------------------------------------------------------------------
fnc_rev_sigmoid <- function(.v) {
  (1 - (1 / (1 + exp(-((.v / 1000) - 4.25))))) * .4 + .6
}

fnc_rev_unit <- function(.p, .v) {
  .p * fnc_rev_sigmoid(.v)
}

fnc_rev <- function(.p, .v) {
  .v * fnc_rev_unit(.p, .v)
}



# Other Functions ---------------------------------------------------------
get_input_table <- function(.v = 0, .p = 0, .cv = 0, .cf = 0, .vb = 0, .vc = 0, 
                            .vd = 0, .d = 0, .vm = 0, .ca = 0) {
  tibble::tibble(
    v = .v, p = .p, cv = .cv, cf = .cf, ca = .ca, vb = .vb, vc = .vc,
    vd = .vd, d = .d, vm = .vm,
  )
}


get_profit <- function(.tab) {
  .tab %>%
    dplyr::mutate(
      discount = fnc_var_disc(.v = v, .vb = vb, .vc = vc, .vd = vd, .d = d),
      var_unit = fnc_var_unit(.cv = cv, .v = v, .vb = vb, .vc = vc, .vd = vd, .d = d),
      var_cost = fnc_var_cost(.cv = cv, .v = v, .vb = vb, .vc = vc, .vd = vd, .d = d),
      
      multiple = fnc_fix_mult(.v = v, .vm = vm),
      fix_unit = fnc_fix_unit(.cf = cf, .v = v, .vm = vm),
      fix_cost = fnc_fix_cost(.cf = cf, .v = v, .vm = vm),
      
      sigmoid = fnc_rev_sigmoid(.v = v),
      price = fnc_rev_unit(.p = p, .v = v),
      revenue = fnc_rev(.p = p, .v = v),
      cm1 = revenue - var_cost,
      cm2 = cm1 - fix_cost,
      profit = cm2 - ca
    ) 
}

get_cm_table <- function(.tab) {
  .tab %>%
    get_profit() %>%
    dplyr::select(
      Revenue = revenue,
      `Variable Cost` = var_cost,
      `Contribution Margin I` = cm1,
      `Fixed Cost` = fix_cost,
      `Contribution Margin II` = cm2,
      `Company Cost` = ca,
      `Profit` = profit
    ) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::mutate(
      perc = value / first(value),
      value = scales::comma(value),
      perc = scales::percent(perc)
    )
}

get_amortization <- function(.tab, .years, .invest, .discount) {
  .tab %>%
    tidyr::expand_grid(year = seq_len(.years)) %>%
    dplyr::select(year, dplyr::everything()) %>%
    get_profit() %>%
    dplyr::mutate(dcf = profit / ((1 + .discount)^year)) %>%
    dplyr::select(year, dcf) %>%
    tibble::add_row(year = 0, dcf = -.invest) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(csum = cumsum(dcf)) %>%
    dplyr::mutate(
      col1 = dplyr::if_else(csum < 0, "red", "green"),
      col2 = dplyr::if_else(dcf < 0, "red", "green")
    ) %>%
    dplyr::select(year, dcf, csum, col1, col2)
}


# Plotting ----------------------------------------------------------------
plot_points <- function(.tab, .x, .y, .xlab = "", .ylab = "", .title = "",.size) {
  .tab %>%
    ggplot2::ggplot(ggplot2::aes(x = {{.x}}, y = {{.y}})) + 
    ggplot2::geom_point(shape = 1, color = "blue", size = .75) + 
    ggplot2::geom_line(linetype = "dashed", color = "blue") + 
    ggthemes::geom_rangeframe() + 
    ggthemes::theme_tufte() + 
    ggplot2::scale_x_continuous(labels = scales::comma, name = .xlab) + 
    ggplot2::scale_y_continuous(labels = scales::comma, name = .ylab) + 
    ggplot2::ggtitle(.title) +
    ggplot2::theme(text = ggplot2::element_text(size = .size))
}

plot_profits <- function(.tab, .x, .y, .xlab = "", .ylab = "", .title = "", .size = 16) {
  .tab %>%
    dplyr::mutate(col = dplyr::if_else({{.y}} < 0, "red", "green")) %>%
    ggplot2::ggplot(ggplot2::aes(x = {{.x}}, y = {{.y}}), color = col, fill = col) + 
    ggplot2::geom_point(aes(color = col), shape = 1, size = 1.25) + 
    ggthemes::geom_rangeframe() + 
    ggthemes::theme_tufte() + 
    ggplot2::scale_color_manual(values = c("darkgreen", "red")) +
    ggplot2::scale_x_continuous(labels = scales::comma, name = .xlab) + 
    ggplot2::scale_y_continuous(labels = scales::comma, name = .ylab) + 
    ggplot2::theme(legend.position = "none") + 
    ggplot2::ggtitle(.title) + 
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::theme(text = ggplot2::element_text(size = .size))
}

plot_amortization <- function(.tab, .size) {
  pos_ <- which(.tab$csum > 0)[1] - 1
  
  .tab %>%
    ggplot2::ggplot(ggplot2::aes(x = year)) +
    ggplot2::geom_point(aes(y = csum, color = col1), shape = 1, size = 2) +
    ggplot2::scale_color_manual(values = c("darkgreen", "red")) +
    ggplot2::geom_col(aes(y = dcf, fill = col2), alpha = .25) +
    ggplot2::scale_fill_manual(values = c("darkgreen", "red")) +
    ggplot2::geom_vline(xintercept = pos_, linetype = "dashed") + 
    ggthemes::geom_rangeframe() +
    ggthemes::theme_tufte() +
    ggplot2::scale_x_continuous(labels = scales::comma, name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::comma, name = NULL) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("Amortization") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::theme(text = ggplot2::element_text(size = .size))
}

plot_monte_carlo <- function(.tab, .size) {
  min_ <- mean(.tab$profit) - 4 * sd(.tab$profit)
  max_ <- mean(.tab$profit) + 4 * sd(.tab$profit)
  
  .tab  %>%
    ggplot2::ggplot(aes(profit)) +
    ggplot2::geom_density(fill = "white", size = 1) +
    ggplot2::annotate("rect", xmin = min_, xmax = 0, ymin = 0, ymax = Inf, alpha = 0.25, fill = "red") +
    ggplot2::annotate("rect", xmin = 0, xmax = max_, ymin = 0, ymax = Inf, alpha = 0.25, fill = "green") +
    ggthemes::theme_tufte() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_x_continuous(labels = scales::comma, name = "Profit", limits = c(min_, max_)) +
    ggplot2::scale_y_continuous(labels = scales::percent, name = "Density") +
    ggplot2::ggtitle(label = "Monte Carlo Simulation") +
    ggplot2::theme(text = ggplot2::element_text(size = .size))
}