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
      # discount = fnc_var_disc(.v = v, .vb = vb, .vc = vc, .vd = vd, .d = d),
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

get_sensitivity <- function(.vol = NULL, .price = NULL, .vc = NULL, .fc = NULL, .vol_base = NULL,
                            .vol_cap = NULL, .vol_dis = NULL,
                            .disc = NULL, .vol_mult = NULL, .cost = 0,
                            .ret = c("CM I", "CM II", "Profit")) {
  
  get_profit = Vectorize(get_profit)
  
  tab_ <- get_input_table(
    .vol = .vol, .price = .price, .vc = .vc, .fc = .fc, .vol_base = .vol_base,
    .vol_cap = .vol_cap, .vol_dis = .vol_dis, .disc = .disc,
    .vol_mult = .vol_mult, .cost = .cost
  ) %>% tidyr::expand_grid(seq = seq(0, 2, .01)) %>%
    dplyr::select(seq, dplyr::everything())
  coln_ <- colnames(tab_)
  
  out_ <- purrr::map_dfr(
    .x = purrr::set_names(coln_[-1], coln_[-1]),
    .f = ~ tab_ %>%
      dplyr::mutate(
        !!dplyr::sym(.x) := !!dplyr::sym(.x) * seq,
        val = get_profit(
          .vol = volume, .price = price, .vc = var_cost, .vol_base = vol_base, 
          .vol_cap = vol_cap, .vol_dis = vol_dis, .disc = disc, 
          .fc = fix_cost, .vol_mult = vol_mult, .cost = firm_cost, .ret = .ret
        ),
      ),
    .id = "input" 
  ) %>% 
    dplyr::mutate(
      seq = seq - 1, 
      type = .ret,
      val_std = val / get_profit(.vol = .vol, .price = .price, .vc = .vc, .fc = .fc, 
                             .vol_base = .vol_base, .vol_cap = .vol_cap, .vol_dis = .vol_dis,
                             .disc = .disc, .vol_mult = .vol_mult, 
                             .cost = .cost, .ret = .ret)
      )
}

plot_points <- function(.tab, .x, .y, .xlab = "", .ylab = "", .title = "") {
  .tab %>%
    ggplot2::ggplot(ggplot2::aes(x = {{.x}}, y = {{.y}})) + 
    ggplot2::geom_point(shape = 1, color = "blue", size = .75) + 
    ggplot2::geom_line(linetype = "dashed", color = "blue") + 
    ggthemes::geom_rangeframe() + 
    ggthemes::theme_tufte() + 
    ggplot2::scale_x_continuous(labels = scales::comma, name = .xlab) + 
    ggplot2::scale_y_continuous(labels = scales::comma, name = .ylab) + 
    ggplot2::ggtitle(.title)
}

plot_profits <- function(.tab, .x, .y, .xlab = "", .ylab = "", .title = "") {
  .tab %>%
    dplyr::mutate(col = dplyr::if_else({{.y}} < 0, "red", "green")) %>%
    ggplot2::ggplot(ggplot2::aes(x = {{.x}}, y = {{.y}}), color = col, fill = col) + 
    ggplot2::geom_point(aes(color = col), shape = 1, size = .75) + 
    ggthemes::geom_rangeframe() + 
    ggthemes::theme_tufte() + 
    ggplot2::scale_color_manual(values = c("darkgreen", "red")) +
    ggplot2::scale_x_continuous(labels = scales::comma, name = .xlab) + 
    ggplot2::scale_y_continuous(labels = scales::comma, name = .ylab) + 
    ggplot2::theme(legend.position = "none") + 
    ggplot2::ggtitle(.title) + 
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
}

plot_sensitivity <- function(.tab) {
  .tab %>%
    dplyr::filter(seq >= - .75, seq <= .75) %>%
    dplyr::mutate(label = dplyr::if_else(seq == .75 | seq == -.75, input, NA_character_)) %>%
    ggplot2::ggplot(ggplot2::aes(x = seq, y = val_std, group = input, label = label)) +
    # ggplot2::geom_point(shape = 1, size = .5, color = "blue") +
    ggplot2::geom_line(linetype = "dashed", color = "blue") + 
    ggthemes::geom_rangeframe() +
    ggthemes::theme_tufte() +
    ggplot2::scale_x_continuous(labels = scales::percent, name = "Delta Input") +
    ggplot2::scale_y_continuous(labels = scales::percent, name = "Delta Profit") +
    ggplot2::theme(legend.position = "none") + 
    ggrepel::geom_label_repel() %>%
    suppressWarnings() %>%
    suppressMessages()
}


plot_cm <- function(.tab) {
  .tab %>%
    ggplot2::ggplot(ggplot2::aes(x = volume, y = value, color = col)) +
    ggplot2::geom_point(shape = 1, size = .25, alpha = .1) +
    ggplot2::scale_color_manual(values = c("green", "red")) +
    ggplot2::geom_line(linetype = "dashed", color = "grey") +
    ggthemes::geom_rangeframe() +
    ggthemes::theme_tufte() +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::scale_y_continuous(labels = scales::comma) + 
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
    ggplot2::facet_wrap(~ name) + 
    ggplot2::theme(legend.position = "none")
}

randomize_input <- function(.tab, .type = c("monte_carlo", "sensitivity"), .n = 1000) {
  type_ <- match.arg(.type, c("monte_carlo", "sensitivity"))
  
  if (type_ == "monte_carlo") {
    mean_ <- rep(.tab$eur, .n)
    min_ <- rep(.tab$min, .n)
    max_ <- rep(.tab$max, .n)
    new_val_ <- ifelse(
      test = min_ < 0 & max_ < 0,
      yes = runif(length(mean_), max_, min_),
      no = runif(length(mean_), min_, max_)
    ) %>% suppressWarnings()
    .tab %>%
      dplyr::slice(rep(seq_len(dplyr::n()), .n)) %>%
      dplyr::mutate(eur = new_val_, run = rep(seq_len(.n), each = nrow(.tab)))
  } else {
    seq0_ <- seq(.05, 1.95, .05)
    new_val_ <- rep(1, nrow(.tab) * length(seq0_) * nrow(.tab))
    seq1_ <- rep(seq0_, nrow(.tab))
    idx_ <- map(seq_len(nrow(.tab)), ~ seq(.x, nrow(.tab) * length(seq0_), nrow(.tab)))
    idx_ <- unlist(map2(idx_, seq_len(nrow(.tab)) - 1, ~ .x + nrow(.tab) * length(seq0_) * .y))
    new_val_[idx_] <- seq1_
    sens_ <- new_val_
    sens_[-idx_] <- NA_real_
    
    .tab %>%
      dplyr::slice(rep(seq_len(dplyr::n()), nrow(.tab) * length(seq0_))) %>%
      dplyr::mutate(eur = eur * new_val_, sensitivity = sens_)
    
  }
  

}

get_cm <- function(.tab) {
  tab_ <- if (!"run" %in% colnames(.tab)) {
    dplyr::select(dplyr::mutate(.tab, run = 1), run, dplyr::everything())
  } else {
    dplyr::select(.tab, run, dplyr::everything())
  }
  
  
  tmp0_ <- tab_ %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(run, type, product) %>%
    dplyr::summarise(eur = sum(eur), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = type, values_from = eur) %>%
    dplyr::transmute(
      run = run,
      product = product,
      `1-Revenue` = Price * Volume,
      `2-Variable Cost` = Variable * Volume,
      `3-Contribution Margin I` = `1-Revenue` + `2-Variable Cost`,
      `4-Product Fix Cost` = FixProduct,
      `5-Contribution Margin II` = `3-Contribution Margin I` + `4-Product Fix Cost`,
      `6-Company Fix Cost` = FixCompany,
    ) %>%
    tidyr::pivot_longer(`1-Revenue`:`6-Company Fix Cost`) %>%
    tidyr::pivot_wider(names_from = product) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(ALL = dplyr::if_else(is.na(ALL), rowSums(.[, -(1:3)]), ALL))
  
  tmp1_ <- tmp0_ %>%
    dtplyr::lazy_dt() %>%
    dplyr::select(-name) %>%
    tidyr::pivot_longer(!dplyr::matches("^run$")) %>%
    dplyr::group_by(run, name) %>%
    dplyr::summarise(Profit = sum(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = Profit) %>%
    dplyr::mutate(name = "7-Profit") %>%
    dplyr::select(run, name, dplyr::everything()) %>%
    tibble::as_tibble()
  
  dplyr::bind_rows(tmp0_, tmp1_) %>%
    dtplyr::lazy_dt() %>%
    dplyr::arrange(run, name)  %>%
    dplyr::mutate(name = gsub("^\\d-", "", name)) %>%
    tibble::as_tibble()
  
}

make_density_plot <- function(.tab, .type) {
  tab_ <- .tab %>%
    dplyr::filter(name == .type) %>%
    tidyr::pivot_longer(!matches("run|name"), names_to = "product") %>%
    dplyr::mutate(value = value / 1e6)
  
  tab_ %>%
    ggplot2::ggplot(aes(value)) +
    ggplot2::geom_density(fill = "grey") +
    ggplot2::facet_wrap(~product) +
    ggplot2::annotate("rect", xmin = min(tab_$value), xmax = 0, ymin = 0, ymax = Inf, alpha = 0.25, fill = "red") +
    ggplot2::annotate("rect", xmin = 0, xmax = max(tab_$value), ymin = 0, ymax = Inf, alpha = 0.25, fill = "green") +
    ggthemes::theme_tufte() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_x_continuous(labels = comma, name = paste0(.type, " (mEUR)")) +
    ggplot2::scale_y_continuous(labels = NULL, name = NULL, minor_breaks = NULL, breaks = NULL) + 
    ggplot2::ggtitle(label = "Monte Carlo Simulation", subtitle = .type)
}