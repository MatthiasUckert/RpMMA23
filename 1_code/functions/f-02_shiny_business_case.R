slider_sidebar <- function() {
  tagList(
    h4("Input: Volume/Price"),
    splitLayout(
      sliderInput(inputId = "v", label = "Volume", min = 500, max = 15000, value = .def_v, step = 100, ticks = FALSE),
      sliderInput(inputId = "p", label = "Price", min = 500, max = 1250, value = .def_p, step = 10, ticks = FALSE)
    ),
    
    h4("Input: Costs"),
    splitLayout(
      sliderInput(inputId = "cv", label = "Variable Cost", min = 200, max = 750, value = .def_cv, step = 25, ticks = FALSE),
      sliderInput(inputId = "cf", label = "Fixed Cost", min = 40000, max = 75000, value = .def_cf, step = 1000, ticks = FALSE)
    ),
    sliderInput(inputId = "ca", label = "Cost", min = 150000, max = 500000, value = .def_ca, step = 25000, ticks = FALSE),
    
    h4("Input: Other Input"),
    splitLayout(
      sliderInput(inputId = "vb", label = "Base Volume", min = 2500, max = 5000, value = .def_vb, step = 100, ticks = FALSE),
      sliderInput(inputId = "vc", label = "Cap Volume", min = 7500, max = 10000, value = .def_vc, step = 100, ticks = FALSE)
    ),
    splitLayout(
      sliderInput(inputId = "vd", label = "Volume Discount", min = 250, max = 1000, value = .def_vd, step = 50, ticks = FALSE),
      sliderInput(inputId = "d", label = "Discount", min = .01, max = .075, value = .def_d, step = .0025, ticks = FALSE)
    ),
    sliderInput(inputId = "vm", label = "Volume Multiplier", min = 500, max = 2500, value = .def_vm, step = 100, ticks = FALSE)
    
  )
}
