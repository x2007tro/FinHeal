##
# utilify functions
DashbdPlotDataFormat <- function(dataset, period){
  income <- round(sum(dataset$amount[dataset$hyper_category == 'Income']),0)
  principal <- round(sum(dataset$amount[dataset$category == 'Mortgage Principal']),0)
  expense <- round(sum(dataset$amount[dataset$hyper_category != 'Income' & dataset$category != 'Mortgage Principal']),0)
  
  df <- data.frame(
    period = paste0(period, " ($", scales::comma(income - expense - principal, accuracy = 1), ")"),
    type = c("Inflow","Outflow","Outflow"),
    category = c("Income","Expense","Mortgage Principal"),
    value = c(income, expense, principal),
    stringsAsFactors = FALSE
  )
  
  return(df)
}

SummaryPlot <- function(plot_data, text_degr = 0, legend_pos = 'right'){
  plot_me <- ggplot(plot_data, aes(x = type, y = value, fill = category)) + 
    geom_bar(stat="identity") + 
    facet_grid(.~period) +
    geom_text(
      aes(label = scales::comma(plot_data$value, accuracy = 1), fontface = 'bold'),
      colour = plot_font_color,
      position = position_stack(vjust = 0.5),
      #nudge_x = 0, nudge_y = -0.5,
      #vjust = 0.5, 
      #hjust = 0,
      check_overlap = T
    ) +
    scale_fill_manual(values = get_palette(length(unique(plot_data$category)))) + 
    labs() +
    theme(
      strip.text.x = element_text(size = 13),
      axis.text.x = element_text(angle = text_degr, size = 13, color = plot_font_color),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      rect = element_rect(fill = '#f2f2f2'),
      panel.background = element_rect(fill = '#f2f2f2'),
      plot.background = element_rect(fill = '#f2f2f2', color = '#f2f2f2'),
      legend.position = legend_pos,
      legend.text = element_text(color = plot_font_color),
      legend.title = element_text(color = plot_font_color))
  return(plot_me)
}

ExpCatPlot <- function(plot_data, plot_type, lenged_pos = 'right'){
  if(plot_type == 'pie'){
    plot_me <- pie(plot_data$value, labels = paste0(plot_data$category, " (", plot_data$value, ")"),
        border = "white", col = RColorBrewer::brewer.pal(9, "Set3"))  
  } else if(plot_type == 'treemap'){
    plot_me <- ggplot(plot_data, 
                      aes(fill = category, 
                          area = value,
                          label = paste0(category, " (",  scales::comma(value, accuracy = 1), ")")),
                          fontface = 'bold') +
      geom_treemap() + 
      geom_treemap_text(colour = plot_font_color, 
                        place = "centre") +
      scale_fill_manual(values =  get_palette(length(unique(plot_data$category)))) + 
      theme(
        axis.text.x = element_text(angle = 0, size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_rect(fill = '#f2f2f2'),
        panel.background = element_rect(fill = '#f2f2f2'),
        plot.background = element_rect(fill = '#f2f2f2', color = '#f2f2f2'),
        legend.position = lenged_pos,
        legend.text = element_text(color = plot_font_color),
        legend.title = element_text(color = plot_font_color))
  } else {
    plot_me <- NULL
  }
  
  return(plot_me)
}

PerIncomeTaxCalc <- function(income, tax_year, tax_residence = c("BC","NB"), tax_table, tax_parameters){
  
  tax_parameter <- tax_parameters %>% 
    dplyr::filter(year == tax_year)
  
  fed_tax_table <- tax_table %>% 
    dplyr::filter(province == 'FED' & year == tax_year)

  fed_tax_obj <- PerIncomeTaxCalcCore(income, "FED", fed_tax_table, tax_parameter)
  
  prov_tax_table <- tax_table %>% 
    dplyr::filter(province == tax_residence & year == tax_year)
  
  prov_tax_obj <- PerIncomeTaxCalcCore(income, tax_residence, prov_tax_table, tax_parameter)

  tax_obj <- list(
    ei = fed_tax_obj$ei,
    cpp = fed_tax_obj$cpp,
    fed_tax = fed_tax_obj$tax,
    prov_tax = prov_tax_obj$tax
  )
}

PerIncomeTaxCalcCore <- function(income, tax_residence = c("FED","BC","NB"), tax_table, tax_parameters){
  
  ## EI and CPP
  ei <- min(income, tax_parameters$ei_maximum_earnings)*tax_parameters$ei_premium_rate
  cpp <- (min(income, tax_parameters$year_maximum_pensionable_earnings) - tax_parameters$cpp_maximum_exemption)*tax_parameters$cpp_contribution_rate
  if(income < tax_parameters$cpp_maximum_exemption){ cpp <- 0 }
  
  ## tax calculation
  gross_tax <- sum(diff(c(0, pmin(income, tax_table$bracket))) * tax_table$tax_rate)
  min_tax_rate <- min(tax_table$tax_rate)
  
  basic_credit <- min(income, tax_parameters[[paste0("basic_personal_amounts_", tolower(tax_residence))]])*min_tax_rate
  employment_credit <- min(income, tax_parameters$canada_employment_credit)*min_tax_rate
  
  if(tax_residence != 'FED'){
    employment_credit <- 0
  }
  ei_tax <- ei*min_tax_rate
  cpp_tax <- cpp*min_tax_rate
  net_tax <- gross_tax - (basic_credit + employment_credit + ei_tax + cpp_tax)
  
  ret_obj <- list(
    ei = round(ei),
    cpp = round(cpp),
    tax = round(net_tax)
  )
}

PSPPCalc <- function(income, tax_year, person = c("tong_li","ke_min","reserved_1","reserved_2"), tax_parameters){
  
  tax_parameter <- tax_parameters %>% 
    dplyr::filter(year == tax_year)
  
  ## pspp
  pspp <- sum(diff(c(0, pmin(income, c(tax_parameter$year_maximum_pensionable_earnings,1000000000)))) *
               c(tax_parameter[[paste0(person,"_pspp_contribution_rate_below_YMPE")]],tax_parameter[[paste0(person,"_pspp_contribution_rate_above_YMPE")]]))
  return(round(pspp))
}

##
# Get Financial information
AmortTableConstr <- function(
  house_prcs,
  irs
){
  res <- lapply(irs, function(ir){
    res2 <- lapply(house_prcs, function(hp){
      mortgage(hp*0.8, ir, 25, T, F)
      
      df <- data.frame(
        `house_price` = hp,
        `down_payment_required` = hp*0.2,
        `loan_amount` = hp*0.8,
        `interest_rate` = ir,
        `monthly_payment` = round(monthPay,0),
        stringsAsFactors = FALSE
      )
      df2 <- NULL
      return(list(df, df2))
    })
    tmp1 <- dplyr::bind_rows(lapply(res2,"[[",1))
    tmp2 <- dplyr::bind_rows(lapply(res2,"[[",2))
    return(list(tmp1, tmp2))
  })
  tmp1 <- dplyr::bind_rows(lapply(res,"[[",1))
  tmp2 <- dplyr::bind_rows(lapply(res,"[[",2))
  return(list(ft = tmp1, st = tmp2))
}