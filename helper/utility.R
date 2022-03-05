##
# utilify functions
DashbdPlotDataFormat <- function(dataset, period){
  income <- round(sum(dataset$amount[dataset$hyper_category == 'Income']),0)
  principal <- round(sum(dataset$amount[dataset$hyper_category == 'Investments']),0)
  expense <- round(sum(dataset$amount[dataset$hyper_category != 'Income' & dataset$hyper_category != 'Investments']),0)
  
  df <- data.frame(
    period = period,
    type = c("Inflow","Outflow","Outflow"),
    category = c("Income","Expense","Mortgage Principal"),
    value = c(income, expense, principal),
    stringsAsFactors = FALSE
  )
  
  return(df)
}

SummaryPlot <- function(plot_data, text_degr = 0){
  plot_me <- ggplot(plot_data, aes(x = type, y = value, fill = category)) + 
    geom_bar(stat="identity") + 
    facet_grid(.~period) +
    geom_text(
      label = plot_data$value,
      #nudge_x = 0.25, nudge_y = 0.25, 
      check_overlap = T
    ) +
    theme(
      axis.text.x = element_text(angle = text_degr, size = 13),
      axis.title.x = element_blank(),
      axis.title.y = element_blank())
  return(plot_me)
}

ExpCatPlot <- function(plot_data, plot_type){
  if(plot_type == 'pie'){
    plot_me <- pie(plot_data$value, labels = paste0(plot_data$category, " (", plot_data$value, ")"),
        border = "white", col = RColorBrewer::brewer.pal(8, "Set2"))  
  } else if(plot_type == 'treemap'){
    plot_me <- ggplot(plot_data, 
                      aes(fill = category, 
                          area = value,
                          label = paste0(category, " (", value, ")"))) +
      geom_treemap() + 
      geom_treemap_text(colour = "white", 
                        place = "centre")
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
