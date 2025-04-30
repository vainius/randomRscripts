library(tidyverse)

VDU <- 2108.88  # 2025 m.
MMA <- 1038
yearly_MMA <- MMA * 12
fixed_monthly_NPD <- 747
fixed_yearly_NPD <- fixed_monthly_NPD * 12

tax_rate1 <- 0.20  # 20% iki 60 VDU
tax_rate2 <- 0.32  # 32% virš 60 VDU
threshold_60VDU <- 60 * VDU  # Riba 60 VDU

calculate_MNPD <- function(yearly_income) {
  if (yearly_income <= yearly_MMA) {
    return(fixed_yearly_NPD)
  } else if (yearly_income <= 28647.48) {
    return(max(0, 8964 - 0.49 * (yearly_income - yearly_MMA)))
  } else {
    return(max(0, 4800 - 0.18 * (yearly_income - 12 * 642)))
  }
}

calculate_tax <- function(yearly_income) {
  mnpd <- calculate_MNPD(yearly_income)
  taxable_income <- max(0, yearly_income - mnpd)
  
  if (taxable_income <= threshold_60VDU) {
    tax <- taxable_income * tax_rate1
  } else {
    tax <- threshold_60VDU * tax_rate1 + (taxable_income - threshold_60VDU) * tax_rate2
  }
  
  return(tax)
}

calculate_marginal_tax_rate <- function(income) {
  current_tax <- calculate_tax(income)
  
  increment <- 100
  higher_income <- income + increment
  higher_tax <- calculate_tax(higher_income)
  
  marginal_rate <- (higher_tax - current_tax) / increment
  
  if (income <= fixed_yearly_NPD) {
    marginal_rate <- 0
  }
  
  return(marginal_rate)
}

calculate_effective_tax_rate <- function(yearly_income) {
  tax <- calculate_tax(yearly_income)
  return(max(0, tax / yearly_income))
}

generate_tax_data <- function() {
  income_range <- c()
  marginal_rates <- c()
  effective_rates <- c()
  
  income <- 0
  while (income <= 12000) {
    yearly_income <- income * 12
    marginal_rate <- calculate_marginal_tax_rate(yearly_income)
    effective_rate <- calculate_effective_tax_rate(yearly_income)
    
    income_range <- c(income_range, income)
    marginal_rates <- c(marginal_rates, marginal_rate * 100) # Konvertuojame į procentus
    effective_rates <- c(effective_rates, effective_rate * 100) # Konvertuojame į procentus
    
    if (income < 1038) {
      income <- income + 50 # Smulkesnis žingsnis aplink MMA
    } else {
      income <- income + 100
      }
  }
  return(data.frame(
    monthly_income = income_range,
    marginal_rate = marginal_rates,
    average_rate = effective_rates
  ))
}

tax_data <- generate_tax_data() %>%
  mutate(income_in_vdu = monthly_income/VDU,
         income = monthly_income * 12,
         country = 'Lithuania')

tax_data %>%
  ggplot(aes(x = monthly_income/VDU)) +
    geom_path(aes(y = marginal_rate, colour = 'Ribinis')) +
    geom_path(aes(y = effective_rate, colour = 'Vidutinis'), linetype = 'dotted') +
  theme_bw() +
  ylab('Pajamų mokesčio tarifas, %') +
  xlab('Mėnesinės pajamos išreikštos per VDU (1 = 2108.88 Eur)') +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  scale_colour_manual(name = "Linija", 
                      values = c("Ribinis" = "red", "Vidutinis" = "blue")) +
  ggtitle('Lietuvos pajamų mokestis')

#

# JK pajamų mokesčio skaičiuoklė ir grafikas

# Apibrėžiame mokesčių ribas ir tarifus
uk_tax_thresholds <- c(0, 12570, 50270, 100000, 125140)
uk_tax_rates <- c(0, 0.2, 0.4, 0.6, 0.45)

# Funkcija, kuri apskaičiuoja mokesčius pagal pajamas
calculate_uk_tax <- function(income) {
  tax <- 0
  for (i in 2:length(uk_tax_thresholds)) {
    prev_threshold <- uk_tax_thresholds[i-1]
    curr_threshold <- uk_tax_thresholds[i]
    rate <- uk_tax_rates[i-1]
    
    if (income > prev_threshold) {
      taxable <- min(income, curr_threshold) - prev_threshold
      tax <- tax + taxable * rate
    }
  }
  
  # Jei pajamos viršija didžiausią slenkstį
  if (income > uk_tax_thresholds[length(uk_tax_thresholds)]) {
    tax <- tax + (income - uk_tax_thresholds[length(uk_tax_thresholds)]) * 
      uk_tax_rates[length(uk_tax_rates)]
  }
  
  return(tax)
}

# Funkcija ribiniam mokesčių tarifui apskaičiuoti
calculate_uk_marginal_rate <- function(income) {
  # Randame, kuriam intervalui priklauso pajamos
  bracket <- length(uk_tax_thresholds)
  for (i in 2:length(uk_tax_thresholds)) {
    if (income < uk_tax_thresholds[i]) {
      bracket <- i - 1
      break
    }
  }
  
  # Grąžiname atitinkamą tarifą
  return(uk_tax_rates[bracket])
}

# Funkcija vidutiniam (efektyviam) mokesčių tarifui apskaičiuoti
calculate_uk_average_rate <- function(income) {
  if (income == 0) return(0)
  return(calculate_uk_tax(income) / income)
}

# Funkcija, kuri grąžina abu mokesčių tarifus
get_uk_tax_rates <- function(income) {
  marginal_rate <- calculate_uk_marginal_rate(income)
  average_rate <- calculate_uk_average_rate(income)
  
  return(list(
    income = income,
    marginal_rate = marginal_rate,
    average_rate = average_rate,
    tax_amount = calculate_uk_tax(income)
  ))
}

# Pavyzdys, kaip naudoti funkciją
example_incomes <- c(0, 10000, 15000, 40000, 60000, 110000, 150000)
for (inc in example_incomes) {
  result <- get_uk_tax_rates(inc)
}

# Duomenų generavimas grafikui
uk_avg_yearly_income = 37430

income_range <- seq(0, 200000, by = 1000)
tax_data_uk <- data.frame(
  income = income_range,
  marginal_rate = sapply(income_range, calculate_uk_marginal_rate) * 100,
  average_rate = sapply(income_range, calculate_uk_average_rate) * 100
) %>%
  mutate(income_in_vdu = income/uk_avg_yearly_income,
         monthly_income = income/12,
         country = 'United Kingdom')

ggplot(tax_data_uk, aes(x = income)) +
  geom_path(aes(y = marginal_rate, colour = 'Ribinis')) +
  geom_path(aes(y = average_rate, colour = 'Vidutinis'), linetype = 'dotted') +
  theme_bw()

#

tax_data_uk %>%
  union_all(tax_data) %>%
  ggplot(aes(x = income_in_vdu)) +
  geom_path(aes(y = marginal_rate, colour = 'Ribinis')) +
  geom_path(aes(y = average_rate, colour = 'Vidutinis'), linetype = 'dotted') +
  theme_bw()  +
  facet_wrap(~country, ncol = 2) +
  theme(legend.position = 'bottom') +
  scale_x_continuous(breaks = seq(0, 6, 1)) +
  xlab('Pajamos lyginant su vidutine alga') +
  ylab('Pajamų mokestis') +
  ggtitle('Ribinis ir vidutinis pajamų mokesčiai')

tax_data_uk %>%
  union_all(tax_data) %>%
  ggplot(aes(x = income_in_vdu, colour = country)) +
  geom_path(aes(y = average_rate)) +
  theme_bw()  +
  theme(legend.position = 'bottom') +
  scale_x_continuous(breaks = seq(0, 6, 1)) +
  xlab('Alga lyginant su šalies vidurkiu\n1 VDU LT = 2109 EUR\n 1 VDU JK = 3119 GBP') +
  ylab('Pajamų mokestis (%, nuo visos algos)') +
  xlim(c(0, 3)) +
  scale_colour_manual(name = "Šalis", 
                      values = c("Lithuania" = "red", "United Kingdom" = "blue")) +
  ggtitle('Pajamų mokesčio palyginimas (normalizuotas)')

tax_data_uk %>%
  union_all(tax_data) %>%
  mutate(monthly_income = if_else(country == 'United Kingdom', monthly_income * 1.15, monthly_income)) %>%
  filter(monthly_income <= 12000) %>%
  ggplot(aes(x = monthly_income, colour = country)) +
  geom_path(aes(y = average_rate)) +
  theme_bw()  +
  theme(legend.position = 'bottom') +
  xlab('Mėnesinė alga (Eur)') +
  ylab('Pajamų mokestis (%, nuo visos algos)') +
  scale_colour_manual(name = "Šalis", 
                      values = c("Lithuania" = "red", "United Kingdom" = "blue")) +
  ggtitle('Pajamų mokesčio palyginimas (nenormalizuotas)') +
  scale_x_continuous(breaks = seq(0, 12000, 1000))
