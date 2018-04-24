library(readxl)
library(tidyverse)

NHE2016 <- read_excel("NHE2016.xlsx")

nhe <- as_tibble(NHE2016)

nh2 <- nh2 %>% 
  filter(row_number() %in% mlocs2) %>% 
  transmute(paste0("Medicaid ", spending_type))

mlocs <- which(nh2$spending_type == "Medicaid (Title XIX)")

mlocs2 <- c(mlocs+1, mlocs+2)

for (i in ex) {
  top = i + 22
  expenditure[i:top,1] <- nh4[i,1]
}

nh %>%
  select(
    'Out of pocket',
    'Private Health Insurance',
    'Medicare',
    'Medicaid Federal',
    'Medicaid State and Local',
    'CHIP Federal',
    'CHIP State and Local',
    'Department of Defense',
    'Department of Veterans Affairs',
    'Worksite Health Care',
    'Other Private Revenues',
    'Indian Health Services',
    'Workers Compensation',
    'General Assistance',
    'Maternal Federal',
    'Maternal State and Local',
    'Vocational Federal',
    'Vocational State and Local',
    'Other Federal Programs*',
    'SAMHSA',
    "Other State and Local Programs**",
    'School Health',
    'Public Health Federal Funds',
    'Public Health State/Local Funds'
  )


nh_final$expenditure <- fct_relevel(nh_final$expenditure,
                                    "Total Hospital Expenditures",
                                    "Total Physician and Clinical Expenditures",
                                    "Total Dental Services Expenditures",
                                    "Total Other Professional Services Expenditures",
                                    "Total Home Health Care Expenditures",
                                    "Other Non-Durable Medical Products Expenditures",
                                    "Total Prescription Drug Expenditures",
                                    "Total Durable Medical Equipment Expenditures",
                                    "Total Nursing Care Facilities and Continuing Care Retirement Communities",
                                    "Total Other Health, Residential, and Personal Care Expenditures",
                                    "Federal Administration Expenditures",
                                    "State and Local  Administration Expenditures",
                                    "Net Cost of Health Insurance Expenditures",
                                    "Public Health Federal Funds")

nh$expenditure <- nh$expenditure %>% 
  fct_relabel(~str_replace_all(.x, c("," = "", "  " = " "))) %>%
  fct_relabel(make.names) %>% 
  fct_relabel(tolower)

cms_programs <- c("Medicare",
                  "Medicaid Federal",
                  "Medicaid State and Local",
                  "CHIP Federal",            
                  "CHIP State and Local")

health_insurance <- c("Private Health Insurance",
                      "Medicare",                    
                      "Medicaid Federal",
                      "Medicaid State and Local",  
                      "CHIP Federal",
                      "CHIP State and Local",         
                      "Department of Defense",
                      "Department of Veterans Affairs")


drug %>% 
  filter(source %in% source_list) %>% 
  ggplot(aes(year, amount, fill = fct_reorder(source, amount, max))) + 
  geom_area(position = 'fill') + 
  scale_fill_brewer(palette = 'Spectral')

stack_plot <- function(data, x, y, f){
  ggplot(data, aes_(as.name(x), as.name(y), fill = as.name(f))) + 
    geom_area(position = 'fill') + 
    scale_fill_brewer(palette = 'Spectral') + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

aes_test <- function(x, y, f) {
  print(aes_(as.name(x), y, col = f))
  print(x)
}
