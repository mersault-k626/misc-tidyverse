# Distinct date for big_mac_latest


View(big_mac_latest %>%
  distinct(year(date)))

View(big_mac%>%
       distinct(year(date)))

# latest with ggthemes::theme_economist()

big_mac_latest %>%
  add_count(country, name = 'country_total') %>% # adds a new column that provide the count of the countries across the table
  filter(country_total == max(country_total)) %>%  # filter for only countries that are present across all years
  ggplot(aes(date, local_price, colour = country)) +
  geom_line(linewidth = 1) +
  expand_limits(y =0) +
  ylab('Local price') +
  xlab('Time') +
  ggtitle('Big Mac index (local price) over time (2000 – 2025)') +
  facet_wrap(~ country, scales = 'free_y') +
  ggthemes::theme_economist() +
  theme(legend.position = 'none') 


big_mac %>%
  add_count(country, name = "country_total") %>%
  filter(country_total == max(country_total)) %>%
  mutate(country = fct_reorder(country, local_price, function(.)))


View(big_mac_latest %>%
       filter(country == 'Switzerland'))


View(big_mac_latest %>%
       filter(country_total > 40) %>%
       distinct(country))
View(big_mac_latest %>%
       filter(country_total == max(country_total)) %>%
       distinct(country))

View(big_mac %>%
       filter(country_total == max(country_total)) %>%
       distinct(country, country_total))

View(big_mac %>%
       distinct(date))

View(big_mac_latest %>%
       distinct(date))




View(big_mac_latest %>%
  add_count(country, name = 'country_total'))


big_mac_latest %>%
  add_count(country, name = 'country_total') %>% 
  filter(country_total == max(country_total)) %>%  
  mutate(country = fct_reorder(country, local_price, function(.) max(.) / min(.))) %>%
  ggplot(aes(date, local_price, colour = country)) +
  geom_line(linewidth = 1) +
  expand_limits(y =0) +
  ylab('Local price') +
  xlab('Time') +
  ggtitle('Big Mac index (local price) over time (2000 – 2025)') +
  facet_wrap(~ country, scales = 'free_y') +
  ggthemes::theme_clean() +
  theme(legend.position = 'none')  


# pandemic marker

big_mac_latest %>%
  add_count(country, name = 'country_total') %>% 
  filter(country_total == max(country_total)) %>%  
  mutate(country = fct_reorder(country, local_price, function(.) max(.) / min(.))) %>%
  ggplot(aes(date, local_price, colour = country)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date("2020-03-01"), 
             linetype = "solid", 
             color = "red", 
             linewidth = .75) +
  geom_vline(xintercept = as.Date("2022-02-24"), 
             linetype = "solid", 
             color = "#0057B7", 
             linewidth = .75) +
  expand_limits(y =0) +
  ylab('Local price') +
  xlab('Time') +
  ggtitle('Big Mac index (local price) over time (2000 – 2025)') +
  facet_wrap(~ country, scales = 'free_y') +
  ggthemes::theme_clean() +
  theme(legend.position = 'none')  

library(ggplot2)
library(ggthemes)
library(forcats)
library(dplyr)

# Suppose you already have big_mac_latest

event_lines <- data.frame(
  event_date = as.Date(c("2020-03-01", "2022-02-24")),
  event_label = c("COVID-19 Pandemic", "Russian invasion of Ukraine")
)

big_mac_latest %>%
  filter(country_total == max(country_total)) %>% 
  mutate(country = fct_reorder(country, dollar_price, function(.) max(.) / min(.))) %>%
  ggplot(aes(date, dollar_price, group = country)) +
  geom_line(aes(color = country), linewidth = 1, show.legend = FALSE) +   # hide country legend
  geom_vline(
    data = event_lines,
    aes(xintercept = event_date, color = event_label),
    linewidth = 0.5,
    show.legend = TRUE
  ) +
  expand_limits(y =0) +
  ylab('Big Mac price (USD)') +
  xlab('Time') +
  ggtitle('Big Mac index (USD) over time (2000 – 2025)') +
  facet_wrap(~ country, scales = 'free_y') +
  ggthemes::theme_clean() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    name = NULL,
    values = c(
      setNames(c("red", "#0057B7"), 
               c("COVID-19 Pandemic", "Russian invasion of Ukraine"))
    ),
    guide = guide_legend(override.aes = list(linetype = "solid", linewidth = .5))
  )










big_mac %>%
  filter(country_total == max(country_total)) %>%
  group_by(country) %>%
  summarise(big_mac_inflation = round(x = (last(local_price) / first(local_price)), digits = 2)) %>%
              mutate(country = fct_reorder(country, big_mac_inflation)) %>%
              ggplot(aes(big_mac_inflation, country, fill = big_mac_inflation)) +
              geom_col() +
              scale_x_log10(breaks = c(1, 3, 10, 30, 100)) + # for tickers
              scale_fill_gradient(low = "green", high = "red")  +
              theme(legend.position = 'none') +
              labs(title = 'Big Mac inflation (ratio/multiplier)',
                   xlab = 'Inflation rate',
                   ylab = 'Country')


big_mac_latest %>%
  filter(country_total == max(country_total)) %>%
  group_by(country) %>%
  summarise(big_mac_inflation = round(x = ((last(local_price) - first(local_price)) / first(local_price) * 100), digits = 2)) %>%
  mutate(country = fct_reorder(country, big_mac_inflation)) %>%
  ggplot(aes(big_mac_inflation, country, fill = big_mac_inflation)) +  
  geom_col() +
  scale_x_log10(labels = scales::comma) + 
  scale_fill_gradient(low = "green", high = "red")  +
  theme(legend.position = 'none') +
  labs(title = 'Big Mac inflation(% change)',
       x = 'Inflation rate (%)',
       y = 'Country')




##### nrow/ncol adjustment

big_mac %>%
  filter(country_total == max(country_total)) %>%  # filter for only countries that are present across all years
  mutate(country = fct_reorder(country, local_price, function(.) max(.) / min(.))) %>% 
  # Reorder country factor levels based on price ratio (max/min) within each country
  # For each country, calculates max(local_prices) / min(local_prices) to get the price ratio
  # Countries are ordered from lowest ratio (most consistent pricing) to highest ratio (most variable pricing)
  # Example: Country with prices $2-$4 has ratio 2.0, country with prices $1-$10 has ratio 10.0
  # Result: Countries with similar min/max prices appear first, countries with large price spreads appear last
  # used here to sort country by price volatility ascending
  # max(.) - min(.) vs max(.) / min(.)
  # the first one provides the absolute range,
  # the latter provides range in terms of proportions
  ggplot(aes(date, local_price, colour = country)) +
  geom_line(linewidth = 1) +
  expand_limits(y =0) +
  ylab('Local price') +
  xlab('Time') +
  ggtitle('Big Mac index (local price) over time (2000 – 2020)') +
  facet_wrap(~ country, scales = 'free_y') +
  ggthemes::theme_clean() +
  theme(legend.position = 'none')

# last / first (range of last by first to better reflect the time-series analysis)
big_mac %>%
  filter(country_total == max(country_total)) %>%  # filter for only countries that are present across all years
  mutate(country = fct_reorder(country, local_price, function(.) last(.) / first(.))) %>% 
  ggplot(aes(date, local_price, colour = country)) +
  geom_line(linewidth = 1) +
  expand_limits(y =0) +
  ylab('Local price') +
  xlab('Time') +
  ggtitle('Big Mac index (local price) over time (2000 – 2020)') +
  facet_wrap(~ country, 
             scales = 'free_y',
             ncol = 4) +
  ggthemes::theme_clean() +
  theme(legend.position = 'none')



big_mac_latest %>%
  filter(country_total == max(country_total)) %>%
  group_by(country) %>%
  summarise(big_mac_inflation = round(x = (last(local_price) / first(local_price)), digits = 2)) %>%
  mutate(country = fct_reorder(country, big_mac_inflation)) %>%
  ggplot(aes(big_mac_inflation, country, fill = big_mac_inflation)) +
  geom_col() +
  geom_text(aes(label = paste0(round(big_mac_inflation, 2), "x")), hjust = -0.25) + #hjust adds padding between the bar and the text
  expand_limits(x = 1) +
  scale_x_log10(
    expand = c(0, 0)
  ) + # for tickers
  scale_fill_gradient(low = "green", high = "red")  +
  theme(legend.position = 'none') +
  ggtitle('Big Mac inflation (ratio/multiplier)') +
  xlab('Inflation rate') +
  ylab('Country')

scale_x_log10(
  breaks = c(1, 3, 10, 30, 100),
  expand = c(0, 0)
  
  
  ####
  
  big_mac_latest %>%
    filter(country_total == max(country_total)) %>%
    group_by(country) %>%
    summarise(big_mac_inflation = round(x = ((last(local_price) - first(local_price)) / first(local_price) * 100), digits = 2)) %>%
    mutate(country = fct_reorder(country, big_mac_inflation)) %>%
    ggplot(aes(big_mac_inflation, country, fill = big_mac_inflation)) +  
    geom_col() +
    expand_limits(x = 1) +
    scale_x_log10(expand = c(0, 0),
                  labels = scales::comma) + 
    scale_fill_gradient(low = "green", high = "red")  +
    theme(legend.position = 'none') +
    labs(title = 'Big Mac inflation(% change)',
         x = 'Inflation rate (%)',
         y = 'Country')
  
  
  # Ratio/multiplier method - better for mathematical modeling and log-scale visualizations  
  big_mac_latest %>%
    filter(country_total == max(country_total)) %>%
    group_by(country) %>%
    summarise(big_mac_inflation = last(local_price) / first(local_price)) %>%
    arrange(desc(big_mac_inflation)) %>%
    mutate(country = fct_reorder(country, big_mac_inflation)) %>%
    ggplot(aes(big_mac_inflation, country, fill = big_mac_inflation)) +
    geom_col()+
    geom_text(aes(label = paste0(round(big_mac_inflation, 1), "x")), hjust = -0.05) + #hjust adds padding between the bar and the text
    scale_x_log10(expand = c(0, 0),
                  labels = scales::comma)+ # for tickers
    scale_fill_gradient(low = "green", high = "red")  +
    theme(legend.position = 'none') +
    ggtitle('Big Mac inflation (ratio/multiplier)') +
    xlab('Inflation rate') +
    ylab('Country')

  
  
  how do I shrink the x so that I can fit geom_text for Argentina:
    
    big_mac_latest %>%
    filter(country_total == max(country_total)) %>%
    group_by(country) %>%
    summarise(big_mac_inflation = last(local_price) / first(local_price)) %>%
    arrange(desc(big_mac_inflation)) %>%
    mutate(country = fct_reorder(country, big_mac_inflation)) %>%
    ggplot(aes(big_mac_inflation, country, fill = big_mac_inflation)) +
    geom_col()+
    geom_text(aes(label = paste0(round(big_mac_inflation, 1), "x"),
                  hjust = ifelse(big_mac_inflation > 50, 1.05, -0.05)),
              color = "white") + #hjust adds padding between the bar and the text
    scale_x_log10(expand = c(0, 0),
                  limits = c(1, NA),
                  labels = scales::comma)+ # for tickers
    scale_fill_gradient(low = "green", high = "red")  +
    theme(legend.position = 'none') +
    ggtitle('Big Mac inflation (ratio/multiplier)') +
    xlab('Inflation rate') +
    ylab('Country')
  
  expand = expansion(mult = c(0, 0.1))


  + geom_text(aes(label = paste0(round(big_mac_inflation, 1), "x"),
                  hjust = ifelse(country == "Argentina", 1.05, -0.05)),  # Inside for Argentina only
              color = ifelse(big_mac_latest_summary$country == "Argentina", "white", "black"))

  
  geom_text(aes(label = paste0(round(big_mac_inflation, 1), "x"),
                hjust = ifelse(big_mac_inflation > 50, 1.05, -0.05)),
            color = "white")

  geom_text(aes(label = paste0(round(big_mac_inflation, 1), "x"),
                hjust = ifelse(big_mac_inflation > 50, 1.05, -0.05)),
            color = "white")
  
  
  
  big_mac_latest %>%
    filter(country_total == max(country_total)) %>%
    group_by(country) %>%
    summarise(big_mac_inflation = round(x = (last(local_price) / first(local_price)), digits = 2)) %>%
    mutate(country = fct_reorder(country, big_mac_inflation)) %>%
    ggplot(aes(big_mac_inflation, country, fill = big_mac_inflation)) +
    geom_col()+
    geom_text(aes(label = paste0(round(big_mac_inflation, 1), "x"),
                  hjust = ifelse(big_mac_inflation > 50, 1.05, -0.25)),  # Inside if >50x
              color = "black") +
    expand_limits(x = 1) +
    scale_x_log10(expand = c(0, 0),
                  labels = scales::comma)+ # for tickers
    scale_fill_gradient(low = "green", high = "red")  +
    theme_clean()+
    theme(legend.position = 'none',
      panel.grid.minor = element_blank(),  # Removes minor grid lines (the dots)
      panel.grid.major = element_blank()   # Removes major grid lines too
 +
    ggtitle('Big Mac inflation (ratio/multiplier)') +
    xlab('Inflation rate') +
    ylab('Country')
  
##
big_mac_latest %>%
  filter(country_total == max(country_total)) %>%
  group_by(country) %>%
  summarise(big_mac_inflation = round(x = ((last(local_price) - first(local_price)) / first(local_price) * 100), digits = 2)) %>%
  arrange(desc(big_mac_inflation)) %>%
  mutate(country = fct_reorder(country, big_mac_inflation)) %>%
  ggplot(aes(big_mac_inflation, country, fill = big_mac_inflation)) +  
  geom_col()+
  geom_text(aes(label = paste0(scales::comma(round(big_mac_inflation, 1)), "%"),
                hjust = ifelse(big_mac_inflation > 750, 1.05, -0.25)),
            color = "black"
            ) +
  expand_limits(x = 1) +
  scale_x_log10(expand = c(0, 0),
                labels = scales::comma) + 
  scale_fill_gradient(low = "green", high = "red")  +
  theme(legend.position = 'none') +
  labs(title = 'Big Mac inflation(% change)',
       x = 'Inflation rate (%)',
       y = 'Country')

geom_text(
  aes(
    label = paste0(scales::comma(round(big_mac_inflation, 1)), "%"),
    hjust = ifelse(big_mac_inflation > 750, 1.05, -0.25)
  ),
  color = "black"
)

big_mac_latest %>%
  filter(country_total == max(country_total)) %>%
  group_by(country) %>%
  summarise(big_mac_inflation = round(x = ((last(local_price) - first(local_price)) / first(local_price) * 100), digits = 2)) %>%
  arrange(desc(big_mac_inflation)) %>%
  mutate(country = fct_reorder(country, big_mac_inflation))
