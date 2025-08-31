# # ggridges not working, need to learn its requirements first lmao
# 
# # brewing_materials %>%
# #   filter(!str_detect(material_type, "Total")) %>%
# #   mutate(type = fct_reorder(type, month_current, sum)) %>%
# #   ggplot(aes(x = date, y =  month_current, colour =type)) +
# #   geom_density_ridges() +
# #   scale_y_log10(labels = scales::comma) +
# #   facet_wrap(facets = type,
# #              ncol = 1,
# #              scales = "free") +
# #   labs(x = "Time",
# #        y = "Pounds used in beer production",
# #        fill = "Materials") +
# #   theme_minimal() 

# to install custom shit from git, use remotes::install_github("username/reponame")


remotes::install_github("datacamp/shinymetrics", force = TRUE)


remotes::install_github("datacamp/tidymetrics", force = TRUE)

remotes::install_github('ramnathv/shinybones')


brewing_materials %>%
  filter(!str_detect(material_type, "Total")) %>%
  mutate(type = fct_reorder(type, month_current, sum)) %>%
  ggplot(aes(x = month_current, y = type, fill = type)) +
  geom_density_ridges(alpha = 0.75) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Pounds used in beer production",
       y = "")+
  theme_minimal() +
  theme(legend.position="none")


brewing_summarised %>%
  filter(material_type == "All", 
         period == "year",
         !material == "All") %>% 
  mutate(material = fct_reorder(material, nb_pounds, .desc = FALSE)) %>%
  ggplot(aes(date, nb_pounds, fill = material)) + 
  geom_col(size = 1) + 
  scale_y_continuous(labels = scales::comma) + xlab("Time") +
  ylab("Materials used (lb)") + ggtitle("Materials used in beer production (2008 -
2015)") + 
  labs(fill = "Materials") +
  theme_clean() +
  theme(legend.position = "bottom")



# shinymetric stuff -----------------------------------

str(brewing_summarised)
head(brewing_summarised, 10)

# Check for any problematic values
summary(brewing_summarised)

# Look for any empty or weird values
apply(brewing_summarised, 2, function(x) table(is.na(x)))

# Try reordering columns (some functions are position-sensitive)
brewing_reordered <- brewing_summarised[, c("date", "period", "material_type", "material", "nb_pounds")]
brewing_metrics <- create_metrics(brewing_reordered)
