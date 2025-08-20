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


remotes::install_github("devOpifex/shinymetrics")


remotes::install_github("datacamp/tidymetrics")


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