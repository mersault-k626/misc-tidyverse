---
  title: "TidyTemplate"
date: 2025-08-16
output: html_document
editor_options: 
  chunk_output_type: inline
---
  
  # TidyTuesday
  
  Join the Data Science Learning Community in the weekly #TidyTuesday event!
Every week we post a raw dataset, a chart or article related to that dataset, and ask you to explore the data.
While the dataset will be “tamed”, it will not always be tidy! As such you might need to apply various R for Data Science techniques to wrangle the data into a true tidy format.
The goal of TidyTuesday is to apply your R skills, get feedback, explore other’s work, and connect with the greater #RStats community!
As such we encourage everyone of all skills to participate!
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r libraries, message=FALSE, warning=FALSE}
library(tidyverse)
library(tidytuesdayR)
library(scales)
library(ggthemes)
theme_set(theme_light())
```

# Load the weekly Data

Download the weekly data and make available in the `tt` object.

```{r Load}
tt <- tt_load('2020-11-03')
ikea <- tt$ikea

glimpse(ikea)

ikea <- ikea %>%
  select(-...1) %>%
  mutate(price_usd = price * .27,
         short_description = str_trim(str_replace_all(short_description, "\\s+", " ")))
```


# Readme

Take a look at the readme for the weekly data to get insight on the dataset.
This includes a data dictionary, source, and a link to an article on the data.

```{r Readme, eval = interactive()}
tt
```


# Glimpse Data

Take an initial look at the format of the data available.

```{r Glimpse}
map(tt, glimpse)

ikea %>%
  count(category, sort = TRUE) %>%
  mutate(category = fct_reorder(category, n)) %>%
  ggplot(aes(n, category, fill = category)) +
  geom_col() +
  xlab("Quantity") +
  ylab(NULL) +
  ggtitle("Ikea Product Category") +
  theme_minimal() +
  theme(legend.position = "none")

```

```{r echo=TRUE}

library(glue)

library(ggridges)


ikea %>%
  add_count(category, name = "category_total") %>% # add_count() and add_tally() are equivalents to count() and tally() but use mutate() instead of summarise() so that they add a new column with group-wise counts.
  mutate(category = glue("{category} ({category_total})"),
         category = fct_reorder(category, price_usd,)) %>%
  ggplot(aes(price_usd, category, fill = other_colors)) +
  geom_density_ridges(alpha = .45) +
  # geom_jitter(width = -0, height = .1, alpha = .25) +
  scale_x_log10(labels = dollar) +
  ylab(NULL) +
  xlab("Price (USD) ") +
  ggtitle("Ikea Product Price by Category") 
```

Exploring the relation between price, category and other_colors would be beneficial if one were to develop a predictive model.


### Exploring common name and category

```{r}

ikea %>%
  mutate(name = fct_lump(name, 20)) %>% # fct_lump() will lump all but the top 20 most common names into "Other"
  filter(!name == "Other") %>%
  count(name, category ,sort = TRUE) %>%
  mutate(name = fct_reorder(name, n, sum),
         category = fct_reorder(category, n, sum)) %>%
  ggplot(aes(n, name, fill = category)) +
  geom_col() +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE)) +
  labs(x = "Quantity",
       y = "Product Name") +
  theme_clean()
```

Variations of prodcuts:
  
  The variation of product name is explained by the wide range of family of products under each name. For instance, Besta is a storage system that comprises different types of storage selections.


### Tinkering with short description (dimensions)

```{r echo=TRUE}

ikea %>%
  separate(short_description,
           c("main_description", "rest"),
           ", ",
           extra = "merge",
           fill = "right") %>%
  extract(rest, 
          "dimension_cm", 
          "([\\d\\-xX]+) cm", 
          remove = FALSE) %>%
  unite(category_and_description, category, main_description, sep = " - ") %>%
  count(category_and_description, sort = TRUE)

```


### Volume

```{r}
ikea %>%
  select(item_id, name, category, price, depth, height, width)

```

- On log scale in price: The data ain't normally distributed; log-scale makes it easier to see the bimodality of the price for each category.





# Wrangle

Explore the data and process it into a nice format for plotting! Access each dataset by name by using a dollarsign after the `tt` object and then the name of the data set.

```{r Wrangle}



```


# Visualize

Using your processed dataset, create your unique visualization.

```{r Visualize}


  
```

# Save Image

Save your image for sharing. Be sure to use the `#TidyTuesday` hashtag in your post on twitter! 

```{r}
# This will save your most recent plot
ggsave(
  filename = "My TidyTuesday Plot.png",
  device = "png"
)
```
