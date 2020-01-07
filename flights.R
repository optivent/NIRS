library(xlsx)
test <- read_excel("C:/Users/igeor/OneDrive/Desktop/Fliege.xlsx") %>%
  as_tibble() %>% 
  tidyr::gather() %>%
  na.omit() %>% 
  rename(Departure = key, Destination = value) %>% 
  arrange(Destination) %>% mutate(Destination = forcats::fct_inorder(Destination)) %>% 
  arrange(Departure) %>% mutate(Departure = forcats::fct_inorder(Departure)) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = Departure, values_from = Destination) %>% 
  select(-row) 
  

# Destination = forcats::fct_inorder(Destination),


group_by(Departure) %>% 
  arrange(Destination) %>% 
  summarise(nr_of_offers = n(),
            Destinations = paste(unique(Destination), collapse = " , ")) %>% 
  ungroup() %>% 
  xlsx::write.xlsx(file = "All_Flights2.xlsx")