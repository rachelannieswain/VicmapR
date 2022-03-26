library(VicmapR)
darebin <- vicmap_query(layer = "datavic:VMADMIN_LGA_POLYGON") %>%
  select(LGA_NAME) %>% 
  filter(LGA_NAME == "DAREBIN") %>%
  collect()

print(darebin)
# Swoops in Darebin

vicmap_query(layer = "datavic:FLORAFUANA1_SWOOPING_BIRD") %>%
  filter(SWOOP_DATE > "2020-12-31z") %>%
  filter(STATUS == "OK") %>%
  select(SWOOP_DATE, NO_OF_BIRDS, SPECIES, COMMENTS) %>%
  filter(INTERSECTS(darebin)) %>%
  collect() %>%
  print()