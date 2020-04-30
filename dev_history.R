usethis::use_git()
usethis::use_github()







require(googlesheets4)
url <- "https://docs.google.com/spreadsheets/d/1s3VJ4M6BlQ6zkaVJiEE86n8hhYJfm4UBj9Gibn3YFQY/edit?usp=sharing"
googlesheets4::sheets_deauth()
edges <- googlesheets4::read_sheet(url,sheet = "relations3")
nodes <- googlesheets4::read_sheet(url,sheet = "agents3") %>% 
  select(name,L1,L2,poids) %>% 
  filter(name %in% unique(c(edges$from,edges$to)))

saveRDS(edges,"edges.rds")
saveRDS(nodes,"nodes.rds")


