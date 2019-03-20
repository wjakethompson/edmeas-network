### Packages -------------------------------------------------------------------
library(tidyverse)
library(tidygraph)
library(ggraph)
library(here)


### Data -----------------------------------------------------------------------
pubs <- read_rds(here("data/cleaned-pubs.rds"))
lookup <- read_rds(here("data/pub-lookup.rds"))


### Functions ------------------------------------------------------------------
make_connections <- function(x, lookup) {
  true_names <- tibble(cited_references = x) %>%
    left_join(lookup, by = "cited_references") %>%
    select(-cited_references) %>%
    rename(cited_references = new_label) %>%
    filter(!is.na(cited_references)) %>%
    pull(cited_references) %>%
    unique()
  
  crossing(cite1 = true_names, cite2 = true_names) %>%
    filter(cite1 != cite2) %>%
    filter(cite1 < cite2) %>%
    arrange(cite1) %>%
    distinct()
}


### Determine citations to include ---------------------------------------------
inc_pubs <- pubs %>%
  select(cited_references) %>%
  unnest() %>%
  left_join(lookup, by = "cited_references") %>%
  select(-cited_references) %>%
  rename(cited_references = new_label) %>%
  filter(!is.na(cited_references)) %>%
  count(cited_references) %>%
  arrange(desc(n)) %>%
  filter(n >= 15)

connections <- pubs %>%
  mutate(matches = map(cited_references, make_connections, lookup = lookup)) %>%
  select(matches) %>%
  unnest() %>%
  count(cite1, cite2) %>%
  arrange(desc(n)) %>%
  filter(n >= 14,
         cite1 %in% inc_pubs$cited_references,
         cite2 %in% inc_pubs$cited_references)

threshold <- 5
while(min(table(c(connections$cite1, connections$cite2))) < threshold) {
  bad_refs <- table(c(connections$cite1, connections$cite2)) %>%
    as_tibble(.name_repair = ~ c("name", "n")) %>%
    filter(n < threshold)
  
  connections <- connections %>%
    anti_join(bad_refs, by = c("cite1" = "name")) %>%
    anti_join(bad_refs, by = c("cite2" = "name"))
}


### Create network -------------------------------------------------------------
nodes <- connections %>%
  select(-n) %>%
  gather(key = "order", value = "name") %>%
  select(name) %>%
  distinct() %>%
  left_join(inc_pubs, by = c("name" = "cited_references")) %>%
  separate(name, into = c("last", "first", "year", "junk"), sep = ", ",
           extra = "merge", fill = "right", remove = FALSE) %>%
  mutate(author = paste0(last, ", ", first),
         citation = paste0(last, ", ", year)) %>%
  select(name, citation, author, first, last, year, n) %>%
  arrange(desc(n)) %>%
  rowid_to_column(var = "id")

edges <- connections %>%
  left_join(select(nodes, name, id), by = c("cite1" = "name")) %>%
  rename(from = id) %>%
  left_join(select(nodes, name, id), by = c("cite2" = "name")) %>%
  rename(to = id) %>%
  select(from, to, weight = n)

network <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE) %>%
  mutate(pagerank = centrality_pagerank(weights = weight, directed = FALSE),
         betweenness = centrality_betweenness(weights = weight,
                                              directed = FALSE),
         louvain = group_louvain(weights = weight),
         louvain = factor(louvain, levels = 1:7,
                          labels =c ("IRT", "DIF", "DCM", "SEM",
                                     "Dimensionality", "Bayesian",
                                     "Model fit")))

write_rds(network, here("data/network.rds"), compress = "gz")


### Descriptive information ----------------------------------------------------
total_pubs <- nrow(pubs)
cited_references <- pubs %>%
  select(cited_references) %>%
  unnest() %>%
  left_join(lookup, by = "cited_references") %>%
  select(-cited_references) %>%
  rename(cited_references = new_label) %>%
  filter(!is.na(cited_references)) %>%
  count(cited_references) %>%
  arrange(desc(n))
retained_pubs <- nrow(nodes)


neighborhood_size <- network %>%
  as_tibble() %>%
  group_by(louvain) %>%
  summarize(n = n()) %>%
  ungroup()

top_pubs <- network %>%
  as_tibble() %>%
  group_by(louvain) %>%
  top_n(5, wt = pagerank) %>%
  arrange(louvain, desc(pagerank)) %>%
  select(id, name, louvain) %>%
  ungroup()


### Circular layout ------------------------------------------------------------
ggraph(network, layout = "linear", circular = TRUE, sort.by = "louvain") +
  geom_edge_arc2(aes(alpha = weight, edge_color = node.louvain),
                 width = 1, show.legend = FALSE) +
  geom_node_point(aes(color = louvain, size = betweenness)) +
  scale_edge_alpha_continuous(range = c(0.2, 0.8)) +
  scale_color_OkabeIto() +
  scale_edge_color_manual(values = palette_OkabeIto[1:7]) +
  scale_size_area() +
  coord_fixed() +
  labs(color = NULL) +
  theme_graph() +
  theme(legend.position = "bottom",
        text = element_text(family = "Arial Narrow")) +
  guides(size = FALSE,
         color = guide_legend(byrow = TRUE, nrow = 1,
                              override.aes = list(size = 3)))




  

  
  

  
  
  ggraph(network, layout = "dh") +
    geom_edge_link2(aes(width = weight, alpha = weight, edge_color = node.louvain),
                    show.legend = FALSE) +
    geom_node_point(aes(fill = louvain, size = betweenness), shape = 21,
                    color = "black") +
    scale_alpha_continuous(range = c(0, 0.5)) +
    scale_fill_OkabeIto() +
    scale_edge_color_manual(values = palette_OkabeIto[1:7]) +
    theme_graph() +
    guides(size = FALSE, fill = guide_legend(override.aes = list(size = 2)))
  
#nicely
#dh
#graphopt
#fr
#lgl

starting_coords <- matrix(data = runif(nrow(nodes) * 2, min = -100, max = 100),
                          ncol = 2)

plots <- map(seq_len(20), function(x, network) {
  ret_plot <- ggraph(network, layout = "dh") +
    geom_edge_link2(aes(width = weight, alpha = weight, edge_color = node.louvain),
                    show.legend = FALSE) +
    geom_node_point(aes(fill = louvain, size = betweenness), shape = 21,
                    color = "black") +
    scale_alpha_continuous(range = c(0, 0.5)) +
    scale_edge_color_manual(values = palette_OkabeIto[1:6]) +
    theme_graph() +
    guides(size = FALSE, fill = guide_legend(override.aes = list(size = 2)))
  
  return(ret_plot)
}, network = network)

iwalk(plots, function(plot, index) {
  ggsave(filename = glue("layout_{index}.png"), plot = plot,
         path = here("layouts"), width = 8, height = 8, units = "in",
         dpi = "retina")
})
