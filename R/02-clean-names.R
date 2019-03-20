library(tidyverse)
library(here)
library(stringdist)

pubs <- read_rds(here("data/cleaned-pubs.rds"))

author_cap <- function(x) {
  name_vec <- flatten_chr(str_split(x, " "))
  
  if (length(name_vec) == 1) return(name_vec)
  
  initials <- name_vec[length(name_vec)]
  last_name <- name_vec[-length(name_vec)]
  
  last_name <- paste(toupper(str_sub(last_name, 1, 1)),
                     tolower(str_sub(last_name, 2, -1)),
                     sep = "", collapse = " ")
  
  paste(paste0(last_name, ","), initials)
}
author_cap <- Vectorize(author_cap)

pub_list <- pubs %>%
  select(cited_references) %>%
  unnest() %>%
  distinct() %>%
  separate(cited_references, into = c("author", "year", "junk"), sep = ", ",
           extra = "merge", fill = "right", remove = FALSE) %>%
  mutate(author = str_replace_all(author, fixed("."), ""),
         author = str_replace_all(author, fixed("*"), ""),
         author = str_replace(author, "(?<=\\b\\w)\\s(?=\\w\\b)", ""),
         author = author_cap(author),
         author = case_when(author == "American Psychological, Association" ~
                              "American Psychological Association",
                            author == "De La Torre, J" ~ "de la Torre, J",
                            author == "Rubin Donald, B" ~ "Rubin, DB",
                            author == "Van Der Linden, WJ" ~
                              "van der Linden, WJ",
                            author == "Van Krimpen-stoop, EMLA" ~
                              "van Krimpen-Stoop, EMLA",
                            author == "Von Davier, M" ~ "von Davier, M",
                            author == "Kunina-habenicht, O" ~
                              "Kunina-Habenicht, O",
                            author == "Vacha-haase, T" ~ "Vacha-Haase, T",
                            author == "Ackerman, T" ~ "Ackerman, TA",
                            author == "Embretson, S" ~ "Embretson, SE",
                            TRUE ~ author),
         spaces = str_count(author, " "),
         hyphens = str_count(author, "-")) %>%
  filter(!str_detect(author, "Department"), !str_detect(author, "Dep, ED"),
         !str_detect(author, "Dep Ed"), !str_detect(author, "Us Dep"),
         !str_detect(author, "Mo Dep High"), !str_detect(author, "Cal Dep"),
         !str_detect(author, "Minn Dep"), !str_detect(author, "Wisc Dep"),
         !str_detect(author, "^[0-9]"), author != "[Anonymous]") %>%
  separate(author, into = c("last", "first"), remove = FALSE, sep = ", ",
           fill = "right") %>%
  separate(first, into = c("init1", "rest"), remove = FALSE, sep = 1,
           extra = "merge") %>%
  group_split(last, init1)

pub_list <- map_dfr(pub_list, function(x) {
  if (nrow(x) == 1) {
    final <- x %>%
      mutate(new_label = cited_references) %>%
      select(cited_references, new_label)
    return(final)
  }
  step_1 <- x %>%
    mutate(new_junk = junk,
           new_junk = str_replace_all(new_junk, "CSE U CAL NAT CT", "CRESST"),
           new_junk = str_replace_all(new_junk, "CRESST U CAL", "CRESST"),
           new_junk = str_replace_all(new_junk, "CSE CRESST", "CRESST"),
           new_junk = str_replace_all(new_junk, "CSE U CAL GRAD S", "CRESST"),
           new_junk = str_replace_all(new_junk, "CSE U CAL NAT CT", "CRESST"),
           new_junk = str_replace_all(new_junk, "CSE UCLA CTR STU", "CRESST"),
           new_junk = str_replace_all(new_junk, "CSE NAT CTR RES", "CRESST"),
           new_junk = str_replace_all(new_junk, "CSE U CAL CTR ST", "CRESST"),
           new_junk = str_replace_all(new_junk, "CSE U CAL NAT RE", "CRESST"),
           new_junk = str_replace_all(new_junk, "CSE", "CRESST"),
           new_junk = case_when(str_detect(new_junk, "^CRESST[0-9]+$") ~
                                  paste(
                                    str_replace_all(new_junk, "[^[:digit:]]", ""),
                                    "CRESST"
                                  ),
                                str_detect(new_junk, "^THESIS") ~ "THESIS",
                                str_detect(new_junk, "^DISS") ~ "DISSERTATION",
                                str_detect(new_junk, "^EQS") ~ "EQS",
                                TRUE ~ new_junk))
  
  combos <- crossing(a = step_1$new_junk, b = step_1$new_junk) %>%
    filter(a != b, a < b) %>%
    mutate(sim = stringsim(a, b, method = "dl"),
           a_vol = str_extract(a, "V[0-9]+"),
           b_vol = str_extract(b, "V[0-9]+")) %>%
    filter(sim >= 0.8, is.na(a_vol) | is.na(b_vol) | a_vol == b_vol)
  
  if (nrow(combos) > 0) {
    combos <- combos %>%
      rowid_to_column(var = "match") %>%
      select(-sim, -a_vol, -b_vol) %>%
      gather(key = "label", value = "orig", -match) %>%
      arrange(match, label) %>%
      group_by(match) %>%
      mutate(new = min(orig)) %>%
      ungroup() %>%
      group_by(orig) %>%
      summarize(new = min(new)) %>%
      select(orig, new)
    
    step_2 <- left_join(step_1, combos, by = c("new_junk" = "orig")) %>%
      mutate(new_junk = case_when(!is.na(new) ~ new, TRUE ~ new_junk)) %>%
      select(-new)
  } else {
    step_2 <- step_1
  }
  
  final <- step_2 %>%
    group_by(new_junk) %>%
    mutate(year = max(year), first = max(first)) %>%
    ungroup() %>%
    mutate(new_label = paste0(last, ", ", first, ", ", year, ", ", new_junk)) %>%
    select(cited_references, new_label)
  
  return(final)
})

write_rds(pub_list, path = here("data/pub-lookup.rds"), compress = "gz")
