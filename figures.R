setwd("D:/Dropbox/Preprinting a pandemic/Code")

#setwd("C://Users//Liam//Documents//GitHub//preprint_changes")

# Load libraries and set theme ----
pacman::p_load("tidyverse", "colorspace", "lubridate", "patchwork", "corrr", "DescTools")

theme_set(theme_minimal() +
            theme(text = element_text(size = 12),
                  axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
                  axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
                  legend.key.size = unit(0.5, "cm"),
                  legend.text = element_text(size = 10),
                  panel.border = element_rect(color = "#E0E0E0", size = 0.5, fill = NA),
                  strip.background = element_rect(fill = "#FAFAFA", color = "#E0E0E0"),
                  panel.spacing = unit(2, "lines")))


# Data import -------------
# Files here have been combined & processed for use in creating figs. Raw analysis files can be found in the "Raw" folder in the repo
preprint_info <- read_csv("./data/preprint_info.csv") 
# Preprint metadata collected as part of Fraser et al (code: 10.5281/zenodo.4501924) 
main_body_changes <- read.csv("./data/main_body_changes.csv")
# Analysis of the changes in panels, tables and preprint-paper metadata 
abstract_scoring <- read_csv("./data/abstract_scoring.csv") %>% 
  filter(exclude == "keep") %>% # Retain only non-excluded abstracts
  mutate(calendar_date = as.numeric(as.Date("30/4/2020", format="%d/%m/%Y") - as.Date(posted_date, format="%d/%m/%Y"))) %>% # Calculate number of days each preprint had been online by latest preprint posting date
  select(-X1.x) 
# Analysis of abstracts using computational methods
preprint_full <- read_csv("./data/preprint_details.csv")
# Preprint metadata collected as part of Fraser et al (code: 10.5281/zenodo.4501924) 
panels <- read_csv("./data/panels_server.csv")
# Total panels broken down by server and COVID-status
rec_scores <- read_csv("./data/rec_scores.csv") %>% # Retain only non-excluded abstracts
  filter(doi %in% abstract_scoring$doi) 
# Granular analysis of abstract changes combined with the overall (Highest) score
abstract_scoring_reconciled <- read_csv("./data/reconciled_scores.csv")

# Altmetric data
# import data from original github
citations <- read_csv("https://raw.githubusercontent.com/preprinting-a-pandemic/pandemic_preprints/1716d724a0072bd63b30b9afed14d00ebd7dd932/data/preprint_citations_20190901_20200430.csv")
comments <- read_csv("https://raw.githubusercontent.com/preprinting-a-pandemic/pandemic_preprints/1716d724a0072bd63b30b9afed14d00ebd7dd932/data/preprint_comments_20190901_20200430.csv")  
altmetric <- read_csv("https://raw.githubusercontent.com/preprinting-a-pandemic/pandemic_preprints/1716d724a0072bd63b30b9afed14d00ebd7dd932/data/preprint_altmetrics_20190901_20200430.csv")

citations <- citations %>% select(doi:citations)
altmetric <- altmetric %>% select(doi:policies)

abstract_scoring_comments <- left_join(abstract_scoring, comments, by = "doi")
abstract_scoring_citations <- left_join(abstract_scoring, citations, by = "doi")
abstract_scoring_altmetrics <- left_join(abstract_scoring, altmetric, by = "doi")

# Define functions ------

# Automated abbreviations for journal titles
j_abbrev <- function(x) {
  x %>%
    gsub("Clinical Immunology", "CIM", .) %>% # Manual override to prevent Cll and ClI being indistinguishable
    gsub("[[:punct:] ]+| and | of | in |the |ournal|ical", " ", .) %>%
    abbreviate(minlength = 3)
}

# Dual heatmap triangles function, adapted from https://stackoverflow.com/a/65919234
make_triangles <- function(x, y, point = "up") {
  x <- as.integer(as.factor((x)))
  y <- as.integer(as.factor((y)))
  if (point == "up") {
    newx <- sapply(x, function(x) {
      c(x - 0.5, x - 0.5, x + 0.5)
    }, simplify = FALSE)
    newy <- sapply(y, function(y) {
      c(y - 0.5, y + 0.5, y + 0.5)
    }, simplify = FALSE)
  } else if (point == "down") {
    newx <- sapply(x, function(x) {
      c(x - 0.5, x + 0.5, x + 0.5)
    }, simplify = FALSE)
    newy <- sapply(y, function(y) {
      c(y - 0.5, y - 0.5, y + 0.5)
    }, simplify = FALSE)
  }
  data.frame(x = unlist(newx), y = unlist(newy))
}

# Figure 1 --------------

F1A <- preprint_full %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "Non-COVID article"
  )) %>% 
  filter(posted_date >= as.Date("2020-01-01")) %>% 
  count(is_published, covid_preprint) %>% 
  group_by(covid_preprint) %>%
  mutate(proportion = (n/sum(n)) * 100) %>% 
  filter(is_published == T) %>%
  ggplot(aes(x = covid_preprint, y = proportion, fill = covid_preprint))+
  geom_col(position = "dodge") +
  labs(y ="Percentage of preprints published \n between 1st Jan - 30 April", 
       x = "") +
  geom_text(aes(label=n, group = covid_preprint), color = "white", size=4, position = position_stack(vjust = .5)) +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

F1B <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    peer_review = factor(peer_review, 
                         levels = c(0:1),
                         labels = c("No",
                                    "Yes"))) %>%
  count(covid_preprint, peer_review) %>%
  group_by(covid_preprint) %>%
  mutate(proportion = (n/sum(n))*100) %>% 
  ungroup %>%
  ggplot(aes(x = peer_review, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "% of published preprints with transparent journal peer review", y = "% of preprints", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  theme(legend.position = "none")

F1C <-   main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    source_data = factor(case_when(source_data == -2 ~ "Less accessible",
                                   source_data == -1  ~ "Upon request",
                                   source_data == 0 ~ "Supplemental or repository",
                                   source_data == 1 ~ "More available",
                                   is.na(source_data) ~ "NA"),
                         levels = c("Less accessible",  "Upon request", "Supplemental or repository", "More available", "NA"))) %>%
  count(covid_preprint, source_data) %>%
  group_by(covid_preprint) %>%
  mutate(proportion = (n/sum(n))*100) %>% 
  ungroup %>% 
  ggplot(aes(x = source_data, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Data availability after publication", y = "Percentage of articles", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  theme(legend.position = "none")

F1D <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    author_list = factor(author_list,
                         levels = c(0:5),
                         labels = c("no change",
                                    "Author added",
                                    "Author removed",
                                    "Change in corresponding author",
                                    "Added & corresponding author change",
                                    "Removed & corresponding author change"))) %>%
  count(covid_preprint, author_list, .drop = FALSE) %>%
  group_by(covid_preprint) %>% 
  mutate(proportion = (n/sum(n))*100) %>%
  ungroup() %>% 
  filter(author_list != "Removed & corresponding author change") %>%
  ggplot(aes(x = author_list, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Change in authors after publication", y = "% of preprints", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  theme(legend.position = "none")

F1E <-  abstract_scoring %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  count(published_journal, covid_preprint) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1) %>% 
  ggplot(aes(x = published_journal, y = n, fill = covid_preprint)) +
  geom_col() +
  labs(y = "Number of preprints \n subsequently published (>1)", 
       x = "Journal title",
       caption = "journals with >1 published preprints only",
       fill = "", 
       color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = qualitative_hcl(2, palette = "Set2")) +
  theme(legend.position = "bottom") 


# Patchwork
layout <- "
AABB
CCDD
EEEE
"  

Fig_1 <- F1A + F1B + F1C + F1D + F1E +
  plot_layout(design = layout) +
  # plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

Fig_1 + 
ggsave("./figures/Fig_1.png", width = 10, height = 12)




# Supp Fig 1 ---------

SF1A <- preprint_full %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "Non-COVID article"
  )) %>% 
  filter(posted_date >= as.Date("2020-01-01")) %>% 
  count(is_published, covid_preprint, source) %>% 
  group_by(covid_preprint, source) %>%
  mutate(proportion = (n/sum(n)) * 100) %>% 
  filter(is_published == T) %>%
  ggplot(aes(x = covid_preprint, y = proportion, fill = covid_preprint))+
  geom_col(position = "dodge") +
  labs(y ="Percentage of preprints published \n between 1st Jan - 30 April", 
       x = "COVID article") +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  facet_wrap(~source)

SF1B <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    peer_review = factor(peer_review, 
                         levels = c(0:1),
                         labels = c("No",
                                    "Yes"))) %>%
  count(covid_preprint, peer_review, source) %>%
  complete(covid_preprint, peer_review, source, fill = list(n = 0)) %>%
  group_by(covid_preprint) %>%
  mutate(proportion = (n/sum(n))*100) %>%
  ungroup %>%
  ggplot(aes(x = peer_review, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Transparent peer-review", y = "% of preprints", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  theme(legend.position = "none") +
  facet_wrap(~source)

SF1C <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    source_data = factor(case_when(source_data == -2 ~ "Less accessible",
                                   source_data == -1  ~ "Upon request",
                                   source_data == 0 ~ "Supplemental or repository",
                                   source_data == 1 ~ "More available",
                                   is.na(source_data) ~ "NA"),
                         levels = c("Less accessible",  "Upon request", "Supplemental or repository", "More available", "NA"))) %>%
  count(covid_preprint, source_data, source) %>%
  complete(covid_preprint, source_data, source, fill = list(n = 0)) %>%
  group_by(covid_preprint) %>%
  mutate(proportion = (n/sum(n))*100) %>%
  ungroup %>% 
  ggplot(aes(x = source_data, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Data availability", y = "Percentage of articles", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  theme(legend.position = "none") +
  facet_wrap(~source)

SF1D <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    author_list = factor(author_list,
                         levels = c(0:5),
                         labels = c("no change",
                                    "Author added",
                                    "Author removed",
                                    "Change in corresponding author",
                                    "Added & corresponding author change",
                                    "Removed & corresponding author change"))) %>%
  count(covid_preprint, author_list, source) %>%
  complete(covid_preprint, author_list, source, fill = list(n = 0)) %>%
  filter(author_list != "Removed & corresponding author change") %>%
  group_by(covid_preprint) %>% 
  mutate(proportion = (n/sum(n))*100) %>%
  ungroup() %>% 
  ggplot(aes(x = author_list, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Change in authors", y = "% of preprints", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  theme(legend.position = "none") +
  facet_wrap(~source)

SF1E <- abstract_scoring %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(source == "biorxiv") %>% 
  count(published_journal, covid_preprint) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1) %>% 
  ggplot(aes(x = published_journal, y = n, fill = covid_preprint)) +
  geom_col() +
  labs(y = "Number of bioRxiv preprints \n subsequently published (>1)", 
       x = "Journal title") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = qualitative_hcl(2, palette = "Set2")) +
  theme(legend.position = "none") 

SF1F <- abstract_scoring %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(source == "medrxiv") %>% 
  count(published_journal, covid_preprint) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1) %>% 
  ggplot(aes(x = published_journal, y = n, fill = covid_preprint)) +
  geom_col() +
  labs(y = "Number of medRxiv preprints \n subsequently published (>1)", 
       x = "Journal title",
       color = "",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = qualitative_hcl(2, palette = "Set2")) +
  theme(legend.position = "bottom") 

# Patchwork
layout_2 <- "
AABB
CCDD
EEFF
"  

S_Fig_1 <- SF1A + SF1B + SF1C + SF1D + SF1E + SF1F +
  plot_layout(design = layout_2) +
  plot_layout(guides = "auto") +
  plot_annotation(tag_levels = "A") #&
theme(legend.position = 'bottom')

S_Fig_1 + 
ggsave("./figures/S_Fig_1.png", width = 10, height = 12)



# Figure 2 ------------
F2A <-  panels %>%   
  mutate(preprint_paper = factor(preprint_paper,
                                 levels = c("COVID preprint", "COVID paper", "Non-COVID preprint", "Non-COVID paper"))) %>%
  ggplot(aes(x = preprint_paper, y = main_total, fill = preprint_paper)) +
  geom_jitter(aes(color = preprint_paper), shape = 21, size = 0.6, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, notch=TRUE) +
  labs(x = "", y = "total panels & tables", fill = "") +
  scale_fill_manual(values = rep(qualitative_hcl(2, palette = "Set2"), each = 2)) +
  scale_color_manual(values = rep(qualitative_hcl(2, palette = "Set2"), each = 2)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(text = element_text(size = 16)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "Total panels and tables") +
  guides(color = FALSE) 

F2B <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  ggplot(aes(x=covid_preprint, y = panel_change, color = covid_preprint, fill = covid_preprint)) +
  geom_dotplot(binaxis = "y", binpositions="all", binwidth=1, stackdir="center", dotsize=0.7) +
  theme_minimal() +
  scale_color_manual(values = qualitative_hcl(2, palette = "Set2")) +
  theme(legend.position = "none") +
  labs(x = "", y = "Difference in number\nof panels and tables")
#  ggsave("./figures/main_panels_change.png", height = 8, width = 10, dpi = 300)

F2C <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    change_outcomes = factor(change_outcomes, 
                             levels = c(0:4),
                             labels = c("No real change", 
                                        "Figures rearranged", 
                                        "Significant content added", 
                                        "Significant content removed", 
                                        "Content added and removed"))) %>%
  count(covid_preprint, change_outcomes) %>%
  group_by(covid_preprint) %>%
  mutate(proportion = (n/sum(n))*100) %>%
  ungroup %>% 
  ggplot(aes(x = change_outcomes, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Main body change", y = "% of preprints", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +  
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Classification of change", y = "Percentage of articles") +
  #  theme(text = element_text(size = 16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) 

F2D <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"), 
    change_outcomes = factor(change_outcomes, 
                             levels = c(0:4),
                             labels = c("No real change", 
                                        "Figures rearranged", 
                                        "Significant content added", 
                                        "Significant content removed", 
                                        "Content added and removed")),
    journal_short = j_abbrev(published_journal)) %>% 
  count(journal_short, covid_preprint, change_outcomes) %>% 
  arrange(desc(n)) %>% 
  filter(covid_preprint == "COVID article") %>% 
  ggplot(aes(x = journal_short, y = n, fill = change_outcomes)) +
  geom_col() +
  labs(y = "Number of COVID-19 preprints \n subsequently published", 
       x = "Journal title",
       fill = "Figures change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = qualitative_hcl(5, palette = "Set2")) +
  theme(legend.position = "none")

F2E <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"), 
    change_outcomes = factor(change_outcomes, 
                             levels = c(0:4),
                             labels = c("No real change", 
                                        "Figures rearranged", 
                                        "Significant content added", 
                                        "Significant content removed", 
                                        "Content added and removed")),
    journal_short = j_abbrev(published_journal)) %>% 
  count(journal_short, covid_preprint, change_outcomes) %>% 
  arrange(desc(n)) %>% 
  filter(covid_preprint == "non-COVID article") %>% 
  ggplot(aes(x = journal_short, y = n, fill = change_outcomes)) +
  geom_col() +
  labs(y = "Number of non-COVID-19 preprints \n subsequently published", 
       x = "Journal title",
       fill = "Figures change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = qualitative_hcl(5, palette = "Set2")) +
  theme(legend.position = "bottom")

main_body_changes %>%
  mutate(journal_short = j_abbrev(published_journal)) %>% 
  select(published_journal, journal_short) %>%
  distinct() %>%
  arrange(journal_short) %>%
  write.csv("journal_key.csv")


# Patchwork
layout_F3 <- "
AABB
CCCC
DDDD
EEEE
"  

Fig_2 <- F2A + F2B + F2C + F2D + F2E +
  plot_layout(design = layout_F3) +
  # plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

Fig_2 + 
ggsave("./figures/Fig_2.png", width = 10, height = 12)

# Supp Fig 2 ----
SF2A <-  panels %>%   
  mutate(preprint_paper = factor(preprint_paper,
                                 levels = c("COVID preprint", "COVID paper", "Non-COVID preprint", "Non-COVID paper"))) %>%
  ggplot(aes(x = preprint_paper, y = main_total, fill = preprint_paper)) +
  geom_jitter(aes(color = preprint_paper), shape = 21, size = 0.6, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, notch=TRUE) +
  labs(x = "", y = "total panels & tables", fill = "") +
  scale_fill_manual(values = rep(qualitative_hcl(2, palette = "Set2"), each = 2)) +
  scale_color_manual(values = rep(qualitative_hcl(2, palette = "Set2"), each = 2)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(size = 6)) +
  labs(x = "", y = "Total panels and tables") +
  guides(color = FALSE) +
  facet_wrap(~source)

SF2B <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  ggplot(aes(y = panel_change, x = covid_preprint, color = covid_preprint, fill = covid_preprint)) +
  geom_dotplot(binaxis = "y", binpositions="all", binwidth=1, stackdir="center", dotsize=0.24) +
  theme_minimal() +
  scale_color_manual(values = qualitative_hcl(2, palette = "Set2")) +
  theme(legend.position = "none") +
  labs(x = "COVID article", y = "Difference in number of panels and tables") +
  facet_wrap(~source)

SF2C <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    change_outcomes = factor(change_outcomes, 
                             levels = c(0:4),
                             labels = c("No real change", 
                                        "Figures rearranged", 
                                        "Significant content added", 
                                        "Significant content removed", 
                                        "Content added and removed"))) %>%
  count(covid_preprint, change_outcomes, source) %>%
  group_by(covid_preprint) %>%
  mutate(proportion = (n/sum(n))*100) %>%
  ungroup %>% 
  ggplot(aes(x = change_outcomes, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Main body change", y = "% of preprints", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +  
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Classification of change", y = "Percentage of articles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  facet_wrap(~source)

# Patchwork
S_Fig_2 <- (SF2A + SF2B) / SF2C +
  # plot_layout(ncol = 2) +
  # plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

S_Fig_2 + 
ggsave("./figures/S_Fig_2.png", width = 10, height = 12)

# Figure 3 -------------

F3A <- abstract_scoring %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(covid_preprint != "NA") %>%
  ggplot(aes(x= covid_preprint, y = `difflib standard change_ratio`, fill = covid_preprint)) +
  geom_jitter(aes(color = covid_preprint), shape = 21, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, notch=TRUE) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "Change ratio (difflib)") 

F3B <-  abstract_scoring %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(covid_preprint != "NA") %>%
  ggplot(aes(x= covid_preprint, y = Word_change_ratio, fill = covid_preprint)) +
  geom_jitter(aes(color = covid_preprint), shape = 21, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, notch=TRUE) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "Change ratio (Microsoft Word)")  

F3C <- abstract_scoring %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(Highest_change != "NA") %>%
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/ \n softening, minor", "Major \n conclusion change"))) %>% 
  count(Highest_change, covid_preprint) %>% 
  ggplot(aes(x = Highest_change, y = n, fill = covid_preprint)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  labs(x = "Scored change", y = "Number of scored abstracts") 

# Prepare and plot dual heatmap, code adapted from https://stackoverflow.com/a/65919234
mydat_wide <- abstract_scoring %>% 
  select(covid_preprint, `1+_annotations`, `1-_annotations`) %>%
  mutate(covid_preprint = case_when(
  covid_preprint == T ~ "COVID article",
  covid_preprint == F ~ "non-COVID article")) %>% 
  filter(covid_preprint != "NA") %>% 
  group_by(covid_preprint, `1+_annotations`, `1-_annotations`) %>%
  count() %>%
  pivot_wider(names_from = "covid_preprint", values_from = "n")
newcoord_up <- make_triangles(mydat_wide$`1+_annotations`, mydat_wide$`1-_annotations`)
newcoord_down <- make_triangles(mydat_wide$`1+_annotations`, mydat_wide$`1-_annotations`, point = "down")
newcoord_down <- newcoord_down %>% select(xdown = x, ydown = y)
repdata <- map_df(1:nrow(mydat_wide), function(i) mydat_wide[rep(i, 3), ])
newdata <- bind_cols(repdata, newcoord_up, newcoord_down)

F3D <- ggplot(newdata) +
  geom_polygon(aes(x = x, y = y, fill = `COVID article`, group = interaction(`1-_annotations`, `1+_annotations`)), color = "black") +
  scale_fill_gradient(low = "#fce5e3", high = "#F8766D", na.value = "white", limits = c(0, 30)) +
  ggnewscale::new_scale_fill() +
  geom_polygon(aes(x = xdown, y = ydown, fill = `non-COVID article`, group = interaction(`1-_annotations`, `1+_annotations`)), color = "black") +
  scale_fill_gradient(low = "#d6feff", high = "#00BFC4", na.value = "white", limits = c(0, 50)) +
  geom_text(data=newdata %>% group_by(`1+_annotations`, `1-_annotations`, `COVID article`, `non-COVID article`) %>% slice(1), 
            aes(label=`COVID article`, x=x+0.25, y=y+0.75)) +
  geom_text(data=newdata %>% group_by(`1+_annotations`, `1-_annotations`, `COVID article`, `non-COVID article`) %>% slice(1), 
            aes(label=`non-COVID article`, x=x+0.75, y=y+0.25)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq_along(unique(mydat_wide$`1+_annotations`)),
                     labels = seq_along(unique(mydat_wide$`1+_annotations`))-1) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq_along(unique(mydat_wide$`1-_annotations`)),
                     labels = seq_along(unique(mydat_wide$`1-_annotations`))-1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  labs(x = "Sum of positive scores", y = "Sum of negative scores", size="") 

F3E <- rec_scores %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>%
  mutate(section = factor(section, 
                          levels = c("context", "results", "conclusions"))) %>%
  count(section, covid_preprint) %>%
  group_by(covid_preprint) %>% 
  mutate(proportion = (n/sum(n))*100) %>% 
  filter(section != "results/context") %>%
  filter(section != "new") %>%
  ungroup() %>% 
  ggplot(aes(x = section, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Location of change", y = "% of annotations in abstracts", color = "", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  guides(color = FALSE)

F3F <-   rec_scores %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>%
  count(`label+modifier`, covid_preprint) %>%
  group_by(covid_preprint) %>% 
  mutate(proportion = (n/sum(n))*100) %>% 
  filter(covid_preprint != "NA") %>% 
  ungroup() %>% 
  ggplot(aes(x = `label+modifier`, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Type of change", y = "% of annotations in abstracts", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") +
  guides(color = FALSE) #+

# Patchwork
Fig_3 <- F3A + F3B + F3C + F3D + F3E + F3F +
  plot_layout(ncol = 2) +
  #plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") 

Fig_3 +
ggsave("./figures/Fig_3.png", width = 10, height = 12)

# Supp Fig 3 ------

SF3A <-  abstract_scoring %>% 
  group_by(Highest_change) %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>% 
  filter(covid_preprint != "NA") %>%
  ggplot(aes(x= Highest_change, y = `difflib standard change_ratio`, fill = covid_preprint)) +
  geom_point(aes(color = covid_preprint), shape = 21, alpha = 0.6, size = 0.6, position=position_jitterdodge(jitter.width = 0.3, dodge.width=0.75)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "scored change", y = "Change ratio", fill = "", color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

SF3B <-  abstract_scoring %>%  
  group_by(Highest_change) %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>%
  filter(covid_preprint != "NA") %>%
  ggplot(aes(x= Highest_change, y = Word_change_ratio, fill = covid_preprint)) +
  geom_point(aes(color = covid_preprint), shape = 21, alpha = 0.6, size = 0.6, position=position_jitterdodge(jitter.width = 0.3, dodge.width=0.75)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Scored change", y = "Word change ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Prepare and plot dual heatmap, code adapted from https://stackoverflow.com/a/65919234
mydat_wide <- abstract_scoring %>% 
  group_by(Highest_change) %>%
  filter(Highest_change != 0) %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("1", "2"),
                                 labels = c("Strengthening/softening, minor", "Major conclusion change"))) %>%
  filter(covid_preprint != "NA")  %>% 
  group_by(Highest_change, covid_preprint, `1+_annotations`, `1-_annotations`) %>%
  count() %>%
  pivot_wider(names_from = "Highest_change", values_from = "n")
newcoord_up <- make_triangles(mydat_wide$`1+_annotations`, mydat_wide$`1-_annotations`)
newcoord_down <- make_triangles(mydat_wide$`1+_annotations`, mydat_wide$`1-_annotations`, point = "down")
newcoord_down <- newcoord_down %>% select(xdown = x, ydown = y)
repdata <- map_df(1:nrow(mydat_wide), function(i) mydat_wide[rep(i, 3), ])
newdata <- bind_cols(repdata, newcoord_up, newcoord_down)

SF3C <- ggplot(newdata) +
  geom_polygon(aes(x = x, y = y, fill = `Strengthening/softening, minor`, group = interaction(`1-_annotations`, `1+_annotations`)), color = "black") +
  scale_fill_gradient(low = "#d3f5c6", high = "#7eba68", na.value = "white", limits = c(0, 14)) +
  ggnewscale::new_scale_fill() +
  geom_polygon(aes(x = xdown, y = ydown, fill = `Major conclusion change`, group = interaction(`1-_annotations`, `1+_annotations`)), color = "black") +
  scale_fill_gradient(low = "#c9e7ff", high = "#6fb1e7", na.value = "white", limits = c(0, 3)) +
  geom_text(data=newdata %>% group_by(`covid_preprint`, `1+_annotations`, `1-_annotations`, `Strengthening/softening, minor`, `Major conclusion change`) %>% slice(1), 
            aes(label=`Strengthening/softening, minor`, x=x+0.25, y=y+0.75),
            size = 2) +
  geom_text(data=newdata %>% group_by(`covid_preprint`, `1+_annotations`, `1-_annotations`, `Strengthening/softening, minor`, `Major conclusion change`) %>% slice(1), 
            aes(label=`Major conclusion change`, x=x+0.75, y=y+0.25),
            size = 2) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq_along(unique(mydat_wide$`1+_annotations`)),
                     labels = seq_along(unique(mydat_wide$`1+_annotations`))-1) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq_along(unique(mydat_wide$`1-_annotations`)),
                     labels = seq_along(unique(mydat_wide$`1-_annotations`))-1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  labs(x = "Sum of positive scores", y = "Sum of negative scores", size="") +
  facet_wrap(~covid_preprint)

SF3D <- rec_scores %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>%
  filter(covid_preprint != 'NA') %>% 
  filter(Highest_change != "0") %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>%
  mutate(section = factor(section, 
                          levels = c("context", "results", "conclusions"))) %>% 
  count(section, Highest_change, covid_preprint) %>%
  group_by(covid_preprint, section) %>% 
  mutate(proportion = (n/sum(n))*100) %>% 
  filter(section != "results/context") %>%
  ggplot(aes(x = section, y = proportion, fill = Highest_change, color = Highest_change)) +
  geom_col(position = "stack", color = "grey50", size = 0.25) +
  geom_text(aes(label=n, group = Highest_change), color = "white", size=4, position = position_stack(vjust = .5)) +
  facet_wrap(~covid_preprint) +
  labs(x = "Location of change", y = "Percentage of annotations \n in abstracts", fill = "", color = "") +
  theme_minimal() +
  scale_fill_manual(values = c("#7EBA68","#6FB1E7")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") +
  guides(color = FALSE)

SF3E <- rec_scores %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>%
  filter(covid_preprint != 'NA') %>% 
  filter(Highest_change != "0") %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>%
  mutate(section = factor(section, 
                          levels = c("context", "results", "conclusions"))) %>%
  count(`label+modifier`, Highest_change, covid_preprint) %>%
  group_by(covid_preprint, `label+modifier`) %>% 
  mutate(proportion = (n/sum(n))*100) %>% 
  filter(Highest_change != "NA") %>%
  ggplot(aes(x = `label+modifier`, y = proportion, fill = Highest_change, color = Highest_change)) +
  geom_col(position = "stack", color = "grey50", size = 0.25) +
  geom_text(aes(label=n, group = Highest_change), color = "white", size=4, position = position_stack(vjust = .5)) +
  facet_wrap(~covid_preprint) +
  labs(x = "Type of change", y = "Percentage of annotations \n in abstracts", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = c("#7EBA68","#6FB1E7")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

SF3F <- abstract_scoring %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>% 
  ggplot(aes(x = delay_in_days, fill = Highest_change, color = Highest_change)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.2) +
  labs(y = "Count of pairs", 
       x = "Delay (days)",
       fill = "Abstract change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = qualitative_hcl(3, palette = "Set2")) +
  theme(legend.position = "none") +
  facet_wrap(~covid_preprint)

SF3G <- abstract_scoring %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(covid_preprint == "COVID article") %>%
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>% 
  count(published_journal, covid_preprint, Highest_change) %>% 
  arrange(desc(n)) %>% 
  #  filter(n > 1) %>% 
  ggplot(aes(x = j_abbrev(published_journal), y = n, fill = Highest_change)) +
  geom_col() +
  labs(y = "Number of COVID-19 preprints \n subsequently published", 
       x = "Journal title",
       fill = "Abstract change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = qualitative_hcl(3, palette = "Set2")) +
  theme(legend.position = "bottom") 

# Patchwork
layout_SF3 <- "
AABB
CCDD
EEFF
GGGG
"  


S_Fig_3 <- SF3A + SF3B + SF3C + SF3D + SF3E + SF3F + SF3G +
  plot_layout(design = layout_SF3) +
  #  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") 

S_Fig_3 + 
  ggsave("./figures/S_Fig_3.png", width = 10, height = 12)

# Fig 4 -----
F4A <- abstract_scoring_altmetrics %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>% 
  filter(twitter >= 2) %>%
  ggplot(aes(x = Highest_change, y = twitter, fill = Highest_change)) +
  geom_jitter(aes(colour = Highest_change), shape = 21, size = 0.6, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  labs(y = "Tweets (>=2)", 
       x = "Degree of change",
       fill = "Abstract change") +
  scale_y_log10(limits = c(1, 1e5), expand = c(0, 0)) +
  scale_fill_manual(values = qualitative_hcl(3, palette = "Set2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") #+
facet_wrap(~covid_preprint)

F4B <- main_body_changes %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    change_outcomes = factor(change_outcomes, 
                             levels = c(0:4),
                             labels = c("No real change", 
                                        "Figures rearranged", 
                                        "Significant content added", 
                                        "Significant content removed", 
                                        "Content added and removed"))) %>% 
  filter(twitter >= 2) %>%
  ggplot(aes(x = change_outcomes, y = twitter, fill = change_outcomes)) +
  geom_jitter(aes(colour = change_outcomes), shape = 21, size = 0.6, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  labs(y = "Tweets (>=2)", 
       x = "Change in figures",
       fill = "") +
  scale_fill_manual(values = qualitative_hcl(5, palette = "Set2")) +
  theme_minimal() +
  scale_y_log10(limits = c(1, 1e5), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") #+
facet_wrap(~covid_preprint)

F4C <- abstract_scoring_comments %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>% 
  filter(comments_count >= 1) %>%
  ggplot(aes(x = Highest_change, y = comments_count, fill = Highest_change)) +
  geom_jitter(aes(colour = Highest_change), shape = 21, size = 0.6, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  labs(y = "Comments (>=1)", 
       x = "Degree of change",
       fill = "Abstract change") +
  scale_fill_manual(values = qualitative_hcl(3, palette = "Set2")) +
  theme_minimal() +
  scale_y_log10(limits = c(1, 1e2), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") #+
facet_wrap(~covid_preprint)

F4D <- main_body_changes %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    change_outcomes = factor(change_outcomes, 
                             levels = c(0:4),
                             labels = c("No real change", 
                                        "Figures rearranged", 
                                        "Significant content added", 
                                        "Significant content removed", 
                                        "Content added and removed"))) %>% 
  filter(comments_count >= 1) %>%
  ggplot(aes(x = change_outcomes, y = comments_count, fill = change_outcomes, color = change_outcomes)) +
  geom_jitter(aes(colour = change_outcomes), shape = 21, size = 0.6, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, color = "black") +
  labs(y = "Comments (>=1)", 
       x = "Change in figures",
       fill = "") +
  scale_fill_manual(values = qualitative_hcl(5, palette = "Set2")) +
  scale_color_manual(values = qualitative_hcl(5, palette = "Set2")) +
  theme_minimal() +
  scale_x_discrete(drop = FALSE) +
  scale_y_log10(limits = c(1, 1e2), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") #+
facet_wrap(~covid_preprint)

F4E <- abstract_scoring_citations %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>% 
  filter(citations >= 1) %>%
  ggplot(aes(x = Highest_change, y = citations, fill = Highest_change, color = Highest_change)) +
  geom_jitter(aes(colour = Highest_change), shape = 21, size = 0.6, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, color = "black") +
  labs(y = "Citations (>=1)", 
       x = "Degree of change",
       fill = "Abstract change") +
  scale_y_log10(limits = c(1, 1e3), expand = c(0, 0)) +
  scale_fill_manual(values = qualitative_hcl(3, palette = "Set2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") #+
facet_wrap(~covid_preprint)

F4F <-  main_body_changes %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    change_outcomes = factor(change_outcomes, 
                             levels = c(0:4),
                             labels = c("No real change", 
                                        "Figures rearranged", 
                                        "Significant content added", 
                                        "Significant content removed", 
                                        "Content added and removed"))) %>% 
  filter(citations >= 1) %>%
  ggplot(aes(x = change_outcomes, y = citations, fill = change_outcomes, color = change_outcomes)) +
  geom_jitter(aes(colour = change_outcomes), shape = 21, size = 0.6, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, color = "black") +
  labs(y = "Citations (>=1)", 
       x = "Change in figures",
       fill = "") +
  scale_fill_manual(values = qualitative_hcl(5, palette = "Set2")) +
  theme_minimal() +
  scale_y_log10(limits = c(1, 1e3), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") #+
facet_wrap(~covid_preprint) 

Fig_4 <- F4A + F4B + F4C + F4D + F4E + F4F +
  plot_layout(ncol = 2) +
  #  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") 

Fig_4 +
  ggsave("./figures/Fig_4.png", height = 10, width = 12)

# Supp Figure 4 ------------- subgroup analysis for infectious disease/epidemiology categories of medRxiv

SF4A <- abstract_scoring %>% 
  filter(category == "infectious diseases" | category == "epidemiology") %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(covid_preprint != "NA") %>%
  ggplot(aes(x= covid_preprint, y = `difflib standard change_ratio`, fill = covid_preprint)) +
  geom_jitter(aes(color = covid_preprint), shape = 21, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, notch=FALSE) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "Change ratio (difflib)") 

SF4B <-  abstract_scoring %>% 
  filter(category == "infectious diseases" | category == "epidemiology") %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(covid_preprint != "NA") %>%
  ggplot(aes(x= covid_preprint, y = Word_change_ratio, fill = covid_preprint)) +
  geom_jitter(aes(color = covid_preprint), shape = 21, alpha = 0.6, width = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, notch=FALSE) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "Change ratio (Microsoft Word)")  

SF4C <- abstract_scoring %>% 
  filter(category == "infectious diseases" | category == "epidemiology") %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(Highest_change != "NA") %>%
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/ \n softening, minor", "Major \n conclusion change"))) %>% 
  count(Highest_change, covid_preprint) %>% 
  ggplot(aes(x = Highest_change, y = n, fill = covid_preprint)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  labs(x = "Scored change", y = "Number of scored abstracts") 

SF4D <- main_body_changes %>%
  filter(category == "infectious diseases" | category == "epidemiology") %>%
  filter(change_outcomes %in% c(0:2)) %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    change_outcomes = factor(change_outcomes, 
                             levels = c(0:2),
                             labels = c("No real change", 
                                        "Figures rearranged", 
                                        "Significant content added"))) %>%
  count(covid_preprint, change_outcomes, .drop = FALSE) %>%
  group_by(covid_preprint) %>%
  mutate(proportion = (n/sum(n))*100) %>%
  ungroup %>% 
  ggplot(aes(x = change_outcomes, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Main body change", y = "% of preprints", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +  
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Classification of change", y = "Percentage of articles") +
  #  theme(text = element_text(size = 16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) 

SF4E <- rec_scores %>%
  left_join(abstract_scoring %>% select(doi, category), by = "doi") %>%
  filter(category == "infectious diseases" | category == "epidemiology") %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>%
  mutate(section = factor(section, 
                          levels = c("context", "results", "conclusions"))) %>%
  count(section, covid_preprint) %>%
  group_by(covid_preprint) %>% 
  mutate(proportion = (n/sum(n))*100) %>% 
  filter(section != "results/context") %>%
  filter(section != "new") %>%
  ungroup() %>% 
  ggplot(aes(x = section, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Location of change", y = "% of annotations in abstracts", color = "", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  guides(color = FALSE)

SF4F <-   rec_scores %>%
  left_join(abstract_scoring %>% select(doi, category), by = "doi") %>%
  filter(category == "infectious diseases" | category == "epidemiology") %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>%
  count(`label+modifier`, covid_preprint, .drop=FALSE) %>%
  complete(`label+modifier`, covid_preprint, fill = list(n = 0)) %>%
  group_by(covid_preprint) %>% 
  mutate(proportion = (n/sum(n))*100) %>% 
  filter(covid_preprint != "NA" & !(is.na(`label+modifier`))) %>% 
  ungroup() %>% 
  ggplot(aes(x = `label+modifier`, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  labs(x = "Type of change", y = "% of annotations in abstracts", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") +
  guides(color = FALSE) #+

# Patchwork
S_Fig_4 <- SF4A + SF4B + SF4C + SF4D + SF4E + SF4F +
  plot_layout(ncol = 2) +
  #plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") 

S_Fig_4 + 
ggsave("./figures/S_Fig_4.png", width = 10, height = 12)



#### Statistical analysis ######

# Statistical power
# Calculate power for various effect sizes using sample of n = 184 for various tests
data.frame(broom::tidy(pwr::pwr.chisq.test(N = 184, w = c(0.1,0.3,0.5), df = 1, sig.level = 0.05)) %>% select(power),
           broom::tidy(pwr::pwr.r.test(n = 184, r = c(0.1,0.3,0.5), sig.level = 0.05)) %>% select(power)) %>%
  setNames(c("power_chisq", "power_corr"))

# Statistical analyses ----
# Percentage of preprints published
# Chi-square test of association
preprint_full %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "Non-COVID article"
  )) %>% 
  filter(posted_date >= as.Date("2020-01-01")) %>%
  with(., table(covid_preprint, is_published)) %>%
  chisq.test()

# Percentage of preprints published with transparent peer review
# Chi-square test of association
main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "Non-COVID article"
  )) %>% 
  with(., table(covid_preprint, peer_review)) %>%
  chisq.test()



# Data availability
# Fisher's exact test of association
set.seed()
main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    source_data = factor(case_when(source_data == -2 ~ "Less accessible",
                                   source_data == -1  ~ "Upon request",
                                   source_data == 0 ~ "Supplemental or repository",
                                   source_data == 1 ~ "More available",
                                   is.na(source_data) ~ "NA"),
                         levels = c("Less accessible",  "Upon request", "Supplemental or repository", "More available", "NA"))) %>%
  with(., table(covid_preprint, source_data)) %>%
  fisher.test(simulate.p.value = TRUE, B = 1000)  

# Author changes
# Chi-square test of association
main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    author_list = ifelse(author_list == 0, 0, 1)) %>%
  with(., table(covid_preprint, author_list)) %>%
  chisq.test()

# Chi-square test of association - additions versus other
main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article"),
    author_list = ifelse(author_list == 1|author_list == 4, 1, 0)) %>%
  with(., table(covid_preprint, author_list)) %>%
  chisq.test()



# Panels and tables numbers
# Mann-Whitney (vs COVID preprint)
panels %>%
  mutate(covid_preprint = case_when(                                    # Redefine COVID vs non-COVID articles
    grepl("^COVID", preprint_paper) == T ~ "COVID article",
    grepl("^COVID", preprint_paper) == F ~ "non-COVID article")) %>%
  filter(grepl("preprint", preprint_paper)) %>%                       # Select only preprints to prevent double accounting
  with(., wilcox.test(main_total ~ covid_preprint))

# Mann-Whitney (vs COVID paper)
panels %>%
  mutate(covid_preprint = case_when(                                    # Redefine COVID vs non-COVID articles
    grepl("^COVID", preprint_paper) == T ~ "COVID article",
    grepl("^COVID", preprint_paper) == F ~ "non-COVID article")) %>%
  filter(grepl("paper", preprint_paper)) %>%                       # Select only papers to prevent double accounting
  with(., wilcox.test(main_total ~ covid_preprint))

# Mann-Whitney (vs source, preprints only)
panels %>%
  filter(grepl("preprint", preprint_paper)) %>%                       # Select only preprints to prevent double accounting
  with(., wilcox.test(main_total ~ source))

# Mann-Whitney (vs source, papers only)
panels %>%
  filter(grepl("paper", preprint_paper)) %>%                       # Select only papers to prevent double accounting
  with(., wilcox.test(main_total ~ source))

# Panels and table change
# Mann-Whitney (vs COVID preprint)
main_body_changes %>%
  with(., wilcox.test(panel_change ~ covid_preprint))

# Fligner-Killeen's test for Homogeneity of Variance (vs COVID preprint)
main_body_changes %>%
  with(., fligner.test(panel_change ~ covid_preprint))

# Mann-Whitney (vs source)
main_body_changes %>%
  with(., wilcox.test(panel_change ~ source))

# Fligner-Killeen's test for Homogeneity of Variance (vs source)
main_body_changes %>%
  with(., fligner.test(panel_change ~ source))



# Degree of changes
# Mann-Whitney (difflib change ratio vs COVID article)
abstract_scoring %>%
  with(., wilcox.test(`difflib standard change_ratio` ~ covid_preprint))

# Mann-Whitney (Word change ratio vs COVID article)
abstract_scoring %>%
  with(., wilcox.test(Word_change_ratio ~ covid_preprint))

# Infectious disease/epi subgroup
abstract_scoring %>% 
  filter(category == "infectious diseases" | category == "epidemiology") %>%
  with(., wilcox.test(`difflib standard change_ratio` ~ covid_preprint))
abstract_scoring %>% 
  filter(category == "infectious diseases" | category == "epidemiology") %>%
  with(., wilcox.test(Word_change_ratio ~ covid_preprint))

# Compare automated versus manual change measures
# Kruskal-Wallis tests (all, COVID, non-COVID)
abstract_scoring %>% 
  select(`difflib standard change_ratio`, Word_change_ratio, `1+_annotations`, `1-_annotations`) %>%
  map_df(~ broom::tidy(kruskal.test(., 
                                    abstract_scoring %>% 
                                      pull(Highest_change))), .id = 'var')
# abstract_scoring %>% 
#   filter(covid_preprint == T) %>%
#   select(`difflib standard change_ratio`, Word_change_ratio, `1+_annotations`, `1-_annotations`) %>%
#   map_df(~ broom::tidy(kruskal.test(., 
#                                 abstract_scoring %>% 
#                                   filter(covid_preprint == T) %>% 
#                                   pull(Highest_change))), .id = 'var')
# abstract_scoring %>% 
#   filter(covid_preprint == F) %>%
#   select(`difflib standard change_ratio`, Word_change_ratio, `1+_annotations`, `1-_annotations`) %>%
#   map_df(~ broom::tidy(kruskal.test(., 
#                                     abstract_scoring %>% 
#                                       filter(covid_preprint == F) %>% 
#                                       pull(Highest_change))), .id = 'var')

# Post-hoc Dunn's tests (all, COVID, non-COVID)
dtt <- abstract_scoring %>% 
  select(`difflib standard change_ratio`, Word_change_ratio, `1+_annotations`, `1-_annotations`) %>%
  map_df(~ broom::tidy(dunn.test::dunn.test(., 
                                            abstract_scoring %>% 
                                              pull(Highest_change),
                                            method = "bonferroni") %>% as.data.frame %>% pull(P.adjusted)), .id = 'var')

# dttf <- abstract_scoring %>% 
#   filter(covid_preprint == T) %>%
#   select(`difflib standard change_ratio`, Word_change_ratio, `1+_annotations`, `1-_annotations`) %>%
#   map_df(~ broom::tidy(dunn.test::dunn.test(., 
#                                             abstract_scoring %>% 
#                                               filter(covid_preprint == T) %>% 
#                                               pull(Highest_change),
#                                             method = "bonferroni") %>% as.data.frame %>% pull(P.adjusted)), .id = 'var')
# 
# dttt <- abstract_scoring %>% 
#   filter(covid_preprint == F) %>%
#   select(`difflib standard change_ratio`, Word_change_ratio, `1+_annotations`, `1-_annotations`) %>%
#   map_df(~ broom::tidy(dunn.test::dunn.test(., 
#                                     abstract_scoring %>% 
#                                       filter(covid_preprint == F) %>% 
#                                       pull(Highest_change),
#                                     method = "bonferroni") %>% as.data.frame %>% pull(P.adjusted)), .id = 'var')



# Publishing delays
# Mann-Whitney (vs covid preprint)
abstract_scoring %>%
  with(., wilcox.test(delay_in_days ~ covid_preprint))

# Median and IQRs
abstract_scoring %>%
  group_by(covid_preprint) %>%
  summarise(median = median(delay_in_days), IQR = IQR(delay_in_days))

# Kruskal-Wallis (vs degree of change)
# COVID-19 articles
abstract_scoring %>%
  filter(covid_preprint == T) %>%
  with(., kruskal.test(delay_in_days, Highest_change))

# Non-COVID-19 articles
abstract_scoring %>%
  filter(covid_preprint == F) %>%
  with(., kruskal.test(delay_in_days, Highest_change))

# Post-hoc Dunn's test
abstract_scoring %>%
  filter(covid_preprint == F) %>%
  with(., dunn.test::dunn.test(delay_in_days, Highest_change, method = "bonferroni"))

# Median and IQRs
abstract_scoring %>%
  filter(covid_preprint == F) %>%
  group_by(Highest_change) %>%
  summarise(median = median(delay_in_days), IQR = IQR(delay_in_days))



# Usage (twitter, comments, citations): univariate tests
# Kruskal-Wallis tests (vs highest change, change outcomes)
abstract_scoring_altmetrics %>% 
  select(Highest_change, change_outcomes) %>%
  map_df(~ broom::tidy(kruskal.test(abstract_scoring_altmetrics %>% pull(twitter), .)), .id = 'var')

abstract_scoring_comments %>% 
  select(Highest_change, change_outcomes) %>%
  map_df(~ broom::tidy(kruskal.test(abstract_scoring_comments %>% pull(comments_count), .)), .id = 'var')

abstract_scoring_citations %>% 
  select(Highest_change, change_outcomes) %>%
  map_df(~ broom::tidy(kruskal.test(abstract_scoring_citations %>% pull(citations), .)), .id = 'var')

# Spearman's rank correlation (vs difflib change ratio, Word change ratio)
abstract_scoring_altmetrics %>% 
  select(`difflib standard change_ratio`, Word_change_ratio) %>%
  map_df(~ broom::tidy(cor.test(abstract_scoring_altmetrics %>% pull(twitter), ., method = "spearman")), .id = 'var')

abstract_scoring_comments %>% 
  select(`difflib standard change_ratio`, Word_change_ratio) %>%
  map_df(~ broom::tidy(cor.test(abstract_scoring_comments %>% pull(comments_count), ., method = "spearman")), .id = 'var')

abstract_scoring_citations %>% 
  select(`difflib standard change_ratio`, Word_change_ratio) %>%
  map_df(~ broom::tidy(cor.test(abstract_scoring_citations %>% pull(citations), ., method = "spearman")), .id = 'var')


# Regression analysis
# Set predictors
form_usage <- formula(. ~ as.factor(Highest_change) + as.factor(change_outcomes) + `difflib standard change_ratio` + Word_change_ratio + covid_preprint + delay_in_days + calendar_date)

# Twitter
# Poisson regression
twitter_poismod <- abstract_scoring_altmetrics %>%
  glm(formula = update(form_usage, twitter ~ .), data = ., family = "poisson")

# Negative binomial regression
twitter_nbmod <- abstract_scoring_altmetrics %>%
  MASS::glm.nb(formula = update(form_usage, twitter ~ .), data = ., control = glm.control(maxit = 100))

# Confirm negative binomial as better fitting model
AIC(twitter_nbmod, twitter_poismod)

# Model summary
twitter_nbmod %>% summary()
# LRT test
drop1(twitter_nbmod, test = "LRT") %>% as.data.frame %>% round(3)
# Check VIF
twitter_nbmod %>% car::vif()
# Calc rate ratios
twitter_nbmod %>% 
  coef() %>% 
  exp %>%
  round(2) %>%
  as.data.frame
# Calc 95% CI rate ratios
twitter_nbmod %>% 
  confint() %>% 
  exp %>%
  round(2)
# Calculate multiplicative rate for each subsequent month of calendar_date
twitter_nbmod %>% coef() %>% .["calendar_date"] %>% exp() %>% .^30 
# Calculate 95% CI multiplicative rates for each subsequent month of calendar_date
twitter_nbmod %>% confint() %>% as.data.frame %>% .["calendar_date",] %>% exp() %>% .^30 


# Comments
# Poisson regression
comment_poismod <- abstract_scoring_comments %>%
  glm(formula = update(form_usage, comments_count ~ .), data = ., family = "poisson")

# Negative binomial regression
comment_nbmod <- abstract_scoring_comments %>%
  MASS::glm.nb(formula = update(form_usage, comments_count ~ .), data = ., control = glm.control(maxit = 100))

# Confirm negative binomial as better fitting model
AIC(comment_nbmod, comment_poismod)

# Model summary
comment_nbmod %>% summary()
# LRT test
drop1(comment_nbmod, test = "LRT") %>% as.data.frame %>% round(3)
# Check VIF
comment_nbmod %>% car::vif()
# Calc rate ratios
comment_nbmod %>% 
  coef() %>% 
  exp %>%
  round(2) %>%
  as.data.frame
# Calc 95% CI rate ratios
comment_nbmod %>% confint(parm = c("(Intercept)", "as.factor(Highest_change)1", "as.factor(Highest_change)2", 
                                   "`difflib standard change_ratio`", "Word_change_ratio", "covid_preprintTRUE", 
                                   "delay_in_days", "calendar_date")) %>% 
  exp %>%
  round(2)
# Calculate multiplicative rate for each subsequent month of calendar_date
comment_nbmod %>% coef() %>% .["calendar_date"] %>% exp() %>% .^30 
# Calculate 95% CI multiplicative rates for each subsequent month of calendar_date
comment_nbmod %>% confint(parm = "calendar_date") %>% as.data.frame %>% exp() %>% .^30 


# Citations
# Poisson regression
citations_poismod <- abstract_scoring_citations %>%
  glm(formula = update(form_usage, citations ~ .), data = ., family = "poisson")

# Negative binomial regression
citations_nbmod <- abstract_scoring_citations %>%
  MASS::glm.nb(formula = update(form_usage, citations ~ .), data = ., control = glm.control(maxit = 100))

# Confirm negative binomial as better fitting model
AIC(citations_nbmod, citations_poismod)

# Model summary
citations_nbmod %>% summary()
# LRT test
drop1(citations_nbmod, test = "LRT") %>% as.data.frame %>% round(3)
# Check VIF
citations_nbmod %>% car::vif()
# Calc rate ratios
citations_nbmod %>% 
  coef() %>% 
  exp %>%
  round(2) %>%
  as.data.frame
# Calc 95% CI rate ratios
citations_nbmod %>% 
  confint() %>% 
  exp %>%
  round(2)
# Calculate multiplicative rate for each subsequent month of calendar_date
citations_nbmod %>% coef() %>% .["calendar_date"] %>% exp() %>% .^30 
# Calculate 95% CI multiplicative rates for each subsequent month of calendar_date
citations_nbmod %>% confint() %>% as.data.frame %>% .["calendar_date",] %>% exp() %>% .^30 