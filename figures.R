# Load libraries and set theme ----
pacman::p_load("tidyverse", "colorspace", "lubridate", "patchwork", "corrr")

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
abstract_scoring <- read_csv("./data/abstract_scoring.csv")
# Analysis of abstracts using computational methods
preprint_full <- read_csv("./data/preprint_details.csv")
# Preprint metadata collected as part of Fraser et al (code: 10.5281/zenodo.4501924) 
panels <- read_csv("./data/panels_server.csv")
# Total panels broken down by server and COVID-status
rec_scores <- read_csv("./data/rec_scores.csv")
# Granular analysis of abstract changes combined with the overall (Highest) score
abstract_scoring_reconciled <- read_csv("./data/reconciled_scores.csv")
# Granular analysis of abstract changes

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
  labs(y = "Number of preprints \n subseqeuntly published (>1)", 
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

Fig_1 #+ 
ggsave("./figures/changes_Fig_1.png", width = 10, height = 14)




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
  labs(y = "Number of bioRxiv preprints \n subseqeuntly published (>1)", 
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
  labs(y = "Number of medRxiv preprints \n subseqeuntly published (>1)", 
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

S_Fig_1 #+ 
ggsave("./figures/changes_S_Fig_1.png", width = 10, height = 12)



# Figure 2 ------------

F2A <-  panels %>%   
  mutate(preprint_paper = factor(preprint_paper,
                                 levels = c("COVID preprint", "COVID paper", "Non-COVID preprint", "Non-COVID paper"))) %>%
  ggplot(aes(x = preprint_paper, y = main_total, fill = preprint_paper)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.2) +
  geom_jitter(shape = 21, size = 0.6, alpha = 0.2, width = 0.3) +
  labs(x = "", y = "total panels & tables", fill = "") +
  scale_fill_manual(values = rep(qualitative_hcl(2, palette = "Set2"), each = 2)) +
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
  ggplot(aes(y = panel_change, x = covid_preprint, color = covid_preprint, fill = covid_preprint)) +
  geom_violin(alpha = 0.7) +
  theme_minimal() +
  scale_color_manual(values = qualitative_hcl(2, palette = "Set2")) +
  theme(legend.position = "none") +
  labs(x = "", y = "Difference in number of panels and tables")
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


# Patchwork
Fig_2 <- (F2A + F2B) / F2C +
  # plot_layout(ncol = 2) +
  # plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

Fig_2 #+ 
ggsave("./figures/changes_Fig_2.png", width = 10, height = 12)

# Supp Fig 2 ----

SF2A <- panels %>%   
  mutate(preprint_paper = factor(preprint_paper,
                                 levels = c("COVID preprint", "COVID paper", "Non-COVID preprint", "Non-COVID paper"))) %>%
  ggplot(aes(x = preprint_paper, y = main_total, fill = preprint_paper)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.2) +
  geom_jitter(shape = 21, size = 0.6, alpha = 0.2, width = 0.3) +
  labs(x = "", y = "total panels & tables", fill = "") +
  scale_fill_manual(values = rep(qualitative_hcl(2, palette = "Set2"), each = 2)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Article type", y = "Total panels and tables") +
  guides(color = FALSE) +
  facet_wrap(~source)

SF2B <- main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  ggplot(aes(y = panel_change, x = covid_preprint, color = covid_preprint, fill = covid_preprint)) +
  geom_violin(alpha = 0.7) +
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

S_Fig_2 #+ 
ggsave("./figures/changes_S_Fig_2.png", width = 10, height = 12)

# Figure 3 -------------

F3A <- abstract_scoring %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(covid_preprint != "NA") %>%
  ggplot(aes(x= covid_preprint, y = `difflib standard change_ratio`, color = covid_preprint)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "Change ratio (difflib)") 

F3B <-  abstract_scoring %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(covid_preprint != "NA") %>%
  ggplot(aes(x= covid_preprint, y = Word_change_ratio, color = covid_preprint)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
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

F3D <- abstract_scoring %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(covid_preprint != "NA") %>% 
  ggplot(aes(x= `1+_annotations`, y= `1-_annotations`, color = covid_preprint)) +
  geom_jitter(alpha = 0.3, position = position_jitterdodge(jitter.height=0.1, jitter.width = 0, dodge.width=0.3)) +
  scale_y_continuous(minor_breaks = seq(0, 10, 1), breaks = seq(0, 8, 2)) +
  scale_x_continuous(minor_breaks = seq(0, 10, 1), breaks = seq(0, 12, 2)) +
  theme_minimal() +
  theme(panel.grid.minor = element_line(colour="grey90", size=0.5)) +
  theme(legend.position = "bottom") +
  labs(x = "Sum of positive scores", y = "Sum of negative scores", fill = "", color = "") 

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

Fig_3 #+ 
ggsave("./figures/changes_Fig_3.png", width = 10, height = 12)

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
  ggplot(aes(x= Highest_change, y = `difflib standard change_ratio`, color = covid_preprint)) +
  geom_jitter(alpha = 0.3, position=position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
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
  ggplot(aes(x= Highest_change, y = Word_change_ratio, color = covid_preprint)) +
  geom_jitter(alpha = 0.3, position=position_dodge(width=0.75)) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Scored change", y = "Word change ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

SF3C <-  abstract_scoring %>% 
  group_by(Highest_change) %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>%
  filter(covid_preprint != "NA") %>% 
  ggplot(aes(x= `1+_annotations`, y= `1-_annotations`, color = Highest_change)) +
  geom_jitter(alpha = 0.3, position = position_jitterdodge(jitter.height=0.1, jitter.width = 0, dodge.width=0.5)) +
  scale_y_continuous(minor_breaks = seq(0, 10, 1), breaks = seq(0, 8, 2)) +
  scale_x_continuous(minor_breaks = seq(0, 10, 1), breaks = seq(0, 12, 2)) +
  theme_minimal() +
  theme(panel.grid.minor = element_line(colour="grey90", size=0.5)) +
  theme(legend.position = "bottom") +
  labs(x = "Sum of positive scores", y = "Sum of negative scores", fill = "", color = "") +
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
  filter(covid_preprint == "COVID article") %>%
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>% 
  count(published_journal, covid_preprint, Highest_change) %>% 
  arrange(desc(n)) %>% 
  #  filter(n > 1) %>% 
  ggplot(aes(x = abbreviate(published_journal, minlength = 4), y = n, fill = Highest_change)) +
  geom_col() +
  labs(y = "Number of COVID-19 preprints \n subseqeuntly published", 
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
#EE#
FFFF
"  


S_Fig_3 <- SF3A + SF3B + SF3C + SF3D + SF3E + SF3F +  
  plot_layout(design = layout_SF3) +
  #  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") 

S_Fig_3 #+ 
ggsave("./figures/changes_S_Fig_3.png", width = 10, height = 12)



# # join / create data frames -----
# 
# abstract_scoring_reconciled <- read_csv("https://raw.githubusercontent.com/preprinting-a-pandemic/preprint_changes/cleaning-up/final_reconciled_annotations.csv?token=AO6LFYUCOAYIT63OUBCSVVDAE64ZI")
# 
# # Abstract_scoring
# abstract_scoring <- left_join(abstract_scoring, preprint_info, by = "doi")
# 
# write_csv(abstract_scoring, "./output/abstract_scoring.csv")
# 
# # main_body_changes
# main_body_changes <- left_join(main_body_changes, preprint_info, by = "doi")
# 
# write_csv(main_body_changes, "./output/main_body_changes.csv") 
# 
# # Reconciled scores with abstract score for better plotting of granular changes
# 
# rec_scores <-  abstract_scoring %>% 
#   select(doi, Highest_change) %>% 
#   left_join(abstract_scoring_reconciled, by = "doi") 
# 
# write_csv(rec_scores, "./output/rec_scores.csv") 


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

# Data availability
# Fisher's exact test of association
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
# Fisher's exact test of association
main_body_changes %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>%
  with(., table(covid_preprint, author_list)) %>%
  fisher.test(simulate.p.value = TRUE, B = 1000)  

# Table for correlations

corr_table <- abstract_scoring %>% 
  select(doi, `difflib standard change_ratio`, Word_change_ratio, `1+_annotations`, `1-_annotations`) %>% 
  left_join(rec_scores, by = "doi")

# Correlation analysis ----

abstract_scoring %>% 
  filter(covid_preprint == T) %>%
  select(Word_change_ratio, `difflib standard change_ratio`, `1+_annotations`, `1-_annotations`) %>%
  map_df(~ broom::tidy(cor.test(., 
                                abstract_scoring %>% 
                                  filter(covid_preprint == T) %>% 
                                  pull(Highest_change),
                                method = "spearman")), .id = 'var')
abstract_scoring %>% 
  filter(covid_preprint == F) %>%
  select(Word_change_ratio, `difflib standard change_ratio`, `1+_annotations`, `1-_annotations`) %>%
  map_df(~ broom::tidy(cor.test(., 
                                abstract_scoring %>% 
                                  filter(covid_preprint == F) %>% 
                                  pull(Highest_change),
                                method = "spearman")), .id = 'var')

# Data tables -----

# How many abstracts change overall
abstract_scoring %>% 
  filter(Highest_change != "NA") %>% 
  group_by(Highest_change) %>% 
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>%
  filter(covid_preprint != "NA") %>% 
  count(Highest_change, covid_preprint) %>% 
  mutate(proportion = (n/sum(n))*100) %>% 
  View()

# Journals publishing by score 
abstract_scoring %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID article",
    covid_preprint == F ~ "non-COVID article")) %>% 
  filter(Highest_change != 'NA') %>% 
  mutate(Highest_change = factor(Highest_change,
                                 levels = c("0", "1", "2"),
                                 labels = c("No Change", "Strengthening/softening, minor", "Major conclusion change"))) %>% 
  count(published_journal, covid_preprint, Highest_change) %>%
  #  filter(score_new == "No Change") %>% 
  View()
