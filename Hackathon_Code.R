library(tidyverse)
library(dplyr)
library(ggplot2)
library(rpart)
library(caret)
library(gtExtras)
library(gt)

#Organizing the Data

Fangraphs_Year_Data <- Fangraphs %>% 
  mutate(Pitching_plus = replace(Pitching_plus, is.na(Pitching_plus), 0),
         Pit_plus_FA = replace(Pit_plus_FA, is.na(Pit_plus_FA), 0),
         Pit_plus_CH = replace(Pit_plus_CH, is.na(Pit_plus_CH), 0),
         Pit_plus_SL = replace(Pit_plus_SL, is.na(Pit_plus_SL), 0),
         Pit_plus_CU = replace(Pit_plus_CU, is.na(Pit_plus_CU), 0),
         Pit_plus_SI = replace(Pit_plus_SI, is.na(Pit_plus_SI), 0),
         Pit_plus_FS = replace(Pit_plus_FS, is.na(Pit_plus_FS), 0),
         Pit_plus_FC = replace(Pit_plus_FC, is.na(Pit_plus_FC), 0)) %>% 
  group_by(MLBAMID, Name, Season, Role) %>% 
  summarise(Innings_Pitched = sum(IP, na.rm = TRUE),
            ERA = sum(ERA * IP, na.rm = TRUE) / sum(IP, na.rm = TRUE),
            K_pct = sum(K_pct * TBF, na.rm = TRUE) / sum(TBF, na.rm = TRUE),
            BB_pct = sum(BB_pct * TBF, na.rm = TRUE) / sum(TBF, na.rm = TRUE),
            GB_pct = sum(GB_pct * Events, na.rm = TRUE) / sum(Events, na.rm = TRUE),
            CSW_pct = sum(CSW_pct * Pitches, na.rm = TRUE) / sum(Pitches, na.rm = TRUE),
            FStrike_pct = sum(FStrike_pct * Pitches, na.rm = TRUE) / sum(Pitches, na.rm = TRUE),
            Zone_pct = sum(Zone_pct * Pitches, na.rm = TRUE) / sum(Pitches, na.rm = TRUE),
            LOB_pct = mean(LOB_pct, na.rm = TRUE),
            BABIP = sum(BABIP * Events, na.rm = TRUE) / sum(Events, na.rm = TRUE),
            WHIP = mean(WHIP, na.rm = TRUE),
            FIP = mean(FIP, na.rm = TRUE),
            SIERA = mean(SIERA, na.rm = TRUE),
            Pace = mean(Pace, na.rm = TRUE ),
            WPA = mean(WPA, na.rm = TRUE),
            Pitching_plus = mean(Pitching_plus, na.rm = TRUE),
            FB_plus = mean(Pit_plus_FA, na.rm = TRUE),
            CH_plus = mean(Pit_plus_CH, na.rm = TRUE),
            SL_plus = mean(Pit_plus_SL, na.rm = TRUE),
            CB_plus = mean(Pit_plus_CU, na.rm = TRUE),
            SI_plus = mean(Pit_plus_SI, na.rm = TRUE),
            SPLIT_plus = mean(Pit_plus_FS, na.rm = TRUE),
            CUT_plus = mean(Pit_plus_FC, na.rm = TRUE)) %>% 
  ungroup() %>%
  filter(Innings_Pitched >= 20) %>%
  mutate(FB_Score = ifelse(FB_plus >= 97, 1, 0),
         CH_Score = ifelse(CH_plus >= 98, 1, 0),
         SL_Score = ifelse(SL_plus >= 106, 1, 0),
         CB_Score = ifelse(CB_plus >= 103, 1, 0),
         SI_Score = ifelse(SI_plus >= 95, 1, 0),
         SPLIT_Score = ifelse(SPLIT_plus >= 105, 1, 0),
         CUT_Score = ifelse(CUT_plus >= 98, 1, 0)) %>% 
  rowwise() %>% 
  mutate(Pitch_Arsenal = sum(FB_Score, CH_Score, SL_Score, CB_Score, SI_Score, SPLIT_Score, CUT_Score))

Fangraphs_Data <- Fangraphs_Year_Data %>% 
  group_by(MLBAMID, Name, Role) %>% 
  summarise(Innings_Pitched = mean(Innings_Pitched, na.rm = TRUE),
            ERA = mean(ERA , na.rm = TRUE),
            K_pct = mean(K_pct, na.rm = TRUE),
            BB_pct = mean(BB_pct, na.rm = TRUE),
            GB_pct = mean(GB_pct, na.rm = TRUE),
            CSW_pct = mean(CSW_pct, na.rm = TRUE),
            Zone_pct = mean(Zone_pct, na.rm = TRUE),
            FStrike_pct = mean(FStrike_pct, na.rm=TRUE),
            Pace = mean(Pace, na.rm = TRUE),
            WHIP = mean(WHIP, na.rm = TRUE),
            LOB_pct = mean(LOB_pct, na.rm = TRUE),
            BABIP = mean(BABIP, na.rm = TRUE),
            FIP = mean(FIP, na.rm= TRUE),
            SIERA = mean(SIERA, na.rm = TRUE),
            WPA = mean(WPA, na.rm = TRUE),
            Pitching_plus = mean(Pitching_plus, na.rm = TRUE),
            FB_plus = mean(FB_plus, na.rm = TRUE),
            CH_plus = mean(CH_plus, na.rm = TRUE),
            SL_plus = mean(SL_plus, na.rm = TRUE),
            CB_plus = mean(CB_plus, na.rm = TRUE),
            SI_plus = mean(SI_plus, na.rm = TRUE),
            SPLIT_plus = mean(SPLIT_plus, na.rm = TRUE),
            CUT_plus = mean(CUT_plus, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(K_pct = K_pct * 100,
         BB_pct = BB_pct * 100,
         GB_pct = GB_pct * 100,
         CSW_pct = CSW_pct * 100,
         BABIP = BABIP * 100,
         FB_Score = ifelse(FB_plus >= 97, 1, 0),
         CH_Score = ifelse(CH_plus >= 98, 1, 0),
         SL_Score = ifelse(SL_plus >= 106, 1, 0),
         CB_Score = ifelse(CB_plus >= 103, 1, 0),
         SI_Score = ifelse(SI_plus >= 95, 1, 0),
         SPLIT_Score = ifelse(SPLIT_plus >= 105, 1, 0),
         CUT_Score = ifelse(CUT_plus >= 98, 1, 0)) %>% 
  rowwise() %>% 
  mutate(Pitch_Arsenal = sum(FB_Score, CH_Score, SL_Score, CB_Score, SI_Score, SPLIT_Score, CUT_Score)) %>% 
  select(-FB_plus, -CH_plus, -SL_plus, -CB_plus, -SI_plus, -SPLIT_plus, -CUT_plus, -FB_Score, -CH_Score, -SL_Score, -CB_Score, -SI_Score, -SPLIT_Score, -CUT_Score, -Pitching_plus)

Savant_Data <- Savant %>% 
  select(player_name, pitcher, release_speed) %>% 
  group_by(pitcher, player_name) %>% 
  summarise(Velocity = mean(release_speed, na.rm = TRUE)) %>% 
  ungroup()

MLB <- right_join(Savant_Data, Fangraphs_Data, by = c("pitcher" = "MLBAMID")) %>% 
  select(-Name) %>% 
  mutate(player_name = replace(player_name, pitcher == "660636", "Diego Castillo 2"),
         player_name = replace(player_name, pitcher == "671106", "Logan Allen 2"),
         player_name = replace(player_name, pitcher == "622491", "Luis Castillo 2"),
         player_name = replace(player_name, pitcher == "642770", "Javy Guerra 2"))

MLB_Name <- MLB$player_name
MLB_ID <- MLB$pitcher

MLB_Tree <- MLB %>%
  select(-pitcher, -player_name)

rownames(MLB_Tree) <- make.unique(MLB_Name)

#Building the Tree

tree.mlb <- rpart(Role ~ ERA+K_pct+BB_pct+GB_pct+Pitch_Arsenal+CSW_pct+FStrike_pct+Zone_pct+Pace+LOB_pct+BABIP+WHIP+SIERA+FIP+WPA, data=MLB_Tree, method="class")

tree.mlb

plot(tree.mlb); text(tree.mlb)

plot(tree.mlb, uniform=TRUE, main="Decision Tree for MLB Data", margin=.1); text(tree.mlb, cex=0.8)

group<-predict(tree.mlb, type="class")

tab.tree <- table(group,Role)

1-(sum(diag(tab.tree)) / sum(tab.tree))


view(group)

MLB_Group <- merge(MLB_Tree, group, by = 0, all = TRUE) 

Predicted_Role <- data.frame(group)
d <- Predicted_Role
names <- rownames(d)
rownames(d) <- NULL
Predicted_Role_Data <- cbind(names,d)

MLB_Group <- left_join(MLB, Predicted_Role_Data, by = c("player_name" = "names"))


#Clustering the Starters

MLB_New_Starters <- MLB_Group %>%
  filter(group == "SP", Role == "RP") %>%
  select(-Innings_Pitched, -Velocity, -WHIP)

MLB_Starters_PCA <- MLB_New_Starters %>%
  select(-pitcher, -player_name, -Role, -group)

MLB_Starters_PCA <- prcomp(MLB_Starters_PCA, center = TRUE, scale = TRUE)

get_eigenvalue(MLB_Starters_PCA)

fviz_eig(MLB_Starters_PCA, addlabels = TRUE) +
  xlab("Principal Component") +
  ylab("% of Variance Explained") +
  labs(title = "**PCA Analysis: Scree Plot**")

pc1 <- fviz_contrib(MLB_Starters_PCA, choice = "var", axes = 1)
pc2 <- fviz_contrib(MLB_Starters_PCA, choice = "var", axes = 2)
pc3 <- fviz_contrib(MLB_Starters_PCA, choice = "var", axes = 3)


plot_grid(pc1, pc2, pc3)

k <- 3

pca_scores <- MLB_Starters_PCA$x

set.seed(1928)
MLB_kmeans_Starters <- kmeans(pca_scores, centers = k)

MLB_kmeans_Starters$cluster

cluster_assignment <- MLB_kmeans_Starters$cluster

MLB_New_Starters$cluster <- cluster_assignment

kmean_dataviz <- MLB_New_Starters %>%
  rename(c(#"Velocity" = Velocity,
    #"Innings Pitched" = Innings_Pitched,
    "Earned Run Average" = ERA,
    "Strikeout Percentage" = K_pct,
    "Walk Percentage" = BB_pct))

kmean_dataviz <- kmean_dataviz %>%
  mutate(cluster = case_when(
    cluster == 1 ~ "Cluster 1",
    cluster == 2 ~ "Cluster 2",
    cluster == 3 ~ "Cluster 3"))

kmean_data_long <- kmean_dataviz %>%
  gather("Variable", "Value", -player_name, -pitcher, -cluster)

ggplot(kmean_data_long, aes(x = Variable, y = Value, color = cluster)) +
  geom_point(size = 3) +
  facet_wrap(~ cluster) +
  scale_color_brewer(palette = "Set1") +
  gghighlight(use_direct_label = FALSE) +
  theme(axis.text = element_text(angle = 90, size = 8),
        strip.text = element_text(face = "bold"),
        legend.position = "none")

Cluster_1_Starters <- MLB_New_Starters %>% 
  filter(cluster == 1)

Cluster_2_Starters <- MLB_New_Starters %>% 
  filter(cluster == 2)

Cluster_3_Starters <- MLB_New_Starters %>%
  filter(cluster == 3)

#Clustering the Relievers 

MLB_New_RP <- MLB_Group %>%
  filter(group == "RP", Role == "SP") %>%
  select(-Innings_Pitched, -Velocity, -WHIP)

MLB_RP_PCA <- MLB_New_RP %>%
  select(-pitcher, -player_name, -Role, -group)

MLB_RP_PCA <- prcomp(MLB_RP_PCA, center = TRUE, scale = TRUE)

get_eigenvalue(MLB_RP_PCA)

fviz_eig(MLB_RP_PCA, addlabels = TRUE) +
  xlab("Principal Component") +
  ylab("% of Variance Explained") +
  labs(title = "**PCA Analysis: Scree Plot**")

pc1 <- fviz_contrib(MLB_RP_PCA, choice = "var", axes = 1)
pc2 <- fviz_contrib(MLB_RP_PCA, choice = "var", axes = 2)
pc3 <- fviz_contrib(MLB_RP_PCA, choice = "var", axes = 1)

plot_grid(pc1, pc2, pc3)

k <- 3

pca_scores_RP <- MLB_RP_PCA$x

set.seed(1928)
MLB_kmeans_RP <- kmeans(pca_scores_RP, centers = k)

MLB_kmeans_RP$cluster

cluster_assignment_RP <- MLB_kmeans_RP$cluster

MLB_New_RP$cluster <- cluster_assignment_RP

kmean_dataviz_RP <- MLB_New_RP %>%
  rename(c(#"Velocity" = Velocity,
    #"Innings Pitched" = Innings_Pitched,
    "Earned Run Average" = ERA,
    "Strikeout Percentage" = K_pct,
    "Walk Percentage" = BB_pct))

kmean_dataviz_RP <- kmean_dataviz_RP %>%
  mutate(cluster = case_when(
    cluster == 1 ~ "Cluster 1",
    cluster == 2 ~ "Cluster 2",
    cluster == 3 ~ "Cluster 3"))

kmean_data_long_RP <- kmean_dataviz_RP %>%
  gather("Variable", "Value", -player_name, -pitcher, -cluster)

ggplot(kmean_data_long_RP, aes(x = Variable, y = Value, color = cluster)) +
  geom_point(size = 3) +
  facet_wrap(~ cluster) +
  scale_color_brewer(palette = "Set1") +
  gghighlight(use_direct_label = FALSE) +
  theme(axis.text = element_text(angle = 90, size = 8),
        strip.text = element_text(face = "bold"),
        legend.position = "none")

Cluster_1_RP <- MLB_New_RP %>% 
  filter(cluster == 1)

Cluster_2_RP <- MLB_New_RP %>% 
  filter(cluster == 2)

Cluster_3_RP <- MLB_New_RP %>% 
  filter(cluster == 3) 

#Drey Jameson Table

DreyTbl <- DreyJamesonPitches %>% 
  select(Pitch, `Percent Thrown`, `Pitches thrown`, `Induced Vertical Break`, `Horizontal Break`, Stuff_Plus, Location_Plus, Pitch_Plus, Run_Value, R_V_One) %>% 
  gt() %>%
  opt_align_table_header("center") %>% 
  cols_align("center") %>% 
  tab_source_note("Table: Ethan Mann | Data: Baseball Savant") %>%
  cols_label(
    Stuff_Plus = "Stuff+",
    Pitch_Plus = "Pitch+", 
    Location_Plus = "Location+",
    Run_Value = "Run Value",
    R_V_One = "Run/Value over 100 pitches"
  ) %>% 
  opt_row_striping() %>% 
  tab_header(title = "Drey Jameson Arsenal") %>% 
  gt_theme_538() 

print(DreyTbl)
