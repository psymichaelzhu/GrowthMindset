pacman::p_load(rstudioapi, bruceR, dplyr, patchwork)

code_path <- getActiveDocumentContext()$path
set.wd(substr(code_path, 1, str_locate(code_path, "GrowthMindset")[2]))
rm(list = ls())
#================================================================
Data_EV <- import("Mindset_InitialState_EV/simulation_results.csv")

Data_EV %>% 
  filter(session_index==0) %>% 
  separate(simulation_id, c("Mindset", "horizon"), sep = "_") %>%
  mutate(Mindset = as.numeric(Mindset),
         horizon = factor(horizon, levels = c("5", "10", "20")),
         state = factor(state)) %>% 
  group_by(Mindset,horizon) %>%
  summarise(cumulative_reward = sum(reward)) %>% 
  ggplot(aes(x = Mindset, y = cumulative_reward)) +
  #barplot
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~horizon) +
  theme_bruce() 

#================================================================
Data_EV_wide <- Data_EV %>%
  filter(action %in% c("A_dependent", "A_independent")) %>%
  separate(simulation_id, c("Mindset", "horizon"), sep = "_") %>%
  mutate(Mindset = as.numeric(Mindset),
         horizon = factor(horizon, levels = c("5", "10", "20")),
         state = factor(state)) %>%
  spread(action, expected_value) %>%
  filter(horizon == "20" & (Mindset %in% c(0, 0.2, 0.3, 0.4, 0.6, 0.9)))

ggplot(Data_EV_wide, aes(x = time, y = state)) +
  geom_tile(aes(fill = ifelse(
    A_dependent > A_independent, "Hard",
    ifelse(A_dependent < A_independent, "Easy", "Random"))),
    color = "white") + 
  scale_fill_manual(values = c("Hard" = "#ee9999", "Easy" = "#99b9ee", "Random" = "#555555")) + 
  facet_wrap(~Mindset) +
  theme_bruce() +
  labs(fill = "Preferred Action")
