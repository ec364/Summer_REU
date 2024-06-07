####Load in Data Sets and Packages
sticky <- sticky_trap_counts
chemical <- HBEFdata_All_2024_05_17


view(sticky)

lynx_fig <- ggplot(lynx_df, aes(x = Year, y = Lynx)) + # basic dataset
  geom_point(size = 3, alpha = 0.75) + # creates scatterplot
  geom_smooth(method = lm, color = "aquamarine2") + # adds linear model
  labs(x = "Year",
       y = "Number of Lynx Trapped",
       title = "Lynx R Dataset",
       subtitle = "Tidy Tuesday demo, September 1",
       caption = "Data Source: Brockwell and Davis 1991") + # includes labels
  theme_classic() + # theme with only axes present
  theme(legend.position = "none") + # suppresses legend
  scale_x_continuous(breaks = seq(1820, 1940, 10)) # edits x axes tick marks

lynx_fig # Calls figure.  