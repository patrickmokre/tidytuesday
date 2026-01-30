(ggplot() +
    geom_sf(data = hex_tt %>% filter(share=="Rest"), aes(fill = share), color = "black", linewidth = .2) +
    geom_sf(data = hex_tt %>% filter(share=="Top 25 Firms"), aes(fill = share), color = "white", linewidth = .2) +
    # geom_sf(data = brazil, fill = NA, color = "black", linewidth = 0.5) + # optional outline
    annotate("label", x = x_center - 4e+05, y = y_top,
             label = paste0(" ", nfirms_top50, " \n Firms "), size = 6, lineheight = .3, linewidth=0.2, label.r=unit(0, "pt"),
             fill = "white", color = "black", fontface = "bold", family="Roboto Slab") +
    annotate("label", x = x_center + 7e+05, y = y_bottom,
             label = paste0(" ", nfirms_rest, " \n Firms "), size = 6, lineheight = .3, linewidth=0.2, label.r=unit(0, "pt"),
             fill = "white", color = "#000000", fontface = "bold", family="Roboto Slab") +
    scale_fill_manual(values = c("Top 25 Firms" = "darkred", "Rest" = "white")) +
    labs(
      title = paste0(" ", nfirms_top50, " Firms own half the \n capital stock in Brazil ") %>% toupper(),
      caption = "Data: Tidy Tuesday 2026, Week 4"
    ) +
    theme_void(base_family = "Roboto Slab") +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 32, hjust = 0.5, lineheight = .3, color="#000000"),
      plot.caption = element_text(size=20, hjust = 0.5, color="#000000"),
      panel.background = element_rect(fill="#ffffff"),
      plot.background = element_rect(fill="#ffffff")
    )
) %>% 
  ggsave("tt2604-d3-pemstyle.png", plot=., width=5, height=5, units="cm")
