
# Library loaded in the app
# library(tidyverse)
# library(ggthemes)

simulate_1_pop <- function(parametre) {
  
  # CONDITIONS DE SIMULATION
  temps = 30 # nb de pas de temps (en années)

  r = parametre[1]
  K = parametre[2]
  N0 = parametre[3]
  
  # what is tau_p
  tau_p = 0.05
  
  # INITIALISATION
  N <- array(0, dim = c(temps))
  Nm <- array(0, dim = c(temps))
  
  # conditions initiales 
  Nm[1] = N0
  N[1] = rlnorm(1, log(Nm[1]), tau_p)
  
  # boucle du temps
  for (t in 1:(temps - 1)) {
    Nm[t + 1] = max(N[t] + r * N[t] * (1 - N[t] / K) , 0.0001 * K)
    N[t + 1] = rlnorm(1, log(Nm[t + 1]), tau_p)
  }
  
  res = tibble(Size = round(N),
               time = 1:temps)
  
  return(res)
}


simulate_n_pop = function(parametre = c(1.2, 100, 0), nb_of_pop=5){
  df_simulations = data.frame()
  for (i in 1:nb_of_pop) {
    data_pop = simulate_1_pop(parametre)
    data_pop$Population = as.factor(rep(i, nrow(data_pop)))
    df_simulations = rbind(df_simulations, data_pop)
  }
  return(df_simulations)
}



plot_isolated_pop = function(parametre = c(1.2, 100,10), nb_of_pop=5, t_max = 30){
  K = parametre[2]
  r = parametre[1]
  df_simu = simulate_n_pop(parametre, nb_of_pop) %>% 
    mutate(r = rep(r, nb_of_pop*t_max),K = rep(K, each = nb_of_pop*t_max))
  ymax = max(df_simu$Size)
  
  plot = ggplot(data = df_simu, aes(x = time, y = Size, color=Population,
                                    text = paste0("r: ", r,
                                                  "\nK: ", K))) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    labs(title = paste("Population dynamic for", nb_of_pop ,"isolated populations"),
         x = "Time",
         y = "Population size",
         color = "Population") +
    theme_hc() +
    theme(axis.title = element_text(),
          text = element_text(family = "Rubik")) +
    scale_color_brewer(palette = "Set1") +
    ylim(0, ymax)
  
  # Interactive graph
  font = list(family = "Rubik", size = 12, color = "white")
  label = list(bordercolor = "transparent", font = font)
  plot_interactif = ggplotly(plot) %>% 
    style(hoverlabel = label) %>% 
    layout(font = font, legend = list(orientation = "h",x = 0.1, y = -0.3)) %>% 
    config(
      modeBarButtonsToRemove = list(
        "zoom2d",
        "pan2d",
        "zoomIn2d",
        "zoomOut2d",
        "autoScale2d",
        "resetScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "sendDataToCloud",
        "toggleHover",
        "resetViews",
        "toggleSpikelines",
        "resetViewMapbox"
      ),
      displaylogo = FALSE
    )
  return(plot_interactif)
}
plot_isolated_pop()
