
# library(tidyverse)
# library(ggthemes)
# library(visNetwork)
# library(plotly)
# library(extrafont)

# 
# sigma_p : the variability associated with the reproduction process to introduce
# stochasticity in the dynamic
# temps : nb de pas de temps (en annees)
simulate_n_metapop <- function(parametre, sigma_p = 0.05, temps = 30) 
{ 
  # CONDITIONS DE SIMULATION
  nb_pop = length(parametre$r)
  r = parametre[[1]] 
  K = parametre[[2]]
  N0 = parametre[[3]]
  R0 = exp(r)
  M = K / (R0 - 1)

  # INITIALISATION
  N <- array(0, dim = c(temps, nb_pop))
  em = array(0, dim = c(temps, nb_pop))
  N[1,] = N0

  # boucle du temps           
  for (t in 1:(temps - 1)) {
    Nt = N[t,]
    # REPRODUCTION
    N_reprod = reproduction(Nt, R0, M)  # reproduction

    # STOCHASTICITY
    N_tplus1 = add_stochasticity(N_reprod, sigma_p)

    # EMIGRATION
    nb_emigrant = calc_nb_emigrant(N_tplus1, K)

    # IMMIGRATION
    nb_immigrant = calc_nb_immigrant(nb_emigrant)

    # Total ; N - emigrant + immigre
    N[t + 1,] = round(N_tplus1 -  nb_emigrant + nb_immigrant)
    em[t+1,] = nb_emigrant
  }
  
    
  df_results = cbind(time = 1:temps,
                 Effectif = N,
                 emigration = em) %>% as.data.frame()
  
  colnames(df_results) = c("time", 
                           1:nb_pop,
                           paste0("emigration_pop", 1:nb_pop))
  return(df_results)
}

reproduction = function(Nt, R0, M){
  Nmt = R0 * Nt / (1+Nt/M)
  Nmt = ifelse(Nmt < 0, K, Nmt)
  return(Nmt)
}

# Ne fonctionne pas bien si r < 0
# R0 = exp(-0.8)
# M = 79 / (R0 - 1)
# reproduction(Nt=138,R0, M )

add_stochasticity = function(Nm_tplus1, sigma_p){
  return(rlnorm(length(Nm_tplus1), log(Nm_tplus1), sigma_p))
}

calc_nb_emigrant = function(N, K){
  # calcul du nombre d'emigrant
  surplus = N - K
  constitutive_migration = 0.05 * N
  nb_emigrant = ifelse(surplus>0, surplus+constitutive_migration, 
                       constitutive_migration)
  return(nb_emigrant)
}

# K = 1000
# N = 1:1100
# E = calc_nb_emigrant(N, K)
# plot(N, E, type = 'l')

calc_nb_immigrant = function(nb_emigrant){
  #chaque pop se partage equitablement les emigrants
  nb_immigrant = (sum(nb_emigrant) - nb_emigrant) / (length(nb_emigrant) - 1)
  return(nb_immigrant)
}

plot_connected_pop = function(parametre, show_K=FALSE){
  t_max = 30
  r = parametre[[1]] 
  K = parametre[[2]] 
  nb_of_pop = length(K)
  
  df_simu = simulate_n_metapop(parametre) %>% 
    select(time,as.character(1:nb_of_pop)) %>% 
    pivot_longer(-time, values_to = "Size", names_to = "Population") %>% 
    mutate(r = rep(r,t_max), K = rep(K, t_max))
  
  plot = ggplot(data = df_simu, 
                aes(x = time, y = Size, color=Population,
                    text = paste0("r: ", r,
                                  "\nK: ", K))) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    labs(title = paste("Population dynamic for", nb_of_pop ,"connected populations"),
         x = "Time",
         y = "Population size",
         color = "Population") +
    theme_hc() +
    theme(axis.title = element_text(),
          text = element_text(family = "Rubik"),
          legend.position = "bottom") +
    scale_color_brewer(palette = "Set1") +
    ylim(0, 1.3*max(K))
  
  if (show_K){ plot = plot +
      geom_hline(yintercept = K, 
                 linetype = "dashed", color = "black")
  }
  
  # Interactive graph
  font = list(family = "Rubik", size = 12, color = "white")
  label = list(bordercolor = "transparent", font = font)
  plot_interactif = ggplotly(plot) %>% 
    style(hoverlabel = label) %>% 
    layout(font = font, legend = list(orientation = "h",x = 0.2, y = -0.3)) %>% 
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
  #config(displayModeBar = FALSE)
  
  return(plot_interactif)
}

# parametre = list(r = c(2, 0), K=c(281, 37), N0=c(10, 10))
# 
# plot_connected_pop(parametre, show_K=TRUE)

# sim2 = simulate_n_metapop(parametre)
# # #
# sim2$effectif_pop1 #effectif pop 1
# # 
# 
# data2 = sim2 %>% 
#   select(time,starts_with("effectif")) %>% 
#   pivot_longer(-time, values_to = "effectif", names_to = "pop") 
# 
# 
# ggplot(data2, aes(x = time, y = effectif, col = pop)) +
#   geom_line(linewidth = 0.8, alpha = 0.8)
# 
# 
# # essai pour quatre pop
# parametre = list(r = c(-0.2, 2.1, 1.2, -0.8), 
#                  K = c(1000, 800, 250, 300), 
#                  N0=c(30, 25, 15, 20))
# 
# simulate_n_metapop2(parametre = parametre)


emigrant_flow = function(res, date) {
  
  nb_pop = (ncol(res) - 1) / 2
  
  # recuperation des tailles de populations
  pop_size = res[date, 2:(nb_pop+1)]
  
  # calculs des flux
  res = res[date, (nb_pop + 2):(2 * nb_pop + 1)]
  id = 1:nb_pop
  tab.em  = data.frame(from = NA, to = NA, width = NA)
  
  for (n in 1:nb_pop) {
    
    em_i = res[, n] / (nb_pop - 1)
    
    tab.em_i  = data.frame(from = rep(n, (nb_pop - 1)),
                       to = id[id != n],
                       width = rep(em_i, (nb_pop - 1)))
    
    tab.em = rbind(tab.em, tab.em_i)
    
  }
  
  flow = tab.em[2:nrow(tab.em), ]
  
  return(list(flow, pop_size))
}


plot_network = function(res, date, K){
  tab = emigrant_flow(res, date)
  
  pop_size = unlist(tab[[2]])
  nb_pop = length(pop_size)

  edges = tab[[1]]
  
  edges$width = 2 * edges$width / mean(edges$width)
  
  nodes <- data.frame(id = 1:nb_pop,
                      value = pop_size,
                      label = paste("", 1:nb_pop))
  
  lnodes <- data.frame(label = c("Colony"),
                       shape = c( "circle"), color = c("blue"))
  
  ledges <- data.frame(color = c("lightblue"),
                       label = c("flow"), arrows =c("from"))
  
  #max_size = 8*(mean(pop_size)/max(K)+1)+(max(pop_size)-min(pop_size)) /max(K)
  max_size = 8*(mean(pop_size)/max(K)+1)+(max(pop_size)-min(pop_size)) /20
  
  min_size = 8*(mean(pop_size)/max(K)+1)
 
  visNetwork(nodes, edges, 
             main = list(text = "Network representation of the metacolony",
                            style = "font-family:Comic Sans MS;color:#000000;font-size:15px;text-align:center;")) %>% 
    visEdges(arrows = 'to', scaling = list(min = 0.5, max = 1.5))%>%
    visNodes(scaling = list(min = min_size, max = max_size), 
             color = list(background = "blue", 
                          border = "lightblue",
                          highlight = "purple"))%>%
    visLayout(randomSeed = 12)%>%
    visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE)
}



parametre = list(r = c(1, 1, 1),
                 K = c(10, 100, 1000),
                 N0 = c(10, 10, 10))
K = c(10, 100, 1000)
res = simulate_n_metapop(parametre)
plot_network(res, date = 1, K)
plot_network(res, date = 10, K)
plot_network(res, date = 11, K)
plot_network(res, date = 20, K)
plot_network(res, date = 30, K)


# parametre = list(r = c(3, -0.8),
#                  K = c(1000, 79),
#                  N0 = rep(68, 2))
# df = simulate_n_metapop(parametre)
# plot_connected_pop(parametre, show_K=TRUE)
