library("writexl")
library(ggplot2)
root_path = "~/Documents/Faculdade/semestre6/MQA/Trab1/"
data_csgo <- read.csv(paste0(root_path, "tb_stats.csv"))
data_csgo <- data_csgo[,!(names(data_csgo) %in% c("X",
                                                 "idPlayer",
                                                 "idLobbyGame",
                                                 "idRoom",
                                                 "qtTkAssist",
                                                 "qtPlusKill",
                                                 "qtHitHeadshot",
                                                 "dtCreatedAt", 
                                                 "flTwitter", 
                                                 "flTwitch",
                                                 "flFacebook",
                                                 "descCountry", 
                                                 "dtBirth", 
                                                 "dtRegistration"))]
write_xlsx(data.frame(summary(data_csgo)), paste0(root_path, "frequencias_missing.xlsx"))
data_csgo_discrete <- data_csgo[,!(names(data_csgo) %in% c("medals", 
                                                           "descMapName", 
                                                           "flWinner"))]

data_csgo_discrete = na.omit(data_csgo_discrete)
for (col in names(data_csgo_discrete)){
  ggsave(paste0(root_path, "/graphs/",col,"_distribution.png"),
         ggplot(data = data_csgo_discrete) + 
          aes(x = data_csgo_discrete[, col]) +
          geom_histogram(bins=20)+
          labs(title = paste0("Distribuição da variavel: ", col, 
                              ", média: ", round(mean(data_csgo_discrete[, col]), digits = 2), 
                              ", D.P.: ", round(sd(data_csgo_discrete[, col]), digits=2)), 
               y="Densidade", 
               x=element_blank())+
          geom_vline(xintercept = mean(data_csgo_discrete[, col]),  
                                       colour = "red",
                                       label="mean")+
          geom_vline(xintercept = mean(data_csgo_discrete[, col])+sd(data_csgo_discrete[, col]),  
                     colour = "blue",
                     label="sd")+
          geom_vline(xintercept = mean(data_csgo_discrete[, col])-sd(data_csgo_discrete[, col]),  
                     colour = "blue",
                     label="sd"),
         width = 20,
         height = 14,
         units = "cm")
  ggsave(paste0(root_path, "/graphs/",col,"_boxplot.png"),
         ggplot(data = data_csgo_discrete) + 
           aes(x = data_csgo_discrete[, col]) +
           geom_boxplot(bins=20)+
           labs(title = paste0("Distribuição da variavel: ", col), 
                y=col, 
                x=element_blank()))
}
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
data_csgo_categorical <- data_csgo[, c("descMapName", "flWinner")]
for (name in names(data_csgo_categorical)){
  ggsave(paste0(root_path, "/graphs/",name,"_distribution.png"),
         ggplot(data = data_csgo_categorical) + 
          aes(x = data_csgo_categorical[, name]) +
          geom_bar()+
          labs(title = paste0("Distribuição da variavel: ",name, 
                              ", Moda: ", Mode(data_csgo_categorical[, name])), 
               y="Densidade", 
               x=element_blank()))
  
}