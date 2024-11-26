
data(friendship, package = "SNA4DSData")
library(btergm)
sapply(friendship, snafun::count_vertices)


# edges: number of edgtes in the network
# mutual: number of mutual (= reciprocated) ties
# ttriple:  number of transitive triples in the network, defined as a set of edges {(i->j),j->k),(i->k)}
# transitiveties: number of ties i->j such that there exists a two-path from i to j
# ctriple: number of cyclic triples in the network, defined as a set of edges of the form 
# {(i->j),(j->k),(k->i)}
# nodeicov: main effect of a covariate for in-edges
# nodeocov: main effect of a covariate for out-edges
# nodeifactor: factor attribute effect for in-edges
# nodeofactor: factor attribute effect for out-edges
# nodematch: homophily
# edgecov: edge covariate
# delrecip(mutuality = FALSE), delayed reciprocity,  If mutuality = TRUE is set, this extends not only to ties, but also non-ties. 
# memory(type = "stability"), checks whether both edges and non-edges are stable between the previous and the current network


model.01 <- btergm::btergm(friendship ~ edges + mutual + ttriple +
                             transitiveties + ctriple + nodeicov("idegsqrt") +
                             nodeicov("odegsqrt") + nodeocov("odegsqrt") +
                             nodeofactor("sex") + nodeifactor("sex") + nodematch("sex") +
                             edgecov(primary), R = 100)

btergm::summary(model.01)
btergm::summary(model.01, type = "basic")
btergm::summary(model.01, type = "bca")


gof.01 <- snafun::stat_plot_gof_as_btergm(model.01, 
                                btergm_statistics = c(btergm::esp, btergm::dsp, 
                                                      btergm::geodesic, btergm::deg, 
                                                      btergm::triad.undirected, btergm::rocpr))



#W model 2, nu zijn delrecip en memory toegevoegd
model.02 <- btergm::btergm(friendship ~ edges + mutual + ttriple +
                             transitiveties + ctriple + nodeicov("idegsqrt") + nodeicov("odegsqrt") +
                             nodeocov("odegsqrt") + nodeofactor("sex") + nodeifactor("sex") +
                             nodematch("sex") + edgecov(primary) + delrecip +
                             memory(type = "stability"), R = 100)
btergm::summary(model.02)
gof.02 <- snafun::stat_plot_gof_as_btergm(model.02,
                                btergm_statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))




# nu data van newerken  1-3 gebruiken om 4 te voorspellen en voor gof
model.03 <- btergm::btergm(friendship[1:3] ~ edges + mutual + ttriple +
                             transitiveties + ctriple + nodeicov("idegsqrt") + nodeicov("odegsqrt") +
                             nodeocov("odegsqrt") + nodeofactor("sex") + nodeifactor("sex") +
                             nodematch("sex") + edgecov(primary) + delrecip() +
                             memory(type = "stability"), R = 100)
gof.03 <- btergm::gof(model.03, nsim = 100, target = friendship[[4]],
                      formula = friendship[3:4] ~ edges + mutual + ttriple +
                        transitiveties + ctriple + nodeicov("idegsqrt") +
                        nodeicov("odegsqrt") + nodeocov("odegsqrt") + nodeofactor("sex") +
                        nodeifactor("sex") + nodematch("sex") + edgecov(primary) +
                        delrecip + memory(type = "stability"), coef = coef(model.03),
                      statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

nw <- simulate(model.03, nsim = 10, index = 4)


btergm::interpret(model.03, type = "dyad", i = 10, j = 15)
btergm::interpret(model.03, type = "dyad", i = 10, j = 15, t = 2)
btergm::interpret(model.03, type = "node", i = 10, j = 13:15, t = 2)



# Als je de gof print, dan zie je een hele serie tabellen. Bijv. voor esp.
# Elke rij geeft aan het aantal esp dat in de data en/of de gesimuleerde netwerken
# voorkwam. In dit geval zijn dat minimaal 0 en maximaal 20.
# # dus: gemiddeld over de 4 netwerken, waren er 17 edges met esp = 0. 
# Het mediane aantal edges met esp = 0 is 18.5.
# 
# NB: hoe vind je die aantallen?
#   ergm::summary_formula(friendship[[1]] ~ esp(d = 0:20))
#   ergm::summary_formula(friendship[[2]] ~ esp(d = 0:20))
#  etc.
#  
#  In de sim kolommen staan deze getallen over alle simulaties.
#  
# 
# In de plot: de gestippelde lijn is de gemiddelde waarde in de observaties, de
# dikke lijn de mediaan.
# 
# De y-as van de plot: dit geeft de relatieve frequentie aan van die waarde.
#   bekijk eens de triad census. De gebobserveerde medianen tellen op tot 2308.5
#   Het percentage edges met waarde 0 is dan 974.5/2308.5 = .42
#   Het percentage edges met waarde 1 is 1022.5/2308.5 = .44
#   En voor 1: 374/2308.5 = .16
#   Dit zijn de y-waarden
# 

# ROC: 
# x-axis: FPR (False Positive Rate) = false alarm rate = % of 0's where a 
# 1 is predicted. 
# y-axis: TPR (True Positive Rate) = sensitivity = recall = %1's that are 
# correctly predicted.
# Best model: bow to the top left corner.
# Poor model: diagonal
# 
# 
# PR-curve: 
#   x-axis: TPR (True Positive Rate) = sensitivity = recall = %1's that are 
# correctly predicted.
# y-axis: Positive Predictive Value/Rate = precision = % of predicted 1's that 
# are correct
# PR-curve is esp useful when there are few 1's and many 0's. In this case, it 
# is mainly important to model the 1's well. The PR-curve only focuses on 
# how well you do when you predict a 1.
# --> in sparse networks, the PR is more relevant than the ROC.
# 
# Best model: bow to the top right corner.
# Poor model: horizontal line (y-value = %of 0's in the data)





 btergm::interpret(model.01, type = "dyad", i = 12, j = 15, t = 3)
