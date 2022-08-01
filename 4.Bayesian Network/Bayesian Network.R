suppressPackageStartupMessages(library(graph))
library(gRain)

g<-list(~Cloudy, ~Sprinkler|Cloudy, ~Rain|Cloudy, ~Wet.Grass|Sprinkler:Rain)
# result='graphNEL'; NEL == Node and Edge List
dg<-dagList(g)
#dg<-dag(~Cloudy, ~Sprinkler*Cloudy, ~Rain*Cloudy, ~Wet.Grass*Sprinkler*Rain)
#dg<-dag(~Cloudy + Sprinkler*Cloudy + Rain*Cloudy + Wet.Grass*Sprinkler*Rain)
#dg<-dag(~Cloudy + Sprinkler|Cloudy + Rain|Cloudy + Wet.Grass|Sprinkler*Rain)
#dg<-dag('Cloudy', c('Sprinkler', 'Cloudy'), c('Rain', 'Cloudy'), c('Wet.Grass', 'Sprinkler', 'Rain'))

nodes(dg)
edges(dg)
plot(dg)
parents('Sprinkler', dg)
children('Sprinkler', dg)
ancestors('Wet.Grass', dg)
ancestralSet('Wet.Grass', dg)
plot(ancestralGraph('Sprinkler', dg), attrs=list(node = list(fillcolor="lightgrey", fontcolor="red")))

adj<-matrix(c(0,0,0,0,1,0,0,0,1,0,0,0,0,1,1,0), ncol=4) # kolumnami
rownames(adj) <- colnames(adj) <- c('Cloudy', 'Sprinkler', 'Rain', 'Wet.Grass')
dg<-as(adj, "graphNEL")

# d-separation / conditional independence
# 'Wet.Grass' is a collider.
ggm::dSep(adj, 'Cloudy', 'Wet.Grass', 'Rain')
ggm::dSep(as(dg, 'matrix'), 'Cloudy', 'Wet.Grass', c('Sprinkler', 'Rain'))
ggm::dSep(adj, 'Sprinkler', 'Rain', NULL) # 'Cloudy'!
ggm::dSep(adj, 'Sprinkler', 'Rain', 'Cloudy')

# Conditional Probability Table
bool<-c('True', 'False')
cloudy<-cptable(~Cloudy, values=c(1, 1), levels=bool)
sprinkler<-cptable(~Sprinkler+Cloudy, values=c(1, 9, 5, 5), levels=bool)
rain<-cptable(~Rain+Cloudy, values=c(8, 2, 2, 8), levels=bool)
wet.grass<-cptable(~Wet.Grass+Sprinkler+Rain, values=c(99, 1, 90, 10, 90, 10, 1, 99), levels=bool)
plist<-compileCPT(list(cloudy, sprinkler, rain, wet.grass))
plist$Cloudy
plist$Rain
plist$Wet.Grass

# Build the netowrk
bn<-grain(plist)
summary(bn)
plot(bn)

# Querying the network
t(querygrain(bn, nodes=c('Sprinkler', 'Cloudy'), type='conditional'))
plist$Sprinkler
querygrain(bn, nodes=c('Sprinkler', 'Cloudy'), type='joint')
querygrain(bn, nodes='Sprinkler', type='marginal')
querygrain(bn, nodes=c('Wet.Grass', 'Cloudy'), type='joint')
t(querygrain(bn, c('Wet.Grass', 'Cloudy'), type='conditional'))
querygrain(bn, 'Wet.Grass', type='marginal')

e<-setEvidence(bn, evidence=list(Wet.Grass='True'))
querygrain(e)
e<-setEvidence(bn, nodes=c('Wet.Grass', 'Sprinkler'), states=c('True', 'False'))
querygrain(e, nodes=c('Cloudy', 'Rain'), type='joint')
t(querygrain(e, nodes=c('Cloudy', 'Rain'), type='conditional'))
