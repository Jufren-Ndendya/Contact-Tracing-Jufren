library(dplyr)
library(igraph)

setwd("D:/RabiesIHI")

animals <- read.csv("animal_bites.csv")
humans  <- read.csv("human_bites.csv")

# standardize species categories
animals$Host <- ifelse(grepl("dog", animals$Species, ignore.case=TRUE), "Dog",
                       ifelse(grepl("jackal|wildlife", animals$Species, ignore.case=TRUE), "Wildlife","Other"))

humans$Host <- ifelse(grepl("dog", humans$Attacking.species, ignore.case=TRUE), "Dog",
                      ifelse(grepl("jackal|wildlife", humans$Attacking.species, ignore.case=TRUE),"Wildlife","Other"))


animals <- animals %>% filter(Suspect=="Yes")
humans  <- humans  %>% filter(Rabid=="Yes")


table(animals$Host)

dog_cases <- animals %>% filter(Host=="Dog")
wild_cases <- animals %>% filter(Host=="Wildlife")

nD <- nrow(dog_cases)
nW <- nrow(wild_cases)

# approximate transmission counts
DD <- sum(animals$Host=="Dog") - 1
DW <- sum(animals$Host=="Wildlife")
WD <- round(0.2 * nW)   # placeholder if direct links unavailable
WW <- round(0.1 * nW)



mDD <- DD/nD
mDW <- DW/nD
mWD <- WD/nW
mWW <- WW/nW

M <- matrix(c(mDD,mDW,mWD,mWW), nrow=2, byrow=TRUE)

rownames(M) <- c("Dog","Wildlife")
colnames(M) <- c("Dog","Wildlife")

M


eig <- eigen(M)
lambda <- max(Re(eig$values))
lambda


if(mWW < 1){
  cat("Wildlife act as spillover hosts; rabies persistence is mainly driven by dogs.\n")
}else{
  cat("Wildlife may independently maintain rabies transmission.\n")
}



wildlife_contribution <- (mWD + mWW) / sum(M)
wildlife_contribution

library(igraph)

g <- graph_from_adjacency_matrix(M,
                                 weighted=TRUE,
                                 mode="directed")

plot(g,
     edge.width = E(g)$weight*10,
     vertex.size=35,
     vertex.color=c("orange","darkgreen"),
     main="Multi-host Rabies Transmission Network")

library(ggplot2)

host_counts <- animals %>%
  count(Host)

ggplot(host_counts, aes(x=Host, y=n, fill=Host)) +
  geom_bar(stat="identity") +
  labs(title="Distribution of Rabies Cases by Host Type",
       x="Host Species",
       y="Number of Cases") +
 # theme_minimal()
theme_classic()



library(reshape2)

M_df <- melt(M)

ggplot(M_df, aes(Var1, Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value,2)), color="white", size=5) +
  scale_fill_gradient(low="lightblue", high="darkred") +
  labs(title="Multi-host Transmission Matrix",
       x="Infecting Host",
       y="Newly Infected Host") +
  theme_classic()


library(igraph)

g <- graph_from_adjacency_matrix(M,
                                 mode="directed",
                                 weighted=TRUE)

plot(g,
     edge.width=E(g)$weight*10,
     vertex.color=c("orange","darkgreen"),
     vertex.size=35,
     main="Multi-host Rabies Transmission Network")



transmission_totals <- data.frame(
  Type=c("Dog→Dog","Dog→Wildlife","Wildlife→Dog","Wildlife→Wildlife"),
  Value=c(mDD,mDW,mWD,mWW)
)

ggplot(transmission_totals, aes(x=Type, y=Value, fill=Type)) +
  geom_bar(stat="identity") +
  labs(title="Cross-Species Rabies Transmission Rates",
       x="Transmission Pathway",
       y="Average Secondary Infections") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45,hjust=1))



wildlife_share <- (mWD + mWW) / sum(M)
dog_share <- (mDD + mDW) / sum(M)

role_df <- data.frame(
  Host=c("Dog-driven transmission","Wildlife-driven transmission"),
  Value=c(dog_share, wildlife_share)
)

ggplot(role_df, aes(x=Host, y=Value, fill=Host)) +
  geom_bar(stat="identity") +
  labs(title="Relative Contribution to Rabies Transmission",
       y="Proportion of Transmission") +
  theme_classic()


wildlife_R <- mWW

plot(c(0,1), c(0,1.5), type="n",
     xlab="Host Type",
     ylab="Reproduction Number",
     xaxt="n",
     main="Wildlife Transmission Potential")

axis(1, at=0.5, labels="Wildlife")

points(0.5, wildlife_R, pch=19, cex=2)
abline(h=1, lty=2, col="red")
text(0.5, wildlife_R+0.1, labels=round(wildlife_R,2))




####




































