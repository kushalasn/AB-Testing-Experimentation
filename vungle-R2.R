library(pwr)

vungle <- read.csv("vungle-ab.csv")

t.test(vungle$eRPM[31:60],vungle$eRPM[1:30])

t.test(vungle$eRPM[31:60],vungle$eRPM[1:30], paired = T)

t.test(vungle$eRPM[vungle$Strategy=="Vungle B"],vungle$eRPM[vungle$Strategy=="Vungle A"], paired = T)


# testing difference in Install per Impression

vungle$Impressions <- as.numeric(gsub(",", "", vungle$Impressions))
vungle$Installs <- as.numeric(gsub(",", "", vungle$Installs))


pool_prop <- sum(vungle$Installs)/sum(vungle$Impressions)

pool_var <- pool_prop*(1-pool_prop)*(1/sum(vungle$Impressions[1:30])+1/sum(vungle$Impressions[31:60]))
pool_se <- sqrt(pool_var)

diff <- sum(vungle$Installs[31:60])/sum(vungle$Impressions[31:60])-sum(vungle$Installs[1:30])/sum(vungle$Impressions[1:30])

t_stat <- diff/pool_se

# power of paired test
pwr.t.test(d=(0.111-0)/0.18,n=30,power= , sig.level=0.05, type = "paired")

# power of unpaired test
pwr.t.test(d=(0.111-0)/0.29,n=30,power= , sig.level=0.05)

#other power analysis
pwr.2p.test(h = ES.h(p1 = 0.001, p2 = 0.005), sig.level = 0.05, power = .80)

pwr.2p.test(h = ES.h(p1 = 0.001, p2 = 0.005), sig.level = 0.05, power = , n = 1000)

pwr.2p2n.test(h = ES.h(p1 = 0.001, p2 = 0.005), n1=2000, n2=, sig.level=0.05, power=0.8)

pwr.t.test(n =, d = 0.5, sig.level = 0.05, power = 0.8)

pwr.t2n.test(n1 = 100, n2 = , d = 0.5, sig.level = 0.05, power = 0.8)

#Std dev of diff of eRPMs = 0.184

pwr.t.test(d=(0.0335-0)/0.184,n=30,power= , sig.level=0.05,type="paired")

pwr.t.test(d=(0.111-0)/0.184,n=30,power= , sig.level=0.05,type="paired")

pwr.t.test(n =, d = 0.01, sig.level = 0.05, power = 0.8)

pwr.t.test(h=ES.h(p1=0.04,p2=0.12),sig.level=0.05,n1=6000,n2=6000)

