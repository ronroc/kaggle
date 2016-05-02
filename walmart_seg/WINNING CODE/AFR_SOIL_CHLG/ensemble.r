source("rozw2.r")
source("rozw3.r")

source("rozw1Ca.r")
source("rozw1P.r")
source("rozw1pH.r")
source("rozw1SOC.r")
source("rozw1Sand.r")

Ca = read.csv("1stsolCa.csv")
P = read.csv("1stsolP.csv")
pH = read.csv("1stsolpH.csv")
SOC = read.csv("1stsolSOC.csv")
Sand = read.csv("1stsolSand.csv")

sol2 = read.csv("2ndsol.csv")
sol3 = read.csv("3rdsol.csv")

res_1 = Ca
res_1$P = P$P
res_1$pH = pH$pH
res_1$SOC = SOC$SOC
res_1$Sand = Sand$Sand

res_2 = sol2
res_2[,2:6] = 0.5 * (sol2[,2:6] + sol3[,2:6])

res = res_1
res[,2:6] = 0.5 * (res_1[,2:6] + res_2[,2:6])
write.csv(res, "final.csv", quote=FALSE, row.names=FALSE)
