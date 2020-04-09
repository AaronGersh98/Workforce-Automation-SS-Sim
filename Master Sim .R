Init = as.matrix(ModelSIm)
Init[which(substring(Init[,1],5,5) == "S"),1] = paste(substr(Init[which(substring(Init[,1],5,5) == "S"),1],1,4),"B", sep = "")
Init[which(substring(Init[,1],5,5) == "U"),1] = paste(substr(Init[which(substring(Init[,1],5,5) == "U"),1],1,4),"R", sep = "")
auto_prob = as.matrix(prob)
set.seed(2)
C = 1
pr = 0
#Workforce Size
N = 200000
#Layoff Rate
L = rnorm(n = 47, mean = 0.0306041009463722, sd = .00079889)
#Long-run average unemployment rate
LRU = .055
#initial unemployment rate
init.u = .036
n = 20
#Number of working years
years = 47
#Downward Pressure on robot replaceable wages
D = c(rep(pr,n),rep(0,years-n-1))
#Need to discuss with Nan
AWI = c(4.7, 4.5, 4.2, 4.1, 4.2,4.2,4.2, 4.3, 4)
AWI = c(c(AWI),rnorm(n = 40, mean = 4, sd = .075))/100

#Number of groups
groups = length(Init[,1])
#Amount of wokers per group
workforce.size = round(N*as.numeric(Init[,3]))
N = sum(workforce.size)
capacity = cbind(workforce.size,matrix(0,groups,(years-1)))
names.row = NA
for (i in 1:groups) {
  names.row = c(names.row,rep(Init[i,1],workforce.size[i]))
}
names.row = names.row[-1]

major.class = unique(substr(Init[,1],1,3))
skill.class = c("B","R")

workforce.state = matrix(0,N,years)
rownames(workforce.state) = names.row

income = matrix(0, N, years)
rownames(income) = names.row

tot.income = matrix(0, groups, years)
rownames(tot.income) = Init[,1]

avg.income = matrix(0, groups, years)
rownames(avg.income) = Init[,1]
avg.income[,1] = as.numeric(Init[,8])

auto.loss = matrix(0,groups,years-1)
rownames(auto.loss) = Init[,1]

tot.loss = matrix(0,groups,years-1)
rownames(tot.loss) = Init[,1]

capacity = matrix(0,groups,years)
rownames(capacity) = Init[,1]

unemployed = matrix(0,groups,years)
rownames(unemployed) = Init[,1]

employed = matrix(0,groups,years)
rownames(employed) = Init[,1]

unemployed.check = matrix(0,groups,years)
rownames(unemployed.check) = Init[,1]

employed.check = matrix(0,groups,years)
rownames(employed.check) = Init[,1]

job.loss = matrix(0,groups,years-1)
rownames(job.loss) = Init[,1]

job.gain = matrix(0,groups,years-1)
rownames(job.gain) = Init[,1]

auto.income = matrix(0,groups,years-1)
rownames(auto.income) = Init[,1]

a.prob = matrix(0,groups,years-1)
rownames(a.prob) = Init[,1]

fstar.2 = matrix(0,groups,years-1)

####Initial Conditions#####

##INITITAL EMPLOYMENT STATES##
for (i in 1:groups) {
  U = sample(which(row.names(workforce.state) == Init[i,1]), size = round(workforce.size[i]*init.u))
  workforce.state[U,1] = 1
}


##INITIAL WAGES##
for (j in 1:N) {
  income[j,1] = abs(workforce.state[j,1]-1)*as.numeric(Init[which(Init == as.character(names.row[j])),8])
}

##INITIAL NUMBER OF WORKERS EMPLOYED IN EACH JOB CLASS##
for (i in 1:groups) {
  employed[i,1] =  length(workforce.state[which(workforce.state[,1] == 0 & 
                                                  row.names(workforce.state) == Init[i,1]), 1])
}

U.n = as.matrix(round(LRU*workforce.size + auto_prob*employed[,1]))
unemployed[,1] = workforce.size - employed[,1]

for (i in 1:groups) {
  unemployed[i,c(1:n)] = round(approx(x = c(unemployed[i,1],U.n[i]), n = n)$y)
  u = rnorm(n = (years - n), mean = (U.n/workforce.size), sd = .00055127336)
  unemployed[i,c((n+1):years)] = round(workforce.size[i]*rnorm(n = (years - n), mean = (U.n[i]/workforce.size[i]), sd = .00055127336))
}

employed = workforce.size - unemployed
auto = matrix(0,groups,n)
for (y in 2:n) {
  auto[,y] = round((unemployed[,y-1]/unemployed[,n-1])*as.matrix(auto_prob)*employed[,1])
}
for (i in 1:groups) {
  auto.loss[i,c(1:(n-1))] = diff(auto[i,])
  a.prob[i,c(1:(n-1))] = auto.loss[i,c(1:(n-1))]/employed[i,c(1:(n-1))]
}

#####EMPLOYMENT AND INCOME SIMULATION, YEARS 2-47####
for (y in 2:years) {
  for (i in 1:groups) {
    #take a random sample of a given job class who were employed in year y-1 to become unemployed in year y
    eu = sample(which(workforce.state[,(y-1)] == 0  & row.names(workforce.state) == Init[i,1]), size = min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))
    job.loss[i,y-1] = length(eu)
    workforce.state[eu,y] = 1
    
    fstar.2[i,y-1] = 1 - (unemployed[i,y] - min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))/unemployed[i,y-1]
    
    #take a random sample of a given job class who were unemployed in year y-1 to remain unemployed in year y
    uu = sample(which(workforce.state[,(y-1)] == 1 & row.names(workforce.state) == Init[i,1]), size = round(unemployed[i,y-1]*(1-fstar.2[i,y-1])))
    job.gain[i,y-1] = unemployed[i,y-1] - length(uu)
    workforce.state[uu,y] = 1
    
    
    employed.check[i,y] = length(which(workforce.state[,y] == 0 & row.names(workforce.state) == Init[i,1]))
  }  
  #unemployed[,y].check = workforce.size - employed.check[,y]
  
  for (j in 1:14) {
    if (major.class[j] != "LIF") {
      auto.income[which(row.names(auto.income) == paste(major.class[j],"B",sep ="-")),y-1] = 
        auto.loss[which(row.names(auto.loss) == paste(major.class[j],"R",sep ="-")),y-1] * C*avg.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1]
      auto.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1] = 0
    }
  }
  
  for (i in 1:groups) {
    tot.income[i,y-1] = sum(income[which(row.names(income)==Init[i,1]),y-1])
    
    #avg. income in year y = [(1+inflation)*(total income year y - lost or gained income from automation)*(urrent employment/prior year employment)]/(current employment)
    if (substr(Init[i,1],5,5) == "R"){
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1])*(1-D[y-1])
    }else {
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1]+(auto.income[i,y-1]/max(employed[i,y],.01)))
    }
    income[which(row.names(income) == Init[i,1]),y] =
      abs(workforce.state[which(row.names(income) == Init[i,1]),y]-1)*avg.income[i,y]
    
    if (y == max(years)){
      tot.income[i,y] = sum(income[which(row.names(income)==Init[i,1]),y])
    }
    
  }
}    

####EMPLOYMENT ANALYSIS####
urate.2 = apply(unemployed,2,sum)/N
plot(urate.2, type = "l")

ss.eligibility.2 = years - apply(workforce.state,1,sum)
##State 0 = noneligible, State 1 = elegible
eligibility.2 = rep(NA,N)
eligibility.2[which(ss.eligibility.2 < 10)] = 0
eligibility.2[which(ss.eligibility.2 > 9)] = 1
#fraction of workforce eligible for OASI
percent.eligible.2 = sum(eligibility.2)/N

#Final unemployment rates by group
ufinal = employed[,47]/workforce.size
barplot(ufinal, ylim = c(0,1), ylab = "Long Run Unemployment", las = 2, cex.axis = 1.5, cex.lab= 1.5)

####INFLATION ANALYSIS####
g.2 = rep(NA,years - 1)
AWI.act = rep(NA, years)
#Adjusted AWI, average salary of everyone working
AWI.act[1] = mean(income[which(income[,1] != 0),1])  

for (y in 1:(years-1)) {
  AWI.act[y+1] = mean(income[which(income[,y+1]!=0),y+1])  
  g.2[y] = (AWI.act[y+1]-AWI.act[y])/AWI.act[y]
}

GINI.2 = rep(NA,y)
GDP.2 = apply(income,2,sum)
for (y in 1:47) {
  sorted.income = sort(x = income[,y])
  percent.income = rep(NA,N)
  percent.income[1] = sorted.income[1]/GDP.2[y]
  B = 0
  for (j in 2:N) {
    percent.income[j] = percent.income[j-1] + (sorted.income[j]/GDP.2[y])
    B = B + percent.income[j]/N
  }
  GINI.2[y] = 1-2*B
}

b.AWI.2 = rep(NA,years)
b.growth.2 = rep(NA,years-1)
b.AWI.2[1] = sum(income[which(substr(row.names(income),5,5)) == "B"],1)
for (y in 2:years) {
  b.AWI.2[y] = sum(income[which(substr(row.names(income),5,5)) == "B"],y)
  b.growth.2[y-1] = (b.AWI.2[y]/b.AWI.2[y-1]) - 1 
}

####TAX REVENUE####
WBL.2 = rep(NA,years)
WBL.2[1] = 137700

for (y in 2:years) {
  WBL.2[y] = WBL.2[y-1]*(1+g.2[y-1])
}
tax.base = matrix(0,N,years)
tax.base.m = matrix(0,N,12*years)
rownames(tax.base) = names.row
for (y in 1:years) {
  for (j in 1:N) {
    tax.base[j,y] = min(income[j,y],WBL.2[y])
    tax.base.m[j,c((1+12*(y-1)):(12*y))] = min(income[j,y],WBL.2[y])/12
  }
}
tot.tax = apply(tax.base, 2, sum)
tot.tax.m = apply(tax.base.m, 2, sum)
taxable.percent.2 = tot.tax/apply(tot.income, 2, sum)
plot(taxable.percent.2,type = "l")


tyield = .025
accum = rep(NA,years*12)
fundsize.t = rep(NA,years*12+1)
fundsize.t[1] = 0
for (m in 1:(12*years)) {
  accum[m] = (1+tyield)^((years*12-m)/12)
  fundsize.t[m+1] = tot.tax[m] + fundsize.t[m]*(1 + tyield)^(1/12)
}

fundsize.2 = t(accum)%*%tot.tax.m

class.tax = matrix(0,groups,years)
rownames(class.tax) = Init[,1]

#total tax base by group per year
for (y in 1:years) {
  for (i in 1:groups) {
    class.tax[i,y] = sum(tax.base[which(row.names(income)==Init[i,1]),y])
  } 
}

#### Benefit Calculations ####
index = rep(NA, years)
index[years] = 1
z = 1+g.2
for (y in 1:(years-1)) {
  index[y] = 1/prod(z[y:(years-1)])
}

index.income = matrix(0,N,years)
rownames(index.income) = names.row
index.income[,years] = tax.base[,years]
for (y in 1:(years-1)) {
  index.income[,y] = tax.base[,y]/index[y]
}

avg.index.2 = matrix(0,groups,years)
rownames(avg.index.2) = Init[,1]
for (y in 1:years) {
  for (i in 1:groups) {
    avg.index.2[i,y] = sum(index.income[which(row.names(index.income)==Init[i,1]),y])/employed[i,y]
  } 
}

sorted.wages = rep(NA,years)
AIME.2 = matrix(NA,N,1)
for (j in 1:N) {
  sorted.wages = index.income[j,order(index.income[j,], decreasing = TRUE)]
  AIME.2[j] = mean(sorted.wages[1:35])/12
}
row.names(AIME.2) = names.row

#Calculate bend points by indexing 2020 bend points 47 years ahead
B1 = 960/index[1]
B2 = 5785/index[1]

PIA.2 = matrix(NA,N,1)
row.names(PIA.2) = names.row
for (j in 1:N) {
  PIA.2[j] = (.9*(min(B1,AIME.2[j])-0)+.32*(min(B2,AIME.2[j])-min(B1,AIME.2[j]))+.15*(AIME.2[j]-min(AIME.2[j],B2)))*eligibility.2[j]
}

RR.2 = PIA.2/AIME.2

####Annuity Calculation####
max.life = 110-(20+years)
EPV = rep(NA,N)
#need to figure out standard deviation
COLA = rnorm(n = 12*max.life, mean = 1.026, sd = 0.0291594016926052)
COLA = COLA^(1/12)
#need to confirm
int = (1.025)^(1/12)
vstar = rep(NA, 12*max.life)
px = 1-Mort$qx
px = px[67:110]
qx = 1-px
tpx = rep(NA,12*max.life+1)
tpx[1] = 1
ax = 0
for (y in 1:max.life) {
  for (m in 1:12) {
    vstar[m+12*(y-1)] = prod(COLA[1:(m+12*(y-1))])*(int)^(-(m+12*(y-1)-1))
    ax = ax + (1/12)*tpx[m+12*(y-1)]*vstar[m+12*(y-1)]
    #UDD Assumption
    if (y > 1) {
      tpx[m+12*(y-1)+1] = (1-(m/12)*qx[y])*prod(px[1:y])
    } else {
      tpx[m+12*(y-1)+1] = 1-(m/12)*qx[1]
    }
  }
}
ax
EPV.2 = 12*ax*PIA.2
tot.EPV.2 = sum(EPV.2)
taxrate.2 = as.numeric(tot.EPV.2/fundsize.2)
taxrate.2

tax.con = matrix(0,N,12*years+1)
for (m in 1:(12*years)) {
  tax.con[,m+1] = tax.con[,m]*(1+tyield)^(1/12)+(taxrate.2)*tax.base.m[,m]
}

total.con = tax.con[,(12*years+1)]
uneligible.con.2 = sum(total.con[which(eligibility.2 == 0)])
uneligible.con.2/sum(total.con)

fund.alt.2 = fundsize.2 - uneligible.con.2
tax.alt.2  = as.numeric(tot.EPV.2/fund.alt.2)
tax.alt.2

NHCE.2 = rep(NA,years)
for (y in 1:years) {
  NHCE.2[y] = length(which(income[,y] < WBL.2[y] & income[,y] != 0))/sum(employed[,y])
}

#Workforce Size
N = 200000
#Layoff Rate
L = rnorm(n = 47, mean = 0.0306041009463722, sd = .00079889)
#Long-run average unemployment rate
LRU = .055
#initial unemployment rate
init.u = .036
n = 30
#Need to discuss with Nan
AWI = c(4.7, 4.5, 4.2, 4.1, 4.2,4.2,4.2, 4.3, 4)
AWI = c(c(AWI),rnorm(n = 40, mean = 4, sd = .075))/100
#Number of working years
years = 47
D = c(rep(pr,n),rep(0,years-n-1))
#Number of groups
groups = length(Init[,1])
#Amount of wokers per group
workforce.size = round(N*as.numeric(Init[,3]))
N = sum(workforce.size)
capacity = cbind(workforce.size,matrix(0,groups,(years-1)))
names.row = NA
for (i in 1:groups) {
  names.row = c(names.row,rep(Init[i,1],workforce.size[i]))
}
names.row = names.row[-1]

major.class = unique(substr(Init[,1],1,3))
skill.class = c("B","R")

workforce.state = matrix(0,N,years)
rownames(workforce.state) = names.row

income = matrix(0, N, years)
rownames(income) = names.row

tot.income = matrix(0, groups, years)
rownames(tot.income) = Init[,1]

avg.income = matrix(0, groups, years)
rownames(avg.income) = Init[,1]
avg.income[,1] = as.numeric(Init[,8])

auto.loss = matrix(0,groups,years-1)
rownames(auto.loss) = Init[,1]

tot.loss = matrix(0,groups,years-1)
rownames(tot.loss) = Init[,1]

capacity = matrix(0,groups,years)
rownames(capacity) = Init[,1]

unemployed = matrix(0,groups,years)
rownames(unemployed) = Init[,1]

employed = matrix(0,groups,years)
rownames(employed) = Init[,1]

unemployed.check = matrix(0,groups,years)
rownames(unemployed.check) = Init[,1]

employed.check = matrix(0,groups,years)
rownames(employed.check) = Init[,1]

job.loss = matrix(0,groups,years-1)
rownames(job.loss) = Init[,1]

job.gain = matrix(0,groups,years-1)
rownames(job.gain) = Init[,1]

auto.income = matrix(0,groups,years-1)
rownames(auto.income) = Init[,1]

a.prob = matrix(0,groups,years-1)
rownames(a.prob) = Init[,1]

fstar.3 = matrix(0,groups,years-1)

####Initial Conditions#####

##INITITAL EMPLOYMENT STATES##

for (i in 1:groups) {
  U = sample(which(row.names(workforce.state) == Init[i,1]), size = round(workforce.size[i]*init.u))
  workforce.state[U,1] = 1
}


##INITIAL WAGES##
for (j in 1:N) {
  income[j,1] = abs(workforce.state[j,1]-1)*as.numeric(Init[which(Init == as.character(names.row[j])),8])
}

##INITIAL NUMBER OF WORKERS EMPLOYED IN EACH JOB CLASS##
for (i in 1:groups) {
  employed[i,1] =  length(workforce.state[which(workforce.state[,1] == 0 & 
                                                  row.names(workforce.state) == Init[i,1]), 1])
}

U.n = as.matrix(round(LRU*workforce.size + auto_prob*employed[,1]))
unemployed[,1] = workforce.size - employed[,1]

for (i in 1:groups) {
  unemployed[i,c(1:n)] = round(approx(x = c(unemployed[i,1],U.n[i]), n = n)$y)
  u = rnorm(n = (years - n), mean = (U.n/workforce.size), sd = .00055127336)
  unemployed[i,c((n+1):years)] = round(workforce.size[i]*rnorm(n = (years - n), mean = (U.n[i]/workforce.size[i]), sd = .00055127336))
}

employed = workforce.size - unemployed
auto = matrix(0,groups,n)
for (y in 2:n) {
  auto[,y] = round((unemployed[,y-1]/unemployed[,n-1])*as.matrix(auto_prob)*employed[,1])
}
for (i in 1:groups) {
  auto.loss[i,c(1:(n-1))] = diff(auto[i,])
  a.prob[i,c(1:(n-1))] = auto.loss[i,c(1:(n-1))]/employed[i,c(1:(n-1))]
}

#####EMPLOYMENT AND INCOME SIMULATION, YEARS 2-47####
for (y in 2:years) {
  for (i in 1:groups) {
    #take a random sample of a given job class who were employed in year y-1 to become unemployed in year y
    eu = sample(which(workforce.state[,(y-1)] == 0  & row.names(workforce.state) == Init[i,1]), size = min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))
    job.loss[i,y-1] = length(eu)
    workforce.state[eu,y] = 1
    
    fstar.3[i,y-1] = 1 - (unemployed[i,y] - min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))/unemployed[i,y-1]
    
    #take a random sample of a given job class who were unemployed in year y-1 to remain unemployed in year y
    uu = sample(which(workforce.state[,(y-1)] == 1 & row.names(workforce.state) == Init[i,1]), size = round(unemployed[i,y-1]*(1-fstar.3[i,y-1])))
    job.gain[i,y-1] = unemployed[i,y-1] - length(uu)
    workforce.state[uu,y] = 1
    
    
    employed.check[i,y] = length(which(workforce.state[,y] == 0 & row.names(workforce.state) == Init[i,1]))
  }  
  #unemployed[,y].check = workforce.size - employed.check[,y]
  
  for (j in 1:14) {
    if (major.class[j] != "LIF") {
      auto.income[which(row.names(auto.income) == paste(major.class[j],"B",sep ="-")),y-1] = 
        C*auto.loss[which(row.names(auto.loss) == paste(major.class[j],"R",sep ="-")),y-1] * avg.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1]
      auto.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1] = 0
    }
  }
  
  for (i in 1:groups) {
    tot.income[i,y-1] = sum(income[which(row.names(income)==Init[i,1]),y-1])
    
    #avg. income in year y = [(1+inflation)*(total income year y - lost or gained income from automation)*(urrent employment/prior year employment)]/(current employment)
    if (substr(Init[i,1],5,5) == "R"){
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1])*(1-D[y-1])
    }else {
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1]+(auto.income[i,y-1]/max(employed[i,y],.01)))
    }    
    income[which(row.names(income) == Init[i,1]),y] =
      abs(workforce.state[which(row.names(income) == Init[i,1]),y]-1)*avg.income[i,y]
    
    if (y == max(years)){
      tot.income[i,y] = sum(income[which(row.names(income)==Init[i,1]),y])
    }
    
  }
}    



####EMPLOYMENT ANALYSIS####
urate.3 = apply(unemployed,2,sum)/N
plot(urate.3, type = "l")

ss.eligibility.3 = years - apply(workforce.state,1,sum)
##State 0 = noneligible, State 1 = elegible
eligibility.3 = rep(NA,N)
eligibility.3[which(ss.eligibility.3 < 10)] = 0
eligibility.3[which(ss.eligibility.3 > 9)] = 1
#fraction of workforce eligible for OASI
percent.eligible.3 = sum(eligibility.3)/N

#Final unemployment rates by group
ufinal = rep(NA,groups)
for (i in 1:groups) {
  ufinal[i] = 1 - employed[i,47]/workforce.size[i]
}
barplot(ufinal)

####INFLATION ANALYSIS####
g.3 = rep(NA,years - 1)
AWI.act = rep(NA, years)
#Adjusted AWI, average salary of everyone working
AWI.act[1] = mean(income[which(income[,1] != 0),1])  

for (y in 1:(years-1)) {
  AWI.act[y+1] = mean(income[which(income[,y+1]!=0),y+1])  
  g.3[y] = (AWI.act[y+1]-AWI.act[y])/AWI.act[y]
}

wage.inflation.3 = matrix(0,groups,years-1)
for (y in 1:years-1) {
  wage.inflation.5[,y] = avg.income[,y+1]/avg.income[,y] - 1
}

pallete = rainbow(n = years - 1)
plot(AWI, type = "l", lwd = 5,xlim = c(0,46) ,ylim = c(-.5,.5))
for (i in 1:groups) {
  lines(wage.inflation.5[i,], col = pallete[i])
}

GINI.3 = rep(NA,y)
GDP.3 = apply(income,2,sum)
for (y in 1:47) {
  sorted.income = sort(x = income[,y])
  percent.income = rep(NA,N)
  percent.income[1] = sorted.income[1]/GDP.3[y]
  B = 0
  for (j in 2:N) {
    percent.income[j] = percent.income[j-1] + (sorted.income[j]/GDP.3[y])
    B = B + percent.income[j]/N
  }
  GINI.3[y] = 1-2*B
}
plot(x = c(1:years), y = GINI.3, type = "l") 

b.AWI.3 = rep(NA,years)
b.growth.3 = rep(NA,years-1)
b.AWI.3[1] = sum(income[which(substr(row.names(income),5,5)) == "B"],1)
for (y in 2:years) {
  b.AWI.3[y] = sum(income[which(substr(row.names(income),5,5)) == "B"],y)
  b.growth.3[y-1] = (b.AWI.3[y]/b.AWI.3[y-1]) - 1 
}

####TAX REVENUE####
WBL.3 = rep(NA,years)
WBL.3[1] = 137700

for (y in 2:years) {
  WBL.3[y] = WBL.3[y-1]*(1+g.3[y-1])
}
tax.base = matrix(0,N,years)
tax.base.m = matrix(0,N,12*years)
rownames(tax.base) = names.row
for (y in 1:years) {
  for (j in 1:N) {
    tax.base[j,y] = min(income[j,y],WBL.3[y])
    tax.base.m[j,c((1+12*(y-1)):(12*y))] = min(income[j,y],WBL.3[y])/12
  }
}
tot.tax = apply(tax.base, 2, sum)
tot.tax.m = apply(tax.base.m, 2, sum)
taxable.percent.3 = tot.tax/apply(tot.income, 2, sum)
plot(taxable.percent.3,type = "l")


tyield = .025
accum = rep(NA,years*12)
fundsize.t = rep(NA,years*12+1)
fundsize.t[1] = 0
for (m in 1:(12*years)) {
  accum[m] = (1+tyield)^((years*12-m)/12)
  fundsize.t[m+1] = tot.tax[m] + fundsize.t[m]*(1 + tyield)^(1/12)
}

fundsize.3 = t(accum)%*%tot.tax.m

class.tax = matrix(0,groups,years)
rownames(class.tax) = Init[,1]

#total tax base by group per year
for (y in 1:years) {
  for (i in 1:groups) {
    class.tax[i,y] = sum(tax.base[which(row.names(income)==Init[i,1]),y])
  } 
}

#### Benefit Calculations ####
index = rep(NA, years)
index[years] = 1
z = 1+g.3
for (y in 1:(years-1)) {
  index[y] = 1/prod(z[y:(years-1)])
}


index.income = matrix(0,N,years)
rownames(index.income) = names.row
index.income[,years] = tax.base[,years]
for (y in 1:(years-1)) {
  index.income[,y] = tax.base[,y]/index[y]
}

avg.index.3 = matrix(0,groups,years)
rownames(avg.index.3) = Init[,1]
for (y in 1:years) {
  for (i in 1:groups) {
    avg.index.3[i,y] = sum(index.income[which(row.names(index.income)==Init[i,1]),y])/employed[i,y]
  } 
}

sorted.wages = rep(NA,years)
AIME.3 = matrix(NA,N,1)
for (j in 1:N) {
  sorted.wages = index.income[j,order(index.income[j,], decreasing = TRUE)]
  AIME.3[j] = mean(sorted.wages[1:35])/12
}
row.names(AIME.3) = names.row

#Calculate bend points by indexing 2020 bend points 47 years ahead
B1 = 960/index[1]
B2 = 5785/index[1]

PIA.3 = matrix(NA,N,1)
row.names(PIA.3) = names.row
for (j in 1:N) {
  PIA.3[j] = (.9*(min(B1,AIME.3[j])-0)+.32*(min(B2,AIME.3[j])-min(B1,AIME.3[j]))+.15*(AIME.3[j]-min(AIME.3[j],B2)))*eligibility.3[j]
}

RR.3 = PIA.3/AIME.3

####Annuity Calculation####
max.life = 110-(20+years)
EPV = rep(NA,N)
#need to figure out standard deviation
COLA = rnorm(n = 12*max.life, mean = 1.026, sd = 0.0291594016926052)
COLA = COLA^(1/12)
#need to confirm
int = (1.025)^(1/12)
vstar = rep(NA, 12*max.life)
px = 1-Mort$qx
px = px[67:110]
qx = 1-px
tpx = rep(NA,12*max.life+1)
tpx[1] = 1
ax = 0
for (y in 1:max.life) {
  for (m in 1:12) {
    vstar[m+12*(y-1)] = prod(COLA[1:(m+12*(y-1))])*(int)^(-(m+12*(y-1)-1))
    ax = ax + (1/12)*tpx[m+12*(y-1)]*vstar[m+12*(y-1)]
    #UDD Assumption
    if (y > 1) {
      tpx[m+12*(y-1)+1] = (1-(m/12)*qx[y])*prod(px[1:y])
    } else {
      tpx[m+12*(y-1)+1] = 1-(m/12)*qx[1]
    }
  }
}
ax
EPV.3 = 12*ax*PIA.3
tot.EPV.3 = sum(EPV.3)
taxrate.3 = as.numeric(tot.EPV.3/fundsize.3)
taxrate.3

tax.con = matrix(0,N,12*years+1)
for (m in 1:(12*years)) {
  tax.con[,m+1] = tax.con[,m]*(1+tyield)^(1/12)+(taxrate.3)*tax.base.m[,m]
}

total.con = tax.con[,(12*years+1)]
uneligible.con.3 = sum(total.con[which(eligibility.3 == 0)])
uneligible.con.3/sum(total.con)

fund.alt.3 = fundsize.3 - uneligible.con.3
tax.alt.3  = as.numeric(tot.EPV.3/fund.alt.3)
tax.alt.3

NHCE.3 = rep(NA,years)
for (y in 1:years) {
  NHCE.3[y] = length(which(income[,y] < WBL.3[y] & income[,y] != 0))/sum(employed[,y])
}


#Workforce Size
N = 200000
#Layoff Rate
L = rnorm(n = 47, mean = 0.0306041009463722, sd = .00079889)
#Long-run average unemployment rate
LRU = .055
#initial unemployment rate
init.u = .036
n = 40
#Need to discuss with Nan
AWI = c(4.7, 4.5, 4.2, 4.1, 4.2,4.2,4.2, 4.3, 4)
AWI = c(c(AWI),rnorm(n = 40, mean = 4, sd = .075))/100
#Number of working years
years = 47
D = c(rep(pr,n),rep(0,years-n-1))
#Number of groups
groups = length(Init[,1])
#Amount of wokers per group
workforce.size = round(N*as.numeric(Init[,3]))
N = sum(workforce.size)
capacity = cbind(workforce.size,matrix(0,groups,(years-1)))
names.row = NA
for (i in 1:groups) {
  names.row = c(names.row,rep(Init[i,1],workforce.size[i]))
}
names.row = names.row[-1]

major.class = unique(substr(Init[,1],1,3))
skill.class = c("B","R")

workforce.state = matrix(0,N,years)
rownames(workforce.state) = names.row

income = matrix(0, N, years)
rownames(income) = names.row

tot.income = matrix(0, groups, years)
rownames(tot.income) = Init[,1]

avg.income = matrix(0, groups, years)
rownames(avg.income) = Init[,1]
avg.income[,1] = as.numeric(Init[,8])

auto.loss = matrix(0,groups,years-1)
rownames(auto.loss) = Init[,1]

tot.loss = matrix(0,groups,years-1)
rownames(tot.loss) = Init[,1]

capacity = matrix(0,groups,years)
rownames(capacity) = Init[,1]

unemployed = matrix(0,groups,years)
rownames(unemployed) = Init[,1]

employed = matrix(0,groups,years)
rownames(employed) = Init[,1]

unemployed.check = matrix(0,groups,years)
rownames(unemployed.check) = Init[,1]

employed.check = matrix(0,groups,years)
rownames(employed.check) = Init[,1]

job.loss = matrix(0,groups,years-1)
rownames(job.loss) = Init[,1]

job.gain = matrix(0,groups,years-1)
rownames(job.gain) = Init[,1]

auto.income = matrix(0,groups,years-1)
rownames(auto.income) = Init[,1]

a.prob = matrix(0,groups,years-1)
rownames(a.prob) = Init[,1]

fstar.4 = matrix(0,groups,years-1)

####Initial Conditions#####

##INITITAL EMPLOYMENT STATES##

for (i in 1:groups) {
  U = sample(which(row.names(workforce.state) == Init[i,1]), size = round(workforce.size[i]*init.u))
  workforce.state[U,1] = 1
}


##INITIAL WAGES##
for (j in 1:N) {
  income[j,1] = abs(workforce.state[j,1]-1)*as.numeric(Init[which(Init == as.character(names.row[j])),8])
}

##INITIAL NUMBER OF WORKERS EMPLOYED IN EACH JOB CLASS##
for (i in 1:groups) {
  employed[i,1] =  length(workforce.state[which(workforce.state[,1] == 0 & 
                                                  row.names(workforce.state) == Init[i,1]), 1])
}

U.n = as.matrix(round(LRU*workforce.size + auto_prob*employed[,1]))
unemployed[,1] = workforce.size - employed[,1]

for (i in 1:groups) {
  unemployed[i,c(1:n)] = round(approx(x = c(unemployed[i,1],U.n[i]), n = n)$y)
  u = rnorm(n = (years - n), mean = (U.n/workforce.size), sd = .00055127336)
  unemployed[i,c((n+1):years)] = round(workforce.size[i]*rnorm(n = (years - n), mean = (U.n[i]/workforce.size[i]), sd = .00055127336))
}

employed = workforce.size - unemployed
auto = matrix(0,groups,n)
for (y in 2:n) {
  auto[,y] = round((unemployed[,y-1]/unemployed[,n-1])*as.matrix(auto_prob)*employed[,1])
}
for (i in 1:groups) {
  auto.loss[i,c(1:(n-1))] = diff(auto[i,])
  a.prob[i,c(1:(n-1))] = auto.loss[i,c(1:(n-1))]/employed[i,c(1:(n-1))]
}

#####EMPLOYMENT AND INCOME SIMULATION, YEARS 2-47####
for (y in 2:years) {
  for (i in 1:groups) {
    #take a random sample of a given job class who were employed in year y-1 to become unemployed in year y
    eu = sample(which(workforce.state[,(y-1)] == 0  & row.names(workforce.state) == Init[i,1]), size = min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))
    job.loss[i,y-1] = length(eu)
    workforce.state[eu,y] = 1
    
    fstar.4[i,y-1] = 1 - (unemployed[i,y] - min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))/unemployed[i,y-1]
    
    #take a random sample of a given job class who were unemployed in year y-1 to remain unemployed in year y
    uu = sample(which(workforce.state[,(y-1)] == 1 & row.names(workforce.state) == Init[i,1]), size = round(unemployed[i,y-1]*(1-fstar.4[i,y-1])))
    job.gain[i,y-1] = unemployed[i,y-1] - length(uu)
    workforce.state[uu,y] = 1
    
    
    employed.check[i,y] = length(which(workforce.state[,y] == 0 & row.names(workforce.state) == Init[i,1]))
  }  
  #unemployed[,y].check = workforce.size - employed.check[,y]
  
  for (j in 1:14) {
    if (major.class[j] != "LIF") {
      auto.income[which(row.names(auto.income) == paste(major.class[j],"B",sep ="-")),y-1] = 
        C*auto.loss[which(row.names(auto.loss) == paste(major.class[j],"R",sep ="-")),y-1] * avg.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1]
      auto.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1] = 0
    }
  }
  
  for (i in 1:groups) {
    tot.income[i,y-1] = sum(income[which(row.names(income)==Init[i,1]),y-1])
    
    #avg. income in year y = [(1+inflation)*(total income year y - lost or gained income from automation)*(urrent employment/prior year employment)]/(current employment)
    if (substr(Init[i,1],5,5) == "R"){
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1])*(1-D[y-1])
    }else {
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1]+(auto.income[i,y-1]/max(employed[i,y],.01)))
    }  
    income[which(row.names(income) == Init[i,1]),y] =
      abs(workforce.state[which(row.names(income) == Init[i,1]),y]-1)*avg.income[i,y]
    
    if (y == max(years)){
      tot.income[i,y] = sum(income[which(row.names(income)==Init[i,1]),y])
    }
    
  }
}    



####EMPLOYMENT ANALYSIS####
urate.4 = apply(unemployed,2,sum)/N
plot(urate.4, type = "l")

ss.eligibility.4 = years - apply(workforce.state,1,sum)
##State 0 = noneligible, State 1 = elegible
eligibility.4 = rep(NA,N)
eligibility.4[which(ss.eligibility.4 < 10)] = 0
eligibility.4[which(ss.eligibility.4 > 9)] = 1
#fraction of workforce eligible for OASI
percent.eligible.4 = sum(eligibility.4)/N

#Final unemployment rates by group
ufinal = rep(NA,groups)
for (i in 1:groups) {
  ufinal[i] = 1 - employed[i,47]/workforce.size[i]
}
barplot(ufinal)

####INFLATION ANALYSIS####
g.4 = rep(NA,years - 1)
AWI.act = rep(NA, years)
#Adjusted AWI, average salary of everyone working
AWI.act[1] = mean(income[which(income[,1] != 0),1])  

for (y in 1:(years-1)) {
  AWI.act[y+1] = mean(income[which(income[,y+1]!=0),y+1])  
  g.4[y] = (AWI.act[y+1]-AWI.act[y])/AWI.act[y]
}

wage.inflation.4 = matrix(0,groups,years-1)
for (y in 1:years-1) {
  wage.inflation.4[,y] = avg.income[,y+1]/avg.income[,y] - 1
}

pallete = rainbow(n = years - 1)
plot(AWI, type = "l", lwd = 5,xlim = c(0,46) ,ylim = c(-.5,.5))
for (i in 1:groups) {
  lines(wage.inflation.4[i,], col = pallete[i])
}

GINI.4 = rep(NA,y)
GDP.4 = apply(income,2,sum)
for (y in 1:47) {
  sorted.income = sort(x = income[,y])
  percent.income = rep(NA,N)
  percent.income[1] = sorted.income[1]/GDP.4[y]
  B = 0
  for (j in 2:N) {
    percent.income[j] = percent.income[j-1] + (sorted.income[j]/GDP.4[y])
    B = B + percent.income[j]/N
  }
  GINI.4[y] = 1-2*B
}
plot(x = c(1:years), y = GINI.4, type = "l") 

b.AWI.4 = rep(NA,years)
b.growth.4 = rep(NA,years-1)
b.AWI.4[1] = sum(income[which(substr(row.names(income),5,5)) == "B"],1)
for (y in 2:years) {
  b.AWI.4[y] = sum(income[which(substr(row.names(income),5,5)) == "B"],y)
  b.growth.4[y-1] = (b.AWI.4[y]/b.AWI.4[y-1]) - 1 
}

####TAX REVENUE####
WBL.4 = rep(NA,years)
WBL.4[1] = 137700

for (y in 2:years) {
  WBL.4[y] = WBL.4[y-1]*(1+g.4[y-1])
}
tax.base = matrix(0,N,years)
tax.base.m = matrix(0,N,12*years)
rownames(tax.base) = names.row
for (y in 1:years) {
  for (j in 1:N) {
    tax.base[j,y] = min(income[j,y],WBL.4[y])
    tax.base.m[j,c((1+12*(y-1)):(12*y))] = min(income[j,y],WBL.4[y])/12
  }
}
tot.tax = apply(tax.base, 2, sum)
tot.tax.m = apply(tax.base.m, 2, sum)
taxable.percent.4 = tot.tax/apply(tot.income, 2, sum)
plot(taxable.percent.4,type = "l")


tyield = .025
accum = rep(NA,years*12)
fundsize.t = rep(NA,years*12+1)
fundsize.t[1] = 0
for (m in 1:(12*years)) {
  accum[m] = (1+tyield)^((years*12-m)/12)
  fundsize.t[m+1] = tot.tax[m] + fundsize.t[m]*(1 + tyield)^(1/12)
}

fundsize.4 = t(accum)%*%tot.tax.m

class.tax = matrix(0,groups,years)
rownames(class.tax) = Init[,1]

#total tax base by group per year
for (y in 1:years) {
  for (i in 1:groups) {
    class.tax[i,y] = sum(tax.base[which(row.names(income)==Init[i,1]),y])
  } 
}

#### Benefit Calculations ####
index = rep(NA, years)
index[years] = 1
z = 1+g.4
for (y in 1:(years-1)) {
  index[y] = 1/prod(z[y:(years-1)])
}


index.income = matrix(0,N,years)
rownames(index.income) = names.row
index.income[,years] = tax.base[,years]
for (y in 1:(years-1)) {
  index.income[,y] = tax.base[,y]/index[y]
}

avg.index.4 = matrix(0,groups,years)
rownames(avg.index.4) = Init[,1]
for (y in 1:years) {
  for (i in 1:groups) {
    avg.index.4[i,y] = sum(index.income[which(row.names(index.income)==Init[i,1]),y])/employed[i,y]
  } 
}

sorted.wages = rep(NA,years)
AIME.4 = matrix(NA,N,1)
for (j in 1:N) {
  sorted.wages = index.income[j,order(index.income[j,], decreasing = TRUE)]
  AIME.4[j] = mean(sorted.wages[1:35])/12
}
row.names(AIME.4) = names.row

#Calculate bend points by indexing 2020 bend points 47 years ahead
B1 = 960/index[1]
B2 = 5785/index[1]

PIA.4 = matrix(NA,N,1)
row.names(PIA.4) = names.row
for (j in 1:N) {
  PIA.4[j] = (.9*(min(B1,AIME.4[j])-0)+.42*(min(B2,AIME.4[j])-min(B1,AIME.4[j]))+.15*(AIME.4[j]-min(AIME.4[j],B2)))*eligibility.4[j]
}

RR.4 = PIA.4/AIME.4

####Annuity Calculation####
max.life = 110-(20+years)
EPV = rep(NA,N)
#need to figure out standard deviation
COLA = rnorm(n = 12*max.life, mean = 1.026, sd = 0.0291594016926052)
COLA = COLA^(1/12)
#need to confirm
int = (1.025)^(1/12)
vstar = rep(NA, 12*max.life)
px = 1-Mort$qx
px = px[67:110]
qx = 1-px
tpx = rep(NA,12*max.life+1)
tpx[1] = 1
ax = 0
for (y in 1:max.life) {
  for (m in 1:12) {
    vstar[m+12*(y-1)] = prod(COLA[1:(m+12*(y-1))])*(int)^(-(m+12*(y-1)-1))
    ax = ax + (1/12)*tpx[m+12*(y-1)]*vstar[m+12*(y-1)]
    #UDD Assumption
    if (y > 1) {
      tpx[m+12*(y-1)+1] = (1-(m/12)*qx[y])*prod(px[1:y])
    } else {
      tpx[m+12*(y-1)+1] = 1-(m/12)*qx[1]
    }
  }
}
ax
EPV.4 = 12*ax*PIA.4
tot.EPV.4 = sum(EPV.4)
taxrate.4 = as.numeric(tot.EPV.4/fundsize.4)
taxrate.4

tax.con = matrix(0,N,12*years+1)
for (m in 1:(12*years)) {
  tax.con[,m+1] = tax.con[,m]*(1+tyield)^(1/12)+(taxrate.4)*tax.base.m[,m]
}

total.con = tax.con[,(12*years+1)]
uneligible.con.4 = sum(total.con[which(eligibility.4 == 0)])
uneligible.con.4/sum(total.con)

fund.alt.4 = fundsize.4 - uneligible.con.4
tax.alt.4  = as.numeric(tot.EPV.4/fund.alt.4)
tax.alt.4

NHCE.4 = rep(NA,years)
for (y in 1:years) {
  NHCE.4[y] = length(which(income[,y] < WBL.4[y] & income[,y] != 0))/sum(employed[,y])
}

#Workforce Size
N = 200000
#Layoff Rate
L = rnorm(n = 47, mean = 0.0306041009463722, sd = .00079889)
#Long-run average unemployment rate
LRU = .055
#initial unemployment rate
init.u = .036
n = 20
#Need to discuss with Nan
AWI = c(4.7, 4.5, 4.2, 4.1, 4.2,4.2,4.2, 4.3, 4)
AWI = c(c(AWI),rnorm(n = 40, mean = 4, sd = .075))/100
#Number of working years
years = 47
D = c(rep(pr,n),rep(0,years-n-1))
#Number of groups
groups = length(Init[,1])
#Amount of wokers per group
workforce.size = round(N*as.numeric(Init[,3]))
N = sum(workforce.size)
capacity = cbind(workforce.size,matrix(0,groups,(years-1)))
names.row = NA
for (i in 1:groups) {
  names.row = c(names.row,rep(Init[i,1],workforce.size[i]))
}
names.row = names.row[-1]

major.class = unique(substr(Init[,1],1,3))
skill.class = c("B","R")

workforce.state = matrix(0,N,years)
rownames(workforce.state) = names.row

income = matrix(0, N, years)
rownames(income) = names.row

tot.income = matrix(0, groups, years)
rownames(tot.income) = Init[,1]

avg.income = matrix(0, groups, years)
rownames(avg.income) = Init[,1]
avg.income[,1] = as.numeric(Init[,8])

auto.loss = matrix(0,groups,years-1)
rownames(auto.loss) = Init[,1]

tot.loss = matrix(0,groups,years-1)
rownames(tot.loss) = Init[,1]

capacity = matrix(0,groups,years)
rownames(capacity) = Init[,1]

unemployed = matrix(0,groups,years)
rownames(unemployed) = Init[,1]

employed = matrix(0,groups,years)
rownames(employed) = Init[,1]

unemployed.check = matrix(0,groups,years)
rownames(unemployed.check) = Init[,1]

employed.check = matrix(0,groups,years)
rownames(employed.check) = Init[,1]

job.loss = matrix(0,groups,years-1)
rownames(job.loss) = Init[,1]

job.gain = matrix(0,groups,years-1)
rownames(job.gain) = Init[,1]

auto.income = matrix(0,groups,years-1)
rownames(auto.income) = Init[,1]

a.prob = matrix(0,groups,years-1)
rownames(a.prob) = Init[,1]

fstar.5 = matrix(0,groups,years-1)

####Initial Conditions#####

##INITITAL EMPLOYMENT STATES##

for (i in 1:groups) {
  U = sample(which(row.names(workforce.state) == Init[i,1]), size = round(workforce.size[i]*init.u))
  workforce.state[U,1] = 1
}


##INITIAL WAGES##
for (j in 1:N) {
  income[j,1] = abs(workforce.state[j,1]-1)*as.numeric(Init[which(Init == as.character(names.row[j])),8])
}

##INITIAL NUMBER OF WORKERS EMPLOYED IN EACH JOB CLASS##
for (i in 1:groups) {
  employed[i,1] =  length(workforce.state[which(workforce.state[,1] == 0 & 
                                                  row.names(workforce.state) == Init[i,1]), 1])
}

U.n = as.matrix(round(LRU*workforce.size + auto_prob*employed[,1]))
unemployed[,1] = workforce.size - employed[,1]

for (i in 1:groups) {
  spline.coef = solve(rbind(c(1,1,1),c(2*n,1,0),c(n^2,n,1)),c(init.u*workforce.size[i],0,U.n[i]))
  unemployed[i,c(1:n)] = round(spline.coef[1]*c(1:n)^2 + spline.coef[2]*c(1:n)+spline.coef[3])
  unemployed[i,c((n+1):years)] = round(workforce.size[i]*rnorm(n = (years - n), mean = (U.n[i]/workforce.size[i]), sd = .00055127336))
}

employed = workforce.size - unemployed
auto = matrix(0,groups,n)
for (y in 2:n) {
  auto[,y] = round((unemployed[,y-1]/unemployed[,n-1])*as.matrix(auto_prob)*employed[,1])
}
for (i in 1:groups) {
  auto.loss[i,c(1:(n-1))] = diff(auto[i,])
  a.prob[i,c(1:(n-1))] = auto.loss[i,c(1:(n-1))]/employed[i,c(1:(n-1))]
}

#####EMPLOYMENT AND INCOME SIMULATION, YEARS 2-47####
for (y in 2:years) {
  for (i in 1:groups) {
    #take a random sample of a given job class who were employed in year y-1 to become unemployed in year y
    eu = sample(which(workforce.state[,(y-1)] == 0  & row.names(workforce.state) == Init[i,1]), size = min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))
    job.loss[i,y-1] = length(eu)
    workforce.state[eu,y] = 1
    
    fstar.5[i,y-1] = 1 - (unemployed[i,y] - min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))/unemployed[i,y-1]
    
    #take a random sample of a given job class who were unemployed in year y-1 to remain unemployed in year y
    uu = sample(which(workforce.state[,(y-1)] == 1 & row.names(workforce.state) == Init[i,1]), size = round(unemployed[i,y-1]*(1-fstar.5[i,y-1])))
    job.gain[i,y-1] = unemployed[i,y-1] - length(uu)
    workforce.state[uu,y] = 1
    
    
    employed.check[i,y] = length(which(workforce.state[,y] == 0 & row.names(workforce.state) == Init[i,1]))
  }  
  #unemployed[,y].check = workforce.size - employed.check[,y]
  
  for (j in 1:14) {
    if (major.class[j] != "LIF") {
      auto.income[which(row.names(auto.income) == paste(major.class[j],"B",sep ="-")),y-1] = 
        C*auto.loss[which(row.names(auto.loss) == paste(major.class[j],"R",sep ="-")),y-1] * avg.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1]
      auto.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1] = 0
    }
  }
  
  for (i in 1:groups) {
    tot.income[i,y-1] = sum(income[which(row.names(income)==Init[i,1]),y-1])
    
    #avg. income in year y = [(1+inflation)*(total income year y - lost or gained income from automation)*(urrent employment/prior year employment)]/(current employment)
    if (substr(Init[i,1],5,5) == "R"){
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1])*(1-D[y-1])
    }else {
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1]+(auto.income[i,y-1]/max(employed[i,y],.01)))
    }    
    income[which(row.names(income) == Init[i,1]),y] =
      abs(workforce.state[which(row.names(income) == Init[i,1]),y]-1)*avg.income[i,y]
    
    if (y == max(years)){
      tot.income[i,y] = sum(income[which(row.names(income)==Init[i,1]),y])
    }
    
  }
}    



####EMPLOYMENT ANALYSIS####
urate.5 = apply(unemployed,2,sum)/N
plot(urate.5, type = "l")

ss.eligibility.5 = years - apply(workforce.state,1,sum)
##State 0 = noneligible, State 1 = elegible
eligibility.5 = rep(NA,N)
eligibility.5[which(ss.eligibility.5 < 10)] = 0
eligibility.5[which(ss.eligibility.5 > 9)] = 1
#fraction of workforce eligible for OASI
percent.eligible.5 = sum(eligibility.5)/N

#Final unemployment rates by group
ufinal = rep(NA,groups)
for (i in 1:groups) {
  ufinal[i] = 1 - employed[i,47]/workforce.size[i]
}
barplot(ufinal)

####INFLATION ANALYSIS####
g.5 = rep(NA,years - 1)
AWI.act = rep(NA, years)
#Adjusted AWI, average salary of everyone working
AWI.act[1] = mean(income[which(income[,1] != 0),1])  

for (y in 1:(years-1)) {
  AWI.act[y+1] = mean(income[which(income[,y+1]!=0),y+1])  
  g.5[y] = (AWI.act[y+1]-AWI.act[y])/AWI.act[y]
}

wage.inflation.5 = matrix(0,groups,years-1)
for (y in 1:years-1) {
  wage.inflation.5[,y] = avg.income[,y+1]/avg.income[,y] - 1
}

pallete = rainbow(n = years - 1)
plot(AWI, type = "l", lwd = 5,xlim = c(0,46) ,ylim = c(-.5,.5))
for (i in 1:groups) {
  lines(wage.inflation.5[i,], col = pallete[i])
}

GINI.5 = rep(NA,y)
GDP.5 = apply(income,2,sum)
for (y in 1:47) {
  sorted.income = sort(x = income[,y])
  percent.income = rep(NA,N)
  percent.income[1] = sorted.income[1]/GDP.5[y]
  B = 0
  for (j in 2:N) {
    percent.income[j] = percent.income[j-1] + (sorted.income[j]/GDP.5[y])
    B = B + percent.income[j]/N
  }
  GINI.5[y] = 1-2*B
}
plot(x = c(1:years), y = GINI.5, type = "l") 

b.AWI.5 = rep(NA,years)
b.growth.5 = rep(NA,years-1)
b.AWI.5[1] = sum(income[which(substr(row.names(income),5,5) == "B"),1])/sum(employed[which(substr(row.names(employed),5,5) == "B"),1])
for (y in 2:years) {
  b.AWI.5[y] = sum(income[which(substr(row.names(income),5,5) == "B"),y])/sum(employed[which(substr(row.names(employed),5,5) == "B"),y])
  b.growth.5[y-1] = (b.AWI.5[y]/b.AWI.5[y-1]) - 1 
}

####TAX REVENUE####
WBL.5 = rep(NA,years)
WBL.5[1] = 137700

for (y in 2:years) {
  WBL.5[y] = WBL.5[y-1]*(1+g.5[y-1])
}

tax.base = matrix(0,N,years)
tax.base.m = matrix(0,N,12*years)
rownames(tax.base) = names.row
for (y in 1:years) {
  for (j in 1:N) {
    tax.base[j,y] = min(income[j,y],WBL.5[y])
    tax.base.m[j,c((1+12*(y-1)):(12*y))] = min(income[j,y],WBL.5[y])/12
  }
}
tot.tax = apply(tax.base, 2, sum)
tot.tax.m = apply(tax.base.m, 2, sum)
taxable.percent.5 = tot.tax/apply(tot.income, 2, sum)
plot(taxable.percent.5,type = "l")


tyield = .025
accum = rep(NA,years*12)
fundsize.t = rep(NA,years*12+1)
fundsize.t[1] = 0
for (m in 1:(12*years)) {
  accum[m] = (1+tyield)^((years*12-m)/12)
  fundsize.t[m+1] = tot.tax[m] + fundsize.t[m]*(1 + tyield)^(1/12)
}

fundsize.5 = t(accum)%*%tot.tax.m

class.tax = matrix(0,groups,years)
rownames(class.tax) = Init[,1]

#total tax base by group per year
for (y in 1:years) {
  for (i in 1:groups) {
    class.tax[i,y] = sum(tax.base[which(row.names(income)==Init[i,1]),y])
  } 
}

#### Benefit Calculations ####
index = rep(NA, years)
index[years] = 1
z = 1+g.5
for (y in 1:(years-1)) {
  index[y] = 1/prod(z[y:(years-1)])
}


index.income = matrix(0,N,years)
rownames(index.income) = names.row
index.income[,years] = tax.base[,years]
for (y in 1:(years-1)) {
  index.income[,y] = tax.base[,y]/index[y]
}

avg.index.5 = matrix(0,groups,years)
rownames(avg.index.5) = Init[,1]
for (y in 1:years) {
  for (i in 1:groups) {
    avg.index.5[i,y] = sum(index.income[which(row.names(index.income)==Init[i,1]),y])/employed[i,y]
  } 
}

sorted.wages = rep(NA,years)
AIME.5 = matrix(NA,N,1)
for (j in 1:N) {
  sorted.wages = index.income[j,order(index.income[j,], decreasing = TRUE)]
  AIME.5[j] = mean(sorted.wages[1:35])/12
}
row.names(AIME.5) = names.row

#Calculate bend points by indexing 2020 bend points 47 years ahead
B1 = 960/index[1]
B2 = 5785/index[1]

PIA.5 = matrix(NA,N,1)
row.names(PIA.5) = names.row
for (j in 1:N) {
  PIA.5[j] = (.9*(min(B1,AIME.5[j])-0)+.32*(min(B2,AIME.5[j])-min(B1,AIME.5[j]))+.15*(AIME.5[j]-min(AIME.5[j],B2)))*eligibility.5[j]
}

RR.5 = PIA.5/AIME.5

####Annuity Calculation####
max.life = 110-(20+years)
EPV = rep(NA,N)
#need to figure out standard deviation
COLA = rnorm(n = 12*max.life, mean = 1.026, sd = 0.0291594016926052)
COLA = COLA^(1/12)
#need to confirm
int = (1.025)^(1/12)
vstar = rep(NA, 12*max.life)
px = 1-Mort$qx
px = px[67:110]
qx = 1-px
tpx = rep(NA,12*max.life+1)
tpx[1] = 1
ax = 0
for (y in 1:max.life) {
  for (m in 1:12) {
    vstar[m+12*(y-1)] = prod(COLA[1:(m+12*(y-1))])*(int)^(-(m+12*(y-1)-1))
    ax = ax + (1/12)*tpx[m+12*(y-1)]*vstar[m+12*(y-1)]
    #UDD Assumption
    if (y > 1) {
      tpx[m+12*(y-1)+1] = (1-(m/12)*qx[y])*prod(px[1:y])
    } else {
      tpx[m+12*(y-1)+1] = 1-(m/12)*qx[1]
    }
  }
}
ax
EPV.5 = 12*ax*PIA.5
tot.EPV.5 = sum(EPV.5)
taxrate.5 = as.numeric(tot.EPV.5/fundsize.5)
taxrate.5

tax.con = matrix(0,N,12*years+1)
for (m in 1:(12*years)) {
  tax.con[,m+1] = tax.con[,m]*(1+tyield)^(1/12)+(taxrate.5)*tax.base.m[,m]
}

total.con = tax.con[,(12*years+1)]
uneligible.con.5 = sum(total.con[which(eligibility.5 == 0)])
uneligible.con.5/sum(total.con)

fund.alt.5 = fundsize.5 - uneligible.con.5
tax.alt.5  = as.numeric(tot.EPV.5/fund.alt.5)
tax.alt.5

NHCE.5 = rep(NA,years)
for (y in 1:years) {
  NHCE.5[y] = length(which(income[,y] < WBL.5[y] & income[,y] != 0))/sum(employed[,y])
}


N = 200000
#Layoff Rate
L = rnorm(n = 47, mean = 0.0306041009463722, sd = .00079889)
#Long-run average unemployment rate
LRU = .055
#initial unemployment rate
init.u = .036
n = 30
#Need to discuss with Nan
AWI = c(4.7, 4.5, 4.2, 4.1, 4.2,4.2,4.2, 4.3, 4)
AWI = c(c(AWI),rnorm(n = 40, mean = 4, sd = .075))/100
#Number of working years
years = 47
D = c(rep(pr,n),rep(0,years-n-1))
#Number of groups
groups = length(Init[,1])
#Amount of wokers per group
workforce.size = round(N*as.numeric(Init[,3]))
N = sum(workforce.size)
capacity = cbind(workforce.size,matrix(0,groups,(years-1)))
names.row = NA
for (i in 1:groups) {
  names.row = c(names.row,rep(Init[i,1],workforce.size[i]))
}
names.row = names.row[-1]

major.class = unique(substr(Init[,1],1,3))
skill.class = c("B","R")

workforce.state = matrix(0,N,years)
rownames(workforce.state) = names.row

income = matrix(0, N, years)
rownames(income) = names.row

tot.income = matrix(0, groups, years)
rownames(tot.income) = Init[,1]

avg.income = matrix(0, groups, years)
rownames(avg.income) = Init[,1]
avg.income[,1] = as.numeric(Init[,8])

auto.loss = matrix(0,groups,years-1)
rownames(auto.loss) = Init[,1]

tot.loss = matrix(0,groups,years-1)
rownames(tot.loss) = Init[,1]

capacity = matrix(0,groups,years)
rownames(capacity) = Init[,1]

unemployed = matrix(0,groups,years)
rownames(unemployed) = Init[,1]

employed = matrix(0,groups,years)
rownames(employed) = Init[,1]

unemployed.check = matrix(0,groups,years)
rownames(unemployed.check) = Init[,1]

employed.check = matrix(0,groups,years)
rownames(employed.check) = Init[,1]

job.loss = matrix(0,groups,years-1)
rownames(job.loss) = Init[,1]

job.gain = matrix(0,groups,years-1)
rownames(job.gain) = Init[,1]

auto.income = matrix(0,groups,years-1)
rownames(auto.income) = Init[,1]

a.prob = matrix(0,groups,years-1)
rownames(a.prob) = Init[,1]

fstar.6 = matrix(0,groups,years-1)

####Initial Conditions#####

##INITITAL EMPLOYMENT STATES##

for (i in 1:groups) {
  U = sample(which(row.names(workforce.state) == Init[i,1]), size = round(workforce.size[i]*init.u))
  workforce.state[U,1] = 1
}


##INITIAL WAGES##
for (j in 1:N) {
  income[j,1] = abs(workforce.state[j,1]-1)*as.numeric(Init[which(Init == as.character(names.row[j])),8])
}

##INITIAL NUMBER OF WORKERS EMPLOYED IN EACH JOB CLASS##
for (i in 1:groups) {
  employed[i,1] =  length(workforce.state[which(workforce.state[,1] == 0 & 
                                                  row.names(workforce.state) == Init[i,1]), 1])
}

U.n = as.matrix(round(LRU*workforce.size + auto_prob*employed[,1]))
unemployed[,1] = workforce.size - employed[,1]

for (i in 1:groups) {
  spline.coef = solve(rbind(c(1,1,1),c(2*n,1,0),c(n^2,n,1)),c(init.u*workforce.size[i],0,U.n[i]))
  unemployed[i,c(1:n)] = round(spline.coef[1]*c(1:n)^2 + spline.coef[2]*c(1:n)+spline.coef[3])
  unemployed[i,c((n+1):years)] = round(workforce.size[i]*rnorm(n = (years - n), mean = (U.n[i]/workforce.size[i]), sd = .00055127336))
}

employed = workforce.size - unemployed
auto = matrix(0,groups,n)
for (y in 2:n) {
  auto[,y] = round((unemployed[,y-1]/unemployed[,n-1])*as.matrix(auto_prob)*employed[,1])
}
for (i in 1:groups) {
  auto.loss[i,c(1:(n-1))] = diff(auto[i,])
  a.prob[i,c(1:(n-1))] = auto.loss[i,c(1:(n-1))]/employed[i,c(1:(n-1))]
}

#####EMPLOYMENT AND INCOME SIMULATION, YEARS 2-47####
for (y in 2:years) {
  for (i in 1:groups) {
    #take a random sample of a given job class who were employed in year y-1 to become unemployed in year y
    eu = sample(which(workforce.state[,(y-1)] == 0  & row.names(workforce.state) == Init[i,1]), size = min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))
    job.loss[i,y-1] = length(eu)
    workforce.state[eu,y] = 1
    
    fstar.6[i,y-1] = 1 - (unemployed[i,y] - min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))/unemployed[i,y-1]
    
    #take a random sample of a given job class who were unemployed in year y-1 to remain unemployed in year y
    uu = sample(which(workforce.state[,(y-1)] == 1 & row.names(workforce.state) == Init[i,1]), size = round(unemployed[i,y-1]*(1-fstar.6[i,y-1])))
    job.gain[i,y-1] = unemployed[i,y-1] - length(uu)
    workforce.state[uu,y] = 1
    
    
    employed.check[i,y] = length(which(workforce.state[,y] == 0 & row.names(workforce.state) == Init[i,1]))
  }  
  #unemployed[,y].check = workforce.size - employed.check[,y]
  
  for (j in 1:14) {
    if (major.class[j] != "LIF") {
      auto.income[which(row.names(auto.income) == paste(major.class[j],"B",sep ="-")),y-1] = 
        C*auto.loss[which(row.names(auto.loss) == paste(major.class[j],"R",sep ="-")),y-1] * avg.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1]
      auto.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1] = 0
    }
  }
  
  for (i in 1:groups) {
    tot.income[i,y-1] = sum(income[which(row.names(income)==Init[i,1]),y-1])
    
    #avg. income in year y = [(1+inflation)*(total income year y - lost or gained income from automation)*(urrent employment/prior year employment)]/(current employment)
    if (substr(Init[i,1],5,5) == "R"){
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1])*(1-D[y-1])
    }else {
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1]+(auto.income[i,y-1]/max(employed[i,y],.01)))
    }    
    income[which(row.names(income) == Init[i,1]),y] =
      abs(workforce.state[which(row.names(income) == Init[i,1]),y]-1)*avg.income[i,y]
    
    if (y == max(years)){
      tot.income[i,y] = sum(income[which(row.names(income)==Init[i,1]),y])
    }
    
  }
}    



####EMPLOYMENT ANALYSIS####
urate.6 = apply(unemployed,2,sum)/N
plot(urate.6, type = "l")

ss.eligibility.6 = years - apply(workforce.state,1,sum)
##State 0 = noneligible, State 1 = elegible
eligibility.6 = rep(NA,N)
eligibility.6[which(ss.eligibility.6 < 10)] = 0
eligibility.6[which(ss.eligibility.6 > 9)] = 1
#fraction of workforce eligible for OASI
percent.eligible.6 = sum(eligibility.6)/N

#Final unemployment rates by group
ufinal = rep(NA,groups)
for (i in 1:groups) {
  ufinal[i] = 1 - employed[i,47]/workforce.size[i]
}
barplot(ufinal)

####INFLATION ANALYSIS####
g.6 = rep(NA,years - 1)
AWI.act = rep(NA, years)
#Adjusted AWI, average salary of everyone working
AWI.act[1] = mean(income[which(income[,1] != 0),1])  

for (y in 1:(years-1)) {
  AWI.act[y+1] = mean(income[which(income[,y+1]!=0),y+1])  
  g.6[y] = (AWI.act[y+1]-AWI.act[y])/AWI.act[y]
}

wage.inflation.6 = matrix(0,groups,years-1)
for (y in 1:years-1) {
  wage.inflation.6[,y] = avg.income[,y+1]/avg.income[,y] - 1
}

pallete = rainbow(n = years - 1)
plot(AWI, type = "l", lwd = 5,xlim = c(0,46) ,ylim = c(-.5,.5))
for (i in 1:groups) {
  lines(wage.inflation.6[i,], col = pallete[i])
}

GINI.6 = rep(NA,y)
GDP.6 = apply(income,2,sum)
for (y in 1:47) {
  sorted.income = sort(x = income[,y])
  percent.income = rep(NA,N)
  percent.income[1] = sorted.income[1]/GDP.6[y]
  B = 0
  for (j in 2:N) {
    percent.income[j] = percent.income[j-1] + (sorted.income[j]/GDP.6[y])
    B = B + percent.income[j]/N
  }
  GINI.6[y] = 1-2*B
}
plot(x = c(1:years), y = GINI.6, type = "l") 

b.AWI.6 = rep(NA,years)
b.growth.6 = rep(NA,years-1)
b.AWI.6[1] = sum(income[which(substr(row.names(income),5,5) == "B"),1])/sum(employed[which(substr(row.names(employed),5,5) == "B"),1])
for (y in 2:years) {
  b.AWI.6[y] = sum(income[which(substr(row.names(income),5,5) == "B"),y])/sum(employed[which(substr(row.names(employed),5,5) == "B"),y])
  b.growth.6[y-1] = (b.AWI.6[y]/b.AWI.6[y-1]) - 1 
}


####TAX REVENUE####
WBL.6 = rep(NA,years)
WBL.6[1] = 137700

for (y in 2:years) {
  WBL.6[y] = WBL.6[y-1]*(1+g.6[y-1])
}

tax.base = matrix(0,N,years)
tax.base.m = matrix(0,N,12*years)
rownames(tax.base) = names.row
for (y in 1:years) {
  for (j in 1:N) {
    tax.base[j,y] = min(income[j,y],WBL.6[y])
    tax.base.m[j,c((1+12*(y-1)):(12*y))] = min(income[j,y],WBL.6[y])/12
  }
}
tot.tax = apply(tax.base, 2, sum)
tot.tax.m = apply(tax.base.m, 2, sum)
taxable.percent.6 = tot.tax/apply(tot.income, 2, sum)
plot(taxable.percent.6,type = "l")


tyield = .025
accum = rep(NA,years*12)
fundsize.t = rep(NA,years*12+1)
fundsize.t[1] = 0
for (m in 1:(12*years)) {
  accum[m] = (1+tyield)^((years*12-m)/12)
  fundsize.t[m+1] = tot.tax[m] + fundsize.t[m]*(1 + tyield)^(1/12)
}

fundsize.6 = t(accum)%*%tot.tax.m

class.tax = matrix(0,groups,years)
rownames(class.tax) = Init[,1]

#total tax base by group per year
for (y in 1:years) {
  for (i in 1:groups) {
    class.tax[i,y] = sum(tax.base[which(row.names(income)==Init[i,1]),y])
  } 
}

#### Benefit Calculations ####
index = rep(NA, years)
index[years] = 1
z = 1+g.6
for (y in 1:(years-1)) {
  index[y] = 1/prod(z[y:(years-1)])
}


index.income = matrix(0,N,years)
rownames(index.income) = names.row
index.income[,years] = tax.base[,years]
for (y in 1:(years-1)) {
  index.income[,y] = tax.base[,y]/index[y]
}

avg.index.6 = matrix(0,groups,years)
rownames(avg.index.6) = Init[,1]
for (y in 1:years) {
  for (i in 1:groups) {
    avg.index.6[i,y] = sum(index.income[which(row.names(index.income)==Init[i,1]),y])/employed[i,y]
  } 
}

sorted.wages = rep(NA,years)
AIME.6 = matrix(NA,N,1)
for (j in 1:N) {
  sorted.wages = index.income[j,order(index.income[j,], decreasing = TRUE)]
  AIME.6[j] = mean(sorted.wages[1:35])/12
}
row.names(AIME.6) = names.row

#Calculate bend points by indexing 2020 bend points 47 years ahead
B1 = 960/index[1]
B2 = 5785/index[1]

PIA.6 = matrix(NA,N,1)
row.names(PIA.6) = names.row
for (j in 1:N) {
  PIA.6[j] = (.9*(min(B1,AIME.6[j])-0)+.32*(min(B2,AIME.6[j])-min(B1,AIME.6[j]))+.15*(AIME.6[j]-min(AIME.6[j],B2)))*eligibility.6[j]
}

RR.6 = PIA.6/AIME.6

####Annuity Calculation####
max.life = 110-(20+years)
EPV = rep(NA,N)
#need to figure out standard deviation
COLA = rnorm(n = 12*max.life, mean = 1.026, sd = 0.0291594016926052)
COLA = COLA^(1/12)
#need to confirm
int = (1.025)^(1/12)
vstar = rep(NA, 12*max.life)
px = 1-Mort$qx
px = px[67:110]
qx = 1-px
tpx = rep(NA,12*max.life+1)
tpx[1] = 1
ax = 0
for (y in 1:max.life) {
  for (m in 1:12) {
    vstar[m+12*(y-1)] = prod(COLA[1:(m+12*(y-1))])*(int)^(-(m+12*(y-1)-1))
    ax = ax + (1/12)*tpx[m+12*(y-1)]*vstar[m+12*(y-1)]
    #UDD Assumption
    if (y > 1) {
      tpx[m+12*(y-1)+1] = (1-(m/12)*qx[y])*prod(px[1:y])
    } else {
      tpx[m+12*(y-1)+1] = 1-(m/12)*qx[1]
    }
  }
}
ax
EPV.6 = 12*ax*PIA.6
tot.EPV.6 = sum(EPV.6)
taxrate.6 = as.numeric(tot.EPV.6/fundsize.6)
taxrate.6

tax.con = matrix(0,N,12*years+1)
for (m in 1:(12*years)) {
  tax.con[,m+1] = tax.con[,m]*(1+tyield)^(1/12)+(taxrate.6)*tax.base.m[,m]
}

total.con = tax.con[,(12*years+1)]
uneligible.con.6 = sum(total.con[which(eligibility.6 == 0)])
uneligible.con.6/sum(total.con)

fund.alt.6 = fundsize.6 - uneligible.con.6
tax.alt.6  = as.numeric(tot.EPV.6/fund.alt.6)
tax.alt.6

NHCE.6 = rep(NA,years)
for (y in 1:years) {
  NHCE.6[y] = length(which(income[,y] < WBL.6[y] & income[,y] != 0))/sum(employed[,y])
}


N = 200000
#Layoff Rate
L = rnorm(n = 47, mean = 0.0306041009463722, sd = .00079889)
#Long-run average unemployment rate
LRU = .055
#initial unemployment rate
init.u = .036
n = 40
#Need to discuss with Nan
AWI = c(4.7, 4.5, 4.2, 4.1, 4.2,4.2,4.2, 4.3, 4)
AWI = c(c(AWI),rnorm(n = 40, mean = 4, sd = .075))/100
#Number of working years
years = 47
D = c(rep(pr,n),rep(0,years-n-1))
#Number of groups
groups = length(Init[,1])
#Amount of wokers per group
workforce.size = round(N*as.numeric(Init[,3]))
N = sum(workforce.size)
capacity = cbind(workforce.size,matrix(0,groups,(years-1)))
names.row = NA
for (i in 1:groups) {
  names.row = c(names.row,rep(Init[i,1],workforce.size[i]))
}
names.row = names.row[-1]

major.class = unique(substr(Init[,1],1,3))
skill.class = c("B","R")

workforce.state = matrix(0,N,years)
rownames(workforce.state) = names.row

income = matrix(0, N, years)
rownames(income) = names.row

tot.income = matrix(0, groups, years)
rownames(tot.income) = Init[,1]

avg.income = matrix(0, groups, years)
rownames(avg.income) = Init[,1]
avg.income[,1] = as.numeric(Init[,8])

auto.loss = matrix(0,groups,years-1)
rownames(auto.loss) = Init[,1]

tot.loss = matrix(0,groups,years-1)
rownames(tot.loss) = Init[,1]

capacity = matrix(0,groups,years)
rownames(capacity) = Init[,1]

unemployed = matrix(0,groups,years)
rownames(unemployed) = Init[,1]

employed = matrix(0,groups,years)
rownames(employed) = Init[,1]

unemployed.check = matrix(0,groups,years)
rownames(unemployed.check) = Init[,1]

employed.check = matrix(0,groups,years)
rownames(employed.check) = Init[,1]

job.loss = matrix(0,groups,years-1)
rownames(job.loss) = Init[,1]

job.gain = matrix(0,groups,years-1)
rownames(job.gain) = Init[,1]

auto.income = matrix(0,groups,years-1)
rownames(auto.income) = Init[,1]

a.prob = matrix(0,groups,years-1)
rownames(a.prob) = Init[,1]

fstar.5 = matrix(0,groups,years-1)

####Initial Conditions#####

##INITITAL EMPLOYMENT STATES##

for (i in 1:groups) {
  U = sample(which(row.names(workforce.state) == Init[i,1]), size = round(workforce.size[i]*init.u))
  workforce.state[U,1] = 1
}


##INITIAL WAGES##
for (j in 1:N) {
  income[j,1] = abs(workforce.state[j,1]-1)*as.numeric(Init[which(Init == as.character(names.row[j])),8])
}

##INITIAL NUMBER OF WORKERS EMPLOYED IN EACH JOB CLASS##
for (i in 1:groups) {
  employed[i,1] =  length(workforce.state[which(workforce.state[,1] == 0 & 
                                                  row.names(workforce.state) == Init[i,1]), 1])
}

U.n = as.matrix(round(LRU*workforce.size + auto_prob*employed[,1]))
unemployed[,1] = workforce.size - employed[,1]

for (i in 1:groups) {
  spline.coef = solve(rbind(c(1,1,1),c(2*n,1,0),c(n^2,n,1)),c(init.u*workforce.size[i],0,U.n[i]))
  unemployed[i,c(1:n)] = round(spline.coef[1]*c(1:n)^2 + spline.coef[2]*c(1:n)+spline.coef[3])
  unemployed[i,c((n+1):years)] = round(workforce.size[i]*rnorm(n = (years - n), mean = (U.n[i]/workforce.size[i]), sd = .00055127336))
}

employed = workforce.size - unemployed
auto = matrix(0,groups,n)
for (y in 2:n) {
  auto[,y] = round((unemployed[,y-1]/unemployed[,n-1])*as.matrix(auto_prob)*employed[,1])
}
for (i in 1:groups) {
  auto.loss[i,c(1:(n-1))] = diff(auto[i,])
  a.prob[i,c(1:(n-1))] = auto.loss[i,c(1:(n-1))]/employed[i,c(1:(n-1))]
}

#####EMPLOYMENT AND INCOME SIMULATION, YEARS 2-47####
for (y in 2:years) {
  for (i in 1:groups) {
    #take a random sample of a given job class who were employed in year y-1 to become unemployed in year y
    eu = sample(which(workforce.state[,(y-1)] == 0  & row.names(workforce.state) == Init[i,1]), size = min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))
    job.loss[i,y-1] = length(eu)
    workforce.state[eu,y] = 1
    
    fstar.7[i,y-1] = 1 - (unemployed[i,y] - min(max(round(employed[i,y-1]*L[y-1])+auto.loss[i,y-1],unemployed[i,y]-unemployed[i,y-1]),unemployed[i,y]))/unemployed[i,y-1]
    
    #take a random sample of a given job class who were unemployed in year y-1 to remain unemployed in year y
    uu = sample(which(workforce.state[,(y-1)] == 1 & row.names(workforce.state) == Init[i,1]), size = round(unemployed[i,y-1]*(1-fstar.7[i,y-1])))
    job.gain[i,y-1] = unemployed[i,y-1] - length(uu)
    workforce.state[uu,y] = 1
    
    
    employed.check[i,y] = length(which(workforce.state[,y] == 0 & row.names(workforce.state) == Init[i,1]))
  }  
  #unemployed[,y].check = workforce.size - employed.check[,y]
  
  for (j in 1:14) {
    if (major.class[j] != "LIF") {
      auto.income[which(row.names(auto.income) == paste(major.class[j],"B",sep ="-")),y-1] = 
        C*auto.loss[which(row.names(auto.loss) == paste(major.class[j],"R",sep ="-")),y-1] * avg.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1]
      auto.income[which(row.names(auto.income) == paste(major.class[j],"R",sep ="-")),y-1] = 0
    }
  }
  
  for (i in 1:groups) {
    tot.income[i,y-1] = sum(income[which(row.names(income)==Init[i,1]),y-1])
    
    #avg. income in year y = [(1+inflation)*(total income year y - lost or gained income from automation)*(urrent employment/prior year employment)]/(current employment)
    if (substr(Init[i,1],5,5) == "R"){
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1])*(1-D[y-1])
    } else {
      avg.income[i,y] = (1+AWI[y-1])*(avg.income[i,y-1]+(auto.income[i,y-1]/max(employed[i,y],.01)))
    }    
    income[which(row.names(income) == Init[i,1]),y] =
      abs(workforce.state[which(row.names(income) == Init[i,1]),y]-1)*avg.income[i,y]
    
    if (y == max(years)){
      tot.income[i,y] = sum(income[which(row.names(income)==Init[i,1]),y])
    }
    
  }
}    



####EMPLOYMENT ANALYSIS####
urate.7 = apply(unemployed,2,sum)/N
plot(urate.7, type = "l")

ss.eligibility.7 = years - apply(workforce.state,1,sum)
##State 0 = noneligible, State 1 = eligible
eligibility.7 = rep(NA,N)
eligibility.7[which(ss.eligibility.7 < 10)] = 0
eligibility.7[which(ss.eligibility.7 > 9)] = 1
#fraction of workforce eligible for OASI
percent.eligible.7 = sum(eligibility.7)/N

#Final unemployment rates by group
ufinal = rep(NA,groups)
for (i in 1:groups) {
  ufinal[i] = 1 - employed[i,47]/workforce.size[i]
}
barplot(ufinal)

####INFLATION ANALYSIS####
g.7 = rep(NA,years - 1)
AWI.act = rep(NA, years)
#Adjusted AWI, average salary of everyone working
AWI.act[1] = mean(income[which(income[,1] != 0),1])  

for (y in 1:(years-1)) {
  AWI.act[y+1] = mean(income[which(income[,y+1]!=0),y+1])  
  g.7[y] = (AWI.act[y+1]-AWI.act[y])/AWI.act[y]
}

GINI.7 = rep(NA,y)
GDP.7 = apply(income,2,sum)
for (y in 1:47) {
  sorted.income = sort(x = income[,y])
  percent.income = rep(NA,N)
  percent.income[1] = sorted.income[1]/GDP[y]
  B = 0
  for (j in 2:N) {
    percent.income[j] = percent.income[j-1] + (sorted.income[j]/GDP.7[y])
    B = B + percent.income[j]/N
  }
  GINI.7[y] = 1-2*B
}
plot(x = c(1:years), y = GINI.7, type = "l") 

b.AWI.7 = rep(NA,years)
b.growth.7 = rep(NA,years-1)
b.AWI.7[1] = mean(income[which(substr(row.names(income),5,5) == "B" & income[,1] != 0),1])
for (y in 2:years) {
  b.AWI.7[y] = mean(income[which(substr(row.names(income),5,5) == "B" & income[,y] != 0),y])
  b.growth.7[y-1] = (b.AWI.7[y]/b.AWI.7[y-1])-1
}


####TAX REVENUE####
WBL.7 = rep(NA,years)
WBL.7[1] = 137700

for (y in 2:years) {
  WBL.7[y] = WBL.7[y-1]*(1+g.7[y-1])
}

tax.base = matrix(0,N,years)
tax.base.m = matrix(0,N,12*years)
rownames(tax.base) = names.row
for (y in 1:years) {
  for (j in 1:N) {
    tax.base[j,y] = min(income[j,y],WBL.7[y])
    tax.base.m[j,c((1+12*(y-1)):(12*y))] = min(income[j,y],WBL.7[y])/12
  }
}
tot.tax = apply(tax.base, 2, sum)
tot.tax.m = apply(tax.base.m, 2, sum)
taxable.percent.7 = tot.tax/apply(tot.income, 2, sum)
plot(taxable.percent.7,type = "l")


tyield = .025
accum = rep(NA,years*12)
fundsize.t = rep(NA,years*12+1)
fundsize.t[1] = 0
for (m in 1:(12*years)) {
  accum[m] = (1+tyield)^((years*12-m)/12)
  fundsize.t[m+1] = tot.tax[m] + fundsize.t[m]*(1 + tyield)^(1/12)
}

fundsize.7 = t(accum)%*%tot.tax.m

class.tax = matrix(0,groups,years)
rownames(class.tax) = Init[,1]

#total tax base by group per year
for (y in 1:years) {
  for (i in 1:groups) {
    class.tax[i,y] = sum(tax.base[which(row.names(income)==Init[i,1]),y])
  } 
}

#### Benefit Calculations ####
index = rep(NA, years)
index[years] = 1
z = 1+g.7
for (y in 1:(years-1)) {
  index[y] = 1/prod(z[y:(years-1)])
}


index.income = matrix(0,N,years)
rownames(index.income) = names.row
index.income[,years] = tax.base[,years]
for (y in 1:(years-1)) {
  index.income[,y] = tax.base[,y]/index[y]
}

avg.index.7 = matrix(0,groups,years)
rownames(avg.index.7) = Init[,1]
for (y in 1:years) {
  for (i in 1:groups) {
    avg.index.7[i,y] = sum(index.income[which(row.names(index.income)==Init[i,1]),y])/employed[i,y]
  } 
}

sorted.wages = rep(NA,years)
AIME.7 = matrix(NA,N,1)
for (j in 1:N) {
  sorted.wages = index.income[j,order(index.income[j,], decreasing = TRUE)]
  AIME.7[j] = mean(sorted.wages[1:35])/12
}
row.names(AIME.7) = names.row

#Calculate bend points by indexing 2020 bend points 47 years ahead
B1 = 960/index[1]
B2 = 5785/index[1]

PIA.7 = matrix(NA,N,1)
row.names(PIA.7) = names.row
for (j in 1:N) {
  PIA.7[j] = (.9*(min(B1,AIME.7[j])-0)+.32*(min(B2,AIME.7[j])-min(B1,AIME.7[j]))+.15*(AIME.7[j]-min(AIME.7[j],B2)))*eligibility.7[j]
}

RR.7 = PIA.7/AIME.7

####Annuity Calculation####
max.life = 110-(20+years)
EPV = rep(NA,N)
#need to figure out standard deviation
COLA = rnorm(n = 12*max.life, mean = 1.026, sd = 0.0291594016926052)
COLA = COLA^(1/12)
#need to confirm
int = (1.025)^(1/12)
vstar = rep(NA, 12*max.life)
px = 1-Mort$qx
px = px[67:110]
qx = 1-px
tpx = rep(NA,12*max.life+1)
tpx[1] = 1
ax = 0
for (y in 1:max.life) {
  for (m in 1:12) {
    vstar[m+12*(y-1)] = prod(COLA[1:(m+12*(y-1))])*(int)^(-(m+12*(y-1)-1))
    ax = ax + (1/12)*tpx[m+12*(y-1)]*vstar[m+12*(y-1)]
    #UDD Assumption
    if (y > 1) {
      tpx[m+12*(y-1)+1] = (1-(m/12)*qx[y])*prod(px[1:y])
    } else {
      tpx[m+12*(y-1)+1] = 1-(m/12)*qx[1]
    }
  }
}
ax
EPV.7 = 12*ax*PIA.7
tot.EPV.7 = sum(EPV.7)
taxrate.7 = as.numeric(tot.EPV.7/fundsize.7)
taxrate.7

tax.con = matrix(0,N,12*years+1)
for (m in 1:(12*years)) {
  tax.con[,m+1] = tax.con[,m]*(1+tyield)^(1/12)+(taxrate.7)*tax.base.m[,m]
}

total.con = tax.con[,(12*years+1)]
uneligible.con.7 = sum(total.con[which(eligibility.7 == 0)])
uneligible.con.7/sum(total.con)

fund.alt.7 = fundsize.7 - uneligible.con.7
tax.alt.7  = as.numeric(tot.EPV.7/fund.alt.7)
tax.alt.7

NHCE.7 = rep(NA,years)
for (y in 1:years) {
  NHCE.7[y] = length(which(income[,y] < WBL.7[y] & income[,y] != 0))/sum(employed[,y])
}


taxrate = c(taxrate.1,taxrate.2,taxrate.3,taxrate.4,taxrate.5,taxrate.6,taxrate.7)
taxrate

growth = 1+rbind(g.2,g.3,g.4,g.5,g.6,g.7)
avg.growth = rep(NA,6)
for (x in 1:6) {
  avg.growth[x] = prod(growth[x,])^(1/length(growth[x,]))
}

tot.RR.1 = sum(PIA.1)/sum(AIME.1)
tot.RR.2 = sum(PIA.2)/sum(AIME.2)
tot.RR.3 = sum(PIA.3)/sum(AIME.3)
tot.RR.4 = sum(PIA.4)/sum(AIME.4)
tot.RR.5 = sum(PIA.5)/sum(AIME.5)
tot.RR.6 = sum(PIA.6)/sum(AIME.6)
tot.RR.7 = sum(PIA.7)/sum(AIME.7)
tot.RR = c(tot.RR.1,tot.RR.2,tot.RR.3,tot.RR.4,tot.RR.5,tot.RR.6,tot.RR.7)

mean(taxable.percent.2);mean(taxable.percent.3);mean(taxable.percent.4);mean(taxable.percent.5);mean(taxable.percent.6);mean(taxable.percent.7)
