#SaveFile <- 2

scp42 <- read.table(paste("A:/OneDrive - Instituto Superior de Engenharia do Porto/Doutoramento/Heuristicas/R/Meta-RaPS/SCP-Instances/scp4",SaveFile,".txt",sep=""), sep = "", header = FALSE, fill = T) 
#scp42 <- read.table("A:/OneDrive - Instituto Superior de Engenharia do Porto/Doutoramento/Heuristicas/R/Meta-RaPS/SCP-Instances/scp42.txt", sep = "", header = FALSE, fill = T) 
#scp42 <- read.table("C:/Users/Miguel/OneDrive - Instituto Superior de Engenharia do Porto/Doutoramento/Heuristicas/R/Meta-RaPS/SCP-Instances/scp49.txt", sep = "", header = FALSE, fill = T) 


scp42W <- scp42[2:85,]
scp42W <- c(t(scp42W))
scp42W <- scp42W[1:1000]
rownames(scp42W) <- NULL

scp42A <- scp42[86:nrow(scp42),]
rownames(scp42A) <- NULL
scp42Att <- scp42A[1:2,]
scp42Att[1,13:36] <- NA

for (i in 3:nrow(scp42A)){
  if (       scp42A[i,1]>max(scp42A[i-1,],na.rm=T)  &&  is.na(scp42A[i-2,12])==FALSE  &&  is.na(scp42A[i-1,12])==FALSE  &&  is.na(scp42A[i-3,12])==TRUE){
    scp42Att[i-2,25:36] <- scp42A[i,]
  } else if (scp42A[i,1]>max(scp42A[i-1,],na.rm=T)  &&  is.na(scp42A[i-1,12])==FALSE  &&  is.na(scp42A[i-2,12])==TRUE){
    scp42Att[i-1,13:24] <- scp42A[i,]
  } else {
    scp42Att[i,1:12] <- scp42A[i,]
  }
}



AttsComb <- scp42Att[complete.cases(scp42Att[ ,2]),]
AttComb <- matrix(0,nrow=200,ncol=length(scp42W))
for (i in 1:200){
  NoNA <- t(AttsComb[i,!is.na(AttsComb[i,])]) #sets na linha i que não são NAs
  AttComb[i,NoNA] <- 1
}


ChosenSets <- cbind(0,as.data.frame(c(t(scp42W))))
colnames(ChosenSets) <- c("Chosen?","Weight")
X <- cbind(c(1:length(scp42W)),ChosenSets) #Solution Set
colnames(X)[1] <- c("ID")
remove(ChosenSets)
X[,4:5] <- c(0)
colnames(X)[4] <- c("k") #No. of currently uncovered rows that could be covered by set
colnames(X)[5] <- c("Dinamic cost")


ChosenAtts <- as.data.frame(c(0))
ChosenAtts[1:200,1] <- 0
colnames(ChosenAtts) <- c("Chosen?")
I <- cbind(c(1:200),ChosenAtts) #Uncovered Rows
colnames(I)[1] <- c("ID")
remove(ChosenAtts)
I[,3] <- 0
colnames(I)[3] <- c("o") #No. selected sets that satisfy this attribute





#Remover sets dominados

SetComb <- list()


for (i in 1:nrow(X)){  #What attributes does this set satisfy
  SetComb[[i]] <- which(AttComb[,i]==1)
}


FR <- 0 #for removal

for (i in 1:nrow(X)){
  NT <- 1:nrow(X) #Not being Tested [!=i]
  for (l in NT[NT!=i])
    if(  all(SetComb[[i]] %in% SetComb[[l]]) & X[i,3]>=X[l,3]  ){ #todos os atributos cobertos por i são cobertos por j e o custo de i é maior ou igual que j. marcar i para remover mais tarde
      FR <- c(FR,i)
    }
}


FR <- unique(FR)[-1]
length(FR)

X <- X[-FR,]
AttComb <- AttComb[,-FR]
X[,1] <- 1:nrow(X)



#Incluir sets obrigatórios logo à partida

SO <- which(rowSums(AttComb)==1)
X[SO,2] <- 1
for(i in SO){
  I[  which(AttComb[,SO]==1),2] <- 1
  I[  which(AttComb[,SO]==1),3] <- I[  which(AttComb[,SO]==1),3]+1
}



remove(scp42,scp42A,scp42Att,i,scp42W,NoNA,AttsComb,l,NT)


save.image(file = paste("X4",SaveFile,".RData",sep = ""))

SaveFile = SaveFile+1
