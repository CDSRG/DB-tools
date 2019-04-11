#LOCATIONS FOR TRUE VALUES
#setA 1:343
#setB 1:156,344:492
#setC 1:84,157:281,344:358,493:509
#setD 1:7,157:227,344:353,493:499,510:516
#setE 1:34,85:94,157:188,228:231,282:293,344:346,359:364,517:561
#setF 74:77,85:89,95:125,228:232,282:287,294:337,365:377
#setG 1:8,35:44,95:100,126:131,157:163,189:196,294:299,338:343,378:382,517:521,562:578
#setH 579:596
#setI 562:565,597:602
#setJ 603:607


setA <- c(rep(1,343),rep(0,264))
setB <- c(rep(1,156),rep(0,187),rep(1,149),rep(0,115))
setC <- c(rep(1,84),rep(0,72),rep(1,125),rep(0,62),rep(1,15),rep(0,134),rep(1,17),rep(0,98))
setD <- c(rep(1,73),rep(0,83),rep(1,71),rep(0,116),rep(1,10),rep(0,139),rep(1,7),rep(0,10),rep(1,7),rep(0,91))
setE <- c(rep(1,34),rep(0,50),rep(1,10),rep(0,62),rep(1,32),rep(0,39),rep(1,4),rep(0,50),rep(1,12),rep(0,50),rep(1,3),rep(0,12),rep(1,6),rep(0,152),rep(1,45),rep(0,46))
setF <- c(rep(0,73),rep(1,4),rep(0,8),rep(1,5),rep(0,5),rep(1,31),rep(0,102),rep(1,5),rep(0,49),rep(1,6),rep(0,6),rep(1,44),rep(0,27),rep(1,13),rep(0,229))
setG <- c(rep(1,8),rep(0,26),rep(1,10),rep(0,50),rep(1,6),rep(0,25),rep(1,6),rep(0,25),rep(1,7),rep(0,25),rep(1,8),rep(0,99),rep(1,6),rep(0,38),rep(1,6),rep(0,34),rep(1,5),rep(0,134),rep(1,5),rep(0,40),rep(1,17),rep(0,27))
setH <- c(rep(0,576),rep(1,20),rep(0,11))
setI <- c(rep(0,561),rep(1,4),rep(0,31),rep(1,6),rep(0,5))
setJ <- c(rep(0,602),rep(1,5))

allSets <- c(setA, setB, setC, setD, setE, setF, setG, setH, setI, setJ)

allSets <- matrix(allSets, 607, 10)
allSets <- as.data.frame(allSets)

install.packages("UpSetR")
require(UpSetR)

upset(allSets, order.by="freq", nsets=10, show.numbers=FALSE)



