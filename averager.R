# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/Impulse");

#overall averager

averager <-function(la,mult,le,ele){
  
  qb1filenames <- list.files((paste0("Outputs/variable_speed/",la,"3")), pattern = (paste0(le,"_",mult,"x_graph.csv")), full.names = TRUE)
  qb1ldf <- lapply(qb1filenames, read.csv)
  qb1alldf <- do.call(rbind, qb1ldf)
  #mean
  qb1meres <- lapply(qb1alldf, function(x) {
    tapply(x, qb1alldf$X, mean)
  })
  qb1meres <- do.call(cbind, qb1meres)[,1:3]
  #max
  qb1mares <- lapply(qb1alldf, function(x) {
    tapply(x, qb1alldf$X, max)
  })
  qb1mares <- do.call(cbind, qb1mares)[,1:3]
  #min
  qb1mires <- lapply(qb1alldf, function(x) {
    tapply(x, qb1alldf$X, min)
  })
  qb1mires <- do.call(cbind, qb1mires)[,1:3]
  #stdev
  qb1sdres <- lapply(qb1alldf, function(x) {
    tapply(x, qb1alldf$X, sd)
  })
  qb1sdres <- do.call(cbind, qb1sdres)[,1:3]
  
  qb1exp<- data.frame(qb1meres,qb1mares,qb1mires,qb1sdres)
  colnames(qb1exp) <- c("X1","mean.t","mean.h","X2","max.t","max.h","X3","min.t","min.h","X4","sd.t","sd.h")
  write.csv(qb1exp, (paste0("Outputs/variable_speed/Overall/",ele,le,mult,".csv")))
}
averager (la="Quad",mult="1",le="b",ele="q")
averager (la="Quad",mult="1",le="c",ele="q")
averager (la="Quad",mult="1",le="o",ele="q")
averager (la="Counter",mult="1",le="b",ele="c")
averager (la="Counter",mult="1",le="c",ele="c")
averager (la="Counter",mult="1",le="o",ele="c")
averager (la="Burst",mult="1",le="b",ele="b")
averager (la="Burst",mult="1",le="c",ele="b")
averager (la="Burst",mult="1",le="o",ele="b")

averager2 <-function(la,mult,le,ele){
  
  qb1filenames <- list.files((paste0("Outputs/variable_speed/",la,"3")), pattern = (paste0(le,"_",mult,"x_mm_graph.csv")), full.names = TRUE)
  qb1ldf <- lapply(qb1filenames, read.csv)
  qb1alldf <- do.call(rbind, qb1ldf)
  #mean
  qb1meres <- lapply(qb1alldf, function(x) {
    tapply(x, qb1alldf$X, mean)
  })
  qb1meres <- do.call(cbind, qb1meres)[,1:3]
  #max
  qb1mares <- lapply(qb1alldf, function(x) {
    tapply(x, qb1alldf$X, max)
  })
  qb1mares <- do.call(cbind, qb1mares)[,1:3]
  #min
  qb1mires <- lapply(qb1alldf, function(x) {
    tapply(x, qb1alldf$X, min)
  })
  qb1mires <- do.call(cbind, qb1mires)[,1:3]
  #stdev
  qb1sdres <- lapply(qb1alldf, function(x) {
    tapply(x, qb1alldf$X, sd)
  })
  qb1sdres <- do.call(cbind, qb1sdres)[,1:3]
  
  qb1exp<- data.frame(qb1meres,qb1mares,qb1mires,qb1sdres)
  colnames(qb1exp) <- c("X1","mean.t","mean.h","X2","max.t","max.h","X3","min.t","min.h","X4","sd.t","sd.h")
  write.csv(qb1exp, (paste0("Outputs/variable_speed/Overall/",ele,le,mult,".csv")))
}

averager2 (la="Quad",mult="2",le="b",ele="q")
averager2 (la="Quad",mult="2",le="c",ele="q")
averager2 (la="Quad",mult="2",le="o",ele="q")
averager2 (la="Quad",mult="2.5",le="b",ele="q")
averager2 (la="Quad",mult="2.5",le="c",ele="q")
averager2 (la="Quad",mult="2.5",le="o",ele="q")
averager2 (la="Quad",mult="3",le="b",ele="q")
averager2 (la="Quad",mult="3",le="c",ele="q")
averager2 (la="Quad",mult="3",le="o",ele="q")

averager2 (la="Counter",mult="2",le="b",ele="c")
averager2 (la="Counter",mult="2",le="c",ele="c")
averager2 (la="Counter",mult="2",le="o",ele="c")
averager2 (la="Counter",mult="2.5",le="b",ele="c")
averager2 (la="Counter",mult="2.5",le="c",ele="c")
averager2 (la="Counter",mult="2.5",le="o",ele="c")
averager2 (la="Counter",mult="3",le="b",ele="c")
averager2 (la="Counter",mult="3",le="c",ele="c")
averager2 (la="Counter",mult="3",le="o",ele="c")

averager2 (la="Burst",mult="2",le="b",ele="b")
averager2 (la="Burst",mult="2",le="c",ele="b")
averager2 (la="Burst",mult="2",le="o",ele="b")
averager2 (la="Burst",mult="2.5",le="b",ele="b")
averager2 (la="Burst",mult="2.5",le="c",ele="b")
averager2 (la="Burst",mult="2.5",le="o",ele="b")
averager2 (la="Burst",mult="3",le="b",ele="b")
averager2 (la="Burst",mult="3",le="c",ele="b")
averager2 (la="Burst",mult="3",le="o",ele="b")
