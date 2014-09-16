#####  fdr-sim.R
#####
#####
################################################

##  Just a flat function for generating levels for false hypotheses
f.flat <- function(mm,pp,LL){
    total <- floor(mm*pp)
    mus <- rep(c(LL,LL/2,LL/4,3*LL/4),ceiling(total/4))
    zeros <- rep(0,mm - total)
    return(c(zeros,mus[1:total]))
}

##  Generating some z-values given parameter specifications
gen <- function(mm, pp, LL, ff){
    true.means <- ff(mm,pp,LL)
    zz <- rnorm(mm, true.means, 1)
    truth <- (true.means == 0)
    p.vals <- round(pmin((1 - pnorm(zz)),pnorm(zz))*2,4)
    dat <- data.frame(truth = truth,
                      true.mean=true.means,
                      zz = zz,
                      p.vals = p.vals)

    return(dat)
        #list(zz = zz, p.vals = p.vals,
         #       truth = truth, means.true = true.means))
}


##  Accept/Reject based on FDR procedure
est.fdr <- function(p.vals,qq){
    p.ord <- order(p.vals)
    p.sort <- sort(p.vals)
    mm <- length(p.vals)

    ##  Performing the FDR procedure on ordered p-values
    labs.ord <- (p.sort <= (1:mm)*qq/mm)

    ##  Creating TRUE/FALSE labels for original ordering
    labs <- rep(TRUE,mm)
    labs[p.ord[labs.ord]] <- FALSE
    return(labs)

}

##  Accept/Reject based on Bonferoni procedure
est.bon <- function(p.vals,alpha){
    return(p.vals > alpha/length(p.vals))
}

##  Accept/Reject with no correction
est.naive <- function(p.vals,alpha){
    return(p.vals > alpha)
}

##  Accept/Reject based on hochberg
est.hoch <- function(p.vals, alpha){
    p.ord <- order(p.vals)
    p.sort <- sort(p.vals)
    mm <- length(p.vals)

    ##  Performing the FDR procedure on ordered p-values
    labs.ord <- (p.sort <= alpha/(mm + 1 - 1:mm))

    ##  Creating TRUE/FALSE labels for original ordering
    labs <- rep(TRUE,mm)
    labs[p.ord[labs.ord]] <- FALSE
    return(labs)

}

##  Add significance test results to data.frame
make.df <- function(dat,alpha=.05,qq=.05){
    naive.labs <- est.naive(dat$p.vals,alpha)
    fdr.labs <- est.fdr(dat$p.vals,qq)
    hoch.labs <- est.hoch(dat$p.vals,alpha)
    bon.labs <- est.bon(dat$p.vals,alpha)
    full.dat <- cbind(dat,
                      naive=naive.labs,fdr=fdr.labs,
                      hochberg=hoch.labs,bonferoni=bon.labs)
    return(full.dat)
}

##  Compute the Power of all tests
get.power <- function(full.dat){
    false.dat <- full.dat[!full.dat$truth,]
    naive.pow <- mean(!false.dat$naive)
    fdr.pow <- mean(!false.dat$fdr)
    hoch.pow <- mean(!false.dat$hoch)
    bon.pow <- mean(!false.dat$bon)
    return(c(naive=naive.pow,fdr=fdr.pow,
             hochberg=hoch.pow,bonferonic=bon.pow))
}

##  Compute the Per Comparison Error Rate for all tests
get.pcer <- function(full.dat){
    true.dat <- full.dat[full.dat$truth,]
    naive.pcer <- mean(!true.dat$naive)
    fdr.pcer <- mean(!true.dat$fdr)
    hoch.pcer <- mean(!true.dat$hoch)
    bon.pcer <- mean(!true.dat$bon)
    return(c(naive=naive.pcer,fdr=fdr.pcer,
             hochberg=hoch.pcer,bonferonic=bon.pcer))
}

##  Compute the Family Wise Error Rate of all tests
get.fwer <- function(full.dat){
    true.dat <- full.dat[full.dat$truth,]
    naive.fwer <- max(!true.dat$naive)
    fdr.fwer <- max(!true.dat$fdr)
    hoch.fwer <- max(!true.dat$hoch)
    bon.fwer <- max(!true.dat$bon)
    return(c(naive=naive.fwer,fdr=fdr.fwer,
             hochberg=hoch.fwer,bonferonic=bon.fwer))

}

##  Compute the False Discovery Rate of all tests
get.fdr <- function(full.dat){
    naive.fdr <- with(full.dat,sum((!naive) * truth)/sum(!naive))
    fdr.fdr <- with(full.dat,sum((!fdr) * truth)/sum(!fdr))
    hoch.fdr <- with(full.dat,sum((!hochberg) * truth)/sum(!hochberg))
    bon.fdr <- with(full.dat,sum((!bonferoni) * truth)/sum(!bonferoni))

    return(c(naive=naive.fdr,fdr=fdr.fdr,
             hochberg=hoch.fdr,bonferonic=bon.fdr))

}

##  Generate a set of sample values,
##  perform significance testing,
##  and analyze the results
run.exp <- function(mm,pp,LL,ff,alpha=.05,qq=alpha){
    dat <- gen(mm,pp,LL,ff)
    full.dat <- make.df(dat,alpha,qq)
    exp.pow <- get.power(full.dat)
    exp.pcer <- get.pcer(full.dat)
    exp.fwer <- get.fwer(full.dat)
    exp.fdr <- get.fdr(full.dat)
    
    ret = list()
    ret$table = rbind(pow=exp.pow,pcer=exp.pcer,fwer=exp.fwer,fdr=exp.fdr)
    ret$full.dat = full.dat
    return(ret)
}


mm <- 16  # total hypothesese
pp <- 0.5 # percentage of true hypotheses
LL <- 5   # maximum true effect
qq <- alpha <- 0.05

# out <- run.exp(mm,pp,LL,f.flat,.05,.05)
# out

# reps <- 2e4
# system.time(big.out <- replicate(reps,run.exp(mm,pp,LL,f.flat,alpha,qq)))

# apply(big.out,c(1,2),mean)
