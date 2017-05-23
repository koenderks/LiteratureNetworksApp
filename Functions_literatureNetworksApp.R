.logProposal <- function(z, n, r){
    
    -((n-3)^1)/2*(z-atanh(r))^2 
    
}

myTruncNormSim <- function(lBound = -Inf, uBound = Inf, mu = 0, sd = 1){
    
    lBoundUni <- pnorm(lBound, mean = mu, sd = sd)
    
    uBoundUni <- pnorm(uBound, mean = mu, sd = sd) 
    
    mySample <- qnorm(runif(1, lBoundUni, uBoundUni), mean = mu, sd = sd)
    
    return(mySample)
    
}

PolyCorFisherBivNorm <- function(xranks,yranks, nSamples = 10000,switchLL = T,
                                 initMeth = "qnorm", nChains = 1, inDepSampler = T){
    require(MASS)
    require(mvtnorm)
    
    n <- length(xranks)
    
    initx <- (sort(rnorm(n))[xranks])
    
    inity <- (sort(rnorm(n))[yranks])
    
    if(initMeth == "qnorm"){
        
        initx <- qnorm((xranks)/(2*n+1))
        
        inity <- qnorm((yranks)/(2*n+1))
        
    }
    
    rho.Samples <- numeric(nSamples)
    
    sampledx <- matrix(nrow=nSamples,ncol=n)
    
    sampledy <- matrix(nrow=nSamples,ncol=n)
    
    xvals <- initx
    
    yvals <- inity
    
    mySigma <- diag(2)
    
    old.Rho.Prop <- cor(xvals,yvals)
    
    logposterold <- -Inf
    
    for(j in 1:nSamples){
        
        for(i in sample(1:n)){
            
            mmx <- mean(yvals[yranks==yranks[i]])
            
            mmy <- mean(xvals[xranks==xranks[i]])
            
            underx <- xvals[xranks<xranks[i]][order(xvals[xranks<xranks[i]],decreasing = T)][1]
            
            upperx <- xvals[xranks>xranks[i]][order(xvals[xranks>xranks[i]],decreasing = F)][1]
            
            if(is.na(underx)){underx <- -Inf}
            
            if(is.na(upperx)){upperx <- Inf}
            
            xvals[i] <- myTruncNormSim(underx,upperx,mu=(old.Rho.Prop*mmx),sd=sqrt(1-old.Rho.Prop^2))
            
            undery <- yvals[yranks<yranks[i]][order(yvals[yranks<yranks[i]],decreasing = T)][1]
            
            uppery <- yvals[yranks>yranks[i]][order(yvals[yranks>yranks[i]],decreasing = F)][1]
            
            if(is.na(undery)){undery <- -Inf}
            
            if(is.na(uppery)){uppery <- Inf}
            
            yvals[i] <- myTruncNormSim(undery,uppery,mu=(old.Rho.Prop*mmy),sd=sqrt(1-old.Rho.Prop^2))
            
        }
        
        xvals <- (xvals-mean(xvals))/sd(xvals)
        
        yvals <- (yvals-mean(yvals))/sd(yvals)
        
        if(switchLL) logposterold <- sum(dmvnorm(cbind(xvals,yvals),c(0,0),sigma=mySigma,log=T)) # calculate loglikelihood for latent values, given proposed rho
        
        if(inDepSampler){
            
            rho.Prop <- .metropolisOneStepBivNormIndep(old.Rho.Prop, n, cor(xvals,yvals), kappa=1, xvals,yvals,logposterold)
            
        } else{rho.Prop <- .metropolisOneStepBivNorm(old.Rho.Prop, n, cor(xvals,yvals), kappa=1, xvals,yvals,logposterold)}
        
        mySigma[2,1] <- mySigma[1,2] <- rho.Prop # input the proposal
        
        logposterold <- sum(dmvnorm(cbind(xvals,yvals),c(0,0),sigma=mySigma,log=T)) # calculate loglikelihood for latent values, given proposed rho
        
        rho.Samples[j] <- rho.Prop
        
        old.Rho.Prop <- rho.Samples[j] # use latest sample for comparing to next proposal
        
        sampledx[j,] <- xvals
        
        sampledy[j,] <- yvals
        
    }
    
    resultsList <- list(rhoSamples = rho.Samples, xSamples = sampledx, ySamples = sampledy)
    
    return(resultsList)
    
}

.metropolisOneStepBivNorm <- function (rhoCurrent, n, r, kappa=1,xvals,yvals, logposterold) {
    
    mySigma <- diag(2)
    
    zCurrent <- atanh(rhoCurrent)
    
    zCandidate <- rnorm(1, mean=atanh(rhoCurrent), sd=1/sqrt(n-3))
    
    rho.Prop <- tanh(zCandidate)
    
    mySigma[2,1] <- mySigma[1,2] <- rho.Prop # input the proposal
    
    logposternew <- sum(dmvnorm(cbind(xvals,yvals),c(0,0),sigma=mySigma,log=T)) # calculate loglikelihood for latent values, given proposed rho
    
    llRatio <- exp(logposternew-logposterold) 
    
    propRatio <- 0 #exp(.logProposal(zCurrent, n, rho.Prop)-.logProposal(zCandidate, n, rhoCurrent))
    #print(c(rho.Prop,llRatio, propRatio, (llRatio+propRatio)))
    
    accept.prob <- min(1,((llRatio+propRatio))) # calculate acceptance probability
    
    if(runif(1) < accept.prob){
        
        return(rho.Prop)
        
    }
    else{
        
        return(rhoCurrent)# if new proposal not accepted, keep the old proposal
        
    }
}

.metropolisOneStepBivNormIndep <- function (rhoCurrent, n, r, kappa=1,xvals,yvals, logposterold) {
    # 
    mySigma <- diag(2)
    
    zCurrent <- atanh(rhoCurrent)
    
    zCandidate <- rnorm(1, mean=atanh(r), sd=1/sqrt(n-3))
    
    rho.Prop <- tanh(zCandidate)
    
    mySigma[2,1] <- mySigma[1,2] <- rho.Prop # input the proposal
    
    logposternew <- sum(dmvnorm(cbind(xvals,yvals),c(0,0),sigma=mySigma,log=T)) # calculate loglikelihood for latent values, given proposed rho
    
    llRatio <- exp(logposternew-logposterold) 
    
    propRatio <- exp(.logProposal(zCurrent, n, r)-.logProposal(zCandidate, n, r))
    #print(c(rho.Prop,llRatio, propRatio, (llRatio+propRatio)))
    
    accept.prob <- min(1,((llRatio*propRatio))) # calculate acceptance probability
    # print(accept.prob)
    
    if(runif(1) < accept.prob){
        
        return(rho.Prop)
        
    }
    else{
        
        return(rhoCurrent)# if new proposal not accepted, keep the old proposal
        
    }
}