require(boot)
require(zoo)
require(MASS)

source('util.R')

COLORS = c('black',  'red', 'blue', 'green', 'purple', 'magenta1', 'gray',
         'cyan', 'darkgreen', 'brown', 'wheat')

unpack = function(l) {
    e = parent.env(environment())
    ns = names(l)
    for (i in 1:length(ns)) {
        assign(ns[i], l[[ns[i]]], envir=e)
    }
}

debug = function(s, verbose) { if (verbose) { cat(s) } }

get_perfect_model = function(C, gammas, alpha=0) {
    model = list(C=C, gammas=gammas, alpha=alpha)
    pred_func = function(model, X, y) inv.logit(as.vector(model$alpha + model$C * ((X^2 - 1)/sqrt(2)) %*% model$gammas))
    expl_func = function(model, X, y) X^2 %*% diag(model$C*model$gammas/sqrt(2))
    opt_func = function(model, X, y) 0*model$gammas
    return(list(model=model, pred_func=pred_func, expl_func=expl_func, opt_func=opt_func))
}

find_alarm_start = function(p_mw, start, alarm_p, alarm_w, align='right') {
    n = length(p_mw)
    p_alarm = rollapply(p_mw, alarm_w, min, align=align, fill=NA) >= alarm_p
    alarm_start = which(p_alarm & 1:n >= start)
    if (length(alarm_start) == 0) {
        return(NA)
    } else {
        return(min(alarm_start))
    }
}

find_alarm_end = function(p_mw, alarm_start, alarm_w, alarm_p, alarm_lag, align='right') {
    n = length(p_mw)
    p_alarm = rollapply(p_mw, alarm_w, min, align=align, fill=NA) >= alarm_p
    alarm_end = which(1:(n - 1) > alarm_start & (is.na(p_alarm[2:n]) | !p_alarm[2:n]))
    if (length(alarm_end) == 0) {
        alarm_end = n
    }
    alarm_end = min(c(alarm_end, alarm_start + alarm_lag))
    return(alarm_end)
}

fix_cause = function(Z, X, y, ix, model, delta, phi=.999) {
    n = nrow(Z)
    m = ncol(Z)
    offsets = model$opt_func(model$model, X[ix,,drop=F]) - colMeans(X[ix,,drop=F])
    n_fix = 0
    fix_start = min(ix)
    for (j in 1:m) {
        if (abs(offsets[j]) > delta) {
            Z[,j] = Z[,j,drop=F] + offsets[j]*(phi^(0:(n - 1)))
            n_fix = n_fix + 1
        }
    }
    return(list(new_Z=Z, n_stop=ifelse(n_fix == 0, 0, 1), n_fix=n_fix))
}

compute_cost = function(p, n_stop, n_fix, c_stop=50, c_fix=10) {
    cost = c(sum(p), n_stop*c_stop, n_fix*c_fix)
    cost = c(cost, sum(cost))
    names(cost) = c('p', 'stop', 'fix', 'total')
    return(cost)
}

plot_costs = function(costs, filename=NULL, xlab='', xaxt=NULL, main='') {
    if (!is.null(filename)) { pdf(filename) }

    n_reps = nrow(costs[[1]])
    means = do.call(rbind, lapply(costs, colMeans))
    se_means = do.call(rbind, lapply(costs, function(X) apply(X, 2, sd)))/sqrt(n_reps)

    ylim = c(0, max(means + se_means))
    plot(means[,4], type='l', ylim=ylim, xlab=xlab, ylab='Cost', main=main,
         xaxt=ifelse(is.null(xaxt), 's', 'n'))
    for (i in 1:3) {
        lines(means[,i], lty=5, col=COLORS[i])
    }
    for (i in 1:4) {
        error_bars(1:nrow(means), means[,i] - se_means[,i], means[,i] + se_means[,i], 
                   col=COLORS[c(1:3, 1)[i]])
    }


    legend(x=length(means[,4]), y=max(means), xjust=1,
           lty=c(5, 5, 5, 1), col=COLORS[c(1:3, 1)], 
           legend=c('Repairs', 'Peaks', 'Fixes', 'Total'))   
    if (!is.null(xaxt)) {
        axis(1, at=1:nrow(means), labels=xaxt)
    }

    if (!is.null(filename)) { dev.off() }
}

flatten_costs = function(costs) {
    if (length(costs) == 1 && class(costs[[1]]) == 'list') {
        return(lists[[1]])
    }
    if (all(lapply(costs, length) == 1) && all(lapply(costs, class) == 'list')) {
        return(do.call(c, costs))
    }
    return(costs)
}

block_boot = function(n, l, n_reps=1) {
    ix = matrix(NA, n, n_reps)
    for (r in 1:n_reps) {
        lo = 1
        for (i in 1:ceiling(n/l)) {
            hi = min(c(lo + l - 1, n))
            l2 = hi - lo + 1
            start = sample(1:(n - l2 + 1), 1)
            ix[lo:hi,r] = start:(start + l2 - 1)
            lo = hi + 1
        }
    }
    return(ix)
}

var1_sim = function(n, m, phi, Sigma=NULL, burnin=10) {
    Z = matrix(NA, n, m)
    for (i in 1:m) {
        Z[,i] = arima.sim(list(ar=phi), n)*sqrt(1 - phi^2)
    }
    return(Z)
}

