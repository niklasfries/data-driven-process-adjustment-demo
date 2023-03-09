source('lib.R')
source('params.R')

# Run single simulation
#
# Input:
# - X: T by m matrix of explanatory column variables
# - y: Vector of length T with the response variable (not used)
# - model: list with 3 elements:
#   - pred_func: function which taḱes the model object, X, y as input 
#                and predicts the defect probabilities
#   - expl_func: function which taḱes the model object, X, y as input 
#                and computes attributions for each x-variable
#   - opt_func:  function which taḱes the model object, X, y as input 
#                and returns optimal values for each x-variable
# - alarm_params: see params.R
# - true_model: same as model, copmutes true costs if provided
# - verbose: print progress if TRUE (default: FALSE)
# - plot: plots to the current sink if TRUE (default: FALSE)
#
# Output: list with the following elements:
# - cost: costs according to model
# - true_cost: costs according to true_model, or NULL
# - p: estimated defect probabilities according to model
# - adjustments: cumulative number of stops and fixes
single_run = function(X, y, model, alarm_params=default_alarm_params(), true_model=NULL, verbose=F, plot=F) {
    unpack(check_alarm_params(alarm_params, scalar=T))

    n = nrow(X)
    m = ncol(X)
    Z = matrix(0, n, m)

    ## Compute true and estimated probabilities
    p = model$pred_func(model$model, X + Z, y)
    p_mw = rollapply(p, alarm_mw, mean, align='right', fill=NA)

    if (plot) {
        par(mar=c(3.5, 2, .1, .1))
        plot(1:n, 0*(1:n), type='l', ylim=c(0, 1), lty=3, col='white', xlab='', ylab='')
        title(xlab='Time', line=2.5)
        # title(ylab='Defect probability', line=2.5)
        lims = par('usr')
        abline(h=alarm_p, lty=3)
    }

    adjustments = matrix(0, n, 2)
    colnames(adjustments) = c('stop', 'fix')
    init_cost = cumsum(p)

    n_stop = 0
    n_fix = 0
    start = 1
    while (start < n - alarm_w) {
        debug(sprintf('start: %d\n', start), verbose)

        ## Find next alarm
        alarm_start = find_alarm_start(p_mw, start, alarm_p, alarm_w)
        if (is.na(alarm_start) || alarm_start > n - alarm_w) {
            break
        }
        debug(sprintf('alarm_start: %d\n', alarm_start), verbose)

        if (plot) {
            plot_ix = start:min(c(n, alarm_start + 200))
            lines(plot_ix, p_mw[plot_ix], type='l', lty=5)
        }

        # Fix cause
        alarm_ix = alarm_start:min(c(alarm_start + alarm_w - 1, n))
        fix = fix_cause(Z=Z[alarm_start:n,], X=X + Z, y=y, ix=alarm_ix, model=model, delta=alarm_delta)
        Z[alarm_start:n,] = fix$new_Z
        n_stop = n_stop + fix$n_stop
        n_fix = n_fix + fix$n_fix
        debug(sprintf('n_fix: %d\n', fix$n_fix), verbose)
        adjustments[min(alarm_ix):n,1] = adjustments[min(alarm_ix):n,1] + fix$n_stop
        adjustments[min(alarm_ix):n,2] = adjustments[min(alarm_ix):n,2] + fix$n_fix

        ## Recompute
        p[alarm_start:n] = model$pred_func(model$model, X[alarm_start:n,] + Z[alarm_start:n,], y[alarm_start:n])
        p_mw[alarm_start:n] = rollapply(p[(alarm_start - alarm_mw + 1):n], alarm_mw, mean)

        # End of alarm
        alarm_end = find_alarm_end(p_mw, alarm_start, alarm_w, alarm_p, alarm_lag)

        if (plot) {
            rect(alarm_start - alarm_w - .5, lims[3], alarm_start - .5, lims[4], 
                 border=NA, col=rgb(0, 0, 0, 1/5))
            abline(v=alarm_start - alarm_w - .5, lty=3)
            abline(v=alarm_start - .5)
        }
        debug(sprintf('alarm_end: %d\n', alarm_end), verbose)

        start = max(alarm_end, alarm_start + alarm_lag)
    }

    cum_cost = apply(adjustments %*% diag(c(50, 10)), 1, sum) + cumsum(p)
    if (plot) {
        lines(p_mw, type='l')
        max_cost = max(c(init_cost, cum_cost))
        lines(cum_cost/max_cost, col='red')
        lines(init_cost/max_cost, col='red', lty=5)
    }

    p = model$pred_func(model$model, X + Z, y)
    cost = compute_cost(p, n_stop, n_fix)
    if (!is.null(true_model)) {
        true_p = true_model$pred_func(true_model$model, X + Z, y)
        true_cost = compute_cost(true_p, n_stop, n_fix)
    } else {
        true_cost = NULL
    }
    debug(sprintf('cost: p: %.1f + %.1f + %d = %.1f\n', 
                  cost[1], cost[2], cost[3], cost[4]), verbose)

    return(list(cost=cost, true_cost=true_cost, p=p, adjustments=adjustments))
}

