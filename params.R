
default_alarm_params = function() 
    list(alarm_mw=20, alarm_w=20, alarm_lag=40, alarm_p=.7, alarm_delta=.1)

get_gammas = function(m, dist) {
    dists = c('point', 'unif', 'tri', 'geo')
    stopifnot(dist %in% dists)
    if (dist == 'point') { gammas = rep(1, m) }
    if (dist == 'unif') { 
        x = rep(floor(m/10), 10)
        rem = m - sum(x)
        if (rem > 0) {
            x[1:rem] = x[1:rem] + 1
        }
        gammas = do.call(c, lapply(1:length(x), function(t) rep(t, x[t])))
    }
    if (dist == 'tri') { 
        x = round((10:1)*(m/(10*11/2)))
        rem = m - sum(x)
        if (rem > 0) {
            x[1:rem] = x[1:rem] + 1
        } else if (rem < 0) {
            rem = -rem
            x[1:rem] = x[1:rem] - 1
        }
        gammas = do.call(c, sapply(1:length(x), function(t) rep(t, x[t])))
    }
    if (dist == 'geo') { gammas = qgeom((0:(m - 1))/m, .6) }
    gammas = gammas/sqrt(sum(gammas^2))
    return(gammas)
}

check_params = function(params, def_params, scalar) {
    if (length(params) == 0) {
        return(def_params)
    }
    par_names = names(params)
    for (i in 1:length(par_names)) {
        if (!par_names[i] %in% names(def_params)) {
            stop(sprintf('Invalid parameter: %s', par_names[i]))
        }
    }
    for (i in 1:length(def_params)) {
        if (!names(def_params)[i] %in% names(params)) {
            params[names(def_params)[i]] = def_params[names(def_params)[i]]
        }
    }
    par_names = names(params)
    for (i in 1:length(par_names)) {
        if (scalar && length(params[[par_names[i]]]) != 1) {
            stop(sprintf('Non-scalar parameter: %s', par_names[i]))
        }
    }
    return(params)
}

check_alarm_params = function(params=default_alarm_params(), scalar=F)
    check_params(params=params, def_params=default_alarm_params(), scalar=scalar)

