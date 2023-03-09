
COLORS = c('black',  'red', 'blue', 'green', 'purple', 'magenta1', 'gray',
         'cyan', 'darkgreen', 'brown', 'wheat')

my_load = function(filename) {
    e = new.env()
    load(filename, e)
    return(as.list(e))
}

rmse = function(x) {
    return(sqrt(mean(x^2)))
}

progress_bar = function(current, max, scale=1) {
    scaled_current = floor(current*scale)
    scaled_max = floor(max*scale)
    printf('\r[')
    if (scaled_current > 0) {
        for (i in 1:scaled_current) {
            printf('=')
        }
    }
    if (scaled_current < scaled_max) {
        for (i in (scaled_current + 1):scaled_max) {
            printf(' ')
        }
    }
    if (current < max) {
        printf(']')
    } else {
        printf(']\n')
    }
}

error_bars = function(t, low, high, col='black', dir='v', lty=1, lwd=1) {
    stopifnot(dir %in% c('h', 'v'))

    keep_ix = which(low != high)
    t = t[keep_ix]
    low = low[keep_ix]
    high = high[keep_ix]

    if (dir == 'v') {
        x1 = t
        x2 = t
        y1 = low
        y2 = high
    } else if (dir == 'h') {
        x1 = low
        x2 = high
        y1 = t
        y2 = t
    }
    arrows(x1, y1, x2, y2, length=.05, angle=90, code=3, col=col, lty=lty, lwd=lwd)
}

columns = function(m) lapply(seq_len(ncol(m)), function(i) m[,i,drop=F])

zip = function(...) {
    lists = list(...)
    stopifnot(length(unique(lapply(lists, length))) == 1)
    return(lapply(1:length(lists[[1]]), function(i) lapply(lists, function(l) l[[i]])))
}

number_columns = function(m) 
    do.call(rbind, lapply(zip(as.list(1:ncol(m)), columns(rmses)), function(z) do.call(cbind, z)))

shuffle = function(x) {
    return(sample(x, length(x), replace=F))
}

which.closest = function(x, vec, log=F) {
    stopifnot(length(x) == 1)
    if (!log) {
        return(which.min(abs(vec - x)))
    } else {
        return(which.min(abs(log(vec/x))))
    }
}

which.closest = function(x, vec, log=F, n=1) {
    stopifnot(length(x) == 1)
    stopifnot(n <= length(vec))
    stopifnot(n > 0)
    if (!log) {
        diffs = abs(vec - x)
    } else {
        diffs = abs(log(vec/x))
    }
    return(order(diffs)[1:n])
}

picknotna = function(a, b) {
    stopifnot(length(a) == length(b))
    b_ix = !is.na(b)
    a[b_ix] = b[b_ix]
    return(a)
}

merge_na = function(...) {
    l = list(...)
    nnas = Reduce('+', lapply(l, function(x) !is.na(x)))
    stopifnot(max(nnas) < 2)
    return(Reduce(picknotna, l))
}

ix_interpolate = function(ixs, vec) {
    stopifnot(all(ixs >= 1 & ixs <= length(vec)))
    pos = 1:length(vec)
    res = NA*ixs
    for (i in 1:length(ixs)) {
        a = sort(which.closest(ixs[i], pos, n=2))
        res[i] = ((ixs[i] - a[1])*vec[a[2]] + (a[2] - ixs[i])*vec[a[1]])/(a[2] - a[1])
    }
    return(res)
}
