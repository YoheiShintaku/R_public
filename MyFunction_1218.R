# generate random walk + noise data
gen_random_walk <- function(N, x_ini, v_var, w_var){
    set.seed(0)
    # ----------------------
    v_sd <- sqrt(v_var)
    w_sd <- sqrt(w_var)
    #
    x <- double(N)
    x[1] <- x_ini
    #
    y <- double(N)
    #
    v <- rnorm(N, mean=0, sd=v_sd)
    w <- rnorm(N, mean=0, sd=w_sd)
    for (i in 2:N)
    {
        x[i] <- x[i-1] + v[i]
        y[i] <- x[i] + w[i]
    }
    return(list(x=x, y=y))
}

# kalman filter for random walk + noise
kalman_random_walk <- function(y, Vv, Vw, filt_Ex_ini, filt_Vx_ini){
    #
    N <- length(y)
    #
    pred_Ex <- double(N)
    pred_Vx <- double(N)
    filt_Ex <- double(N)
    filt_Vx <- double(N)
    # predict and filter
    filt_Ex[1] <- filt_Ex_ini
    filt_Vx[1] <- filt_Vx_ini
    lnlike = 0
    for (i in 2:N){
        # predict
        pred_Ex[i] = filt_Ex[i-1]
        pred_Vx[i] = filt_Vx[i-1] + Vv
        # filter
        K = pred_Vx[i] / (pred_Vx[i] + Vv)
        filt_Ex[i] = pred_Ex[i] + K * (y[i] - pred_Ex[i])
        filt_Vx[i] = pred_Vx[i] - K * pred_Vx[i]
        # likelihood
        Ey = filt_Ex[i]
        Vy = filt_Vx[i] + Vw
        likelihood = 1 / sqrt(2*pi*Vy) * exp(
            -1 * (y[i] - Ey )^2 / ( 2 * Vy )
        )
        lnlike = lnlike + log(likelihood)
    }
    #
    return( list(
        filt_Ex = filt_Ex,
        lnlike = lnlike * -1
        ))
}
