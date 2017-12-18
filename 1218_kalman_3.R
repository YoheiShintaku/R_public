# ======================================================
# ランダムウォーク＋ノイズ。データ生成とカルマンフィルタ。
# 最尤推定 -> パラメーターが全く合わない。何かおかしい。
# ======================================================

# generate random walk + noise data ===========================
source( "C:/Users/yohei.shintaku/Documents/MyFunction_1218.R" )
ret = gen_random_walk(
    N=300,
    x_ini=0, 
    v_var=2, 
    w_var=10
)
x = ret$x
y = ret$y
# plot
ylim = c(-10,10)
plot(y, type="l", col='black', ylim = ylim, ylab="")
par(new=T)
plot(x, type="l", col='blue', ylim = ylim, ylab="")


# 最尤法によるパラメーター推定 ================================
# 目的関数：パラメーターを受け取って尤度を返す関数
mostlikely <- function(para) {
    # kalman filter with para
    ret = kalman_random_walk(
        y = y,
        Vv = para[1], 
        Vw = para[2],
        filt_Ex_ini = y[1],
        filt_Vx_ini = 5
    )
    # return inverse of log likelihood
    return(ret$lnlike)
}
# 最適値探索
est = optim(
    c(5, 5),
    mostlikely,
    NULL,
    method="BFGS",
    hessian=TRUE,
    control=list(trace=1,REPORT=1)
)
#mostlikely(c(2, 10))
# kalman filter ===============================================
ret = kalman_random_walk(
    y = y,
    Vv = est$par[1], 
    Vw = est$par[2],
    filt_Ex_ini = y[1],
    filt_Vx_ini = 5
)
filt_Ex = ret$filt_Ex
lnlike = ret$lnlike

# plot estimated state -----------
plot(y, type="l", col='black', ylim = ylim, ylab="")
par(new=T)
plot(filt_Ex, type="l", col='blue', ylim = ylim, ylab="")
#par(new=T)
#plot(x, type="l", col='blue', ylim = ylim, ylab="")
