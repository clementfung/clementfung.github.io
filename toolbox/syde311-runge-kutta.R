# edit this function
doMath <- function(t,w) {

    ans <- 2 * (1 - w) / (t ^ 2 * sin(w));

    return(ans);

}

doRKStep <- function(t,w,h) {

    m1 <- doMath(t, w);
    m2 <- doMath(t + 0.5 * h, w + 0.5 * h * m1);
    m3 <- doMath(t + 0.5 * h, w + 0.5 * h * m2);
    m4 <- doMath(t + h, w + h * m3);

    wn <- w + h / 6 * (m1 + (2 * m2) + (2 * m3) + m4);
    return(wn);

}
