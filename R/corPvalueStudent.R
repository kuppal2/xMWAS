corPvalueStudent <-
function(cor, nSamples)
{
    #calculate p-value based on correlation and number of samples
    T = sqrt(nSamples - 2) * cor/sqrt(1 - cor^2)
    
    return(2 * pt(abs(T), nSamples - 2, lower.tail = FALSE))
}
