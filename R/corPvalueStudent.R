corPvalueStudent <-
function (cor, nSamples)
{
    T = sqrt(nSamples - 2) * cor/sqrt(1 - cor^2)
    2 * pt(abs(T), nSamples - 2, lower.tail = FALSE)
}
