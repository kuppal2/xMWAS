<<<<<<< HEAD
corPvalueStudent <-
function (cor, nSamples)
{
    T = sqrt(nSamples - 2) * cor/sqrt(1 - cor^2)
    2 * pt(abs(T), nSamples - 2, lower.tail = FALSE)
}
=======
corPvalueStudent <-
function (cor, nSamples)
{
    T = sqrt(nSamples - 2) * cor/sqrt(1 - cor^2)
    2 * pt(abs(T), nSamples - 2, lower.tail = FALSE)
}
>>>>>>> d85ff5fd429b8ce2c4d44411a09b32765ce92b65
