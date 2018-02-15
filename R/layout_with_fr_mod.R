layout_with_fr_mod <-
function (graph, coords = NULL, dim = 2, niter = 500, start.temp = sqrt(vcount(graph)),
grid = c("auto", "grid", "nogrid"), weights = NULL, minx = NULL,
maxx = NULL, miny = NULL, maxy = NULL, minz = NULL, maxz = NULL,
coolexp, maxdelta, area, repulserad, maxiter)
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(coords)) {
        coords <- as.matrix(structure(as.double(coords), dim = dim(coords)))
    }
    dim <- as.integer(dim)
    if (dim != 2L && dim != 3L) {
        stop("Dimension must be two or three")
    }
    if (!missing(niter) && !missing(maxiter)) {
        stop("Both `niter' and `maxiter' are given, give only one of them")
    }
    if (!missing(maxiter))
    niter <- maxiter
    niter <- as.integer(niter)
    start.temp <- as.numeric(start.temp)
    grid <- "nogrid" #igraph.match.arg(grid)
    grid <- switch(grid, grid = 0L, nogrid = 1L, auto = 2L)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    if (!is.null(minx))
    minx <- as.numeric(minx)
    if (!is.null(maxx))
    maxx <- as.numeric(maxx)
    if (!is.null(miny))
    miny <- as.numeric(miny)
    if (!is.null(maxy))
    maxy <- as.numeric(maxy)
    if (!is.null(minz))
    minz <- as.numeric(minz)
    if (!is.null(maxz))
    maxz <- as.numeric(maxz)
    if (!missing(coolexp)) {
        warning("Argument `coolexp' is deprecated and has no effect")
    }
    if (!missing(maxdelta)) {
        warning("Argument `maxdelta' is deprecated and has no effect")
    }
    if (!missing(area)) {
        warning("Argument `area' is deprecated and has no effect")
    }
    if (!missing(repulserad)) {
        warning("Argument `repulserad' is deprecated and has no effect")
    }
    on.exit(.Call(C_R_igraph_finalizer))
    if (dim == 2) {
        res <- .Call(C_R_igraph_layout_fruchterman_reingold,
        graph, coords, niter, start.temp, weights, minx,
        maxx, miny, maxy, grid)
    }
    else {
        res <- .Call(C_R_igraph_layout_fruchterman_reingold_3d,
        graph, coords, niter, start.temp, weights, minx,
        maxx, miny, maxy, minz, maxz)
    }
    res
}
