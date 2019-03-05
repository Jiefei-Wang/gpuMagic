# This is a temporary replacement of the hash package
#' @include tools.R
hash <- function() {
    x = new.env()
    x$data = list()
    x = structure(x, class = "hash")
    x
}



is.hash <- function(x) {
    is(x,"hash")
}

keys <- function(x) {
    names(x$data)
}

has.key <- function(key, x) {
    !is.null(x$data[[key]])
}

values <- function(x) {
    x$data
}

del <- function(key, x) {
    key = toCharacter(key)
    x$data[[key]] = NULL
    
}

clear <- function(x) {
    x$data = list()
}

"[[.hash" <- function(x, key) {
    key = toCharacter(key)
    x$data[[key]]
    # x[[key]] NextMethod(x,key)
}



"[[<-.hash" <- function(x, key, value) {
    key = toCharacter(key)
    x$data[[key]] = value
    x
}
"[.hash" <- function(x, key) {
    key = toCharacter(key)
    x$data[[key]]
}

"[<-.hash" <- function(x, key, value) {
    key = toCharacter(key)
    x$data[[key]] = value
    x
}

as.list.hash <- function(x) {
    x$data
}
print.hash <- function(x) {
    cat("Hash object:\n")
    print(ls.str(x$data))
}
copy <- function(x) {
    y = hash()
    y$data = x$data
    y
}
