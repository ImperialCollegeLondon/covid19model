filenames <- list.files("web/figures/", recursive = TRUE, full.names=TRUE)

for( f in filenames ){
  
  x <- readLines(f)
  y <- gsub( "Aerial", "Arial, Helvetica, sans-serif", x )
  cat(y, file=f, sep="\n")
  
}