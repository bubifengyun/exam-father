#!/usr/bin/env R 
# _render.R 

args = commandArgs(TRUE) 
papertype = args[1] 
paperid = args[2] 
quiet = FALSE 
formats = 'bookdown::pdf_book' 
cat(sprintf("paperid = '%s'",paperid), file = "_paperid.R") 

src = (function() { 
    attr(body(sys.function()), 'srcfile') })()$filename
if (is.null(src) || src == '') src = '.' 
owd = setwd(dirname(src)) 


# render the book to all formats unless they are specified via command-line args 
for (fmt in formats) { 
    cmd = sprintf("bookdown::render_book(
        'index.Rmd', '%s', quiet = %s, output_options = list(template = 'latex/%s-template.tex'))", 
        fmt, quiet, papertype) 
    res = bookdown:::Rscript(c('-e', shQuote(cmd))) 
    if (res != 0) stop('Failed to compile the book to ', fmt) 
} 

setwd(owd)
