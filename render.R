file_name <- 'st558-651-project-1-James-Carr.Rmd'
out_name <- 'readme.md'
out_format <- 'github_document'
rmarkdown::render(input = file_name,
                  output_format = out_format,
                  output_file = out_name)

