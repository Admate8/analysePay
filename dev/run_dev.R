# Sass code compilation
sass::sass(input = sass::sass_file("inst/app/www/custom_try.sass"), output = "inst/app/www/custom_try.css", cache = NULL)

# Sass code compilation
sass::sass(input = sass::sass_file("inst/app/www/custom_try.sass"), output = "inst/app/www/custom_try.css", cache = NULL)

options(golem.app.prod = FALSE)
options(shiny.port = httpuv::randomPort())
golem::detach_all_attached()
golem::document_and_reload()
run_app()
