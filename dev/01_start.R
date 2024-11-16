golem::fill_desc(
  pkg_name = "analysePay",
  pkg_title = "analysePay",
  pkg_description = "Simulate policies impact on your wages.",
  author_first_name = "Adrian",
  author_last_name = "Wisnios",
  author_email = "adrian9.wisnios@gmail.com",
  repo_url = "https://github.com/Admate8/analysePay",
  pkg_version = "0.0.0.9000"
)
golem::set_golem_options()
golem::install_dev_deps()
usethis::use_mit_license("MIT")
usethis::use_readme_rmd(open = FALSE)
devtools::build_readme()
usethis::use_code_of_conduct(contact = "adrian9.wisnios@gmail.com")
usethis::use_lifecycle_badge("Experimental")
# usethis::use_news_md(open = FALSE)
# usethis::use_git()
# golem::use_recommended_tests()
golem::use_favicon()
# golem::use_utils_ui(with_test = TRUE)
# golem::use_utils_server(with_test = TRUE)
rstudioapi::navigateToFile("dev/02_dev.R")
