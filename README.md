# `{submitr}` : A package for logging learnr tutorial events

This is in early development, so regard it mainly as a demo.

See `inst/tutorials/Testing/Testing.Rmd` for a working example. You can get this most easily from the  GitHub repo. (Otherwise, you'll have  to use `system.file("tutorials/Testing/Testing.Rmd")`.)

You can login with any of the  passwords contained in the [password Google Sheet](https://docs.google.com/spreadsheets/d/1WvQDD1sQHcc_aZ1eisWkZvhU4a3jEAR2HXt_1REojpM/edit?usp=sharing).

From within the tutorial, you can download the submissions by setting the user  ID to  "instructor" with  the password "isostats".  If you want to monitor the updating  of the submissions, look at the [submission storage Google Sheet](https://docs.google.com/spreadsheets/d/14z7FM64GUq8nc6Q4zvJKH5ynEY2lBttiIIqAxmEgojA/edit?usp=sharing)

**Notices**:

- If you write your own tutorial, keep in  mind that access to Google  Sheets requires a login token. There is such a token in the  `inst/tutorials/Testing/Testing.Rmd` directory. You can write  your own `{submitr}` tutorials to  access the same Google Sheet storage by putting them  in the same `inst/tutorials/Testing/` directory. Alternatively, follow the instructions in  the vignette and set up  your own Google Sheets for storage, creating a new token for them.

- Installing `submitr` should install  the current GitHub version of `googlesheets4`. If not, `remotes::install_github("tidyverse/googlesheets4")`

- When you "Run document"  with `Testing.Rmd`, it's best to clear the pre-rendered output cache (which you can do from the dropdown  menu beside "Run document" or simply by removing the `Testing.html` file that sits  alongside `Testing.Rmd`). I do not understand why this  is necessarily. When I fail to do this, updates to the `submitr` package -- especially the `submitr_login_script.Rmd`  -- aren't reflected in the running app.

- Much of the action behind `{submitr}` is triggered by  the `submitr_login_script.Rmd` which is included in a tutorial document using  a `{knitr}` "child" chunk. In particular, `submitr_login_script.Rmd` is where the Shiny login widgets are created and where the Shiny  reactives live that make the widgets operational.

Status Apr 2, 2020:  `Testing.Rmd` runs well from within an RStudio session. It is not yet working when deployed to ShinyApps.io. An earlier version (<dtkaplan.shinyapps.io/markr-demo>) does work. I am not sure what is going wrong now.

