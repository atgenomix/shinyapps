# shinyapps
Shiny apps on Atgenomix SeqsLab

This package is the collection of Shiny apps on Atgenomix SeqsLab

### Development Guide:
1. Start by using [Shiny modules](https://mastering-shiny.org/scaling-modules.html)
as the basic components. Then, assemble them into a Shiny app.   
Each Shiny app is an exported function of this package.  

* `app_xxx.R`: A Shiny app
* `mod_xxx.R`: The Shiny modules

2. Code style follows [Tidyverse style guide](https://style.tidyverse.org/).
