
shinyUI(
    fluidPage(
        theme = shinytheme("cyborg"),
        # Add Javascript ----
        tags$head(
            tags$link(rel="stylesheet", type="text/css",href="style.css"),
            tags$script(type="text/javascript", src = "md5.js"),
            tags$script(type="text/javascript", src = "passwdInputBinding.js"),
            tags$style(
                HTML(
                    '.skin-blue .main-header .navbar {
                      background-color: #04121a;
        }
        
        .skin-blue .main-header .logo:hover {
                      background-color: #01070a;
        }

        .skin-blue .main-header .logo {
                      background-color: #020f17;
                                 color: #fff;
                         border-bottom: 0 solid transparent;
        }

        .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
        background-color: #000a0e;
        }

        .progress-bar {
            float: left;
                width: 0;
                height: 80%;
                font-size: 8px;
                line-height: 15px;
                color: black;
                text-align: center;
                background-color: #d5d1d0;
                -webkit-box-shadow: inset 0 -1px 0 rgba(0,0,0,.15);
                box-shadow: inset 0 -1px 0 rgba(0,0,0,.15);
                -webkit-transition: width .6s ease;
                -o-transition: width .6s ease;
                transition: width .6s ease;
        }

              .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #dd4b39;
              }

        .box-header .box-title, .box-header>.fa, .box-header>.glyphicon, .box-header>.ion {
            display: inline-block;
                font-size: 22px;
                margin: 0;
                line-height: 1;
                color: black;
        }

          .box-body {
              border-top-left-radius: 0;
                  border-top-right-radius: 0;
                  border-bottom-right-radius: 3px;
                  border-bottom-left-radius: 3px;
                  padding: 10px;
                  background-color: black;
                  color: white;
          }

            .shiny-output-error-validation {
                color: #F44336;
                    font-size: 15px;
            }

          b, strong {
              font-weight: 700;
                  color: #F44336;
          }

          body {
              font-family: "Source Sans Pro",Calibri,Candara,Arial,sans-serif;
              background-color: white;
          }
          

          .input-group-btn:first-child>.btn, .input-group-btn:first-child>.btn-group {
              margin-right: -1px;
              background-color: black;
              color: azure;
              border-color: darkgrey;
          }
          
          h2, .h2 {
              font-size: 45px;
              color: black; 
          }

        '
                )
            )
        ),
        useShinyjs(),
        
        titlePanel("Algoritma Capstone Machine Learning", windowTitle = "Algoritma Capstone Machine Learning"),
        br(),
        br(),
        
        # UI output ----
        
        uiOutput("app")
    )
)
