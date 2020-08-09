tagList(
  # shinythemes::themeSelector(),
  navbarPage(
    theme = "superhero",  # <--- To use a theme, uncomment this
    "An Analysis Of NYC Public School SAT Scores",

dashboardPage(
  dashboardHeader(title = 'NYDOE'),
  dashboardSidebar(
    sidebarUserPanel('',image = 'https://patch.com/img/cdn20/users/22845980/20170712/111640/styles/raw/public/processed_images/nyc_dept_of_edu-1499871891-7952.jpg'),
    sidebarMenu(
      menuItem('Overview',tabName = 'map',icon = icon('map')),
      menuItem('Analysis', tabName = 'analysis', icon = icon('book')),
      menuItem('Selector', tabName = 'graphing', icon = icon('chart-bar')),
      menuItem('Public School Data',tabName = 'data',icon = icon('database')),
      menuItem('Charter School Data',tabName = 'data_charter',icon = icon('coins')),
      menuItem('About Me', tabName = 'me', icon = icon('address-card'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'map',
              tabsetPanel(type = 'tabs',
                          tabPanel(
                            'Schools',
                            fluidRow(
                              box(leafletOutput("map"),
                                  p(),
                                  width = 12,
                                  height = '200%')
                            ),
                            fluidRow(
                              box(plotlyOutput('plot3'), width = 12)
                            ),
                            fluidRow(
                              box(plotOutput('plot4'), width = 12)
                            )
                          )
              )
              ),
        tabItem(tabName = 'analysis',
                tabsetPanel(type = 'tabs',
                    tabPanel(
                      'By School',
                      fluidRow(
                        box(plotlyOutput('plot1'), width = 12)
                      ),
                      fluidRow(
                        box(plotlyOutput('plot2'), width = 12)
                        )
                      ),
                    tabPanel(
                      'Charter School Openings',
                      fluidRow(
                        box(plotlyOutput('plot6'), width = 12)
                      ),
                      fluidRow(
                        box(plotlyOutput('plot7'), width = 12)
                      )
                    ),
                    tabPanel(
                      'Private Enrollment',
                      fluidRow(
                        box(plotOutput('plot5'), width = 12)
                      ),
                      fluidRow(
                        box(plotlyOutput('plot8'), width = 12)
                      ),
                      fluidRow(
                        box(plotlyOutput('plot9'), width = 12)
                      )
                    )
                  )
              ),
      tabItem(tabName = 'graphing',
              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("minBox"),
                       infoBoxOutput("avgBox")),
                            fluidRow(
                              column(9,
                                     htmlOutput('hist'),height = 900
                                    
                              ),
                              column(3,
                                     selectizeInput('selected',
                                                    'Select Iterm to Display',
                                                    choices=colnames(scores)[c(12, 17, 18, 19)]
                              )
                            )
                          )
              # )
              # )
      ),
      tabItem(tabName = 'data',
              fluidRow(
                box(dataTableOutput('table'),width=12)
              )
              ),
      tabItem(tabName = 'data_charter',
              fluidRow(
                box(dataTableOutput('table2'),width=12)
              )
      ),
      tabItem(tabName = 'me',
              fluidRow(column(12, ( a("GitHub",  target="_blank", href =  "https://github.com/dbrancusi")))),
              fluidRow(column(12, ( a("LinkedIn",  target="_blank", href =  "https://www.linkedin.com/in/daniel-brancusi-0560a75/"))))#,
              # fluidRow(
              #   box(plotOutput('image'), width = 12)
              # )

      )
    )
      
  )
)


)
)
