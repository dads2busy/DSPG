library(dashboardthemes)
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "black"
  ,primaryFontColor = "black"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "white"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(35,45,75)"
  
  ,headerButtonBackColor = "rgb(35,45,75)"
  ,headerButtonIconColor = "white"
  ,headerButtonBackColorHover = "white"
  ,headerButtonIconColorHover = "rgb(35,45,75)"
  
  ,headerBackColor = "rgb(35,45,75)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  # ,sidebarBackColor = cssGradientThreeColors(
  #   direction = "down"
  #   ,colorStart = "rgb(20,97,117)"
  #   ,colorMiddle = "rgb(56,161,187)"
  #   ,colorEnd = "rgb(3,22,56)"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 50
  #   ,colorEndPos = 100
  # )
  ,sidebarBackColor = "white"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "grey"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(white)" #*
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "black" #*
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = "rgb(235, 95, 12)"
  #   cssGradientThreeColors(
  #   direction = "right"
  #   ,colorStart = "rgba(44,222,235,1)"
  #   ,colorMiddle = "rgba(44,222,235,1)"
  #   ,colorEnd = "rgba(0,255,213,1)"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 30
  #   ,colorEndPos = 100
  # )
  ,sidebarTabTextColorSelected = "white"
  ,sidebarTabRadiusSelected = "0px 0px 0px 0px"
  
  ,sidebarTabBackColorHover = "rgb(235, 95, 12)" #*
  #   cssGradientThreeColors(
  #   direction = "right"
  #   ,colorStart = "rgba(44,222,235,1)"
  #   ,colorMiddle = "rgba(44,222,235,1)"
  #   ,colorEnd = "rgba(0,255,213,1)"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 30
  #   ,colorEndPos = 100
  # )
  ,sidebarTabTextColorHover = "white"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "white" #*
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 0px 0px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 21
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(35, 45, 75)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "white"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "white" #*
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "grey"
  ,buttonTextColorHover = "grey"
  ,buttonBorderColorHover = "grey"
  
  ,textboxBackColor = "white"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)
