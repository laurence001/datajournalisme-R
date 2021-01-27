library(highcharter)

themer <- hc_theme(
  colors = c('#FCB156','#D7393E','#6C085E','#E85541', '#FB8D46', '#F95035', '#E01F26','#AF082B'),
  chart = list(
    backgroundColor = '#FFFFFF'
  ),
  loading = list (
    hideDuration = 1000,
    showDuration = 1000
  ),
  title = list(
    style = list(
      color = '#000000',
      fontFamily = "Roboto",
      fontWeight = "bold",
      fontSize= "22px"
    )
  ),
  subtitle = list(
    style = list(
      color = '#000000',
      fontFamily = "Roboto",
      fontSize = "19px"
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = 'Open Sans',
      color = 'black',
      fontSize = '14px'
    ),
    itemHoverStyle = list(
      color = 'gray'
    ),
    plotOptions = list(
      dataLabels = list(
        style = list (
          fontSize = '16px',
          fontFamily = 'Open Sans',
          fontWeight = 'normal',
          textShadow = FALSE,
          textOutline = FALSE
        )
      )
    ),
    yAxis = list(
      dataLabel = list(
        fontSize = '19px'
      ),
      labels = list(
        fontSize = '16px'
      )
    ),
    hc_xAxis = list(
      fontSize = '16px',
      gridLineWidth = 1
    )
  )
)
