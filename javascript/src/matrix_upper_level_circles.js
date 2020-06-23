var chart2 = Chart()
  .svgHeight(600)
  .svgWidth(window.innerWidth)
  //.container('#matrix-container')
  .data(data)
  .render();

document.getElementById("htmlwidget_container").style.position = "relative";