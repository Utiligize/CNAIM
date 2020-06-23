var chart2 = Chart3()
  .svgHeight(600)
  .svgWidth(window.innerWidth)
  //.container('#matrix-container')
  .data(data)
  .render();

document.getElementById("htmlwidget_container").style.position = "relative";