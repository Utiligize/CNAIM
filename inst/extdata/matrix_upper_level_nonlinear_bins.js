var chart = Chart3()
  .svgHeight(600)
 // .svgWidth(window.innerWidth)
    .svgWidth(800)
  .container('#d3-chart3')
  .data(data)
  .render();

//document.getElementById("htmlwidget_container").style.position = "relative";