var chart = Chart2()
  .svgHeight(600)
 // .svgWidth(window.innerWidth)
    .svgWidth(800)
  .container('#d3-chart2')
  .data(data)
  .render();

//document.getElementById("htmlwidget_container").style.position = "relative";