var chart = Chart()
  .svgHeight(600)
    .svgWidth(800)
  //.svgWidth(window.innerWidth)
  .container('#d3-chart1')
  //.container(container)
  .data(data)
  .render();

//document.getElementById("htmlwidget_container").style.position = "relative";