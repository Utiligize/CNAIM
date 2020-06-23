/*

This code is based on following convention:

https://github.com/bumbeishvili/d3-coding-conventions/blob/84b538fa99e43647d0d4717247d7b650cb9049eb/README.md




*/


function Chart() {
	// Exposed variables
	var attrs = {
		id: 'ID' + Math.floor(Math.random() * 1000000), // Id for event handlings
		svgWidth: 400,
		svgHeight: 400,
		marginTop: 35,
		marginBottom: 5,
		marginRight: 5,
		marginLeft: 50,
		container: 'body',
		defaultTextFill: '#2C3E50',
		defaultFont: 'Helvetica',
		prefix: { x: 'HI', y: 'C' },
		axisColor: '#b3b3b3',
		label: { font: 'Arial', color: '#000000', fontSize: 17 },
		tickMark: { font: 'Verdana', fontSize: 25 },
		axisHeaders: { verticalDistance: 20, horizontalDistance: 25 },
		legend: {
			height: 20,
			width: 100,
			rectVerticalDistance: 35,
			textVerticalDistance: 25,
			fontSize: 20,
			//records: [{ name: 'Lav', color: '#63be7b' }, { name: 'Mellem', color: '#ffe984' }, { name: 'Høj', color: '#f8696b' }]
			records: [{ name: 'Low', color: '#63be7b' }, { name: 'Medium', color: '#ffe984' }, { name: 'High', color: '#f8696b' }]
		},
		points: {
			color: 'black', radius: 3,
			data: [] //not using anymore
		},
		data: null
	};

	//InnerFunctions which will update visuals
	var updateData;

	//Main chart object
	var main = function () {
		//Drawing containers
		var container = d3.select(attrs.container);

		//Calculated properties
		var calc = {};
		calc.id = 'ID' + Math.floor(Math.random() * 1000000); // id for event handlings
		calc.chartLeftMargin = attrs.marginLeft;
		calc.chartTopMargin = attrs.marginTop;
		calc.chartWidth = attrs.svgWidth - attrs.marginRight - calc.chartLeftMargin;
		calc.chartHeight = attrs.svgHeight - attrs.marginBottom - calc.chartTopMargin;

		d3.selectAll('#custom-sized-matrix-tooltip, #custom-sized-matrix').remove();

		// Define the div for the tooltip
		var tooltip = d3.select('body').append('div')
			.attr('class', 'square-matrix-tooltip')
			.attr('id', 'custom-sized-matrix-tooltip')
			.style('opacity', 0);

		//Add svg
		var svg = container
			.patternify({ tag: 'svg', selector: 'svg-chart-container' })
			.attr('id', 'custom-sized-matrix')
			.attr('width', attrs.svgWidth)
			.attr('height', attrs.svgHeight)
			.attr('font-family', attrs.defaultFont)
			.attr('overflow', 'visible');

		//Add container g element
		var chart = svg
			.patternify({ tag: 'g', selector: 'chart' })
			.attr('transform', 'translate(' + calc.chartLeftMargin + ',' + calc.chartTopMargin + ')');

		var valueRange = [1, d3.max(attrs.data.map(d => +d.x)) + d3.max(attrs.data.map(d => +d.y))];

		//set colors based on risk level
		var colorScale = d3.scaleLinear()
			.domain([valueRange[0], d3.mean(valueRange), valueRange[1]])
			.range(attrs.legend.records.map(x => x.color));

    const data0 = attrs.data.filter(c=>c.x!='na' || c.y !='na')

		//group data by x values, vertical columns
		//var groupedByX = d3.nest().key(d => d.x).entries(attrs.data);
				var groupedByX = d3.nest().key(d => d.x).entries(data0);

		//group data by y values, horizontal rows
	  //var groupedByY = d3.nest().key(d => d.y).entries(attrs.data);
				var groupedByY = d3.nest().key(d => d.y).entries(data0);

		var xAxisGroup = chart.patternify({ tag: 'g', selector: 'x-axis-container' });
		var yAxisGroup = chart.patternify({ tag: 'g', selector: 'y-axis-container' });
		var matrixGroup = chart.patternify({ tag: 'g', selector: 'matrix-group' });
		var legendsGroup = chart.patternify({ tag: 'g', selector: 'legends-group' });

		calc.defaultRectWidth = attrs.svgWidth / (groupedByX.length + 3);
		calc.defaultRectHeight = attrs.svgHeight / (groupedByY.length + 4);

		attrs.data.forEach(function (d) {
			d.width = calc.defaultRectWidth;
			d.height = calc.defaultRectHeight;
		});

		addAxes();
		addAxisHeaders();
		addMatrixRects();
		addSums();
		addLegends();
		addPointCircles(attrs, calc);

		//Functions

		function addPointCircles(attrs, calc) {
			var pointsContainer = chart.patternify({ tag: 'g', selector: 'points-container' });
      //console.log(JSON.stringify(attrs.data))
      
      const data1 = attrs.data.filter(c=>c.point_x!='na' || c.point_y !='na')
    
     // console.table
			var pointCircles = pointsContainer
				.patternify({ tag: 'circle', selector: 'point-circles', data: data1 })
			  .attr('r', data1[0]['dot_radius'])
				.attr('fill', attrs.points.color)
				.attr('cx', d => getPosition(calc.defaultRectWidth, groupedByY, d.point_x))
				.attr('cy', d => getPosition(calc.defaultRectHeight, groupedByX, d.point_y));
		}

		function getPosition(initialMargin, groupedBlocks, currentPercentage) {
			var chartSize = initialMargin * groupedBlocks[0].values.length;
			var x = chartSize * currentPercentage / 100;
			x += initialMargin;

			return x;
		}

		function addAxisHeaders() {
			var xAxisHeader = xAxisGroup
				.patternify({ tag: 'text', selector: 'x-axis-header' })
				.attr('font-size', attrs.legend.fontSize)
				.attr('font-family', attrs.label.font)
				.attr('text-anchor', 'middle')
				.attr("dominant-baseline", "middle")
				//.text('Helbredsindikator')
				.text('Health indicator')
				.attr('x', function (d) {
					var xAxisWidth = xAxisGroup.node().getBoundingClientRect().width;
					var x = xAxisWidth / 2;

					return x;
				})
				.attr('y', -attrs.axisHeaders.verticalDistance);

			var yAxisHeader = yAxisGroup
				.patternify({ tag: 'text', selector: 'y-axis-header' })
				.attr('font-size', attrs.legend.fontSize)
				.attr('font-family', attrs.label.font)
				.attr('text-anchor', 'middle')
				.attr("dominant-baseline", "middle")
				//.text('Konsekvenser ved fejl')
				.text('Consequences of failure')
				.attr('transform', 'rotate(270)')
				.attr('x', function (d) {
					var yAxisHeight = yAxisGroup.node().getBoundingClientRect().height;
					return -yAxisHeight / 2.5;
				})
				.attr('y', -attrs.axisHeaders.horizontalDistance);
		}

		function addLegends() {
			var legendRects = legendsGroup
				.patternify({ tag: 'rect', selector: 'legend-rect', data: attrs.legend.records })
				.attr('width', attrs.legend.width)
				.attr('height', attrs.legend.height)
				.attr('x', (x, i) => (i * attrs.legend.width))
				.attr('fill', d => d.color);

			var legendTexts = legendsGroup
				.patternify({ tag: 'text', selector: 'legend-text', data: attrs.legend.records })
				.attr('font-size', attrs.legend.fontSize)
				.attr('font-family', attrs.label.font)
				.attr('text-anchor', 'middle')
				.attr("dominant-baseline", "middle")
				.text(d => d.name)
				.attr('x', (x, i) => (i * attrs.legend.width) + attrs.legend.width / 2)
				.attr('y', attrs.legend.height + attrs.legend.textVerticalDistance);

			legendsGroup.attr('transform', function (d) {
				var chartWidth = matrixGroup.node().getBoundingClientRect().width + calc.defaultRectWidth;
				var legendWidth = this.getBoundingClientRect().width;
				var x = (chartWidth - legendWidth) / 2;
				var y = matrixGroup.node().getBoundingClientRect().height + calc.defaultRectHeight + attrs.legend.rectVerticalDistance;
				return 'translate(' + x + ',' + y + ')';
			});
		}

		function addSums() {
			groupedByY.forEach(function (column, index) {
				var sum = d3.sum(column.values.map(x => x.value)).toFixed(1);
				var rowFirstBlock = groupedByY[index].values[0];

				matrixGroup.append('text')
					.classed('tick-mark', true)
					.attr('font-size', attrs.label.fontSize - 2)
					.attr('font-family', attrs.label.font)
					.attr('dy', 1.5)
					.attr('x', () => calc.defaultRectWidth + (groupedByY[0].values.length * calc.defaultRectWidth) + calc.defaultRectWidth / 2)
					.attr('y', rowFirstBlock.yPosition + rowFirstBlock.height / 2)
					.attr('text-anchor', 'middle')
					.attr("dominant-baseline", "middle")
					.text(sum);
			});

			groupedByX.forEach(function (row, index) {
				var columnFirstBlock = groupedByX[index].values[0];

				matrixGroup.append('text')
					.classed('tick-mark', true)
					.attr('font-size', attrs.label.fontSize - 2)
					.attr('font-family', attrs.label.font)
					.attr('dy', 1.5)
					.attr('x', columnFirstBlock.xPosition + columnFirstBlock.width / 2)
					.attr('y', () => calc.defaultRectHeight + (groupedByX[0].values.length * calc.defaultRectHeight) + calc.defaultRectHeight / 2)
					.attr('text-anchor', 'middle')
					.attr("dominant-baseline", "middle")
					.text(d3.sum(row.values.map(x => x.value)).toFixed(1));
			});
		}

		function addMatrixRects() {
			groupedByY.forEach(function (row, index) {
				row.values.forEach(function (record, i) {
				 // console.log(JSON.stringify(groupedByY))

					// add x axis rects
					var matrixRect = matrixGroup
						.append('rect')
						.classed('matrix-rect', true)
						.attr('width', record.width)
						.attr('height', record.height)
						.attr('x', record.xPosition)
						.attr('y', record.yPosition)
						.attr('fill', colorScale(+record.x + +record.y))
						.attr('data-color', colorScale(+record.x + +record.y))
						.attr('stroke', '#fff')
						.attr('stroke-width', '1px')
						.attr('opacity', '1')
						.attr('cursor', 'pointer')
						.on('mouseenter', function () {
							d3.select(this).attr('opacity', '0.6');

							tooltip.transition().duration(200).style("opacity", 1);

							var text = record.mouse_over_text.split('\n').join(' <br/>');

							tooltip.html(text).style("left", (d3.event.pageX - 140) + "px").style("top", (d3.event.pageY - 40) + "px").style("position", "absolute")
								.style("text-align", "center")
								.style("width", "200px")
								.style("height", "30px")
								.style("padding", "8px")
								.style("font", "12px sans-serif")
								.style("background", "white")
								.style("border", "0px")
								.style("border-radius", "8px")
								.style("pointer-events", "none")
								.style("fill", "#333333");
						})
						.on('mouseleave', function (x) {
							d3.select(this).attr('opacity', '1');

							tooltip.transition().duration(500).style("opacity", 0);
						});
				});
			});
		}

		function addAxes() {
			addAxisRects();

			addAxisTexts()
		}

		function addAxisRects() {
			// add x axis rects
			var xAxisRects = xAxisGroup
				.patternify({ tag: 'rect', selector: 'x-axis-rect', data: groupedByX })
				.attr('width', function (d) {
					return d.values[0].width;
				})
				.attr('height', function (d) {
					return calc.defaultRectWidth;
				})
				.attr('x', function (d, i) {
					var x;

					if (i === 0) x = calc.defaultRectWidth;
					else {
						var previousColumn = groupedByX[i - 1];
						var previousColumnX = previousColumn.values[0].xPosition;
						var previousColumnWidth = previousColumn.values[0].width;

						x = previousColumnX + previousColumnWidth;
					}

					var currentColumn = groupedByX[i];
					currentColumn.values.forEach(block => block.xPosition = x);

					return x;
				})
				.attr('y', 0)
				.attr('fill', attrs.axisColor)
				.attr('stroke', '#fff')
				.attr('stroke-width', '1px');

			// add y axis rects
			var yAxisRects = yAxisGroup
				.patternify({ tag: 'rect', selector: 'y-axis-rect', data: groupedByY })
				.attr('width', function (d) {
					return calc.defaultRectWidth;
				})
				.attr('height', function (d) {
					return d.values[0].height;
				})
				.attr('x', 0)
				.attr('y', function (d, i) {
					var y;

					if (i === 0) y = calc.defaultRectHeight;
					else {
						var previousRow = groupedByY[i - 1];
						var previousRowY = previousRow.values[0].yPosition;
						var previousRowHeight = previousRow.values[0].height;

						y = previousRowY + previousRowHeight;
					}

					var currentRow = groupedByY[i];
					currentRow.values.forEach(block => block.yPosition = y);

					return y;
				})
				.attr('fill', attrs.axisColor)
				.attr('stroke', '#fff')
				.attr('stroke-width', '1px');
		}

		function addAxisTexts() {
			//var xAxisLabels = ['M DKK'].concat(groupedByX.map(d => 'HI' + d.key)).concat('Sum');
			var prepend = 'M EUR';
			var append = 'Sum';

			var xAxisLabels = [prepend].concat(groupedByX.map(d => 'HI' + d.key)).concat(append);
			var yAxisLabels = groupedByY.map(d => 'C' + d.key).concat(append);

			// add x axis texts
			var xAxisTexts = xAxisGroup
				.patternify({ tag: 'text', selector: 'x-axis-text', data: xAxisLabels })
				.attr('font-size', attrs.label.fontSize)
				.attr('font-family', attrs.label.font)
				.attr('fill', attrs.label.color)
				.attr('text-anchor', 'middle')
				.attr("dominant-baseline", "middle")
				.text(d => d)
				.attr('x', function (d, i) {
					var x;

					if (d === prepend) {
						var nextColumnBlock = groupedByX[i].values[0];
						x = nextColumnBlock.xPosition - calc.defaultRectWidth / 2;
					}
					else if (d === append) {
						var previousColumnBlock = groupedByX[i - 2].values[0];
						x = previousColumnBlock.xPosition + previousColumnBlock.width + calc.defaultRectWidth / 2;
					}
					else {
						var currentColumnBlock = groupedByX[i - 1].values[0];
						x = currentColumnBlock.xPosition + currentColumnBlock.width / 2;
					}

					return x;
				})
				.attr('y', calc.defaultRectHeight / 2);

			// add y axis texts
			var yAxisTexts = yAxisGroup
				.patternify({ tag: 'text', selector: 'y-axis-text', data: yAxisLabels })
				.attr('font-size', attrs.label.fontSize)
				.attr('font-family', attrs.label.font)
				.attr('fill', attrs.label.color)
				.attr('text-anchor', 'middle')
				.attr("dominant-baseline", "middle")
				.text(d => d)
				.attr('x', calc.defaultRectWidth / 2)
				.attr('y', function (d, i) {
					var y;

					if (d === append) {
						var previousRowBlock = groupedByY[i - 1].values[0];
						y = previousRowBlock.yPosition + previousRowBlock.height + calc.defaultRectHeight / 2;
					}
					else {
						var currentRowBlock = groupedByY[i].values[0];
						y = currentRowBlock.yPosition + currentRowBlock.height / 2;
					}

					return y;
				});
		}

		// Smoothly handle data updating
		updateData = function () { };

		//#########################################  UTIL FUNCS ##################################

	};

	//----------- PROTOTYPE FUNCTIONS  ----------------------
	d3.selection.prototype.patternify = function (params) {
		var container = this;
		var selector = params.selector;
		var elementTag = params.tag;
		var data = params.data || [selector];

		// Pattern in action
		var selection = container.selectAll('.' + selector).data(data, (d, i) => {
			if (typeof d === 'object') {
				if (d.id) {
					return d.id;
				}
			}
			return i;
		});
		selection.exit().remove();
		selection = selection.enter().append(elementTag).merge(selection);
		selection.attr('class', selector);
		return selection;
	};

	//Dynamic keys functions
	Object.keys(attrs).forEach((key) => {
		// Attach variables to main function
		return (main[key] = function (_) {
			var string = `attrs['${key}'] = _`;
			if (!arguments.length) {
				return eval(` attrs['${key}'];`);
			}
			eval(string);
			return main;
		});
	});

	//Set attrs as property
	main.attrs = attrs;

	//Exposed update functions
	main.data = function (value) {
		if (!arguments.length) return attrs.data;
		attrs.data = value;
		if (typeof updateData === 'function') {
			updateData();
		}
		return main;
	};

	// Run  visual
	main.render = function () {
		main();
		return main;
	};

	return main;
}
