async function drawBar(){

  //ACCESS data

  const dataset = await d3.csv("protest_per_week_usa.csv")
  const yAccessor = d => +d.count
  const parseTime = d3.timeParse("%Y-%m-%d")
  const xAccessor = d => parseTime(d.week)

  console.log(xAccessor[0])

//Create chart DIMENSIONS

  const width = window.innerWidth*.9

  let dimensions = {
    width: width,
    height: width*.33,
    margin: {
      top: 50,
      right: 10,
      bottom: 50,
      left: 50,
    },
  }

  dimensions.boundedWidth = dimensions.width
    - dimensions.margin.left
    - dimensions.margin.right
  dimensions.boundedHeight = dimensions.height
    -dimensions.margin.top
    -dimensions.margin.bottom

//Draw canvas

  const wrapper = d3.select("#wrapper")
    .append("svg")
      .attr("width", dimensions.width)
      .attr("height", dimensions.height)

  const bounds = wrapper.append("g")
    .style("transform", `translate(${
      dimensions.margin.left
    }px,${
    dimensions.margin.top
  }px)`)

  const xScale = d3.scaleBand()
    .domain(d3.range(dataset.length))
    .range([0, dimensions.boundedWidth])

  const yScale = d3.scaleLinear()
    .domain(d3.extent(dataset, yAccessor))
    .range([0, dimensions.boundedHeight])
    .nice()

//Draw data
  const barPadding = 1

  const mouseOver = function(d){
    //reference for mouse position came from here: https://www.d3-graph-gallery.com/graph/interactivity_tooltip.html

    var xPosition = parseFloat(d3.select(this).attr("x")) + xScale.bandwidth()*.25 + 70;
    var yPosition = d3.mouse(this)[1] - 35;

    d3.select("#tooltip").classed("hidden", false)

    d3.select("#tooltip")
      .style("left", xPosition + "px")
      .style("top", yPosition + "px")
      .select("#value")
      .text(yAccessor(d))

  }

  const mouseMove = function(d){
    var xPosition = parseFloat(d3.select(this).attr("x")) + xScale.bandwidth()*.25 + 70;
    var yPosition = d3.mouse(this)[1];

    d3.select("#tooltip")
    .style("left", xPosition + "px")
    .style("top", yPosition + "px")

  }

  const mouseOut = function(d){
    d3.select("#tooltip")
      .classed("hidden", true)
  }

  const bar = bounds.selectAll("rect")
    .data(dataset)
  .enter()
    .append("rect")
    .attr("x", function(d, i){
      return xScale(i);
    })
    .attr("y", d => dimensions.boundedHeight - yScale(yAccessor(d)))
    .attr("width", xScale.bandwidth() - barPadding)
    .attr("height", d => yScale(yAccessor(d)))
    .on("mouseover", mouseOver)
    .on("mousemove", mouseMove)
    .on("mouseout", mouseOut)


//Add peripherals
  const xAxisScale = d3.scaleTime()
      .domain(d3.extent(dataset, xAccessor))
      .range([0, dimensions.boundedWidth])

    const xAxisGenerator = d3.axisBottom()
        .scale(xAxisScale)

   const xAxis = bounds.append("g")
        .call(xAxisGenerator)
        .style("transform", `translateY(${dimensions.boundedHeight}px)`)


  const yAxisScale = d3.scaleLinear()
    .domain(d3.extent(dataset, yAccessor))
    .range([dimensions.boundedHeight, 0])
    .nice()

  const yAxisGenerator = d3.axisLeft()
    .scale(yAxisScale)

  const yAxis = bounds.append("g")
    .call(yAxisGenerator)

  const yAxisLabel = yAxis.append("text")
    .attr("y", 0)
    .attr("x", dimensions.margin.right)
    .attr("fill", "black")
    .style("font-size", "1.4em")
    .text("Protests per week")
    .style("text-anchor", "start")

    ////////// slider, based on Jane Pong: https://bl.ocks.org/officeofjane/47d2b0bfeecfcb41d2212d06d095c763 //////////


}


drawBar()
