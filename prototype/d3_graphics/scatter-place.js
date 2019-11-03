async function drawScatter() {

//ACCESS DATA
  const dataset = await d3.csv("all_county_cbsa_summary.csv")
  //console.table(dataset[0])
  const xAccessor = d => d.log_pop
  const yAccessor = d => d.protest_per_10k
  const nameAccessor = d=> d.NAME
  const protest_countAccessor = d => d.protest_count
  const populationAccessor = d => d.pop

//CREATE CHART DIMENSIONS

  const width = window.innerWidth * .9

  const height = width*.5

  let dimensions = {
    width: width,
    height: height,
    margin: {
      top: 10,
      right: 10,
      bottom: 50,
      left: 50,
    },
  }

  dimensions.boundedWidth = dimensions.width
    - dimensions.margin.left
    - dimensions.margin.right
  dimensions.boundedHeight = dimensions.height
    - dimensions.margin.top
    - dimensions.margin.bottom

//DRAW CANVAS

  const wrapper = d3.select("#wrapper")
    .append("svg")
      .attr("width", dimensions.width)
      .attr("height", dimensions.height)

  const bounds = wrapper.append("g")
    .style("transform", `translate(${
      dimensions.margin.left
    }px, ${
      dimensions.margin.top
    }px)`)


//CREATE SCALES

  const xScale = d3.scaleLinear()
    .domain(d3.extent(dataset, xAccessor))
    .range([0, dimensions.boundedWidth])
    .nice()

  const yScale = d3.scaleLinear()
  //  .domain(d3.extent(dataset, yAccessor))
    .domain([0, 20])
    .range([dimensions.boundedHeight, 0])
    .nice()

//DRAW DATA

  const dots = bounds.selectAll("circle")
      .data(dataset)
    .enter().append("circle")
      .attr("cx", d => xScale(xAccessor(d)))
      .attr("cy", d => yScale(yAccessor(d)))
      .attr("r", 2)

//ADD PERIPHERALS

  const xAxisGenerator = d3.axisBottom()
    .scale(xScale)

  const xAxis = bounds.append("g")
    .call(xAxisGenerator)
      .style("transform", `translateY(${dimensions.boundedHeight}px)`)

  const xAxisLabel = xAxis.append("text")
    .attr("x", dimensions.boundedWidth/2)
    .attr("y", dimensions.margin.bottom - 10)
    .attr("fill", "black")
    .style("font-size", "1.4em")
    .html("Population (log scale)")


  const yAxisGenerator = d3.axisLeft()
    .scale(yScale)

  const yAxis = bounds.append("g")
    .call(yAxisGenerator)

  const yAxisLabel = yAxis.append("text")
    .attr("x", -dimensions.boundedHeight/2)
    .attr("y", -dimensions.margin.left + 10)
    .attr("fill", "black")
    .style("font-size", "1.4em")
    .html("Protests per 10k people")
    .style("transform", "rotate(-90deg)")
    .style("text-anchor", "middle")

//SET UP INTERACTIONS

dots.on("mouseenter", function(datum, index, nodes){
  console.log({datum, index, nodes})
})

bounds.selectAll("circle")
    .on("mouseenter", onMouseEnter)
    .on("mouseleave", onMouseLeave)

  const tooltip = d3.select("#tooltip")

  function onMouseEnter(datum){
    tooltip.select("#name")
      .text(nameAccessor(datum))
    tooltip.select("#protest_count")
      .text(protest_countAccessor(datum))
    tooltip.select("#residents")
      .text(populationAccessor(datum))
    tooltip.style("opacity", 1)
  }


  function onMouseLeave(){
    tooltip.style("opacity", 0)

  }

}

drawScatter()
