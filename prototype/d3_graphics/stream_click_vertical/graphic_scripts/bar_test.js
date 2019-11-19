async function drawBar(){

  let topic = "women"
  let topicID = "#"+topic+"-stream"

  console.log(topicID)

  const dataset = await d3.csv("month_top_topics_long.csv")

  console.log(dataset)

  const section = dataset.filter(function(d) {return d.topic == topic})

  console.log(section)

  const xAccessor = d => +d.value
  const parseTime = d3.timeParse("%Y-%")
  const yAccessor = d => parseTime(d.month_yr)

  const height = d3.max([window.innerHeight*.6, 800])
  const width = d3.min([window.innerWidth, 800])

  const dimensions = {
      width: width,
      height: height,
      margin: {
        top: 50,
        bottom: 0,
        left: 10,
        right: 10
      },
      bar:{
        margin: {
          right: 100
        }
      }
    }

      dimensions.boundedHeight = dimensions.height
       - dimensions.margin.top - dimensions.margin.bottom
      dimensions.boundedWidth = dimensions.width
       -  dimensions.margin.left - dimensions.margin.right


    console.log(dimensions)

    const wrapper = d3.select("#streamgraph")

    const barWrapper = wrapper.append("svg")
     .attr("width", dimensions.width)
     .attr("height", dimensions.boundedHeight)
     .attr("id", "barWrapper")

     const streamBounds = barWrapper.append("g")
       .attr("id", "streamBounds")
       .style("transform", `translate(${
           dimensions.margin.left
         }px, ${
           dimensions.margin.top
         }px)`)

    const barBounds = barWrapper.append("g")
      .attr("id", "barBounds")
      .style("transform", `translate(${
          dimensions.margin.left
        }px, ${
          dimensions.margin.top
        }px)`)

  const yBarScale = d3.scaleBand()
    .domain(d3.range(section.length))
    .range([0,dimensions.boundedHeight])


  const xBarScale = d3.scaleLinear()
    .domain(d3.extent(section, xAccessor))
    .range([0,dimensions.boundedHeight - dimensions.bar.margin.right])
    .nice()

    console.log(d3.extent(section, xAccessor))


    const stream = await d3.xml('img/vertical_streamgraph.svg')
       .then(data => {
      streamBounds.node().append(data.documentElement)
      })


    d3.select("#annotations")
      .classed("hidden", true)

    d3.select("#grid")
      .style("opacity", ".5")
      //.classed("hidden", true)

    let topicSelect = "#stream path:not("+topicID+")"

    d3.selectAll(topicSelect)
      .style("fill", "white")
      .style("stroke", "white")

    d3.selectAll("#stream path")
      .style("opacity", ".2")

    //draw dataset

    const barPadding = 1

    const bar = barBounds.selectAll("rect")
      .data(section)
      .enter()
        .append("rect")
        .attr("y", function(d, i){
          return yBarScale(i);
        })
        .attr("x", 0)
        .attr("width", d => xBarScale(xAccessor(d)))
        .attr("height", yBarScale.bandwidth() - barPadding)

    // const yAxisScale = d3.scaleTime()
    //   .domain(d3.extent(section, yAccessor))
    //   .range([0, dimensions.boundedHeight])
    //
    // const yAxisGenerator = d3.axisLeft()
    //   .scale(yBarScale)
    //
    // const yAxis = barBounds.append("g")
    //   .call(yAxisGenerator)
    //   .attr("id", "stream-bar-y-axis")

    const xAxisGenerator = d3.axisTop()
      .scale(xBarScale)

    const xAxis = barBounds.append("g")
      .call(xAxisGenerator)
      .attr("color", "white")

    const xAxisLabel = barWrapper.append("text")
      .attr("y", 15)
      .attr("x", dimensions.margin.right)
      .attr("fill", "white")
      .text("Protests per month")
      .style("font-size", "1rem")

}

drawBar()
