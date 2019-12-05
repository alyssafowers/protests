async function streamEverythingFunction(){
  var whichTopic = document.querySelector("#stream-legend")

    const width = d3.min([window.innerWidth, 800])
    const height = width*.9

    const dimensions = {
      width: width,
      height: height,
      bar: {
        height: width*.5,
        margin: {
          top: 30,
          bottom: 30,
          left: 60,
          right: 30
        }
      }
    }

  dimensions.bar.boundedHeight = dimensions.bar.height
      - dimensions.bar.margin.top - dimensions.bar.margin.bottom
     dimensions.bar.boundedWidth = dimensions.width
      - dimensions.bar.margin.left - dimensions.bar.margin.right

    const barWrapper = d3.select("#streamgraph")
      .append("svg")
      .attr("id", "bar-wrapper")
      .attr("width", dimensions.width)
      .attr("height", dimensions.bar.height)


    const barYAxisLabel = barWrapper.append("text")
      .text("Protests per month")
      .style("transform", `translate(${
          dimensions.bar.margin.left
        }px,1.5rem)`)
      .attr("fill", "white")
      .attr("font-size", ".8rem")

  const barBounds = barWrapper.append("g")
    .attr("id", "bar-bounds")
    .style("transform", `translate(${
        dimensions.bar.margin.left
      }px, ${
        dimensions.bar.margin.top
      }px)`)

  const fillColors = {
    supreme_court: "rgb(157, 106, 118)",
    women: "rgb(5, 69, 81)",
    police: "rgb(86, 25, 74)",
    education: "rgb(3, 3, 4)",
    executive: "rgb(243, 227, 211)",
    immigration: "rgb(97, 135, 150)",
    guns: "rgb(20, 67, 127)",
    race_confed: "rgb(124, 105, 164)",
    collective_bargaining: "rgb(15, 41, 90)",
    environment: "rgb(25, 101, 91)",
    healthcare: "rgb(73, 49, 118)",
    other: "rgb(113, 118, 137)"
  }

  const textColors = {
    supreme_court: "white",
    women: "white",
    police: "white",
    education: "white",
    executive: "black",
    immigration: "white",
    guns: "white",
    race_confed: "white",
    collective_bargaining: "white",
    environment: "white",
    healthcare: "white",
    other: "white"
  }

  const dataset_bar = await d3.csv("./month_top_topics_long.csv")

      const parseTime = d3.timeParse("%Y-%m")
      const formatDate = d3.timeFormat("%m/%y")
      const yAccessor = d => +d.value
      const xAccessor = d => parseTime(d.month_yr)

      const startDate=d3.min(dataset_bar, xAccessor)
      const endDate=d3.max(dataset_bar, xAccessor)

      const minValue = d3.min(dataset_bar, yAccessor)
      const maxValue = d3.max(dataset_bar, yAccessor)

      const barPadding = 5

      const xBarScale = d3.scaleTime()
        .domain([startDate, endDate])
        .range([0, dimensions.bar.boundedWidth])

      const xAxisGenerator = d3.axisBottom()
        .scale(xBarScale)

      const xAxis = barBounds.append("g")
        .call(xAxisGenerator)
        .style("transform", `translateY(${dimensions.bar.boundedHeight}px)`)
        .attr("id", "bar-x-axis")

      const monthCount = 34

      var barWidth = (dimensions.bar.boundedWidth / monthCount) - (barPadding)

      focus = "supreme_court"

      var section = dataset_bar.filter(function(d){ return d.topic == focus})

      const yBarScale = d3.scaleLinear()
            .domain(d3.extent(section, yAccessor))
            .range([0, dimensions.bar.boundedHeight])
            .nice()

      const yAxisScale = d3.scaleLinear()
            .domain(d3.extent(section, yAccessor))
            .range([dimensions.bar.boundedHeight, 0])
            .nice()

        const yAxisGenerator = d3.axisLeft()
              .scale(yAxisScale)
              .ticks(5)

        const yAxis = barBounds.append("g")
          .call(yAxisGenerator)
          .attr("id", "bar-y-axis")
          .attr("transform", "translate(-10,0)")

    const bar = barBounds.selectAll("rect")
      .data(section)

    bar.enter()
      .append("rect")
      .attr("x", d =>  xBarScale(xAccessor(d)) - barWidth/2)
      .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
      .attr("width", barWidth)
      .attr("height", d => yBarScale(yAccessor(d)))
      .attr("class", focus)
      .attr("fill", fillColors[focus])

      function drawBarTopic(focus, label){

        section = dataset_bar.filter(function(d){ return d.topic == focus})

        d3.select("#stream-bar-topic")
          // .transition()
          // .duration(1000)
          .style("opacity", "0")

        const barUpdate = d3.selectAll("#bar-bounds")

        barUpdate.selectAll("rect")
          .data(section)
          .transition()
          .duration(1000)
            .attr("height", d => yBarScale(yAccessor(d)))
            .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
            .style("fill", fillColors[focus])
        d3.select("#stream-bar-topic")
          .text(label)
          .transition()
          .duration(1000)
            .style("opacity", "1")
            .style("background-color", fillColors[focus])
            .style("color", textColors[focus])

        setTimeout(function(){
          const scaleUpdate = d3.selectAll("#bar-bounds").transition().duration(1000)

          yAxisScale.domain(d3.extent(section, yAccessor)).nice()

          yBarScale.domain(d3.extent(section, yAccessor)).nice()

          scaleUpdate.selectAll("rect")
            .attr("height", d => yBarScale(yAccessor(d)))
            .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))

            scaleUpdate.selectAll("#bar-y-axis")
              .call(yAxisGenerator)

        },1000)
      }

      whichTopic.addEventListener("click", function(){
        event.preventDefault()
        stream = event.target.id
        console.log(event.target)
        label = event.target.attributes.label.nodeValue
            let focusEnd = stream.length - 7
            let focus = stream.substring(0, focusEnd)
            drawBarTopic(focus, label)

      }, false)

}

streamEverythingFunction()
