//to make the text scale in the annotations: width/800 * (however big the text is by default)

async function streamEverythingFunction(){
  var whichTopic = document.querySelector("#stream-legend")

  var width

  if(window.innerWidth < 800){
    width = d3.max([375, window.innerWidth*.8]) - 30
  } else {
    width = d3.min([800, window.innerWidth*.8]) - 30
  }

  const height = d3.max([width*.5, 400])

    const dimensions = {
      width: width,
      height: height,
      bar: {
        height: height,
        margin: {
          top: 50,
          bottom: 35,
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
      .text("Protests")
      .style("transform", `translate(${
          dimensions.bar.margin.left
        }px,1rem)`)
      .attr("fill", "white")
      .attr("font-size", ".8rem")
      .style("text-anchor", "end")


      barWrapper.append("text")
      .text("per month")
      .style("transform", `translate(${
          dimensions.bar.margin.left
        }px,2rem)`)
      .attr("fill", "white")
      .attr("font-size", ".8rem")
      .style("text-anchor", "end")



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

      // const xAxis = barBounds.append("g")
      //   .call(xAxisGenerator)
      //   .style("transform", `translateY(${dimensions.bar.boundedHeight}px)`)
      //   .attr("id", "bar-x-axis")

        //custom X axis to match styling elsewhere
        var parsePreciseTime = d3.timeParse("%Y-%m-%d")

      barBounds.append("line")
          .attr("x1", xBarScale(parsePreciseTime("2017-12-16")))
          .attr("x2", xBarScale(parsePreciseTime("2017-12-16")))
          .attr("y1", dimensions.bar.height)
          .attr("y2", dimensions.bar.boundedHeight)
          .attr("stroke", "white")

        barBounds.append("line")
        .attr("x1", xBarScale(parsePreciseTime("2018-12-16")))
        .attr("x2", xBarScale(parsePreciseTime("2018-12-16")))
          .attr("y1", dimensions.bar.height)
          .attr("y2", dimensions.bar.boundedHeight)
          .attr("stroke", "white")

          barBounds.append("line")
          .attr("x1", 0)
          .attr("x2", dimensions.bar.boundedWidth)
            .attr("y1", dimensions.bar.boundedHeight)
            .attr("y2", dimensions.bar.boundedHeight)
            .attr("stroke", "white")

        barBounds.append("text")
          .text("2017")
          .style("transform", `translate(${
              xBarScale(parsePreciseTime("2017-6-15"))
            }px,${
                dimensions.bar.boundedHeight + 35
              }px)`)
          .style("fill", "white")
          .style("font-size", "1rem")
          .style("text-anchor", "middle")


        barBounds.append("text")
          .text("2018")
          .style("transform", `translate(${
              xBarScale(parsePreciseTime("2018-6-15"))
            }px,${
                dimensions.bar.boundedHeight + 35
              }px)`)
          .style("fill", "white")
          .style("font-size", "1rem")
          .style("text-anchor", "middle")

        barBounds.append("text")
          .text("2019")
          .style("transform", `translate(${
              xBarScale(parsePreciseTime("2019-5-15"))
            }px,${
                dimensions.bar.boundedHeight + 35
              }px)`)
          .style("fill", "white")
          .style("font-size", "1rem")
          .style("text-anchor", "middle")

        var months = [
          {letter: "J",
          space: "2017-1"},
          {letter: "F",
          space: "2017-2"},
          {letter: "M",
          space: "2017-3"},
          {letter: "A",
          space: "2017-4"},
          {letter: "M",
          space: "2017-5"},
          {letter: "J",
          space: "2017-6"},
          {letter: "J",
          space: "2017-7"},
          {letter: "A",
          space: "2017-8"},
          {letter: "S",
          space: "2017-9"},
          {letter: "O",
          space: "2017-10"},
          {letter: "N",
          space: "2017-11"},
          {letter: "D",
          space: "2017-12"},
          {letter: "J",
          space: "2018-1"},
          {letter: "F",
          space: "2018-2"},
          {letter: "M",
          space: "2018-3"},
          {letter: "A",
          space: "2018-4"},
          {letter: "M",
          space: "2018-5"},
          {letter: "J",
          space: "2018-6"},
          {letter: "J",
          space: "2018-7"},
          {letter: "A",
          space: "2018-8"},
          {letter: "S",
          space: "2018-9"},
          {letter: "O",
          space: "2018-10"},
          {letter: "N",
          space: "2018-11"},
          {letter: "D",
          space: "2018-12"},
          {letter: "J",
          space: "2019-1"},
          {letter: "F",
          space: "2019-2"},
          {letter: "M",
          space: "2019-3"},
          {letter: "A",
          space: "2019-4"},
          {letter: "M",
          space: "2019-5"},
          {letter: "J",
          space: "2019-6"},
          {letter: "J",
          space: "2019-7"},
          {letter: "A",
          space: "2019-8"},
          {letter: "S",
          space: "2019-9"},
          {letter: "O",
          space: "2019-10"}
        ]



          const dateLabel = barWrapper.append("g")
            .attr("id","month-label")
            .style("transform", `translate(${
                dimensions.bar.margin.left - 3
              }px,${
                  dimensions.bar.height - 21
                }px)`)

          dateLabel.selectAll("text")
            .data(months)
            .enter()
            .append("text")
            .data(months)
            .text(d => d.letter)
            .attr("x", d => xBarScale(parseTime(d.space)))
            .attr("y", 0)
            .style("fill", "white")
            .style("font-size", ".5rem")



          // const barYAxisLabel = barWrapper.append("text")
          //   .text("Protests")
          //   .style("transform", `translate(${
          //       dimensions.bar.margin.left
          //     }px,1rem)`)
          //   .attr("fill", "white")
          //   .attr("font-size", ".8rem")
          //   .style("text-anchor", "end")


      const monthCount = 34

      var barWidth = (dimensions.bar.boundedWidth / monthCount) - (barPadding)

      focus = "women"

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

      const annotation = await d3.csv("./annotation.csv")

      var section_annotate = annotation.filter(function(d){ return d.topic == focus})
      console.log(section_annotate)

      const anParseTime = d3.timeParse("%m/%d/%y")
      const anDateAccessor = d => anParseTime(d.date)
      const xAdjustAccessor = d => +d.adjustX
      const yAdjustAccessor = d => +d.adjustY

      const annotationBounds = barWrapper.append("g")
        .attr("id", "annotation-bounds")
        .style("transform", `translate(${
            dimensions.bar.margin.left
          }px, ${
            dimensions.bar.margin.top
          }px)`)

      const barAnnotate = annotationBounds.selectAll("text")
        .data(section_annotate)
        .enter()
        .append("text")
        .text(d => d.annotation1)
        .attr("x", d => xBarScale(anDateAccessor(d))+(xAdjustAccessor(d)*barWidth))
        .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAdjustAccessor(d)))
        .attr("fill", d => d.fill)
        .attr("text-anchor", d => d.align)
        .attr("font-size", d => (.7*d.size)+"rem")

      function drawBarTopic(focus, label){

        console.log(focus)

        section = dataset_bar.filter(function(d){ return d.topic == focus})
        section_annotate = annotation.filter(function(d){ return d.topic == focus})

        d3.select("#stream-bar-topic")
          .style("opacity", "0")

        annotationBounds.style("opacity", "0")

        const barUpdate = d3.selectAll("#bar-bounds")

        barUpdate.selectAll("rect")
          .data(section)
          .transition()
          .duration(750)
            .attr("height", d => yBarScale(yAccessor(d)))
            .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
            .style("fill", fillColors[focus])
        d3.select("#stream-bar-topic")
          .text(label)
          .transition()
          .duration(750)
            .style("opacity", "1")
            .style("background-color", fillColors[focus])
            .style("color", textColors[focus])

        const annotationUpdate = d3.selectAll("#annotation-bounds")

      // annotationUpdate
      //   .data(section_annotate)
      //   .exit()
      //   .remove()

        setTimeout(function(){
          const scaleUpdate = d3.selectAll("#bar-bounds").transition().duration(750)

          yAxisScale.domain(d3.extent(section, yAccessor)).nice()

          yBarScale.domain(d3.extent(section, yAccessor)).nice()


          scaleUpdate.selectAll("rect")
            .attr("height", d => yBarScale(yAccessor(d)))
            .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))

            scaleUpdate.selectAll("#bar-y-axis")
              .call(yAxisGenerator)

              annotationUpdate.selectAll("text").remove()


              annotationUpdate.selectAll("text")
                .data(section_annotate)
                .enter()
                .append("text")
                .text(d => d.annotation1)
                .attr("x", d => xBarScale(anDateAccessor(d))+(xAdjustAccessor(d)*barWidth))
                .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAdjustAccessor(d)))
                .attr("fill", d => d.fill)
                .attr("text-anchor", d => d.align)

        },750)

        setTimeout(function(){
          annotationUpdate.transition()
              .duration(150)
              .style("opacity", "1")
        },1500)
      }

      whichTopic.addEventListener("click", function(){
        console.log(event.target.attributes)

        event.preventDefault()
        stream = event.target.id
        label = event.target.attributes.label.nodeValue
            let focusEnd = stream.length - 7
            let focus = stream.substring(0, focusEnd)
            drawBarTopic(focus, label)

      }, false)

}

streamEverythingFunction()
