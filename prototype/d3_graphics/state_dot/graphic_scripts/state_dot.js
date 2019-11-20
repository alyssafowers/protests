//not sure if axis is where it needs to be

async function stateDot(){

  const dataset = await d3.csv("topic_percentile_by_state.csv")
  const xAccessor = d => +d.perc_of_state_protest
  const stateAccessor = d => d.state


  const width = d3.min([window.innerWidth, 800])
  const height = width*1.25

  var topics = ["guns", "immigration", "women", "supreme_court", "other"]

  const dimensions = {
      width: width,
      margin: {
        top: 50,
        bottom: 50,
        left: 100,
        right: 10,
        between: 20
      },
      dotHeight: 10,
      dotMargin: 20
    }

  dimensions.boundedHeight = (dimensions.dotHeight*topics.length)+(dimensions.dotMargin*(topics.length+1))

  dimensions.height = dimensions.boundedHeight+dimensions.margin.top+dimensions.margin.bottom

  dimensions.boundedWidth = dimensions.width
   -  dimensions.margin.left - dimensions.margin.right


   console.log(dimensions)

   const wrapper = d3.select("#state-dotplot")

   const dotWrapper = wrapper.append("svg")
    .attr("width", dimensions.width)
    .attr("height", dimensions.height)
    .attr("id", "dotWrapper")

    const dotBounds = dotWrapper.append("g")
      .attr("id", "dotBounds")
      .style("transform", `translate(${
          dimensions.margin.left
        }px, ${
          dimensions.margin.top
        }px)`)

        const labelBounds = dotWrapper.append("g")
          .attr("id", "labelBounds")
          .style("transform", `translate(${
              0
            }px, ${
              dimensions.margin.top
            }px)`)


        const xScale = d3.scaleLinear()
          .domain(d3.extent(dataset, xAccessor))
          .range([0,dimensions.boundedWidth])
          .nice()

          var formatPercent = d3.format(".0%")

          const xAxisGenerator = d3.axisTop()
              .scale(xScale)
              .tickFormat(formatPercent)

         const xAxis = dotWrapper.append("g")
              .call(xAxisGenerator)
              .style("transform", `translate(${
                  dimensions.margin.left
                }px, ${
                  dimensions.margin.top/2
                }px)`)


    async function drawDots(topicName){

      const topicBounds = dotBounds.append("g")
        .attr("id", topicName)
        // .style("transform", `0px, ${
        //     yPosition
        //   }px)`)


      const section = dataset.filter(function(d) {return d.topic == topicName})

      const mouseOver = function(d){
        //reference for mouse position came from here: https://www.d3-graph-gallery.com/graph/interactivity_tooltip.html

        var xPosition = parseFloat(d3.select(this).attr("cx"))+dimensions.margin.left*2
        var yMousePosition = d3.mouse(this)[1]+dimensions.margin.top-dimensions.dotHeight

        d3.select("#tooltip").classed("hidden", false)

        d3.select("#tooltip")
          .style("left", xPosition + "px")
          .style("top", yMousePosition + "px")
          .select("#state-percentile")
          .text(formatPercent(xAccessor(d)))
          .select("#tooltip-state")
          .text(stateAccessor(d))

      }

      const mouseMove = function(d){
        var xPosition = parseFloat(d3.select(this).attr("cx"))+dimensions.margin.left*2
        var yPosition = d3.mouse(this)[1]+dimensions.margin.top-dimensions.dotHeight

        d3.select("#tooltip")
        .style("left", xPosition + "px")
        .style("top", yPosition + "px")
        .select("#tooltip-state")
        .text(stateAccessor(d))


      }

      const mouseOut = function(d){
        d3.select("#tooltip")
          .classed("hidden", true)
      }

      const guideLine = topicBounds.append("line")
      .attr("x1", 0)
      .attr("x2", dimensions.boundedWidth)
        .attr("y1", yPosition)
        .attr("y2", yPosition)
        .style("stroke", "lightgray")
        .style("stroke-width", ".5")


        //to put guidelines all the way to beginning of labels:

        // const guideLine = labelBounds.append("line")
        //   .attr("x1", 0)
        //   .attr("x2", xScale(d3.max(dataset, d => d.perc_of_state_protest))+dimensions.margin.left)
        //   .attr("y1", yPosition)
        //   .attr("y2", yPosition)
        //   .style("stroke", "lightgray")
        //   .style("stroke-width", ".5")

        //draw dataset

        const dots = topicBounds.selectAll("circle")
          .data(section)
            .enter()
              .append("circle")
              .attr("cx", d=> xScale(xAccessor(d)))
              .attr("cy", yPosition)
              .attr("r", dimensions.dotHeight/2)
              .attr("fill", "black")
              .attr("opacity", ".3")
              .on("mouseover", mouseOver)
              .on("mousemove", mouseMove)
              .on("mouseout", mouseOut)


        //add label

        const lineLabel = labelBounds.append("text")
            .text(topicName)
            .attr("fill", "black")
            .attr("x", dimensions.margin.left - 10)
            .attr("y", yPosition+dimensions.dotMargin/4)
            .attr("text-anchor", "end")

        //got a little help from https://stackoverflow.com/questions/56226717/return-value-based-on-max-min-of-another-value-in-the-same-object
        //if this doesn't work later, remember to include this in the index file: <script src="https://d3js.org/d3-array.v2.min.js"></script>

        var highPerc = d3.max(section, d => d.perc_of_state_protest)
        var highPercIndex = d3.maxIndex(section, d => d.perc_of_state_protest)
        var highPercState = section[highPercIndex].state

      //console.log(highPerc, highPercIndex, highPercState)

    //  console.log(xScale(highPerc))
      const highestState = topicBounds.append("text")
        .text(highPercState)
        .attr("fill", "black")
        .attr("x", xScale(highPerc)+dimensions.dotHeight)
        .attr("y", yPosition)


      }

    var yPosition

  for(i = 0; i < topics.length; i++){
      yPosition = 2 + (i*2*dimensions.dotMargin)
      drawDots(topics[i])
    }

    }






stateDot()
