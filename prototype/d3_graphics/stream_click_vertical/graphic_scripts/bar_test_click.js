//can't get SVG streamgraph and bar chart to have the same dimensions for
//some reason. can I somehow retrieve dimensions from the streamgraph and then
//apply them to the bar chart...?

//also need to have a way to return to the stream graph, change color to match legend,
//have title that says what topic it actually is.

async function streamClick(){

  const dataset = await d3.csv("month_top_topics_long.csv")

  //const height = d3.max([window.innerHeight*.6, 800])

  const width = d3.min([window.innerWidth, 800])
  const height = width*1.25


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
          dimensions.margin.top*.25
        }px)`)

   const barBounds = barWrapper.append("g")
     .attr("id", "barBounds")
     .style("transform", `translate(${
         dimensions.margin.left
       }px, ${
         dimensions.margin.top
       }px)`)

   const stream = await d3.xml('img/vertical_streamgraph.svg')
      .then(data => {
     streamBounds.node().append(data.documentElement)
     })


     d3.selectAll("#immigration-bar, #guns-bar, #return")
       .style("opacity", "0")

      async function drawBar(topicName){

        //let topic = "women"
        let topicID = "#"+topicName+"-stream"
        let topicBar = "#"+topicName+"-bar"

      //  console.log(otherBar)


        const section = dataset.filter(function(d) {return d.topic == topicName})

        const xAccessor = d => +d.value
        const parseTime = d3.timeParse("%Y-%")
        const yAccessor = d => parseTime(d.month_yr)


      const yBarScale = d3.scaleBand()
        .domain(d3.range(section.length))
        .range([0,dimensions.boundedHeight])


      const xBarScale = d3.scaleLinear()
        .domain(d3.extent(section, xAccessor))
        .range([0,dimensions.boundedWidth - dimensions.bar.margin.right])
        .nice()

        d3.select("#annotations")
          .classed("hidden", true)

        d3.select("#grid")
          .style("opacity", ".5")
          //.classed("hidden", true)

        let topicSelect = "#stream path:not("+topicID+")"

        d3.selectAll(topicSelect)
          .style("opacity", "0")

        d3.selectAll(topicID)
          .style("opacity", ".2")

          d3.selectAll(topicBar)
            .style("opacity", "1")

          d3.selectAll("#return")
              .style("opacity", "1")

            console.log(topicBar)

        //draw dataset
        //
        // const barPadding = 1
        //
        // const bar = barBounds.selectAll("rect")
        //   .data(section)
        //   .enter()
        //     .append("rect")
        //     .attr("y", function(d, i){
        //       return yBarScale(i);
        //     })
        //     .attr("x", 0)
        //     .attr("width", d => xBarScale(xAccessor(d)))
        //     .attr("height", yBarScale.bandwidth() - barPadding)
        //
        // // const yAxisScale = d3.scaleTime()
        // //   .domain(d3.extent(section, yAccessor))
        // //   .range([0, dimensions.boundedHeight])
        // //
        // // const yAxisGenerator = d3.axisLeft()
        // //   .scale(yBarScale)
        // //
        // // const yAxis = barBounds.append("g")
        // //   .call(yAxisGenerator)
        // //   .attr("id", "stream-bar-y-axis")
        //
        // const xAxisGenerator = d3.axisTop()
        //   .scale(xBarScale)
        //
        // const xAxis = barBounds.append("g")
        //   .call(xAxisGenerator)
        //   .style("color", "white")
        //
        // const xAxisLabel = barWrapper.append("text")
        //   .attr("y", 15)
        //   .attr("x", dimensions.margin.right)
        //   .attr("fill", "white")
        //   .text("Protests per month")
        //   .style("font-size", "1rem")

    }


    //click function made with a little help from https://www.d3-graph-gallery.com/graph/bubblemap_buttonControl.html

    // function streamClick(){
    //   d3.selectAll("#stream path").each(function(d){
    //     streamPick = d3.select(this)
    //     topic = streamPick.property("id")
    //   })
    //
    //   console.log(topic)
    // }

    function streamClick(){
      let topicID = "#"+ this.id
      //console.log(topicID)

      let topicEnd = topicID.length - 7

      let topic = topicID.substring(1, topicEnd)
      console.log(topic)

      drawBar(topic)



    }

    function reDraw(){
      d3.selectAll("#immigration-bar, #guns-bar")
        .style("opacity", "0")

      d3.selectAll("#stream path, #grid")
        .style("opacity", "1")

      d3.selectAll("#annotations")
        .classed("hidden", false)

    console.log("see you")


    }

    d3.selectAll("#stream path").on("click", streamClick)
    d3.selectAll("#return").on("click", reDraw)


}



streamClick()
