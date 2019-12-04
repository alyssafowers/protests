//this technically mostly works right now, but goddamn is it buggy.
//issues with layering in clickable legend, lots of things got
//weird modified names from Illustrator that I had to go in manually
//to change, etc. Check these things before despairing over code
//in final version.

//also, had to put streamgraph on top of bar chart to make it clickable,
//but now it's also in front when the bar charts are doing their thing
//and that's no good.

async function streamClick(){

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
    }
  }

  const wrapper = d3.select("#streamgraph")

  const bounds = wrapper.append("g")
    .attr("id", "stream-bounds")
    .style("transform", `translate(${
        dimensions.margin.left
      }px, ${
        dimensions.margin.top*.25
      }px)`)

  const stream = await d3.xml('img/vertical_streamgraph_for_site_redo.svg')
      .then(data => {
          bounds.node().append(data.documentElement)
        })

  // const barCharts = d3.selectAll("#bar-charts")

  const barCharts = d3.selectAll("#bar-charts g").attr("opacity", "0")

  console.log(barCharts)

  async function drawBar(topicName){
    console.log("read drawBar")
    let topicStream = "#"+topicName+"-stream"
    let topicBar = "#"+topicName+"-bar"
    let topicBarGroups = "#"+topicName+"-bar g"

    let streamNotSelected = "#stream path:not("+topicStream+")"
    let barNotSelected = "#bar-charts path:not("+topicBar+")"

    console.log(topicStream+" "+topicBar)

    d3.selectAll("#annotations")
      .attr("opacity", "0")

    d3.selectAll("#stream-grid")
      .attr("opacity", "0")

      d3.selectAll(".bar-grid")
        .attr("opacity", "1")

    d3.select(topicStream)
      .attr("opacity", ".3")
      .lower()

    d3.selectAll(streamNotSelected)
      .attr("opacity", "0")

    d3.selectAll(topicBar)
      .attr("opacity", "1")
      .raise()

      d3.selectAll(topicBarGroups)
        .attr("opacity", "1")

    d3.selectAll("#legend")
      .attr("display", "none")

    d3.selectAll("#title")
      .attr("opacity", "0")

    d3.select("#return")
      .attr("opacity", "1")

    console.log(topicBar)
  }

  var topic

    function streamClick(){
      console.log("read streamClick")
      let topicID = "#"+this.id
      let topicEnd = topicID.length -7

       topic = topicID.substring(1, topicEnd)

      drawBar(topic)
      console.log(topic)

    }

    function reDraw(){
      console.log("read reDraw")
      barCharts.attr("opacity", "0")

      d3.selectAll("#stream path")
        .attr("opacity", "1")
        .raise()

      d3.selectAll("#annotations")
        .attr("opacity", "1")
        .raise()

      d3.selectAll("#legend")
        .attr("display", "inline")

        d3.selectAll("#title")
          .attr("opacity", "1")

      d3.selectAll("#stream-grid")
        .attr("opacity", "1")

        d3.selectAll(".bar-grid")
          .attr("opacity", "0")
    }

    function legendClick(legendName){

      //going to need to muck around with the structure of the legend section so that I'm selecting the groups with legend blocks within them, instead of just the rectangles

      let legendClass = "."+this.id
      let legendEnd = legendClass.length-7
      topic = legendClass.substring(1, legendEnd)

      console.log(legendClass)
      console.log(topic)

      drawBar(topic)

    }

    d3.selectAll("#stream path").on("click", streamClick)
    d3.selectAll("#return").on("click", reDraw)
    d3.selectAll("#legend rect").on("click", legendClick)

}

streamClick()
