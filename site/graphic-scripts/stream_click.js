//on click, fade out other parts of the streamgraph, and have the clicked part
//drop down to become an area chart?


// Things are working okay to click the gun stream and have the
// bar chart show up, but can't get the Y axis to appear for
// some reason (REFUSES to translate to the right, no matter
// what I do with barBounds and .style("translate")). margin
// information is reading into the objects created by the
// axis, they just have negative x's for some reason???

//this is still ALL fucked up--can't get stuff to align, would prefer to have
//bar chart invisible until someone clicks on a stream, somehow illegibly
//small??? argh! Maybe just use vertical version everywhere?


async function streamClick(){

  //creating overall dimensions

  const width = d3.min([window.innerWidth*.9, 800])

  const dimensions = {
    stream: {
      width: width,
      height: width*.7,
      margin: {
        top: 10,
        bottom: 30,
        left: 10,
        right: 10
      }
    },
    bar: {
      width: width,
      height: width*.5,
      margin: {
        top: 20,
        bottom: 30,
        left: 50,
        right: 30
        }
      },
      total:{
        width: width
      }
    }


   dimensions.stream.boundedHeight = dimensions.stream.height
    - dimensions.stream.margin.top - dimensions.stream.margin.bottom
   dimensions.stream.boundedWidth = dimensions.stream.width
    -  dimensions.stream.margin.left - dimensions.stream.margin.right

   dimensions.bar.boundedHeight = dimensions.bar.height
    - dimensions.bar.margin.top - dimensions.bar.margin.bottom
   dimensions.bar.boundedWidth = dimensions.bar.width
    - dimensions.bar.margin.left - dimensions.bar.margin.right
   dimensions.bar.boundedTop = dimensions.stream.height + dimensions.bar.margin.top

   dimensions.total.height = dimensions.bar.height + dimensions.stream.height

  const wrapper = d3.select("#streamgraph")

 const streamWrapper = wrapper.append("svg")
  .attr("width", dimensions.stream.width)
  .attr("height", dimensions.stream.height)
  .attr("id", "streamWrapper")
  .style("margin-left", (window.innerWidth - dimensions.width)/2)
  .style("margin-right", (window.innerWidth - dimensions.width)/2)

   const streamBounds = streamWrapper.append("g")
       // .style("transform", `translate(${
       //   dimensions.stream.margin.left
       // })px, ${
       //   dimensions.stream.margin.top
       // })px`)
       .attr("id", "streamBounds")

  const barWrapper = wrapper.append("svg")
    .attr("width", dimensions.bar.width)
    .attr("height", dimensions.bar.height)
    .attr("id", "barWrapper")
    .attr("y", dimensions.stream.height)
    .classed("hidden", true)

  const barBounds = barWrapper.append("g")
    .attr("id", "barBounds")

//Bringing in the streamgraph SVG

   const stream = await d3.xml('img/vertical-streamgraph-for-site.svg')
      .then(data => {
     streamBounds.node().append(data.documentElement)
     })

//Function to draw bar chart

  async function drawBar(){
  //access dataset

  const dataset = await d3.csv("protest_per_week_usa.csv")
  const yAccessor = d => +d.count
  const parseTime = d3.timeParse("%Y-%m-%d")
  const xAccessor = d => parseTime(d.week)

  //canvas is already drawn and margins already set above

  //draw scales

  const xScale = d3.scaleBand()
    .domain(d3.range(dataset.length))
    .range([0, dimensions.bar.boundedWidth])

  const yScale = d3.scaleLinear()
      .domain(d3.extent(dataset, yAccessor))
      .range([0, dimensions.bar.boundedHeight])
      .nice()

  //draw data

//  console.log(dataset)
  const barPadding = 1

  const bar = barBounds.selectAll("rect")
    .data(dataset)
  .enter()
    .append("rect")
    .attr("x", function(d, i){
      return xScale(i);
    })
    .attr("y", d => dimensions.bar.boundedHeight - yScale(yAccessor(d)))
    .attr("width", xScale.bandwidth() - barPadding)
    .attr("height", d => yScale(yAccessor(d)))


        //draw peripherals

        const xAxisScale = d3.scaleTime()
            .domain(d3.extent(dataset, xAccessor))
            .range([0, dimensions.bar.boundedWidth])

          const xAxisGenerator = d3.axisBottom()
              .scale(xAxisScale)

         const xAxis = barBounds.append("g")
              .call(xAxisGenerator)
              .style("transform", `translateY(${dimensions.bar.boundedHeight}px)`)

        const yAxisScale = d3.scaleLinear()
          .domain(d3.extent(dataset, yAccessor))
          .range([dimensions.total.height, dimensions.stream.height])
          .nice()

        const yAxisGenerator = d3.axisLeft()
          .scale(yAxisScale)

        const yAxis = barWrapper.append("svg")
          .call(yAxisGenerator)

        const yAxisLabel = yAxis.append("text")
          .attr("y", dimensions.stream.height)
          .attr("x", dimensions.bar.margin.left)
          .attr("fill", "black")
          .style("font-size", "1.4em")
          .text("Protests per week")
          .style("text-anchor", "start")


      console.log(yAxis[0])

//  console.log(dataset)

  }


     const clickStream = function(d){
       d3.select("path#gun-stream")
         .style("display", "none")

      const dot = barBounds.append("circle")
        .attr("cx", 13)
        .attr("cy", 15)
        .attr("r", "10")
        .attr("fill", "orange")

       }


       d3.select("path#gun-stream")
         .on("click", drawBar)


  }


streamClick()
