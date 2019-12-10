//something is broken with the time slider, argh

async function mapAndBar(){

////map setup///

  const stateShapes = await d3.json("us-states.json")
  const dataset_map = await d3.csv("constellation_background_condensed.csv")

  const latAccessor = d => +d.longitude
  const longAccessor = d => +d.latitude
  const mapParseTime = d3.timeParse("%Y-%m-%d")
  const mapWeekAccessor = d => mapParseTime(d.week)

//   const width = window.innerWidth * 0.75

  var width

  if(window.innerWidth < 800){
    width = d3.max([375, window.innerWidth*.9])
  } else {
    width = d3.min([800, window.innerWidth*.75])
  }
  console.log(window.innerWidth*.75)

  const dimensions = {
    map: {
      width: width,
      margin: {
        top: 5,
        right: 30,
        bottom: 5,
        left: 30,
      },
    },
    bar: {
      width: width,
      height: 200,
      margin: {
        top: 15,
        bottom: 30,
        left: 30,
        right: 30
        }
      },
      total:{
      },
      slider: {
        height: 70,
        width: width,
        margin: {
          left: 80,
          right: 80,
          top: 20
        }
      }
    }

   dimensions.bar.boundedHeight = dimensions.bar.height
    - dimensions.bar.margin.top - dimensions.bar.margin.bottom
   dimensions.bar.boundedWidth = dimensions.bar.width
    - dimensions.slider.margin.left - dimensions.slider.margin.right

    dimensions.slider.boundedWidth = dimensions.slider.width
      - dimensions.slider.margin.left
      - dimensions.slider.margin.right

  dimensions.map.boundedWidth = dimensions.map.width
    - dimensions.map.margin.left
    - dimensions.map.margin.right

  dimensions.bar.boundedTop = dimensions.map.height + dimensions.bar.margin.top

  dimensions.total.width = width
  dimensions.total.height = dimensions.bar.height + dimensions.slider.height


  const sphere = ({type: "Sphere"})

  const projection = d3.geoAlbersUsa()
    .fitWidth(dimensions.map.boundedWidth, sphere)

  const pathGenerator = d3.geoPath(projection)
  const [[x0, y0], [x1, y1]] = pathGenerator.bounds(sphere)

  dimensions.map.boundedHeight = y1
  dimensions.map.height = dimensions.map.boundedHeight
    + dimensions.map.margin.top
    + dimensions.map.margin.bottom

  dimensions.bar.top = dimensions.slider.height
  dimensions.bar.boundedTop = dimensions.map.height + dimensions.bar.margin.top

  // console.log("dimensions.bar.top = " + dimensions.bar.top + " from dimensions.bar.height " + dimensions.bar.height + " and dimensions.map.height " + dimensions.map.height)


  const mapWrapper = d3.select("#map-wrapper")
    .append("svg")
      .attr("width", dimensions.map.width)
      .attr("height", dimensions.map.height)


  const bounds_map = mapWrapper.append("g")
    .style("transform", `translate(${
      dimensions.map.margin.left
    }px, ${
      dimensions.map.margin.top
    }px)`)
    .attr("id", "map-states-group")

  const bounds_map_background = mapWrapper.append("g")
    .style("transform", `translate(${
      dimensions.map.margin.left
    }px, ${
      dimensions.map.margin.top
    }px)`)
    .attr("id", "map-background-stars")

    const bounds_map_stars = mapWrapper.append("g")
      .style("transform", `translate(${
        dimensions.map.margin.left
      }px, ${
        dimensions.map.margin.top
      }px)`)
      .attr("id", "map-stars-group")

      const states = bounds_map.selectAll(".state")
        .data(stateShapes.features)
        .enter().append("path")
          .attr("class", "state")
          .attr("d", pathGenerator)
          .attr("fill", "black")


    const backgroundStars = bounds_map_background.selectAll("circle")
        .data(dataset_map)
        .enter().append("circle")
        .attr("cx", d => projection([latAccessor(d), longAccessor(d)])[0])
        .attr("cy", d => projection([latAccessor(d), longAccessor(d)])[1])
        .attr("fill", "gray")
        .attr("r", 1)
        .attr("opacity", .1)


    ///bar setup////


    const dataset_bar = await d3.csv("./protest_per_week_usa.csv")
    // console.log(dataset_bar)
    const parseTime = d3.timeParse("%m/%d/%y")
    const formatDate = d3.timeFormat("%m/%d/%y")
    const yAccessor = d => +d.count
    const xAccessor = d => parseTime(d.week)

    const startDate = d3.min(dataset_bar, xAccessor)
    const endDate = d3.max(dataset_bar, xAccessor)

    var playButton = document.querySelector("#play-button")
    var playButtonText = document.querySelector("#play-button-text")
    var barControl = document.querySelector("#bar-control")

    console.log(barControl)
    console.log(barControl.offsetWidth)

    var buttonLeft = (width - dimensions.bar.boundedWidth)/3
    var traceLineLeft = 0

    if(barControl.offsetWidth >= 900){
    playButton.style.left = buttonLeft + "px"
    traceLineLeft = buttonLeft - 2

    console.log(traceLineLeft)
  }


    // playButton.setAttribute("offsetLeft", (width - dimensions.bar.boundedWidth)/2)
    //
    // console.log(playButton.offsetLeft)


  // console.log(startDate)

    var weekCount = Math.round(((endDate - startDate)/86400000)/7)

    const xSliderScale = d3.scaleTime()
      .domain([startDate, endDate])
      .range([0, dimensions.slider.boundedWidth])
      .clamp(true)

    const yBarScale = d3.scaleLinear()
      .domain(d3.extent(dataset_bar, yAccessor))
      .range([0, dimensions.bar.boundedHeight])
      .nice()

    const xBarScale = d3.scaleTime()
        .domain([startDate, endDate])
        .range([0, dimensions.bar.boundedWidth])

    const barWrapper = d3.select("#bar-plot-wrapper")
      .append("svg")
        .attr("width", dimensions.bar.width)
        .attr("height", dimensions.bar.height)

    const fullWrapper = d3.select("#full-bar-wrapper")

    var targetValue = dimensions.slider.boundedWidth

    var sliderTime = d3
          .sliderBottom()
          .min(startDate)
          .max(endDate)
          .step(1000*60*60*24*7)
          .width(dimensions.slider.boundedWidth)
          .ticks(0)
          .tickFormat(d3.timeFormat("Week of %b %d, %Y"))
          .tickValues(xAccessor(dataset_bar))
          .default(startDate)


    var gTime = d3.select("div#slider-time")
          .append("svg")
          .attr("width", dimensions.slider.width)
          .attr("height", dimensions.slider.height)
          .append("g")
          .attr("id", "slider-wrapper")
          .attr("transform", "translate(" + dimensions.slider.margin.left + ","+ dimensions.slider.margin.top+")")

    var pickSlider = gTime.call(sliderTime)

    playButton.className = ""

    const traceLineWrapper = fullWrapper.append("g")
            .attr("transform", "translate(" + (dimensions.slider.margin.left+traceLineLeft) + ","+ dimensions.slider.margin.top+")")

    var handleBounds = d3.select("g.parameter-value")
      .attr("id", "handle-bounds")

    var bounds_bar = barWrapper.append("g")
          .attr("class", "plot")
          .attr("transform", "translate(" + dimensions.slider.margin.left + "," + dimensions.bar.margin.top + ")")
          .attr("id", "main-chart-bounds")

    var bounds_bar_highlighting = barWrapper.append("g")
      .attr("class", "plot")
      .attr("transform", "translate(" + dimensions.slider.margin.left + "," + dimensions.bar.margin.top + ")")
      .attr("id", "highlight-bounds")

  const barPadding = 1

  var weekCount = Math.round(((endDate - startDate)/86400000)/7)
  var barWidth = (dimensions.bar.boundedWidth / weekCount) - (barPadding)

  const mouseOver = function(d){
    //reference for mouse position came from here: https://www.d3-graph-gallery.com/graph/interactivity_tooltip.html

    var xPosition = parseFloat(d3.select(this).attr("x")) + dimensions.bar.margin.left + 40;
    var yPosition = d3.mouse(this)[1] + dimensions.bar.top - 15;

    d3.select("#bar-tooltip").classed("hidden", false)

    d3.select("#bar-tooltip")
      .style("left", xPosition + "px")
      .style("top", yPosition + "px")
      .select("#protest-count")
      .text(yAccessor(d))

    d3.select("#week-bar-tooltip")
        .text(formatDate(xAccessor(d)))


    //  .text(d.week)
  }


  const mouseMove = function(d){
    var xPosition = parseFloat(d3.select(this).attr("x")) + dimensions.bar.margin.left + 40;
//     var yPosition = d3.mouse(this)[1] + dimensions.bar.margin.top + dimensions.bar.top + 125;
     var yPosition = d3.mouse(this)[1] + dimensions.bar.top - 15;


    d3.select("#bar-tooltip")
      .style("left", xPosition + "px")
      .style("top", yPosition + "px")
      .select("#protest-count")
      .text(yAccessor(d))

    d3.select("#week-bar-tooltip")
      .text(d.week)

  }

  const mouseOut = function(d){
    d3.select("#bar-tooltip")
      .classed("hidden", true)
  }

// console.log(dimensions)

  const bar = bounds_bar.selectAll("rect")
    .data(dataset_bar)
    .enter()
    .append("rect")
    .attr("x", d =>  xBarScale(xAccessor(d)))
    .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
    .attr("width", barWidth)
    .attr("height", d => yBarScale(yAccessor(d)))
    .attr("id", function(d){return "wk" + d.week_id})
    .on("mouseover", mouseOver)
    .on("mousemove", mouseMove)
    .on("mouseout", mouseOut)

    const xAxisGenerator = d3.axisBottom()
        .scale(xBarScale);

//    const xAxis = bounds_bar.append("g")
//         .call(xAxisGenerator)
//         .style("transform", `translateY(${dimensions.bar.boundedHeight}px)`)
//         .attr("id", "bar-x-axis");

//custom x axis because I'm a crazy person

    var parsePreciseTime = d3.timeParse("%Y-%m-%d");

    bounds_bar.append("line")
      .attr("x1", xBarScale(parsePreciseTime("2017-12-16")))
      .attr("x2", xBarScale(parsePreciseTime("2017-12-16")))
      .attr("y1", dimensions.bar.height)
      .attr("y2", dimensions.bar.boundedHeight)
      .attr("stroke", "white")

    bounds_bar.append("line")
    .attr("x1", xBarScale(parsePreciseTime("2018-12-16")))
    .attr("x2", xBarScale(parsePreciseTime("2018-12-16")))
      .attr("y1", dimensions.bar.height)
      .attr("y2", dimensions.bar.boundedHeight)
      .attr("stroke", "white")

      bounds_bar.append("line")
      .attr("x1", 0)
      .attr("x2", dimensions.bar.boundedWidth)
        .attr("y1", dimensions.bar.boundedHeight)
        .attr("y2", dimensions.bar.boundedHeight)
        .attr("stroke", "white")

    bounds_bar.append("text")
      .text("2017")
      .style("transform", `translate(${
          xBarScale(parsePreciseTime("2017-6-15"))
        }px,${
            dimensions.bar.boundedHeight + 30
          }px)`)
      .style("fill", "white")
      .style("font-size", "1rem")
      .style("text-anchor", "middle")


    bounds_bar.append("text")
      .text("2018")
      .style("transform", `translate(${
          xBarScale(parsePreciseTime("2018-6-15"))
        }px,${
            dimensions.bar.boundedHeight + 30
          }px)`)
      .style("fill", "white")
      .style("font-size", "1rem")
      .style("text-anchor", "middle")

    bounds_bar.append("text")
      .text("2019")
      .style("transform", `translate(${
          xBarScale(parsePreciseTime("2019-5-15"))
        }px,${
            dimensions.bar.boundedHeight + 30
          }px)`)
      .style("fill", "white")
      .style("font-size", "1rem")
      .style("text-anchor", "middle")

      bounds_bar.append("line")
      .attr("x1", 0)
      .attr("x2", dimensions.bar.boundedWidth)
        .attr("y1", dimensions.bar.boundedHeight)
        .attr("y2", dimensions.bar.boundedHeight)
        .attr("stroke", "white")

    const yAxisScale = d3.scaleLinear()
      .domain(d3.extent(dataset_bar, yAccessor))
      .range([dimensions.bar.boundedHeight, 0])
      .nice()

    const yAxisGenerator = d3.axisLeft()
      .scale(yAxisScale)
      .ticks(5)


    const yAxis = bounds_bar.append("g")
      .call(yAxisGenerator)
      .attr("id", "bar-y-axis")
      .attr("transform", "translate(-5,0)")


    const yAxisLabel = yAxis.append("text")
      .attr("y", 0)
      .attr("x", 10)
      .attr("fill", "black")
      .style("font-size", "1.4em")
      .text("Protests per week")
      .style("text-anchor", "start")

    ///labelling important events:

    var fontSize = ((width/800)*.7)+"rem"

    var lineGap = 150*(width/800)

    console.log(fontSize)

    bounds_bar.append("text")
      .text("March for Our Lives")
      .attr("x", xBarScale(parseTime("3/19/18"))+5)
      .attr("y", dimensions.bar.boundedHeight - yBarScale(1150+lineGap*2))
      .attr("font-size", fontSize)

    bounds_bar.append("text")
      .text("and school walkouts")
      .attr("x", xBarScale(parseTime("3/19/18"))+5)
      .attr("y", dimensions.bar.boundedHeight - yBarScale(1150 +lineGap))
      .attr("font-size", fontSize)

    bounds_bar.append("text")
      .text("for gun control")
      .attr("x", xBarScale(parseTime("3/19/18"))+5)
      .attr("y", dimensions.bar.boundedHeight - yBarScale(1150))
      .attr("font-size", fontSize)

    bounds_bar.append("text")
      .text("Climate")
      .attr("x", xBarScale(parseTime("9/16/19"))+10)
      .attr("y", dimensions.bar.boundedHeight - yBarScale(370))
      .attr("font-size", fontSize)

    bounds_bar.append("text")
      .text("Strike")
      .attr("x", xBarScale(parseTime("9/16/19"))+10)
      .attr("y", dimensions.bar.boundedHeight - yBarScale(370-lineGap))
      .attr("font-size", fontSize)


      bounds_bar.append("text")
        .text("Women's")
        .attr("x", xBarScale(parseTime("1/16/17"))+10)
        .attr("y", dimensions.bar.boundedHeight - yBarScale(490))
        .attr("font-size", fontSize)


      bounds_bar.append("text")
        .text("March")
        .attr("x", xBarScale(parseTime("1/16/17"))+10)
        .attr("y", dimensions.bar.boundedHeight - yBarScale(490-lineGap))
        .attr("font-size", fontSize)


        bounds_bar.append("text")
          .text("Families Belong Together")
          .attr("x", xBarScale(parseTime("6/25/18"))+10)
          .attr("y", dimensions.bar.boundedHeight - yBarScale(800))
          .attr("font-size", fontSize)

      bounds_bar.append("text")
        .text("against immigrant")
        .attr("x", xBarScale(parseTime("6/25/18"))+10)
        .attr("y", dimensions.bar.boundedHeight - yBarScale(800-lineGap))
        .attr("font-size", fontSize)

        bounds_bar.append("text")
          .text("family separation")
          .attr("x", xBarScale(parseTime("6/25/18"))+10)
          .attr("y", dimensions.bar.boundedHeight - yBarScale(800-lineGap*2))
          .attr("font-size", fontSize)

      ////function to draw highlight bars

      //bug here I can't figure out how to fix: make highlight bars clickable so that
      //slider can jump back in time by clicking on bars, not just forward in time

    const drawHighlight = function(data) {

      var highlights = bounds_bar_highlighting.selectAll(".highlight")
            .data(data)
      ///append new bars

      highlights.enter()
        .append("rect")
        .attr("x", d =>  xBarScale(xAccessor(d)))
        .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
        .attr("width", barWidth)
        .attr("height", d => yBarScale(yAccessor(d)))
        .attr("fill", "lightgray")
        .attr("id", function(d){return "wk" + d.week_id})
        .attr("class", "highlight")
        .on("mouseover", mouseOver)
        .on("mousemove", mouseMove)
        .on("mouseout", mouseOut)

      //remove any bars no longer included in the dataset

      highlights.exit()
        .remove()

    }

    const drawStars = function(data){

      bounds_map_stars.selectAll("circle")
        .data(data)
        .exit()
        .remove()

     stars = bounds_map_stars.selectAll(".star")
               .data(data)

       stars.enter()
         .append("circle")
         .attr("cx", d => projection([latAccessor(d), longAccessor(d)])[0])
         .attr("cy", d => projection([latAccessor(d), longAccessor(d)])[1])
         .attr("fill", "darkgray")
         .attr("r", 1)
         .attr("opacity", .2)
         .transition().duration(250)
           .attr("fill", "white")
           .attr("r", 1.5)
           .attr("opacity", .9)
         .transition().duration(249)
           .attr("fill", "lightgray")
           .attr("r", 1.5)
           .attr("opacity", 1)

      starsOn = false

      // console.log("drawStars occurred")
      // console.log(data)

    }

    var firstStarDate = d3.min(dataset_map, mapWeekAccessor)

    firstStarDate = firstStarDate.getTime()

    var starsData = dataset_map.filter(function (d){
      return mapWeekAccessor(d).getTime() == firstStarDate })

    var stars = bounds_map_stars.selectAll(".star")
           .data(starsData)

     stars.enter()
       .append("circle")
       .attr("cx", d => projection([latAccessor(d), longAccessor(d)])[0])
       .attr("cy", d => projection([latAccessor(d), longAccessor(d)])[1])
       .attr("fill", "darkgray")
       .attr("r", 1)
       .attr("opacity", .2)
       .transition().duration(250)
         .attr("fill", "white")
         .attr("r", 1.5)
         .attr("opacity", .9)
       .transition().duration(249)
         .attr("fill", "lightgray")
         .attr("r", 1.5)
         .attr("opacity", 1)

    var starsOn = true

    ///setting up for interaction

    var counter = 0

    var play = true

    var highlightData

    var sliderNewVal

    function playWeekChange(){
      // console.log("Play is " + play)

      if(play == true){

      counter = counter + 1

      sliderNewVal = d3.timeMonday.round(d3.timeWeek.offset(startDate, counter))

      // console.log(sliderNewVal)

     highlightData = dataset_bar.filter(function (d){
        return xAccessor(d) <= sliderNewVal; })

     starsData = dataset_map.filter(function (d){
      return mapWeekAccessor(d).getTime() == sliderNewVal.getTime() })

      // console.log(highlightData)

      drawHighlight(highlightData)

      drawStars(starsData)

      sliderTime.value(sliderNewVal)
      // console.log("playWeekChange seconds of sliderNewVal for " + sliderNewVal + " = "+sliderNewVal.getTime())


      }

    }

    playButton.addEventListener("click", function(){
      event.preventDefault()
      play = !play
      // console.log(play)
      if(play == true){
        d3.select(playButtonText)
          .text("Pause")
        d3.select("#button-pause-image")
          .style("display", "inline")
          d3.select("#button-play-image")
          .style("display", "none")
      } else {
        d3.select(playButtonText)
          .text("Play")
        d3.select("#button-play-image")
          .style("display", "inline")
          d3.select("#button-pause-image")
          .style("display", "none")
      }
    }, false)

    setInterval(playWeekChange, 500)

    function sliderClick(){
      play = false;
      d3.select(playButtonText)
              .text("Play")
      d3.select("#button-play-image")
      .style("display", "inline")
        d3.select("#button-pause-image")
        .style("display", "none")
    }

    var traceLine = document.getElementById("trace-line")
    var leftTransform
    // console.log(traceLine)

    function sliderEnd(){
      sliderNewVal = d3.timeMonday.round(sliderTime.value())
      // console.log(sliderNewVal)

      // console.log(sliderTime.value())

      highlightData = dataset_bar.filter(function (d){
        return xAccessor(d) <= sliderNewVal; })

     starsData = dataset_map.filter(function (d){
      return mapWeekAccessor(d).getTime() == sliderNewVal.getTime() })

     drawHighlight(highlightData)

      drawStars(starsData)

      counter = (((sliderNewVal - startDate)/86400000)/7)

      d3.select("#trace-line")
        .style("opacity", "0")
        .attr("transform")

    // console.log("sliderEnd seconds of sliderNewVal for " + sliderNewVal + " = "+sliderNewVal.getTime())

    sliderClick()


    }

    function sliderChange(){

      var handle = document.getElementById("handle-bounds")
      leftTransform = handle.transform.baseVal[0]["matrix"].e

      d3.select("#trace-line-wrapper")
        .attr("transform", "translate(" + (leftTransform + dimensions.slider.margin.left + traceLineLeft) + ","+ (dimensions.slider.margin.top)+")")
        .attr("height", dimensions.total.height - (dimensions.slider.height/2))
        .attr("width", barWidth)

        d3.select("#trace-line")
          .style("opacity", .5)
          .attr("y2", dimensions.total.height - (dimensions.slider.height/2))
          .style("stroke", "white")
          .style("stroke-width", barWidth)

      sliderClick()

    }

    var xClickPosition

    function clickBar(){
      xClickPosition = parseFloat(d3.select(this).attr("x"))

      weekClicked = xBarScale.invert(xClickPosition)

      sliderNewVal = weekClicked

      sliderTime.value(sliderNewVal)

      highlightData = dataset_bar.filter(function (d){
        return xAccessor(d) <= sliderNewVal; })

    starsData = dataset_map.filter(function (d){
      return mapWeekAccessor(d).getTime() == sliderNewVal.getTime() })

      drawHighlight(highlightData)
      drawStars(starsData)

      counter = (((sliderNewVal - startDate)/86400000)/7)

      sliderClick()

  // console.log("clickBar seconds of sliderNewVal for " + sliderNewVal + " = "+sliderNewVal.getTime())

      // sliderTime.value()
    }

    sliderTime
      .on("end", sliderEnd)
      .on("drag", sliderChange)

    bar
      .on("click", clickBar)

}

mapAndBar()
