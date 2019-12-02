async function mapAndBar(){

////map setup///

  const stateShapes = await d3.json("us-states.json")
  const dataset_map = await d3.csv("all_protests_major_tags.csv")

  const latAccessor = d => +d.longitude
  const longAccessor = d => +d.latitude
  const mapParseTime = d3.timeParse("%Y-%m-%d")
  const mapWeekAccessor = d => mapParseTime(d.week)

//   const width = window.innerWidth * 0.75

  const width = d3.min([window.innerWidth*.9, 800])


  const dimensions = {
    map: {
      width: width*.8,
      margin: {
        top: 5,
        right: 30,
        bottom: 5,
        left: 30,
      },
    },
    bar: {
      width: width,
      height: width*.25,
      margin: {
        top: 15,
        bottom: 30,
        left: 70,
        right: 70
        }
      },
      total:{
      },
      slider: {
        height: 70,
        width: width+60,
        margin: {
          left: 100,
          right: 100,
          top: 20
        }
      }
    }

   dimensions.bar.boundedHeight = dimensions.bar.height
    - dimensions.bar.margin.top - dimensions.bar.margin.bottom
   dimensions.bar.boundedWidth = dimensions.bar.width
    - dimensions.bar.margin.left - dimensions.bar.margin.right

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

  dimensions.total.height = dimensions.bar.height + dimensions.map.height
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


    const dataset_bar = await d3.csv("./../protest_per_week_usa.csv")
    // console.log(dataset_bar)
    const parseTime = d3.timeParse("%Y-%m-%d")
    const formatDate = d3.timeFormat("%m/%d/%y")
    const yAccessor = d => +d.count
    const xAccessor = d => parseTime(d.week)

    const startDate = d3.min(dataset_bar, xAccessor)
    const endDate = d3.max(dataset_bar, xAccessor)

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
          .tickFormat(d3.timeFormat("Week of %B %d, %Y"))
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

    const traceLineWrapper = fullWrapper.append("g")
            .attr("transform", "translate(" + dimensions.slider.margin.left + ","+ dimensions.slider.margin.top+")")

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
        .scale(xBarScale)

   const xAxis = bounds_bar.append("g")
        .call(xAxisGenerator)
        .style("transform", `translateY(${dimensions.bar.boundedHeight}px)`)
        .attr("id", "bar-x-axis")

    const yAxisScale = d3.scaleLinear()
      .domain(d3.extent(dataset_bar, yAccessor))
      .range([dimensions.bar.boundedHeight, 0])
      .nice()

    const yAxisGenerator = d3.axisLeft()
      .scale(yAxisScale)

    const yAxis = bounds_bar.append("g")
      .call(yAxisGenerator)
      .attr("id", "bar-y-axis")

    const yAxisLabel = yAxis.append("text")
      .attr("y", 0)
      .attr("x", 10)
      .attr("fill", "black")
      .style("font-size", "1.4em")
      .text("Protests per week")
      .style("text-anchor", "start")

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

    var playButton = document.querySelector("#play-button")

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
        d3.select(playButton)
          .text("Pause")
      } else {
        d3.select(playButton)
          .text("Play")
      }
    }, false)

    setInterval(playWeekChange, 500)

    function sliderClick(){
      play = false;
      d3.select(playButton)
              .text("Pause")
      console.log("animation paused")
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
        .attr("transform", "translate(" + (leftTransform + dimensions.slider.margin.left) + ","+ (dimensions.slider.margin.top+40)+")")
        .attr("height", dimensions.total.height+55)
        .attr("width", barWidth)

        d3.select("#trace-line")
          .style("opacity", .5)
          .attr("y2", dimensions.total.height - (dimensions.slider.margin.top+30))
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
