async function timerTry(){

  const dataset_bar = await d3.csv("protest_per_week_usa.csv")
  const parseTime = d3.timeParse("%m/%d/%y")
  const formatDate = d3.timeFormat("%m/%d/%y")
  const yAccessor = d => +d.count
  const xAccessor = d => parseTime(d.week)

  const startDate = d3.min(dataset_bar, xAccessor)
  const endDate = d3.max(dataset_bar, xAccessor)

  var weekCount = Math.round(((endDate - startDate)/86400000)/7)


  const width = window.innerWidth * 0.75

  const dimensions = {

    bar: {
      width: width,
      height: width*.25,
      margin: {
        top: 10,
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

    dimensions.total.width = width
    dimensions.total.height = dimensions.bar.height + dimensions.slider.height

    dimensions.bar.top = dimensions.slider.height

    dimensions.slider.boundedWidth = dimensions.slider.width
      - dimensions.slider.margin.left
      - dimensions.slider.margin.right

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

      var xPosition = parseFloat(d3.select(this).attr("x")) + dimensions.bar.margin.left + 125;
      var yPosition = d3.mouse(this)[1]+ dimensions.bar.margin.top + dimensions.bar.top + 150;
      // console.log("xPosition "+xPosition+" = "+parseFloat(d3.select(this).attr("x"))+"+"+dimensions.bar.margin.left+"+"+"125")

      d3.select("#bar-tooltip").classed("hidden", false)

      d3.select("#bar-tooltip")
        .style("left", xPosition + "px")
        .style("top", yPosition + "px")
        .select("#protest-count")
        .text(yAccessor(d))

      d3.select("#week-bar-tooltip")
        .text(d.week)
    }


    const mouseMove = function(d){
      var xPosition = parseFloat(d3.select(this).attr("x")) + dimensions.bar.margin.left+(dimensions.bar.width*.17)  ;
      var yPosition = d3.mouse(this)[1] + dimensions.bar.margin.top + dimensions.bar.top + 100;

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

    const bar = bounds_bar.selectAll("rect")
      .data(dataset_bar)
      .enter()
      .append("rect")
      .attr("x", d =>  xBarScale(xAccessor(d)))
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

        //canNOT get this line to extend down into plot, which I want it to do!!!

        // const traceLine = handleBounds.append("line")
        //   .attr("x1", 0)
        //   .attr("y1", 0)
        //   .attr("x2", 0)
        //   .attr("y2", dimensions.total.height)
        //   .attr("stroke", "black")

////function to draw highlight bars

      //bug here I can't figure out how to fix: make highlight bars clickable so that
      //slider can jump back in time by clicking on bars, not just forward in time

    const drawHighlight = function(data) {

      var highlights = bounds_bar_highlighting.selectAll(".highlight")
            .data(data)
      ///append new bars

      highlights.enter()
        .append("rect")
        .attr("x", d =>  xBarScale(xAccessor(d)))
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

      sliderNewVal = d3.timeWeek.offset(startDate, counter)

      // console.log(sliderNewVal)

      highlightData = dataset_bar.filter(function (d){
        return xAccessor(d) <= sliderNewVal; })

      // console.log(highlightData)

      drawHighlight(highlightData)

      sliderTime.value(sliderNewVal)

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

    var traceLine = document.getElementById("trace-line")
    var leftTransform
    // console.log(traceLine)

    function sliderEnd(){
      sliderNewVal = sliderTime.value()
      // console.log(sliderNewVal)

      highlightData = dataset_bar.filter(function (d){
        return xAccessor(d) <= sliderNewVal; })

      drawHighlight(highlightData)

      counter = (((sliderNewVal - startDate)/86400000)/7)

      d3.select("#trace-line")
        .style("opacity", "0")
        .attr("transform")

    }

    function sliderChange(){

      var handle = document.getElementById("handle-bounds")
      leftTransform = handle.transform.baseVal[0]["matrix"].e
      console.log(traceLine)

      d3.select("#trace-line-wrapper")
        .attr("transform", "translate(" + (leftTransform + dimensions.slider.margin.left) + ","+ (dimensions.slider.margin.top+40)+")")
        .attr("height", dimensions.total.height+55)
        .attr("width", barWidth)

        d3.select("#trace-line")
          .style("opacity", .5)
          .attr("y2", dimensions.total.height - (dimensions.slider.margin.top+30))
          .style("stroke", "white")
          .style("stroke-width", barWidth)

      console.log(leftTransform)


    }

    var xClickPosition

    function clickBar(){
      xClickPosition = parseFloat(d3.select(this).attr("x"))

      weekClicked = xBarScale.invert(xClickPosition)

      sliderNewVal = weekClicked

      sliderTime.value(sliderNewVal)

      highlightData = dataset_bar.filter(function (d){
        return xAccessor(d) <= sliderNewVal; })

      console.log(highlightData)

      drawHighlight(highlightData)

      counter = (((sliderNewVal - startDate)/86400000)/7)

      // sliderTime.value()
    }

    sliderTime
      .on("end", sliderEnd)
      .on("drag", sliderChange)

    bar
      .on("click", clickBar)

}

timerTry()
