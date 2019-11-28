//working from this slider tutorial: https://bl.ocks.org/johnwalley/e1d256b81e51da68f7feb632a53c3518

async function timerTry(){

const dataset_bar = await d3.csv("protest_per_week_usa.csv")

const parseTime = d3.timeParse("%m/%d/%y")
const formatDate = d3.timeFormat("%m/%d/%y")
const formatDateIntoYear = d3.timeFormat("%Y")
const yAccessor = d => +d.count
const xAccessor = d => parseTime(d.week)
const weekIdAccessor = d => +d.week_id


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
    label: {
      width: width,
      height: 70,
      margin: {
        top: 15,
        bottom: 10,
        left: 70
      },
      sliderStart: 30
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

dimensions.label.boundedHeight = dimensions.label.height
  - dimensions.label.margin.top - dimensions.label.margin.top

  dimensions.total.width = width
  dimensions.total.height = dimensions.bar.height + dimensions.label.height + dimensions.label.sliderStart

  dimensions.bar.top = dimensions.slider.height

  dimensions.slider.boundedWidth = dimensions.slider.width
    - dimensions.slider.margin.left
    - dimensions.slider.margin.right

    const xBarScale = d3.scaleBand()
      .domain(d3.range(dataset_bar.length))
      .range([0, dimensions.bar.boundedWidth])

    const xSliderScale = d3.scaleTime()
      .domain([startDate, endDate])
      .range([0, targetValue])
      .clamp(true)

    const yBarScale = d3.scaleLinear()
      .domain(d3.extent(dataset_bar, yAccessor))
      .range([0, dimensions.bar.boundedHeight])
      .nice()

    const xAxisScale = d3.scaleTime()
        .domain([startDate, endDate])
        .range([0, dimensions.bar.boundedWidth])

  const barWrapper = d3.select("#bar-wrapper")
    .append("svg")
      .attr("width", dimensions.bar.width)
      .attr("height", dimensions.bar.height)

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
        // .on("onchange", val => {
        //   d3.select("p#value-time").text(d3.timeFormat("Week of %m/%d/%Y")(val))
        // })

    var gTime = d3.select("div#slider-time")
          .append("svg")
          .attr("width", dimensions.slider.width)
          .attr("height", dimensions.slider.height)
          .append("g")
          .attr("transform", "translate(" + dimensions.slider.margin.left + ","+ dimensions.slider.margin.top+")")

    var pickSlider = gTime.call(sliderTime)

    //d3.select("p#value-time").text(d3.timeFormat("Week of %m/%d/%Y")(sliderTime.value()))

        var bounds_bar = barWrapper.append("g")
              .attr("class", "plot")
              .attr("transform", "translate(" + dimensions.bar.margin.left + "," + dimensions.bar.margin.top + ")")

        // var bounds_bar_highlighting = barWrapper.append("g")
        //   .attr("class", "plot")
        //   .attr("transform", "translate(" + dimensions.bar.margin.left + "," + dimensions.bar.margin.top + ")")

        const barPadding = 1

        //this is how many weeks there are in the dataset--use this to make barwidth
        var weekCount = Math.round(((endDate - startDate)/86400000)/7)
        var barWidth = (dimensions.bar.boundedWidth / weekCount) - (barPadding)

        const mouseOver = function(d){
          //reference for mouse position came from here: https://www.d3-graph-gallery.com/graph/interactivity_tooltip.html

          var xPosition = parseFloat(d3.select(this).attr("x")) + dimensions.bar.margin.left + 125;
          var yPosition = d3.mouse(this)[1]+ dimensions.bar.margin.top + dimensions.bar.top + 150;

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
          var xPosition = parseFloat(d3.select(this).attr("x")) + dimensions.bar.margin.left + 125 ;
          var yPosition = d3.mouse(this)[1] + dimensions.bar.margin.top + dimensions.bar.top + 150;

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

        const barID = d => Math.round(((xAccessor(d) - startDate)/86400000/7))


        const bar = bounds_bar.selectAll("rect")
          .data(dataset_bar)
          .enter()
          .append("rect")
          .attr("x", d =>  xAxisScale(xAccessor(d)))
          .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
          .attr("width", barWidth)
          .attr("height", d => yBarScale(yAccessor(d)))
          .attr("id", function(d){return "wk" + d.week_id})
          .on("mouseover", mouseOver)
          .on("mousemove", mouseMove)
          .on("mouseout", mouseOut)
      //
      // var barHighlight = bounds_bar_highlighting.selectAll("rect")
      //   .data(dataset_bar)
      //   .enter()
      //   .append("rect")
      //   .attr("x", d =>  xAxisScale(xAccessor(d)))
      //   .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
      //   .attr("width", barWidth)
      //   .attr("height", d => yBarScale(yAccessor(d)))
      //   .attr("id", d => "wk" + barID(d))
      //   .attr("fill", "black")
      //     .on("mouseover", mouseOver)
      //     .on("mousemove", mouseMove)
      //     .on("mouseout", mouseOut)

      const xAxisGenerator = d3.axisBottom()
          .scale(xAxisScale)

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

    var counter = 0

    var play = false

    var playButton = document.querySelector("#play-button")


    playButton.addEventListener("click", function(){

      event.preventDefault()
      play = !play
      console.log(play)
      //how to change text?
      if(play == true){
        d3.select(playButton)
          .text("Pause")
      } else {
        d3.select(playButton)
          .text("Play")
      }

    }, false)

    function weekChange() {

    // var barHighlight = bounds_bar_highlighting.selectAll("rect")
    //   .data(dataset_bar)
    //   .enter()
    //   .append("rect")
    //   .attr("x", d =>  xAxisScale(xAccessor(d)))
    //   .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
    //   .attr("width", barWidth)
    //   .attr("height", d => yBarScale(yAccessor(d)))
    //   .attr("id", d => "wk" + barID(d))
    //   .attr("fill", "black")
    //     .on("mouseover", mouseOver)
    //     .on("mousemove", mouseMove)
    //     .on("mouseout", mouseOut)

      if(play == true){

        var lastWeekId = "#wk"+counter

        d3.select(lastWeekId)
          .attr("fill", "black")
        console.log(counter)

        var sliderNewVal = d3.timeWeek.offset(startDate, counter)

        counter = counter + 1

        var thisWeekId = "#wk"+counter
        console.log(thisWeekId)
        d3.select(thisWeekId)
          .attr("fill", "lightgray")

        //would be great to make this smooth instead of jerky, but having trouble
        //figuring out a way to add a transition to this

        sliderTime.value(sliderNewVal)

      } else {
        var sliderWeekPosition = sliderTime.value()
        counter = (((sliderWeekPosition - startDate)/86400000)/7)

      }
    }

    setInterval(weekChange, 500)

    function sliderChange(){


            console.log("change registered sliderChange")
            var oldCounter = counter
            var sliderWeekPosition = sliderTime.value()

            counter = (((sliderWeekPosition - startDate)/86400000)/7)

            var lastWeekId = "#wk"+oldCounter
            var thisWeekId = "#wk"+counter
            console.log(lastWeekId, thisWeekId)

            d3.select(thisWeekId)
              .attr("fill", "lightgray")

            d3.select(lastWeekId)
              .attr("fill", "black")
      //
      //
      // console.log("change registered sliderChange")
      //
      //
      // var sliderWeekPosition = sliderTime.value()
      // var oldSliderCounter = counter
      // var counter = (((sliderWeekPosition - startDate)/86400000)/7)
      //
      // var lastWeekIdSlider = "#wk"+oldSliderCounter
      //
      //
      // var thisWeekIdSlider = "#wk"+counter
      //
      // d3.select(thisWeekIdSlider)
      //   .attr("fill", "lightgray")
      //
      // d3.select(lastWeekIdSlider)
      //   .attr("fill", "black")

      //
      // if(counter < oldCounter){
      //   var highlightData = dataset_bar.filter(function (d){
      //     return xAccessor(d) <= sliderWeekPosition;
      //   })
      //
      //   console.log("sliderChange counter smaller")
      //
      //   var barHighlight = bounds_bar_highlighting.selectAll("rect")
      //     .data(highlightData)
      //     .exit()
      //     .remove()
      //
      //     var barHighlight = bounds_bar_highlighting.selectAll("rect")
      //       .data(highlightData)
      //       .enter()
      //       .append("rect")
      //       .attr("x", d =>  xAxisScale(xAccessor(d)))
      //       .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
      //       .attr("width", barWidth)
      //       .attr("height", d => yBarScale(yAccessor(d)))
      //       .attr("id", d => "wk" + barID(d))
      //       .attr("fill", "black")
      //         .on("mouseover", mouseOver)
      //         .on("mousemove", mouseMove)
      //         .on("mouseout", mouseOut)
      //
      // } else if(counter > oldCounter){
      //
      //   var highlightData = dataset_bar.filter(function (d){
      //     return xAccessor(d) <= sliderWeekPosition;
      //   })
      //
      //   console.log("sliderChange counter bigger old counter = "+oldCounter+" new counter = "+counter)
      //
      //   console.log(highlightData)
      //
      //   var barHighlight = bounds_bar_highlighting.selectAll("rect")
      //     .data(highlightData)
      //     .exit()
      //     .remove()
      //
      //   var barHighlight = bounds_bar_highlighting.selectAll("rect")
      //     .data(highlightData)
      //     .enter()
      //     .append("rect")
      //     .attr("x", d =>  xAxisScale(xAccessor(d)))
      //     .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
      //     .attr("width", barWidth)
      //     .attr("height", d => yBarScale(yAccessor(d)))
      //     .attr("id", d => "wk" + barID(d))
      //     .attr("fill", "lightgray")
      //       .on("mouseover", mouseOver)
      //       .on("mousemove", mouseMove)
      //       .on("mouseout", mouseOut)
      //
      // }

    }


      // var sliderChooseBarId = "#wk"+counter
      //
      // d3.select(sliderChooseBarId).attr("fill", "white")


    function sliderEnd(){

      console.log("change registered sliderEnd")
      var oldCounter = counter
      var sliderWeekPosition = sliderTime.value()

      counter = (((sliderWeekPosition - startDate)/86400000)/7)

      var lastWeekId = "#wk"+oldCounter
      var thisWeekId = "#wk"+counter
      console.log(lastWeekId, thisWeekId)

      d3.select(thisWeekId)
        .attr("fill", "lightgray")

      d3.select(lastWeekId)
        .attr("fill", "black")

      //
      // if(counter < oldCounter){
      //   var highlightData = dataset_bar.filter(function (d){
      //     return xAccessor(d) >= sliderWeekPosition;
      //   })
      //
      //   console.log("sliderEnd counter smaller")
      //
      //   var barHighlight = bounds_bar_highlighting.selectAll("rect")
      //     .data(highlightData)
      //     .enter()
      //     .append("rect")
      //     .attr("x", d =>  xAxisScale(xAccessor(d)))
      //     .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
      //     .attr("width", barWidth)
      //     .attr("height", d => yBarScale(yAccessor(d)))
      //     .attr("id", d => "wk" + barID(d))
      //     .attr("fill", "black")
      //       .on("mouseover", mouseOver)
      //       .on("mousemove", mouseMove)
      //       .on("mouseout", mouseOut)
      // } else if(counter > oldCounter){
      //
      //   var highlightData = dataset_bar.filter(function (d){
      //     return xAccessor(d) <= sliderWeekPosition;
      //   })
      //
      //   console.log("sliderEnd counter smaller")
      //
      //   var barHighlight = bounds_bar_highlighting.selectAll("rect")
      //     .data(highlightData)
      //     .enter()
      //     .append("rect")
      //     .attr("x", d =>  xAxisScale(xAccessor(d)))
      //     .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
      //     .attr("width", barWidth)
      //     .attr("height", d => yBarScale(yAccessor(d)))
      //     .attr("id", d => "wk" + barID(d))
      //       .attr("fill", "lightgray")
      //       .on("mouseover", mouseOver)
      //       .on("mousemove", mouseMove)
      //       .on("mouseout", mouseOut)
      // }
        // var sliderChooseBarId = "#wk"+counter
        //
        // d3.select(sliderChooseBarId).attr("fill", "lightgray")

    }


    sliderTime
    //.on("onchange", sliderChange)
    .on("end", sliderEnd)


  }

  timerTry()
