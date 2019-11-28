//slider based on Jane Pong's work here: https://bl.ocks.org/officeofjane/47d2b0bfeecfcb41d2212d06d095c763


async function drawBar(){

  const dataset_bar = await d3.csv("protest_per_week_usa.csv")

  const parseTime = d3.timeParse("%m/%d/%y")
  const formatDate = d3.timeFormat("%m/%d/%y")
  const formatDateIntoYear = d3.timeFormat("%Y")
  const yAccessor = d => +d.count
  const xAccessor = d => parseTime(d.week)
  const weekIdAccessor = d => +d.week_id


  const startDate = d3.min(dataset_bar, xAccessor)
  const endDate = d3.max(dataset_bar, xAccessor)

  console.log(d3.max(dataset_bar, weekIdAccessor))

  const width = window.innerWidth * 0.75

  const dimensions = {

    bar: {
      width: width,
      height: width*.25,
      margin: {
        top: 10,
        bottom: 30,
        left: 70,
        right: 30
        }
      },
      label: {
        width: width,
        height: 100,
        margin: {
          top: 15,
          bottom: 10,
          left: 70
        },
        sliderStart: 30
      },
      total:{

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

    dimensions.bar.top = dimensions.label.height


  const barWrapper = d3.select("#bar-wrapper")
    .append("svg")
      .attr("width", dimensions.total.width)
      .attr("height", dimensions.total.height)

  var targetValue = dimensions.bar.boundedWidth

//try making a time-based scale for x bars, as in the x axis scale below, calculate bar width, then calculate x position by scale - half of bar width?

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

      console.log(xSliderScale(endDate))


  //slider

  var moving = false
  var currentValue = 0

  var playButton = d3.select("#play-button")

  var slider = barWrapper.append("g")
    .attr("class", "slider")
    .attr("transform", "translate(" + dimensions.label.margin.left + "," + "0)")

  slider.append("line")
      .attr("class", "track")
      .attr("x1", xSliderScale.range()[0])
      .attr("x2", xSliderScale.range()[1])
      .attr("transform", "translate(0,"+dimensions.label.sliderStart + ")")
    .select(function() { return this.parentNode.appendChild(this.cloneNode(true))})
      .attr("class", "track-inset")
      .attr("transform", "translate(0,"+dimensions.label.sliderStart + ")")
    .select(function() {return this.parentNode.appendChild(this.cloneNode(true))})
      .attr("class", "track-overlay")
      .attr("transform", "translate(0,"+dimensions.label.sliderStart + ")")
      .call(d3.drag()
        .on("start.interrupt", function() {slider.interrupt()})
        .on("start drag", function(){
          currentValue = d3.event.x
          update(x.invert(currentValue))
        })
      )

    slider.insert("g", ".track-overlay")
        .attr("class", "ticks")
        .attr("transform", "translate(0," + (dimensions.label.sliderStart) + ")")
        .attr("id", "slider-ticks")
      .selectAll("text")
        .data(xSliderScale.ticks(3))
        .enter()
        .append("text")
        .attr("x", xSliderScale)
        .attr("y", (dimensions.label.sliderStart))
        .attr("text-anchor", "middle")
        .text(function(d) {return formatDateIntoYear(d)})

    d3.select("#slider-ticks").append("text")
      .attr("y", (dimensions.label.sliderStart))
      .attr("x", 0)
      .attr("text-anchor", "middle")
      .text("2017")

    var handle = slider.insert("circle", ".track-overlay")
      .attr("class", "handle")
      .attr("r", 9)
      .attr("transform", "translate(0,"+dimensions.label.sliderStart + ")")


    var label = slider.append("text")
      .attr("class", "label")
      .attr("text-anchor", "middle")
      .text(formatDate(startDate))
      .attr("transform", "translate(0,"+dimensions.label.margin.top + ")")
      .style("color", "black")

      ///draw bar chart

    var bounds_bar = barWrapper.append("g")
          .attr("class", "plot")
          .attr("transform", "translate(" + dimensions.bar.margin.left + "," + dimensions.bar.top + ")")

    var bounds_bar_highlighting = barWrapper.append("g")
      .attr("class", "plot")
      .attr("transform", "translate(" + dimensions.bar.margin.left + "," + dimensions.bar.top + ")")

    const barPadding = 1

    //this is how many weeks there are in the dataset--use this to make barwidth
    var weekCount = Math.round(((endDate - startDate)/86400000)/7)
    var barWidth = (dimensions.bar.boundedWidth / weekCount) - (barPadding)

    console.log(barWidth)

    const mouseOver = function(d){
      //reference for mouse position came from here: https://www.d3-graph-gallery.com/graph/interactivity_tooltip.html

      var xPosition = parseFloat(d3.select(this).attr("x")) + xBarScale.bandwidth()*.25 + 70;
      var yPosition = d3.mouse(this)[1]+dimensions.bar.top + dimensions.bar.margin.top + 60;

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
      var xPosition = parseFloat(d3.select(this).attr("x")) + xBarScale.bandwidth()*.25 + 70;
      var yPosition = d3.mouse(this)[1]+dimensions.bar.top + dimensions.bar.margin.top + 60;

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
      .attr("x", d =>  xAxisScale(xAccessor(d)))
      .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
      .attr("width", barWidth)
      .attr("height", d => yBarScale(yAccessor(d)))
      .attr("id", function(d){return "wk" + d.week_id})
      .on("mouseover", mouseOver)
      .on("mousemove", mouseMove)
      .on("mouseout", mouseOut)


  //Add peripherals


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

      playButton
        .on("click", function(){
          var button = d3.select(this)
          if (button.text() == "Pause"){
            moving = false
            clearInterval(timer)
            button.text("Play")
          } else {
            moving = true
            timer = setInterval(step, 100)
            button.text("Pause")
          }
          console.log("Slider moving: " + moving)
        })


      function seeWeekChange(data){

        const seeChange = bounds_bar_highlighting.selectAll("rect")
          .data(data)
          .enter()
          .append("circle")
          .attr("x", d =>  xAxisScale(xAccessor(d)))
          .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
          .attr("width", barWidth)
          .attr("height", d => yBarScale(yAccessor(d)))
          .attr("fill", "white")


      }

      function update(h){
        console.log(h)

        handle.attr("cx", xSliderScale(h))
        label
        .attr("x", xSliderScale(h))
        .text(formatDate(h))

        var newData = dataset_bar.filter(function (d){
          return xAccessor(d) <= h;
        })

        console.log(newData)

        seeChange(newData)
      }

      function step() {
        update(xSliderScale.invert(currentValue));
        currentValue = currentValue + (targetValue/weekCount)
        if (currentValue > targetValue){
          moving = false;
          currentValue = 0;
          clearInterval(timer);
          playButton.text("Play");
          console.log("Slider moving "+ moving)
        }
      }



}

drawBar()




// async function drawMapAndBar() {
//
//   //get data
//
//   // for some reason this absolutely WILL NOT work with the original lat_long data,
//   // but does work for data in the additional_tags csv???? even though I copy/pasted
//   // the values over and it's the exact same shit? also won't work if I call the
//   // additional_tags columns latitude and longitude. however, if I call the
//   // protest_lat_long columns internal_point_longitude etc, that doesn't fix the problem.
//   // WHAT DO, D3
//
//   //for date slider, start with this: https://bl.ocks.org/officeofjane/47d2b0bfeecfcb41d2212d06d095c763 and if that doesn't work
//   //try spiral example here for play/pause/restart buttons? https://observablehq.com/@palewire/svg-path-animations-d3-transition
//
//
//   const stateShapes = await d3.json("us-states.json")
//   const dataset_map = await d3.csv("protest_additional_tags_2.csv")
//   const dataset_bar = await d3.csv("protest_per_week_usa.csv")
//
//   const latAccessor = d => +d.internal_point_latitude
//   const longAccessor = d => +d.internal_point_longitude
//
//   const yAccessor = d => +d.count
//   const parseTime = d3.timeParse("%Y-%m-%d")
//   const xAccessor = d => parseTime(d.week)
//
//
//   let weekMax = d3.max(dataset_map, function(d) { return +d.week_id;} );
//
//
//   //set chart dimensions
//
//   const width = window.innerWidth * 0.5
//
//
//   const dimensions = {
//     map: {
//       width: width,
//       margin: {
//         top: 5,
//         right: 30,
//         bottom: 10,
//         left: 30,
//       },
//     },
//     bar: {
//       width: width,
//       height: width*.25,
//       margin: {
//         top: 10,
//         bottom: 30,
//         left: 50,
//         right: 30
//         }
//       },
//       total:{
//         width: width
//       },
//     label: {
//       width: width,
//       height: 100
//     }
//     }
//
//    dimensions.bar.boundedHeight = dimensions.bar.height
//     - dimensions.bar.margin.top - dimensions.bar.margin.bottom
//    dimensions.bar.boundedWidth = dimensions.bar.width
//     - dimensions.bar.margin.left - dimensions.bar.margin.right
//
//   dimensions.map.boundedWidth = dimensions.map.width
//     - dimensions.map.margin.left
//     - dimensions.map.margin.right
//
//   const sphere = ({type: "Sphere"})
//
//   const projection = d3.geoAlbersUsa()
//     .fitWidth(dimensions.map.boundedWidth, sphere)
//
//   const pathGenerator = d3.geoPath(projection)
//   const [[x0, y0], [x1, y1]] = pathGenerator.bounds(sphere)
//
//   dimensions.map.boundedHeight = y1
//   dimensions.map.height = dimensions.map.boundedHeight
//     + dimensions.map.margin.top
//     + dimensions.map.margin.bottom
//
//   dimensions.total.height = dimensions.bar.height + dimensions.map.height
//
//   dimensions.bar.boundedTop = dimensions.map.height + dimensions.bar.margin.top
//
//     //draw canvas
//
//     const mapWrapper = d3.select("#map-wrapper")
//       .append("svg")
//         .attr("width", dimensions.map.width)
//         .attr("height", dimensions.map.height)
//
//     const barWrapper = d3.select("#bar-wrapper")
//       .append("svg")
//         .attr("width", dimensions.bar.width)
//         .attr("height", dimensions.bar.height)
//
//     const labelWrapper = d3.select("#week-tooltip")
//       .append("svg")
//       .attr("width", dimensions.label.width)
//       .attr("height", dimensions.label.height)
//
//     //map canvas:
//
//     const bounds_map = mapWrapper.append("g")
//       .style("transform", `translate(${
//         dimensions.map.margin.left
//       }px, ${
//         dimensions.map.margin.top
//       }px)`)
//       .attr("id", "map-states-group")
//
//     const bounds_map_background = mapWrapper.append("g")
//       .style("transform", `translate(${
//         dimensions.map.margin.left
//       }px, ${
//         dimensions.map.margin.top
//       }px)`)
//       .attr("id", "map-background-stars")
//
//       const bounds_map_stars = mapWrapper.append("g")
//         .style("transform", `translate(${
//           dimensions.map.margin.left
//         }px, ${
//           dimensions.map.margin.top
//         }px)`)
//         .attr("id", "map-stars-group")
//
//
//
//
//   //bar canvas:
//
//     const bounds_bar = barWrapper.append("g")
//         .style("transform", `translate(${
//           dimensions.bar.margin.left
//         }px, ${
//           dimensions.bar.margin.top
//         }px)`)
//       .attr("id", "bar-group")
//
//
//     // create scales
//
//       const xBarScale = d3.scaleBand()
//         .domain(d3.range(dataset_bar.length))
//         .range([0, dimensions.bar.boundedWidth])
//
//       const xSliderScale = d3.scaleTime()
//         .domain([d3.extent(dataset_bar, xAccessor)])
//         .range([0, dimensions.bar.boundedWidth])
//         .clamp(true)
//
//       const yBarScale = d3.scaleLinear()
//         .domain(d3.extent(dataset_bar, yAccessor))
//         .range([0, dimensions.bar.boundedHeight])
//         .nice()
//
//       ////setup for slider and play/pause button:
//
//       var moving = false;
//       var currentValue = 0;
//       var targetValue = dimensions.bar.boundedWidth;
//
//       var playButton = d3.select("#play-button")
//
//       var slider = labelWrapper.append("g")
//         .attr("class", "slider")
//         .attr("transform", "translate(" + dimensions.bar.margin.left + "," + dimensions.bar.boundedHeight/5 + ")")
//
//       slider.append("line")
//         .attr("class", "track")
//         .attr("x1", xSliderScale.range()[0])
//         .attr("x2", xSliderScale.range()[1])
//         .select(function() { return this.parentNode.appendChild(this.cloneNode(true)); })
//           .attr("class", "track-inset")
//         .select(function() { return this.parentNode.appendChild(this.cloneNode(true)); })
//           .attr("class", "track-overlay")
//           .call(d3.drag()
//             .on("start.interrupt", function(){slider.interrupt(); })
//             .on("start drag", function() {
//               currentValue = d3.event.x;
//               update(xBarScale.invert(currentValue));
//             })
//           )
//
//       slider.insert("g", ".track-overlay")
//           .attr("class", "ticks")
//           .attr("transform", "translate(0,"+18+")")
//         .selectAll("text")
//           .data(xSliderScale.ticks(10))
//           .enter()
//           .append("text")
//           .attr("x", xBarScale)
//           .attr("y", 10)
//           .attr("text-anchor", "middle")
//           .text(function(d) { return formatDateIntoYear(d)})
//
//       var handle = slider.insert("circle", ".track-overlay")
//         .attr("class", "handle")
//         .attr("r", 9)
//
//       var label = slider.append("text")
//         .attr("class", "label")
//         .attr("text-anchor", "middle")
//         .attr("transform", "translate(0," + 30 + ")")
//         .text("label will go here")
//
//
//     d3.csv()
//
//     //draw states and background stars
//
//     const states = bounds_map.selectAll(".state")
//       .data(stateShapes.features)
//       .enter().append("path")
//         .attr("class", "state")
//         .attr("d", pathGenerator)
//         .attr("fill", "black")
//
//     const backgroundStars = bounds_map_background.selectAll("circle")
//         .data(dataset_map)
//         .enter().append("circle")
//         .attr("cx", d => projection([latAccessor(d), longAccessor(d)])[0])
//         .attr("cy", d => projection([latAccessor(d), longAccessor(d)])[1])
//         .attr("fill", "darkgray")
//         .attr("r", .5)
//         .attr("opacity", .2)
//
//
//     let wk = 0
//
//     //////draw the full bar chart
//
//     function drawBar(data){
//       const barPadding = 1
//
//       const mouseOver = function(d){
//         //reference for mouse position came from here: https://www.d3-graph-gallery.com/graph/interactivity_tooltip.html
//
//         var xPosition = parseFloat(d3.select(this).attr("x")) + xBarScale.bandwidth()*.25 + 70;
//         var yPosition = d3.mouse(this)[1]+dimensions.bar.boundedTop;
//
//         d3.select("#bar-tooltip").classed("hidden", false)
//
//         d3.select("#bar-tooltip")
//           .style("left", xPosition + "px")
//           .style("top", yPosition + "px")
//           .select("#protest-count")
//           .text(yAccessor(d))
//
//         d3.select("#week-bar-tooltip")
//           .text(d.week)
//
//
//       }
//
//       const mouseMove = function(d){
//         var xPosition = parseFloat(d3.select(this).attr("x")) + xBarScale.bandwidth()*.25 + 70;
//         var yPosition = d3.mouse(this)[1]+dimensions.bar.boundedTop;
//
//         // d3.select("#bar-tooltip")
//         // .style("left", xPosition + "px")
//         // .style("top", yPosition + "px")
//
//       }
//
//       const mouseOut = function(d){
//         d3.select("#bar-tooltip")
//           .classed("hidden", true)
//       }
//
//       const bar = bounds_bar.selectAll("rect")
//         .data(dataset_bar)
//       .enter()
//         .append("rect")
//         .attr("x", function(d, i){
//           return xBarScale(i);
//         })
//         .attr("y", d => dimensions.bar.boundedHeight - yBarScale(yAccessor(d)))
//         .attr("width", xBarScale.bandwidth() - barPadding)
//         .attr("height", d => yBarScale(yAccessor(d)))
//         .attr("id", function(d){return "wk" + d.week_id})
//         .on("mouseover", mouseOver)
//         .on("mousemove", mouseMove)
//         .on("mouseout", mouseOut)
//
//
//     //Add peripherals
//       const xAxisScale = d3.scaleTime()
//           .domain(d3.extent(dataset_bar, xAccessor))
//           .range([0, dimensions.bar.boundedWidth])
//
//         const xAxisGenerator = d3.axisBottom()
//             .scale(xAxisScale)
//
//        const xAxis = bounds_bar.append("g")
//             .call(xAxisGenerator)
//             .style("transform", `translateY(${dimensions.bar.boundedHeight}px)`)
//             .attr("id", "bar-x-axis")
//
//
//       const yAxisScale = d3.scaleLinear()
//         .domain(d3.extent(dataset_bar, yAccessor))
//         .range([dimensions.bar.boundedHeight, 0])
//         .nice()
//
//       const yAxisGenerator = d3.axisLeft()
//         .scale(yAxisScale)
//
//       const yAxis = bounds_bar.append("g")
//         .call(yAxisGenerator)
//         .attr("id", "bar-y-axis")
//
//       // const yAxisLabel = yAxis.append("text")
//       //   .attr("y", 0)
//       //   .attr("x", dimensions.bar.margin.right)
//       //   .attr("fill", "black")
//       //   .style("font-size", "1.4em")
//       //   .text("Protests per week")
//       //   .style("text-anchor", "start")
//
//
//     // const betweenChartsWeekLabel = bounds_bar.append("text")
//     //   .attr("y", 0)
//     //   //why does THIS of all things work to center the week label? WHO KNOWS
//     //   .attr("x", dimensions.bar.boundedWidth/2 - dimensions.bar.margin.left*2)
//     //     .attr("fill", "black")
//     //   .text("Protests during the week of ")
//     //   .style("text-anchor", "start")
//     //   .style("text-align", "center")
//     //   .attr("id", "week-tooltip")
//     //   .append("span")
//     //     .attr("id", "week-loop")
//     //     .attr("fill", "black")
//
//
//     //
//     // const betweenChartsWeekLabel = d3.select("#week-tooltip")
//     //   .style("transform", `translate(${
//     //     0
//     //   }px, ${
//     //     dimensions.bar.boundedTop
//     //   }px)`)
//
//
//
//
//
//
//     //     let wkTooltipTop = dimensions.bar.boundedTop
//     //
//     // const positionWeekTooltip = d3.select("#week-tooltip")
//     //   .attr("position", "absolute")
//     //   .attr("top", wkTooltipTop)
//
//
//
//     }
//
//     drawBar()
//
//
//
// function drawProtestLoop(data){
//
//   let section_map = data.filter(function(d) {return d.week_id == wk})
//   let weekSelector = "#wk"+wk
//
//   const updateLabel = function(d){
//
//         d3.select("#week-tooltip-text")
//           .data(section_map)
//           .classed("hidden", false)
//           .select("#week-loop")
//           .text(d => d.week)
//         }
//
//   updateLabel()
//
//   d3.select(weekSelector)
//       .attr("fill", "white")
//
//   setTimeout(()=> {
//         d3.select(weekSelector)
//           .attr("fill", "darkgray")
//       }, 1000)
//
//
//   const dots = bounds_map_stars.selectAll("circle")
//                 .data(section_map)
//   dots
//   .enter().append("circle")
//     .merge(dots)
//       .attr("cx", d => projection([latAccessor(d), longAccessor(d)])[0])
//       .attr("cy", d => projection([latAccessor(d), longAccessor(d)])[1])
//       .attr("fill", "darkgray")
//       .attr("r", 1)
//       .attr("opacity", .2)
//       .transition().duration(500)
//         .attr("fill", "white")
//         .attr("r", 2)
//         .attr("opacity", 1)
//       .transition().duration(500)
//         .attr("fill", "darkgray")
//         .attr("r", 1)
//         .attr("opacity", .2)
//
//   dots.exit()
//       .remove()
//
//     }
//
// var play = true
//
// var playButton = document.querySelector("#play-button")
//
// playButton.addEventListener("click", function(){
//
//   event.preventDefault()
//   play = !play
//   console.log(play)
// }, false)
//
// let protestCycle = function(){for (let counter = 1; counter <= weekMax; counter = counter+1) {
//     if(play == true){
//       setTimeout(()=> {
//         wk = wk+1
//         drawProtestLoop(dataset_map)
//     //    console.log("counter: " + counter + " week: " + wk)
//       }, 1000*counter)
//     }
//   }
// }
//
// for(let rep = 0; rep <=5; rep = rep+1){
//   setTimeout(()=> {
//     protestCycle()
//     wk = 0
//   }, 1000*(weekMax+1)*rep)
// }
//
// }
//
// drawMapAndBar()
