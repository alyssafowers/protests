async function oneFunctionToRuleThemAll(){

  const dataset_const_segment = await d3.csv("all_constellation_lines.csv")
  const dataset_const_points = await d3.csv("all_constellation_points.csv")
  const dataset_all_points = await d3.csv("constellation_background_condensed.csv")

  async function stateDraw(){
    const stateShapes = await d3.json("us-states.json")
    if(window.innerWidth < 800){
        width = d3.max([375, window.innerWidth*.7]) - 30
      } else {
        width = d3.min([800, window.innerWidth*.7]) - 30
      }
    // const width = window.innerWidth*.9

    const dimensions = {
      width: width,
      margin: {
        top: 10,
        right: 10,
        bottom: 10,
        left: 10
      }
    }

    dimensions.boundedWidth = dimensions.width
      - dimensions.margin.left
      - dimensions.margin.right

    const sphere = ({type: "Sphere"})

    const projection = d3.geoAlbersUsa()
      .fitWidth(dimensions.boundedWidth, sphere)

    const pathGenerator = d3.geoPath(projection)
    const [[x0, y0], [x1, y1]] = pathGenerator.bounds(sphere)

    dimensions.boundedHeight = y1
    // dimensions.boundedHeight = 400;
    dimensions.height = dimensions.boundedHeight
      + dimensions.margin.top
      + dimensions.margin.bottom

    const wrapper = d3.select("#const-map-wrapper")
      .append("svg")
      .attr("width", dimensions.width)
      .attr("height", dimensions.height)
      .attr("id", "wrapper")

    const boundsMap = wrapper.append("g")
      .style("transform", `translate(${
        dimensions.margin.left
      }px, ${
        dimensions.margin.top
      }px)`)

    const states = boundsMap.selectAll(".state")
      .data(stateShapes.features)
      .enter().append("path")
        .attr("class", "state")
        .attr("d", pathGenerator)
        .attr("fill", "black")

    const topLeftAnnotationBounds = d3.select("#constellation-top-left-annotation").append("g")

    const topLeftAnnotation = topLeftAnnotationBounds.append("text")
      .text("this is where the top left annotation goes")
        .attr("fill", "black")

    const topRightAnnotationBounds = d3.select("#constellation-top-right-annotation").append("g")

    const topRightAnnotation = topRightAnnotationBounds.append("text")
      .text("this is where the top right annotation goes")
        .attr("fill", "black")

    const bottomLeftAnnotationBounds = d3.select("#constellation-bottom-left-annotation").append("g")

    const bottomLeftAnnotation = bottomLeftAnnotationBounds.append("text")
      .text("this is where the bottom left annotation goes")
        .attr("fill", "black")

    const bottomRightAnnotationBounds = d3.select("#constellation-bottom-right-annotation").append("g")

    const bottomRightAnnotation = bottomRightAnnotationBounds.append("text")
      .text("this is where the bottom right annotation goes")
        .attr("fill", "black")

    d3.select("#constellation-bottom-right-annotation").style("bottom", "200px")


  }

  async function constellationDraw(focus, name){

    //if the function has been called before, clearing out the g elements
    //so I can write a new constellation:

    var boundsBackgroundExists = document.getElementById("bounds-background")
    var boundsConstExists = document.getElementById("bounds-const")



    if(boundsBackgroundExists){

      boundsBackgroundExists.remove()
      boundsConstExists.remove()
    }


    const pairAccessorLong1 = d => +d.place_1_lat
    const pairAccessorLat1 = d => +d.place_1_long
    const pairAccessorLong2 = d => +d.place_2_lat
    const pairAccessorLat2 = d => +d.place_2_long
    const pairAccessorWeight = d => +d.count


    const pointLongAccessor = d => +d.internal_point_latitude
    const pointLatAccessor = d => +d.internal_point_longitude
    const pointSizeAccessor = d => +d.topic_protest
    const pointNameAccessor = d => d.location_name


    const backgroundLongAccessor = d => +d.longitude
    const backgroundLatAccessor = d => +d.latitude

    const section_const_segment = dataset_const_segment.filter(function(d) {return d.topic == focus})
    const section_const_points = dataset_const_points.filter(function(d) {return d.topic == focus})
    const section_all_points = dataset_all_points.filter(function(d) {return d[focus] == 1})

    const dimensions = {
      width: width,
      margin: {
        top: 10,
        right: 10,
        bottom: 10,
        left: 10
      }
    }

    dimensions.boundedWidth = dimensions.width
      - dimensions.margin.left
      - dimensions.margin.right

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

    const sphere = ({type: "Sphere"})

    const projection = d3.geoAlbersUsa()
      .fitWidth(dimensions.boundedWidth, sphere)

    const pathGenerator = d3.geoPath(projection)
    const [[x0, y0], [x1, y1]] = pathGenerator.bounds(sphere)

    dimensions.boundedHeight = y1
    // dimensions.boundedHeight = 430;
    dimensions.height = dimensions.boundedHeight
      + dimensions.margin.top
      + dimensions.margin.bottom

    const boundsBackground = d3.select("#wrapper").append("g")
      .style("transform", `translate(${
        dimensions.margin.left
      }px, ${
        dimensions.margin.top
      }px)`)
      .attr("id", "bounds-background")

      //separate g for constellation places so that I can have multiple
      //sets of circles:

      const boundsConst = d3.select("#wrapper").append("g")
        .style("transform", `translate(${
          dimensions.margin.left
        }px, ${
          dimensions.margin.top
        }px)`)
        .attr("id", "bounds-const")

    const circleScale = d3.scaleSqrt()
      .domain([1, 100])
      .range([1, 7])

    const weightScale = d3.scaleLog()
      .domain([1, 20])
      .range([.4, 2])

    const opacityScale = d3.scaleLinear()
      .domain([1, 20])
      .range([.5, .8])


    const mouseOver = function(d){
      var xPosition = d3.mouse(this)[0]
      var yPosition = d3.mouse(this)[1]

      d3.select("#const-map-tooltip").classed("hidden", false)

      d3.select("#const-map-tooltip")
        .style("left", xPosition + "px")
        .style("top", yPosition + "px")

        d3.select("#const-map-place")
        .text(pointNameAccessor(d))
    }

    const mouseMove = function(d){
      var xPosition = d3.mouse(this)[0]+15
      var yPosition = d3.mouse(this)[1]+15

      d3.select("#const-map-tooltip")
      .style("left", xPosition + "px")
      .style("top", yPosition + "px")

    }

    const mouseOut = function(d){
      d3.select("#const-map-tooltip")
        .classed("hidden", true)
    }

    const backgroundStars = boundsBackground.selectAll("circle")
      .data(section_all_points)
      .enter()
      .append("circle")
      .attr("cx", d => projection([backgroundLongAccessor(d), backgroundLatAccessor(d)])[0])
      .attr("cy", d => projection([backgroundLongAccessor(d), backgroundLatAccessor(d)])[1])
      .attr("fill", "gray")
      .attr("r", 1)
      .attr("opacity", 0)
      .transition()
        .duration(300)
        .attr("opacity", .8)


    //Would prefer to have the connections as arcs/curves instead of straight lines,
    //but trying to get it that way is a time sink. Try again later.


    const constPlaces = boundsConst.selectAll("circle")
      .data(section_const_points)
      .join("circle")
      .attr("cx", d => projection([pointLatAccessor(d), pointLongAccessor(d)])[0])
      .attr("cy", d => projection([pointLatAccessor(d), pointLongAccessor(d)])[1])
      .attr("fill", "white")
      .attr("r", function(d){ return circleScale(pointSizeAccessor(d))})
      .attr("opacity", 0)
      // .attr("opacity", 1)
      .on("mouseover",mouseOver)
      .on("mousemove",mouseMove)
      .on("mouseout", mouseOut)

      d3.select("#constellation-topic")
        .text(name)
        .style("background-color", fillColors[focus])
        .style("color", textColors[focus])
        .transition().duration(300)
          .style("opacity", "1")


      setTimeout(function(){
        constPlaces.transition().duration(300)
        .attr("opacity", 1)
      },200)

      setTimeout(function(){
        const constLines = boundsBackground.selectAll("line")
          .data(section_const_segment)
          .enter()
          .append("line")
          .attr("x1", d => projection([pairAccessorLat1(d), pairAccessorLong1(d)])[0])
          .attr("y1", d => projection([pairAccessorLat1(d), pairAccessorLong1(d)])[1])
          .attr("x2", d => projection([pairAccessorLat2(d), pairAccessorLong2(d)])[0])
          .attr("y2", d => projection([pairAccessorLat2(d), pairAccessorLong2(d)])[1])
          .attr("stroke", "white")
          .attr("stroke-width", function(d){return weightScale(pairAccessorWeight(d))})
          .attr("opacity", 0)
          .transition()
            .duration(200)
            .attr("opacity", function(d){return opacityScale(pairAccessorWeight(d))})
      },200)

      bottomRightAnnotationBounds


      bottomRightAnnotation



      d3.select("#constellation-bottom-right-annotation").style("bottom", "200px")

  }

  async function chooseConstellation(){
    stateDraw()

    const dataset_const_segment = await d3.csv("all_constellation_lines.csv")
    const dataset_const_points = await d3.csv("all_constellation_points.csv")
    const dataset_all_points = await d3.csv("all_protests_major_tags.csv")

    const place_annotation = await d3.csv("constellation_annotation.csv")


    constellationDraw("guns", "guns")

    var constSelect = document.querySelector("#constellation-selector")

    constSelect.addEventListener("click", function(){

      var starFade = d3.select("#wrapper").selectAll("circle")
        .transition()
        .duration(300)
          .attr("opacity", 0)

      var connectionFade = d3.select("#wrapper").selectAll("line")
      .transition()
      .duration(500)
        .attr("opacity", 0)

        d3.select("#constellation-topic")
          .transition().duration(300)
            .style("opacity", "0")


      event.preventDefault()
      focus = event.target.id
      console.log(event.target)
      name = event.target.attributes.label.nodeValue
      console.log(name)

      setTimeout(function(){
        constellationDraw(focus, name)
      },500)
    }, false)

  }

   chooseConstellation()
}

oneFunctionToRuleThemAll()
