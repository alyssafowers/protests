async function oneFunctionToRuleThemAll(){

  const dataset_const_segment = await d3.csv("all_constellation_lines.csv")
  const dataset_const_points = await d3.csv("all_constellation_points.csv")
  const dataset_all_points = await d3.csv("all_protests_major_tags.csv")

  async function stateDraw(){
    const stateShapes = await d3.json("us-states.json")
    const width = 600
    // const width = window.innerWidth*.5

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


  }

  async function constellationDraw(focus){

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


    const width = 600

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
      .attr("fill", "darkgray")
      .attr("r", 1)
      .attr("opacity", .2)

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
      .attr("opacity", function(d){return opacityScale(pairAccessorWeight(d))})


    //Would prefer to have the connections as arcs/curves instead of straight lines,
    //but trying to get it that way is a time sink. Try again later.

    // const constLines = bounds.selectAll("path")
    //   .data(dataset_const_segment)
    //   .enter()
    //   .append("path")
    //   .attr("d", "C " + (d => projection([pairAccessorLat1(d), pairAccessorLong1(d)])[0]) + " " + (d => projection([pairAccessorLat1(d), pairAccessorLong1(d)])[1]) + ", " + (d => projection([pairAccessorLat2(d), pairAccessorLong2(d)])[0]) + " " + (d => projection([pairAccessorLat2(d), pairAccessorLong2(d)])[1]), ", 10 10")
    //   .attr("stroke", "white")
    //   .attr("stroke-width", function(d){return weightScale(pairAccessorWeight(d))})
    //   .attr("opacity", function(d){return weightScale(pairAccessorWeight(d))})


    const constPlaces = boundsConst.selectAll("circle")
      .data(section_const_points)
      .join("circle")
      .attr("cx", d => projection([pointLatAccessor(d), pointLongAccessor(d)])[0])
      .attr("cy", d => projection([pointLatAccessor(d), pointLongAccessor(d)])[1])
      .attr("fill", "white")
      .attr("r", function(d){ return circleScale(pointSizeAccessor(d))})
      .attr("opacity", 1)
      .on("mouseover",mouseOver)
      .on("mousemove",mouseMove)
      .on("mouseout", mouseOut)

  }

  async function chooseConstellation(){
    stateDraw()

    const dataset_const_segment = await d3.csv("all_constellation_lines.csv")
    const dataset_const_points = await d3.csv("all_constellation_points.csv")
    const dataset_all_points = await d3.csv("all_protests_major_tags.csv")

    constellationDraw("guns")



    var constSelect = document.querySelector("#constellation-selector")

    constSelect.addEventListener("click", function(){

      event.preventDefault()
      focus = event.target.id
      console.log
      constellationDraw(focus)
    }, false)


  }

   chooseConstellation()
}

oneFunctionToRuleThemAll()
