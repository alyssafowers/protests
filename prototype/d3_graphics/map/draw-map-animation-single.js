async function drawMap() {

  //get data

  // for some reason this absolutely WILL NOT work with the original lat_long data,
  // but does work for data in the additional_tags csv???? even though I copy/pasted
  // the values over and it's the exact same shit? also won't work if I call the
  // additional_tags columns latitude and longitude. however, if I call the
  // protest_lat_long columns internal_point_longitude etc, that doesn't fix the problem.
  // WHAT DO, D3

  //also I suspect hawaii isn't mapping correctly? why are all the Protests
  //in the exact same spot

  const stateShapes = await d3.json("us-states.json")
  const dataset = await d3.csv("protest_additional_tags_2.csv")

  const latAccessor = d => +d.internal_point_latitude
  const longAccessor = d => +d.internal_point_longitude
  const weekIdAccessor = d => +d.week_id


  console.log(dataset)
  console.log(latAccessor(dataset)[0])

  //const thisWeek = d => filter(weekIdAccessor(d) == 1)
//  console.log(thisWeek(dataset))


  //set chart dimensions

  let dimensions = {
    width: window.innerWidth * 0.9,
    margin: {
      top: 10,
      right: 10,
      bottom: 10,
      left: 10,
    },
  }

  console.log(dataset.length)

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

    //draw canvas

    const wrapper = d3.select("#wrapper")
      .append("svg")
        .attr("width", dimensions.width)
        .attr("height", dimensions.height)

    const bounds = wrapper.append("g")
      .style("transform", `translate(${
        dimensions.margin.left
      }px, ${
        dimensions.margin.top
      }px)`)

    // create scales

    //draw data

    const earth = bounds.append("path")
      .attr("class", "earth")
      .attr("d", pathGenerator(sphere))

    const states = bounds.selectAll(".state")
      .data(stateShapes.features)
      .enter().append("path")
        .attr("class", "state")
        .attr("d", pathGenerator)

    let wk = 2

function drawProtest(data){

  let section = data.filter(function(d) {return d.week_id == wk})

  const dots = bounds.selectAll("circle")
                .data(section)
  dots
  .enter().append("circle")
    .merge(dots)
      .attr("cx", d => projection([latAccessor(d), longAccessor(d)])[0])
      .attr("cy", d => projection([latAccessor(d), longAccessor(d)])[1])
      .attr("fill", "black")
      .attr("r", 2)
      .transition().duration(600)
        .attr("fill", "white")
      .transition().duration(600)
        .attr("fill", "black")
    // .attr("opacity", .3)

  }

  drawProtest(dataset)

  wk = 4

  setTimeout(() => {
      drawProtest(dataset)
    }, 1000)

    wk = 10

    setTimeout(() => {
        drawProtest(dataset)
      }, 2000)



    // const protest = bounds.selectAll("circle")
    //   .data(dataset)
    //   .enter()
    //   .append("circle")
    //   .filter(function(d) {return d.week_id == 2})
    //     .attr("cx", d => projection([latAccessor(d), longAccessor(d)])[0])
    //     .attr("cy", d => projection([latAccessor(d), longAccessor(d)])[1])
    //     .attr("fill", "white")
    //     .attr("r", 2)
    //     .attr("opacity", .3)


        //while loop that circles through like this: https://bl.ocks.org/d3noob/bf44061b1d443f455b3f857f82721372
        //for as long as play is true, have a button that reverses the value of play
        //whenever clicked

}

drawMap()
