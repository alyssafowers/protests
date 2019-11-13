async function drawMap() {

  //get data

  // for some reason this absolutely WILL NOT work with the original lat_long data,
  // but does work for data in the additional_tags csv???? even though I copy/pasted
  // the values over and it's the exact same shit? also won't work if I call the
  // additional_tags columns latitude and longitude. however, if I call the
  // protest_lat_long columns internal_point_longitude etc, that doesn't fix the problem.
  // WHAT DO, D3

  //for date slider, start with this: https://bl.ocks.org/officeofjane/47d2b0bfeecfcb41d2212d06d095c763 and if that doesn't work
  //try spiral example here for play/pause/restart buttons? https://observablehq.com/@palewire/svg-path-animations-d3-transition


  const stateShapes = await d3.json("us-states.json")
  const dataset = await d3.csv("protest_additional_tags_2.csv")
  const dataset_2 = dataset

  const latAccessor = d => +d.internal_point_latitude
  const longAccessor = d => +d.internal_point_longitude
  const weekIdAccessor = d => +d.week_id

  let weekMax = d3.max(dataset, function(d) { return +d.week_id;} );

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

    const states = bounds.selectAll(".state")
      .data(stateShapes.features)
      .enter().append("path")
        .attr("class", "state")
        .attr("d", pathGenerator)
        .attr("fill", "black")

    let wk = 0

function drawProtestLoop(data){

  let section = data.filter(function(d) {return d.week_id == wk})

  const dots = bounds.selectAll("circle")
                .data(section)
  dots
  .enter().append("circle")
    .merge(dots)
      .attr("cx", d => projection([latAccessor(d), longAccessor(d)])[0])
      .attr("cy", d => projection([latAccessor(d), longAccessor(d)])[1])
      .attr("fill", "black")
      .attr("r", 1)
      .attr("opacity", .7)
      .transition().duration(500)
        .attr("fill", "white")
        .attr("r", 2)
      .transition().duration(500)
        .attr("fill", "black")
        .attr("r", 1)

  dots.exit()
      .remove()

    dots.exit().remove()
  }

let protestCycle = function(){for (let counter = 1; counter <= weekMax; counter = counter+1) {
    setTimeout(()=> {
      wk = wk+1
      drawProtestLoop(dataset)
      console.log("counter: " + counter + " week: " + wk)
    }, 1000*counter)
  }
}

console.log("protestCycle defined")

for(let rep = 0; rep <=5; rep = rep+1){
  setTimeout(()=> {
    console.log("rep: "+rep)
    protestCycle()
    wk = 0
  }, 1000*(weekMax+1)*rep)
}

  //
  // setTimeout(()=> {
  //   bounds.selectAll("circle")
  //     .data(dataset)
  //     .enter().append("circle")
  //     .attr("cx", d => projection([latAccessor(d), longAccessor(d)])[0])
  //     .attr("cy", d => projection([latAccessor(d), longAccessor(d)])[1])
  //     .attr("fill", "black")
  //     .attr("r", 1)
  //     .attr("opacity", .3)
  //     .transition().duration(500)
  //       .attr("fill", "white")
  //   }, 3500)








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