function sequence(){

  let dimensions = {
    width: window.innerWidth * 0.9,
    height: window.innerWidth * 0.9/4,
    margin: {
      top: 10,
      right: 10,
      bottom: 10,
      left: 10,
     },
    }

  let start = [1, 2, 3, 4, 5, 6];


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


  const ball = bounds.append("circle")
    .attr("cx", start[0])
    .attr("cy", dimensions.height/2)
    .attr("fill", "purple")
    .attr("r", 5)

  while(start <= wrapper.width){
    start = start + 10
    ball.cx = start
  }

  console.log(start)

}

sequence()
