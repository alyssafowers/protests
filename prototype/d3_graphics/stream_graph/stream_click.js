//on click, fade out other parts of the streamgraph, and have the clicked part
//drop down to become an area chart?

function streamClick(){

  const clickStream = function(d){
    d3.select("#gun-stream")
      .style("display", "none")

    d3.select("#gun-area")
      .style("display", "initial")
    }

    const clickArea = function(d){
      d3.select("#gun-area")
        .style("display", "none")

      d3.select("#gun-stream")
        .style("display", "initial")
      }

    d3.select("#gun-stream")
      .on("click", clickStream)

      d3.select("#gun-area")
        .on("click", clickArea)


  }


streamClick()
