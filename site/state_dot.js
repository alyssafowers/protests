//not sure if axis is where it needs to be

async function stateDot(){

  var topics = ["guns",
    "immigration",
    "women",
    "supreme_court",
    "environment",
    "education",
    "collective_bargaining",
    "executive",
    "healthcare",
    "police",
    "race_confed",
    "other"]

  var topicsFull = {
    guns: {
      lineLabel: "Guns",
      captionLabel: "guns"
    },
    immigration: {
      lineLabel: "Immigration",
      captionLabel: "immigration"
    },
    women : {
      lineLabel: "Women's rights",
      captionLabel: "women's rights"
    },
    supreme_court: {
      lineLabel: "Supreme Court",
      captionLabel: "the Supreme Court"
    },
    other: {
      lineLabel: "Other topics",
      captionLabel: "other topics"
    },
    environment: {
      lineLabel: "Environment",
      captionLabel: "the environment"
    },
    education: {
      lineLabel: "Education",
      captionLabel: "education"
    },
    collective_bargaining: {
      lineLabel: "Collective bargaining",
      captionLabel: "collective bargaining"
    },
    executive: {
      lineLabel: "Executive Branch",
      captionLabel: "the Executive Branch"
    },
    healthcare: {
      lineLabel: "Healthcare",
      captionLabel: "healthcare"
    },
    police: {
      lineLabel: "Police",
      captionLabel: "the police"
    },
    race_confed: {
      //this should be "racial justice or white supremacy," but struggling to get a line break in there
      lineLabel: "Racial justice",
      captionLabel: "racial justice or white supremacy"
    }
  }

  const fillColors = {
    supreme_court: "rgb(95, 154, 102)",
    women: "rgb(35, 77, 78)",
    police: "rgb(65, 163, 192)",
    education: "rgb(51, 143, 163)",
    executive: "rgb(184, 195, 226)",
    immigration: "rgb(72, 124, 188)",
    guns: "rgb(107, 69, 152)",
    race_confed: "rgb(226, 136, 183)",
    collective_bargaining: "rgb(178, 37, 113)",
    environment: "rgb(91, 61, 93)",
    healthcare: "rgb(173, 96, 162)",
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
    race_confed: "black",
    collective_bargaining: "white",
    environment: "white",
    healthcare: "white",
    other: "white"
  }

  const stateNameFull = { AZ: 'Arizona',
    AL: 'Alabama',
    AK: 'Alaska',
    AR: 'Arkansas',
    CA: 'California',
    CO: 'Colorado',
    CT: 'Connecticut',
    DC: 'Washington, DC',
    DE: 'Delaware',
    FL: 'Florida',
    GA: 'Georgia',
    HI: 'Hawaii',
    ID: 'Idaho',
    IL: 'Illinois',
    IN: 'Indiana',
    IA: 'Iowa',
    KS: 'Kansas',
    KY: 'Kentucky',
    LA: 'Louisiana',
    ME: 'Maine',
    MD: 'Maryland',
    MA: 'Massachusetts',
    MI: 'Michigan',
    MN: 'Minnesota',
    MS: 'Mississippi',
    MO: 'Missouri',
    MT: 'Montana',
    NE: 'Nebraska',
    NV: 'Nevada',
    NH: 'New Hampshire',
    NJ: 'New Jersey',
    NM: 'New Mexico',
    NY: 'New York',
    NC: 'North Carolina',
    ND: 'North Dakota',
    OH: 'Ohio',
    OK: 'Oklahoma',
    OR: 'Oregon',
    PA: 'Pennsylvania',
    RI: 'Rhode Island',
    SC: 'South Carolina',
    SD: 'South Dakota',
    TN: 'Tennessee',
    TX: 'Texas',
    UT: 'Utah',
    VT: 'Vermont',
    VA: 'Virginia',
    WA: 'Washington State',
    WV: 'West Virginia',
    WI: 'Wisconsin',
    WY: 'Wyoming' }

    const dataset = await d3.csv("topic_percentile_by_state.csv")
    const xAccessor = d => +d.perc_of_state_protest
    const stateAccessor = d => d.state

    const highPercentile = dataset.filter(function(d) {return +d.percentile > .8})
    const lowPercentile = dataset.filter(function(d) {return d.percentile < .25})

    const width = d3.min([window.innerWidth, 800])
    const height = width*1.25

    const dimensions = {
    width: width,
    margin: {
      top: 30,
      bottom: 60,
      left: 180,
      right: 20,
    },
    dotHeight: 10,
    dotMargin: 15
    }


  dimensions.boundedHeight = (dimensions.dotHeight*(topics.length+1))+(dimensions.dotMargin*(topics.length+4))

  dimensions.height = dimensions.boundedHeight+dimensions.margin.top+dimensions.margin.bottom

  dimensions.boundedWidth = dimensions.width
   -  dimensions.margin.left - dimensions.margin.right

   const wrapper = d3.select("#state-dotplot")

   const dotWrapper = wrapper.append("svg")
    .attr("width", dimensions.width)
    .attr("height", dimensions.height)
    .attr("id", "dotWrapper")

    const dotBounds = dotWrapper.append("g")
      .attr("id", "dotBounds")
      .style("transform", `translate(${
          dimensions.margin.left
        }px, ${
          dimensions.margin.top
        }px)`)

        const labelBounds = dotWrapper.append("g")
          .attr("id", "labelBounds")
          .style("transform", `translate(${
              0
            }px, ${
              dimensions.margin.top
            }px)`)


        const xScale = d3.scaleLinear()
          .domain(d3.extent(dataset, xAccessor))
          .range([0,dimensions.boundedWidth])
          .nice()

          var formatPercent = d3.format(".0%")

          const xAxisGenerator = d3.axisBottom()
              .scale(xScale)
              .tickFormat(formatPercent)

         const xAxis = dotWrapper.append("g")

         xAxis.call(xAxisGenerator)
              .style("transform", `translate(${
                  dimensions.margin.left
                }px, ${
                  dimensions.boundedHeight + dimensions.dotHeight+dimensions.dotMargin
                }px)`)

         const xAxisLabel = xAxis.append("text")
               .attr("y",50)
               .attr("x", dimensions.boundedWidth/2)
               .attr("fill", "black")
               .style("text-anchor", "middle")
               .text("Percent of protests in each state about topic")


        // const xAxisLabel = dotWrapper.append("text")
        //   .text("Percent of protests about topic in state")


    async function drawDots(topicName){

      const topicBounds = dotBounds.append("g")
        .attr("id", topicName)

      const section = dataset.filter(function(d) {return d.topic == topicName})

      const mouseOverStateDot = function(d){
        //reference for mouse position came from here: https://www.d3-graph-gallery.com/graph/interactivity_tooltip.html

        // var xPosition = parseFloat(d3.select(this).attr("cx"))+dimensions.margin.left*2
        // var yMousePosition = d3.mouse(this)[1]+dimensions.margin.top+(dimensions.dotMargin*13)
        var xPosition = parseFloat(d3.select(this).attr("cx"))
        var yMousePosition = d3.mouse(this)[1]-dimensions.dotHeight

        d3.select("#state-dot-tooltip").classed("hidden", false)

        d3.select("#state-dot-tooltip")
          .style("left", xPosition + "px")
          .style("top", yMousePosition + "px")
          .select("#state-percentile")
          .text(formatPercent(xAccessor(d)))
        d3.select("#tooltip-state")
          .text(stateNameFull[stateAccessor(d)])

          d3.select("#dot-topic")
          .text(topicsFull[topicName].captionLabel)
      }

      const mouseMoveStateDot = function(d){
        // var xPosition = parseFloat(d3.select(this).attr("cx"))+dimensions.margin.left*2

        var yPosition = d3.mouse(this)[1]+dimensions.margin.top-dimensions.dotHeight

        var xPosition = parseFloat(d3.select(this).attr("cx"))
        // var yMousePosition = d3.mouse(this)[1]

        d3.select("#state-dot-tooltip")
        .style("left", xPosition + "px")
        .style("top", yPosition + "px")
        .select("#state-percentile")
        .text(formatPercent(xAccessor(d)))
        .select("#tooltip-state")
        .text(stateNameFull[stateAccessor(d)])
        .select("#dot-topic")
        .text(topicName)

      }

      const mouseOutStateDot = function(d){
        d3.select("#state-dot-tooltip")
          .classed("hidden", true)
      }

      const guideLine = topicBounds.append("line")
      .attr("x1", 0)
      .attr("x2", dimensions.boundedWidth)
        .attr("y1", yPosition)
        .attr("y2", yPosition)
        .style("stroke", "lightgray")
        .style("stroke-width", ".5")

        //draw dataset

        var fillColor = fillColors[topicName]

        const dots = topicBounds.selectAll("circle")
          .data(section)
            .enter()
              .append("circle")
              .attr("cx", d=> xScale(xAccessor(d)))
              .attr("cy", yPosition)
              .attr("r", dimensions.dotHeight/2)
              .attr("opacity", ".5")
              .attr("class", d => stateAccessor(d))
              .attr("fill", fillColor)
              .on("mouseover", mouseOverStateDot)
              .on("mousemove", mouseMoveStateDot)
              .on("mouseout", mouseOutStateDot)


        //add label

        var thisLineLabel = topicsFull[topicName].lineLabel


        const lineLabel = labelBounds.append("text")
            .text(thisLineLabel)
            .attr("fill", "black")
            .attr("x", dimensions.margin.left - 10)
            .attr("y", yPosition+dimensions.dotMargin/4)
            .attr("text-anchor", "end")

        //got a little help from https://stackoverflow.com/questions/56226717/return-value-based-on-max-min-of-another-value-in-the-same-object
        //if this doesn't work later, remember to include this in the index file: <script src="https://d3js.org/d3-array.v2.min.js"></script>

        var topPerc = d3.max(section, d => d.perc_of_state_protest)
        var topPercIndex = d3.maxIndex(section, d => d.perc_of_state_protest)
        var topPercState = section[topPercIndex].state

    //this would add a label to the dot for the state with the highest percentila, but ultimately found it distracting & removed

      // const highestState = topicBounds.append("text")
      //   .text(topPercState)
      //   .attr("fill", "black")
      //   .attr("x", xScale(topPerc)+dimensions.dotHeight)
      //   .attr("y", yPosition)
      //   .attr("class", "highestState")


      }

    var yPosition
    var lowestTopic
    //Actually draw the chart:

  for(i = 0; i < topics.length; i++){
      yPosition = 2 + (i*2*dimensions.dotMargin)
      drawDots(topics[i])
    }

    //selecting Florida for initial page load

    var selectedStateClass = ".FL"

    d3.selectAll(".FL")
      .attr("opacity", 1)
      .attr("r", dimensions.dotHeight)
      .attr("stroke", "white")
      .raise()

    //Behavior when state is selected:

    var selectedState
    var selectedStateDots

    d3.select("#stateSelect").on("change", function(){

      d3.selectAll(selectedStateClass)
        .lower()
        .attr("opacity", ".5")
        .attr("r", dimensions.dotHeight/2)
        .attr("stroke","null")

      selectedState = d3.select(this).property("value")
      selectedStateClass = "."+selectedState

      d3.select("#state-sentence")
        .classed("hidden", false)

    selectedStateClass = "."+selectedState

    selectedStateDots = d3.selectAll(selectedStateClass)
        .attr("opacity", 1)
        .attr("r", dimensions.dotHeight)
        .attr("stroke", "white")
        .raise()

        d3.select("#state-name")
          .text(stateNameFull[selectedState])

    d3.selectAll(".highestState").classed("hidden", true)

    var highPercentileThisState = highPercentile.filter(function(d) {return d.state == selectedState})

    var topTopicOne = highPercentileThisState[0].topic

    if (highPercentileThisState.length <2){
      d3.select("#top-protest-topic-one")
        .text(topicsFull[topTopicOne].captionLabel)
        .style("background-color", fillColors[highPercentileThisState[0].topic])
        .style("color", textColors[highPercentileThisState[0].topic])

      d3.select("#top-protest-topic-bridge")
        .classed("hidden", true)

      d3.select("#top-protest-topic-two")
        .classed("hidden", true)
    } else {
      var topTopicTwo = highPercentileThisState[1].topic

      d3.select("#top-protest-topic-one")
        .text(topicsFull[topTopicOne].captionLabel)
        .style("background-color", fillColors[highPercentileThisState[0].topic])
        .style("color", textColors[highPercentileThisState[0].topic])

      d3.select("#top-protest-topic-two")
        .text(topicsFull[topTopicTwo].captionLabel)
        .style("background-color", fillColors[highPercentileThisState[1].topic])
        .style("color", textColors[highPercentileThisState[1].topic])

      d3.select("#top-protest-topic-bridge")
        .classed("hidden", false)

      d3.select("#top-protest-topic-two")
        .classed("hidden", false)
    }

    var lowPercentileThisState = lowPercentile.filter(function(d) {return d.state == selectedState})
          .sort(function(x){return d3.ascending(x.percentile)})

    var bottomTopic = lowPercentileThisState.length - 1
    var secondBottomTopic = lowPercentileThisState.length - 2

    var bottomTopicOne = lowPercentileThisState[bottomTopic].topic

    if (lowPercentileThisState.length < 2){
      d3.select("#bottom-protest-topic-one")
        .text(topicsFull[bottomTopicOne].captionLabel)
        .style("background-color", fillColors[lowPercentileThisState[0].topic])
        .style("color", textColors[lowPercentileThisState[0].topic])

      d3.select("#bottom-protest-topic-bridge")
        .classed("hidden", true)

      d3.select("#bottom-protest-topic-two")
        .classed("hidden", true)
    } else {
      var bottomTopicTwo = lowPercentileThisState[secondBottomTopic].topic

      d3.select("#bottom-protest-topic-one")
        .text(topicsFull[bottomTopicOne].captionLabel)
        .style("background-color", fillColors[lowPercentileThisState[bottomTopic].topic])
        .style("color", textColors[lowPercentileThisState[bottomTopic].topic])

      d3.select("#bottom-protest-topic-two")
        .text(topicsFull[bottomTopicTwo].captionLabel)
        .style("background-color", fillColors[lowPercentileThisState[secondBottomTopic].topic])
        .style("color", textColors[lowPercentileThisState[secondBottomTopic].topic])

      d3.select("#bottom-protest-topic-bridge")
        .classed("hidden", false)

      d3.select("#bottom-protest-topic-two")
        .classed("hidden", false)
    }


    })


    }




stateDot()
