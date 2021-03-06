

//This is a templet I got from  https://bl.ocks.org/mbostock made by Mike Bostock
// General Public License
var svg = d3.select("svg"),
    width = +svg.attr("width"),
    height = +svg.attr("height");
var tempcolor;
var social_capital = d3.map();//hold key value pairs which is our data

var path = d3.geoPath();

var x = d3.scaleLinear() //this linear method is to scale the legend
    .domain([90, 100])//1,10
    .rangeRound([700, 830]); //ORIGINAL 800 860

var color = d3.scaleThreshold() //continues to discgrete mapping
    .domain(d3.range(92, 100))//set color change here
    .range(d3.schemeBlues[9]);

var g = svg.append("g") //create a container contain the bar which is used for legend
    .attr("class", "key")
    .attr("transform", "translate(0,40)");

g.selectAll("rect") // bar chart
  .data(color.range().map(function (d) {
      d = color.invertExtent(d); // Returns the extent of values in the domain
      if (d[0] == null) d[0] = x.domain()[0]; //use linear method to scale
      if (d[1] == null) d[1] = x.domain()[1];
      return d;
  }))//data
  .enter().append("rect")
    .attr("height", 12) //index bar height
    .attr("x", function (d) { return x(d[0]); })
    .attr("width", function (d) { return x(d[1]) - x(d[0]); })
    .attr("fill", function (d) { return color(d[0]); });

g.append("text")
    .attr("class", "caption")
    .attr("x", x.range()[0])
    .attr("y", -6)
    .attr("fill", "#000")
    .attr("text-anchor", "start")
    .attr("font-weight", "bold")
    .text("Employment Rate"); //the legend scale

g.call(d3.axisBottom(x)
    .tickSize(13)
    .tickFormat(function (x, i) { return i ? x : x ; })
    .tickValues(color.domain()))
  .select(".domain")
    .remove();


//you control how many tasks run at the same time. When all the tasks complete, or an error occurs,
//the queue passes the results to your await callback.
//initialize the map with data 2009
d3.queue()
   .defer(d3.json, "https://d3js.org/us-10m.v1.json")
   .defer(d3.tsv, "../output/rate_2009.tsv", function (d) { social_capital.set(d.id, +d.rate); }) //read the data and store in social_capital(set)
   .await(ready);

function ready(error, us) {
    if (error) throw error;
    // svg.selectAll("g").remove();

    svg.append("g") //g is a container
      .attr("class", "counties")
      .selectAll("path")
      .data(topojson.feature(us, us.objects.counties).features)//topojson format  geometry or feature object
      // Returns the GeoJSON Feature topojson.feature(topology, object)
      //it seems that some county are missing
      .enter().append("path")
        .attr("fill", function (d) { return color(d.rate = social_capital.get(d.id)); })//fill the color by data
        //if undefinced then
        .attr("d", path)
        .on("mouseover",function(d){
          tempcolor=this.style.fill
          d3.select(this)
            .style("opacity",0.8)
            .style("fill","red")
        })
        .on("mouseout",function(d){
          d3.select(this)
            .style("opacity",1)
            .style("fill",tempcolor)
        })//d attribute means the shape is a series of command of movement
      .append("title")
        .text(function (d) { return "value:"+ d.rate+" code: "+ d.id; })
      .tran
        ;//this return the mousemove.on text

    svg.append("path")
        .datum(topojson.mesh(us, us.objects.states, function (a, b) { return a !== b; }))
        .attr("class", "states")//stroke line with state
        .attr("d", path);

    svg.append("text")
          .attr("class", "caption")
          .attr("x", 150)
          .attr("y", 30)
          .attr("fill", "#000")
          .attr("font-size","20")
          .attr("text-anchor", "start")
          .attr("font-weight", "bold")
          .text("Employment rate by County");//if not found then undefined

}


function UpdateData14(){

  d3.queue()
     .defer(d3.json, "https://d3js.org/us-10m.v1.json")
     .defer(d3.tsv, "../output/drawemployment.tsv", function (d) { social_capital.set(d.id, d.rate); }) //read the data and store in social_capital(set)
     .await(ready);


}
function UpdateData09(){
  d3.queue()
     .defer(d3.json, "https://d3js.org/us-10m.v1.json")
     .defer(d3.tsv, "../output/rate_2009.tsv", function (d) { social_capital.set(d.id, d.rate); }) //read the data and store in social_capital(set)
     .await(ready);
}

function UpdateData05(){
  d3.queue()
     .defer(d3.json, "https://d3js.org/us-10m.v1.json")
     .defer(d3.tsv, "../output/rate_2005.tsv", function (d) { social_capital.set(d.id, d.rate); }) //read the data and store in social_capital(set)
     .await(ready);
}
function UpdateData97(){
  d3.queue()
     .defer(d3.json, "https://d3js.org/us-10m.v1.json")
     .defer(d3.tsv, "../output/rate_1997.tsv", function (d) { social_capital.set(d.id, d.rate); }) //read the data and store in social_capital(set)
     .await(ready);
}
