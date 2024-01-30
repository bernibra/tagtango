#' stankeyNetwork_js
#'
#' @description A fct function to pass javascript code to the stankey network (there are probably better ways to do so).
#'
#' @return Returns js code for the stankey network.
#'
#' @noRd

stankeyNetwork_js <- function(links = TRUE){
    return(
        'function() {
            var nodes = d3.selectAll(".node");
            var links = d3.selectAll(".link");
            var colorbase = "rgb(248,241,222)";
            var color1 = "#D1E1D0";
            var color2 = "#E6D2AE";
            var color3 = "#DCD9BD";
            var lowopacity = 0.2;
            var highopacity = 0.55;

            var source = null;
            var target = null;

            var selection = {
              first_selection : false,
              second_selection : false,
              target1 : null,
              target2 : null,
              source1 : null,
              source2 : null,
              selected : function(source, target){
                if((this.source1 == source && this.target1 == target) || (this.source2 == source && this.target2 == target) ||
                   (this.source1 == source && !this.target1) || (!this.source2 && this.target2 == target) ||
                   (!this.source1 && this.target1 == target) || (this.source2 == source && !this.target2)){
                  return true;
                }else{
                  return false;
                }
              },
              overlap : function() {
                 if(this.first_selection && this.second_selection){
                  if (
                        (this.source1 == this.source2 && ( this.target1 == null || this.target2 == null ) ) ||
                        (this.target1 == this.target2 && ( this.source1 == null || this.source2 == null ) )
                  ){
                    return true;
                  }else{
                    return false;
                  }
                 }else{
                  return false;
                 }
              }
            }

            function clear(obj){
              obj.first_selection = false;
              obj.second_selection = false;
              obj.target1 = null;
              obj.target2 = null;
              obj.source1 = null;
              obj.source2 = null;
            }

            function selectlink(obj, source, target){
                if(obj.first_selection && obj.second_selection){
                  obj = clear(obj);
                  return false;
                }else if(obj.first_selection){
                  if(obj.target1 == target && obj.source1 == source){
                    obj = clear(obj);
                    return false;
                  }else{
                    obj.target2 = target;
                    obj.source2 = source;
                    obj.second_selection = true;
                    return true;
                  }
                }else{
                   obj.target1 = target;
                   obj.source1 = source;
                   obj.first_selection = true;
                   return true;
                }
            }

            Shiny.onInputChange("sankeyNetwork_1-target1", selection.target1);
            Shiny.onInputChange("sankeyNetwork_1-target2", selection.target2);
            Shiny.onInputChange("sankeyNetwork_1-source1", selection.source1);
            Shiny.onInputChange("sankeyNetwork_1-source2", selection.source2);

            nodes.on("mousedown.drag", null);

            nodes.style("opacity", 0.85).
              on("mouseover", function(d) {
                if(d.group == "source"){
                  links.filter(function(l){return (l.source.name == d.name);}).style("stroke-opacity", highopacity);
                }else{
                  links.filter(function(l){return (l.target.name == d.name);}).style("stroke-opacity", highopacity);
                }
              }).
              on("mouseout", function(d) {
                links.filter(function(k){return !selection.selected(k.source.name, k.target.name);})
                  .style("stroke-opacity", lowopacity);
              }).
              on(
                 "click",function(d) {
                         if(d.group == "source"){
                           if(selectlink(selection, d.name, null)){
                            if(selection.second_selection){
                              links.filter(function(k){return (k.source.name == d.name);}).
                                      style("stroke", function(i){
                                        return (i.target.name == selection.target1 ) ? color3 : color2;
                                              }).
                                      style("stroke-opacity", highopacity);


                              Shiny.onInputChange("sankeyNetwork_1-target2", selection.target2);
                              Shiny.onInputChange("sankeyNetwork_1-source2", selection.source2);
                              Shiny.onInputChange("sankeyNetwork_1-width", window.innerWidth);
                              Shiny.onInputChange("sankeyNetwork_1-height", window.innerHeight);

                            }else{
                                links.filter(function(k){return (k.source.name == d.name);}).
                                      style("stroke", color1).
                                      style("stroke-opacity", highopacity);

                                Shiny.onInputChange("sankeyNetwork_1-target1", selection.target1);
                                Shiny.onInputChange("sankeyNetwork_1-source1", selection.source1);
                                Shiny.onInputChange("sankeyNetwork_1-width", window.innerWidth);
                                Shiny.onInputChange("sankeyNetwork_1-height", window.innerHeight);
                            }
                          }else{
                            links.style("stroke",colorbase).style("stroke-opacity", lowopacity);

                            Shiny.onInputChange("sankeyNetwork_1-target1", selection.target1);
                            Shiny.onInputChange("sankeyNetwork_1-target2", selection.target2);
                            Shiny.onInputChange("sankeyNetwork_1-source1", selection.source1);
                            Shiny.onInputChange("sankeyNetwork_1-source2", selection.source2);
                            Shiny.onInputChange("sankeyNetwork_1-width", window.innerWidth);
                            Shiny.onInputChange("sankeyNetwork_1-height", window.innerHeight);

                          }
                         }else{
                           if(selectlink(selection, null, d.name)){
                            if(selection.second_selection){
                                links.filter(function(k){return (k.target.name == d.name);}).
                                      style("stroke", function(i){
                                        return (i.source.name == selection.source1) ? color3 : color2;
                                              }).
                                      style("stroke-opacity", highopacity);

                                Shiny.onInputChange("sankeyNetwork_1-target2", selection.target2);
                                Shiny.onInputChange("sankeyNetwork_1-source2", selection.source2);
                                Shiny.onInputChange("sankeyNetwork_1-width", window.innerWidth);
                                Shiny.onInputChange("sankeyNetwork_1-height", window.innerHeight);

                            }else{
                                links.filter(function(k){return (k.target.name == d.name);}).
                                      style("stroke", color1).style("stroke-opacity", highopacity);

                                Shiny.onInputChange("sankeyNetwork_1-target1", selection.target1);
                                Shiny.onInputChange("sankeyNetwork_1-source1", selection.source1);
                                Shiny.onInputChange("sankeyNetwork_1-width", window.innerWidth);
                                Shiny.onInputChange("sankeyNetwork_1-height", window.innerHeight);

                            }
                          }else{
                            links.style("stroke",colorbase).style("stroke-opacity", lowopacity);


                            Shiny.onInputChange("sankeyNetwork_1-target1", selection.target1);
                            Shiny.onInputChange("sankeyNetwork_1-target2", selection.target2);
                            Shiny.onInputChange("sankeyNetwork_1-source1", selection.source1);
                            Shiny.onInputChange("sankeyNetwork_1-source2", selection.source2);
                            Shiny.onInputChange("sankeyNetwork_1-width", window.innerWidth);
                            Shiny.onInputChange("sankeyNetwork_1-height", window.innerHeight);
                          }
                         }

             });



            links.style("stroke", colorbase).
              on("mouseover", function (d) {
                        d3.select(this).style("stroke-opacity", highopacity);
                      }).
              on("mouseout", function (d) {
                      d3.select(this).style("stroke-opacity", function(l){
                           return selection.selected(d.source.name, d.target.name) ? highopacity: lowopacity;
                         })
                      }).
              on("click",function(d) {

                                    if(selectlink(selection, d.source.name, d.target.name)){
                                      if(selection.second_selection){
                                        links.filter(function(k){return (k.source.name == d.source.name && k.target.name == d.target.name);}).
                                                style("stroke", function(i){return selection.overlap( ) ? color3 : color2;
                                              }).
                                              style("stroke-opacity", highopacity);

                                        Shiny.onInputChange("sankeyNetwork_1-target2", selection.target2);
                                        Shiny.onInputChange("sankeyNetwork_1-source2", selection.source2);
                                        Shiny.onInputChange("sankeyNetwork_1-width", window.innerWidth);
                                        Shiny.onInputChange("sankeyNetwork_1-height", window.innerHeight);

                                      }else{
                                        links.filter(function(k){return (k.source.name == d.source.name && k.target.name == d.target.name);}).
                                              style("stroke", color1).
                                              style("stroke-opacity", highopacity);

                                        Shiny.onInputChange("sankeyNetwork_1-target1", selection.target1);
                                        Shiny.onInputChange("sankeyNetwork_1-source1", selection.source1);
                                        Shiny.onInputChange("sankeyNetwork_1-width", window.innerWidth);
                                        Shiny.onInputChange("sankeyNetwork_1-height", window.innerHeight);
                                      }
                                    }else{
                                      links.style("stroke",colorbase).style("stroke-opacity", lowopacity);

                                      Shiny.onInputChange("sankeyNetwork_1-target1", selection.target1);
                                      Shiny.onInputChange("sankeyNetwork_1-target2", selection.target2);
                                      Shiny.onInputChange("sankeyNetwork_1-source1", selection.source1);
                                      Shiny.onInputChange("sankeyNetwork_1-source2", selection.source2);
                                      Shiny.onInputChange("sankeyNetwork_1-width", window.innerWidth);
                                      Shiny.onInputChange("sankeyNetwork_1-height", window.innerHeight);
                                    }
                            });

            d3.selectAll(".node text").style("fill", colorbase);

        }'
      )
  }
