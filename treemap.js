"use strict";
window.addEventListener("load", function() {
    var svg = document.getElementsByTagName('object')[0].getSVGDocument();
    var rects = svg.getElementsByTagName('rect');
    [].forEach.call(rects, function(rect) {
        rect.setAttribute('stroke-width', 0);
        rect.setAttribute('stroke', 'black');
    });
    var visible = [];
    svg.addEventListener("mouseover", function(event) {
        console.log(event.target.id);
        while(visible.length > 0) {
            visible.pop().setAttribute('stroke-width', 0);
        }
        var ids = event.target.id.split('-');
        while(ids.length > 0) {
            var id = ids.join('-');
            var rect = svg.getElementById(id);
            rect.setAttribute('stroke-width', 1);
            visible.push(rect);
            ids.pop();
        }
    });
});
