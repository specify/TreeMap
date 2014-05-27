"use strict";
window.addEventListener("load", function() {
    var transparency = 0.1;
    var svg = document.getElementsByTagName('object')[0].getSVGDocument();
    var rects = svg.getElementsByTagName('rect');
    svg.addEventListener("mouseenter", function(event) {
        [].forEach.call(rects, function(rect) {
            rect.setAttribute('fill-opacity', transparency);
            rect.setAttribute('stroke', 'none');
            rect.setAttribute('stroke-width', 0.5);
        });
    });
    var visible = [];
    svg.addEventListener("mouseover", function(event) {
        console.log(event.target.id);
        [].forEach.call(visible, function(rect) {
            rect.setAttribute('fill-opacity', transparency);
            rect.setAttribute('stroke', 'none');
        });
        visible = [];
        var ids = event.target.id.split('-');
        while(ids.length > 0) {
            var id = ids.join('-');
            var rect = svg.getElementById(id);
            rect.setAttribute('fill-opacity', 1);
            rect.setAttribute('stroke', 'black');
            visible.push(rect);
            ids.pop();
        }
    });
    svg.addEventListener("mouseleave", function(event) {
        [].forEach.call(rects, function(rect) {
            rect.setAttribute('fill-opacity', 1);
            rect.setAttribute('stroke', 'none');
        });
    });
});
