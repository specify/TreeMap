"use strict";
window.addEventListener("load", function() {
    var transparency = 0.1;
    var svg = document.getElementsByTagName('object')[0].getSVGDocument();
    var heading = document.getElementsByTagName('h2')[0];
    var scaling = svg.getElementsByTagName('svg')[0].getAttribute('viewBox').split(' ').slice(2).map(Number);
    scaling[0] /= parseFloat(svg.getElementsByTagName('svg')[0].getAttribute('width'));
    scaling[1] /= parseFloat(svg.getElementsByTagName('svg')[0].getAttribute('height'));
    console.log(scaling);

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
        var names = [];
        while(ids.length > 0) {
            var id = ids.join('-');
            var rect = svg.getElementById(id);
            rect.setAttribute('fill-opacity', 1);
            rect.setAttribute('stroke', 'black');
            visible.push(rect);
            names.unshift(rect.getElementsByTagName('title')[0].textContent);
            ids.pop();
        }
        names.pop();
        // heading.textContent = names.join(" ");
    });
    var zoom = [];
    svg.addEventListener("click", function(event) {
        return;
        console.log(event.target.id);
        var ids = event.target.id.split('-');
        var newZoom = [];
        for (var i = 0; i < ids.length; i++) {
            newZoom.push(ids[i]);
            if (ids[i] != zoom[i]) break;
        }
        zoom = newZoom;
        var zoomRect = svg.getElementById(zoom.join('-'));
        heading.textContent = zoomRect.getElementsByTagName('title')[0].textContent;
        var attrs = {};
        ['x', 'y', 'width', 'height'].forEach(function(attr) {
            attrs[attr] = zoomRect.getAttribute(attr);
        });
        console.log(attrs);
        var viewBox = [attrs.x,
                       attrs.y,
                       attrs.width,
                       attrs.height];
        console.log(viewBox);
        svg.getElementsByTagName('svg')[0].setAttribute('viewBox', viewBox.join(' '));
    });
    var dragOrigin;
    svg.addEventListener("mousedown", function(event) {
        var viewOrigin = svg.getElementsByTagName('svg')[0].getAttribute('viewBox').split(' ').map(Number);
        console.log(viewOrigin);
        dragOrigin = [event.clientX * scaling[0] + viewOrigin[0],
                      event.clientY * scaling[1] + viewOrigin[1],
                      viewOrigin[2],
                      viewOrigin[3]];
        console.log(dragOrigin);
    });
    svg.addEventListener("mousemove", function(event) {
        if (dragOrigin) {
            var viewBox = [
                dragOrigin[0] - event.clientX * scaling[0],
                dragOrigin[1] - event.clientY * scaling[1],
                dragOrigin[2],
                dragOrigin[3]];
            console.log(viewBox);
            svg.getElementsByTagName('svg')[0].setAttribute('viewBox', viewBox.join(' '));
        }
    });
    svg.addEventListener("mouseup", function(event) {
        dragOrigin = null;
    });
    svg.addEventListener("mousewheel", function(event) {
        event.preventDefault();
        var factor = (event.wheelDelta > 0) ? 4/5 : 5/4;
        scaling = scaling.map(function(x) { return factor*x; });
        var viewBox = svg.getElementsByTagName('svg')[0].getAttribute('viewBox').split(' ').map(Number);
        viewBox[2] *= factor;
        viewBox[3] *= factor;
        svg.getElementsByTagName('svg')[0].setAttribute('viewBox', viewBox.join(' '));
    });
    svg.addEventListener("mouseleave", function(event) {
        dragOrigin = null;
        [].forEach.call(visible, function(rect) {
            rect.setAttribute('fill-opacity', transparency);
            rect.setAttribute('stroke', 'none');
        });
    });
});
