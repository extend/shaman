// Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

$(document).ready(function(){
	shaman.d3.line_chart = function(selector, opts){
		if (opts == null){opts = {}}
		if (opts.width == null){opts.width = 240}
		if (opts.height == null){opts.height = 120}
		if (opts.margin == null){opts.margin = [20, 20, 30, 50]}

		var svg = d3.select(selector)
				.attr("width", opts.width + opts.margin[1] + opts.margin[3])
				.attr("height", opts.height + opts.margin[0] + opts.margin[2])
			.append("g")
				.attr("transform", "translate(" + opts.margin[3] + "," + opts.margin[0] + ")");
		
		var x = d3.scale.linear();
		var y = d3.scale.linear().range([opts.height, 0]);
		var yAxis = d3.svg.axis().scale(y).orient("left");

		var gY = svg.append("g")
			.attr("class", "y axis");

		var line = d3.svg.line()
			.x(function(d, i){return x(i)})
			.y(function(d){return y(d)});

		var path = svg.append("path")
			.attr("class", "line");

		this.update = function(data){
			x.range([0, data.length * opts.width / 60])
				.domain(d3.extent(data, function(d, i){return i}));
			y.domain(d3.extent(data, function(d){return d}));

			yAxis.tickValues([Math.min.apply(null, data), Math.max.apply(null, data)]);
			gY.call(yAxis);

			path.datum(data).attr("d", line);
		}

		this.terminate = function(){
			d3.selectAll(selector + " *").remove();
		}
	};
});
