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
	shaman.d3.stacked_area_chart = function(selector, opts){
		if (opts == null){opts = {}}
		if (opts.width == null){opts.width = 400}
		if (opts.height == null){opts.height = 200}
		if (opts.margin == null){opts.margin = [20, 20, 30, 100]}
		if (opts.names == null){opts.names = []}
		if (opts.min == null){opts.min = 0}
		if (opts.max == null){opts.max = 100}

		var svg = d3.select(selector)
				.attr("width", opts.width + opts.margin[1] + opts.margin[3])
				.attr("height", opts.height + opts.margin[0] + opts.margin[2])
			.append("g")
				.attr("transform", "translate(" + opts.margin[3] + "," + opts.margin[0] + ")");

		var x = d3.scale.linear();
		var y = d3.scale.linear().range([opts.height, 0]);
		var color = d3.scale.category10().domain(opts.names);

		var yAxis = d3.svg.axis().scale(y).orient("left");

		var gY = svg.append("g")
			.attr("class", "y axis");

		var area = d3.svg.area()
			.x(function(d) { return x(d.x); })
			.y0(function(d) { return y(d.y0); })
			.y1(function(d) { return y(d.y0 + d.y); });

		this.update = function(data){
			var min = shaman.util.isFunction(opts.min) ? opts.min(data) : opts.min;
			var max = shaman.util.isFunction(opts.max) ? opts.max(data) : opts.max;

			x.range([0, data.length * opts.width / 60])
				.domain(d3.extent(data, function(d, i){return i}));
			y.domain([min, max]);

			yAxis.tickValues([min, max]);
			gY.call(yAxis);

			var stack = d3.layout.stack()
				.values(function(d) { return d.values; });

			var things = stack(color.domain().map(function(name) {
				return {
					name: name,
					values: data.map(function(d, i) {
						return {x: i, y: d[name]};
					})
				};
			}));

			svg.selectAll(".thing path").data(things)
				.enter()
					.append("g").attr("class", "thing")
					.append("path").attr("class", "area")
						.style("fill", function(d) { return color(d.name); });
			svg.selectAll(".thing path")
				.attr("d", function(d) { return area(d.values); })
		}

		// @todo terminate?
	};
});
