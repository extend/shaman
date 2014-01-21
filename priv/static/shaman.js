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

// Hash.

var hash = function(){
	return (location.hash == "") ? "#" : location.hash;
}

var section = function(){
	return (location.hash == "") ? "nodes" : location.hash.substr(1);
}

var hashchange = function(){
	$(".navbar-nav li").removeClass("active");
	$(".navbar-nav a[href=" + hash() + "]").parent().addClass("active");

	$("div.section").hide();
	$("div#" + section()).show();
};

// Websocket.

var ws;
var ws_handlers = {};

var ws_init = function(f){
	ws = new window.WebSocket("ws://" + window.location.host + "/ws");
	if (f){
		ws.onopen = f;
	}
	ws.onclose = function(){
		setTimeout(function(){
			ws_init();
		}, 500);
	}
	ws.onmessage = function(e){
		var obj = JSON.parse(e.data);
		for (var i = 0; i < obj.length; i++){
			ws_handlers[obj[i].t](obj[i]);
		}
	}
}

var ws_add_handler = function(t, f){
	ws_handlers[t] = f;
}

// Live handlers.

var live_handlers = {};

var live_data = function(e){
	live_handlers[e.n].update(e.d);
}

var live_add_handler = function(n, o){
	if (o.init != null){
		o.init();
	}

	live_handlers[n] = o;
}

var selectData = function(selector, data){
	var ret = [];

	d3.selectAll(selector).each(function(){
		var name = $(this).attr("data-name");
		ret.push({n: name, v: data[name]});
	});

	return ret;
}

// Nodes.

live_add_handler("nodes", {
	update: function(data){
		var rows = d3.select("#nodes-table>tbody")
			.selectAll("tr")
			.data(data, function(d){return d.name});
		rows.enter().append("tr");
		rows.exit().remove();

		var cells = rows.selectAll("td")
			.data(function(d){
				return selectData("#nodes-table>thead>tr>th", d);
			});
		cells.enter().append("td");
		cells.text(function(d){return d.v});
		cells.exit().remove();
	}
});

// Processes.

var processes_graph = function(selector, opts){
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

live_add_handler("processes", {
	// Pid currently shown in the modal.
	modalPid: null,
	modalReds: [],
	modalRedsHistGraph: null,

	init: function(){
		var self = this;

		$("#processes-modal").on("hidden.bs.modal", function(){
			self.modalPid = null;
			self.modalReds = [];
			self.modalRedsHistGraph.terminate();
			self.modalRedsHistGraph = null;
		});
	},

	// Update the list of processes and the modal if any.
	update: function(data){
		var self = this;

		// @todo Yeah no idea how to multi-node list of processes yet.
		d3.select("#processes span.node").text(data.node);

		var rows = d3.select("#processes-table>tbody")
			.selectAll("tr")
			.data(data.data, function(d){return d.pid});
		rows.enter().append("tr").on("click", function(d){
			self.modalPid = d.pid;
			self.modalReds = [];
			self.modalRedsHistGraph = new processes_graph("#processes-modal-reds-history");

			d3.select("#processes-modal span.pid").text(d.pid);
			$("#processes-modal .data").empty();
			$("#processes-modal path").attr("d", "");
			$("#processes-modal").modal({keyboard: true}); // @todo Keyboard doesn't work.
		});
		rows.exit().remove();

		var cells = rows.selectAll("td")
			.data(function(d){
				var new_reds = 0;
				if (this.length && this[2].__data__){
					new_reds = d.reds - this[2].__data__.v;
					this[0].parentElement.__data__.new_reds = new_reds;
				}

				return selectData("#processes-table>thead>tr>th", d);
			});
		cells.enter().append("td");
		cells.text(function(d){return d.v});
		cells.exit().remove();

		if (self.modalPid){
			var md = d3.selectAll('#processes-table>tbody>tr')
				.filter(function(d){return d.pid == self.modalPid})
				[0][0].__data__;

			var vals = d3.selectAll("#processes-modal .data")
				.data(function(){
					return selectData("#processes-modal .data", md);
				});
			vals.text(function(d){return d.v});

			// Keep the last 60 new_reds and update the graph.
			self.modalReds.push(md.new_reds);
			self.modalReds = self.modalReds.slice(-60);
			self.modalRedsHistGraph.update(self.modalReds);
		}

		$("#processes-table>tbody>tr").tsort("td:eq(3)", {order: "desc"});
	}
});

// Init.

$(document).ready(function(){
	// Fix dropdown form click problem.
	$('.dropdown-menu').click(function(e){
		e.stopPropagation();
	});

	// Hash.

	$(".navbar-nav a[href=" + hash() + "]").parent().addClass("active");

	$("div.section").hide();
	$("div#" + section()).show();

	window.onhashchange = hashchange;

	// Websocket.

	ws_add_handler("data", live_data);
	ws_init();

	// Nodes.

	$("#nodes-connect").submit(function(e){
		e.preventDefault();
		ws.send(JSON.stringify({
			"t": "nodes.connect",
			"name": $("#nodes-connect input[name=name]").val(),
			"cookie": $("#nodes-connect input[name=cookie]").val()
		}));
	});
});
