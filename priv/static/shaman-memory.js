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

var shaman_memory = (function(){
	var self = {};

	self.link = "#memory";
	self.title = "Memory";
	self.html = '\
<div id="memory" class="container shaman-page">\
</div>';

	var memory = {};

	self.onInit = function(){};
	self.onNodeChange = function(){};

	self.onData = function(node, data){
		if (!memory[node]){
			$("#memory").append('<svg data-node="' + node + '"></svg>');

			memory[node] = {};
			memory[node].d = [];
			memory[node].g = new shaman.d3.stacked_area_chart(
				'#memory svg[data-node="' + node + '"]', {
					names: ["code", "atom", "ets", "binary", "processes"],
					max: function(d){return shaman.util.arrayKeyMax(d, "total")}
				});
		}

		memory[node].d.push(data);
		memory[node].d = memory[node].d.slice(-60);
		memory[node].g.update(memory[node].d);
	}

	return self;
}());

$(document).ready(function(){
	shaman
		.add_page(shaman_memory)
		.add_data_handler("memory", shaman_memory);
});
