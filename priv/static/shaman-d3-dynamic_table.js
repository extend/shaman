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
	shaman.d3.dynamic_table = function(bodySelector, namesSelector, opts){
		this.update = function(data){
			var rows = d3.select(bodySelector)
				.selectAll("tr")
				.data(data, function(d){return d[opts.key]});
			var tr = rows.enter().append("tr");
			if (opts.onClick){
				tr.on("click", opts.onClick);
			}
			rows.exit().remove();

			var cells = rows.selectAll("td")
				.data(function(d){
					if (opts.onDataUpdate){
						opts.onDataUpdate(d, this);
					}
					return shaman.d3.selectData(namesSelector, d)
				});
			cells.enter().append("td");
			cells.text(function(d){return d.v});
			cells.exit().remove();
		};
	};
});
