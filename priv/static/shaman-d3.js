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
	shaman.d3 = {
		selectData: function(selector, data){
			var ret = [];

			d3.selectAll(selector).each(function(){
				var name = $(this).attr("data-name");
				ret.push({n: name, v: data[name]});
			});

			return ret;
		}
	};

	shaman.util = (function(){
		var self = {};

		self.isFunction = function(o){
			return !!(o && o.constructor && o.call && o.apply);
		};

		self.arrayKeyMax = function(a, k){
			var max = a[0][k];

			for (var i = a.length - 1; i > 0; i--){
				if (a[i][k] > max) max = a[i][k];
			}

			return max;
		};

		return self;
	}());
});
