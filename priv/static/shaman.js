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

var shaman = (function(){
	var self = {};

	var ws;
	var message_handlers = {};
	var data_handlers = {};
	var pages = [];

	self.currentNode = null;

	self.add_message_handler = function(t, f){
		message_handlers[t] = f;

		return self;
	};

	self.add_data_handler = function(n, o){
		data_handlers[n] = o;

		return self;
	};

	self.add_page = function(o){
		$("#shaman-navbar").append('<li><a href="' + o.link + '">' + o.title + '</a></li>');
		$("body").append(o.html);

		o.onInit();
		pages.push(o);

		return self;
	}

	self.connect = function(){
		ws = new window.WebSocket("ws://" + window.location.host + "/ws");

		ws.onclose = function(){
			setTimeout(function(){shaman.connect();}, 500);
		}

		ws.onmessage = function(e){
			var obj = JSON.parse(e.data);
			for (var i = 0; i < obj.length; i++){
				if (obj[i].t == "data"){
					if (data_handlers[obj[i].n]){
						data_handlers[obj[i].n].onData(obj[i].o, obj[i].d);
					} else{
						console.log("No data handler defined: " + obj[i].n);
					}
				} else{
					message_handlers[obj[i].t](obj[i]);
				}
			}
		}

		return self;
	};

	self.send = function(o){
		ws.send(JSON.stringify(o));
	};

	var hash = function(){
		return (location.hash == "") ? "#" : location.hash;
	}

	// The first page is the default page.
	var currentPage = function(){
	return (location.hash == "")
		? $("#shaman-navbar li:first a").attr("href").substr(1)
		: location.hash.substr(1);
	}

	var hashChange = function(){
		$(".navbar-nav li").removeClass("active");
		$(".navbar-nav a[href=" + hash() + "]").parent().addClass("active");

		$("div.shaman-page").hide();
		$("div#" + currentPage()).show();
	};

	var nodeChange = function(){
		self.currentNode = $(this).val();

		for (var i = 0; i < pages.length; i++){
			pages[i].onNodeChange();
		}
	};

	self.conjure = function(){
		self.connect();

		// Pages.
		$(".navbar-nav a[href=" + hash() + "]").parent().addClass("active");
		$("div.shaman-page").hide();
		$("div#" + currentPage()).show();
		window.onhashchange = hashChange;

		// Node selection.
		$("#current-node").change(nodeChange);
	};

	return self;
}());

$(document).ready(function(){
	// Fix dropdown form click problem.
	$('.dropdown-menu').click(function(e){
		e.stopPropagation();
	});

	// Start Shaman.
	shaman.conjure();
});
