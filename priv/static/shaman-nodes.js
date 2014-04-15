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

var shaman_nodes = (function(){
	var self = {};

	self.link = "#nodes";
	self.title = "Nodes";
	self.html = '\
<div id="nodes" class="container shaman-page">\
	<div class="panel panel-default">\
		<div class="panel-heading">Connected nodes</div>\
		<div class="panel-body">This table lists all nodes currently connected to Shaman.</div>\
\
		<table id="nodes-table" class="table table-striped">\
		<thead>\
			<tr>\
				<th data-name="name">Name</th>\
				<th data-name="release">Release</th>\
				<th data-name="alien_started">Alien started?</th>\
				<th data-name="shaman_enabled">Shaman enabled?</th>\
			</tr>\
		</thead>\
		<tbody></tbody>\
		<tfoot><tr><td colspan="4">\
			<div class="btn-group">\
				<div class="btn-group">\
					<button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown">\
						Connect to node\
						<span class="caret"></span>\
					</button>\
					<ul class="dropdown-menu"><li style="padding:15px;padding-bottom:0">\
						<!-- @todo better style -->\
						<form id="nodes-connect">\
							<input name="name" type="text" placeholder="Node name"/>\
							<input name="cookie" type="text" placeholder="Cookie"/>\
							<input type="submit" value="Connect to node"/>\
						</form>\
					</li></ul>\
				</div>\
			</div>\
		</td></tr></tfoot>\
		</table>\
	</div>\
</div>';

	var currentNodeSelect = null;
	var nodesTable = null;

	self.onInit = function(){
		$("#nodes-connect").submit(function(e){
			e.preventDefault();
			shaman.send({
				"t": "nodes.connect",
				"name": $("#nodes-connect input[name=name]").val(),
				"cookie": $("#nodes-connect input[name=cookie]").val()
			});
		});

		currentNodeSelect = new shaman.d3.select(
			"#current-node",
			{text: "name", value: "name"});
		nodesTable = new shaman.d3.dynamic_table(
			"#nodes-table>tbody",
			"#nodes-table>thead>tr>th",
			{key: "name"});
	};

	self.onNodeChange = function(){};

	self.onData = function(node, data){
		if (shaman.currentNode == null){
			shaman.currentNode = data[0].name;
		}

		currentNodeSelect.update(data);
		nodesTable.update(data);
	};

	return self;
}());

$(document).ready(function(){
	shaman
		.add_page(shaman_nodes)
		.add_data_handler("nodes", shaman_nodes);
});
