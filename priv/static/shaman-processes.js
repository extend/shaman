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

var shaman_processes = (function(){
	var self = {};

	self.link = "#processes";
	self.title = "Processes";
	self.html = '\
<div id="processes" class="container shaman-page">\
	<div class="panel panel-default">\
		<div class="panel-heading">Processes</div>\
\
		<table id="processes-table" class="table table-striped">\
		<thead>\
			<tr>\
				<th data-name="pid">Pid</th>\
				<th data-name="init">Initial call</th>\
				<th data-name="reds">Reductions</th>\
				<th data-name="new_reds">New reductions</th>\
				<th data-name="mem">Memory</th>\
				<th data-name="msgq">MsgQ</th>\
				<th data-name="current">Current function</th>\
			</tr>\
		</thead>\
		<tbody></tbody>\
		</table>\
	</div>\
\
	<div class="modal fade" id="processes-modal" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">\
	<div class="modal-dialog">\
	<div class="modal-content">\
		<div class="modal-header">\
			<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>\
			<h4 class="modal-title" id="myModalLabel">Process <span class="pid"></span></h4>\
		</div>\
		<div class="modal-body">\
			<div class="panel panel-default">\
				<div class="panel-heading">Overview</div>\
\
				<table class="table table-striped">\
				<tbody>\
					<tr><th>Initial call</th><td class="data" data-name="init"></td></tr>\
					<tr><th>Current function</th><td class="data" data-name="current"></td></tr>\
					<tr><th>Registered name</th><td class="data" data-name="reg_name"></td></tr>\
					<tr><th>Status</th><td class="data" data-name="status"></td></tr>\
					<tr><th>Message queue size</th><td class="data" data-name="msgq"></td></tr>\
					<tr><th>Priority</th><td class="data" data-name="priority"></td></tr>\
					<tr><th>Traps exits</th><td class="data" data-name="trap_exit"></td></tr>\
				</tbody>\
				</table>\
			</div>\
\
			<div class="panel panel-default">\
				<div class="panel-heading">Reductions</div>\
\
				<table class="table table-striped">\
				<tbody>\
					<tr><th>Total</th><td class="data" data-name="reds"></td></tr>\
					<tr><th>New</th><td class="data" data-name="new_reds"></td></tr>\
					<tr><th>Last 60s</th><td><svg id="processes-modal-reds-history"></svg></td></tr>\
				</tbody>\
				</table>\
			</div>\
\
			<div class="panel panel-default">\
				<div class="panel-heading">Memory and GC</div>\
\
				<table class="table table-striped">\
				<tbody>\
					<tr><th>Memory</th><td class="data" data-name="mem"></td></tr>\
					<tr><th>GC min heap size</th><td class="data" data-name="gc_min_heap_size"></td></tr>\
					<tr><th>GC fullsweep after</th><td class="data" data-name="gc_fullsweep_after"></td></tr>\
				</tbody>\
				</table>\
			</div>\
		</div>\
		<div class="modal-footer">\
			<button type="button" class="btn btn-default" data-dismiss="modal">Close</button>\
		</div>\
	</div>\
	</div>\
	</div>\
</div>';

	var modalPid = null;
	var modalNode = null;
	var modalReds = [];
	var modalRedsHistGraph = null;

	var processesTable = null;

	self.onInit = function(){
		$("#processes-modal").on("hidden.bs.modal", function(){
			modalPid = null;
			modalReds = [];
			modalRedsHistGraph.terminate();
			modalRedsHistGraph = null;
		});

		processesTable = new shaman.d3.dynamic_table(
			"#processes-table>tbody",
			"#processes-table>thead>tr>th", {
				key: "pid",
				onClick: function(d){
					modalPid = d.pid;
					modalReds = [];
					modalRedsHistGraph = new shaman.d3.line_chart("#processes-modal-reds-history");

					d3.select("#processes-modal span.pid").text(d.pid);
					$("#processes-modal .data").empty();
					$("#processes-modal path").attr("d", "");
					$("#processes-modal").modal({keyboard: true}); // @todo Keyboard doesn't work.
				},
				onDataUpdate: function(d, el){
					if (el.length && el[2].__data__){
						el[0].parentElement.__data__.new_reds = d.reds - el[2].__data__.v;
					}
				}
			});
	};

	self.onNodeChange = function(){
		$("#processes-table>tbody").empty();
	};

	self.onData = function(node, data){
		if (node == shaman.currentNode){
			modalNode = node;
			processesTable.update(data);
		}

		if (modalPid && node == modalNode){
			var md = d3.selectAll('#processes-table>tbody>tr')
				.filter(function(d){return d.pid == modalPid})
				[0][0].__data__;

			// @todo Might be worth creating a static_table object.
			var vals = d3.selectAll("#processes-modal .data")
				.data(function(){
					return shaman.d3.selectData("#processes-modal .data", md);
				});
			vals.text(function(d){return d.v});

			// Keep the last 60 new_reds and update the graph.
			modalReds.push(md.new_reds);
			modalReds = modalReds.slice(-60);
			modalRedsHistGraph.update(modalReds);
		}

		$("#processes-table>tbody>tr").tsort("td:eq(3)", {order: "desc"});
	};

	return self;
}());

$(document).ready(function(){
	shaman
		.add_page(shaman_processes)
		.add_data_handler("processes", shaman_processes);
});
