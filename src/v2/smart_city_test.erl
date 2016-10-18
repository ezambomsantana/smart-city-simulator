% Copyright (C) 2008-2014 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% Integration test for the soda deterministic example case.
%
% See also:
%
% - class_SodaVendingMachine.erl
% - class_DeterministicThirstyCustomer.erl
%
-module(smart_city_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").


iterate_list( _ListCount, _GraphPid , [] , _Graph ) ->
	ok;

iterate_list( ListCount, GraphPid, [ Car | MoreCars] , Graph ) ->

	Count = element ( 3 , Car ),

	create_cars( ListCount , element (1 , string:to_integer(Count)) , GraphPid , Car , Graph ),

	iterate_list( ListCount + 1, GraphPid, MoreCars , Graph ).



create_cars( _ListCount , _CarCount = 0 , _GraphPid ,  _Car , _Graph ) ->
	
	ok;


create_cars( ListCount , CarCount , GraphPid ,  Car , Graph ) ->

	CarName = io_lib:format( "Car # ~B ~B",
		[ ListCount , CarCount ] ),

	Origin = element ( 1 , Car ),
	Destination = element ( 2 , Car ),

	Path = digraph:get_path( Graph , list_to_atom(Origin) , list_to_atom(Destination)),

	class_Actor:create_initial_actor( class_Car,
		  [ CarName , GraphPid , Origin , Path ] ),

	create_cars( ListCount , CarCount - 1 , GraphPid ,  Car , Graph ).




% Runs the test.
%
-spec run() -> no_return().
run() ->


	?test_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca Smart City Integration Test",

	  % Using 100Hz here:
	  tick_duration = 0.1

	  % We leave it to the default specification (all_outputs):
	  % result_specification =
	  %  [ { targeted_patterns, [ {".*",[data_and_plot]} ] },
	  %    { blacklisted_patterns, ["^Second" ] } ]

	  %result_specification = [ { targeted_patterns, [ {".*",data_only} ] } ]

	},


	DeploymentSettings = #deployment_settings{

		computing_hosts = { use_host_file_otherwise_local,
							"sim-diasca-host-candidates.txt" },

		%node_availability_tolerance = fail_on_unavailable_node,

		% We want to embed additionally this test and its specific
		% prerequisites, defined in the Mock Simulators:
		%
		additional_elements_to_deploy = [ { ".", code } ],

		% Note that the configuration file below has not to be declared above as
		% well:
		enable_data_exchanger = { true, [ "soda_parameters.cfg" ] },

		enable_performance_tracker = false

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
							   DeploymentSettings, LoadBalancingSettings ),


	G = digraph:new(),
	V1 = digraph:add_vertex(G, r1),
	V2 = digraph:add_vertex(G, r2),
	V3 = digraph:add_vertex(G, r3),
	V4 = digraph:add_vertex(G, r4),
	V5 = digraph:add_vertex(G, r5),
	%V6 = digraph:add_vertex(G, r6),
	V7 = digraph:add_vertex(G, r7),
	V8 = digraph:add_vertex(G, r8),
	V9 = digraph:add_vertex(G, r9),
	V10 = digraph:add_vertex(G, r10),
	V11 = digraph:add_vertex(G, r11),


	digraph:add_edge(G, V1, V2),
	digraph:add_edge(G, V2, V3),
	digraph:add_edge(G, V2, V4),
	digraph:add_edge(G, V3, V7),
	digraph:add_edge(G, V4, V5),
	digraph:add_edge(G, V5, V7),
	digraph:add_edge(G, V7, V8),
	digraph:add_edge(G, V5, V11),
	digraph:add_edge(G, V7, V10),
	digraph:add_edge(G, V7, V9),
	digraph:add_edge(G, V11, V10),
	digraph:add_edge(G, V10, V9),
	digraph:add_edge(G, V9, V8),


	digraph:add_edge(G, V2, V1),
	digraph:add_edge(G, V3, V2),
	digraph:add_edge(G, V4, V2),
	digraph:add_edge(G, V7, V3),
	digraph:add_edge(G, V5, V4),
	digraph:add_edge(G, V7, V5),
	digraph:add_edge(G, V8, V7),
	digraph:add_edge(G, V11, V5),
	digraph:add_edge(G, V10, V7),
	digraph:add_edge(G, V9, V7),
	digraph:add_edge(G, V10, V11),
	digraph:add_edge(G, V9, V10),
	digraph:add_edge(G, V8, V9),

	ListVertex = [{r1, {10 , 0}}, {r2, {10 , 0}}, {r3, {10 , 0}}, {r4, {10 , 0}}, {r5, {10 , 0}}, {r6, {10 , 0}}, {r7, {10 , 0}}, {r8, {10 , 0}}, {r9, {10 , 0}}, {r10, {10 , 0}}, {r11, {10 , 0}}],

	Graph = class_Actor:create_initial_actor( class_City,
		[ _GraphName="sp" , ListVertex ] ),

	ListCars = matrix_parser:show("/home/eduardo/scsimulator/trips.xml"),

	iterate_list( 0 , Graph , ListCars , G ),

	% We want this test to end once a specified virtual duration elapsed, in
	% seconds:
	SimulationDuration = 1000,

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, for a stop after a duration "
					"in virtual time of ~Bms.", [ SimulationDuration ] ),

	RootTimeManagerPid ! { startFor, [ SimulationDuration, self() ] },

	?test_info( "Waiting for the simulation to end, "
				"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously, "
						"specified stop tick must have been reached." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?test_stop.
