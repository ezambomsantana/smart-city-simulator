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


iterate_list( _ListCount, _GraphPid , [] , _Graph , _Filename ) ->
	ok;

iterate_list( ListCount, GraphPid, [ Car | MoreCars] , Graph , Filename ) ->

	Count = element ( 3 , Car ),

	create_cars( ListCount , element (1 , string:to_integer(Count)) , GraphPid , Car , Graph , false , Filename ),

	iterate_list( ListCount + 1, GraphPid, MoreCars , Graph , Filename ).



create_cars( _ListCount , _CarCount = 0 , _GraphPid ,  _Car , _Graph , _Path , _Filename ) ->
	
	ok;


create_cars( ListCount , CarCount , GraphPid ,  Car , Graph , Path , Filename ) ->

	CarName = io_lib:format( "~B~B",
		[ ListCount , CarCount ] ),

	Origin = element ( 1 , Car ),
	Destination = element ( 2 , Car ),
	StartTime = element ( 4 , Car ),
	LinkOrigin = element ( 5 , Car ),

	case Path of

		false ->

			NewPath = digraph:get_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) ),

			class_Actor:create_initial_actor( class_Car,
		  		[ CarName , GraphPid , Origin , NewPath , element( 1 , string:to_integer( StartTime )) , LinkOrigin , Filename ] ),

			create_cars( ListCount , CarCount - 1 , GraphPid ,  Car , Graph , NewPath , Filename );

		_ ->

			class_Actor:create_initial_actor( class_Car,
		  		[ CarName , GraphPid , Origin , Path , element( 1 , string:to_integer( StartTime )) , LinkOrigin , Filename ] ),

			create_cars( ListCount , CarCount - 1 , GraphPid ,  Car , Graph , Path , Filename )

	end.



create_map_list( Graph ) ->
	
	Edges = digraph:edges( Graph ),


	create_map_list( Edges , Graph , [] ).


create_map_list([] , _Graph , List) ->
	List;

create_map_list([Element | MoreElements] , Graph , List) ->
	
	{_E, _V1, _V2, _Label} = digraph:edge( Graph , Element ),
	

	Vertices = list_to_atom(lists:concat( [ _V1 , _V2 ] )),

	NewElement = [{ Vertices , { list_to_atom(_Label) , 10 , 0 } }], 

	create_map_list( MoreElements , Graph , List ++ NewElement ).



% Runs the test.
%
-spec run() -> no_return().
run() ->	


	?test_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca Smart City Integration Test",

	  % Using 100Hz here:
	  tick_duration = 1

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

		enable_performance_tracker = true

	},

	Config = config_parser:show("/home/eduardo/config.xml"),

	ListCars = matrix_parser:show( element( 4 , Config ) ),

	G = matsim_to_digraph:show( element( 3 , Config ) , false ),

 	Filename = element( 1 , Config ),

    	InitFile = file_utils:open( Filename, _Opts=[ append, delayed_write ] ),

    	file_utils:write( InitFile, "<events version=\"1.0\">\n" ),
    	file_utils:close( InitFile ),


	ListVertex = create_map_list( G ),


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
							   DeploymentSettings, LoadBalancingSettings ),

	Graph = class_Actor:create_initial_actor( class_City,
		[ _GraphName="sp" , ListVertex , element( 3 , Config ) ] ),


	iterate_list( 1 , Graph , ListCars , G , Filename ),

	% We want this test to end once a specified virtual duration elapsed, in
	% seconds:
	SimulationDuration = element( 1 , string:to_integer(element( 2 , Config ) ) ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, for a stop after a duration "
					"in virtual time of ~Bms.", [ SimulationDuration ] ),

	RootTimeManagerPid ! { startFor, [ SimulationDuration, self() ] },

	?test_info( "Waiting for the simulation to end, "
				"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->

        		CloseFile = file_utils:open( Filename, _Opts=[ append, delayed_write ] ),

        		file_utils:write( CloseFile, "</events>" ),
       			file_utils:close( CloseFile ),
			?test_info( "Simulation stopped spontaneously, "
						"specified stop tick must have been reached." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?test_stop.
