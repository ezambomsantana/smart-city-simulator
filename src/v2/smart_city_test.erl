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



create_map_list( Graph ) ->
	
	Vertices = digraph:vertices( Graph ),

	create_map_list( Vertices , [] ).


create_map_list([] , List) ->
	List;

create_map_list([Element | MoreElements] , List) ->

	NewElement = [{ Element , { 10 , 0 } }], 

	create_map_list( MoreElements , List ++ NewElement ).



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



	G = osm_to_graph:init( "/home/eduardo/scsimulator/map.osm" ),

	ListVertex = create_map_list( G ),

    	io:format("vakue dict: ~w~n", [ ListVertex ]),


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
