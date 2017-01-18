% Author: Eduardo Santana (efzambom@ime.usp.br)

-module(smart_city_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").


create_log( _Number = 0, _Filename , _List ) ->
	_List ;

create_log( Number , Filename , List  ) ->

	LogName = io_lib:format( "Log~B", [ Number ] ),

	LogPID = class_Actor:create_initial_actor( class_Log,
		 [ LogName , Filename ] ),

	create_log( Number - 1 , LogPID , List ++ [ LogPID ] ).


iterate_list( _ListCount, _ListVertex , [] , _Graph , _Filename , _LogPID ) ->
	ok;

iterate_list( ListCount, ListVertex , [ Car | MoreCars] , Graph , Filename , LogPID ) ->

	Count = element ( 3 , Car ),

	create_cars( ListCount , element (1 , string:to_integer(Count)) , ListVertex , Car , Graph , false , Filename , LogPID ),

	iterate_list( ListCount + 1, ListVertex , MoreCars , Graph , Filename , LogPID ).



create_cars( _ListCount , _CarCount = 0 , _ListVertex ,  _Car , _Graph , _Path , _Filename , _LogPID ) ->
	
	ok;


create_cars( ListCount , CarCount , ListVertex ,  Car , Graph , Path , Filename , LogPID ) ->

	CarName = io_lib:format( "~B~B",
		[ ListCount , CarCount ] ),

	Origin = element ( 1 , Car ),
	Destination = element ( 2 , Car ),
	StartTime = element ( 4 , Car ),
	LinkOrigin = element ( 5 , Car ),

	case Path of

		false ->

			NewPath = digraph:get_short_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) ),

			ListVertexPath = get_path_nodes( NewPath , ListVertex , [] ),

			ElectedPIDIndex = class_RandomManager:get_uniform_value( length( LogPID ) ),

			LogPIDChoose = list_utils:get_element_at( LogPID , ElectedPIDIndex ),

			class_Actor:create_initial_actor( class_Car,
		  		[ CarName , ListVertexPath , Origin , NewPath , element( 1 , string:to_integer( StartTime )) , LinkOrigin , Filename, LogPIDChoose ] ),

			create_cars( ListCount , CarCount - 1 , ListVertex ,  Car , Graph , NewPath , Filename , LogPID );

		_ ->

			ListVertexPath = get_path_nodes( Path , ListVertex , [] ),

			ElectedPIDIndex = class_RandomManager:get_uniform_value( length( LogPID ) ),

			LogPIDChoose = list_utils:get_element_at( LogPID , ElectedPIDIndex ),

			class_Actor:create_initial_actor( class_Car,
		  		[ CarName , ListVertexPath , Origin , Path , element( 1 , string:to_integer( StartTime )) , LinkOrigin , Filename, LogPIDChoose ] ),

			create_cars( ListCount , CarCount - 1 , ListVertex ,  Car , Graph , Path , Filename , LogPID )

	end.


get_path_nodes( [] , _ListVertex , List ) ->
	
	List;

get_path_nodes( [ Node | MoreNodes] , ListVertex , List ) ->

	Element = dict:find( Node , ListVertex ),

	ElementList = [{ Node , element( 2 , Element) }],

	get_path_nodes( MoreNodes , ListVertex , List ++ ElementList ).	


% for each vertex is necessary to save its out links
create_map_list([] , _Graph , List) ->
	List;

create_map_list([Element | MoreElements] , Graph , List) ->
	
	{_E, _V1, _V2, _Label} = digraph:edge( Graph , Element ),

	Id = element( 1 , _Label),
	Length = element( 2 , _Label),
	
	Vertices = list_to_atom(lists:concat( [ _V1 , _V2 ] )),

	NewElement = [{ Vertices , { list_to_atom( Id ) , Length } }], 

	create_map_list( MoreElements , Graph , List ++ NewElement ).


% Create the actors that represent the city vertex

create_street_list( Graph ) ->
	
	Vertices = digraph:vertices( Graph ),

	create_street_list( Vertices , [] , Graph ).

create_street_list([] , List , _Graph ) ->
	List;

create_street_list([Element | MoreElements] , List , Graph) ->

	Edges = digraph:out_edges( Graph , Element ),

	ListEdges = create_map_list( Edges , Graph , [] ),

	StreetPID = class_Actor:create_initial_actor( class_Street,
		  [ atom_to_list(Element) , ListEdges ] ),

	NewElement = [{ Element , StreetPID }], 

	create_street_list( MoreElements , List ++ NewElement , Graph ).

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




	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
							   DeploymentSettings, LoadBalancingSettings ),

	Config = config_parser:show("/home/santaned/scsimulator/config.xml"),

	ListCars = matrix_parser:show( element( 4 , Config ) ),

	G = matsim_to_digraph:show( element( 3 , Config ) , false ),

 	Filename = element( 1 , Config ),

    	InitFile = file_utils:open( Filename, _Opts=[ append, delayed_write ] ),

    	file_utils:write( InitFile, "<events version=\"1.0\">\n" ),
    	file_utils:close( InitFile ),

	% create the vertices actors
	ListVertex  = create_street_list( G ),

	LogList = create_log( 100 , Filename , [] ),

	% create the cars
	iterate_list( 1 , dict:from_list( ListVertex ) , ListCars , G , Filename , LogList ),

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
