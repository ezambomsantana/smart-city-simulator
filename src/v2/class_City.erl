%Class that represents a simple City Map
-module(class_City).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CityName , ListVertex).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, getPosition/3).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Smart-City.City").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").

% Creates a new city graph
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , sensor_type()) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CityName ),


        DictVertices = dict:from_list(ListVertex),
	MapRemove = dict:new(),


	G = osm_to_graph:init( "/home/eduardo/scsimulator/map.osm" ),


	setAttributes( ActorState, [
		{ city_name, CityName },
		{ graph, G },
		{ dict, DictVertices },
		{ probe_pid, non_wanted_probe },	
		{ map_remove , MapRemove },		
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }
							] ).
% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Destructor don't do nothing in this class.
	State.

% The City is a passive actor. Never start spontanely an action
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	
	Filename = text_utils:format(
				 "/home/eduardo/~s",
				 [ "log_sc_simulator.log" ] ),

	DictVertices = getAttribute(State, dict),

	MapRemove = getAttribute( State, map_remove ),
			
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	NewDictVertices = case dict:is_key(CurrentTickOffset, MapRemove) of

		true ->

		
			Element = element ( 2 , dict:find( CurrentTickOffset , MapRemove )),

			List = dict:to_list( Element ),

			remove_cars_list ( List , DictVertices );		

		false -> DictVertices

	end,

	NewState = setAttribute( State , dict , NewDictVertices ),

	InitFile = file_utils:open( Filename, _Opts=[ append, delayed_write ] ),

	file_utils:write( InitFile, "city," ),
	file_utils:write( InitFile, "~w,", [ CurrentTickOffset ] ),
	file_utils:write( InitFile, "~w\n", [ self() ] ),

	print_vertices_situation( InitFile , dict:to_list( NewDictVertices ) ),

	file_utils:close( InitFile ),		

	ScheduledState = executeOneway( NewState , scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).

remove_cars_list( [] , DictVertices ) ->

	DictVertices;

remove_cars_list( [ Head | MoreElements] , DictVertices ) ->

	Vertex = element ( 2 , dict:find( element (1 , Head ) , DictVertices )),

	NewDictVertices = dict:store( element (1 , Head ) , { element(1, Vertex) , element(2, Vertex) - element ( 2 , Head ) } , DictVertices),

	remove_cars_list( MoreElements , NewDictVertices ).


print_vertices_situation( _InitFile , [] ) ->

	ok;

print_vertices_situation( InitFile , [ Head | MoreVertices ] ) ->

	file_utils:write( InitFile, "vertex," ),
	file_utils:write( InitFile, "~w,", [ element( 1 , Head ) ] ),

	VertexData = element( 2 , Head ),
	file_utils:write( InitFile, "~w,", [ element( 1 , VertexData ) ] ), % Time
	file_utils:write( InitFile, "~w\n", [ element( 2 , VertexData ) ] ), % Num of cars	

	print_vertices_situation( InitFile , MoreVertices).
	

% Called by a car wanting to know his next position.
%
% (actor oneway)
%
-spec getPosition( wooper:state(), car_index(), pid() ) ->
					   class_Actor:actor_oneway_return().
getPosition( State, Position, CarPID ) ->

	Graph = getAttribute(State, graph),

	case digraph:vertex(Graph, Position) of
		
		{ V , _Label } -> 
			
			DictVertices = getAttribute(State, dict),

			Vertex = element( 2 , dict:find(V, DictVertices)),

			% Update information about the number of vehicles in the vertex that the car arrived.

			NewDictVertices = dict:store( V , { element(1, Vertex) , element(2, Vertex) + 1} , DictVertices),

			removeAttribute( State , dict ),

			NewState = setAttribute( State , dict , NewDictVertices ),

			% Remove the information of where the cars were in the last interaction.
			MapRemove = getAttribute( State, map_remove ),
			
			TickScheduled = class_Actor:get_current_tick_offset( State ) + element(1, Vertex), 

			NewMapRemove = case dict:is_key(TickScheduled, MapRemove) of

				true ->
	
					Element = element ( 2 , dict:find( TickScheduled , MapRemove )),

					NewElement = case dict:is_key(V, Element) of

						true ->
	
							ElementVertix = element( 2 , dict:find( V , Element ) ),

							dict:store( V , ElementVertix + 1 , Element);

						false ->

							dict:store( V , 1 , Element)

					end,
					
					dict:store ( TickScheduled , NewElement , MapRemove );

				false ->

					NovoDict = dict:new(),

					NovoDictElement = dict:store( V , 1 , NovoDict),

					dict:store( TickScheduled , NovoDictElement , MapRemove)

			end,

			% Add the new car position to the list and its deadline.
			FinalState = setAttribute( NewState , map_remove , NewMapRemove ),


			class_Actor:send_actor_message( CarPID,
					{ go, { Position , element(1, Vertex) } }, FinalState ) ;

		false ->					
			State

	end.


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	SimulationInitialTick = ?getAttr(initial_tick),

	% Checking:
	true = ( SimulationInitialTick =/= undefined ),

	case ?getAttr(probe_pid) of

		non_wanted_probe ->
			ok;

		ProbePid ->
			ProbePid ! { setTickOffset, SimulationInitialTick }

	end,

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).
