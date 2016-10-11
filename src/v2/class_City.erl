%Class that represents a simple City Map
-module(class_City).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CityName).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1 ).

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
				class_Actor:name()) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CityName ),

	G = digraph:new(),
	V1 = digraph:add_vertex(G, r1),
	V2 = digraph:add_vertex(G, r2),
	V3 = digraph:add_vertex(G, r3),
	V4 = digraph:add_vertex(G, r4),
	V5 = digraph:add_vertex(G, r5),

	digraph:add_edge(G, V1, V2),
	digraph:add_edge(G, V2, V4),
	digraph:add_edge(G, V2, V5),
	digraph:add_edge(G, V2, V3),
	digraph:add_edge(G, V3, V4),
	digraph:add_edge(G, V3, V2),
	digraph:add_edge(G, V2, V1),

	ListVertex = [{r1, {10 , 0}}, {r2, {20 , 0}}, {r3, {30 , 0}}, {r4, {30 , 0}}, {r5, {30 , 0}}],
        DictVertices = dict:from_list(ListVertex),

	setAttributes( ActorState, [
		{ city_name, CityName },
		{ graph, G },
		{ dict, DictVertices },
		{ probe_pid, non_wanted_probe },			
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

	InitFile = file_utils:open( Filename, _Opts=[ append, delayed_write ] ),

	file_utils:write( InitFile, "city," ),
	file_utils:write( InitFile, "~w,", [ CurrentTickOffset ] ),
	file_utils:write( InitFile, "~w,", [ self() ] ),
	file_utils:write( InitFile, "~w,", [ DictVertices ] ),
		
	file_utils:close( InitFile ),

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).
	

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

			NewDictVertices = dict:store( V , { element(1, Vertex) , element(2, Vertex) + 1} , DictVertices),

			removeAttribute( State , dict ),

			NewState = setAttribute( State , dict , NewDictVertices ),

			class_Actor:send_actor_message( CarPID,
					{ go, { Position , element(1, Vertex) } }, NewState ) ;

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
