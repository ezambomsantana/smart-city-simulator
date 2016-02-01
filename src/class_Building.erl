%Class that represents a simple Sensor
-module(class_Building).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, BuildingName , Lat , Long , Routes ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Smart-City.Sensor").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% Creates a new soda vending machine.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), lat(), long(), routes() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, BuildingName ),

	%?send_info_fmt( ActorState,
	%	"Creating a new sensor, position [~B,~B], "
	%	"and it initial value is ~B.",
	%	[ SensorLat, SensorLong, InitialValue ] ),

	% Depending on the choice of the result manager, it will be either a PID (if
	% the corresponding result is wanted) or a 'non_wanted_probe' atom:

	setAttributes( ActorState, [	
		{ building_name, BuildingName },
		{ lat, Lat },
		{ long, Long },
		{ routes, Routes},
		{ car_count, 1},
		{ probe_pid, non_wanted_probe },
		{ energy_comsuption, 0 },
		{ water_comsuption, 0 },
		{ next_consuption_tick, 1 },
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }
							] ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	%?info_fmt( "Deleting sensor, position [~B,~B], "
	%	, [ ?getAttr(sensor_lat), ?getAttr(sensor_long) ] ),

	% Then allow chaining:
	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	Action = class_RandomManager:get_uniform_value(10000),	

	case Action < 9998 of

		true -> case is_to_consume( State ) of

			true ->
				NewState = generate_consuption( State ),
	
				?wooper_return_state_only( NewState );
			false -> 
				executeOneway( State, scheduleNextSpontaneousTick )
			end;			
		false -> 
			NewState = create_car( State ),

			?wooper_return_state_only( NewState )
	end.

	
-spec generate_consuption( wooper:state() ) -> oneway_return().
generate_consuption( State ) ->

	EnergyComsuption = 10 + class_RandomManager:get_positive_integer_gaussian_value(_Mu=30, _Sigma=5.0 ),
	
	WaterComsuption = 10 + class_RandomManager:get_positive_integer_gaussian_value(_Mu=30, _Sigma=5.0 ),

	EnergyState = setAttribute( State, energy_comsuption, EnergyComsuption ),

	NewState = setAttribute( EnergyState, energy_comsuption, WaterComsuption ),

	Filename = text_utils:format(
				 "/home/eduardo/sc-monitor/locations/buildings/~s.xml",
				 [ getAttribute( NewState, building_name ) ] ),

	Lat = getAttribute( NewState, lat ),

	Long = getAttribute( NewState, long ),

	InitFile = file_utils:open( Filename, _Opts=[ write, delayed_write ] ),

	file_utils:write( InitFile, "<locations>", [] ),	
	file_utils:write( InitFile, "<energy> ~w </energy>", [ EnergyComsuption  ] ),
	file_utils:write( InitFile, "<water> ~w </water>", [ WaterComsuption  ] ),
	file_utils:write( InitFile, "<lat> ~w </lat>", [ Lat  ] ),
	file_utils:write( InitFile, "<long> ~w </long>", [ Long  ] ),
	file_utils:write( InitFile, "</locations>", [] ),
		
	file_utils:close( InitFile ),

	NextConsuption = 5
		+ class_RandomManager:get_positive_integer_gaussian_value(
			_Mu2=3, _Sigma2=5.0 ),

	CurrentTick = class_Actor:get_current_tick( NewState ),

	TickDuration = class_Actor:convert_seconds_to_non_null_ticks(
					 NextConsuption, _MaxRelativeErrorForTest=0.50, NewState ),

	TickState = setAttribute( NewState, next_consuption_tick,
								 CurrentTick + TickDuration ),	

	executeOneway( TickState, scheduleNextSpontaneousTick ).

-spec is_to_consume( wooper:state() ) -> boolean().
is_to_consume( State ) ->

	CurrentTick = class_Actor:get_current_tick( State ),

	case ?getAttr(next_consuption_tick) of

		MoveTick when CurrentTick >= MoveTick ->
			true;

		_ ->
			false

	end.


-spec create_car( wooper:state() ) -> oneway_return().
create_car( State ) ->	

	BuildingName = getAttribute( State , building_name ),

	CarCount = getAttribute( State , car_count ),

	Routes = getAttribute( State , routes ),

	ElectedRouteIndex = class_RandomManager:get_uniform_value( length( Routes ) ),	

	ElectedRoute = list_utils:get_element_at( Routes, ElectedRouteIndex ),

	CarName = io_lib:format( "~s_car_~B", [ BuildingName , CarCount ] ),

	NewState = class_Actor:create_actor( class_Car,
		[ _CarName=CarName,
		  _KnowRoutePID1=ElectedRoute ],
		  State),

	NewNewState = setAttribute( NewState , car_count , CarCount + 1 ),

	executeOneway( NewNewState, scheduleNextSpontaneousTick ).


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

