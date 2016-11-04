%Class that represents a car that can moves around the city graph
-module(class_Car).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName, GraphPID , Origin, Path , StartTime , LinkOrigin ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/7, new_link/7,
		 synchronous_new/7, synchronous_new_link/7,
		 synchronous_timed_new/7, synchronous_timed_new_link/7,
		 remote_new/8, remote_new_link/8, remote_synchronous_new/8,
		 remote_synchronous_new_link/8, remote_synchronisable_new_link/8,
		 remote_synchronous_timed_new/8, remote_synchronous_timed_new_link/8,
		 construct/8, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, go/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Smart-City.Car").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").

% Creates a new car
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , sensor_type() , sensor_type() , sensor_type() , sensor_type() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->


	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

	setAttributes( ActorState, [
		{ car_name, CarName },
		{ link_origin, LinkOrigin },
		{ graph_pid, GraphPID },
		{ origin , Origin },
		{ path , Path },
		{ index , 1 },
		{ speed , 0 },
		{ next_move_tick, 1 },
		{ car_position, -1 },
		{ probe_pid, non_wanted_probe },	
		{ start_time , StartTime },					
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }
							] ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Destructor don't do nothing in this class.
	State.

% The core of the car behaviour.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
	
	NewState = request_position( State ),
	
	?wooper_return_state_only( NewState ).

-spec request_position( wooper:state() ) -> wooper:state().
request_position( State ) ->

	Index = getAttribute(State, index),

	Path = ?getAttr(path),

	case Path of 
	
		false ->

			State;

		_ ->

			case Index + 1 < length( Path ) of

				true ->
	
					InitialVertice = list_utils:get_element_at( Path, Index ),

					FinalVertice = list_utils:get_element_at( Path, Index + 1 ),

					class_Actor:send_actor_message( ?getAttr(graph_pid),
						{ getPosition, { InitialVertice , FinalVertice } }, setAttribute( State , index, Index + 1) );

				false ->

					State
	
			end

	end.

	

% Called by the route with the requested position. Write the file to show the position of the car in the map.
%
% (actor oneway)
%
-spec go( wooper:state(), value(), pid() ) -> class_Actor:actor_oneway_return().
go( State, PositionTime , _GraphPID ) ->

	move ( State , PositionTime ).

-spec move( wooper:state(), car_position() ) -> class_Actor:actor_oneway_return().
move( State, PositionTime ) ->

	NewPosition = element( 1 , PositionTime ),
	Time = element( 2 , PositionTime),

	Speed = getAttribute(State, speed)
		+ class_RandomManager:get_positive_integer_gaussian_value(
			_Mu=5, _Sigma=4.0 ),


	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	
    	Filename = text_utils:format(
                 "/home/eduardo/~s",
                 [ "log_sc_simulator.xml" ] ),

	LastPosition = getAttribute( State , car_position ),
	NewState = setAttribute( State, car_position, NewPosition ),
		

	NewStateSpeed = case Speed > 50 of

		true ->
			setAttribute(NewState, speed, 50);
		false -> 
			setAttribute(NewState, speed, Speed)
	end,

	CurrentTick = class_Actor:get_current_tick( NewState ),

	NextMove = 60 - getAttribute(State, speed),

	TickDuration = class_Actor:convert_seconds_to_non_null_ticks(
					 NextMove, _MaxRelativeErrorForTest=0.50, NewStateSpeed ),

	TickState = setAttribute( NewStateSpeed, next_move_tick,
								 CurrentTick + TickDuration ),	


    	InitFile = file_utils:open( Filename, _Opts=[ append, delayed_write ] ),

	
   	CarId = getAttribute( State , car_name ),

%		io:format("vertice: ~w~n", [ Vertices ]),


%			io:format("vertice: ~w~n", [ Vertex ]),

	case LastPosition == -1 of

		false ->
			file_utils:write( InitFile, "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , LastPosition , CarId ] );
		true -> 
			ok
	end,

   	file_utils:write( InitFile, "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , NewPosition , CarId ] ),
   	file_utils:close( InitFile ),

 
%	executeOneway( TickState , scheduleNextSpontaneousTick ).



	
	executeOneway( TickState, addSpontaneousTick, CurrentTickOffset + Time ).


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

	Time = getAttribute( State, start_time ),

    	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

   	CarId = getAttribute( State , car_name ),

	LinkOrigin = getAttribute(State, link_origin),

    	Filename = text_utils:format(
                 "/home/eduardo/~s",
                 [ "log_sc_simulator.xml" ] ),

    	InitFile = file_utils:open( Filename, _Opts=[ append, delayed_write ] ),

   	file_utils:write( InitFile, "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\"/>\n", [ CurrentTickOffset , CarId , LinkOrigin ] ),
   	file_utils:write( InitFile, "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\"/>\n", [ CurrentTickOffset , CarId , LinkOrigin ] ),
  	file_utils:write( InitFile, "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ] ),
  	file_utils:write( InitFile, "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , LinkOrigin , CarId ] ),
   	file_utils:close( InitFile ),

	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + Time ),

	?wooper_return_state_only( ScheduledState ).
