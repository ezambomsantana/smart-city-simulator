%Class that represents a car that can moves around the city graph
-module(class_Car).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName, ListVertex , Origin, Path , StartTime , LinkOrigin , Filename , LogPID ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/9, new_link/9,
		 synchronous_new/9, synchronous_new_link/9,
		 synchronous_timed_new/9, synchronous_timed_new_link/9,
		 remote_new/10, remote_new_link/10, remote_synchronous_new/10,
		 remote_synchronous_new_link/10, remote_synchronisable_new_link/10,
		 remote_synchronous_timed_new/10, remote_synchronous_timed_new_link/10,
		 construct/10, destruct/1 ).

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
				class_Actor:name(), pid() , sensor_type() , sensor_type() , sensor_type() , sensor_type() , file() , sensor_type() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->


	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

        DictVertices = dict:from_list( ListVertex ),

	setAttributes( ActorState, [
		{ car_name, CarName },
		{ link_origin, LinkOrigin },
		{ dict , DictVertices },
		{ origin , Origin },
		{ path , Path },
		{ log_pid, LogPID },
		{ filename , Filename },
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

					DictVertices = getAttribute( State , dict ),

					Vertices = list_to_atom(lists:concat( [ InitialVertice , FinalVertice ] )),

					VertexPID = element( 2 , dict:find( InitialVertice , DictVertices)),	
	
					class_Actor:send_actor_message( VertexPID ,
						{ getPosition, { Vertices } }, setAttribute( State , index, Index + 1) );

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

	Speed = getAttribute(State, speed) + 10,
	%	+ class_RandomManager:get_positive_integer_gaussian_value(
	%		_Mu=5, _Sigma=4.0 ),


	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	


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

	_TickState = setAttribute( NewStateSpeed, next_move_tick,
								 CurrentTick + TickDuration ),	

	
  	CarId = getAttribute( State , car_name ),
    	Filename = getAttribute( State , filename ),	

	io:format("vInicio: ~w~n", [ Filename  ]),

	FinalState = case LastPosition == -1 of

		false ->



			LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , atom_to_list(LastPosition) , CarId ] ),
			NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\"/>\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId ] ),

			TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

			file_utils:write( Filename , TextFile  );

		true -> 

			LinkOrigin = getAttribute(State, link_origin),
	   
   			Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\"/>\n", [ CurrentTickOffset , CarId , LinkOrigin ] ),
   			Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\"/>\n", [ CurrentTickOffset , CarId , LinkOrigin ] ),
  			Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ] ),
  			Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , LinkOrigin , CarId ] ),
  	
			NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\"/>\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId ] ),

			TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),

			file_utils:write( Filename , TextFile )
	end,

	executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + Time ).


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

	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + Time ),

	?wooper_return_state_only( ScheduledState ).
