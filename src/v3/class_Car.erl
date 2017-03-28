%Class that represents a car that can moves around the city graph
-module(class_Car).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName, ListVertex , Origin, Path , StartTime , LinkOrigin , LogPID ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/8, new_link/8,
		 synchronous_new/8, synchronous_new_link/8,
		 synchronous_timed_new/8, synchronous_timed_new_link/8,
		 remote_new/9, remote_new_link/9, remote_synchronous_new/9,
		 remote_synchronous_new_link/9, remote_synchronisable_new_link/9,
		 remote_synchronous_timed_new/9, remote_synchronous_timed_new_link/9,
		 construct/9, destruct/1 ).

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
				class_Actor:name(), pid() , sensor_type() , sensor_type() , sensor_type() , sensor_type() , sensor_type() ) -> wooper:state().
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
		{ car_position, -1 },
		{ start_time , StartTime }					
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

remove_first( [ _First | List ] ) ->
	
	List.
	

-spec request_position( wooper:state() ) -> wooper:state().
request_position( State ) ->

	Path = ?getAttr(path),

	case Path of 

		finish ->

			executeOneway( State , declareTermination );			
	
		false ->

			State;

		_ ->

			case length( Path ) > 1 of

				true ->				
	
					InitialVertice = list_utils:get_element_at( Path, 1 ),

					FinalVertice = list_utils:get_element_at( Path, 2 ),

					DictVertices = getAttribute( State , dict ),

					Vertices = list_to_atom(lists:concat( [ InitialVertice , FinalVertice ] )),

					VertexPID = element( 2 , dict:find( InitialVertice , DictVertices)),	

					DictCleaned = dict:erase( InitialVertice , DictVertices ),

					CleanState = setAttribute( State , dict , DictCleaned ),

					PathRest = remove_first( Path ),

					FinalState = setAttribute( CleanState , path, PathRest ),
	
					class_Actor:send_actor_message( VertexPID ,
						{ getPosition, { Vertices } }, FinalState );

				false ->							

					LastPosition = getAttribute( State , car_position ),

					case LastPosition == -1 of

						true ->
							
							executeOneway( State , declareTermination );	

						false ->

							CarId = getAttribute( State , car_name ),	

							CurrentTickOffset = class_Actor:get_current_tick_offset( State ),   	

							TextFile = io_lib:format( "~w,4,~s,~s\n",  
								[ CurrentTickOffset , CarId , atom_to_list(LastPosition) ] ), % events 1 - depart, 2 - enters, 3 - left, 4 - arrive
	
							LogPid = ?getAttr(log_pid),

							NewState = setAttribute( State, path, finish ),

							NewNewState = class_Actor:send_actor_message( LogPid ,
								{ receive_action, { TextFile } }, NewState ),

							executeOneway( NewNewState , addSpontaneousTick, CurrentTickOffset + 1 )

					end

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

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	LastPosition = getAttribute( State , car_position ),	
	NewState = setAttribute( State, car_position, NewPosition ),
		
  	CarId = getAttribute( State , car_name ),

	FinalState = case LastPosition == -1 of

		false ->

			LastPositionText = io_lib:format( "~w,3,~s,~s\n", [ CurrentTickOffset , atom_to_list(LastPosition) , CarId ] ),
			NextPositionText = io_lib:format( "~w,2,~s,~s\n", [  CurrentTickOffset , atom_to_list(NewPosition) , CarId ] ),

			TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

			X = ?getAttr(log_pid),

			class_Actor:send_actor_message( X,
				{ receive_action, { TextFile } }, NewState );

		true -> 

			LinkOrigin = getAttribute(State, link_origin),
	   
   			Text2 = io_lib:format( "~w,1,~s,car", [ CurrentTickOffset , LinkOrigin ] ),  	
			NextPositionText = io_lib:format( "~w,2,~s,~s\n", [  CurrentTickOffset , atom_to_list(NewPosition) , CarId ] ),

			TextFile = lists:concat( [ Text2 , NextPositionText  ] ),

			X = ?getAttr(log_pid),
			
			class_Actor:send_actor_message( X,
				{ receive_action, { TextFile } }, NewState )

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

	Time = getAttribute( State, start_time ),

    	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),   	

	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + Time ),

	?wooper_return_state_only( ScheduledState ).
