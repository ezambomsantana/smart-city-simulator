-module(create_cars).



% usage:
%
% l(osm_parser).
% osm_parser:show("map.osm").

-export([
         iterate_list/7
        ]).


% Init the XML processing

iterate_list( _ListCount, _ListVertex , [] , _Graph , _LogPID , Name , Sup ) ->
	Sup ! { Name };

iterate_list( ListCount, ListVertex , [ Car | MoreCars] , Graph , LogPID , Name , Sup ) ->

	Count = element ( 3 , Car ),

	create_cars( ListCount , element (1 , string:to_integer(Count)) , ListVertex , Car , Graph , false , LogPID , Name ),

	iterate_list( ListCount + 1, ListVertex , MoreCars , Graph , LogPID , Name , Sup ).



create_cars( _ListCount , _CarCount = 0 , _ListVertex ,  _Car , _Graph , _Path , _LogPID , _Name ) ->
	
	ok;


create_cars( ListCount , CarCount , ListVertex ,  Car , Graph , Path , LogPID , Name ) ->

	CarName = io_lib:format( "~B~B~s",
		[ ListCount , CarCount, Name ] ),

	Origin = element ( 1 , Car ),
	Destination = element ( 2 , Car ),
	StartTime = element ( 4 , Car ),
	LinkOrigin = element ( 5 , Car ),
	Type = element ( 6 , Car ),

	case Path of

		false ->

			NewPath = digraph:get_short_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) ),

			ListVertexPath = get_path_nodes( NewPath , ListVertex , [] ),

			ElectedPIDIndex = class_RandomManager:get_uniform_value( length( LogPID ) ),

			LogPIDChoose = list_utils:get_element_at( LogPID , ElectedPIDIndex ),

			class_Actor:create_initial_actor( class_Car,
		  		[ CarName , ListVertexPath , Origin , NewPath , element( 1 , string:to_integer( StartTime )) , LinkOrigin , LogPIDChoose , Type ] ),

			create_cars( ListCount , CarCount - 1 , ListVertex ,  Car , Graph , NewPath , LogPID , Name  );

		_ ->

			ListVertexPath = get_path_nodes( Path , ListVertex , [] ),

			ElectedPIDIndex = class_RandomManager:get_uniform_value( length( LogPID ) ),

			LogPIDChoose = list_utils:get_element_at( LogPID , ElectedPIDIndex ),

			class_Actor:create_initial_actor( class_Car,
		  		[ CarName , ListVertexPath , Origin , Path , element( 1 , string:to_integer( StartTime )) , LinkOrigin , LogPIDChoose , Type ] ),

			create_cars( ListCount , CarCount - 1 , ListVertex ,  Car , Graph , Path , LogPID , Name  )

	end.


get_path_nodes( [] , _ListVertex , List ) ->
	
	List;

get_path_nodes( [ Node | MoreNodes] , ListVertex , List ) ->

	Element = dict:find( Node , ListVertex ),

	ElementList = [{ Node , element( 2 , Element) }],

	get_path_nodes( MoreNodes , ListVertex , List ++ ElementList ).	
