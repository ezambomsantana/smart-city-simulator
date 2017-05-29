-module(matrix_parser).
-include_lib("xmerl/include/xmerl.hrl").

% usage:
%
% l(osm_parser).
% osm_parser:show("map.osm").

-export([
         show/1
        ]).

% Init the XML processing
show(Infilename) ->
    {Doc, _Misc} = xmerl_scan:file(Infilename),
    init(Doc).

% read the OSM tag and extract all children
init(Node) ->
    case Node of
        #xmlElement{name=Name, content=Content} ->
            
	    case Name of
		
		scsimulator_matrix -> 

			List = trips(Content , []),
			List;

		_ -> ok

	    end;
            _ -> ok
    end.

trips([], List) ->
    List;

trips([Node | MoreNodes], List) ->
    Element = extract_node(Node),
    case Element of

	ok ->
    		
		trips(MoreNodes , List);

	_ ->
		NewList = List ++ Element,
		trips(MoreNodes , NewList)

    end.

%
% Show a node/element and then the children of that node.
extract_node(Node) ->

    case Node of
        #xmlElement{name=Name, attributes=Attributes} ->
            
	    case Name of
		
		trip -> 
			
			Origin = children( Attributes , origin ),
			Destination = children( Attributes , destination ),
			Count = children( Attributes , count ),
			StartTime = children( Attributes , start ),
			LinkOrigin = children( Attributes , link_origin ),
			Type = children( Attributes , type ),
			Mode = children( Attributes , mode ),
			[ { Origin , Destination , Count , StartTime , LinkOrigin , Type , Mode } ];

		_ ->
			ok
	    end;

            _ -> ok
    end.

children( [] , _Type ) ->
    ok;

children( [Node | MoreNodes] , Type ) ->
    Element = extract_children( Node , Type ),
    case Element of

	ok ->
    		
		children( MoreNodes , Type );

	_ ->
		Element

    end.


extract_children( Node , Type ) ->

    case Node of
        #xmlAttribute{name=Name, value=Value} ->
            
	    case Name of
		
		Type -> 

			Value;

		_ -> ok

	    end;
            _ -> ok
    end.
