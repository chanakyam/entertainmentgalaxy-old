-module(photo_gallery_handler).

-export([init/3]).

-export([content_types_provided/2]).
-export([welcome/2]).
-export([terminate/3]).

%% Init
init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

%% Callbacks
content_types_provided(Req, State) ->
	{[		
		{<<"text/html">>, welcome}	
	], Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% API
welcome(Req, State) -> 
 	{CategoryBinary, _} = cowboy_req:qs_val(<<"c">>, Req),
 	Category = binary_to_list(CategoryBinary),
 	
	Url = "http://api.contentapi.ws/videos?channel=movies&limit=1&skip=2&format=long",
	% io:format("movies url: ~p~n",[Url]),
	{ok, "200", _, Response_mlb} = ibrowse:send_req(Url,[],get,[],[]),
	ResponseParams_mlb = jsx:decode(list_to_binary(Response_mlb)),	
	[Params] = proplists:get_value(<<"articles">>, ResponseParams_mlb),	

	Url_all_news = "http://api.contentapi.ws/news?channel=image_galleries&limit=50&skip=0",
	{ok, "200", _, Response} = ibrowse:send_req(Url_all_news,[],get,[],[]),
	ResponseParams = jsx:decode(list_to_binary(Response)),	
	ParamsAllNews = proplists:get_value(<<"articles">>, ResponseParams),

 	{ok, Body} = photo_gallery_dtl:render([{<<"videoParam">>,Params},{<<"news_category">>,Category},{<<"allnews">>,ParamsAllNews}]),
 		{Body, Req, State}.	