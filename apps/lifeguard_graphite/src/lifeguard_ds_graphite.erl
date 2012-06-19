%% @doc This is a data source for Lifeguard that retrieves data from
%% a graphite installation.

-module(lifeguard_ds_graphite).
-behavior(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(args, {
        target, % The target graph
        from    % From what time
    }).

-record(state, {
        host % Host of the graphite server
    }).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Start the data source in a supervision tree. The ServerRef is given
%% by Lifeguard and is the atom that we should register ourselves locally
%% under. The Name and Args are what are configured in the application
%% configuration.
start_link(ServerRef, _Name, Args) ->
    gen_server:start_link({local, ServerRef}, ?MODULE, Args, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Args) ->
    case is_proplist(Args) of
        false ->
            % The arguments should be a proplist so we show an error and
            % crash out.
            lager:error("Arguments must be a proplist."),
            {stop, invalid_args};
        true ->
            case proplists:lookup(host, Args) of
                {host, Host} ->
                    lager:info("Started the graphite data source..."),
                    {ok, #state{host=Host}};
                none ->
                    lager:error("Host must be given."),
                    {stop, invalid_args}
            end
    end.

handle_call({get, [Object]}, _From, State) ->
    % Validate our arguments
    lager:debug("Get: ~p", [Object]),
    Result = case extract_args(Object) of
        {error, Reasons} ->
            % Something bad happened so that's just our result
            {error, Reasons};
        {ok, Args} ->
            % The object is valid so let's go forth and get the graphite data
            HTTPOptions = [
                    {autoredirect, true},
                    {connect_timeout, 5000},
                    {relaxed, true},
                    {timeout, 5000}
            ],
            ReqOptions = [
                    {body_format, binary},
                    {sync, true}
            ],
            Url = io_lib:format("http://~s/render/?target=~s&from=~s&format=raw",
                                [State#state.host,
                                 Args#args.target,
                                 Args#args.from]),
            Url2 = lists:flatten(Url),
            lager:debug("Requesting: ~s", [Url2]),
            {ok, HTTPResult} = httpc:request(get, {Url2, []}, HTTPOptions, ReqOptions),
            {{"HTTP/1.1", 200, _Reason}, _Headers, HTTPBody} = HTTPResult,
            lager:debug("Graphite response: ~p", [HTTPBody]),

            % Parse the body for the numbers and return that.
            {ok, DataPoints} = parse_data_points(HTTPBody),

            {ok, DataPoints}
    end,

    {reply, Result, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Extracts the arguments from a proplist into a proper record so
%% that it is easier to use around the library.
extract_args(Object) ->
    case is_proplist(Object) of
        false ->
            {error, <<"Argument must be an object.">>};
        true ->
            extract_args_get_target(Object, #args{})
    end.

extract_args_get_target(Object, Result) ->
    case proplists:lookup(<<"target">>, Object) of
        {<<"target">>, Value} ->
            extract_args_get_from(Object, Result#args{target=Value});
        none ->
            {error, <<"A target must be given.">>}
    end.

extract_args_get_from(Object, Result) ->
    case proplists:lookup(<<"from">>, Object) of
        {<<"from">>, Value} ->
            {ok, Result#args{from=Value}};
        none ->
            {error, <<"A from value must be given.">>}
    end.

%% @doc Returns a boolean of whether a given list is a valid proplist
%% or not.
is_proplist([]) ->
    true;
is_proplist([{_Key, _Value} | Rest]) ->
    is_proplist(Rest);
is_proplist(_Other) ->
    false.

%% @doc Parses the data points out of the graphite response body, returning
%% a list of numbers.
parse_data_points(RawBody) ->
    % The body usually ends in a newline, which we must strip out.
    Body = case binary:last(RawBody) of
        $\n -> binary:part(RawBody, 0, byte_size(RawBody) - 1);
        _   -> RawBody
    end,

    % Split on the pipe and convert the numbers to floats
    case binary:split(Body, <<"|">>) of
        [_Before, NumberList] ->
            Numbers = lists:map(fun(X) ->
                            case X of
                                <<"None">> -> null;
                                Number ->
                                    {Float, _Rest} = string:to_float(binary_to_list(Number)),
                                    Float
                            end
                    end, binary:split(NumberList, <<",">>, [global])),
            {ok, Numbers};
        _ ->
            {error, bad_format}
    end.

-ifdef(TEST).

extract_args_bad_argument_test() ->
    {error, _Reasons} = extract_args(5),
    {error, _Reasons} = extract_args("bad"),
    {error, _Reasons} = extract_args(<<"nope">>).

extract_args_test() ->
    {ok, Result} = extract_args([
                {<<"target">>, <<"foo">>},
                {<<"from">>, <<"bar">>}]),

    <<"foo">> = Result#args.target,
    <<"bar">> = Result#args.from.

is_proplist_test() ->
    false = is_proplist(12),
    true = is_proplist([]),
    true = is_proplist([{key, value}]),
    true = is_proplist([{key, value}, {key2, value2}]).

parse_data_points_test() ->
    {error, bad_format} = parse_data_points(<<"bad\n">>),
    {ok, [1.0,-2.0,3.0]} = parse_data_points(<<"count,1340124510,1340128110,10|1.0,-2.0,3.0\n">>).

parse_data_points_null_test() ->
    % Test None in the middle
    {ok, [1.0,null,2.0]} = parse_data_points(<<"foo|1.0,None,2.0\n">>),

    % Test None at the end
    {ok, [1.0, 2.0, null]} = parse_data_points(<<"foo|1.0,2.0,None\n">>).

-endif.
