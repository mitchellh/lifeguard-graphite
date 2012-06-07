%% @doc This is a data source for Lifeguard that retrieves data from
%% a graphite installation.

-module(lifeguard_ds_graphite).
-behavior(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Start the data source in a supervision tree. The ServerRef is given
%% by Lifeguard and is the atom that we should register ourselves locally
%% under. The Name and Args are what are configured in the application
%% configuration.
start_link(ServerRef, _Name, _Args) ->
    gen_server:start_link({local, ServerRef}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    lager:info("Started the graphite data source..."),
    {ok, no_state}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-endif.
