-module(treewalker_crawler).

-behaviour(gen_statem).

%% API
-export([start_link/2]).

%% gen_statem callbacks
-export([init/1,
         callback_mode/0,
         handle_event/4,
         terminate/3]).

-record(data, {config :: config()}).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).

-type config() :: treewalker_crawler_config:config().

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(term(), config()) -> {ok, pid()} | {error, term()}.
start_link(Id, Config) ->
    gen_statem:start_link(?VIA_GPROC(Id), ?MODULE, [Config], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    [handle_event_function].

init([Config]) ->
    {ok, start, #data{config = Config}}.

handle_event(cast, _Message, _State, _Data) ->
    keep_state_and_data;
handle_event({call, From}, _Action, State, Data) ->
    {next_state, State, Data, [{reply, From, Data}]};
handle_event(info, _Msg, _State, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
