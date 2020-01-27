-module(treewalker_dispatcher).

-include_lib("kernel/include/logger.hrl").
-include_lib("src/treewalker.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(state, {config :: config(),
                retry_policy :: retry_policy(),
                callers_by_workers_pids = #{} :: #{pid() := pid()}}).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).
-define(MIN_RETRY_DELAY, 1000).
-define(MAX_RETRY_DELAY, 5 * 60000).
-define(MAX_RETRIES, 5).

-type retry_policy() :: treewalker_worker:retry_policy().
-type config() :: treewalker_crawler_config:config().

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(term(), config()) -> {ok, pid()} | {error, term()}.
start_link(Id, Config) ->
    gen_server:start_link(?VIA_GPROC(Id), ?MODULE, [Config], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Config]) ->
    process_flag(trap_exit, true),
    MinDelay = application:get_env(treewalker, min_retry_delay, ?MIN_RETRY_DELAY),
    MaxDelay = application:get_env(treewalker, max_retry_delay, ?MAX_RETRY_DELAY),
    MaxRetry = application:get_env(treewalker, max_retries, ?MAX_RETRIES),
    RetryPolicy = #retry_policy{max_retry = MaxRetry, min_delay = MinDelay, max_delay = MaxDelay},
    {ok, #state{config = Config, retry_policy = RetryPolicy}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({request, Caller, Url}, State) ->
    ?LOG_INFO(#{what => request_received, requester => Caller, status => start, url => Url}),
    UpdatedState = try_start_worker(Caller, Url, State),
    {noreply, UpdatedState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({treewalker_worker, _Pid, _Url, _Reason}, State) ->
    %% TODO: Respond to caller
    {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    %% TODO: Respond to caller
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

try_start_worker(Caller, Url, State=#state{retry_policy = RetryPolicy, config = Config}) ->
    case treewalker_worker:start_link(Url, RetryPolicy, Config, self()) of
        {ok, Pid} ->
            ?LOG_INFO(#{what => request_received, requester => Caller, status => in_progress,
                        worker_pid => Pid, url => Url}),
            CallersByWorkersPids = State#state.callers_by_workers_pids,
            State#state{callers_by_workers_pids = CallersByWorkersPids#{Pid => Caller}};
        Error={error, _} ->
            ?LOG_ERROR(#{what => request_received, requester => Caller, status => done,
                         result => error, reason => Error}),
            Caller ! {?MODULE, Url, Error},
            State
    end.
