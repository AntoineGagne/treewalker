-module(treewalker_dispatcher).

-include_lib("kernel/include/logger.hrl").
-include_lib("src/treewalker.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2,
         request/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(state, {config :: config(),
                retry_policy :: retry_policy(),
                max_concurrent_worker :: pos_integer(),
                pending_requests = queue:new() :: queue:queue({reference(), pid(), url()}),
                callers_by_workers_pids = #{} :: #{pid() := {reference(), pid()}}}).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).
-define(MIN_RETRY_DELAY, 1000).
-define(MAX_RETRY_DELAY, 5 * 60000).
-define(MAX_RETRIES, 5).
-define(MAX_CONCURRENT_WORKERS, 10).

-type url() :: treewalker_page:url().
-type retry_policy() :: treewalker_worker:retry_policy().
-type config() :: treewalker_crawler_config:config().
-type id() :: treewalker_crawler_sup:dispatcher_id().

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(id(), config()) -> {ok, pid()} | {error, term()}.
start_link(Id, Config) ->
    gen_server:start_link(?VIA_GPROC(Id), ?MODULE, [Config], []).

-spec request(id(), url()) -> reference().
request(Id, Url) ->
    gen_server:call(?VIA_GPROC(Id), {request, self(), Url}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Config]) ->
    process_flag(trap_exit, true),
    MinDelay = application:get_env(treewalker, min_retry_delay, ?MIN_RETRY_DELAY),
    MaxDelay = application:get_env(treewalker, max_retry_delay, ?MAX_RETRY_DELAY),
    MaxRetry = application:get_env(treewalker, max_retries, ?MAX_RETRIES),
    MaxConcurrentWorker = application:get_env(treewalker,
                                              max_concurrent_worker,
                                              ?MAX_CONCURRENT_WORKERS),
    RetryPolicy = #retry_policy{max_retry = MaxRetry, min_delay = MinDelay, max_delay = MaxDelay},
    {ok, #state{config = Config,
                retry_policy = RetryPolicy,
                max_concurrent_worker = MaxConcurrentWorker}}.

handle_call({request, Caller, Url}, _, State=#state{pending_requests = PendingRequests}) ->
    ?LOG_INFO(#{what => request_received, requester => Caller, status => queuing, url => Url}),
    After = rand:uniform(5),
    erlang:send_after(After * 1000, self(), start),
    Ref = make_ref(),
    {reply, Ref, State#state{pending_requests = queue:in({Ref, Caller, Url}, PendingRequests)}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, State=#state{callers_by_workers_pids = Workers,
                                max_concurrent_worker = MaxConcurrentWorker})
  when map_size(Workers) < MaxConcurrentWorker ->
    ?LOG_INFO(#{what => worker_start, status => start, workers_amount => map_size(Workers)}),
    try_start_pending_requests(State);
handle_info(start, State) ->
    {noreply, State};

handle_info({treewalker_worker, Pid, Reason}, State) ->
    ?LOG_DEBUG(#{what => worker_response, pid => Pid}),
    UpdatedState = maybe_response_to_caller(Reason, Pid, State),
    After = rand:uniform(5),
    erlang:send_after(After * 1000, self(), start),
    {noreply, UpdatedState};
handle_info({'EXIT', Pid, Reason}, State) ->
    ?LOG_DEBUG(#{what => worker_exit, pid => Pid, reason => Reason}),
    UpdatedState = maybe_response_to_caller({error, Reason}, Pid, State),
    After = rand:uniform(5),
    erlang:send_after(After * 1000, self(), start),
    {noreply, UpdatedState};

handle_info(Message, State) ->
    ?LOG_WARNING(#{what => unexpected_message, message => Message}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

try_start_worker(Caller, Ref, Url, State=#state{retry_policy = RetryPolicy, config = Config}) ->
    case treewalker_worker:start_link(Url, RetryPolicy, Config, self()) of
        {ok, Pid} ->
            ?LOG_INFO(#{what => request_received, requester => Caller, status => in_progress,
                        worker_pid => Pid, url => Url}),
            CallersByWorkersPids = State#state.callers_by_workers_pids,
            State#state{callers_by_workers_pids = CallersByWorkersPids#{Pid => {Ref, Caller}}};
        Error={error, _} ->
            ?LOG_ERROR(#{what => request_received, requester => Caller, status => done,
                         result => error, reason => Error}),
            Caller ! {?MODULE, Ref, Error},
            State
    end.

maybe_response_to_caller(Response, Pid, State) ->
    case maps:take(Pid, State#state.callers_by_workers_pids) of
        {{Ref, Caller}, CallersByWorkersPids} ->
            Caller ! {?MODULE, Ref, Response},
            State#state{callers_by_workers_pids = CallersByWorkersPids};
        error ->
            State
    end.

try_start_pending_requests(State=#state{pending_requests = PendingRequests}) ->
    case queue:out(PendingRequests) of
        {{value, {Ref, Caller, Url}}, UpdatedRequests} ->
            UpdatedState = State#state{pending_requests = UpdatedRequests},
            {noreply, try_start_worker(Caller, Ref, Url, UpdatedState)};
        {empty, _} ->
            {noreply, State}
    end.
