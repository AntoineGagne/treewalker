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
                callers_by_workers_pids = #{} :: #{pid() := {url(), pid()}}}).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).
-define(MIN_RETRY_DELAY, 1000).
-define(MAX_RETRY_DELAY, 5 * 60000).
-define(MAX_RETRIES, 5).

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
    RetryPolicy = #retry_policy{max_retry = MaxRetry, min_delay = MinDelay, max_delay = MaxDelay},
    {ok, #state{config = Config, retry_policy = RetryPolicy}}.

handle_call({request, Caller, Url}, _From, State) ->
    ?LOG_INFO(#{what => request_received, requester => Caller, status => start, url => Url}),
    Ref = make_ref(),
    UpdatedState = try_start_worker(Caller, Ref, Url, State),
    {reply, Ref, UpdatedState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({treewalker_worker, Pid, Url, Reason}, State) ->
    ?LOG_DEBUG(#{what => worker_response, pid => Pid, url => Url, reason => Reason}),
    maybe_response_to_caller(Reason, Pid, State);
handle_info({'EXIT', Pid, Reason}, State) ->
    ?LOG_ERROR(#{what => worker_error, pid => Pid, result => error, reason => Reason}),
    maybe_response_to_caller({error, Reason}, Pid, State);

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
            State#state{callers_by_workers_pids = CallersByWorkersPids#{Pid => {Ref, Url, Caller}}};
        Error={error, _} ->
            ?LOG_ERROR(#{what => request_received, requester => Caller, status => done,
                         result => error, reason => Error}),
            Caller ! {?MODULE, Ref, Url, Error},
            State
    end.

maybe_response_to_caller(Response, Pid, State) ->
    case maps:take(Pid, State#state.callers_by_workers_pids) of
        {{Ref, Url, Caller}, CallersByWorkersPids} ->
            Caller ! {?MODULE, Ref, Url, Response},
            {noreply, State#state{callers_by_workers_pids = CallersByWorkersPids}};
        error ->
            {noreply, State}
    end.
