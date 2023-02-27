-module(treewalker_worker).

-include_lib("kernel/include/logger.hrl").
-include_lib("src/treewalker.hrl").

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
         handle_continue/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(state, {retry_policy :: retry_policy(),
                retry_ref = undefined :: undefined | reference(),
                request = undefined :: undefined | request(),
                parent_pid :: pid(),
                config :: config()}).

-record(request, {backoff :: backoff:backoff(),
                  pid :: undefined | pid(),
                  url :: url(),
                  retry_ref = undefined :: undefined | reference(),
                  worker :: fun (() -> term()),
                  retry_count = 0 :: non_neg_integer()}).

-type retry_policy() :: #retry_policy{}.
-type config() :: treewalker_crawler_config:config().
-type url() :: treewalker_page:url().
-type request() :: #request{}.

-export_type([retry_policy/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(url(), retry_policy(), config(), pid()) -> {ok, pid()} | {error, term()}.
start_link(Url, RetryPolicy, Config, ParentPid) ->
    gen_server:start_link(?MODULE, [Url, RetryPolicy, Config, ParentPid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Url, RetryPolicy, Config, ParentPid]) ->
    process_flag(trap_exit, true),
    {ok, #state{retry_policy = RetryPolicy, config = Config, parent_pid = ParentPid},
     {continue, {fetch, Url}}}.

handle_continue({fetch, Url}, State) ->
    Request = make_request(Url, State),
    {noreply, State#state{request = Request}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, Ref, retry}, State=#state{request = #request{retry_ref = Ref}=Request}) ->
    Pid = spawn_link(Request#request.worker),
    {noreply, State#state{request = Request#request{pid = Pid}}};

handle_info({treewalker_worker, Pid, Result={ok, _}},
            State=#state{request = #request{pid = Pid, url = Url}}) ->
    ?LOG_DEBUG(#{what => worker_finished, pid => Pid, url => Url, result => ok}),
    State#state.parent_pid ! {?MODULE, self(), Result},
    {stop, normal, State};
handle_info({treewalker_worker, Pid, Error={error, _}},
            State=#state{request = #request{pid = Pid, url = Url}}) ->
    ?LOG_ERROR(#{what => worker_error, pid => Pid, url => Url, result => error, reason => Error}),
    maybe_retry(State);
handle_info({'EXIT', Pid, Reason}, State=#state{request = #request{pid = Pid}}) ->
    ?LOG_ERROR(#{what => worker_error, pid => Pid, reason => Reason}),
    maybe_retry(State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_retry(State=#state{retry_policy = #retry_policy{max_retry = MaxRetries},
                         request = #request{retry_count = Count}=Request})
    when Count =< MaxRetries ->
    {_, Backoff} = backoff:fail(Request#request.backoff),
    Ref = backoff:fire(Backoff),
    Updated = Request#request{backoff = Backoff, retry_ref = Ref, retry_count = Count + 1,
                              pid = undefined},
    {noreply, State#state{request = Updated}};
maybe_retry(State=#state{parent_pid = ParentPid}) ->
    ?LOG_ERROR(#{what => maximum_retries_reached, action => closing}),
    Error = {error, maximum_retries_reached},
    ParentPid ! {?MODULE, self(), Error},
    {stop, Error, State}.

make_request(Url, #state{config = Config, retry_policy = RetryPolicy}) ->
    Worker = worker(Url, Config),
    Backoff = backoff:init(RetryPolicy#retry_policy.min_delay,
                           RetryPolicy#retry_policy.max_delay,
                           self(),
                           retry),
    Pid = spawn_link(Worker),
    #request{pid = Pid, backoff = Backoff, worker = Worker, url = Url}.

worker(Url, Config) ->
    ParentPid = self(),
    fun () ->
            Fetcher = treewalker_crawler_config:fetcher(Config),
            Options = treewalker_crawler_config:fetcher_options(Config),
            Result = Fetcher:request(Url, treewalker_crawler_config:user_agent(Config), Options),
            ParentPid ! {?MODULE, self(), Result}
    end.
