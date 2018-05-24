%%%-------------------------------------------------------------------
%%% @author Marcin Malenczuk
%%% @copyright (C) 2018, Marcin Malenczuk
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2018 13:51
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Marcin Malenczuk").

-behaviour(gen_server).
-include("pollution_rec.hrl").
%% API
-export([start_link/0,
  getMonitor/0,
  addStation/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getStationMean/2,
  getDailyMean/2,
  getMinMaxValue/3,
  crash/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {monitor = #monitor{}}).

%%%===================================================================
%%% API
%%%===================================================================

getMonitor() ->
  gen_server:call(?SERVER, {get_monitor}).

addStation(Name, Location) ->
  gen_server:call(?SERVER, {add_station, Name, Location}).

addValue(StationInfo, DateTime, Type, Value) ->
  gen_server:call(?SERVER, {add_value, StationInfo, DateTime, Type, Value}).

removeValue(StationInfo, DateTime, Type) ->
  gen_server:call(?SERVER, {remove_value, StationInfo, DateTime, Type}).

getOneValue(StationInfo, DateTime, Type) ->
  gen_server:call(?SERVER, {get_one_value, StationInfo, DateTime, Type}).

getStationMean(StationInfo, Type) ->
  gen_server:call(?SERVER, {get_station_mean, StationInfo, Type}).

getDailyMean(Date, Type) ->
  gen_server:call(?SERVER, {get_daily_mean, Date, Type}).

getMinMaxValue(StationInfo, Date, Type) ->
  gen_server:call(?SERVER, {get_min_max_value, StationInfo, Date, Type}).

crash() ->
  gen_server:cast(?SERVER, {crash}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, Monitor} = pollution:createMonitor(),
  {ok, #state{monitor = Monitor}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({get_monitor}, _From, State) ->
  {reply, State#state.monitor, State};

handle_call({add_station, Name, Location}, _From, State) ->
  case pollution:addStation(State#state.monitor, Name, Location) of
    {ok, Value} ->
      {reply, Value, State#state{monitor = Value}};
    {error, Cause} ->
      {reply, Cause, State}
  end;

handle_call({add_value, StationInfo, DateTime, Type, Value}, _From, State) ->
  case pollution:addValue(State#state.monitor, StationInfo, DateTime, Type, Value) of
    {ok, Value} ->
      {reply, Value, State#state{monitor = Value}};
    {error, Cause} ->
      {reply, Cause, State}
  end;

handle_call({remove_value, StationInfo, DateTime, Type}, _From, State) ->
  case pollution:removeValue(State#state.monitor, StationInfo, DateTime, Type) of
    {ok, Value} ->
      {reply, Value, State#state{monitor = Value}};
    {error, Cause} ->
      {reply, Cause, State}
  end;

handle_call({get_one_value, StationInfo, DateTime, Type}, _From, State) ->
  case pollution:getOneValue(State#state.monitor, StationInfo, DateTime, Type) of
    {ok, Value} ->
      {reply, Value, State};
    {error, Cause} ->
      {reply, Cause, State}
  end;

handle_call({get_station_mean, StationInfo, Type}, _From, State) ->
  case pollution:getStationMean(State#state.monitor, StationInfo, Type) of
    {ok, Value} ->
      {reply, Value, State};
    {error, Cause} ->
      {reply, Cause, State}
  end;

handle_call({get_daily_mean, Date, Type}, _From, State) ->
  case pollution:getDailyMean(State#state.monitor, Date, Type) of
    {ok, Value} ->
      {reply, Value, State};
    {error, Cause} ->
      {reply, Cause, State}
  end;

handle_call({get_min_max_value, StationInfo, Date, Type}, _From, State) ->
  case pollution:getMinMaxValue(State#state.monitor, StationInfo, Date, Type) of
    {ok, Value} ->
      {reply, Value, State};
    {error, Cause} ->
      {reply, Cause, State}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({crash}, State) ->
  1/0,
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

