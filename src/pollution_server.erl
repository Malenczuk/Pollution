%%%-------------------------------------------------------------------
%%% @author Marcin Malenczuk
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Marcin Malenczuk").

%% API
-export([start/0, stop/0, init/0, getMonitor/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getMinMaxValue/3]).

-define(responseMonitor(Pid, Monitor, Returned), case Returned of
                                                   {ok, NewMonitor} ->
                                                     Pid ! {ok, NewMonitor},
                                                     loop(NewMonitor);
                                                   {error, Msg} ->
                                                     Pid ! {error, Msg},
                                                     loop(Monitor)
                                                 end).

start() ->
  register(pServer, spawn(?MODULE, init, [])).

init() ->
  Monitor = (element(2, pollution:createMonitor())),
  loop(Monitor).

stop() ->
  request(stop, []).

getMonitor() ->
  request(get_monitor, []).

addStation(Name, Location) ->
  request(add_station, [Name, Location]).

addValue(StationInfo, DateTime, Type, Value) ->
  request(add_value, [StationInfo, DateTime, Type, Value]).

removeValue(StationInfo, DateTime, Type) ->
  request(remove_value, [StationInfo, DateTime, Type]).

getOneValue(StationInfo, DateTime, Type) ->
  request(get_one_value, [StationInfo, DateTime, Type]).

getStationMean(StationInfo, Type) ->
  request(get_station_mean, [StationInfo, Type]).

getDailyMean(Date, Type) ->
  request(get_daily_mean, [Date, Type]).

getMinMaxValue(StationInfo, Date, Type) ->
  request(get_min_max_value, [StationInfo, Date, Type]).


request(RequestType, Args) when is_list(Args) ->
  pServer ! {RequestType, self(), Args},
  receive
    Message -> Message
  after
    1000 -> {error, timeout}
  end.

loop(Monitor) ->
  receive
    {add_station, Pid, [Name, Location]} ->
      Returned = pollution:addStation(Monitor, Name, Location),
      ?responseMonitor(Pid, Monitor, Returned);
    {add_value, Pid, [StationInfo, DateTime, Type, Value]} ->
      Returned = pollution:addValue(Monitor, StationInfo, DateTime, Type, Value),
      ?responseMonitor(Pid, Monitor, Returned);
    {remove_value, Pid, [StationInfo, DateTime, Type]} ->
      Returned = pollution:removeValue(Monitor, StationInfo, DateTime, Type),
      ?responseMonitor(Pid, Monitor, Returned);
    {get_one_value, Pid, [StationInfo, DateTime, Type]} ->
      Returned = pollution:getOneValue(Monitor, StationInfo, DateTime, Type),
      ?responseMonitor(Pid, Monitor, Returned);
    {get_station_mean, Pid, [StationInfo, Type]} ->
      Pid ! pollution:getStationMean(Monitor, StationInfo, Type),
      loop(Monitor);
    {get_daily_mean, Pid, [Date, Type]} ->
      Pid ! pollution:getDailyMean(Monitor, Date, Type),
      loop(Monitor);
    {get_min_max_value, Pid, [StationInfo, Date, Type]} ->
      Pid ! pollution:getMinMaxValue(Monitor, StationInfo, Date, Type),
      loop(Monitor);
    {get_monitor, Pid, []} ->
      Pid ! {ok, Monitor},
      loop(Monitor);
    {stop, Pid, []} ->
      Pid ! {ok, Monitor},
      ok
  end.
