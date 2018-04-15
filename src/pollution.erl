%%%-------------------------------------------------------------------
%%% @author Marcin Maleńczuk
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pollution).
-author("Marcin Maleńczuk").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getMinMaxValue/4]).

-include("pollution_rec.hrl").

createMonitor() -> {ok, #monitor{}}.



addStation(Monitor, Name, {_, _} = Location)
  when is_record(Monitor, monitor) ->
  case not usedName(Monitor, Name) and not usedLocation(Monitor, Location) of
    false -> {error, "Station already exists"};
    true -> {ok,
      Monitor#monitor{
        locations = maps:put(Location, Name, Monitor#monitor.locations),
        stations = maps:put(
          Name,
          #station{name = Name, location = Location},
          Monitor#monitor.stations)}}
  end;

addStation(_, _, _) ->
  {error, "Wrong arguments"}.



addValue(Monitor, Station, {{_, _, _}, {_, _, _}} = DateTime, Type, Value)
  when is_record(Monitor, monitor),
  is_record(Station, station) ->
  case maps:find({DateTime, Type}, Station#station.measurements) of
    {ok, _} -> {error, "Measurement of that type already exists at that date"};
    error -> {ok,
      Monitor#monitor{
      stations = maps:put(
        Station#station.name,
        Station#station{
          measurements = maps:put(
            {DateTime, Type},
            #measurement{
              type = Type,
              datetime = DateTime,
              value = Value},
            Station#station.measurements)},
        Monitor#monitor.stations)}}
  end;

addValue(Monitor, {_, _} = Location, {{_, _, _}, {_, _, _}} = DateTime, Type, Value)
  when is_record(Monitor, monitor) ->
  case findStationAtLocation(Monitor, Location) of
    {ok, Station} -> addValue(Monitor, Station, DateTime, Type, Value);
    {error, Msg} -> {error, Msg}
  end;

addValue(Monitor, Name, {{_, _, _}, {_, _, _}} = DateTime, Type, Value)
  when is_record(Monitor, monitor) ->
  case findStationWithName(Monitor, Name) of
    {ok, Station} -> addValue(Monitor, Station, DateTime, Type, Value);
    {error, Msg} -> {error, Msg}
  end;

addValue(_, _, _, _, _) -> {error, "Wrong arguments"}.



removeValue(Monitor, Station, {{_, _, _}, {_, _, _}} = DateTime, Type)
  when is_record(Monitor, monitor),
  is_record(Station, station) ->
  case maps:find({DateTime, Type}, Station#station.measurements) of
    error -> {error, "Measurement of that type doesn't exists at that date"};
    {ok, _} -> {ok,
      Monitor#monitor{
        stations = maps:update(
          Station#station.name,
          Station#station{
            measurements = maps:remove(
              {DateTime, Type},
              Station#station.measurements)},
          Monitor#monitor.stations)}}
  end;

removeValue(Monitor, {_, _} = Location, {{_, _, _}, {_, _, _}} = DateTime, Type)
  when is_record(Monitor, monitor) ->
  case findStationAtLocation(Monitor, Location) of
    {ok, Station} -> removeValue(Monitor, Station, DateTime, Type);
    {error, Msg} -> {error, Msg}
  end;

removeValue(Monitor, Name, {{_, _, _}, {_, _, _}} = DateTime, Type)
  when is_record(Monitor, monitor) ->
  case findStationWithName(Monitor, Name) of
    {ok, Station} -> removeValue(Monitor, Station, DateTime, Type);
    {error, Msg} -> {error, Msg}
  end;

removeValue(_, _, _, _) -> {error, "Wrong arguments"}.



getOneValue(_, Station, {{_, _, _}, {_, _, _}} = DateTime, Type)
  when is_record(Station, station) ->
  case maps:find({DateTime, Type}, Station#station.measurements) of
    error -> {error, "Measurement of that type doesn't exists at that date"};
    {ok, Measurement} -> {ok, Measurement#measurement.value}
  end;

getOneValue(Monitor, {_, _} = Location, {{_, _, _}, {_, _, _}} = DateTime, Type)
  when is_record(Monitor, monitor) ->
  case findStationAtLocation(Monitor, Location) of
    {ok, Station} -> getOneValue(Monitor, Station, DateTime, Type);
    {error, Msg} -> {error, Msg}
  end;

getOneValue(Monitor, Name, {{_, _, _}, {_, _, _}} = DateTime, Type)
  when is_record(Monitor, monitor) ->
  case findStationWithName(Monitor, Name) of
    {ok, Station} -> getOneValue(Monitor, Station, DateTime, Type);
    {error, Msg} -> {error, Msg}
  end;

getOneValue(_, _, _, _) -> {error, "Wrong arguments"}.



getStationMean(Monitor, Station, Type)
  when is_record(Monitor, monitor),
  is_record(Station, station) ->
  {Sum, Count} = maps:fold(
    fun (_, M, {S, C}) -> {S + M#measurement.value, C + 1} end,
    {0, 0},
    maps:filter(
      fun (_, M) -> M#measurement.type == Type end,
      Station#station.measurements)),
  safeDiv(Sum, Count);

getStationMean(Monitor, {_, _} = Location, Type)
  when is_record(Monitor, monitor) ->
  case findStationAtLocation(Monitor, Location) of
    {ok, Station} -> getStationMean(Monitor, Station, Type);
    {error, Msg} -> {error, Msg}
  end;

getStationMean(Monitor, Name, Type)
  when is_record(Monitor, monitor) ->
  case findStationWithName(Monitor, Name) of
    {ok, Station} ->getStationMean(Monitor, Station, Type);
    {error, Msg} -> {error, Msg}
  end;

getStationMean(_, _, _) -> {error, "Wrong arguments"}.



getStationDailyMean(Station, {_, _, _} = Date, Type)
  when is_record(Station, station) ->
  {Sum, Count} = maps:fold(
    fun (_, M, {S, C}) -> {S + M#measurement.value, C + 1} end,
    {0, 0},
    maps:filter(
      fun (_, M) -> (M#measurement.type == Type) and (element(1, M#measurement.datetime) == Date) end,
      Station#station.measurements)),
  safeDiv(Sum, Count);

getStationDailyMean(_, _, _) -> {error, "Wrong arguments"}.



getDailyMean(Monitor, {_, _, _} = Date, Type)
  when is_record(Monitor, monitor) ->
  {Sum, Count} = maps:fold(
    fun (_, 0, {S, C}) -> {S, C};
        (_, StationDailyMean, {S, C}) -> {S + StationDailyMean, C + 1} end,
    {0, 0},
    maps:map(
      fun (_, S) -> getStationDailyMean(S, Date, Type) end,
      Monitor#monitor.stations)),
  safeDiv(Sum, Count);

getDailyMean(_, _, _) -> {error, "Wrong arguments"}.



getMinMaxValue(_, Station, {_, _, _} = Date, Type)
  when is_record(Station, station) ->
  maps:fold(
    fun (_, M, {nomeasurement, nomeasurement}) -> {M#measurement.value, M#measurement.value};
        (_, M, {MinV, MaxV}) when M#measurement.value < MinV -> {M#measurement.value, MaxV};
        (_, M, {MinV, MaxV}) when M#measurement.value > MaxV -> {MinV, M#measurement.value};
        (_, _, V) -> V end,
    {nomeasurement, nomeasurement},
    maps:filter(
      fun (_, M) -> (M#measurement.type == Type) and (element(1, M#measurement.datetime) == Date) end,
      Station#station.measurements));

getMinMaxValue(Monitor, {_, _} = Location, {_, _, _} = Date, Type)
  when is_record(Monitor, monitor) ->
  case findStationAtLocation(Monitor, Location) of
    {ok, Station} -> getMinMaxValue(Monitor, Station, Date, Type);
    {error, Msg} -> {error, Msg}
  end;

getMinMaxValue(Monitor, Name, {_, _, _} = Date, Type)
  when is_record(Monitor, monitor) ->
  case findStationWithName(Monitor, Name) of
    {ok, Station} -> getMinMaxValue(Monitor, Station, Date, Type);
    {error, Msg} -> {error, Msg}
  end;

getMinMaxValue(_, _, _, _) -> {error, "Wrong arguments"}.



usedName(Monitor, Name) ->
  case maps:find(Name, Monitor#monitor.stations) of
    {ok, _} -> true;
    error -> false
  end.

usedLocation(Monitor, {_, _} = Location) ->
  case maps:find(Location, Monitor#monitor.locations) of
    {ok, _} -> true;
    error -> false
  end.

findStationWithName(Monitor, Name) ->
  case maps:find(Name, Monitor#monitor.stations) of
    {ok, Station} -> {ok, Station};
    error -> {error, "No station with that name"}
  end.

findStationAtLocation(Monitor, {_, _} = Location) ->
  case maps:find(Location, Monitor#monitor.locations) of
    {ok, Name} -> findStationWithName(Monitor, Name);
    error -> {error, "No station at that location"}
  end.

safeDiv(_, Y) when Y == 0 -> 0;
safeDiv(X, Y) -> X / Y.