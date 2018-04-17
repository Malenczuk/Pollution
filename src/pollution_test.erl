%%%-------------------------------------------------------------------
%%% @author Marcin Maleńczuk
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("Marcin Maleńczuk").

-include_lib("eunit/include/eunit.hrl").
-include("pollution_rec.hrl").
-compile(export_all).

createMonitor_test() ->
  ?assertEqual({ok, {monitor, #{}}}, pollution:createMonitor()).

addStation_test() ->
  {ok, Monitor} = pollution:createMonitor(),
  Station1 = #station{name = "1", location = {1, 1}},
  Station2 = #station{name = "2", location = {2, 2}},

  {ok, Monitor1} = pollution:addStation(Monitor, "1", {1, 1}),

  ?assertEqual(
    Station1,
    maps:get({"1", {1, 1}}, Monitor1#monitor.stations)
  ),

  {ok, Monitor2} = pollution:addStation(Monitor1, "2", {2, 2}),

  ?assertEqual(
    #{{"1", {1, 1}} => Station1, {"2", {2, 2}} => Station2},
    Monitor2#monitor.stations
  ),

  ?assertEqual(
    {error, "Station already exists"},
    pollution:addStation(Monitor2, "2", {2, 2})
  ).

findStation_test() ->
  {ok, Monitor} = pollution:createMonitor(),
  Station1 = #station{name = "1", location = {1, 1}},
  {ok, Monitor1} = pollution:addStation(Monitor, "1", {1, 1}),

  ?assertEqual(
    {ok, Station1},
    pollution:findStation(Monitor1, "1")
  ),

  ?assertEqual(
    {ok, Station1},
    pollution:findStation(Monitor1, {1, 1})
  ),

  ?assertEqual(
    {error, "No station with that name"},
    pollution:findStation(Monitor1, "2")
  ),

  ?assertEqual(
    {error, "No station at that location"},
    pollution:findStation(Monitor1, {2, 2})
  ).


addValue_test() ->
  Time = calendar:local_time(),
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "1", {1, 1}),
  {ok, Monitor2} = pollution:addValue(Monitor1, "1", Time, "pm10", 50),

  ?assertEqual(
    {monitor,
      #{{"1", {1, 1}} =>
      {station, "1",
        {1, 1},
        #{{Time, "pm10"} =>
        {measurement, "pm10", 50, Time}}}}},
    Monitor2
  ),

  ?assertEqual(
    {error, "Measurement of that type already exists at that date"},
    pollution:addValue(Monitor2, "1", Time, "pm10", 75)
  ),

  ?assertEqual(
    {error, "No station with that name"},
    pollution:addValue(Monitor2, "2", Time, "pm10", 50)
  ),

  ?assertEqual(
    {error, "No station at that location"},
    pollution:addValue(Monitor2, {2, 2}, Time, "pm10", 50)
  ).

removeValue_test() ->
  DateTime = calendar:local_time(),
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "1", {1, 1}),
  {ok, Monitor2} = pollution:addValue(Monitor1, "1", DateTime, "pm10", 50),
  {ok, Monitor3} = pollution:removeValue(Monitor2, "1", DateTime, "pm10"),

  ?assertEqual(
    {monitor,
      #{{"1", {1, 1}} =>
      {station, "1",
        {1, 1},
        #{}}}},
    Monitor3
  ),

  ?assertEqual(
    {error, "Measurement of that type doesn't exists at that date"},
    pollution:removeValue(Monitor2, "1", DateTime, "pm25")
  ).

getOneValue_test() ->
  DateTime = calendar:local_time(),
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "1", {1, 1}),
  {ok, Monitor2} = pollution:addValue(Monitor1, "1", DateTime, "pm10", 50),

  ?assertEqual(
    {ok, 50},
    pollution:getOneValue(Monitor2, "1", DateTime, "pm10")
  ),

  ?assertEqual(
    {error, "Measurement of that type doesn't exists at that date"},
    pollution:getOneValue(Monitor2, "1", DateTime, "pm25")
  ).

getStationMean_test() ->
  {Date, {H, M, S}} = calendar:local_time(),
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "1", {1, 1}),
  {ok, Monitor2} = pollution:addStation(Monitor1, "2", {2, 2}),
  {ok, Monitor3} = pollution:addValue(Monitor2, "1", {Date, {H, M, S}}, "pm10", 50),
  {ok, Monitor4} = pollution:addValue(Monitor3, "1", {Date, {H + 1, M, S}}, "pm10", 60),
  {ok, Monitor5} = pollution:addValue(Monitor4, "1", {Date, {H + 2, M, S}}, "pm10", 70),
  {ok, Monitor6} = pollution:addValue(Monitor5, "1", {Date, {H + 2, M, S}}, "pm25", 50),
  {ok, Monitor7} = pollution:addValue(Monitor6, "2", {Date, {H, M, S}}, "pm10", 50),

  ?assertEqual(
    {ok, 60.0},
    pollution:getStationMean(Monitor7, "1", "pm10")
  ).

getStationDailyMean_test() ->
  {{Y, M, D}, Time} = calendar:local_time(),
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "1", {1, 1}),
  {ok, Monitor2} = pollution:addValue(Monitor1, "1", {{Y, M, D}, Time}, "pm10", 50),
  {ok, Monitor3} = pollution:addValue(Monitor2, "1", {{Y, M, D}, Time}, "pm25", 70),
  {ok, Monitor4} = pollution:addValue(Monitor3, "1", {{Y, M, D + 1}, Time}, "pm10", 60),
  {ok, Station} = pollution:findStation(Monitor4, "1"),

  ?assertEqual(
    {ok, 50.0},
    pollution:getStationDailyMean(Station, {Y, M, D}, "pm10")
  ).

getDailyMean_test() ->
  {{Y, M, D}, Time} = calendar:local_time(),
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "1", {1, 1}),
  {ok, Monitor2} = pollution:addStation(Monitor1, "2", {2, 2}),
  {ok, Monitor3} = pollution:addValue(Monitor2, "1", {{Y, M, D}, Time}, "pm10", 50),
  {ok, Monitor4} = pollution:addValue(Monitor3, "1", {{Y, M, D}, Time}, "pm25", 90),
  {ok, Monitor5} = pollution:addValue(Monitor4, "1", {{Y, M, D + 1}, Time}, "pm10", 60),
  {ok, Monitor6} = pollution:addValue(Monitor5, "2", {{Y, M, D}, Time}, "pm25", 60),

  ?assertEqual(
    {ok, 75.0},
    pollution:getDailyMean(Monitor6, {Y, M, D}, "pm25")
  ).

  

getMinMaxValue_test() ->
  {Date, {H, M, S}} = calendar:local_time(),
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "1", {1, 1}),
  {ok, Monitor2} = pollution:addStation(Monitor1, "2", {2, 2}),
  {ok, Monitor3} = pollution:addValue(Monitor2, "1", {Date, {H, M, S}}, "pm10", 50),
  {ok, Monitor4} = pollution:addValue(Monitor3, "1", {Date, {H + 1, M, S}}, "pm10", 60),
  {ok, Monitor5} = pollution:addValue(Monitor4, "1", {Date, {H + 2, M, S}}, "pm10", 70),
  {ok, Monitor6} = pollution:addValue(Monitor5, "1", {Date, {H + 2, M, S}}, "pm25", 50),
  {ok, Monitor7} = pollution:addValue(Monitor6, "2", {Date, {H, M, S}}, "pm10", 100),

  ?assertEqual(
    {ok, {50, 70}},
    pollution:getMinMaxValue(Monitor7, "1", Date ,"pm10")
  ),

  ?assertEqual(
    {ok, {nomeasurement, nomeasurement}},
    pollution:getMinMaxValue(Monitor7, "2", Date ,"pm25")
  ).