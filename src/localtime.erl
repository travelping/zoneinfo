-module(localtime).

-export([from_local_time/1, from_local_time/2, from_local_time/3,
	 from_universal_time/1, from_universal_time/2,
	 to_local_time/1, to_local_time/2,
	 to_universal_time/1,
	 to_rfc3339/1,
	 tz_offset/1,
	 tz_name/1
	]).

-include("zoneinfo.hrl").

%% API functions
-ignore_xref([from_local_time/1, from_local_time/2, from_local_time/3,
	      from_universal_time/1, from_universal_time/2,
	      to_local_time/1, to_local_time/2,
	      to_universal_time/1,
	      to_rfc3339/1,
	      tz_name/1,
	      tz_offset/1
	     ]).

%%%===================================================================
%%% API
%%%===================================================================

from_local_time(DateTime) ->
    Zone = os:getenv("TZ", "localtime"),
    from_local_time(DateTime, Zone).

from_local_time(DateTime, ZoneInfo) ->
    from_local_time(DateTime, undefined, ZoneInfo).

from_local_time(DateTime, TZ, ZoneInfo)
  when is_record(ZoneInfo, zoneinfo) ->
    from_local_time_(DateTime, TZ, ZoneInfo);
from_local_time(DateTime, TZ, Name) ->
    {ok, ZoneInfo} = zoneinfo:get(Name),
    from_local_time_(DateTime, TZ, ZoneInfo).

from_universal_time(DateTime) ->
    from_universal_time(DateTime, "Etc/UTC").

from_universal_time(DateTime, ZoneInfo)
  when is_record(ZoneInfo, zoneinfo) ->
    from_universal_time_(DateTime, ZoneInfo);
from_universal_time(DateTime, Name) ->
    {ok, ZoneInfo} = zoneinfo:get(Name),
    from_universal_time_(DateTime, ZoneInfo).

to_local_time({DateTime, Off, _Name}) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(DateTime) + Off).

to_local_time({DateTime, _, _}, ZoneInfo) ->
    from_universal_time(DateTime, ZoneInfo).

to_universal_time({DateTime, _, _}) ->
    DateTime.

to_rfc3339(Time) ->
    {{Y, M, D}, {H, Min, S}} = to_local_time(Time),
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w~s",
		  [Y, M, D, H, Min, S, tz(tz_offset(Time))]).

tz_offset({_, Offset, _}) -> Offset.
tz_name({_, _, Name}) -> Name.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tz(0) ->
    "Z";
tz(I) when I < 0 ->
    io_lib:format("-~2..0w:~2..0w", [I div 3600, (I rem 3600) div 60]);
tz(I) ->
    io_lib:format("+~2..0w:~2..0w", [I div 3600, (I rem 3600) div 60]).

from_local_time_(DateTime, TZ, ZoneInfo) ->
    Secs = calendar:datetime_to_gregorian_seconds(DateTime),
    Times0 =
	lists:map(fun(Design) -> mk_local_time(Design, Secs, ZoneInfo) end,
		  zoneinfo:find(Secs, local, ZoneInfo)),
    Times = case TZ of
		 undefined ->
		     Times0;
		 _ ->
		     lists:filter(fun({_, _, Name}) -> Name == TZ end, Times0)
	     end,
    case Times of
	[]     -> false;
	[Time] -> Time;
	List   -> List
    end.

mk_local_time(Design, Secs, #zoneinfo{tt_info = Info, zone_abrev = Abrev}) ->
    #ttinfo{ut_off = Off, desig_idx = Idx} = maps:get(Design, Info),
    Name = maps:get(Idx, Abrev),
    {calendar:gregorian_seconds_to_datetime(Secs - Off), Off, Name}.

from_universal_time_(DateTime, #zoneinfo{tt_info = Info, zone_abrev = Abrev} = ZoneInfo) ->
    Secs = calendar:datetime_to_gregorian_seconds(DateTime),
    case zoneinfo:find(Secs, utc, ZoneInfo) of
	[Design] ->
	    #ttinfo{ut_off = Off, desig_idx = Idx} = maps:get(Design, Info),
	    Name = maps:get(Idx, Abrev),
	    {DateTime, Off, Name};
	_ ->
	    false
    end.
