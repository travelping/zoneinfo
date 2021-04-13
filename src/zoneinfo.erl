-module(zoneinfo).

-behavior(gen_server).

%% API
-export([start_link/0, load/1, add/2, find/3]).
-export([debug_zoneinfo/1, dump_treeish/2, tz_to_ranges/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("zoneinfo.hrl").

%% API functions
-ignore_xref([start_link/0, add/2, load/1, find/3]).

%% debug functions
-ignore_xref([debug_zoneinfo/1, dump_treeish/2, tz_to_ranges/2]).

-define(SERVER, ?MODULE).

-define(SECONDS_PER_DAY, 86400).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970*?SECONDS_PER_DAY)).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

load(File) ->
    {ok, Data} = file:read_file(File),
    ZoneInfo = parse_header_v0(Data),
    treeish(ZoneInfo).

find(TT, utc, #zoneinfo{utc = Treeish}) ->
    find_tt(Treeish, TT, []);
find(TT, local, #zoneinfo{local = Treeish}) ->
    find_tt(Treeish, TT, []).

debug_zoneinfo(ZoneInfo) ->
    io:format("TT:~n"),
    lists:foreach(
      fun({Secs, Design}) ->
	      DateTime = calendar:gregorian_seconds_to_datetime(Secs),
	      io:format("~p: ~s~n", [DateTime, fmt_tt_info(Design, ZoneInfo)])
      end, ZoneInfo#zoneinfo.tt),
    io:format("TTinfo: ~p~n", [ZoneInfo#zoneinfo.tt_info]),
    io:format("ZoneAbrev: ~p~n", [ZoneInfo#zoneinfo.zone_abrev]),
    io:format("LeapSec: ~p~n", [ZoneInfo#zoneinfo.leap_sec]),
    ok.

dump_treeish({_, _, _, Left, Right} = Me, ZoneInfo) ->
    dump_treeish(Left, ZoneInfo),
    print_tt(Me, ZoneInfo),
    dump_treeish(Right, ZoneInfo);
dump_treeish(undefined, _) ->
    ok.

print_tt({Start, End, Design, _, _}, ZoneInfo) ->
    From = calendar:gregorian_seconds_to_datetime(Start),
    To = calendar:gregorian_seconds_to_datetime(End),
    io:format("~p - ~p: ~s~n", [From, To, fmt_tt_info(Design, ZoneInfo)]).

add(Name, File) ->
    ZoneInfo = load(File),
    gen_server:call(?SERVER, {add, Name, ZoneInfo}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?MODULE, [ordered_set, private, {keypos, 1}]),
    {ok, #{}}.

handle_call({add, Name, ZoneInfo}, _From, State) ->
    Result = ets:insert(?MODULE, {Name, ZoneInfo}),
    {reply, Result, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_header_v0(<<"TZif", Version:8, _:15/bytes,
	       TTisutCnt:32, TTisstdCnt:32, LeapCnt:32,
	       TimeCnt:32, TypeCnt:32, CharCnt:32, Rest0/binary>>) ->
    <<TTbin:(TimeCnt * 4)/bytes,
      NextTTIdxBin:TimeCnt/bytes,
      TTinfoBin:(TypeCnt * 6)/bytes,
      ZoneAbrevBin:CharCnt/bytes,
      LeapSecBin:(LeapCnt * 8)/bytes,
      IsStdBin:TTisstdCnt/bytes,
      IsUtBin:TTisutCnt/bytes,
      Rest1/binary>> = Rest0,

    ZoneInfo =
	#zoneinfo{
	   tt = lists:zip([?SECONDS_FROM_0_TO_1970 + X || <<X:32/signed>> <= TTbin],
			  binary_to_list(NextTTIdxBin)),
       tt_info =
	   maps:from_list(
	     lists:zip(
	       lists:seq(0, TypeCnt - 1),
	       lists:zipwith3(
		 fun({UtOff, IsDst, DesigIdx}, IsStd, IsUt) ->
			 #ttinfo{
			    ut_off = UtOff,
			    is_dst = to_bool(IsDst),
			    desig_idx = DesigIdx,
			    is_std = to_bool(IsStd),
			    is_ut = to_bool(IsUt)}
		 end,
		 [{UtOff, IsDst, DesigIdx} ||
		     <<UtOff:32/signed, IsDst:8, DesigIdx:8>> <= TTinfoBin],
		 binary_to_list(IsStdBin),
		 binary_to_list(IsUtBin)))),
	   zone_abrev = parse_zone_abrev(0, ZoneAbrevBin, #{}),
	   leap_sec = [{Trans, Corr} || <<Trans:32, Corr:32/signed>> <= LeapSecBin]
	  },
    case Version of
	$2 -> parse_header_v2(Rest1);
	_ -> ZoneInfo
    end.

parse_header_v2(<<"TZif", $2:8, _:15/bytes,
	       TTisutCnt:32, TTisstdCnt:32, LeapCnt:32,
	       TimeCnt:32, TypeCnt:32, CharCnt:32, Rest0/binary>>) ->
    <<TTbin:(TimeCnt * 8)/bytes,
      NextTTIdxBin:TimeCnt/bytes,
      TTinfoBin:(TypeCnt * 6)/bytes,
      ZoneAbrevBin:CharCnt/bytes,
      LeapSecBin:(LeapCnt * 12)/bytes,
      IsStdBin:TTisstdCnt/bytes,
      IsUtBin:TTisutCnt/bytes,
      Rest1/binary>> = Rest0,

    io:format("Rest1: ~p~n", [Rest1]),
    {TZ, Rest2} = parse_tz(Rest1),
    io:format("TZ: ~p~nRest2: ~p~n", [TZ, Rest2]),

    #zoneinfo{
       tt = lists:zip([?SECONDS_FROM_0_TO_1970 + X || <<X:64/signed>> <= TTbin],
		      binary_to_list(NextTTIdxBin)),
       tt_info =
	   maps:from_list(
	     lists:zip(
	       lists:seq(0, TypeCnt - 1),
	       lists:zipwith3(
		 fun({UtOff, IsDst, DesigIdx}, IsStd, IsUt) ->
			 #ttinfo{
			    ut_off = UtOff,
			    is_dst = to_bool(IsDst),
			    desig_idx = DesigIdx,
			    is_std = to_bool(IsStd),
			    is_ut = to_bool(IsUt)}
		 end,
		 [{UtOff, IsDst, DesigIdx} ||
		     <<UtOff:32/signed, IsDst:8, DesigIdx:8>> <= TTinfoBin],
		 binary_to_list(IsStdBin),
		 binary_to_list(IsUtBin)))),
       zone_abrev = parse_zone_abrev(0, ZoneAbrevBin, #{}),
       leap_sec = [{Trans, Corr} || <<Trans:32, Corr:32/signed>> <= LeapSecBin],
       tz = TZ
      }.

parse_tz(<<"\n", Next/binary>> = V) ->
    case binary:split(Next, <<"\n">>) of
	[TZ, Rest] -> {parse_posix_tz(TZ), Rest};
	_          -> {undefined, V}
    end;
parse_tz(V) ->
    {undefined, V}.

parse_posix_tz(TZ) ->
    [ZoneAbrev|DateTimes] = binary:split(TZ, <<",">>, [global]),
    {parse_zone_abrev(ZoneAbrev), lists:map(fun parse_date_times/1, DateTimes)}.

parse_zone_abrev(ZoneAbrev) ->
    {Name0, Next0} = getzname(ZoneAbrev),
    {Offs0, Next1} = getoffset(Next0),
    case Next1 of
	<<>> ->
	    [{Name0, Offs0}];
	_ ->
	    {Name1, Next2} = getzname(Next1),
	    {Offs1, _Next} = getoffset(Next2),
	    [{Name0, Offs0}, {Name1, Offs1}]
    end.

getzname(<<"<", Next/binary>>) ->
    [Name, Next] = binary:split(Next, <<">">>),
    {Name, Next};
getzname(Bin) ->
    getzname(Bin, <<>>).

getzname(<<>>, Acc) ->
    {Acc, <<>>};
getzname(<<C:8, _/binary>> = Rest, Acc)
  when C == $-; (C >= $0 andalso C =< $9) ->
    {Acc, Rest};
getzname(<<C:8, Next/binary>>, Acc) ->
    getzname(Next, <<Acc/binary, C:8>>).

getoffset(<<>>) -> {3600, <<>>};
getoffset(Next) -> getoffset(Next, <<>>).

getoffset(<<C:8, Next/binary>>, Acc)
  when C == $:; C == $-; C == $+; (C >= $0 andalso C =< $9) ->
    getoffset(Next, <<Acc/binary, C:8>>);
getoffset(Rest, Acc) ->
    {-hms_to_secs(Acc), Rest}.

hms_to_secs(<<"-", HMS/binary>>) ->
    -hms_to_secs(binary:split(HMS, <<":">>, [global]), 3600, 0);
hms_to_secs(HMS) ->
    hms_to_secs(binary:split(HMS, <<":">>, [global]), 3600, 0).

hms_to_secs([], _, Secs) ->
    Secs;
hms_to_secs(_, 0, Secs) ->
    Secs;
hms_to_secs([H|T], Mult, Secs) ->
    hms_to_secs(T, Mult div 60, Secs + binary_to_integer(H) * Mult).

parse_date_times(DateTime) ->
    [Date|Time] = binary:split(DateTime, <<"/">>, [global]),
    {parse_tz_date(Date), parse_tz_time(Time)}.

parse_tz_date(<<"M", Date/binary>>) ->
    [M, N, D] = binary:split(Date, <<".">>, [global]),
    {binary_to_integer(M), binary_to_integer(N), binary_to_integer(D)};
parse_tz_date(<<"J", Date/binary>>) ->
    {'J', binary_to_integer(Date)};
parse_tz_date(Date) ->
    binary_to_integer(Date).

parse_tz_time([])     -> 7200;
parse_tz_time([Time]) -> hms_to_secs(Time).

tz_to_ranges(Year, {[{Zone, Offs}], _}) ->
    {calendar:datetime_to_gregorian_seconds({{Year, 1, 1}, {0, 0, 0}}) - Offs,
     calendar:datetime_to_gregorian_seconds({{Year + 1, 1, 1}, {0, 0, 0}}) - Offs,
     {Zone, Offs}}.

to_bool(Int) -> Int > 0.

fmt_tt_info(Design, ZoneInfo) ->
    #ttinfo{
       ut_off = UtOff, is_dst = IsDst, desig_idx = DesigIdx,
       is_std = IsStd, is_ut = IsUt} =
	maps:get(Design, ZoneInfo#zoneinfo.tt_info),
    io_lib:format("~s, isdst=~p gmtoff=~p isstd=~p isut=~p",
		  [maps:get(DesigIdx, ZoneInfo#zoneinfo.zone_abrev),
		   IsDst, UtOff, IsStd, IsUt]).

parse_zone_abrev(_, <<>>, ZoneAbrev) ->
    ZoneAbrev;
parse_zone_abrev(Pos, ZoneAbrevBin, ZoneAbrev) ->
    [Zone, Next] = binary:split(ZoneAbrevBin, <<0>>),
    parse_zone_abrev(Pos + size(Zone) + 1, Next, ZoneAbrev#{Pos => Zone}).

treeish(#zoneinfo{tt = TT, tt_info = Info} = ZoneInfo) ->
    ZoneInfo#zoneinfo{
      utc = cons_tree(ranges(TT)),
      local = cons_tree(adjust_local(Info, ranges(TT)))
     }.

adjust_local(Info, TT) ->
    lists:map(
      fun({Start, End, Design}) ->
	      #ttinfo{ut_off = Off} = maps:get(Design, Info),
	      {Start + Off, End + Off, Design}
      end, TT).

ranges([H|T]) ->
    ranges(H, T, []).

ranges({Start, Design}, [{End, _}], Ranges) ->
    [{Start, End, Design} | Ranges];
ranges({Start, Design}, [{End, _} = H | T], Ranges) ->
    [{Start, End, Design} | ranges(H, T, Ranges)].

find_tt({Start, End, Design, Left, Right}, TT, Acc) when TT > Start, TT < End ->
    find_tt(Right, TT, find_tt(Left, TT, [Design|Acc]));
find_tt({_Start, End, _Design, _Left, Right}, TT, Acc) when TT >= End ->
    find_tt(Right, TT, Acc);
find_tt({Start, _End, _Design, Left, _Right}, TT, Acc) when TT =< Start ->
    find_tt(Left, TT, Acc);
find_tt(_, _, Acc) ->
    Acc.

cons_tree([]) ->
    undefined;
cons_tree(L) ->
    {Left, Right} = lists:split(round(length(L)/2), L),
    [{Start, End, Design} | LeftSide] = lists:reverse(Left),
    {Start, End, Design, cons_tree(lists:reverse(LeftSide)),
cons_tree(Right)}.
