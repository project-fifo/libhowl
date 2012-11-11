-module(libhowl).

-export([
	 start/0, 
	 servers/0
	]).


-export([
	 send/2
	]).

%%%===================================================================
%%% Generatl Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Starts the service.
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------

-spec start() -> ok.
start() ->
    application:start(mdns_client_lib),
    application:start(libhowl).

%%--------------------------------------------------------------------
%% @doc Gets a list of servers
%% @spec servers() -> [term()]
%% @end
%%--------------------------------------------------------------------

-spec servers() -> [term()].

servers() ->
    libhowl_server:servers().

%%--------------------------------------------------------------------
%% @doc Sends a message to a channel.
%% @spec send(Channel::term(), Message::term()) -> ok
%% @end
%%--------------------------------------------------------------------

-spec send(Channel::term(), Message::term()) -> 
		  {ok, Token::{token, binary()}} |
		  {error, not_found}.

send(Channel, Message) ->
    send({msg, Channel, Message}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Sends a message.
%% @spec send(Msg::term()) -> {ok, Reply::term()} | {error, no_server}
%% @end
%%--------------------------------------------------------------------

-spec send(Msg::term()) -> {ok, Reply::term()} | {error, no_server}.
send(Msg) ->
    libhowl_server:cast(Msg).
