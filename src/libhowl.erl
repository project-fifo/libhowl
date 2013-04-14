-module(libhowl).

-export([
         start/0,
         servers/0
        ]).

-export([
         send/2,
         send/1,
         version/0
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
%% @private
%% @doc Fetches version
%% @spec version() -> binary
%% @end
%%--------------------------------------------------------------------

-spec version() ->
                     binary() |
                     {error, no_servers}.

version() ->
    ServerVersion = call(version),
    ServerVersion.

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

-spec send(Channel::fifo:uuid(), Message::fifo:object()) -> ok.
send(Channel, Message) ->
    send_({msg, Channel, Message}).

%%--------------------------------------------------------------------
%% @doc Sends a message to a channel.
%% @spec send([{Channel::term(), Message::term()}]) -> ok
%% @end
%%--------------------------------------------------------------------

-spec send([{Channel::fifo:uuid(), Message::fifo:object()}]) -> ok.

send(Messages) ->
    send_({msg, Messages}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Sends a message.
%% @spec send(Msg::term()) -> {ok, Reply::term()} | {error, no_server}
%% @end
%%--------------------------------------------------------------------

-spec send_(Msg::term()) -> ok.
send_(Msg) ->
    libhowl_server:cast(Msg).

-spec call(Msg::fifo:howl_message()) ->
                  atom() |
                  {ok, Reply::term()} |
                  {error, no_server}.
call(Msg) ->
    case libhowl_server:call(Msg) of
        {reply, Reply} ->
            Reply;
        E ->
            E
    end.
