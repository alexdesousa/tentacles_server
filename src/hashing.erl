-module(hashing).

-export([md5/1, sha1/1]).

-spec md5(Data :: any()) -> string().
%% @doc Encrypts Erlang terms to a md5 hash.
md5(Data) ->
    general(md5, Data).

-spec sha1(Data :: any()) -> string().
%% @doc Encrypts Erlang terms to a md5 hash.
sha1(Data) ->
    general(sha, Data).

-spec general( EncryptionType :: atom()
             , Data           :: any()) -> string().
%% @doc General function to encrypt data.
general(Type, Data) ->
    Hash_list = binary_to_list(erlang:apply(crypto, Type, [term_to_binary(Data)])),
    lists:flatten(list_to_hex(Hash_list)).

-spec list_to_hex(Integers :: [integer()]) -> Hexadecimals when Hexadecimals :: [string()].
%% @doc Gets the hexadecimal representation of every integer in the list.
list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

-spec int_to_hex(Integer :: integer()) -> Hexadecimal when Hexadecimal :: string().
%% @doc Gets the hexadecimal representation of an integer.
int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

-spec hex(Decimal :: integer()) -> Hexadecimal when Hexadecimal :: char().
%% @doc Integer to Hex.
hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).
