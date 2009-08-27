-module(u).
-author('olivier@biniou.info').

-include_lib("wx/include/gl.hrl").

-export([uts/0]).
-export([set_model_view/0]).


%% UNIX timestamp
uts() ->
    {M, S, _} = now(),
    1000000 * M + S.


%% set model view matrix to identity
set_model_view() ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity().

