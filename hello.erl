-module(hello).
-author('olivier@biniou.info').

-include_lib("wx/include/gl.hrl").

-export([start/0, loop0/2]).
-export([on/0, off/0]).

-define(ZERO, 0.0).
-define(ONE,  1.0).

-record(state, {enabled=true, gl, rot=0, list}).


start() ->
    {Env, GL} = gui:env(),
    Pid = spawn(?MODULE, loop0, [Env, GL]),
    register(?MODULE, Pid),
    gui:connect(Pid).


on() ->
    ?MODULE ! enabled.
off() ->
    ?MODULE ! disabled.


%% TODO delete list upon exit
loop0(Env, GL) ->
    wx:set_env(Env),
    List = wxGLFontServer:makeList("Hello world!"),
    State = #state{gl=GL, list=List},
    loop(State).

loop(State) ->
    receive
	enabled ->
	    loop(State#state{enabled=true});

	disabled ->
	    loop(State#state{enabled=false});

	{Pid, {Ref, draw}} ->
	    draw(State),
	    Pid ! {Ref, ok},
	    NRot = (State#state.rot + 2) rem 360,
	    loop(State#state{rot=NRot})
    end.


draw(State) ->
    case State#state.enabled of
	false ->
	    ok;
	true ->
	    hello(State)
    end.

hello(State) ->
    wxGLCanvas:setCurrent(State#state.gl),

    gl:enable(?GL_LIGHT0),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_COLOR_MATERIAL),

    gl:disable(?GL_DEPTH_TEST),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE),
    gl:enable(?GL_BLEND),
    gl:enable(?GL_TEXTURE_2D),

    test1(State),

    gl:disable(?GL_LIGHTING),

    test2(State),

    gl:enable(?GL_DEPTH_TEST),
    gl:disable(?GL_TEXTURE_2D),
    gl:disable(?GL_BLEND).


test1(State) ->
    u:set_model_view(),
    gl:rotatef(State#state.rot, 1, 0, 0),
    gl:translatef(-1.0, -0.5, 0),

    String = "Erlang r0cks !",
    Size = wxGLFontServer:textSize(String),
    scale(Size),
    gl:color3ub(0, 255, 0),
    wxGLFontServer:render(String).


test2(State) ->
    u:set_model_view(),
    gl:rotatef(-45, 0, 1, 0),
    gl:translatef(-1.0, -0.5, 0),

    {Size, List} = State#state.list,
    scale(Size),

    gl:color3ub(0, 0, 255),
    gl:callList(List).    


scale({Width, Height}) ->
    gl:scalef(2.0/Width, 1.0/Height, 1.0).
    
