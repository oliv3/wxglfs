-module(hello).
-author('olivier@biniou.info').

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-export([start/0, loop0/2]).
-export([on/0, off/0]).

-define(ZERO, 0.0).
-define(ONE,  1.0).

-record(state, {enabled=true, gl, rot=0, list, file}).


start() ->
    {Env, GL} = gui:env(),
    Pid = spawn_link(?MODULE, loop0, [Env, GL]),
    register(?MODULE, Pid),
    gui:connect(Pid).


on() ->
    ?MODULE ! enabled.
off() ->
    ?MODULE ! disabled.


%% TODO delete list upon exit
loop0(Env, GL) ->
    wx:set_env(Env),
    Font = wxFont:new(20, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ?wxFONTENCODING_ISO8859_15),
    {ok, teletype_20} = wx_glfont:load_font(Font, [{name, teletype_20}]),
    List = wx_glfont:makeList(teletype_20, "Hello world!"),
    {ok, File0} = file:read_file(?MODULE_STRING ++ ".erl"),
    File = re:split(File0, "\r?\n", [{return, list}]),
    State = #state{gl=GL, list=List, file=File},
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

    test3(State),
    
    gl:enable(?GL_DEPTH_TEST),
    gl:disable(?GL_TEXTURE_2D),
    gl:disable(?GL_BLEND).


test1(State) ->
    u:set_model_view(),
    gl:rotatef(State#state.rot, 1, 0, 0),
    gl:translatef(-1.0, -0.5, 0),

    String = "Erlang r0cks !",
    Size = wx_glfont:textSize(teletype_20, String),
    scale(Size),
    gl:color3ub(0, 255, 0),
    wx_glfont:render(teletype_20, String).


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
    
test3(#state{gl=GL, file=File}) ->
    gl:color3ub(255, 255, 255),
    {W,H} = wxWindow:getSize(GL),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:ortho2D(0, W, 0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),
    TextH = float(wx_glfont:height(teletype_20)),
    gl:translatef(30.0, H-30.0-TextH, 0.0),
    wx_glfont:render(teletype_20, "This is teletype_20"),
    lists:foreach(fun(Row) ->
			  gl:translatef(0.0, -TextH, 0.0),
			  wx_glfont:render(teletype_20, Row)
		  end, File),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW).

    
