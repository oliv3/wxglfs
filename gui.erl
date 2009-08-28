-module(gui).
-author('olivier@biniou.info').

-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl").

-export([start/0, stop/0, init/0]).
-export([env/0, connect/1]).

-define(WIDTH,  640).
-define(HEIGHT, 480).
-define(FPS,    25).
-define(IFPS,   trunc(1000/?FPS)).

-record(state, {
	  frame,
	  gl, 
	  connected=[],
	  zmax = 1000000.0, %% FIXME
	  fov = 50,
	  rot = {30, 56, 0},
	  mouse
	 }).


start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid).


init() ->
    Wx = wx:new(),
    {Frame, GL} = wx:batch(fun() -> create_window(Wx) end),
    wxWindow:show(Frame),

    %% fs:start(wx:get_env(), GL),
    wx_glfont:start(GL),

    State = #state{frame=Frame, gl=GL},
    loop(State),
    stop().


stop() ->
    wx_glfont:stop(),
    %% fs:stop(),
    wx:destroy(),
    init:stop().


create_window(Wx) ->
    Frame = wxFrame:new(Wx, ?wxID_ANY, "wxGLFontServer demo",
			[{pos, {0, 0}}, {size, {?WIDTH, ?HEIGHT}},
			 {style, ?wxDEFAULT_FRAME_STYLE}]),
    
    %% wxFrame:setIcon(Frame, wxIcon:new("wxwin.ico")),
    Opts = [{size, {?WIDTH, ?HEIGHT}}],
    GLAttrib = [{attribList, [?WX_GL_RGBA, ?WX_GL_DOUBLEBUFFER, ?WX_GL_DEPTH_SIZE, 24, 0]}],
    GL = wxGLCanvas:new(Frame, Opts ++ GLAttrib),

    wxFrame:connect(Frame, enter_window),
    wxFrame:connect(Frame, close_window),
    %% wxFrame:connect(Frame, key_up),
    wxFrame:connect(GL,    left_down),
    wxFrame:connect(GL,    mousewheel),
    wxFrame:connect(GL,    motion),
    
    {Frame, GL}.


env() ->
    Ref = make_ref(),
    ?MODULE ! {self(), {Ref, env}},
    receive
	{Ref, Env} ->
	    Env
    end.


connect(Pid) ->
    ?MODULE ! {connect, Pid}.


loop(State) ->
    receive
	{connect, Client} ->
	    NewConn = [Client|State#state.connected],
	    loop(State#state{connected=NewConn});
	    
	{Pid, {Ref, env}} ->
	    Pid ! {Ref, {wx:get_env(), State#state.gl}},
	    loop(State);

	#wx{event=#wxClose{}} ->
	    ok;

	#wx{event=#wxMouse{} = MouseEvent} ->
	    NewState = handle_mouse(State, MouseEvent),
	    loop(NewState);

	M ->
	    io:format("[~s] got M= ~p~n", [?MODULE, M]),
	    loop(State)

    after 0 ->
	    %% TODO timer draw/1 et ajuster sleep/0 en consequence
	    draw(State),
	    sleep(),
	    loop(State)
    end.
	    

sleep() ->
    timer:sleep(?IFPS).


draw(State) ->
    GL = State#state.gl,
    wxGLCanvas:setCurrent(GL),
    set_view(State),
    draw_scene(State#state.connected),
    wxGLCanvas:swapBuffers(GL).


draw_scene([]) ->
    ok;
draw_scene(List) ->
    F = fun(Pid) ->
		Ref = make_ref(),
		Pid ! {self(), {Ref, draw}},
		receive
		    {Ref, ok} ->
			ok
		end
	end,
    lists:foreach(F, List).


set_view(State) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:enable(?GL_BLEND),

    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_DEPTH_TEST),
    gl:clearColor(1.0,0.0,0.0,1.0),
    gl:clearDepth(State#state.zmax),

    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),

    glu:perspective(State#state.fov, ?WIDTH/?HEIGHT, 0.1, State#state.zmax),
    %% GL_PERSPECTIVE_CORRECTION_HINT = 3152 (0xc50)
    %% GL_NICEST = 4354 (0x1102)
    %% gl:hint(3152, 4354),

    glu:lookAt(0, 0, 3.14,
	       0, 0, -3.14,
	       0, 1, 0),

    {RotX, RotY, RotZ} = State#state.rot,
    gl:rotatef(RotX, 1.0, 0.0, 0.0),
    gl:rotatef(RotY, 0.0, 1.0, 0.0),
    gl:rotatef(RotZ, 0.0, 0.0, 1.0),
    
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT).


handle_mouse(State, #wxMouse{type=enter_window}) ->
    wxFrame:setFocus(State#state.gl),
    State;
handle_mouse(State, #wxMouse{type=left_down, x=X, y=Y}) ->
    State#state{mouse={X, Y}};
handle_mouse(State, #wxMouse{type=motion, leftDown=true, x=X, y=Y}) ->
    {OldX, OldY} = State#state.mouse,
    DX = X - OldX,
    DY = Y - OldY,
    {RX, RY, RZ} = State#state.rot,
    NRX = trunc(RX+DY+360) rem 360,
    NRY = trunc(RY+DX+360) rem 360,
    NRot = {NRX, NRY, RZ},
    State#state{rot=NRot, mouse={X, Y}};
handle_mouse(State, #wxMouse{type=motion}) ->
    State;
handle_mouse(State, #wxMouse{type=mousewheel, wheelRotation=R}) when R < 0 ->
    NewFOV = State#state.fov + 1,
    State#state{fov = NewFOV};
handle_mouse(State, #wxMouse{type=mousewheel}) ->
    NewFOV = State#state.fov - 1,
    State#state{fov = NewFOV};
handle_mouse(State, _Event) ->
    io:format("[~s] got Other: ~p~n", [?MODULE, _Event]),
    State.
