%%%-------------------------------------------------------------------
%%% File    : wxGLFontServer.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : OpenGL text rendering using wxFont
%%%
%%% Created : 27 Aug 2009 by Olivier <olivier@biniou.info>
%%%-------------------------------------------------------------------

%%
%%   I want wx_name for erlang based tools in wx
%%   and have wxName for real wxWidgets classes only.
%%   i.e to avoid future name clashes with real wxWidgets classes.
%%
-module(wx_glfont).

%% API
-export([start/1, stop/0]).
-export([load_font/1, load_font/2,
	 textSize/2, render/2, makeList/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(ZERO, 0.0).
-define(ONE,  1.0).

-define(TEXSIZE, 32). %% TODO make this parametrable

-record(state, {gl, fonts, count=0}).
-record(glyph, {w, tid}).
-record(font,  {wx, glyphs, height}).

%% TODO trap_exit to free textures on exit

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(GLContext) ->
    Env = wx:get_env(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Env, GLContext], []).

load_font(WxFont) ->
    load_font(WxFont, []).

load_font(WxFont, Options) ->
    gen_server:call(?SERVER, {load_font, WxFont, Options}).

stop() ->
    gen_server:cast(?SERVER, stop).

textSize(GLFont, String) ->
    gen_server:call(?SERVER, {textSize, GLFont, String}).

%% format(GLFont, Format, Args) ->
%%     gen_server:call(?SERVER, {format, GLFont, Format, Args}).

%% Remove the ones below, keep above
render(GLFont, String) ->
    gen_server:call(?SERVER, {render, GLFont, String}).

makeList(GLFont, String) ->
    gen_server:call(?SERVER, {makeList, GLFont, String}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Env, Context]) ->
    wx:set_env(Env),
    wxGLCanvas:setCurrent(Context),   
    {ok, #state{gl=Context, fonts=gb_trees:empty()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({load_font, Font, Options}, _From,
	    State = #state{fonts=Fs0, count=N}) ->
    case wxFont:ok(Font) of
	true ->
	    %% TODO SHOULD work with non-fixed fonts
	    true = wxFont:isFixedWidth(Font),
	    Glyphs = make_glyphs(Font),
	    Height = get_height(Font),
	    
	    F = #font{wx=Font, glyphs=Glyphs, height=Height},
	    Count = N+1,
	    Name = proplists:get_value(name, Options, Count),
	    Fs = gb_trees:insert(Name, F, Fs0), 
	    {reply, {ok, Name}, State#state{fonts=Fs, count=Count}};
	false ->
	    {error, not_ok}
    end;

handle_call({textSize, Fname,String}, _From, State=#state{fonts=Fs}) ->
    Font  = gb_trees:get(Fname, Fs),
    Reply = text_size(Font, String),
    {reply, Reply, State};

handle_call({render, Fname, String}, _From, State=#state{fonts=Fs}) ->
    Font  = gb_trees:get(Fname, Fs),
    Reply = render_text(Font, String),
    {reply, Reply, State};

handle_call({makeList, Fname, String}, _From, State=#state{fonts=Fs}) ->
    Font  = gb_trees:get(Fname, Fs),
    Reply = make_list(Font, String),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
make_glyphs(Font) ->
    make_glyphs(Font, 255, []).

make_glyphs(_Font, 0, Acc0) ->
    list_to_tuple(Acc0);
make_glyphs(Font, Char, Acc) ->
    Glyph = make_glyph(Font, Char),
    make_glyphs(Font, Char-1, [Glyph|Acc]).

make_glyph(_Font, Char) when Char < 32 ->
    undefined;
make_glyph(Font, Char) ->
    %% FIXME leaks here ? what should we destroy/1 ?

    MDC = memory_dc(Font),
    {Width, Height} = wxDC:getTextExtent(MDC, [Char]),

    Bitmap = wxBitmap:new(Width, Height),
    ok = wxMemoryDC:selectObject(MDC, Bitmap),

    BG = {0, 0, 0},
    Brush = wxBrush:new(BG, [{style, ?wxSOLID}]),
    wxMemoryDC:setBackground(MDC, Brush),
    wxMemoryDC:clear(MDC),

    FG = {255, 255, 255},
    wxMemoryDC:setTextForeground(MDC, FG),
    wxMemoryDC:drawText(MDC, [Char], {0, 0}),

    Image0 = wxBitmap:convertToImage(Bitmap),
    Image1 = wxImage:rescale(Image0, ?TEXSIZE, ?TEXSIZE),
    Image2 = wxImage:mirror(Image1, [{horizontally, false}]),

    BinData = wxImage:getData(Image2),

    make_glyph2(BinData, Width).

make_glyph2(null, _W) ->
    undefined;
make_glyph2(Data, Width) ->
    [TexId] = gl:genTextures(1),

    gl:bindTexture(?GL_TEXTURE_2D, TexId),

    %% gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),

    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),

    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB, ?TEXSIZE, ?TEXSIZE,
		  0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Data),

    #glyph{w=Width, tid=TexId}.

get_height(Font) ->
    MDC = memory_dc(Font),
    Height = wxMemoryDC:getCharHeight(MDC),
    wxMemoryDC:destroy(MDC),
    Height.

memory_dc(Font) ->
    MDC = wxMemoryDC:new(),
    wxMemoryDC:setFont(MDC, Font),
    MDC.

text_size(#font{wx=Font}, String) ->
    MDC = memory_dc(Font),
    Size = wxDC:getTextExtent(MDC, String),
    wxMemoryDC:destroy(MDC),
    Size.

render_text(Font, String) ->
    render_text(Font, String, 0.0).

render_text(_Font, [], _X) ->
    ok;
render_text(Font, [H|Tail], X0) ->
    Glyph = element(H, Font#font.glyphs),
    GWidth = Glyph#glyph.w,
    TexId = Glyph#glyph.tid,

    X1 = X0 + GWidth,
    Height = Font#font.height,
    tex_quad(TexId, X0, X1, Height),
    
    render_text(Font, Tail, X1).


make_list(Font, String) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    Width = make_list(Font, String, 0.0, 0),
    gl:endList(),
    {{Width, Font#font.height}, List}.

make_list(_Font, [], _X, Width) ->
    Width;
make_list(Font, [H|Tail], X0, Width) ->
    Glyph = element(H, Font#font.glyphs),
    GWidth = Glyph#glyph.w,
    TexId = Glyph#glyph.tid,

    X1 = X0 + GWidth,
    Height = Font#font.height,
    tex_quad(TexId, X0, X1, Height),

    NewWidth = Width + GWidth,

    make_list(Font, Tail, X1, NewWidth).


tex_quad(TexId, X0, X1, H) ->
    gl:bindTexture(?GL_TEXTURE_2D, TexId),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(?ZERO, ?ZERO), gl:vertex3f(X0, ?ZERO, ?ZERO),
    gl:texCoord2f(?ONE,  ?ZERO), gl:vertex3f(X1, ?ZERO, ?ZERO),
    gl:texCoord2f(?ONE,  ?ONE ), gl:vertex3f(X1,     H, ?ZERO),
    gl:texCoord2f(?ZERO, ?ONE ), gl:vertex3f(X0,     H, ?ZERO),
    gl:'end'().




