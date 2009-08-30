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
-export([load_font/1, load_font/2,
	 text_size/2, height/1, render/2, make_list/2]).

-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-record(glyph, {u, v, w}).
-record(font,  {wx, tex, glyphs, height, ih, iw}).

-define(F32, 32/float-native).

%% TODO trap_exit to free textures on exit

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

load_font(WxFont) ->
    load_font(WxFont, []).

load_font(WxFont, Options) ->
    case wxFont:ok(WxFont) of
	true ->
	    {Height, AWidth} = get_height(WxFont), 
	    GLFont = gen_glfont(WxFont, AWidth, Height, Options),
	    {ok, GLFont};
	false ->
	    {error, not_ok}
    end.

text_size(#font{wx=Font}, String) ->
    MDC = memory_dc(Font),
    Size = wxDC:getTextExtent(MDC, String),
    wxMemoryDC:destroy(MDC),
    Size.

height(#font{height=Height}) ->
    Height.

%% format(GLFont, Format, Args) ->
%%     gen_server:call(?SERVER, {format, GLFont, Format, Args}).

%% Remove the ones below, keep above
render(#font{} = GLFont, String) ->
    render_text(GLFont, String).

make_list(#font{} = Font, String) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    Res = render_text2(Font, String),
    gl:endList(),
    {Res, List}.    

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
gen_glfont(Font, AW, H, Options) ->
    {From, To} = proplists:get_value(range, Options, {32, 256}),
    NoChars = To - From,
    {TW,TH} = calc_tex_size(NoChars, AW, H), 
    From = erlang:min(From, To), %% Assert range    
    
    {Bin, Glyphs} = make_glyphs(Font,From,To,TW,TH),
    TexId = gen_texture(TW,TH,Bin,Options),
    
    #font{wx=Font, tex=TexId, glyphs=Glyphs, height=H, ih=H/TH, iw=1/TW}.

make_glyphs(Font,From,To,TW,TH) ->
    MDC = memory_dc(Font),
    Bitmap = wxBitmap:new(TW, TH),
    ok = wxMemoryDC:selectObject(MDC, Bitmap),

    BG = {0, 0, 0, 0},
    Brush = wxBrush:new(BG, [{style, ?wxSOLID}]),
    wxMemoryDC:setBackground(MDC, Brush),
    wxMemoryDC:clear(MDC),

    FG = {255, 255, 255, 255},
    wxMemoryDC:setTextForeground(MDC, FG),    
    Glyphs = make_glyphs(MDC, From, To, 0, 0, TW, TH, array:new()),
    Image0 = wxBitmap:convertToImage(Bitmap),
    debug(Image0),
    %%Image1 = wxImage:rescale(Image0, ?TEXSIZE, ?TEXSIZE),
    %%Image2  = wxImage:mirror(Image0, [{horizontally, false}]),    
    BinData = wxImage:getData(Image0),
    %% BinData = wxImage:getAlpha(Image2),
    {BinData, Glyphs}.

make_glyphs(DC, From, To, X, Y, TW, TH, Acc0)
  when From < To ->
    {Acc,Xp,Yp} = make_glyph(DC, From, X, Y, TW, TH, Acc0),
    make_glyphs(DC, From+1, To, Xp, Yp, TW, TH, Acc);
make_glyphs(_DC, _From, _To, _X, _Y, _TW, _TH, Acc) ->
    Acc.

make_glyph(DC, Char, X0, Y0,  TW, TH, Acc0) ->
    {Width, Height} = wxDC:getTextExtent(DC, [Char]),
    Xt = X0+Width,
    true = (Y0 + Height) =< TH, %% Assert that we fit inside texture

    case Xt > TW of
	true -> 
	    X = Width,  Y = Y0+Height,
	    X1 = 0, Y1 = Y;
	false ->
	    X = Xt,  Y = Y0,
	    X1 = X0, Y1 = Y0	     
    end,
    wxMemoryDC:drawText(DC, [Char], {X1, Y1}),
    G = #glyph{w=Width, u=X1/TW, v=(Y1)/TH},
    {array:set(Char, G, Acc0), X+1, Y}.
    

gen_texture(TW,TH,Bin,Options) ->
    [TexId] = gl:genTextures(1),

    gl:bindTexture(?GL_TEXTURE_2D, TexId),

    %% gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),

    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),

    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB, TW, TH,
		  0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Bin),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    TexId.

get_height(Font) ->
    MDC = memory_dc(Font),
    Height = wxMemoryDC:getCharHeight(MDC),
    AverW  = wxMemoryDC:getCharWidth(MDC),
    wxMemoryDC:destroy(MDC),
    {Height, AverW}.

memory_dc(Font) ->
    MDC = wxMemoryDC:new(),
    wxMemoryDC:setFont(MDC, Font),
    MDC.

render_text(Font, String) ->
    render_text2(Font, String),
    ok.

render_text2(#font{tex=TexId, glyphs=Gs, height=H, ih=IH, iw=IW}, String) ->
    {Size, Bin} = render_text3(String, Gs, IH, IW, H, {0, <<>>}),
    case Bin of
	<<>> -> ok;	
	<<_:2/unit:32, TxBin/bytes>> ->
	    gl:bindTexture(?GL_TEXTURE_2D, TexId),
	    gl:vertexPointer(2, ?GL_FLOAT, 16, Bin),
	    gl:texCoordPointer(2, ?GL_FLOAT, 16, TxBin),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
	    gl:drawArrays(?GL_QUADS, 0, byte_size(Bin) div 16),
	    gl:disableClientState(?GL_VERTEX_ARRAY),
	    gl:disableClientState(?GL_TEXTURE_COORD_ARRAY)
    end,
    Size.
    
render_text3([], _Gs, _IH, _IW, H, {W, Bin}) ->
    {{W,H},Bin};
render_text3([Char|String], Gs, IH, IW, H, Data0) ->
    case array:get(Char, Gs) of
	undefined when Char =:= 9 -> %% TAB
	    Space = array:get(32, Gs),
	    Data = lists:foldl(fun(_, Data) ->
				       render_glyph(Space,H,IW,IH,Data)
			       end, Data0, "        "),
	    render_text3(String, Gs, IH, IW, H, Data);
	undefined -> %% Should we render something strange here
	    render_text3(String, Gs, IH, IW, H, Data0);
	Glyph ->
	    %%io:format("~s ~p~n",[[Char], Glyph]),
	    Data = render_glyph(Glyph,H,IW,IH,Data0), 
	    render_text3(String, Gs, IH, IW, H, Data)
    end.

render_glyph(#glyph{u=U,v=V,w=W},H,IW,IH, {X0,Bin}) -> 
    X1 = X0 + W,
    UD = U + (W-1)*IW,
    VD = V + IH,
    {X1,
     <<Bin/binary,         %% wxImage: 0,0 is upper left turn each 
      X0:?F32,0:?F32, U:?F32, VD:?F32, % Vertex lower left, UV-coord up-left
      X1:?F32,0:?F32, UD:?F32,VD:?F32, % Vertex lower right,UV-coord up-right
      X1:?F32,H:?F32, UD:?F32, V:?F32, % Vertex upper right,UV-coord down-right
      X0:?F32,H:?F32, U:?F32,  V:?F32  % Vertex upper left, UV-coord down-left
      >>
    }.

%% render_glyph(#glyph{u=U,v=V,w=W},X0,H,IW,IH) -> 
%%     X1 = X0 + W,
%%     UD = U + (W-1)*IW,
%%     VD = V + IH,
%%     gl:texCoord2f(U,  VD),  gl:vertex2i(X0, 0),
%%     gl:texCoord2f(UD, VD),  gl:vertex2i(X1, 0),
%%     gl:texCoord2f(UD, V), gl:vertex2i(X1, H),
%%     gl:texCoord2f(U,  V), gl:vertex2i(X0, H),
%%     X1.

calc_tex_size(No, CW, CH) ->
    %% Add some extra chars to be sure it fits.
    calc_tex_size(No+5, 1, No+5, CW, CH, {undefined, undefined}, undefined).

calc_tex_size(X, Y, No, CW, CH, Prev = {BestArea,Dec}, BestCoord)
  when Y =< No ->
    Xp = tsize(X*CW),
    Yp = tsize(Y*CH),
    Area = Xp * Yp,
    Square = abs(Xp - Yp),
    NextX = ((No-1) div (Y+1)) + 1,
    if Area < BestArea ->
	    %%io:format("Best is ~p ~p ~p ~n", [Area, Xp,Yp]),
	    calc_tex_size(NextX, Y+1, No, CW, CH, {Area,Square}, {Xp,Yp});
       Area == BestArea, Square < Dec ->
	    %%io:format("Best is ~p ~p ~p ~n", [Area, Xp,Yp]),
	    calc_tex_size(NextX, Y+1, No, CW, CH, {Area,Square}, {Xp,Yp});
       true ->
	    calc_tex_size(NextX, Y+1, No, CW, CH, Prev, BestCoord)
    end;
calc_tex_size(_, _, _, _, _, _, BestCoord) ->
    BestCoord.
    

floor(T,N) ->
    X = T div N,
    if (T rem N) =:= 0 ->
	    X;
       true -> 
	    X+1
    end.

tsize(X0) ->
    Pow = trunc(log2(X0)),
    case (1 bsl Pow) of
	X0 -> X0;
	_ -> 1 bsl (Pow+1)
    end.
  
log2(X) ->
    math:log(X) / math:log(2).
    

check_pow2(NoX, W, Pow2) when NoX * W > Pow2 ->
    check_pow2(NoX, W, Pow2*2);
check_pow2(NoX, W, Pow2) ->
    {trunc((Pow2 - NoX*W)/W), Pow2}.


debug(Image) ->
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "DEBUG",
			[{size, {600, 600}}]),
    Panel = wxPanel:new(Frame),
    Paint = fun(_,_) ->	    
		    DC=wxPaintDC:new(Panel),
		    Bmp = wxBitmap:new(Image),
		    wxDC:drawBitmap(DC, Bmp, {0,0}),
		    wxPaintDC:destroy(DC),
		    wxBitmap:destroy(Bmp)
	    end,
    wxFrame:connect(Panel, paint, [{callback, Paint}]),
    wxFrame:show(Frame).
