%%%-------------------------------------------------------------------
%%% File    : wx_glfont.erl
%%% Author  : Olivier <olivier@biniou.info> 
%%%           Dan Gudmundsson <dgud@erlang.org>
%%% Description : OpenGL text rendering using wxFont
%%%
%%% Created : 27 Aug 2009 by Olivier <olivier@biniou.info>
%%%-------------------------------------------------------------------
%%%
%%% @doc A api for generating textures and rendering strings via opengl.
%%%
%%% The 'load_font' functions loads and generates a texture from an
%%% #wxFont{}. The textures are white on black with an alpha channel.
%%% The default mode for the texture created is ?GL_MODULATE.
%%% That way you can use gl:color3x to specify the color of the text.
%%% 
%%% This is a low level interface a library the intention is that 
%%% you should be able to use it to build a text rendering interface
%%% above that changes fonts and colors and maybe even sizes.

-module(wx_glfont).

%% API
-export([load_font/1, load_font/2,
	 text_size/2, height/1, tex_id/1,
	 render/2, render_to_list/2, 
	 render_to_binary/2, render_to_binary/3]).

-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-record(glyph, {u, v, w}).
-record(font,  {wx, tex, glyphs, height, ih, iw}).

-define(F32, 32/float-native).
-define(SPACE_X, 3).  % Space between chars
-define(SPACE_Y, 2).  % Space between rows
-define(BIN_XTRA, 0). %% Safe off-heap alloc


%%====================================================================
%% API
%%====================================================================

%-type font_info() :: opaque().

%% @spec(::wxFont()) -> font_info() | {error, Reason}
%% @equiv(load_font(WxFont, [])
load_font(WxFont) ->
    load_font(WxFont, []).

%% @spec(::wxFont(), [Options]) -> font_info() | {error, Reason}
%% Option = {range, [{CS1,CE1},...]} | 
%%              {tex_mode,GL_MODE} | 
%%              {tex_min, MinFilter} | {tex_mag, MagFilter}
%%              {tex_wrap_s, WrapS}  | {tex_wrap_s, WrapS}
%%              {tex_gen_mipmap, ?GL_TRUE|?GL_FALSE}
%% @desc Prepare a font to be used in a wxGLCANVAS.
%% Renders each character in the range into a texture.
%% Range is a list of unicode code points ranges to be used when 
%% rendering, default [{32, 256}].
load_font(WxFont, Options) ->
    case wxFont:ok(WxFont) of
	true ->
	    {Height, AWidth} = get_height(WxFont), 
	    GLFont = gen_glfont(WxFont, AWidth, Height, Options),
	    {ok, GLFont};
	false ->
	    {error, not_ok}
    end.

%% @spec(font_info()) -> {integer(), integer()}.
%% @desc Returns the size of a string (in scale 1.0).
text_size(#font{wx=Font}, String) ->
    MDC  = memory_dc(Font),
    Size = wxDC:getTextExtent(MDC, String),
    wxMemoryDC:destroy(MDC),
    Size.

%% @spec(font_info()) -> {integer(), integer()}.
%% @desc Returns the height of characters and rows.
height(#font{height=Height}) ->
    Height.

%% @spec(font_info()) -> integer()
%% @desc Returns the texture id.
tex_id(#font{tex=TexId}) ->
    TexId.

%% @spec(font_info(), unicode:charlist()) -> ok.
%% @desc Directly renders the string.
render(#font{} = GLFont, String) ->
    render_text(GLFont, String).

%% @spec(font_info(), unicode:charlist()) -> {Size::size(), DisplayList::integer()}.
%% @desc Renders the string.
render_to_list(#font{} = Font, String) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    Res = render_text2(Font, String),
    gl:endList(),
    {Res, List}.    

%% @spec(Font::font_info(), unicode:charlist()) -> 
%%       {W::integer(), H::integer(), binary()}.
%% @desc Renders the string to an interleaved vertex array.
%% @equiv render_to_binary(Font, String, {0,0,<<>>}).
render_to_binary(#font{glyphs=Gs, height=H, ih=IH, iw=IW}, String) ->
    render_text3(String, Gs, IH, IW, H, {0,0, <<>>}).

%% @spec(Font::font_info(), unicode:charlist(), {X0,Y0,Bin}) -> 
%%       {X::integer(), Y::integer(), Bin::binary()}.
%% @desc Renders the string to an interleaved vertex array.
%% The render starts at position X0 and Y0 and appends to Bin,
%% and returns the next position.
%%
%% The binary can be rendered by the following code:
%%
%%     gl:bindTexture(?GL_TEXTURE_2D, wx_glfont:tex_id(Font)),</br>
%%     gl:vertexPointer(2, ?GL_FLOAT, 16, Bin), </br>
%%     gl:texCoordPointer(2, ?GL_FLOAT, 16, TxBin),</br>
%%     gl:enableClientState(?GL_VERTEX_ARRAY),</br>
%%     gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),</br>
%%     gl:drawArrays(?GL_QUADS, 0, (byte_size(Bin)-?BIN_XTRA) div 16),</br>
render_to_binary(#font{glyphs=Gs, height=H, ih=IH, iw=IW}, 
		 String, Data = {W,H,D}) 
  when is_integer(W), is_integer(H), is_binary(D) ->
    render_text3(String, Gs, IH, IW, H, Data).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
gen_glfont(Font, AW, H, Options) ->
    Ranges0 = proplists:get_value(range, Options, [{32, 256}]),
    {NoChars, Ranges} = char_ranges(Ranges0),
    {TW,TH} = calc_tex_size(NoChars, AW, H),
    %% io:format("Tex Sz: ~p ~p ~n",[TW,TH]),
    {Bin, HaveAlpha, Glyphs} = make_glyphs(Font,Ranges,H,TW,TH),
    TexId = gen_texture(TW,TH,Bin,HaveAlpha,Options),
    
    #font{wx=Font, tex=TexId, glyphs=Glyphs, height=H, ih=H/TH, iw=1/TW}.

char_ranges(Ranges0) when is_list(Ranges0) ->
    Ranges = lists:sort(Ranges0),
    Count = lists:foldl(fun({From, To}, Count) when From =< To ->
				Count + (To - From)
			end, 0, Ranges),
    {Count, Ranges}.
    

make_glyphs(Font,Ranges,H, TW,TH) ->
    MDC = memory_dc(Font),
    Bitmap = wxBitmap:new(TW, TH, [{depth,32}]),
    ok = wxMemoryDC:selectObject(MDC, Bitmap),

    BG = {0, 0, 0, 0},
    Brush = wxBrush:new(BG, [{style, ?wxSOLID}]),
    wxMemoryDC:setBackground(MDC, Brush),
    wxMemoryDC:clear(MDC),

    FG = {255, 255, 255, 255},
    wxMemoryDC:setTextForeground(MDC, FG),
    wxMemoryDC:setTextBackground(MDC, BG),
    Glyphs = make_glyphs(MDC, Ranges, 0, 0, H, TW, TH, array:new()),
    Image = wxBitmap:convertToImage(Bitmap),
    %% debug(Image),  

    BinData = wxImage:getData(Image),
    Alpha = case wxImage:hasAlpha(Image) of
		true ->
		    %%io:format("A = ~p ~n", [wxImage:hasAlpha(Image)]),
		    wxImage:getAlpha(Image);
		false ->
		    false
	    end,

    wxBrush:destroy(Brush),
    wxImage:destroy(Image),
    wxBitmap:destroy(Bitmap),
    wxMemoryDC:destroy(MDC),
    greyscale(BinData, Alpha, Glyphs).

%% Minimize texture space, use greyscale images
greyscale(BinData, false, Glyphs) ->  %% Alpha use gray scale value
    Bin = << <<255:8, A:8>> || <<A:8,_:8,_:8>> <= BinData>>,
    {Bin, true, Glyphs};
greyscale(BinData, Alpha, Glyphs) ->
    {greyscale2(BinData, Alpha, <<>>), true, Glyphs}.

greyscale2(<<R:8,_:8,_:8, Cs/bytes>>, <<A:8, As/bytes>>, Acc) ->
    greyscale2(Cs, As, <<Acc/bytes, R:8, A:8>>);
greyscale2(<<>>, <<>>, Acc) ->
    Acc.

make_glyphs(DC, [{From, To}|Ranges], X, Y, H, TW, TH, Acc0)
  when From < To ->
    {Acc,Xp,Yp} = make_glyph(DC, From, X, Y, H, TW, TH, Acc0),
    make_glyphs(DC, [{From+1, To}|Ranges], Xp, Yp, H, TW, TH, Acc);
make_glyphs(DC, [_|Ranges], X, Y, H, TW, TH, Acc) ->
    make_glyphs(DC, Ranges, X, Y, H, TW, TH, Acc);
make_glyphs(_DC, [], _X, _Y, _H, _TW, _TH, Acc) ->
    Acc.

make_glyph(DC, Char, X0, Y0, Height, TW, TH, Acc0) ->
    {Width, _H} = wxDC:getTextExtent(DC, [Char]),
    Xt = X0+Width,
    case (Y0 + Height) =< TH of
	true -> %% Assert that we fit inside texture
	    case Xt > TW of
		true -> 
		    X = Width,  Y = Y0+Height+?SPACE_Y,
		    X1 = 0, Y1 = Y;
		false ->
		    X  = Xt,  Y = Y0,
		    X1 = X0, Y1 = Y0 
	    end,
	    wxMemoryDC:drawText(DC, [Char], {X1, Y1}),
	    G = #glyph{w=Width, u=X1/TW, v=(Y1)/TH},
	    {array:set(Char, G, Acc0), X+?SPACE_X, Y};
	false ->
	    io:format("Tex ~p,~p to small Ignore Char ~p(~ts)~n",
		      [TW,TH,Char,[Char]]),
	    {Acc0, X0, Y0}
    end.
	    

gen_texture(TW,TH,Bin,HaveAlpha,Options) ->
    [TexId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TexId),

    %% gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    Mode = proplists:get_value(tex_mode, Options, ?GL_MODULATE),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, Mode),

    MinF = proplists:get_value(tex_min, Options, ?GL_LINEAR),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,MinF),
    MagF = proplists:get_value(tex_mag, Options, ?GL_LINEAR),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_MAG_FILTER,MagF),
    
    WS = proplists:get_value(tex_wrap_s, Options, ?GL_CLAMP),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S, WS),
    WT = proplists:get_value(tex_wrap_t, Options, ?GL_CLAMP),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T, WT),    

    GEN_MM = proplists:get_value(tex_gen_mipmap, Options, ?GL_FALSE),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_GENERATE_MIPMAP, GEN_MM),
    if GEN_MM == ?GL_TRUE ->
	    gl:generateMipmapEXT(?GL_TEXTURE_2D);
       true -> ignore
    end,
    
    %% io:format("HaveAlpha ~p ~n",[HaveAlpha]),
    case HaveAlpha of
	true ->
	    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_LUMINANCE8_ALPHA8,
			  TW, TH,  0, ?GL_LUMINANCE_ALPHA,
			  ?GL_UNSIGNED_BYTE, Bin);
	false ->
	    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_LUMINANCE8,
			  TW, TH,  0, ?GL_LUMINANCE,
			  ?GL_UNSIGNED_BYTE, Bin)
    end,
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
    {RX,RY,Bin} = render_text3(String, Gs, IH, IW, H, {0,0, <<>>}),
    case Bin of
	<<_:?BIN_XTRA/bytes>> -> ok;
	<<_:2/unit:32, TxBin/bytes>> ->
	    wx:retain_memory(Bin),
	    gl:bindTexture(?GL_TEXTURE_2D, TexId),
	    gl:vertexPointer(2, ?GL_FLOAT, 16, Bin),
	    gl:texCoordPointer(2, ?GL_FLOAT, 16, TxBin),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
	    gl:drawArrays(?GL_QUADS, 0, (byte_size(Bin)-?BIN_XTRA) div 16),
	    gl:disableClientState(?GL_VERTEX_ARRAY),
	    gl:disableClientState(?GL_TEXTURE_COORD_ARRAY),
	    wx:release_memory(Bin)
    end,
    {RX,abs(RY-H)}.

render_text3([Char|String], Gs, IH, IW, H, Data0) when is_integer(Char) ->
    case array:get(Char, Gs) of
	#glyph{}=Glyph ->
	    Data = render_glyph(Glyph,H,IW,IH,Data0), 
	    render_text3(String, Gs, IH, IW, H, Data);
	undefined when Char =:= 10 -> %% NL
	    {_,H0,Bin} = Data0,
	    render_text3(String, Gs, IH, IW, H, {0, H0-H, Bin});
	undefined when Char =:= 9 -> %% TAB
	    Space = array:get(32, Gs),
	    Data  = lists:foldl(fun(_, Data) ->
					render_glyph(Space,H,IW,IH,Data)
				end, Data0, "        "),
	    render_text3(String, Gs, IH, IW, H, Data);
	undefined -> %% Should we render something strange here
	    render_text3(String, Gs, IH, IW, H, Data0)
    end;
render_text3([Other|String], Gs, IH, IW, H, Data0) ->
    Data = render_text3(Other, Gs, IH, IW, H, Data0),
    render_text3(String, Gs, IH, IW, H, Data);
render_text3(Bin, Gs, IH, IW, H, Data) when is_binary(Bin) ->
    render_text3(unicode:characters_to_list(Bin), Gs, IH, IW, H, Data);
render_text3([], _Gs, _IH, _IW, _H, {W,H0,Bin}) ->
    {W, H0, <<Bin/bytes, 0:(?BIN_XTRA*8)>>}.

render_glyph(#glyph{u=U,v=V,w=W},H,IW,IH, {X0,Y0,Bin}) -> 
    X1 = X0 + W,
    UD = U + W*IW,
    VD = V + IH,
    YH = Y0+H,
    {X1,Y0,
     <<Bin/binary,         %% wxImage: 0,0 is upper left turn each 
       X0:?F32,Y0:?F32, U:?F32, VD:?F32, % Vertex lower left, UV-coord up-left
       X1:?F32,Y0:?F32, UD:?F32,VD:?F32, % Vertex lower right,UV-coord up-right
       X1:?F32,YH:?F32, UD:?F32, V:?F32, % Vertex upper right,UV-coord down-right
       X0:?F32,YH:?F32, U:?F32,  V:?F32  % Vertex upper left, UV-coord down-left
     >>
    }.

%% The code above is the same as the code below.
%% render_glyph(#glyph{u=U,v=V,w=W},X0,H,IW,IH) -> 
%%     X1 = X0 + W,
%%     UD = U + (W-1)*IW,
%%     VD = V + IH,
%%     gl:texCoord2f(U,  VD),  gl:vertex2i(X0, 0),
%%     gl:texCoord2f(UD, VD),  gl:vertex2i(X1, 0),
%%     gl:texCoord2f(UD, V), gl:vertex2i(X1, H),
%%     gl:texCoord2f(U,  V), gl:vertex2i(X0, H),
%%     X1.

%% Calculate texture size

calc_tex_size(No, CW, CH) ->
    %% Add some extra chars to be sure it fits.
    calc_tex_size(No+45, 1, No+45, CW, CH, {undefined, undefined}, undefined).

calc_tex_size(X, Y, No, CW, CH, Prev = {BestArea,Dec}, BestCoord)
  when Y =< No ->
    Xp = tsize(X*CW+X*?SPACE_X), %% + Empty space between chars
    Yp = tsize(Y*CH+Y*?SPACE_Y), %% + Empty space between rows
    Area = Xp * Yp,
    Square = abs(Xp - Yp),
    NextX = ((No-1) div (Y+1)) + 1,
    if Area < BestArea ->
	    %io:format("Best is ~p ~p ~p ~n", [Area, Xp,Yp]),
	    calc_tex_size(NextX, Y+1, No, CW, CH, {Area,Square}, {Xp,Yp});
       Area == BestArea, Square < Dec ->
	    %io:format("Best is ~p ~p ~p ~n", [Area, Xp,Yp]),
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

debug(Image0) ->
    Image = wxImage:copy(Image0),
    W = wxImage:getWidth(Image),
    H = wxImage:getHeight(Image),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "DEBUG", [{size, {W+10, H+10}}]),
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
