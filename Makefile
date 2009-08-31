all: erlang

erlang: gui.beam cube.beam u.beam hello.beam wx_glfont.beam wxGLFontServer.beam

%.beam: %.erl
	@erlc $<

run: all
	erl -setcookie $(COOKIE) -sname gui -s gui -s cube -s hello

clean:
	@rm -f *.beam erl_crash.dump *~
