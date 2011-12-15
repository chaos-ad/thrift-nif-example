all: get-deps src/thrift/test_types.erl compile

src/thrift/test_types.erl: thrift/test.thrift
	mkdir -p src/thrift include/thrift c_src/thrift
	thrift --gen erl --gen cpp:erl_nif -o thrift thrift/test.thrift
	(cd src/thrift     && ln -fs ../../thrift/gen-erl/*.erl ./)
	(cd c_src/thrift   && ln -fs ../../thrift/gen-cpp/*.*   ./)
	(cd include/thrift && ln -fs ../../thrift/gen-erl/*.hrl ./)

compile:
	./rebar compile

get-deps:
	./rebar get-deps

clean:
	./rebar clean
	rm -rfv erl_crash.dump

distclean: clean
	rm -rf src/thrift c_src/thrift include/thrift thrift/gen-erl thrift/gen-cpp
	rm -rfv ebin deps


