all: fib_sample
	@#erlc sample_transform.erl
	@#erlc -pa ../ebin -pa . sample.erl 

fib_sample:
	@erlc fib_add_transform.erl
	@erlc -pa ../ebin -pa . fib_add_transform_sample.erl
	@erlc -pa ../ebin -pa . fib_add_transform_sample2.erl

clean:
	@rm -rf *.beam
