.Phony: clean
clean:
	fd -tf ".o|.hi" -x rm {}
	fd -t executable -x rm {}

.Phony: test
test:
	@for file in */*hs; do echo $$file; doctest $$file || exit 1; done  

