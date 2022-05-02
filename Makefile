.Phony: clean
clean:
	fd -tf ".o|.hi" -x rm {}
	fd -t executable -x rm {}
