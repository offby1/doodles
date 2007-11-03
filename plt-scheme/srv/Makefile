check: server.ss
	./$^ &
	sleep 1
	(echo list-tables; echo later; echo join-any; echo '(join 1)'; echo '(join palooka)'; echo list-tables) | nc localhost 1234 -w 1
	(echo list-tables; echo join-any; echo list-tables) | nc localhost 1234 -w 1
	echo die | nc localhost 1234 -w 1

.PHONY: check
