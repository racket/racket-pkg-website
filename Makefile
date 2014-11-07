all: compile

run:
	./run

bounce:
	sudo svc -du /etc/service/$$(basename $$(pwd))

start-service:
	sudo svc -u /etc/service/$$(basename $$(pwd))

stop-service:
	sudo svc -d /etc/service/$$(basename $$(pwd))

compile:
	raco make src/main.rkt

clean:
	find . -depth -type d -iname compiled -exec rm -rf {} \;

.PHONY: run bounce
