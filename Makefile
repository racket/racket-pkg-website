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
	raco make src/main.rkt src/site.rkt

clean:
	find . -depth -type d -iname compiled -exec rm -rf {} \;

.PHONY: run bounce

###########################################################################

keys: private-key.pem server-cert.pem

private-key.pem:
	openssl genpkey -algorithm RSA -out $@

server-cert.pem: private-key.pem
	openssl req -new -x509 -days 365 \
		-subj /CN=beta.package.database.localhost \
		-key private-key.pem -out $@

clean-keys:
	rm -f private-key.pem server-cert.pem
