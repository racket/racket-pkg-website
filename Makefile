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
	raco make src/main.rkt src/site.rkt src/backup/main.rkt

clean:
	find . -depth -type d -iname compiled -exec rm -rf {} \;

.PHONY: run bounce

###########################################################################

ROOT=compiled/root

keys: $(ROOT)/private-key.pem $(ROOT)/server-cert.pem

$(ROOT)/private-key.pem:
	mkdir -p $(ROOT)
	openssl genpkey -algorithm RSA -out $@

$(ROOT)/server-cert.pem: $(ROOT)/private-key.pem
	mkdir -p $(ROOT)
	openssl req -new -x509 -days 365 \
		-subj /CN=beta.package.database.localhost \
		-key $(ROOT)/private-key.pem -out $@

clean-keys:
	rm -f $(ROOT)/private-key.pem $(ROOT)/server-cert.pem
