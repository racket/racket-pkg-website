# Racket Bootstrap Template Website

## Local testing

Running src/main.rkt starts a local server.

## Deployment

### Supervision

Startable using djb's [daemontools](http://cr.yp.to/daemontools.html);
symlink this directory into your services directory and start it as
usual. The `run` script starts the program, and `log/run` sets up
logging of stdout/stderr.

If the file `run-prelude` exists in the same directory as `run`, it
will be dotted in before racket is invoked. I use this to update my
`PATH` to include my locally-built racket `bin` directory, necessary
because I don't have a system-wide racket.

On Debian, daemontools can be installed with `apt-get install
daemontools daemontools-run`, and the services directory is
`/etc/service/`.

### Control signals

You can send signals to the running service by creating files in
`/etc/service/webservice/signals/`. For example:

 - creating `.pull-required` causes the server to shell out to `git
   pull` and then exit. Daemontools will restart it.

 - creating `.restart-required` causes it to exit, to be restarted by
   daemontools.

So long as `sudo chmod 0777 /etc/service/webservice/signals`, these
are useful for non-root administrators to control the running service.

In particular, a git `post-receive` hook can be used to create the
`.pull-required` signal in order to update the service on git push.
