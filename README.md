# Racket Package Library Website

## Prerequisites

You will need to install the following Racket packages:

    raco pkg install reloadable

## Local testing

You will need some dummy SSL keys. Run `make keys` to produce some.

Running `src/main.rkt` starts a local server. For your convenience,

    make compile run

compiles the code and starts the server.

### Automatic code reloading

If you would like to enable the automatic code-reloading feature, set
the environment variable `SITE_RELOADABLE` to a non-empty string. (A
good place to do that is in a `run-prelude` script; see below.)

You must also delete any compiled code `.zo` files. Otherwise, the
system will not be able to correctly replace modules while running.

Therefore, when using automatic code reloading, use just

    make run

and make sure to run `make clean` beforehand, if you've run `make
compile` at all previously.

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

## Copyright and License

Copyright &copy; 2014 Tony Garnock-Jones

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
