# Racket Package Library Website

## Prerequisites

You will need to install the following Racket packages:

    raco pkg install --skip-installed \
         'https://github.com/racket/infrastructure-userdb.git#main' \
         reloadable \
         aws

## Configuration

See the directory `configs/` for example configuration files. To
select a configuration file, set the environment variable `CONFIG` to
its base filename. For example, to select `configs/live.rkt`, set
`CONFIG` to `live`. A good place to do this is in the `run-prelude`
file; see the description of `run-prelude` below.

If `CONFIG` is not set, it defaults to the short hostname of the
machine (`hostname -s`).

Within a configuration file, configuration details are to be given as
a hashtable to `main`.

Keys useful for deployment:

 - *port*: number; default the value of the `SITE_PORT` environment
   variable, if defined; otherwise, 7443.
 - *ssl?*: boolean; default `#t`.
 - *reloadable?*: boolean; `#t` if the `SITE_RELOADABLE` environment
   variable is defined; otherwise, `#f`.
 - *recent-seconds*: number, in seconds; default 172800. Packages
   modified fewer than this many seconds ago are considered "recent",
   and displayed as such in the UI.
 - *static-output-type*: either `'aws-s3` or `'file`.
    - When `'file`,
	   - *static-content-target-directory*: either `#f` or a string
		 denoting a path to a folder to which the static content of
		 the site will be copied.
    - When `'aws-s3`,
       - *aws-s3-bucket+path*: a string naming an S3 bucket and path.
         Must end with a forward slash, ".../". AWS access keys are
         loaded per the documentation for the `aws` module; usually
         from a file `~/.aws-keys`.
 - *dynamic-urlprefix*: string; absolute or relative URL, prepended to
   URLs targetting dynamic content on the site.
 - *static-urlprefix*: string; absolute or relative URL, prepended to
   relative URLs referring to static HTML files placed in
   `static-generated-directory`.
 - *pkg-index-generated-directory*: a string pointing to where the
   `pkg-index` package places its redered files, to be served
   statically. The source file `static.rkt` in this codebase knows
   precisely which files and directories within
   `pkg-index-generated-directory` to upload to the final site.
 - *user-directory*: directory containing the user database; should be
   the same as `pkg-index` uses.
 - *email-sender-address*: string; defaults to `pkgs@racket-lang.org`.
   Used as the "from" address when sending authentication emails on
   behalf of the server.

Keys useful for development:

 - *package-index-url*: string; default
   `http://pkgs.racket-lang.org/pkgs-all.json.gz`.
 - *package-fetch-interval*; number, in seconds; default 300.
 - *session-lifetime*: number, in seconds; default 604800.
 - *static-generated-directory*: string; names a directory relative to
   `src/` within which generated static HTML files are to be placed.
   Must be writable by the user running the server.
 - *disable-cache?*: boolean; default `#f`.
 - *backend-baseurl*: string; default `https://pkgd.racket-lang.org`.
   Must point to the backend package server API root, such that (for
   example) `/jsonp/authenticate`, when appended to it, resolves to
   the authentication call.
 - *pkg-build-baseurl*: string; default
   `http://pkg-build.racket-lang.org/`. Used to build URLs relative to
   the package build host, for such as documentation links and build
   reports.

## Local testing

You will need some dummy SSL keys. Run `make keys` to produce some.

Running `src/main.rkt` starts a local server. For your convenience,

    make compile run

compiles the code and starts the server.

### Automatic code reloading

If you would like to enable the automatic code-reloading feature, set
the environment variable `SITE_RELOADABLE` to a non-empty string or
set the `reloadable?` configuration variable to `#t`.

You must also delete any compiled code `.zo` files. Otherwise, the
system will not be able to correctly replace modules while running.

Therefore, when using automatic code reloading, use just

    make run

and make sure to run `make clean` beforehand, if you've run `make
compile` at all previously.

### Testing configuration

The configuration that will be convenient for local testing is `testing`. 
To use this configuration, you must create `pkgs-all.json.gz` at the root 
of the project. You could do so with the following command:

```
$ wget --header="Accept-Encoding: gzip" https://pkgs.racket-lang.org/pkgs-all.json.gz
```

Then run by using the command

```
$ CONFIG=testing make compile run
```

The configuration enables the automatic code reloading described above.

## Deployment

### Static Content

The site can be set up to run either

 0. entirely dynamically, generating package pages on-the-fly for each
    request;
 0. both statically and dynamically, with HTML renderings of package
    pages stored on and served from disk like other static resources
    such as Javascript and CSS; or
 0. both statically and dynamically, as the previous option, but
    additionally replicating both static and generated content to a
    local file-system directory and invoking an optional update hook
    that can be used to further replicate the content to S3 or a
    remote host.

The default is mixed static/dynamic, with no additional replication.

For a fully dynamic site, set configuration variable `disable-cache?`
to `#t`.

To enable replication, set configuration variable
`static-content-target-directory` to a non-`#f` value, and optionally
set `static-content-update-hook` to a string containing a shell
command to execute every time the static content is updated.

#### S3 Content

To set up an S3 bucket---let's call it `s3.example`---for use with
this site, follow these steps:

 0. Create the bucket ("`s3.example`")
 0. Optionally add a CNAME record to DNS mapping `s3.example` to
    `s3.example.s3-website-us-east-1.amazonaws.com`. If you do, static
    resources will be available at `http://s3.example/`; if not, at
    the longer URL.
 0. Enable "Static Website Hosting" for the bucket. Set the index
    document to `index.html` and the error document to `not-found`.

Then, under "Permissions", click "Add bucket policy", and add
something like the following.

    {
      "Id": "RacketPackageWebsiteS3Policy",
      "Version": "2012-10-17",
      "Statement": [
        {
          "Sid": "RacketPackageWebsiteS3PolicyStmt1",
          "Action": "s3:*",
          "Effect": "Allow",
          "Resource": ["arn:aws:s3:::s3.example",
                       "arn:aws:s3:::s3.example/*"],
          "Principal": {
            "AWS": ["<<<ARN OF THE USER TO WHOM ACCESS SHOULD BE GRANTED>>>"]
          }
        }
      ]
    }

The user will need to be able to read and write objects and set CORS
policy. (CORS is configured automatically by code in
`src/static.rkt`.)

### Supervision

Startable using djb's [daemontools](http://cr.yp.to/daemontools.html);
symlink this directory into your services directory and start it as
usual. The `run` script starts the program, and `log/run` sets up
logging of stdout/stderr.

If the file `run-prelude` exists in the same directory as `run`, it
will be dotted in before racket is invoked. I use this to update my
`PATH` to include my locally-built racket `bin` directory, necessary
because I don't have a system-wide racket, and to select an
appropriate `CONFIG` setting.

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

 - creating `.reload` causes an explicit code reload. Useful when
   automatic code reloading is disabled.

 - creating `.fetchindex` causes an immediate refetch of the package
   index from the backend server.

 - creating `.rerender` causes an immediate rerendering of all
   generated static HTML files.

See `src/signals.rkt` for details of the available signals.

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
