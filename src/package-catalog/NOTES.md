# New design

Packages have *authoritative* (human-managed) and *computed* keys in
the database. Then, separately, a static-rendered form of the package
description hashtable is computed from the database record.

Package ownership is determined by the presence or absence of an email
address in the package's `authors` list.

Authoritative keys:

 - `name`, string
 - `source`, quasi-URL
 - `description`, string
 - `tags`, list of strings
 - `authors`, list of strings (email addresses)
     - NB. existing code treats `author` as authoritative, with
       `authors` computed
	 - If an email address is present in this list, then the
	   corresponding user may edit/delete the package, including
	   changing ownership of it.
 - `versions`
     - hash table mapping version name string (NOT `'default`!) to
       hash table containing a `source` key mapping to a quasi-URL
     - note that no default entry is to be present in this table:
       instead, it's computed (for the benefit of 5.3.6 and older) as
       part of the computation and static-rendering step.
 - `ring`, number; 0, 1, or 2. Updateable by catalog admin only
 - `last-edit`

Computed keys:

 - `author`, string, space-joined `authors`
 - `last-updated`
 - `last-checked`
 - `versions`
     - each version gets its checksum computed, and placed in a
       `checksum` key alongside its `source` key.
 - `checksum-error`
     - `#f` if no error; otherwise, a string. In the existing code,
	   the first checksum-computation to yield an error is stored
	   here, and the remainder of the computations are abandoned. In
	   the new code, this should store a record of all the failed
	   computations.
 - `checksum`
     - checksum for the top-level (default) source
 - `conflicts`
 - `modules`
 - `dependencies`

In the rendered form of a package record, the default source and
checksum and the versions table are arranged differently. If a version
named `"5.3.6"` exists, its source (and checksum) are used at
top-level; and either way, the default source and checksum are copied
into a version named `'default`. In addition, each version in the
`versions` table (including `'default`) has a `source_url` field added
to it, with an HTTP(S) URL for humans to visit heuristically derived
from the `source` quasi-URL.

The rendered form also has the following additional top-level keys:

 - `build`, a hash-table:
	 - currently includes:
	    - `success-log`
	    - `failure-log`
		- `dep-failure-log`
		- `conflicts-log`, either `#f`, a build-host-relative URL
          string pointing at the conflicts log, or `(list/c "indirect"
          string?)`, which again seems to point at some kind of log
          but flagged somehow? Ah, this kind of indirect report means
          that one of the dependencies of the package has a conflict.
	    - `docs`, a list of
		   - `(list/c (or/c "main" "extract" "salvage" string?
             string?)`, where the last string is the URL-fragment
             relative to the build host where the rendered
             documentation is stored and the penultimate string is the
             name of this chunk of documentation.
		   - `(list/c "none" string?)`, where the last string is the
             name of the chunk of documentation, but no rendered form
             is available.
	 - should also include:
	    - `test-success-log`
		- `test-failure-log`
		- `min-failure-log` - records problems due to missing
          environmental dependencies. See
          http://pkg-build.racket-lang.org/
 - `search-terms`, a hash-table where each present key has `#t` as its
   value. Each key is a symbol. Keys that may be present:
     - one per tag in the package's `tags` list (as symbols)
	 - `ring:N` where N corresponds to the package's ring
	 - `author:X` where X is drawn from the package's `authors` list
	 - `:no-tag:` if `tags` is empty
	 - `:error:` if `checksum-error` is non-false
	 - `:no-desc:` if `description` is the empty string
	 - `:conflicts:` if `conflicts` is not the empty list
	 - `:build-success:` if the success-log is non-false
	 - `:build-fail:` if the failure-log is non-false
	 - `:build-dep-fail:` if the dep-failure-log is non-false
	 - `:build-conflicts:` if the conflicts-log is non-false
	 - `:docs:` if some docs exist and not all of them are `doc/none` instances
	 - `:docs-error:` if some docs exist but none of them is a `doc/main` instance

# JSON variations on various records

 - Racket lists, numbers and booleans map to JSON lists, numbers and booleans
 - Racket strings and symbols map to JSON strings
 - Racket keywords map to a JSON hash with key "kw" and value the
   result of `keyword->string` on the keyword
 - Racket hash tables map to JSON hashes; keys may be either strings or symbols.

# Users

User records are currently just a file containing only their bcrypted
passwords. They should probably also have an `administrator?` flag
associated with them.

# Notes on existing package catalog code

## Existing API

The JSONP requests are all GET requests. Clients include a spurious
unique parameter to avoid cache problems.

 - `/jsonp/authenticate` - registration/validation/login
     - `email`
	 - `passwd`
	 - `code` - optional; used only when email not registered or
       password incorrect.

 - `/jsonp/update` - causes a refresh of all packages editable by the current user

 - `/jsonp/package/del` - delete a package, if current user is an author
     - `pkg`

 - `/jsonp/package/modify` - create or update (including renaming) a package
     - `pkg` - old/existing package name; empty to create a package
	 - `name` - new/updated name
	 - `description`
	 - `source`

 - `/jsonp/package/version/add` - add a non-default version to a package
     - `pkg`
	 - `version`
	 - `source`

 - `/jsonp/package/version/del` - remove a non-default version from a package
     - `pkg`
	 - `version`

 - `/jsonp/package/tag/add` - add a tag to a package
     - `pkg`
	 - `tag`

 - `/jsonp/package/tag/del` - remove a tag from a package
     - `pkg`
	 - `tag`

 - `/jsonp/package/author/add` - add an author to a package
     - `pkg`
	 - `author`

 - `/jsonp/package/author/del` - remove an author from a package
     - `pkg`
	 - `author`

 - `/jsonp/package/curate` - change the ring of a package. Only
   accessible to site administrators.
     - `pkg`
	 - `ring` - string form of new ring number; e.g. `"2"`.

 - `/jsonp/notice` - retrieves the current notice text

The following request is not JSONP, and requires that the method be
POST, not GET:

 - `/api/upload` - accessible only to site administrators. Uploads
   multiple raw package descriptions at once.
    - POST data is read as Racket data. It is to be a `(list/c string?
      string? (hash/c string? package/c))`, where `package/c` is a
      hashtable containing a bunch of keys to be merged with any
      existing keys in the package database.

## Package details

Each package is given:

 - `checksum`
 - `checksum-error`

The static-rendered version adds:

 - `default` version info, with
    - `source` from the main table
	- `checksum` from the main table
	- `source_url` computed from the adjacent source field
 - `authors` list, presumably split from `author` field?
 - `build` table
 - `search-terms` table

From the raw DB:

	#hasheq((name . "ansi")
			(source . "github://github.com/tonyg/racket-ansi/master")
			(last-updated . 1420421711)
			(last-edit . 1418835706)
			(last-checked . 1421174660)
			(versions . #hash())
			(tags . ("terminal"))
			(checksum-error . #f)
			(ring . 1)
			(checksum . "0f9cc06dffa81100df9617ba9deeb46382013e90")
			(author . "tonygarnockjones@gmail.com")
			(conflicts . ())
			(description . "ANSI and VT10x escape sequences for Racket.")
			(modules . ((lib "ansi/ansi.rkt")
						(lib "ansi/test-modes.rkt")
						(lib "ansi/test-raw.rkt")
						(lib "ansi/test-ansi.rkt")
						(lib "ansi/lcd-terminal.rkt")
						(lib "ansi/private/install.rkt")
						(lib "ansi/main.rkt")))
			(dependencies . ("base" "dynext-lib" "rackunit-lib")))

From the static-rendered version:

	#hasheq((name . "ansi")
			(source . "github://github.com/tonyg/racket-ansi/master")
			(last-updated . 1420421711)
			(last-edit . 1418835706)
			(last-checked . 1421174660)
			(versions
			 . #hash((default
					   . #hasheq((source . "github://github.com/tonyg/racket-ansi/master")
								 (checksum . "0f9cc06dffa81100df9617ba9deeb46382013e90")
								 (source_url . "http://github.com/tonyg/racket-ansi/tree/master")))))
			(tags . ("terminal"))
			(checksum-error . #f)
			(ring . 1)
			(checksum . "0f9cc06dffa81100df9617ba9deeb46382013e90")
			(author . "tonygarnockjones@gmail.com")
			(conflicts . ())
			(description . "ANSI and VT10x escape sequences for Racket.")
			(modules . ((lib "ansi/ansi.rkt")
						(lib "ansi/test-modes.rkt")
						(lib "ansi/test-raw.rkt")
						(lib "ansi/test-ansi.rkt")
						(lib "ansi/lcd-terminal.rkt")
						(lib "ansi/private/install.rkt")
						(lib "ansi/main.rkt")))
			(dependencies . ("base" "dynext-lib" "rackunit-lib"))
			(authors . ("tonygarnockjones@gmail.com"))
			(build
			 . #hash((docs . ())
					 (success-log . "server/built/install/ansi.txt")
					 (failure-log . #f)
					 (dep-failure-log . #f)
					 (conflicts-log . #f)))
			(search-terms
			 . #hasheq((:build-success: . #t)
					   (terminal . #t)
					   (ring:1 . #t)
					   (author:tonygarnockjones@gmail.com . #t))))


A richer raw DB record:

    #hash((name . "racket-lib")
		  (source . "git://github.com/plt/racket/?path=pkgs/racket-lib")
		  (author . "eli@racket-lang.org jay@racket-lang.org matthias@racket-lang.org mflatt@racket-lang.org robby@racket-lang.org ryanc@racket-lang.org samth@racket-lang.org")
		  (last-updated . 1420948817)
		  (last-edit . 1418046514)
		  (last-checked . 1421095037)
		  (versions . #hash(("5.3.5" . #hash((source . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip") (checksum . "9f098dddde7f217879070816090c1e8e74d49432")))
			                ("5.3.4" . #hash((source . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip") (checksum . "9f098dddde7f217879070816090c1e8e74d49432")))
							("5.3.6" . #hash((source . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip") (checksum . "9f098dddde7f217879070816090c1e8e74d49432")))))
		  (tags . ("main-distribution"))
		  (checksum-error . #f)
		  (ring . 0)
		  (checksum . "486debd70483427f0a90b53cb9c52cf51e899a37")
		  (description . "Combines platform-specific native libraries that are useful for base Racket")
		  (modules . ())
		  (dependencies . (("racket-win32-i386-2" #:platform "win32\\i386") ("racket-win32-x86_64-2" #:platform "win32\\x86_64") ("racket-x86_64-linux-natipkg-2" #:platform "x86_64-linux-natipkg") ("db-ppc-macosx" #:platform "ppc-macosx") ("db-win32-i386" #:platform "win32\\i386") ("db-win32-x86_64" #:platform "win32\\x86_64") ("db-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg") ("com-win32-i386" #:platform "win32\\i386") ("com-win32-x86_64" #:platform "win32\\x86_64")))
		  (conflicts . ()))

A richer static-rendered description:

    #hash((name . "racket-lib")
		  (source . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")
		  (author . "eli@racket-lang.org jay@racket-lang.org matthias@racket-lang.org mflatt@racket-lang.org robby@racket-lang.org ryanc@racket-lang.org samth@racket-lang.org")
		  (last-updated . 1421178060)
		  (last-checked . 1421178060)
		  (last-edit . 1418046514)
		  (versions
		   . #hash((default
			        . #hasheq((source . "git://github.com/plt/racket/?path=pkgs/racket-lib")
						      (checksum . "9f3c82c30ad1741d35c11ea3e1bb510119e7f476")
							  (source_url . "git://github.com/plt/racket/?path=pkgs/racket-lib")))
		           ("5.3.5"
			        . #hash((source . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")
						    (checksum . "9f098dddde7f217879070816090c1e8e74d49432")
							(source_url . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")))
	               ("5.3.4"
				    . #hash((source . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")
					        (checksum . "9f098dddde7f217879070816090c1e8e74d49432")
							(source_url . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")))
	               ("5.3.6"
				    . #hash((source . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")
					        (checksum . "9f098dddde7f217879070816090c1e8e74d49432")
							(source_url . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")))))
	      (tags . ("main-distribution"))
		  (checksum-error . #f)
		  (checksum . "9f098dddde7f217879070816090c1e8e74d49432")
		  (ring . 0)
		  (description . "Combines platform-specific native libraries that are useful for base Racket")
		  (modules . ())
		  (dependencies . (("racket-win32-i386-2" #:platform "win32\\i386")
			               ("racket-win32-x86_64-2" #:platform "win32\\x86_64")
						   ("racket-x86_64-linux-natipkg-2" #:platform "x86_64-linux-natipkg")
						   ("db-ppc-macosx" #:platform "ppc-macosx")
						   ("db-win32-i386" #:platform "win32\\i386")
						   ("db-win32-x86_64" #:platform "win32\\x86_64")
						   ("db-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg")
						   ("com-win32-i386" #:platform "win32\\i386")
						   ("com-win32-x86_64" #:platform "win32\\x86_64")))
	      (conflicts . ())
		  (authors . ("eli@racket-lang.org" "jay@racket-lang.org" "matthias@racket-lang.org" "mflatt@racket-lang.org" "robby@racket-lang.org" "ryanc@racket-lang.org" "samth@racket-lang.org"))
		  (build . #hash((docs . ())
		                 (success-log . #f)
						 (failure-log . #f)
						 (dep-failure-log . #f)
						 (conflicts-log . #f)))
	      (search-terms . #hasheq((author:mflatt@racket-lang.org . #t)
			                      (author:eli@racket-lang.org . #t)
								  (main-distribution . #t)
								  (ring:0 . #t)
								  (author:robby@racket-lang.org . #t)
								  (author:samth@racket-lang.org . #t)
								  (author:ryanc@racket-lang.org . #t)
								  (author:jay@racket-lang.org . #t)
								  (author:matthias@racket-lang.org . #t))))

## Summary.rktd from the build server

Sometimes only the `docs` key is present.

    ("rmacs" . #hash((author . "tonygarnockjones@gmail.com")
                     (docs . ())
                     (success-log . "server/built/install/rmacs.txt")
                     (failure-log . #f)
                     (dep-failure-log . #f)
                     (test-success-log . #f)
                     (test-failure-log . "server/built/test-fail/rmacs.txt")
                     (min-failure-log . #f)
                     (conflicts-log . #f)))

