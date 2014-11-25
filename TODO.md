Documentation/help text on the edit package page?

Move `generic-input` and friends into bootstrap.rkt

Somehow present all the tags available for filtering on the search page
 - maybe just a big list?
 - maybe checkboxes?

Want to be able to see packages with build problems, and packages without build problems
 - on the search page? would a display of all tags help with this?
 - or on the main page?

Find a better table-sorter plugin that makes sorting by multiple
columns one after the other more discoverable. (At the moment, you
have to hold down shift, and it selects them in the ~reverse order.)

Some way of getting a list of all packages
 - "you should have the "285 package in the index" have a link like "hiding main-distribution (show)""

you should be able to specify git repos over http/https
you should be able to specify subdirs in git repos
you should check the existence of repos/github repos/urls/etc via XHR

Much more input validation is needed

Mobile nav doesn't show up

Table on index page doesn't fit properly on mobile. Somehow make it responsive?

There's github API for determining whether there's a preferred README
for a repo. We're currently doing a brute, dumb thing (downloading the
HTML and looking for an id of "readme" with a regex!), and should use
the API instead:

 - HEAD https://api.github.com/repos/USERNAME/REPONAME/readme
 - if it's 200, there's a README
 - if it's 404, there isn't

Further, if you GET that URL instead of HEAD, you get JSON with an
"html_url" field that links to a rendered version. However, the JSON
response also includes the actual text of the README (!), and the link
isn't to the main page for the repo, so it might be better to just
link to `...#readme` as we currently do and ignore the URL that Github
suggests.

## Design possibilities

"the big gray box should probably not go all the way to the top of the page"
