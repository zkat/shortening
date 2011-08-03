# About

This is a short and sweet url shortener meant for personal use. It's a handy utility for those who
want short urls but cannot or do not want to use public URL shorteners. I, for example, use it with
a weechat plugin, since people might send me private, sensitive URLs that it may not be appropriate
to send to an external server with guessable URLs.

# Configuration

shortening will first look in `~/.shortening.conf`, and then in `/etc/shortening.conf` for existing
configuration files. There is an example configuration file in the source directory, named
`shortening.conf.example`.

Here is the entirety of the default configuration file:

    [shortening]
    port = 8181 # Port to start the web server in.
    max-db-size = 100 # Maximum number of URLs to keep around at a time.
    url-length = 6 # Length of the randomly-generated short URLs.
    external-db = # See below

* If external-db is blank (or there is no config file), URLs are kept in-memory, and will not be
  persisted to disk. This means all URLs will be lost when the shortening process shuts down.
* external-db accepts absolute (/foo/bar), relative (foo/bar/), and ~/-expanded (~/.shortening.db)
  pathnames.
* ~username/ pathnames are not accepted.

# Usage

You may start shortening simply by invoking its binary. It has no command-line arguments.

shortening uses a REST API for its shortening service:

*http://host/api* `url origin`

  Adds a URL to the database. The ORIGIN parameter is optional. The response will be a string,
  prefixed by /. Making a request `http://host` + this string will redirect the client to the URL
  parameter.

*http://host/* `plainp`

  Requesting the root document will display a list of all links currently in the database, listed by
  short-url, long-url, and origin (if any). The HTML version also provides a simple form where you
  can submit your own URLs without using the API.

  If PLAINP is submitted with any value, shortener will respond with a plain-text version of the
  current URL database, in the following format: `("<short-url>" "<long-url>" <origin>)`, where
  `<origin>` will either be a string wrapped in double-quotes, or `NIL`.

# Building

Shortening is Common Lisp software. In order to build it, you will first have to install one of the
supported Lisp implementations:

 * CLISP <http://www.clisp.org/>
 * SBCL <http://www.sbcl.org/>
 * Clozure CL <http://www.clozure.com/clozurecl.html>

You will also need to have Quicklisp installed in order to pull in shortening's dependencies. You
can get quicklisp here: http://www.quicklisp.org/beta/

Here are the steps to build a clisp-based binary:

    $ cd /path/to/shortening/
    $ clisp
    > (load "/path/to/quicklisp/setup.lisp") ; Optional if quicklisp is auto-loaded.
    > (load "make")

You can then execute the 'shortening' binary.

# Using shortening remotely

Shortening is designed to be used locally. As such, it has no authorization built in. If you want to
be able to access URLs hosted by shortening remotely, you must arrange to make the proper ports
available to yourself. *It is highly recommended you do not simply open the ports up to the
internet*.

Instead, consider an alternative such as ssh tunneling to make the same URLs accessible from any of
your machines.

# Notes/known issues/warnings

* *WARNING* There is no authorization built into shortening. If you expose the port to the internet,
   *you may be at risk of being used by spammers.*
* Killing the program with Control+c does not currently work in Clozure CL.
* If you run this on localhost, you can have some fun with your hosts file. I added 'lch' as a local
  hostname, so I can access shortening using <http://lch:8181/ShRTuRL>.

# License

Shortening is public domain software. I, the author, revoke any and all copyright I may have over
the code in this repository. Do whatever you want with it. I won't complain if you credit me.

If you live in an oppressive country that makes release into the public domain illegal, you may use
the license in the OPPRESSED file included with the shortening sources for legal comfort.
