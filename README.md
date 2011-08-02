This is a short and sweet url shortener meant to be run locally.

The config file is .shortening.conf. It's pretty self-explanatory and will appear after the first
run of the program.

shortening exposes a REST API, where http://host:port/api?url=http://www.google.com/ will return a
simple string, prefixed by a /. Appending this string to http://host:port and visiting the URL will
result in a redirect to the 'long' version of the URL.

Here are the steps to build a clisp-based binary:
$ clisp
> (ql:quickload 'shortening)
> (ext:saveinitmem "shortening" :quiet t :init-function #'shortening:init :norc t :executable t)

You should then be able to run shortening.
