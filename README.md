# Rackmount

Rackmount is a simple HTTP server written in the [Racket](https://racket-lang.org) programming language.

## Invocation

Recognized command line arguments:

**Required**

- `-w`, `--working-dir`: Set the working directory for the server.
- `-p`, `--port`: Set the port number to bind to.

**Optional**

- `-h`, `--hostname`: Set the hostname of the interface to bind to. If not set, the server will bind to any interface.

## Author

Chris Vig (chris@invictus.so)