# Rackmount

Rackmount is a simple HTTP server written in the [Racket](https://racket-lang.org) programming language. Parallelism for any number of processors is provided through use of Racket's "places" interface.

## Invocation

Recognized command line arguments:

**Required**

- `-w`, `--working-dir`: Set the working directory for the server.
- `-p`, `--port`: Set the port number to bind to.

**Optional**

- `-j`, `--workers`: Set the number of worker places to create. Defaults to one.
- `-i`, `--interface`: Set the hostname of the interface to bind to. If not set, the server will bind to any available interface.
- `-z`, `--client-timeout`: Sets a client timeout, in seconds. Each HTTP transaction must complete in less than this period of time, otherwise the connection will be dropped.
- `-l`, `--log-level`: Sets the logging level for the server. Defaults to `trace`.

## Author

Chris Vig (chris@invictus.so)