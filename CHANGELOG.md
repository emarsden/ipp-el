# Changelog

## [0.8] - 2025-07-20

- Use the plz library to make HTTP requests via curl (falling back to the url.el support built into
  Emacs), rather than rolling our own HTTP support code. This should make the library much more
  compliant with the IPP specifications (for example, supporting HTTP chunked encoding and HTTP/2
  connections) and work with more IPP implementations.

- New customizable variable `ipp-user-name`.


## [0.7] - 2023-07-13

- New customizable variable `ipp-default-printer`.


## [0.6] - 2022-08-21

- Moved from cl library to cl-lib

- Distributed via github repository



## [0.5] - 2001

This version was distributed from http://purl.org/net/emarsden/home/downloads/
and via the EmacsWiki. 
