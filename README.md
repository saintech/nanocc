# nanocc

[![Build Status](https://travis-ci.org/saintech/nanocc.svg?branch=master)](https://travis-ci.org/saintech/nanocc)
[![Coverage Status](https://coveralls.io/repos/github/saintech/nanocc/badge.svg?branch=master)](https://coveralls.io/github/saintech/nanocc?branch=master)
[![Go Report Card](https://goreportcard.com/badge/github.com/saintech/nanocc)](https://goreportcard.com/report/github.com/saintech/nanocc)
[![Lines of Go Code](https://img.shields.io/badge/dynamic/json.svg?label=go%20lines&url=https%3A%2F%2Fapi.codetabs.com%2Fcount-loc%2Fget%3Frepo%3Dsaintech%2Fnanocc&query=%24%5B%3F(%40.language%3D%3D%22Go%22)%5D.linesOfCode&colorB=5272B4&maxAge=300)](https://api.codetabs.com/count-loc/get?repo=saintech/nanocc)

## A Nano C Compiller & VM

**nanocc** is port of very primitive "C to custom bytecode" compiller and runner - [rswier/c4][1]. This port is mostly a line-by-line translation from c to go, and is in parity with commit [3f098f3][2].

[1]: https://github.com/rswier/c4
[2]: https://github.com/rswier/c4/tree/3f098f3

## Motivation

This project was created to examine the work of the most popular of the simplest C compilers - **c4, "C in four functions"**. The goal is to be as close to the original and as easy as possible to understand.

## Features

The compiler does not support the entire C syntax, only a small part of it. As it is written in the original:

    // char, int, and pointer types
    // if, while, return, and expression statements
    // just enough features to allow self-compilation and a bit more

So, the compiler is able to compile its C version and then itself.

## Differences from c4

 * The most important difference - **nanocc** uses one big byte array as VM memory instead of shared with unsafe pointers in **c4**.
 * Many comments have been added (many of which are translated from [comzyh/c4](https://github.com/comzyh/c4)).
 * Tests. Many C tests sources are taken from [EarlGray/c4](https://github.com/EarlGray/c4) (under GPL-2.0).

## Installing / Getting started

**Via [Go][3]**

```sh
# Download/Update/Instal
$ go get -u github.com/saintech/nanocc
$ cd $(go env GOPATH)/src/github.com/saintech/nanocc/testdata

# Usage
$ nanocc
usage: nanocc [-s] [-d] file ...

# Compile and run hello.c
$ nanocc hello.c
hello, world
exit(0) cycle = 9

# Compile and run its C version which compile and run hello.c
$ nanocc c4.c hello.c
hello, world
exit(0) cycle = 9
exit(0) cycle = 25983

# Compile and run its C version which compile and run self
# which compile and run hello.c
$ nanocc c4.c c4.c hello.c
hello, world
exit(0) cycle = 9
exit(0) cycle = 25983
exit(0) cycle = 10154153

# We need to go deeper...
```

![We need to go deeper](https://i.kym-cdn.com/entries/icons/original/000/012/886/wntgd.jpg)

[3]: https://golang.org/

## Contribution

The door is always open :blush:

## Thanks

 * [Robert Swierczek](https://github.com/rswier) for original project
 * [Comzyh](https://github.com/comzyh) for commenting "с4" in Chinese
 * [Dmytro Sirenko](https://github.com/EarlGray) for C test sources
 * [Elliot Chance](https://github.com/elliotchance) for [c2go](https://github.com/elliotchance/c2go) tool

## License

It is in the **public domain** under the [WTFPL](http://www.wtfpl.net/about/) license.
