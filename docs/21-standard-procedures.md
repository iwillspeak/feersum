# Standard Procedure Support

This document lists a high-level overview of Feersum's support for the R7RS
standard procedures:

[TOC]

## Equivalence Predicates

All Equivalence predicates are fully supported.

## Numbers

Some numeric literals are supported. A subset of the numeric operations are
supported. Things generally conform to the numeric tower with only inexact
IEE double precision floats as supported numeric value types.

## Booleans

Boolean literals and procedures are fully supported.

## Pairs and Lists

Pairs and lists are supported. Some core procedures, such as `cons` and `list`
are supported.

## Symbols

Symbol literals are fully supported. All symbol procedures are supported.

## Characters

Character literals are supported, with Unicode support limited to utf-16 code
units. No Character procedures are implemented.

## Strings

String literals are supported. Unicode character escapes are limited to utf-16
code units. No string procedures are implemented.

## Vectors

Vector literals and some core vector procedures are supported.

## Bytevectors

Byte vector literals and some core byte vector procedures are supported.

## Control Features

No control procedures are supported.

## Exceptions

Some core library procedures may throw exceptions. The `guard` form is not
supported. There is no ability to throw exceptions from Scheme code yet.

## Environments and Evaluation

No support for dynamic evaluation is provided outside the built-in REPL.

## Input and Output

The `display` and `newline` procedures are the only supported IO procedures.

## System Interface 

None of the system interface is supported.