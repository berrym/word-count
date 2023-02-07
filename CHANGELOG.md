# Changelog for `word-count`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2023-01-25

Initial commit.

## 0.2.0.0 - 2023-02-07

Refactored to use binary files and ByteStrings. File statistics are kept in a
tuple type named FileStats. Micro lenses are used to access the tuples.
