# Threadscope

Installing threadscope has been a nightmare in osx.

How to install it in osx:

- <https://github.com/haskell/ThreadScope/releases>
- Download the v0.2.11 `threadscope.osx.gz`
- untar
- rename it to threadscope
- try to execute it: `$ ./threadscope`
- If the executable works, you are done (probably it will output some warnings in the terminal, ignore it).
- If not, install:
  - `$ brew install gtk+`
  - `$ brew install gtk-mac-integration`

