Random's Little Helper
======================


Analyzes pasted Local, gives quick overview about top corporations/alliances (by player count) and
recent kill stats.

A public instance is running at https://randoms-littlehelper.rhcloud.com/


How to compile/test
-------------------

In sbt:

    ~reStart               # to start the server
    ~appJVM/test           # to run the tests
    clean                  # clean
    universal:packageBin   # produces a zip file

Copyright & License
-------------------

Copyright (c) YC118, The Elders of Pator Tech School.

See [LICENSE.md](LICENSE.md)




IGB Notes
---------

* ScalaJS works if you include es5-shim and es5-sham
* classList does not work, use setAttribute
