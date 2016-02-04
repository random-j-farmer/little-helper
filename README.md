Random's Little Helper
======================


Analyzes pasted Local, gives quick overview bout top corporations/alliances (by player count) and
recent kill stats.

A public instance is running at https://randoms-littlehelper.rhcloud.com/


How to compile/test
-------------------

In sbt:

    ~reStart               # to start the server
    ~appJVM/test           # to run the tests
    clean                  # clean
    universal:packageBin   # produces a zip file


IGB Notes
---------

* ScalaJS works if you include es5-shim and es5-sham
* classList does not work, use setAttribute
