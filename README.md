Random's Little Helper
======================

Local and D-Scan analyzer.  A public instance is running at https://randoms-littlehelper.rhcloud.com/

Local
-----

Analyzes pasted local.  It presents 2 lists:  notable corporations or alliances (with more than 10% of the total players in local),
and all the pilots sorted by their kills in the last 2 months (according to zkillboard).  It prioritizes a fast response
over displaying out-of-date data.

D-Scan
------

Analyzes D-Scan.  Sorts by number of results per type.


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
* input.select() needs a delay in IGB
