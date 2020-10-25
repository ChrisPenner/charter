# Charter

`charter` is a typed Haskell layer on top of [Google Charts](https://developers.google.com/chart) 
which allows quickly and easily visualizing your Haskell data in the browser!

It also provides **live-updates** as your data changes in Haskell-land.

Why Google Charts?

Google Charts provides a large amount of flexible data representations which 
use a relatively consistent data format, making it easy to switch between chart types.

## Goals

Goals of this library include:

* Quick to start
* Batteries Included
* Well-typed, but not at the expense of usability
* Smooth learning curve as you add more complex options to your charts
* Live chart updates!

Non-Goals of this library include:

* Writing charts to disk: sorry, try taking a screenshot of your browser instead!
* Performance: All your data turns into JSON and gets piped through the browser, it'll be slower than other solutions on large datasets.
* Composability with other solutions: This library is "battery included" and doesn't provide many extension points.

## Roadmap

* Better Date, Time, and DateTime support
* Support for Geocharts, TreeMaps, Tables, Timeline, Gauges
* Support multiple charts on a screen
* Support combo charts
