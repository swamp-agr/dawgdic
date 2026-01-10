# Revision history for `dawgdic`

## 1.0.0 -- 2026-01-10

* Remove all traces from the code (see [#30](https://github.com/swamp-agr/dawgdic/pull/30)).
* **Breaking change**: ensure binary compatibility across two implementations via custom `Binary` instances for `Dictionary`, `Guide` and `RankedGuide` (see [#29](https://github.com/swamp-agr/dawgdic/pull/29)). 
* Port missing `RankedGuide` from C++ (see [#26](https://github.com/swamp-agr/dawgdic/pull/26)).
* **Breaking change**: include `Dictionary` in `Guide` (see [#25](https://github.com/swamp-agr/dawgdic/pull/25)).
* Port `dawgdic-find` and `dawgdic-build` binaries from C++ to a single executable with sub-commands (see [#23](https://github.com/swamp-agr/dawgdic/pull/23)).

## 0.1.0 -- 2025-08-28

* First version. Released on an unsuspecting world.
