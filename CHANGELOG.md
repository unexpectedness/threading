# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [0.3.4] - 2018-03-08
### Changed
- upgraded `shuriken` to version `0.14.36`.

## [0.3.3] - 2018-09-16
### Fixed:
- `let->` now properly threads the expression to the body expressions.

## [0.3.2] - 2018-09-16
### Fixed:
- updated shuriken to version `0.14.16` in to order to fix order of
  `map-vals->` and `map-keys->` on sequences.

## [0.3.1] - 2018-09-15
### Fixed:
- updated shuriken to version `0.14.15` in to order to fix order of
  `map-vals->` and `map-keys->` on lists.

## [0.3.0] - 2018-09-08
- `•-` stores and now threads its expression to the form as well.

## [0.2.0] - 2018-09-05
### Added
- `let->` & `binding->` arrows.
- Teleport fletching/arrow: `•-` & `-•`.
- `>-args` fletching.

### Changed
- `>-` becomes `>>-` and vice-versa.


## [0.1.10] - 2018-09-05
### Added
- `mapv->`, `juxt->`, `let->`.

## [0.1.9] - 2018-09-04
### Changed
- Updated `shuriken` to `0.14.9`.

## [0.1.8] - 2018-08-22
### Added
- `mapcat->`.

## [0.1.7] - 2018-06-15
### Added
- `map-keys->` & `map-vals->`.

## [0.1.6] - 2018-05-21
### Fixed
- protect `pp->` & `pp->>` against infinite sequences.

## [0.1.5] - 2018-05-21
### Added
- `not->`.

## [0.1.4] - 2018-05-20
### Added
- arrow fletchings: `>-` & `>>-`.

## [0.1.3] - 2018-05-19
### Added
- `map->` and `map->>`.
- README + Documentation + API doc.

## [0.1.2] - 2018-05-09
### Added
- `<<-` to complement `<-` in the context of forms threading in the style
  of `->>`.

## [0.1.1] - 2018-05-06
### Added
- `if-not->` & `if-not->>`.
- `when-not->` & `when-not->>`.

## [0.1.0] - 2018-04-30
Initial release.
