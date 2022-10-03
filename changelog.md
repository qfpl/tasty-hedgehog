# Revision history for tasty-hedgehog

## 1.3.1.0 -- 2022-10-03

* The instructions for reproducing test failures are now more clearly distinguished from `hedgehog`'s own instructions and include a pattern in the example to limit which tests are re-run. ([#62](https://github.com/qfpl/tasty-hedgehog/pull/62))

## 1.3.0.0 -- 2022-08-22

* The `testProperty` function has been undeprecated. Its behaviour differs from that in version `1.1.0.0` and below in that it now passes no `PropertyName` to Hedgehog. Therefore, Hedgehog will render the text `<property>` in its instructions for reproducing test failures, as opposed to whatever description is provided for `testProperty`.

## 1.2.0.0 -- 2022-03-07

* Add `testPropertyNamed` function and deprecate `testProperty`.

## 1.1.0.0 -- 2021-04-03

* Add fromGroup function

## 1.0.1.0 -- 2021-01-25

* Automatically enable or disable colour, based on the same criteria
  that hedgehog itself checks.

## 1.0.0.2  -- 2020-01-16

* Upgrade to `hedgehog-1.0.2`

## 1.0.0.1  -- 2019-05-22

* Fixed test result reporting to made plain hedgehog's messages (fixes #30)

## 1.0.0.0  -- 2019-05-17

* Removed support for GHC < 8
* Upgrade to `hedgehog-1`

## 0.2.0.0  -- 2018-03-13

* Removes the verbosity option, which was unsupported
* Fixes a bug in configuration option handling, which
  was overwriting use configuration with the defaults.

## 0.1.0.2  -- 2018-01-22

* Ease bounds to allow for `tasty` 1.0.

## 0.1.0.1  -- 2018-08-24

* Exposed the various tasty options.

## 0.1.0.0  -- 2017-08-24

* First version. Released on an unsuspecting world.
