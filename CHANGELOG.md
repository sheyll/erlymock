# Changelog

## Version 7.2.{4,5}

* Try to fix nixpkgs based `hexBuild` errors be
  replacing '-include_lib' with '-include'

## Version 7.2.3

* Fix cover compilation

## Version 7.2.2

* Simplify the internal module loading
* Improve module loading stability

### Details

* Replace `em_mocking_queue` by `em_module_loader`
* Discourage concurrent mocking
* Fix the race condition introduced in 7.1.0
* Add `em:lock()' and `em:unlock()' to mark sections of code
  that would appreciate that no modules are mocked.
* Hopefully fix #8
* Respect `sticky_dir`
* Add printf debugging profile `rebar3 as printf_debugging eunit`
