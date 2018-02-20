# Changelog

## Version 7.2.0

* Replace `em_mocking_queue` by `em_module_loader`
* Discourage concurrent mocking
* Fix the race condition introduced in 7.1.0
* Add `em:lock()' and `em:unlock()' to mark sections of code
  that would appreciate that no modules are mocked.
* Hopefully fix #8
