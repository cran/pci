# pci 1.0.1

* Fixes an issue where vectors carrying a 'names' attribute passed as arguments to the `flag_na()` internal function caused an unexpected error.
* Fixes the unintended behavior of PCI function output carrying a 'names' attribute when any inputs were named; now it's always `NULL`.
