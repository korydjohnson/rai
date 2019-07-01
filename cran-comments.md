## Resubmission
This is a resubmission. In this version I have:

* Replaced T and F by TRUE and FALSE.

* Used requested format for references.

* Modified the predict method to accept more parameters.

* A reviewer claimed that I modify the global environment through my use of "<<-". I do not. I modify the value of private variables held by closures. If there is a mistake in this, I appologize. I rechecked all uses of "<<-" as well as the global environment before and after running test code. I found no modifications of the global environment.

## Test environments
* local Ubuntu 18.04 install, R 3.6.0
* win-builder (release and devel)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

New submission

## Downstream dependencies
There are currently no downstream dependencies for this package.
