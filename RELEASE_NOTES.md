# Enterprise Chef Release Notes

## 11.1.3 (Unreleased)

### What's New:

The following items are new for Enterprise Chef 11.1.3 and/or are changes from previous versions:

### Bug Fixes:

The following items are the set of bug fixes that have been applied since Enterprise Chef 11.1.2:

### Security Fixes:

The following items are the set of security fixes that have been
applied since Enterprise Chef 11.1.2:

## 11.1.2 (2014-02-28)

### Security Fixes:

The following items are the set of security fixes that have been applied since Enterprise Chef 11.1.1:

* [opscode-webui] Don't log or email the Rails session or environment from the exception handler. Doing so can cause user-submitted form values like passwords to be logged and emailed to administrators of the Enterprise Chef server when exceptions occur on the Management Console.