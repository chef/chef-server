# Enterprise Chef Release Notes

## 1.4.8

### Security Fixes:

The following items are the set of security fixes that have been applied since Enterprise Chef 1.4.7:

* [opscode-webui] Don't log or email the Rails session or enviroment from the exception handler. Doing so can cause user-submitted form values like passwords to be logged and emailed to administrators of the Enterprise Chef server when exceptions occur on the Managment Console.