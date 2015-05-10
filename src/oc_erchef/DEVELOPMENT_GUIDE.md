# oc_erchef Development

This document is aimed to give you some hints and best practices around
developing in `oc_erchef`. Please contact a maintainer or lieutenant
[here](https://github.com/chef/chef-server/blob/master/MAINTAINERS.md)
if you find anything out of date.

## Handling Dates in oc_chef_wm

For all API date inputs and outputs, we will be strictly using ISO8601 in
UTC format, which looks like `YYYY-MM-DDThh:mm:ssZ` where `T` is the character
`T` that separates years, months, days from hours, minutes, seconds and `Z`
is the character `Z` to denote UTC time as is standard in ISO8601. We should
strictly validate all date inputs to webmachine / our API to match the above format, and
all dates should be outputted in said format.

The exception to this is that "infinity" is a valid date input and can be parsed by all base functions
and properly inserted into the database with a little massaging (also in base modules).

#### Validating Date Inputs

We have added a validation and sqerl date sanitizing function that you should use on your date object fields
during validation:

```
chef_object_base:validate_and_sanitize_date_field(ChefEjsonObject, FieldBinary).
```

where `FieldBinary` is a valid binary of the format <<"YYYY-MM-DDThh:mm:ssZ">> or <<"infinity">>. If it is
any other binary, the function will `throw(bad_date, FieldBinary)`, which will throw you a nice error message
related to wrong date formatting for your field.

Also be aware that this function will also alter your `ChefEjsonObject`'s `FieldBinary` in a way that `sqerl`
can parse (see next section).

#### Unfortunate Quarks and Necessary Parsing

Unfortunately, some of the libraries `sqerl` depends on (namely `epgsql_idatetime`)
does not know how to properly validate proper ISO UTC formatted strings.

Therefore, before using `sqerl` to insert any timestring into the database, the string
should be parsed so that `sqerl` can read it (basically, removing the Z, since UTC is assumed).

All of this will be done automatically if you use `chef_object_base:validate_and_sanitize_date_field(ChefEjsonObject, FieldBinary)`
to valiate and update your object json as described above, so you don't need to worry about this if you use that function.
