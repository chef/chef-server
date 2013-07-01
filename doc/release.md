Cutting a New Release of Bifrost
================================

To cut a proper release, we're using the [rebar_lock_deps_plugin][].
The executive summary is this:

```
BUMP=patch make prepare_release && rebar commit-release && rebar tag-release
```

Substitute `minor` or `major` for `BUMP` as necessary.  Note that the
presence of the `USE_REBAR_LOCKED` on `master` will cause all
subsequent builds to use the locked dependencies.

[rebar_lock_deps_plugin]:https://github.com/seth/rebar_lock_deps_plugin
