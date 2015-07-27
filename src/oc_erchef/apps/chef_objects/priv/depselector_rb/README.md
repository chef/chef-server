## Getting depselector_rb working with Mac OS X & Homebrew

#### Step 1: Nuke your `gecode` from orbit

it's the only way to be sure.

```
brew remove --force gecode
```

#### Step 2: Force Homebrew to be into `gecode` 3.7.3

If you've been having problems with this bit of ruby, it's probably
because somewhere your system is finding `gecode` 4.0 or higher. This
ruby code only works with 3.7.


##### 2a: get all up in homebrew

```
cd /usr/local/Library/Formula
```
##### 2b: checkout the 3.7.3 version of the fomula

```
git checkout 2d5ed4c70ce486a0e43467716c07c8de8af4d96e gecode.rb
```

##### 2c: Homebrew doesn't do MD5 anymore

Delete the following line (#6) from `gecode.rb`:

```
md5 '7a5cb9945e0bb48f222992f2106130ac'
```

#### Step 3: Install gecode

```
brew install gecode
```

#### Step 4: Pin gecode

This will keep you from accidentally a 4.x

```
brew pin gecode
```

#### Step 5: Use this code!

Back down in `src/oc_erchef` use the makefile to delete your broken
version of this:

```
make bundle_clean
```

Then you can use it to build a new one that works!

```
make bundle
```

If you set your environment `USE_SYSTEM_GECODE=1`, it will skip the
ruby FFI parts and be generally faster for you.
