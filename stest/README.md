# bookshelf_bench

Stress tests for bookshelf.

## Usage

The basho_bench tests in bookshelf_bench are intended to run against an externally running instance of bookshelf. The easiest way to get bookshelf running externally is to run it in a vm as a component of either chef-server or private-chef. bookshelf is easily limited by CPU, so limiting the number of CPUs available to the VM makes it easier to push bookshelf to the limits. To avoid SSL negotiation, the default bookshelf_bench config connects to localhost:4321, so setting up SSH port forwarding is an easy way to get access to bookshelf on a local port. With bookshelf running on a VM, we'll call it `api.bookshelf.vm`, do the following:

In a separate session, set up the ssh tunnel:

```
ssh -L 4321:localhost:4321 api.bookshelf.vm
```

Copy and edit the bookshelf_bench.config (detailed instructions are commented in the config file):

```
cp bookshelf_bench.config.example bookshelf_bench.config
```

Run the benchmark:

```
make bench
```

Once complete, you can view a graph of the benchmark data:

```
make view
```

## Bookshelf Failure Modes

The reason for this stress test is that bookshelf began to fail by running out of processes at large scale loads. We needed a way to locally reproduce the failure scenario that we saw happening in the wild. Running the stress test  itself isn't always sufficient to trigger the failure mode, but doing so will likely trigger the behavior that leads to failure. In the specific instance that we set out to test for, bookshelf was crashing with the following:

```
[error] Too many processes
```

We needed to see if, under load, bookshelf was leaking processes. We monitored the process by attaching to the running bookshelf vm:

```
/opt/opscode/embedded/service/bookshelf/bin/bookshelf attach
```

The process monitoring code is as follows:

```erlang
CountPids = fun() ->
                    io:format("pid count: ~p\n",
[erlang:system_info(process_count)]),
                    ok
                                end.

LoopShow = fun(Me, What, T) ->
                   What(),
                                      timer:sleep(T),
                                                         Me(Me, What, T)
           end.

XX = spawn(fun() -> LoopShow(LoopShow, CountPids, 2000) end).
```

The monitoring code above resulted in the following output from the console:

```
pid count: 7679
pid count: 8349
pid count: 8982
pid count: 9635
pid count: 10241
pid count: 10914
pid count: 11507
pid count: 12226
pid count: 12867
pid count: 13555
pid count: 14202
pid count: 14907
pid count: 15297
pid count: 15841
pid count: 16490
pid count: 17184
pid count: 17874
pid count: 18483
```