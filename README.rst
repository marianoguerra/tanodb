tanodb
===========

A riak_core application

Build
-----

::

    rebar3 release

Test
----

::

    rebar3 ct

Run
---

::

    rebar3 run

Try
---

::

    1> tanodb:ping().
    {pong,753586781748746817198774991869333432010090217472}

Quit
----

::

    2> q().

Play with Clustering
--------------------

Build 3 releases that can run on the same machine::

    make devrel

Start them in different consoles::

    make dev1-console
    make dev2-console
    make dev3-console

join 2 nodes to the first one::

    make devrel-join

check the status of the cluster::

    make devrel-status

you should see something like this::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    joining     0.0%      --      'tanodb2@127.0.0.1'
    joining     0.0%      --      'tanodb3@127.0.0.1'
    valid     100.0%      --      'tanodb1@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:1 / Leaving:0 / Exiting:0 / Joining:2 / Down:0

it should say that 3 nodes are joining, now check the cluster plan::

    make devrel-cluster-plan

it should display the cluster plan, now we can commit the plan::

    make devrel-cluster-commit

check the status of the cluster again::

    make devrel-status

you could see the vnodes transfering::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      75.0%     25.0%    'tanodb1@127.0.0.1'
    valid       9.4%     25.0%    'tanodb2@127.0.0.1'
    valid       7.8%     25.0%    'tanodb3@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

at some point you should see something like this::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      33.3%      --      'tanodb1@127.0.0.1'
    valid      33.3%      --      'tanodb2@127.0.0.1'
    valid      33.3%      --      'tanodb3@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

when you are bored you can stop them::

    make devrel-stop


TODO
----

* define license and create LICENSE file

License
-------

TODO
