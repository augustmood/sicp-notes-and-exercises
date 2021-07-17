# Exercise 3.48

Give a scenario where the deadlock-avoidance mechanism described above does not
work. (Hint: In the exchange problem, each process knows in advance which
accounts it will need to get access to. Consider a situation where a process
must get access to some shared resources before it can know which additional
shared resources it will require.)

#

In the case that the procedure needed to be serialized is not known yet, we can
only decide the order of mutex to be required by the parameters account id. But
if the passed procedure itself is protected by another serializers as the first
one that we acquired in the current procedure, we will get the deadlock.
