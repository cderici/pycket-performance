These are some Kubernetes specs that can be used to run Pycket on a kubernetes cluster.

The setup is as follows:

- We have a Kubernetes cluster running on a master node with a working kubectl.
- We have a docker image of Pycket that we want to run on the cluster. (use cderici/pycket:latest if you don't have one)
- We have a persistent volume that we want to use to store the data. (the specs to create one are in this folder). Apply the persistent volume spec first, then the persistent volume claim spec.
- The mounted persistent volume looks like this:

```
$ ls benchmarks/
experiment-status  kubejobs  scripts  src  timings-pycket
```

We use control-room to create the kubejobs and scripts for each benchmark. Each job runs a separate script that runs a benchmark and stores the results in the timings-pycket folder.

A sample script, and a sample job are provided in this folder.