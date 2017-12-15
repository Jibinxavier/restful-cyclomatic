# restful-cyclomatic
### Overview 
This project investigates the impact on performance if we increase the number of nodes working on an embarrassingly parallel task. The nodes will calculate cyclomatic complexity of git repo. It implements work stealing, work pushing and master slave patterns. The graph shows the expected performance:- as the number of thread increases the number time taken decreases

In this implementation manager first compiles all the work that needs to be done. In this case it goes through every commit and gathers filepaths in it, to form a tuple (commitid , filepath) (called task) and inserts in a database.

Meanwhile, when workers start they register themselves to the manager. 

#### Usage
- docker-compose build
- docker-compose up
- For different configs python benchmark_config_builder.py -h
#### Factors that affect all the patterns
- Latency in sending or asking for work
- This was tested on a laptop, therefore the figures here is not entirely comparable to a node running this task. But on the other hand this task often requires I/O operation which could lead the processor being idle. This could be expolited if there are processes running parallelly, as it is less likely that all processes do their I/O at the same time.
- Running on Docker container. As stated in the above point, the resources are shared and could lead to more contention as the number of workers increase.
#### Work Stealing (WS)
The client continously makes get requests asking for task and the manager picks a task from the compiled task and assigns it to the worker. When task is assigned manager stores a timestamp, to indicate when the task was assigned and a flag to check if task was completed.

The manager will assign woker will assign the work to someone else if the result wasnt returned within a threshold.

-  This is pattern is advantageous when the manager is not able to predict how long will a node take to complete a task and assign task accordingly. <sup>1</sup>
- It also away achieving load balancing
- In this experiment however work stealing was not slower than work push and master slave. This is probably because of the latency in communicating. The server has to respond to request for tasks and also results. Therefore, the communication can get queued up.


#### Work Pushing (WP)
Here the master pushes all the work to the client's queue. The worker then takes one by one, computes and sends the result back.

- Work pushing seems to be the best-performing. This is due to less congestion at the server, as the tasks are pushed to the workers queue. Rather than workers asking which in turn creates a lot more traffic.


#### Master Slave (MS)

For the master slave pattern. The manager sends the task to the worker (tuple).This is then inserted into the worker queue. The worker then assigns a thread to handle the worker and the sends back the result.

- According to the graph master slave seems to be the second best performing. However, the performance is lower because of the overhead of creating more processes. 
- It is slightly better than the WS, thanks to lower traffic.





Things that affect the performance:
not running on proper node 
Since Docker shares the host resource
https://docs.docker.com/engine/faq/

By default the pattern is work stealing

#### Improvements
- More accurate 
-Deploy it on an Iaas
- Better management of work allocation. Assess the size of the file and assign that to the fastest worker
- Since the messages are small they could combined into batches

![](./result.png/?raw=true "Results")

<sup>1</sup> http://www.well-typed.com/blog/71/