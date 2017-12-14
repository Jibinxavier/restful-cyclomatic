# restful-cyclomatic
### Overview 
This project investigates the impact on performance if we increase the number of nodes working on an embarrassingly parallel task. The nodes will calculate cyclomatic complexity of git repo. It implements work stealing, work pushing and master slave patterns.

At the very start the manager compiles all the work that needs to be done. In this case it goes through every commit and gathers files in it to form a tuple (commitid , filepath).

#### Usage
- docker-compose build
- docker-compose up
- For different configs python benchmark_config_builder.py -h
#### Factors that affect all the patterns
- Latency in sending or asking for work
- This was tested on a laptop, therefore the figures here is not entirely comparable to a node running this task. But on the other hand this task often requires I/O operation which could lead the processor being idle. This could be expolited if there are processes running parallelly, as it is less likely that all processes do their I/O at the same time.
- Running on Docker container. As stated in the above point, the resources are shared and could lead to more contention as the number of workers increase.
#### Master Slave

For the master slave pattern. The manager sends the task to the worker (tuple).This is then inserted into the worker queue. The worker then assigns a thread to handle the worker and the sends back the result.

According to the graph the cost of assigning a thread to each work is expensive
#### Work Stealing



#### Work Pushing
Things that affect the performance:
not running on proper node 
Since Docker shares the host resource
https://docs.docker.com/engine/faq/

By default the pattern is work stealing

#### Improvements
- Deploy it on AWS
- Better management of work allocation. Assess the size of the file and assign that to the fastest worker
- 