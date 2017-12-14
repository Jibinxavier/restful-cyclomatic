import subprocess
import os
import time
import sys
import yaml 
import argparse
import copy
WORKERS = 5
NUMBER_RUNS = WORKERS
PATTERNS = ["WORK_PUSHING","WORK_STEALING","MASTER_SLAVE"]

def write_config(config,custom_config):
    with open(custom_config, 'w') as file:
        try:
            yaml.dump(config, file, default_flow_style=False)
        except yaml.YAMLError as exc:
            print(exc)
def get_config():

    with open("./docker-compose.yml", 'r') as stream:
        try:
            return yaml.load(stream)
        except yaml.YAMLError as exc:
            print(exc)
def line_count(file):
    if os.path.isfile(file):
        return len(open(filename).readlines())
    else:
        return 0
def wait_for_result(file):
    prev_count = line_count(file) 
    while(True): 
        curr_count = line_count(file)

        if prev_count < curr_count:
            break
# def main( total_workers):
#     all_config = get_config() 

#     for pattern in PATTERNS: 
#         new_config = {
#             "version": '2',
#             "services": {}
#             }

#         new_config["services"]["db"] = all_config["services"]["db"]
#         new_config["services"]["res-master"] = all_config["services"]["res-master"]

#         os.environ["PATTERN"] = pattern
       
#         for r in range(NUMBER_RUNS):
#             # remove all containers for a fresh start
#             os.system("docker rm -f $(docker ps -a -q) ")
             
           
#             for worker in range(total_workers+1):  
#                 work_name = "worker" + str(worker)
#                 new_config["services"][work_name] = all_config["services"][work_name]
#             write_config(new_config,"benchmark-compose.yml")
#             os.system("docker-compose -f benchmark-compose.yml build  ")
#             os.system("docker-compose -f benchmark-compose.yml  up -d   ")
               
#             wait_for_result("./results/" + pattern + ".csv")
def main( total_workers, pattern,repo):
    all_config = get_config()  
    start_port = 8000
    new_config = {
        "version": '2',
        "services": {}
        }

    new_config["services"]["db"] = all_config["services"]["db"]
    new_config["services"]["res-master"] = all_config["services"]["res-master"]
    new_config["services"]["res-master"]["environment"] = ["PATTERN="+pattern, "REPO="+repo]
 
    os.environ["PATTERN"] = pattern
       
 
    sampleworker = all_config["services"]["worker0"]
 
    for i in range(total_workers):  
        port_num = start_port + i
        work_name = "worker" + str(i)
        new_config["services"][work_name] = copy.deepcopy(sampleworker)
        new_config["services"][work_name]["environment"] = ["PATTERN="+pattern, "REPO="+repo, "port=" + str(port_num)]
        new_config ["services"][work_name]["ports"]= ["{}:{}".format( port_num,port_num )]
    write_config(new_config,"benchmark-compose.yml")
            
    # create_graphs()

    
if __name__ == "__main__":
    repo = "https://github.com/cpbuckingham/python.git"
    
    parser = argparse.ArgumentParser(description='Creates a config for getting results')
    


    
    parser.add_argument('--worker_n',type=int, default=5, help='Number of workers')
                
    parser.add_argument('--pattern', choices=["WORK_STEALING","MASTER_SLAVE", "WORK_PUSHING"],
                        help='Pattern for distributing work', default="WORK_STEALING")

    parser.add_argument('--repo', default=repo, help='Repo for computing the cyclomatic complexity')
 
    
     
    args = parser.parse_args()
 
    main( args.worker_n, args.pattern, args.repo)