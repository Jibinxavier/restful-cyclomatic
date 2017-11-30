import argparse
import glob
import requests
import json
import git
import os
import csv
from requests.exceptions import ConnectionError 
import yaml
from pymongo import MongoClient
# import lizard
from datetime import date, datetime

class DateTimeEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, datetime):
            return o.isoformat()

        return json.JSONEncoder.default(self, o)
def mongo_conn():
    client = MongoClient("localhost",27017)
    return client.test_database
def send_post_msg(url, data):
    """
    """

    headers = {'Content-type': 'application/json'}
    
    return requests.post(url, data=json.dumps(data,cls=DateTimeEncoder), headers=headers)




def get_msg(url):
    res = requests.get(url).json()
    
    return json.loads(res["result"])
def git_clone(repoUrl,repo_path):
    if not os.path.exists(repo_path):
        git.Git().clone(repoUrl, repo_path)
def get_all_commits(repoUrl,repo_path): 
    """
        returns a list of git.Commit objects
    """
    git_clone(repoUrl,repo_path)
    repo = git.Repo(repo_path)
    
    return list(repo.iter_commits())

def git_checkout(commit_obj,repo_path):

    """
        need to check if it will accept strings 
    """
    repo = git.Repo(repo_path)
    git1 = repo.git
    git1.checkout(commit_obj)
   
def get_all_files(path, ext):
    """
        Get all files with an extension
    """
    files = glob.glob(path + '/**/*' + ext, recursive = True)
    return files
def parse_args():
    """ 
    this could be the url of the repo
    """

    parser = argparse.ArgumentParser(description='Distributed cyclomatic computation')
    


    parser.add_argument('--manager', default= False, 
                help='manager function')
    parser.add_argument('--worker', default=False,
                help='The upper bound date (yyyy-mm-aa)')
    parser.add_argument('--port', default=8000,
                help='port number')
    args = parser.parse_args()  
    

    return args
def get_avg_cyclo(db):

    total_n =db.jobs.find({}).count()
    result = db.jobs.aggregate(
        [{"$group": { "_id":"results", "totalAmount": { "$sum": "$result"},}}]
        )
    result = list(result)[0] 
        
    if (result["totalAmount"] != 0 and total_n!=0 ):
        return result["totalAmount"]/total_n
        
    else:
        # print( "Aggregated sum is {}. Total number of commits {}".\
        #     format(result["totalAmount"],total_n))
        return 0
def get_time_taken(db):
    time_now = datetime.now()
    earliest_job = db.jobs.aggregate([ 
        { "$group": { "_id": {},"min": { "$min": "$assigned_time" }}}
        ])
    earliest_job = list(earliest_job)[0]['min']
    return (time_now-earliest_job).total_seconds()

def record_results(file, headers=["avg_cyclomatic_complex","n_workers","time_taken"]):
    db = mongo_conn() 
    n_workers = db.workers.find({}).count() 
    avg = get_avg_cyclo(db) 
    time_taken = get_time_taken(db)  
    print(" work done. The average complexity is {} ".format(avg)) 
    if os.path.isfile(file): 
        with  open(file, 'a') as csvfile:   
            writer = csv.writer(csvfile, delimiter=',',) 
            writer.writerow([avg,  n_workers, time_taken])
    else:
        with  open(file, 'a') as csvfile:   
            writer = csv.writer(csvfile, delimiter=',',) 
            writer.writerow(headers)
            writer.writerow([avg,  n_workers, time_taken])
 
def worker_node_ports(base="docker-compose.yml"):
    ports = []
    with open(base, 'r') as stream:
        try:
            
            res = yaml.load(stream)
            print(yaml.load(stream))
            for k,v in res["services"].items():
                port = res["services"][ k].get('environment')
                if port: 
                    ports.append(port[0].split("=")[1] )

        except yaml.YAMLError as exc:
            print(exc)

    return ports
#   print(get_all_commits("https://github.com/cpbuckingham/python.git","/tmp/python"))

# test = get_all_commits("https://github.com/cpbuckingham/python.git","/tmp/python")
# 
# 

