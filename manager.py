import os
import hashlib
import json
import random
import string
import datetime
import csv
import threading  
import time
import sys
from flask import Flask
from flask import jsonify
from flask import request
from pymongo import MongoClient

from flask import Flask 
from bson import json_util
from config import MANAGER_PORT, REPO_URL

from helper import get_all_commits,\
                   get_all_files,\
                   git_checkout,\
                   record_results,\
                   worker_node_ports,\
                   send_post_msg,\
                   mongo_conn

from requests.exceptions import ConnectionError
app = Flask(__name__)

# connect to MongoDB with the defaults
 
 
db = mongo_conn()
db.workers.drop()
db.jobs.drop()



def compile_work(repo,ext=".py"):
    # get all the commits
    # get all the files
    # store them in the database
    db = mongo_conn()
    repo_path = "/tmp/" + repo.split("/")[-1].split(".")[0]
    #============================#
    # shutil.rmtree(repo_path)
    #===========================#
    commits = get_all_commits(repo,repo_path)
    for c in commits:
        git_checkout(c, repo_path)
        print("commit "+ str(c))
        initial = datetime.datetime.now() - datetime.timedelta(minutes=10)
        for file in get_all_files(repo_path, ext):
            db.jobs.insert(
                {"commitid": str(c),
                "fpath":file,
                "repo_path": repo_path,
                "repo_url": repo,
                "assigned_time": initial,
                "completed": False,
                "worker_addr": "",
                "result": 0
                }
            )

         
@app.route('/client/register', methods=['POST'])
def register_worker():
    db = mongo_conn()
    data = request.get_json(force=True)
     
    address = request.remote_addr
    user_id = db.workers.count() 

    db.workers.insert(
        {"user_id": user_id,
        "address":address,
        "port" : data["port"]
        }
    )
    
    print("REgister"+ str(list(db.workers.find({})) ),file=sys.stderr)
    print("Client "+ address)
    return jsonify({})
 
@app.route('/client/work', methods=['GET'])
def distribute_work():
    # do query to find out to get a job
    # if the job time is beyond 2 mins assign it to someone else   
    address = request.remote_addr
    
 
 


    db = mongo_conn()
    # need to check if it was already assigned and have not timed out
    timeout = datetime.datetime.now() - datetime.timedelta(minutes=1)
    work =db.jobs.find_one_and_update({"$and": [{"completed": False},{"assigned_time":{"$lt": timeout}}]},
                                       {"$set": {
                                          "assigned_time":datetime.datetime.now(),
                                          "worker_addr":address
                                          }
                                        }
                            )

    # if it cant find any it will shutdown 
    return jsonify({"result": json_util.dumps(work)})


     
@app.route('/work/result', methods=['POST'])
def register_result():
   
    data = request.get_json(force=True)
     
    address = request.remote_addr 
    db = mongo_conn()

    
    if data:
    
        db.jobs.find_one_and_update({ "fpath":data["fpath"],
                                    "commitid":data["commitid"]
                                    },
                                    {"$set": { 
                                        "completed": True,
                                        "result": data["result"]
                                        }
                                    }
                                )
    if (db.jobs.find({"completed": False}).count() ==0):
        f_path = "results/{}.csv".format(os.environ["PATTERN"]) 
        
        print("WORK_COMPLETE",file=sys.stderr)
        record_results(f_path)
        
    return jsonify({})
def push_work():
    """
        Work pushing 
        This assumes that workers run on localhost
    """
    db = mongo_conn()
    ports = worker_node_ports()
    tol_workers = len(ports)
    # dont have to do this because they are on localhost
    
    client = MongoClient("localhost",27017)
    db = client.test_database
    
    while True:
        # need to check if it was already assigned and have not timed out
        try:    
           
            reg_workers = list(db.workers.find({})) 
            if len(reg_workers) != 0:
                timeout = datetime.datetime.now() - datetime.timedelta(minutes=1)
                jobs = db.jobs.find({"$and": [{"completed": False},
                                {"assigned_time": {"$lt": timeout}}]}
                                )
                i = 0
                #print(" job length {}". format(len(list(jobs))))

                for job in jobs:
                    
                    _id = job.pop('_id', None)
                    if i == len(reg_workers): # sort of round robin
                        i = 0
                    # port = ports[i]
                    port = reg_workers[i]["port"]
                    url = "http://localhost:{}/client/dowork".format(port)
                    print(url,  len(reg_workers))
                    send_post_msg(url, job)

                    
                    db.jobs.find_and_modify({"_id":_id},
                     {"$set": {'assigned_time': datetime.datetime.now()}}, 
                     upsert=False)
                    
                    
                    i+=1
            else:
                print("no workers"+ str(list(db.workers.find({})) ),file=sys.stderr)
                time.sleep(3)
        except ConnectionError as e:
            print("connection error going to sleep for 5 seconds")
            time.sleep(5)
                                     
  


 

if __name__ == '__main__':
    
    """   
 
    """
    
    # import resource 
    # t = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1024
    # print(t)
    # print (resource.getrusage(resource.RUSAGE_THREAD).ru_maxrss / 1024, 'MB')
    # print(resource.getrusage(resource.RUSAGE_SELF))
    
    if os.environ.get("REPO")== "" or  os.environ.get("REPO") == None  :
        repo = "https://github.com/cpbuckingham/python.git"
    else:
        repo = os.environ.get("REPO")

    compile_work(repo)

    target_func = None 
    # by default work stealing, therefore there
    # is no need to spawn a new thread

    # push_work function is used for master slave 
    if os.environ.get("PATTERN") == "MASTER_SLAVE" or  os.environ.get("PATTERN") == "WORK_PUSHING":
        target_func = push_work 
        print("heree")
        t = threading.Thread(target=target_func, args = ())
        t.daemon = True
        t.start()
    
    else: # by default work stealing
        
        os.environ["PATTERN"] = "WORK_STEALING" 
 
        
    

    app.run(host='0.0.0.0', port=MANAGER_PORT, processes = 7)