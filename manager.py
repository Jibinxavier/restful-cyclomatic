import base64
import hashlib
import json
import random
import string
import datetime
import csv
from flask import Flask
from flask import jsonify
from flask import request
from pymongo import MongoClient

from flask import Flask 
from bson import json_util
from config import MANAGER_PORT, REPO_URL

from helper import get_all_commits, get_all_files, git_checkout

app = Flask(__name__)

# connect to MongoDB with the defaults
 
def mongo_conn():
    client = MongoClient("localhost",27017)
    return client.test_database
db = mongo_conn()
db.workers.drop()
db.jobs.drop()



def compile_work(ext=".py"):
    # get all the commits
    # get all the files
    # store them in the database
    db = mongo_conn()
    repo_path = "/tmp/" + REPO_URL.split("/")[-1].split(".")[0]
    commits = get_all_commits(REPO_URL,repo_path)
    for c in commits:
        git_checkout(c, repo_path)
        print("commit "+ str(c))
        initial = datetime.datetime.now() - datetime.timedelta(minutes=10)
        for file in get_all_files(repo_path, ext):
            db.jobs.insert(
                {"commitid": str(c),
                "fpath":file,
                "repo_path": repo_path,
                "repo_url": REPO_URL,
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
        "address":address 
        }
    )
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
    time_now = datetime.datetime.now()
    earliest_job = db.jobs.aggregate([ 
        { "$group": { "_id": {},"min": { "$min": "$assigned_time" }}}
        ])
    earliest_job = list(earliest_job)[0]['min']
    return (time_now-earliest_job).total_seconds()

def record_results(file):
    db = mongo_conn() 
    n_workers = db.workers.find({}).count() 
    avg = get_avg_cyclo(db) 
    time_taken = get_time_taken(db)  
    print(" work done. The average complexity is {} ".format(avg)) 
 
    with  open(file, 'a') as csvfile:   
        writer = csv.writer(csvfile, delimiter=',',) 
        writer.writerow([avg,  n_workers, time_taken])
     
@app.route('/work/result', methods=['POST'])
def register_result():
    
    data = request.get_json(force=True)
     
    address = request.remote_addr 
    db = mongo_conn()

    
    if data:
        db.jobs.find_one_and_update({"worker_addr":address,
                                    "fpath":data["fpath"],
                                    "commitid":data["commitid"]
                                    },
                                    {"$set": { 
                                        "completed": True,
                                        "result": data["result"]
                                        }
                                    }
                                )
    if (db.jobs.find({"completed": False}).count() ==0):
       record_results("results/time_taken.csv")
        
    return jsonify({})
if __name__ == '__main__':
    
    """x    
    Manager needs to build a queue of (commitid file folder)

    """
    import resource 
    t = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1024
    print(t)
    print (resource.getrusage(resource.RUSAGE_THREAD).ru_maxrss / 1024, 'MB')
    print(resource.getrusage(resource.RUSAGE_SELF))
    
    app.run(host='0.0.0.0', port=MANAGER_PORT, processes = 7)