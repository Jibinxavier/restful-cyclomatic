import base64
import hashlib
import json
import random
import string
import datetime
 
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
 
  
client = MongoClient("localhost",27017)
db = client.test_database
db.workers.drop()
db.jobs.drop()
def compile_work(ext=".py"):
    # get all the commits
    # get all the files
    # store them in the database
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
                "assigned_time": initial,
                "completed": False,
                "worker_addr": "",
                "result": 0
                }
            )

         
@app.route('/client/register', methods=['POST'])
def register_worker():
    
    data = request.get_json(force=True)
     
    address = request.remote_addr
    user_id = db.workers.count()
     
    db.workers.insert(
        {"user_id": user_id,
        "address":address 
        }
    )
    return jsonify({})
 
@app.route('/client/work', methods=['GET'])
def distribute_work():
    # do query to find out to get a job
    # if the job time is beyond 2 mins assign it to someone else   
    address = request.remote_addr

    # need to check if it was already assigned and have not timed out
    timeout = datetime.datetime.now() - datetime.timedelta(minutes=1)
    work =db.jobs.find_one_and_update({"completed": False,"assigned_time":{"$lt": timeout}},
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
  
    if data:
        db.jobs.find_one_and_update({"worker_addr":address,
                                    "fpath":data["fpath"]
                                    },
                                    {"$set": { 
                                        "completed": True,
                                        "result": data["result"]
                                        }
                                    }
                                )
    return jsonify({})
if __name__ == '__main__':
    
    """x    
    Manager needs to build a queue of (commitid file folder)

    """ 
    

    compile_work()
    app.run(host='0.0.0.0', port=MANAGER_PORT)