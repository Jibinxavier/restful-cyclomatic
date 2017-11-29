import base64
import hashlib
import json
import random
import string
import lizard
import time
import os
import sys
from flask import Flask
from flask import jsonify
from flask import request
from pymongo import MongoClient


from flask import Flask
from flask_pymongo import PyMongo

import requests
from config import MANAGER_URL

import threading    
from helper import send_post_msg, get_msg, git_clone, git_checkout

import resource

from requests.exceptions import ConnectionError
app = Flask(__name__)

def register():
    """
        Send a post request to register with manager
    """
    while True:
    
        try:
            send_post_msg(MANAGER_URL + '/client/register', {})
            print("completed registration")
            break
        except ConnectionError as e:
            print("connection refused. Going to sleep")
            time.sleep(5)
def send_result(data):
    send_post_msg(MANAGER_URL + '/work/result', data)

def __dowork__(config):
    
    result = {}
    start = resource.getrusage(resource.RUSAGE_SELF) # resource metrics

    print("Got job {} Commit id {} ".format(config["fpath"], config["commitid"]))
 
    git_clone(config["repo_url"],config["repo_path"])

    git_checkout(config["commitid"],config["repo_path"])
    cyclomatic = lizard.analyze_file(config["fpath"]).average_cyclomatic_complexity

    result = {"fpath":config["fpath"], 
                "result": cyclomatic, 
                "commitid":config["commitid"]
                }
    end =  resource.getrusage(resource.RUSAGE_SELF) #resource metrics


    diff_ucpu = now.ru_utime - start.ru_utime
    print("time in user mode {}".format(diff_ucpu))

    print (resource.getrusage(resource.RUSAGE_THREAD).ru_maxrss / 1024, 'MB')
    print (resource.getrusage(resource.RUSAGE_THREAD).ru_maxrss / 1024, 'MB')
    
    send_result(result)  # return result
    
     
    

def request_work():
    """
        this may have to be separate thread
        dont want to block the main app
    """ 
    no_work_timeout = 5
    while True:
     
        try:
            data = get_msg(MANAGER_URL + '/client/work')
            if data is None:
                print("No job")
                no_work_timeout-=1

                if(no_work_timeout <=0):
                    print("="*40)
                    print("Finished computation shutting down")
                    print("="*40)
                    break

                time.sleep(2)

            else :
                __dowork__(data)
             
        except ConnectionError as e:
            print("connection error going to sleep for 5 seconds")
            time.sleep(5)
        # if data is empty wait work, sleepfor 5 minutes if empty again shutdown
        
        
     
@app.route('/client/dowork', methods=['POST'])
def dowork():
    pass


if __name__ == "__main__":
    """
        At the start manager might be slow to start
    """
    
    register()
    t = threading.Thread(target=request_work, args = ())
    t.daemon = True
    t.start()
    port = os.environ.get('port',8090)
    app.run(host='0.0.0.0', port=port )
    