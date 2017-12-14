import base64
import hashlib
import json
import random
import string
import lizard
import time
import os
import sys
import queue
from flask import Flask
from flask import jsonify
from flask import request
from pymongo import MongoClient 
from flask import Flask
from flask_pymongo import PyMongo
import threading 
import requests
from config import MANAGER_URL

   
from helper import send_post_msg, get_msg, git_clone, git_checkout
from concurrent.futures import ThreadPoolExecutor
import resource

from requests.exceptions import ConnectionError
app = Flask(__name__)
q = queue.Queue()
def register():
    """
        Send a post request to register with manager
    """
    while True:
    
        try:
            send_post_msg(MANAGER_URL + '/client/register', {"port": os.environ.get('port',8083)})
            print("completed registration")
            break
        except ConnectionError as e:
            print("connection refused. Going to sleep")
            time.sleep(5)
def send_result(data):
    send_post_msg(MANAGER_URL + '/work/result', data)

def calc_cyclo(config):
    
    result = {}
    # start = resource.getrusage(resource.RUSAGE_SELF) # resource metrics

    print("Got job {} Commit id {} ".format(config["fpath"], config["commitid"]))
 
    git_clone(config["repo_url"],config["repo_path"])

    git_checkout(config["commitid"],config["repo_path"])
    cyclomatic = lizard.analyze_file(config["fpath"]).average_cyclomatic_complexity

    result = {"fpath":config["fpath"], 
                "result": cyclomatic, 
                "commitid":config["commitid"]
                }
    # end =  resource.getrusage(resource.RUSAGE_SELF) #resource metrics


    # diff_ucpu = end.ru_utime - start.ru_utime
    # print("time in user mode {}".format(diff_ucpu))

    # print (resource.getrusage(resource.RUSAGE_THREAD).ru_maxrss / 1024, 'MB')
    # print (resource.getrusage(resource.RUSAGE_THREAD).ru_maxrss / 1024, 'MB')
    
    send_result(result)  # return result
    
     
    

def request_work():
    """
        Work-Stealing pattern worker, asks for work
        this may have to be separate thread
        dont want to block the main app
    """ 
    no_work_timeout = 5
    while True:
     
        try:
            config = get_msg(MANAGER_URL + '/client/work')
            if config is None:
                print("No job")
                no_work_timeout-=1

                if(no_work_timeout <=0):
                    print("="*40)
                    print("Finished computation shutting down")
                    print("="*40)
                    break

                time.sleep(2)

            else :
                calc_cyclo(config)
             
        except ConnectionError as e:
            print("connection error going to sleep for 5 seconds")
            time.sleep(5)
        # if data is empty wait work, sleepfor 5 minutes if empty again shutdown
        
        
def __do_work__():
    timeout = 10
    while True: 
        if q.empty():
            timeout -=1
            time.sleep(5) 
            if timeout == 0:
                break 
        else:
            timeout = 10
            config = q.get()
            calc_cyclo(config)
def __master_slave__():
    target_func = __do_work__
    t = threading.Thread(target=target_func, args =())
    t.daemon = True
    t.start()
    


@app.route('/client/dowork', methods=['POST'])
def dowork():
    """
        populate the queue 
    """
    data = request.get_json(force=True)
    q.put(data)
   
    return jsonify({})


if __name__ == "__main__":
    """
        At the start manager might be slow to start
    """
    
    register()
    target_func = request_work
    args = ()
    if os.environ.get("PATTERN") == "MASTER_SLAVE":
        target_func = __do_work__ 
        executor = ThreadPoolExecutor(max_workers=1000)
        a = executor.submit(target_func)
    elif os.environ.get("PATTERN") == "WORK_PUSHING":
        target_func = __do_work__
        t = threading.Thread(target=target_func, args =())
        t.daemon = True
        t.start()
   
    else: # by default work stealing
        target_func = request_work
        t = threading.Thread(target=target_func, args =())
        t.daemon = True
        t.start()
    # if os.environ.get("PATTERN") == "WORK_STEALING":
    #     target_func = request_work
    #     t = threading.Thread(target=target_func, args =())
    #     t.daemon = True
    #     t.start()
    # elif os.environ.get("PATTERN") == "WORK_PUSHING":
    #     target_func = __do_work__
    #     t = threading.Thread(target=target_func, args =())
    #     t.daemon = True
    #     t.start()
   
    # by default it is master slave
        
    


    
    port = os.environ.get('port',8083)
    app.run(host='0.0.0.0', port=int(port) )
    