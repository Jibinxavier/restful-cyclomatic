import base64
import hashlib
import json
import random
import string
 
from flask import Flask
from flask import jsonify
from flask import request
from pymongo import MongoClient

from flask import Flask
from flask_pymongo import PyMongo

import requests
from config import MANAGER_URL


from helper import send_post_msg, get_msg
app = Flask(__name__)

def register():
    """
        Send a post request to register with manager
    """
    send_post_msg(MANAGER_URL + '/client/register', {})

def send_result(data):
    send_post_msg(MANAGER_URL + '/work/result', data)

def __dowork__(config):

    result = {}
 
    if config is None:
        print("No job")
        #sleep
    else:
        print( "Got job "+ config["fpath"] )
        cyclomatic = 2
        
        result = {"fpath":config["fpath"], 
                    "result": cyclomatic}
        send_result(result)  # return result
    
     
    

def request_work():
    """
        this may have to be separate thread
        dont want to block the main app
    """ 
    data = get_msg(MANAGER_URL + '/client/work')
    # if data is empty wait work, sleepfor 5 minutes if empty again shutdown
    __dowork__(data)
     
@app.route('/client/dowork', methods=['POST'])
def dowork():
    pass


if __name__ == "__main__":
    register()
    request_work()