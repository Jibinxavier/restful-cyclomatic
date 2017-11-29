from pymongo import MongoClient
from bson import json_util
client = MongoClient("localhost",27017)
db = client.test_database
cursor = db.jobs.find({})
for document in cursor:
        print(document)
cursor = db.jobs.find({"completed": False,})
print("\n \n \n")
for document in cursor:
        print(document)
print(db.jobs.find({}).count())
print("left to do {}". format(db.jobs.find({"completed": False,}).count()))

res = db.jobs.aggregate([ 
    { "$group": { "_id": {},"max": { "$max": "$assigned_time" },"min": { "$min": "$assigned_time" } 
    }}
])
print(list(res)) 