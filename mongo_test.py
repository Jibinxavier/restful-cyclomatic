from pymongo import MongoClient
client = MongoClient("localhost",27017)
db = client.test_database
cursor = db.jobs.find({})
for document in cursor:
        print(document)
