import argparse
import glob
import requests
import json
import git
import os
def send_post_msg(url, data):
    headers = {'Content-type': 'application/json'}
     
    return requests.post(url, data=json.dumps(data), headers=headers)


def get_msg(url):
    res = requests.get(url).json()
    
    return json.loads(res["result"])

def get_all_commits(repoUrl,repo_path): 
    """
        returns a list of git.Commit objects
    """
    if not os.path.exists(repo_path):
        git.Git().clone(repoUrl, repo_path)
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
#   print(get_all_commits("https://github.com/cpbuckingham/python.git","/tmp/python"))

# test = get_all_commits("https://github.com/cpbuckingham/python.git","/tmp/python")
# 
# 