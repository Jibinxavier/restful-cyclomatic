import os
import matplotlib.pyplot as plt
import pandas as pd
def get_all_results():
    
    results = {}
    for file in os.listdir("./results") :
        metrics = pd.read_csv("./results/" +file)
        pattern = file.split(".csv")[0]
        results[pattern]= metrics
    return results

def create_graphs():
    
    fig, ax = plt.subplots( nrows=1, ncols=1 )
    results = get_all_results()
    colors = ["g", "r", "b"]
    ax.set_ylabel('Time Taken in seconds')
    width = 0.27 
    for (i, k) in enumerate(results):
      
        ax.bar(results[k]["n_workers"] + width * i, results[k]["time_taken"],width=width,color=colors[i], label=k)
       
   
    ax.set_xlabel('Number of workers')
    ax.legend(bbox_to_anchor=(1.006, 1.0), loc=1, borderaxespad=0.2)
    fig.savefig('result.png')   # save the figure to file
    plt.close(fig)    # close the figure

 
if __name__ == "__main__":
    create_graphs()