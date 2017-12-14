
#!/bin/bash
 
 

DEFAULT_NUM_W=5

 
# if [ -z "$1" ]
#   then
   
#     python create_fileservers.py $DEFAULT_NUM_FS
# else
#     python create_fileservers.py $1
# fi

#
# docker build -t cyclomatic_complexity .
docker rm -f $(docker ps -a -q)
patterns=( WORK_STEALING WORK_PUSHING MASTER_SLAVE)
 
# creating containers and starting them
for pattern in ${patterns[@]}
do  
    for i in $(seq 1 $DEFAULT_NUM_W); 
    do 
        printf  "\nExecuting config with:- pattern %s number of workers %d \n\n\n" $pattern $i
        python benchmark_config_builder.py  --worker_n $i --pattern $pattern 
     
        docker-compose -f benchmark-compose.yml up -d   #putting it to background
        while true
        do 
            if docker logs res_master 2>&1 |  grep -Fxq "WORK_COMPLETE"  
            then
                echo "Got result for ${pattern}"
                break
            else
                sleep 20
            fi
        done
        docker rm -f $(docker ps -a -q)
    done
    
     
done
python plot_results.py
 