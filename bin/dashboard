#!/bin/bash

# Check if the file with hostnames is provided
if [ -z "$1" ]; then
    hostfile="${HOME}/.ssh/racimolab.txt"
else
    hostfile=$1
fi

session_name=$(basename $hostfile | sed 's/\..*$//')

if ! tmux ls | grep -q "^$session_name:"; then
    echo "Loading hostnames from ${hostfile} and creating a tmux session"

    # Read the file and store the hostnames in an array
    declare -a HOSTNAMES
    while IFS= read -r line; do
        HOSTNAMES+=("$line")
        echo $line
    done < "$hostfile"

    if [ ${#HOSTNAMES[@]} -eq 0 ]; then
        echo "No hostnames found in the file"
        exit 1
    fi

    # Create a new tmux session with the first host
    echo "Connecting to ${HOSTNAMES[0]}..."
    tmux new-session -d -s "$session_name" -n ${HOSTNAMES[0]} "ssh ${HOSTNAMES[0]}"

    # Iterate through the rest of the hostnames and create a new window for each
    for i in "${HOSTNAMES[@]:1}"; do
        echo "Connecting to ${i}..."
        tmux new-window -t "$session_name" -n $i "ssh ${i}"
    done
else
    echo "A tmux session ${session_name} already exists!"
fi

tmux attach -t $session_name

