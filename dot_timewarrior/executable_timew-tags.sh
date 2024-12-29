#!/bin/bash

tags=$(timew tags | \
	        tail -n+4 | \
	        head -n-1 | \
		    grep -P '^.*[^\s](?=\s+-\s*)' -o | \
		    sort
)
tagsarray=($tags)

for i in "${!tagsarray[@]}"; do
    printf "%s) %s\n" "$i" "${tagsarray[$i]}"
done

printf 'Select an interface from the above list: '
IFS= read -r opt

echo "you selected ${tagsarray[$opt]}" 
