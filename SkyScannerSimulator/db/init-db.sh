#!/bin/bash
scripts=`ls scripts/*.sql`
for script in $scripts
do
	for i in {1..50};
	do
	    /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P $SA_PASSWORD -d master -i $script
	    if [ $? -eq 0 ]
	    then
		echo "$script completed"
		break
	    else
		echo "not ready yet..."
		sleep 1
	    fi
	done
done
