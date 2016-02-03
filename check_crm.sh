#!/bin/bash
#
# Check pacemaker resources and nodes via crmsh

PREFIX=/tmp/check_pacemaker

RESLIST=$PREFIX.resources.list
RESSTAT=$PREFIX.resources.stat
RESERR=$PREFIX.resources.err
NODEONL=$PREFIX.node.online
NODEOFFL=$PREFIX.node.offline
NODESTANDBY=$PREFIX.node.standby

ERRRES=0
ERRNODE=0

case "$1" in
        resources)
                ###############################################
                # GET RESOURCE LIST
                crm configure show |grep "primitive\|ms" |grep -v "colocation\|order\|params" |awk '{ print $2 }' > $RESLIST
                if [ $? -gt 0 ];then
                        echo "ERROR getting resources!"
                        exit
                fi

                ###############################################
                # CHECK RESOURCE STATUS
                for res in `cat $RESLIST`; do
                        crm resource status $res >> $RESSTAT 2>&1
                done

                ###############################################
                # CHECK FOR FAILED RESOURCES
                cat $RESSTAT |grep "NOT" > $RESERR
                if [ -s $RESERR ];then
                        ERRRES=1
                fi

                ###############################################
                # STATUS OUTPUT
                if [ $ERRRES -gt 0 ];then
                        STATUS="CRITICAL - `cat $RESERR`"
                elif [ $ERRRES -eq 0 ];then
                        STATUS="OK - all resources running"
                else
                        STATUS="UNKNOWN - please check manually!"
                fi

                ###############################################
                # LONG OUTPUT
                LONGOUTPUT=`cat $RESSTAT`

                ###############################################
                # FINAL OUTPUT
                echo "$STATUS\n"
                echo "$LONGOUTPUT"

                ###############################################
                # CLEANUP
                test -f $RESLIST && rm $RESLIST
                test -f $RESSTAT && rm $RESSTAT
                test -f $RESERR && rm $RESERR
        ;;
        nodes)
                ###############################################
                # CHECK ONLINE
                crm_mon -1 |grep -i "online" > $NODEONL
                if [ $? -gt 0 ];then
                        ERRNODE=2
                fi

                ###############################################
                # CHECK OFFLINE
                crm_mon -1 |grep -i "offline" > $NODEOFFL

                ###############################################
                # CHECK STANDBY
                crm_mon -1 |grep -i "standby" > $NODESTANDBY
                if [ $? -eq 0 ];then
                        ERRNODE=1
                fi

                ###############################################
                # STATUS OUTPUT
                if [ $ERRNODE -gt 1 ];then
                        STATUS="CRITICAL - `cat $NODEOFFL`"
                elif [ $ERRNODE -eq 1 ];then
                        STATUS="WARNING - `cat $NODEOFFL $NODESTANDBY`"
                elif [ $ERRNODE -eq 0 ];then
                        STATUS="OK - all nodes are online"
                else
                        STATUS="UNKNOWN - please check manually!"
                fi

                ###############################################
                # LONG OUTPUT
                LONGOUTPUT=`crm_mon -1 |grep -i "online\|offline\|standby"`

                ###############################################
                # FINAL OUTPUT
                echo "$STATUS\n"
                echo "$LONGOUTPUT"
        ;;
        *)
                echo "Usage only (resources | nodes)"
                exit
        ;;
esac
