#!/bin/bash
#
# I am potentially invoked when ping-service.sh (which see) fails to
# contact the racket-pkg-website service.
#

# Request a dump of running threads:
dumprequestfile=$HOME/racket-pkg-website/signals/.dumpinfo
touch $dumprequestfile

# Wait a few seconds for the dump to complete:
sleep 10

# Tar up the most recent few hours' worth of logs:
logarchive=$HOME/ping-failure-logs-$(date +%Y%m%d%H%M%S).tar.gz
(
    cd $HOME/service/racket-pkg-website/log/main/; \
    ls -tr | tail -n 10 | xargs tar -zcf $logarchive \
)

# Restart the service using daemontools.
if [ -f $dumprequestfile ]
then
    # If the `.dumpinfo` signal is still there after our sleep, then
    # the process is so far off the rails we shouldn't bother waiting
    # for it, so kill it hard.
    echo "Killing service hard and restarting it."
    svc -dku $HOME/service/racket-pkg-website
else
    # Otherwise, it's at least partially awake, so try asking it
    # nicely.
    echo "Politely requesting service termination before restart."
    svc -du $HOME/service/racket-pkg-website
fi

# Finally, complain out loud. We expect to be running in some kind of
# cron-ish context, so the output we produce here will likely find its
# way into an email to a responsible party.
echo "racket-pkg-website on-ping-service-failure.sh invoked. logarchive=$logarchive"
