#!/bin/bash
#
# I am potentially invoked when ping-service.sh (which see) fails to
# contact the racket-pkg-website service.
#

# Request a dump of running threads:
touch $HOME/racket-pkg-website/signals/.dumpinfo

# Wait a few seconds for the dump to complete:
sleep 10

# Tar up the most recent few hours' worth of logs:
logarchive=$HOME/ping-failure-logs-$(date +%Y%m%d%H%M%S).tar.gz
(
    cd $HOME/service/racket-pkg-website/log/main/; \
    ls -tr | tail -n 10 | xargs tar -zcf $logarchive \
)

# Restart the service using daemontools:
svc -du $HOME/service/racket-pkg-website

# Finally, complain out loud. We expect to be running in some kind of
# cron-ish context, so the output we produce here will likely find its
# way into an email to a responsible party.
echo "racket-pkg-website on-ping-service-failure.sh invoked. logarchive=$logarchive"
