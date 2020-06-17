#!/bin/bash
gzip -c -r /var/www/dokuwiki/ > wiki.zip
rclone copy ./wiki.zip drive:Lore/Backups -P
rm wiki.zip
