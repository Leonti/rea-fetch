#!/usr/bin/env bash

./rea-fetch

cd /root/reaResults

for i in */; do
    zip -r "${i%/}.zip" "$i";
    rm -rf "$i"
done

ls /root/reaResults
