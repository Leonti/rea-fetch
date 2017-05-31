#!/usr/bin/env bash

./rea-fetch

cd /root/reaResults
pages=$(ls -R /root/reaResults)

for i in */; do
    zip -r "${i%/}.zip" "$i";
    rm -rf "$i"
done

cd /root/reaSoldResults
soldPages=$(ls -R /root/reaSoldResults)

for i in */; do
    zip -r "${i%/}-sold.zip" "$i";
    rm -rf "$i"
done

#ls -R /root/reaResults

aws s3 cp /root/reaResults s3://leonti-rea-crawler/ --recursive
aws s3 cp /root/reaSoldResults s3://leonti-rea-crawler/ --recursive

soldPagesText=$'\nSold properties:\n\n'$soldPages
content=$'New pages have been fetched from REA website:\n\n'$pages$soldPagesText

subject="REA Fetch results "$(date +"%Y.%m.%d")

echo "$content"
echo "$subject"

curl -s --user 'api:'$MAILGUN_API_KEY \
    https://api.mailgun.net/v3/leonti.me/messages \
    -F from='REA Fetch <leonti@leonti.me>' \
    -F to=prishelec@gmail.com \
    -F subject="$subject" \
    -F text="$content"
