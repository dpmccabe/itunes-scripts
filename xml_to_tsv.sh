export PATH=/usr/local/bin/:$PATH

cat ~/Music/iTunes\ Library.xml | gawk 'match($0, /<key>(Track ID|Total Time|Track Number|Rating|Genre|Play Count|Play Date UTC|Grouping)<\/key><(string|integer|date)>(.+)<\/(string|integer|date)>/, m) { print m[1]":="m[3]; }' | tr '\n' '\t' | awk '{gsub(/\tTrack ID/, "\nTrack ID")}1' | grep -E '\tRating:=(60|80|100)\t' | awk '{gsub(/\t$/, "")}1' > ~/Music/starred.txt
