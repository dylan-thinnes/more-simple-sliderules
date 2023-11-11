echo """
<html><body>
<svg width=1800 height=1000>
`cat "$1" | tr '\n' ' ' | sed -E 's/<[^>]*>//;s/<[^>]*>//;s/<[^>]*>//;s|</svg>||'`
</svg>
</body></html>
""" > $1.html
