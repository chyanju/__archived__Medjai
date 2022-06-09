#!/bin/bash
dpath="$1"

dfolder="$(dirname "${dpath}")"
dname="$(basename "${dpath}" ".cairo")"
opath="${dfolder}/${dname}_compiled.json"

# echo "${dpath}"
# echo "${dfolder}"
# echo "${dname}"
# echo "${opath}"

echo "# compiling ${dpath} to ${opath}..."
cairo-compile "${dpath}" --output "${opath}"
echo "# racket ./cairo-run.rkt --cname ${opath}"
racket ./cairo-run.rkt --cname ${opath}
