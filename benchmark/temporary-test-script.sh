reset
stack install
git pull --rebase
# (for _ in $(seq 100) ; do for f in *bug* ; do echo $f ; pun --check $f ; done ; done) > testoutput.txt
git add testoutput.txt
git commit -m "testoutput"
git push
