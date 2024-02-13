### Code for Paper: (Submitted to Journal of Artificial Intelligence Research) 
## On the Finite Axiomatisability of Qualitative Distance Logicsover 2D Euclidean Space

### Set Up && Environment
- Running environment: Ubuntu 18.04
- Programming environment: Poplog (can be downloaded via [this link](https://www.cs.bham.ac.uk/research/projects/poplog/freepoplog.html))
- Installation script:
```
sudo apt-get update && sudo apt-get install -y gcc build-essential tcsh libc6 libncurses5 libncurses5-dev libstdc++6 libxext6 libxext-dev libx11-6 libx11-dev libxt-dev libmotif-dev ncurses-dev patch xterm wget espeak

echo "check_certificate=off" >> /etc/wgetrc

mkdir ~/poplog

cd ~/poplog

wget http://www.cs.bham.ac.uk/research/projects/poplog/V16/getpoplog.sh

bash ./getpoplog.sh -nopie

echo "export usepop=`cat ~/poplog/poplog_base/USEPOP`" >> ~/.bashrc

echo "source ~/poplog/poplog_base/pop/com/poplog.sh" >> ~/.bashrc
```

### Run Code
- To run the Pop11 reasoner: 
    - `` pop11 Poplog/new_LD_reasoner.p``
- To run the Z3 Python reasoner:
     - `` python3 Z3/LBPT_NF.py``

### Contact
- For any question about this paper and project, please contact Dr. Heshan Du (Heshan.Du@nottingham.edu.cn).