Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/bionic64"

  config.vm.provision "shell", inline: <<-SHELL
    sudo apt-get update
    sudo apt-get install git -y
    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh
    bash ~/miniconda.sh -b -p $HOME/miniconda
    ~/miniconda/bin/conda create -n alphapose python=3.6 -y
    conda activate alphapose
    conda install pytorch==1.1.0 torchvision==0.3.0
    git clone https://github.com/MVIG-SJTU/AlphaPose.git
    cd AlphaPose
    export PATH=/usr/local/cuda/bin/:$PATH
    export LD_LIBRARY_PATH=/usr/local/cuda/lib64/:$LD_LIBRARY_PATH
    python -m pip install cython
    sudo apt-get install libyaml-dev
    ################Only For Ubuntu 18.04#################
	sudo apt-get install locales
	export LANG=C.UTF-8
	######################################################
	python setup.py build develop
  SHELL
end