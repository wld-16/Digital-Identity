Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/bionic64"
  config.vm.provider "virtualbox" do |v|
    v.memory = 8192
    v.cpus = 4
    v.gui = true
  end

  config.vm.provision "shell", inline: <<-SHELL
    sudo apt-get update
    sudo apt-get install git -y
    sudo add-apt-repository ppa:deadsnakes/ppa -y
    sudo apt install python3.8 -y
    sudo apt install python3-pip -y
    git clone -b pytorch https://github.com/MVIG-SJTU/AlphaPose.git
    pip3 install torch==0.4.0
    pip3 install torchvision==0.2.0
    pip3 install visdom
    pip3 install nibabel
    pip3 install pandas
    pip3 install tqdm
    pip3 install matplotlib
    pip3 install opencv-python
    pip3 install scikit-build
    pip3 install numpy
    pip3 install cmake
    sudo apt install cmake -y
    pip3 install -r  -vv AlphaPose/requirements.txt
    echo "Now provide the models"
  SHELL
  config.vm.provision "file", source: "./duc_se.pth", destination: "/tmp/duc_se.pth"
  config.vm.provision "file", source: "./yolov3-spp.weights", destination: "/tmp/yolov3-spp.weights"

  config.vm.provision "shell", inline: <<-SHELL 
    sudo mv /tmp/duc_se.pth /home/vagrant/AlphaPose/models/sppe/duc_se.pth
    sudo mv /tmp/yolov3-spp.weights /home/vagrant/AlphaPose/models/yolo/yolov3-spp.weights
  SHELL

  config.vm.provision "shell", inline: "sudo sed -i 's/^# deb/deb/g' /etc/apt/sources.list" 
  config.vm.provision "shell", inline: "sudo apt-get update && sudo apt-get upgrade -y"
  config.vm.provision "shell", inline: "sudo loadkeys fr"
  config.vm.provision "shell", inline: "sudo apt-get install -y gnome-session gdm3"
  config.vm.provision "shell", inline: "sudo sed -i 's/XKBLAYOUT=\"us\"/XKBLAYOUT=\"fr\"/g' /etc/default/keyboard" 
  config.vm.provision "shell", inline: "sudo apt-get install -y virtualbox-guest-dkms virtualbox-guest-utils virtualbox-guest-x11"
  config.vm.provision "shell", inline: "sudo shutdown -r now"
end