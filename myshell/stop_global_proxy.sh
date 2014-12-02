# !/bin/bash

systemctl stop shadowsocks@poecfw.service
systemctl stop redsocks
iptables-restore /etc/iptables/iptables.rules
