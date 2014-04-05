# !/bin/bash

docker run -expose 3306 -p 3306:3306 -d --name mysql mysql-server /usr/bin/mysqld_safe
