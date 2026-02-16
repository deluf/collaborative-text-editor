
# Installation

VMs:
- 10.2.1.23 (RIP)
- 10.2.1.24 (node1, Nginx)
- 10.2.1.41 (node2)
- 10.2.1.42 (node3)

On all nodes:
```bash
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
mv rebar3 /usr/local/bin/

git clone https://github.com/deluf/collaborative-text-editor

cd collaborative-text-editor/erlang/

rebar3 shell --name node{N}@{IP} --setcookie collaborative-text-editor --config node.config
```

On the NginX node:
```bash
apt update

apt install nginx
systemctl start nginx
systemctl enable nginx
rm -rf /var/www/html/*

cd --
mv collaborative-text-editor/web/* /var/www/html/

# Write this in /etc/nginx/sites-available/default
http {
    # ---------------------------------------------------------
    # 1. Global Settings for WebSockets & Load Balancing
    # ---------------------------------------------------------

    # Map directive for WebSocket upgrade headers
    map $http_upgrade $connection_upgrade {
        default upgrade;
        '' close;
    }

    # Your Erlang cluster with consistent hashing
    upstream erlang_nodes {
        hash $uri consistent;

        server 10.2.1.24:8086;
        server 10.2.1.41:8086;
        server 10.2.1.42:8086;
    }

    # ---------------------------------------------------------
    # 2. Web Server (Static Files on Port 80)
    # ---------------------------------------------------------
    server {
        listen 80 default_server;
        listen [::]:80 default_server;

        root /var/www/html;
        server_name _;

        location / {
            try_files $uri $uri/ =404;
        }

        location = /note {
            try_files /note.html =404;
        }
    }

    # ---------------------------------------------------------
    # 3. WebSocket Load Balancer (Proxy on Port 8080)
    # ---------------------------------------------------------
    server {
        listen 8080;
        server_name _;

        location / {
            proxy_pass http://erlang_nodes;

            # Essential headers for WebSocket Upgrade
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection $connection_upgrade;

            # Forward the original host and client IP
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;

            # Prevent NGINX from dropping long-lived, idle WebSocket connections
            proxy_read_timeout 3600s;
            proxy_send_timeout 3600s;
        }
    }
}
##########################################

nginx -t
systemctl reload nginx
```


---

TODO:
rename mycookie to something more sensato (tipo collaborative-text-editor)
vedere se un erlang crasha cosa fa nginx
cosa succede se un erlang va giu (si riconnettono dopo?)

Maximum number of active users per document, with extra users having to wait in queue ?
List of currently active users ?
Lo username puo essere infinito lol