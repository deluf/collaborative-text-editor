
# Installation instructions

Nodes:
- 10.2.1.3   (Nginx)
- 10.2.1.24 (Erlang node1)
- 10.2.1.41 (Erlang node2)
- 10.2.1.42 (Erlang node3)

---

## Erlang nodes

Run once:
```bash
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
mv rebar3 /usr/local/bin/
```

Then, create a `deploy.sh` script to automatically update and run the application:
```bash
clear
cd --
rm -rf collaborative-text-editor
git clone https://github.com/deluf/collaborative-text-editor
cd collaborative-text-editor/erlang
rebar3 shell --name node{N}@{IP} --setcookie collaborative-text-editor --config node.config
```

---

## Nginx node

Run once:
```bash
apt update
apt install nginx
systemctl start nginx
```

Finally, create a `deploy.sh` script to automatically update and run the application:
```bash
clear
cd --
rm -rf collaborative-text-editor
git clone https://github.com/deluf/collaborative-text-editor
rm -rf /var/www/html/*
mv collaborative-text-editor/web/* /var/www/html/
cp collaborative-text-editor/nginx.conf /etc/nginx/sites-available/default
nginx -t
systemctl reload nginx
```
