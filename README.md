
# Installation

VMs:
- 10.2.1.23 (RIP)
- 10.2.1.24 (already set-up)
- 10.2.1.41
- 10.2.1.42

First:

```bash
apt update

apt install nginx
systemctl start nginx
systemctl enable nginx
rm -rf /var/www/html/*

# Write this in /etc/nginx/sites-available/default under the other location rule
location = /note {
    try_files /note.html =404;
}

wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
mv rebar3 /usr/local/bin/

git clone https://github.com/deluf/collaborative-text-editor

mv collaborative-text-editor/web/static/ /var/www/html/
mv collaborative-text-editor/web/templates/* /var/www/html

cd collaborative-text-editor/erlang/
```

Then:

- Single-node setup:
```bash
rebar3 shell
```

- Distributed setup:
```bash
rebar3 shell --name node{N}@127.0.0.1 --setcookie mycookie --config config/node{N}.config
```
