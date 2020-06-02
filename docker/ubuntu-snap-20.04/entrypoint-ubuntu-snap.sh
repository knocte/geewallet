#!/usr/bin/env bash
set -euxo pipefail

# Update system
apt update -y && apt upgrade -y

# Install dependencies
DEBIAN_FRONTEND=noninteractive apt install lsb-release git docker.io -y

# Install snap and snapcraft
./docker/ubuntu-snap-20.04/scripts/install_snap.sh

# Build repo from source inside snappy container
docker exec snappy ./configure.sh --prefix=./staging
docker exec snappy make
docker exec snappy make install

# Install snapcraft and dependencies
docker exec snappy snap version
docker exec snappy snap install core20
docker exec snappy snap install --classic --stable snapcraft
docker exec snappy snapcraft --version

# Build snap package
docker exec snappy snapcraft --destructive-mode

# Upload snap package
cat <<EOF | docker exec --interactive -e SNAPCRAFT_LOGIN=$SNAPCRAFT_LOGIN snappy sh
if [ ! -e *.snap ]; then
    echo "No snap package found."
    exit 1
fi

if [ -e snapcraft.login ]; then
    echo "snapcraft.login found, skipping log-in"
else
    if [ ! -z "$SNAPCRAFT_LOGIN" ]; then
        echo "$SNAPCRAFT_LOGIN" > snapcraft.login
    else
        echo "No login details found, initiating manual logging-in..."
        snapcraft export-login snapcraft.login
    fi
fi
snapcraft login --with snapcraft.login
snapcraft push *.snap --release=beta
EOF
