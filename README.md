
# Table of Contents



![img](https://img.shields.io/badge/Haskell-purple.svg?style=for-the-badge&logo=haskell&logoColor=white)
![img](https://img.shields.io/badge/Yesod-blue.svg?style=for-the-badge&logo=haskell&logoColor=white)
![img](https://img.shields.io/badge/MongoDB-%234ea94b.svg?style=for-the-badge&logo=mongodb&logoColor=white)

[Visit Tablatura here !](https://tablatura.jjba.org/)

This software is available under the GNU General Public License (GPL) v3.0

Tablatura is a CMS for string instrument tabs, made with Haskell.

Tablatura is a free (as in freedom) management system mostly meant for
tablatures, also known as tabs, of string instruments, such as guitar, bass,
banjo, ukelele, etc.

Easily save your favourite tabs and access them easily in a neatly
organized way.

The aim is to provide a self-hosting but also publicly hosted knowledge
base of music tabs, without aim of profit, only of project maintenance
through donations.

Created with Haskell and the amazing Yesod framework. The persistence
layer consists of MongoDB.

You are free to see all the source code powering Tablatura here:
[https://git.sr.ht/~teutonicjoe/tablatura](https://git.sr.ht/~teutonicjoe/tablatura)

[You can view some more technical documentation about tablatura here](https://docs.jjba.org/tablatura/).

This Makefile "makes" our life easier 😃

    # Copyright © 2022 Josep Bigorra
    #
    # This file is part of Tablatura.
    # Tablatura is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
    #   either version 3 of the License, or (at your option) any later version.
    #
    # Tablatura is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    #   without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    #
    # See the GNU General Public License for more details.
    # You should have received a copy of the GNU General Public License along with Foobar.
    # If not, see <https://www.gnu.org/licenses/>.
    
    #   run docker things only  after doing a commit and a =docker login= !!!
    $(eval TAG_NAME=$(shell git rev-parse HEAD))
    #TAG_NAME = $(git rev-parse HEAD)
    $(eval REGISTRY_URL="europe-west4-docker.pkg.dev/tablatura/tablatura-docker-images")
    
    spin-up-production:
            docker-compose --env-file config/.env -f config/docker/prod-docker-compose.yaml up -d;
    
    build-backend-docker-image: clean
            docker build -f config/docker/tablatura_backend.Dockerfile . -t tablatura_backend;
    tag-backend-image-with-current-sha:
            docker tag tablatura_backend "${REGISTRY_URL}/tablatura_backend:${TAG_NAME}";
    push-backend-image-with-current-sha:
            docker push "${REGISTRY_URL}/tablatura_backend:${TAG_NAME}";
    
    build-nginx-docker-image:
            docker build -f config/docker/tablatura_nginx.Dockerfile . -t tablatura_proxy;
    tag-nginx-image-with-current-sha:
            docker tag tablatura_proxy "${REGISTRY_URL}/tablatura_proxy:${TAG_NAME}";
    push-nginx-image-with-current-sha:
            docker push "${REGISTRY_URL}/tablatura_proxy:${TAG_NAME}";
    
    post-build-details:
            echo "TAG_NAME was: ${TAG_NAME}"
    devel:
            source config/.env && stack exec -- yesod devel
    
    sass-watch:
            sass --watch css/main.scss static/css/tablatura.css
    
    clean:
            rm -fv tablatura.cabal
            rm -rfv tablatura-tmp
            rm -rfv .stack-work
            rm -rfv yesod-devel

