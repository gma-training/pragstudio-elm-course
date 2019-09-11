FROM node:7.2.0

LABEL maintainer="Graham Ashton <graham@effectif.com>" version="1.0"

RUN npm install -g \
    elm-test@0.18.0 \
    elm@0.18.0

EXPOSE 8000

WORKDIR /home/node/app
RUN chown node: /home/node/app

USER node:node

COPY elm-package.json .
RUN elm package install -y

CMD ["/bin/bash"]
