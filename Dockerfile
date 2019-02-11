FROM circleci/node:11

RUN sudo apt-get update \
    && sudo apt-get install -y ocaml-nox

CMD ["node"]