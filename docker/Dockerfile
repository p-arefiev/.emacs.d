FROM ubuntu:18.04

# Emacs
RUN apt-get update && apt-get install -y git \
software-properties-common \
&& apt-add-repository ppa:kelleyk/emacs \
&& apt-get update && apt-get install -y emacs26

# Clone emacs repo config
RUN ["git clone", "https://gitlab.com/Mrspook/emacs.d",".emacs.d"]

CMD ["git pull"]
CMD /usr/bin/emacs
