FROM chyanju/medjai:base

RUN git clone https://github.com/Veridise/pip-cairo-lang.git && \
    cd pip-cairo-lang/ && \
    pip install . && \
    cd ..

RUN git clone https://github.com/Veridise/Medjai.git && \
    cd Medjai/ && \
    git checkout demo0

CMD [ "/bin/bash" ]