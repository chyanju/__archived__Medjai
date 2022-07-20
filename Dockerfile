FROM chyanju/papyrus:base

RUN git clone https://github.com/chyanju/pip-cairo-lang.git && \
    cd pip-cairo-lang/ && \
    pip install . && \
    cd ..

RUN git clone https://github.com/chyanju/Papyrus.git && \
    cd Papyrus/ && \
    git checkout jacob/erc20test

CMD [ "/bin/bash" ]