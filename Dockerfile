FROM perl:5.42.0-slim AS builder
COPY . /work
WORKDIR /work
# Install to a "private" base dir for easy copying without duplication in the final image
RUN perl Makefile.PL INSTALL_BASE=/usr/src/app && make install

FROM perl:5.42.0-slim
ENV PATH=/usr/src/app/bin:$PATH PERL5LIB=/usr/src/app/lib/perl5
USER 65534:65534
COPY --from=builder /usr/src/app /usr/src/app
ENTRYPOINT ["perltidy"]
CMD ["--help"]
