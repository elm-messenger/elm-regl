docs:
	NAME=Basic make -C test release
	NAME=Mask make -C test release
	NAME=Particle make -C test release
	NAME=Stress make -C test release

.PHONY: docs
