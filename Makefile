docs: docs/Camera.js docs/Basic.js docs/Mask.js docs/Particle.js docs/Stress.js

docs/Camera.js: test/src/Camera.elm
	NAME=Camera make -C test release

docs/Basic.js: test/src/Basic.elm
	NAME=Basic make -C test release

docs/Mask.js: test/src/Mask.elm
	NAME=Mask make -C test release

docs/Particle.js: test/src/Particle.elm
	NAME=Particle make -C test release

docs/Stress.js: test/src/Stress.elm
	NAME=Stress make -C test release

docs/Text.js: test/src/Text.elm
	NAME=Text make -C test release


.PHONY: docs
