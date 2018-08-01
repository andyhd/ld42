buildapp: package.lisp main.lisp
	sbcl --no-userinit --no-sysinit --non-interactive \
		--load ~/quicklisp/setup.lisp \
		--eval '(ql:quickload :trivial-gamekit)' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")' && \
	buildapp \
		--manifest-file quicklisp-manifest.txt \
		--load-system trivial-gamekit \
		--load-system ld42 \
		--compress-core \
		--eval "(defun main (args) (declare (ignore args)) (gamekit:start 'ld42::ld42))" \
		--entry main \
		--output ld42.bin

clean:
	rm -rf ld42.bin quicklisp-manifest.txt build

dist: main.lisp
	sbcl --eval "(ql:quickload '(trivial-gamekit/distribution ld42))" \
		 --eval "(gamekit.distribution:deliver :ld42 'ld42::ld42)" \
		 --quit

run: main.lisp
	LD42_ROOT=${PWD} \
	sbcl --eval '(ql:quickload :trivial-gamekit)' \
		 --load main.lisp \
		 --eval '(main "")'

swank:
	LD42_ROOT=${PWD} \
	sbcl --eval '(ql:quickload :trivial-gamekit)' \
	     --load ${HOME}/.vim/bundle/slimv/slime/start-swank.lisp
