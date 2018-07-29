buildapp: main.lisp
	sbcl --no-userinit --no-sysinit --non-interactive \
		--load ~/quicklisp/setup.lisp \
		--eval '(ql:quickload "trivial-gamekit")' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")' && \
	buildapp \
		--manifest-file quicklisp-manifest.txt \
		--load-system trivial-gamekit \
		--compress-core \
		--entry main \
		--load main.lisp \
		--output ld42.bin

clean:
	rm -rf ld42.bin quicklisp-manifest.txt build


dist: main.lisp
	sbcl --eval "(ql:quickload '(trivial-gamekit/distribution ld42))" \
	     --eval "(gamekit.distribution:deliver :ld42 'cl-user::example)" \
	     --quit
