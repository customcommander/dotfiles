install: $(HOME)/.gitconfig \
         $(HOME)/.gitignore

$(HOME)/.gitconfig: git/.gitconfig
	cp $^ $@

$(HOME)/.gitignore: git/.gitignore
	cp $^ $@
