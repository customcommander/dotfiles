install: $(HOME)/.gitconfig \
         $(HOME)/.gitconfig-local \
         $(HOME)/.gitignore

$(HOME)/.gitconfig: git/.gitconfig
	cp $^ $@

$(HOME)/.gitconfig-local:; touch ~/.gitconfig-local

$(HOME)/.gitignore: git/.gitignore
	cp $^ $@
