install: $(HOME)/.gitconfig \
         $(HOME)/.gitconfig-local \
         $(HOME)/.gitignore \
         $(HOME)/.emacs.d/init.el

$(HOME)/.gitconfig: git/.gitconfig
	cp $^ $@

$(HOME)/.gitconfig-local:; touch ~/.gitconfig-local

$(HOME)/.gitignore: git/.gitignore
	cp $^ $@

$(HOME)/.emacs.d/init.el: emacs/init.el
	mkdir -p $(@D)
	cp $^ $@
