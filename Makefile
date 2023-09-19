install: $(HOME)/.gitconfig \
         $(HOME)/.gitconfig-local \
         $(HOME)/.gitignore \
         $(HOME)/.emacs.d/init.el

$(HOME)/.gitconfig: files/.gitconfig
	cp -f $^ $@

$(HOME)/.gitconfig-local:; touch $(HOME)/.gitconfig-local

$(HOME)/.gitignore: files/.gitignore
	cp -f $^ $@

$(HOME)/.emacs.d/init.el: files/init.el
	mkdir -p $(@D)
	cp $^ $@
