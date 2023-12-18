install: $(HOME)/.gitconfig \
         $(HOME)/.gitconfig-local \
         $(HOME)/.gitignore \
         $(HOME)/.emacs.d/init.el \
         $(HOME)/.yabairc \
         $(HOME)/.skhdrc

$(HOME)/.gitconfig: files/.gitconfig
	cp -f $^ $@

$(HOME)/.gitconfig-local:; touch $(HOME)/.gitconfig-local

$(HOME)/.gitignore: files/.gitignore
	cp -f $^ $@

$(HOME)/.yabairc: files/.yabairc
	cp -f $^ $@
	chmod +x $@

$(HOME)/.skhdrc: files/.skhdrc
	cp -f $^ $@

$(HOME)/.emacs.d/init.el: files/init.el
	mkdir -p $(@D)
	cp $^ $@
