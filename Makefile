elpa: *.el
	@version=`grep -o "Version: .*" diff-hl.el | cut -c 11- | tr -d '\r'`; \
	dir=diff-hl-$$version; \
	mkdir -p "$$dir"; \
	cp -r diff-hl*.el diff-hl-$$version; \
	echo "(define-package \"diff-hl\" \"$$version\" \
	\"Highlight uncommitted changes\")" \
	> "$$dir"/diff-hl-pkg.el; \
	tar cvf diff-hl-$$version.tar --mode 644 "$$dir"

clean:
	@rm -rf diff-hl-*/ diff-hl-*.tar *.elc
