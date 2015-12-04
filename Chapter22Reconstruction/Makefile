haste-dist/recon.hs: src/Main.hs src/Recon.hs src/Recon/Parse.hs src/Recon/Recon.hs src/Recon/Type.hs
	hastec -package-conf .cabal-sandbox/x86_64-osx-haste-0.5.3-ghc-7.10.2-packages.conf.d -hide-package monads-tf --debug --outdir=haste-dist -outputdir haste-dist -o haste-dist/recon.hs -isrc -Wall src/Main.hs

.PHONY: clean
clean:
	$(RM) -r haste-dist src/*.{o,hi,jsmod} src/Recon/*.{o,hi,jsmod}
