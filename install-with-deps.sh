#!/bin/bash
git clone --depth=1 git://github.com/ivanperez-keera/gloss-gtk.git
git clone --depth=1 git://github.com/keera-studios/gtk-helpers.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-environment-gtk.git
git clone --depth=1 git://github.com/keera-studios/MissingK.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-model-protectedmodels.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-view-gtk.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-view.git
git clone --depth=1 git://github.com/keera-studios/hails-reactive-gtk.git
git clone --depth=1 git://github.com/keera-studios/hails-reactivevalues.git
git clone --depth=1 git://github.com/ivanperez-keera/keera-callbacks.git
git clone --depth=1 git://github.com/keera-studios/keera-hails.git
git clone --depth=1 git://github.com/ivanperez-keera/SoOSiM-ui.git
git clone --depth=1 git://github.com/ivanperez-keera/SoOSiM-components.git
git clone --depth=1 git://github.com/christiaanb/SoOSiM.git

export PATH=$PATH:$PWD/cabal-dev/bin
cabal-dev install gtk2hs-buildtools
cabal-dev add-source gloss-gtk
cabal-dev add-source gtk-helpers
cabal-dev add-source MissingK
cabal-dev add-source keera-callbacks
cabal-dev add-source hails-reactivevalues
cabal-dev add-source hails-reactive-gtk
cabal-dev add-source hails-mvc-view
cabal-dev add-source hails-mvc-view-gtk
cabal-dev add-source hails-mvc-model-protectedmodels
cabal-dev add-source hails-mvc-environment-gtk
cabal-dev add-source keera-hails
cabal-dev install keera-hails
cabal-dev add-source SoOSiM
cabal-dev add-source SoOSiM-components
cabal-dev add-source SoOSiM-ui
cabal-dev install SoOSiM-ui
