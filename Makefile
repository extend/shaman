# See LICENSE for licensing information.

PROJECT = shaman

# Dependencies.

DEPS = alien cowboy gproc jsx
dep_alien = https://github.com/extend/alien master
dep_cowboy = pkg://cowboy master
dep_gproc = https://github.com/uwiger/gproc.git master
dep_jsx = pkg://jsx master

# Standard targets.

include erlang.mk
