
PROJECT = tuah
DEPS = cowboy erlydtl jsx lager cowlib ranch bson mongodb sync poolboy

dep_mongodb = git https://github.com/comtihon/mongodb-erlang.git master

ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
    +warn_shadow_vars +warn_obsolete_guard +warn_missing_spec

include erlang.mk
