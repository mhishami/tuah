
PROJECT = tuah
DEPS = cowboy erlydtl jsx lager cowlib ranch bson mongodb

dep_mongodb = git https://github.com/comtihon/mongodb-erlang.git master

include erlang.mk
