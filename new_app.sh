#!/bin/bash
#
function new_app {
	echo "Creating new app: $1"
	CURR=$(pwd)
	ROOT="../$1"
	SKEL="example/foo"
	echo "Creating directory..."
	$(mkdir -p $ROOT/include)
	echo "Creating template files..."
	$(cp -r $SKEL/erlang.mk $SKEL/priv $SKEL/templates $ROOT/.)
	echo "Creating bootstrap files..."
	$(cd $ROOT; make -f erlang.mk bootstrap bootstrap-rel)
	echo "Finishing up..."
	$(sed s/foo/$1/g $SKEL/Makefile > $ROOT/Makefile)
	$(sed s/tuah/$1/g .gitignore > $ROOT/.gitignore)
	$(sed s/foo/$1/g $SKEL/src/foo.app.src > $ROOT/src/${1}.app.src)
	$(sed s/foo/$1/g $SKEL/src/foo_app.erl > $ROOT/src/${1}_app.erl)
	$(sed s/foo/$1/g $SKEL/src/home_controller.erl > $ROOT/src/home_controller.erl)
	$(sed s/foo/$1/g $SKEL/src/auth_controller.erl > $ROOT/src/auth_controller.erl)
	$(sed s/foo/$1/g $SKEL/src/secret_controller.erl > $ROOT/src/secret_controller.erl)
	$(sed s/foo/$1/g $SKEL/include/foo.hrl > $ROOT/include/${1}.hrl)
	$(sed s/foo/$1/g $SKEL/run.sh > $ROOT/run.sh; chmod +x $ROOT/run.sh)
	echo "Done. Your app is created in $ROOT."
	echo "+----------------------+"
	echo "|     Happy coding!    |"
	echo "+----------------------+"
}

if [ "$1" = "" ]; then
	echo "USAGE: $0 appname"
	exit 1
else
	new_app $1
fi


