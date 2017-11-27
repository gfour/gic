# Finds the location of gic binary.

CABAL_GIC="./dist/build/gic/gic"
MAKE_GIC="./gic"

if [ ! -e "${GIC}" ]; then
    if [ -x "$(command -v stack)" ]; then
	GIC=`stack path --system-ghc --local-install-root`/bin/gic
    else
	if [ -e "${CABAL_GIC}" ]; then
	    GIC="${CABAL_GIC}"
	else
	    if [ -e "${MAKE_GIC}" ]; then
		GIC="${MAKE_GIC}"
	    else
		echo "Could not find gic, plase set environment variable GIC (checked: '${GIC}', '${CABAL_GIC}', '${MAKE_GIC}', plus 'stack')."
		exit
	    fi
	fi
    fi
fi
