#!/bin/sh

set -xe

skip_tests() {
    # Add a way to run all the tests, even the skipped ones, with an environment
    # variable, so that it can be set for a branch or fork in GitLab.
    if test "$SKIPPED_TESTS" != "force"
    then
        # Skip tests from ci/skipped_tests if this is the right OS version
        # The file
        os_id="$(sh -c '. /etc/os-release; echo "${NAME}-${VERSION_ID}"')"

        echo OS ID: $os_id

        # Skips comments and empty lines
        grep '^[^#]' ci/skipped_tests | while read skipper
        do
            regex="${skipper%%;*}"
            tests="${skipper#*;}"

            echo SKIPPER: "$skipper"
            echo REGEX: "$regex"
            echo TESTS: "$tests"

            # Ignore lines not meant for current $os_id
            if ! echo "$os_id" | grep -q "$regex"; then echo NOPE; continue; fi

            echo SKIPPING $tests

            for test_case in $tests
            do
                test_case_bckup="${test_case}_skipped_test"
                if ! git ls-files "$test_case" | grep -q "$test_case"
                then
                    $MAKE -C "$(dirname "$test_case")" "$(basename "$test_case")" 2>/dev/null || :
                fi
                echo Backing up "$test_case" to "${test_case_bckup}"
                cp "$test_case" "${test_case_bckup}"

                echo 'echo "Test skipped based on ci/skipped_tests file"'> "$test_case"
                echo 'exit 77'>> "$test_case"
                chmod +x "$test_case"
            done
        done
    fi
}

restore_tests() {
    find . -name '*_skipped_test' -print | while read test_case_bckup
    do
        test_case="${test_case_bckup%_skipped_test}"

        echo Moving "${test_case_bckup}" back to "${test_case}"
        rm -f "${test_case}"
        mv -f "${test_case_bckup}" "${test_case}"
    done
}

run_checks() {
    skip_tests
    failed=0
    ${MAKE} "$@" || failed=1
    restore_tests
    return $failed
}

main() {
    MAKE="${MAKE-make -j $(getconf _NPROCESSORS_ONLN)}"

    autoreconf -if

    CONFIG_ARGS="\
--enable-gcc-warnings \
"

    if test skip = "$GNUTLS"
    then
        CONFIG_ARGS="$CONFIG_ARGS --without-gnutls"
    else
        CONFIG_ARGS="$CONFIG_ARGS --with-gnutls"
    fi

    if test skip = "$LIBXML2"
    then
        CONFIG_ARGS="$CONFIG_ARGS --without-libxml2"
    else
        CONFIG_ARGS="$CONFIG_ARGS --with-libxml2"
    fi

    if test -n "$CROSS"
    then
        CONFIG_ARGS="$CONFIG_ARGS
            --disable-fuse
            --disable-ocaml
            --disable-python
            --disable-golang"
    else
        CONFIG_ARGS="$CONFIG_ARGS
            --enable-fuse
            --enable-ocaml
            --enable-python"
        if test "$GOLANG" = "skip"
        then
            CONFIG_ARGS="$CONFIG_ARGS --disable-golang"
        else
            CONFIG_ARGS="$CONFIG_ARGS --enable-golang"
        fi
    fi

    ./configure $CONFIG_ARGS $CONFIGURE_OPTS

    $MAKE

    if test -n "$CROSS" && test "$CROSS" != "i686"
    then
        echo "Possibly run tests with an emulator in the future"
        return 0
    fi

    if test "$(uname)" != "Linux"
    then
        echo "Tests are temporarily skipped on non-Linux platforms"
        return 0
    fi

    run_checks check

    if test "$CHECK_VALGRIND" = "force"
    then
        run_checks check-valgrind
    fi

    if test "$DIST" != "skip"
    then
        $MAKE dist
        $MAKE maintainer-check-extra-dist
    fi

    if test "$DISTCHECK" = "force"
    then
        $MAKE distcheck
    fi
}

main "$@"
