.PHONY: check bad good clean all clean-silent annnounce expected

HIGHSCORE_FILE=../.highscore

all:

announce:
	@ if [ x$$VERBOSE != x1 ]; then \
	    echo "HINT: set VERBOSE=1, to show tests inputs and results."; \
          fi

install-test:
	@ for i in bad/* good/*; do		\
	  echo "Install in $$i"; 		\
	  cp Makefile.test $$i/Makefile;	\
	done

build:
	$(MAKE) -C ..

${HIGHSCORE_FILE}:
	echo -n 0 > $@

check:	build announce ${HIGHSCORE_FILE}
	@ for i in bad/* good/*; do					    \
	  $(MAKE) --silent -C $$i check 2> /dev/null || true;    	    \
	done; \
	echo "--------------------------------------------------------"; \
	echo "Global results:"; \
	(cat bad/*/bad-ok-counter 2> /dev/null) > bad-ok-counter || true; \
	(cat bad/*/bad-counter    2> /dev/null) > bad-counter    || true; \
	bad_ct=`wc -l bad-ok-counter | cut -f1 -d' '`; \
	bad_all=`wc -l bad-counter | cut -f1 -d' '`; \
	echo "BAD:   "$$bad_ct		    \
	/ $$bad_all; \
	(cat good/*/good-ok-counter 2> /dev/null) > good-ok-counter \
     || true; \
	(cat good/*/good-counter 2> /dev/null) > good-counter || true; \
	good_ct=`wc -l good-ok-counter | cut -f1 -d' '`; \
	good_all=`wc -l good-counter | cut -f1 -d' '`; \
	echo "GOOD:  "$$good_ct		    \
	/ $$good_all; \
	score=$$((bad_ct+good_ct)); \
	best_score=`cat ${HIGHSCORE_FILE}`; \
	echo; \
	echo -n "ALL:   "$$score / $$((bad_all+good_all)); \
	 if [ $$score -le $$best_score ]; then \
    echo " (best: $$best_score)"; \
	 else \
	echo $$score > ${HIGHSCORE_FILE}; \
	echo; \
	echo "\033[40m\033[1;33m==================== NEW HIGHSCORE =====================\033[0m"; \
	echo "\033[40m\033[1;31m                                                        \033[0m"; \
	echo "\033[40m\033[1;31m     .           ._              .                  .   \033[0m"; \
	echo "\033[40m\033[1;31m  .        -.   /         .                    .        \033[0m"; \
	echo "\033[40m\033[1;31m             \\ /              ._   |   _-               \033[0m"; \
	echo "\033[40m\033[1;31m        .---- O ----.            -_| .'                 \033[0m"; \
	echo "\033[40m\033[1;31m       '    /  \\       .      .--- O ---.    .     .    \033[0m"; \
	echo "\033[40m\033[1;31m           '    \\                 / \\    \\              \033[0m"; \
	echo "\033[40m\033[1;31m     .           \\               /   |                  \033[0m"; \
	echo "\033[40m\033[1;31m                  \\             /                  .    \033[0m"; \
	echo "\033[40m\033[1;31m                   \\           /                        \033[0m"; \
	echo "\033[40m\033[1;31m                                                        \033[0m"; \
	echo "\033[40m\033[1;32m > $$score is the new highscore!                            \033[0m"; \
	 fi; \
	echo "--------------------------------------------------------";


clean:
	@ for i in bad/* good/*; do					    \
	  $(MAKE) --silent -C $$i clean 2> /dev/null || true;    	    \
	done;
	@ rm -r --force *counter *~

expected: build
	@ for i in bad/* good/*; do					    \
	  $(MAKE) --silent -C $$i expected 2> /dev/null || true;    	    \
	done;
