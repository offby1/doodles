noinst_PROGRAMS = anagrams
anagrams_SOURCES = numeric-bag.h numeric-bag.cc dict.h dict.cc anagrams.cc
anagrams_LDADD = -lgmp

AM_CXXFLAGS =
AM_LDFLAGS  =

if PROFILE
AM_CXXFLAGS += -pg -ftest-coverage -fprofile-arcs -fno-default-inline
AM_LDFLAGS  += -pg
endif PROFILE

TESTS = Ernest.sh

CLEANFILES = gmon.out *.gcov *.ii *.s *.bb *.bbg *.gcda *.gcno
if PROFILE
CLEANFILES +=  *.gcov *.bb *.bbg *.gcda *.gcno *.da
endif PROFILE
