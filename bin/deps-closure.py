#!/usr/bin/env python3

import fileinput
import re
import subprocess
import sys


PKG_RE = \
  re.compile(
    r'([a-zA-Z]+[0-9]*(?:-[a-zA-Z]+[0-9]*)*)' # naked package name
    r'(?:-(?:[0-9](?:\.[0-9])+))?')           # possible package version

PKG_DEPS_RE = \
  re.compile(
    r'([a-zA-Z]+[0-9]*(?:-[a-zA-Z]+[0-9]*)*)'
    r'-[0-9\.]+'
    r'-[a-z0-9]{10,}')

def get_deps(pkg_name):
  deps_out = subprocess.check_output(
    "cabal sandbox hc-pkg field {} depends".format(pkg_name),
    shell=True)
  pkgs = PKG_DEPS_RE.findall(deps_out.decode('UTF-8'))
  return set(pkgs)


packages = set()

for line in fileinput.input():
  match = PKG_RE.match(line.strip())
  if match:
    pkg_name = match.group(1)
    packages.add(pkg_name)

# print("* to process: {}".format(sorted(packages)))

visited = set()
while packages:
  pkg_name = packages.pop()
  if pkg_name in visited: continue

  # add this package to visited list so that it won't be processed again
  visited.add(pkg_name)

  # get its immediate deps
  this_deps = get_deps(pkg_name)
  # print("* immediate deps of {}: {}".format(pkg_name, sorted(this_deps)))

  # remove any visited ones from its immediate deps
  this_deps.difference_update(visited)
  this_deps.difference_update(packages)
  # print("* adding to the set: {}".format(sorted(this_deps)))

  # add remaining, unvisited pkgs to packages
  packages.update(this_deps)


for pkg_name in sorted(visited):
  print(pkg_name)


