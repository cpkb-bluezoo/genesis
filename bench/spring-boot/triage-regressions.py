#!/usr/bin/env python3
"""Bucket genesis vs javac Spring smoke regressions from saved report XML.

JUnit Console Launcher can emit the same test id in more than one report file.
When merging results, a failure must not be overwritten by a later pass entry
(see run_tests() in smoke.py).

Usage:
  triage-regressions.py /path/to/work-with-reports/javac /path/to/work-with-reports/genesis

Each argument is a smoke work directory; reports are read from reports/javac and
reports/genesis beneath it. Pass two work dirs from the same smoke run, or one
dir that contains both report trees.
"""
from __future__ import print_function

import glob
import html
import os
import sys
import xml.etree.ElementTree as ET
from collections import Counter


def _merge_reports(reports_dir):
    outcomes = {}
    failures = {}
    for path in sorted(glob.glob(os.path.join(reports_dir, "**", "TEST-*.xml"), recursive=True)):
        try:
            root = ET.parse(path).getroot()
        except ET.ParseError:
            continue
        for tc in root.iter("testcase"):
            ident = "%s.%s" % (tc.get("classname"), tc.get("name"))
            fail = tc.find("failure") or tc.find("error")
            if fail is not None:
                outcomes[ident] = "failed"
                failures[ident] = fail
            elif tc.find("skipped") is not None:
                if outcomes.get(ident) != "failed":
                    outcomes[ident] = "skipped"
            elif ident not in outcomes:
                outcomes[ident] = "passed"
    return outcomes, failures


def _failure_summary(fail_elem):
    if fail_elem is None:
        return "<no failure element>"
    msg = html.unescape(fail_elem.get("message") or "").strip()
    if not msg:
        body = html.unescape(fail_elem.text or "").strip()
        msg = body.split("\n")[0][:120] if body else ""
    if not msg:
        return "<empty message>"
    return msg.split("\n")[0][:100]


def main():
    if len(sys.argv) < 2:
        print(__doc__, file=sys.stderr)
        return 2
    javac_dir = sys.argv[1]
    genesis_dir = sys.argv[2] if len(sys.argv) > 2 else sys.argv[1]
    javac_reports = os.path.join(javac_dir, "reports", "javac")
    genesis_reports = os.path.join(genesis_dir, "reports", "genesis")
    if not os.path.isdir(javac_reports):
        die("missing %s" % javac_reports)
    if not os.path.isdir(genesis_reports):
        die("missing %s" % genesis_reports)

    javac, _ = _merge_reports(javac_reports)
    genesis, genesis_fail = _merge_reports(genesis_reports)
    baseline = {i for i, o in javac.items() if o == "passed"}
    regressions = sorted(i for i in baseline if genesis.get(i) != "passed")
    print("javac baseline pass: %d" % len(baseline))
    print("genesis pass on baseline: %d" % sum(1 for i in baseline if genesis.get(i) == "passed"))
    print("regressions: %d" % len(regressions))
    on_baseline = sum(1 for i in baseline if genesis.get(i) == "passed")
    goal = 15055
    print("gap_to_goal_%d: %d" % (goal, max(0, goal - on_baseline)))
    buckets = Counter(_failure_summary(genesis_fail.get(i)) for i in regressions)
    print()
    for msg, count in buckets.most_common(25):
        print("%4d  %s" % (count, msg))
    return 0


def die(msg):
    print("error: %s" % msg, file=sys.stderr)
    sys.exit(1)


if __name__ == "__main__":
    sys.exit(main())
