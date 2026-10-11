#!/usr/bin/env python3
#
# smoke.py - compile Spring Boot with genesis and javac, and run its unit tests
#
# Copyright (C) 2026 Chris Burdess <dog@gnu.org>
#
# This file is part of genesis; see COPYING for the license terms.
#
# Takes a release of Spring Boot from a git checkout (never modifying it),
# works out the projects, their dependencies and the jars they need (see
# model.py and maven.py: no Gradle, no Maven), compiles the main, testFixtures
# and test source sets of core/, module/ and test-support/ with each compiler,
# one project at a time in dependency order with the project's own classpath,
# and runs each project's tests with the JUnit console launcher.
#
# javac is the baseline: a project javac cannot compile, and a test javac's
# classes fail, are not held against genesis. The smoke fails when genesis
# cannot compile a project javac can, or when a test passes on javac's classes
# and fails on genesis's.
#
# Environment:
#   SB_DIR          git checkout of spring-boot (default: ../spring-boot)
#   SB_REF          tag or commit to build (default: v4.1.1)
#   SB_CACHE        jar and pom cache (default: ~/.cache/genesis-bench/maven)
#   SB_PROJECTS     regular expression: only the projects whose path matches
#   SB_COMPILERS    comma separated: javac, genesis (default: both)
#   SB_JOBS         projects handled at once (default: number of processors)
#   SB_TEST_TIMEOUT seconds a project's tests may run (default: 900)
#   SB_NO_TESTS     if set, compile only
#   SB_KEEP         if set, keep the work directory
#   GENESIS         genesis binary (default: ../src/genesis from this directory)
#   JAVA_HOME       JDK 25 that ships jmods (required)

import concurrent.futures
import datetime
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
import time
import xml.etree.ElementTree as ET

import maven
import model

HERE = os.path.dirname(os.path.abspath(__file__))
TOP = os.path.abspath(os.path.join(HERE, "..", ".."))

SB_DIR = os.path.abspath(os.environ.get("SB_DIR", os.path.join(TOP, "..", "spring-boot")))
SB_REF = os.environ.get("SB_REF", "v4.1.1")
SB_VERSION = SB_REF[1:] if SB_REF.startswith("v") else SB_REF
JOBS = int(os.environ.get("SB_JOBS", os.cpu_count() or 2))
TEST_TIMEOUT = int(os.environ.get("SB_TEST_TIMEOUT", "900"))
COMPILERS = os.environ.get("SB_COMPILERS", "javac,genesis").split(",")
PROJECTS_RE = re.compile(os.environ["SB_PROJECTS"]) if os.environ.get("SB_PROJECTS") else None
RELEASE = "17"          # Spring Boot's own --release

JAVA_HOME = os.environ.get("JAVA_HOME", "")
GENESIS = os.environ.get("GENESIS", os.path.join(TOP, "src", "genesis"))
JAVAC = os.path.join(JAVA_HOME, "bin", "javac")
JAVA = os.path.join(JAVA_HOME, "bin", "java")
LOCK = os.path.join(HERE, "%s.lock" % SB_VERSION)


def log(msg=""):
    print(msg, flush=True)


def die(msg):
    print("error: %s" % msg, file=sys.stderr)
    sys.exit(1)


def run(cmd, log_path=None, timeout=None, cwd=None):
    """Run a command in its own process group; (exit status, seconds)."""
    start = time.time()
    out = open(log_path, "wb") if log_path else subprocess.DEVNULL
    proc = subprocess.Popen(cmd, stdout=out, stderr=subprocess.STDOUT, cwd=cwd,
                            start_new_session=True)
    try:
        status = proc.wait(timeout=timeout)
    except subprocess.TimeoutExpired:
        os.killpg(proc.pid, signal.SIGKILL)
        proc.wait()
        status = -9
        if log_path:
            out.write(b"\n[smoke] killed after %d seconds\n" % timeout)
    if log_path:
        out.close()
    return status, time.time() - start


# ----------------------------------------------------------------------------
# Setup

def extract_source(work):
    src = os.path.join(work, "src")
    os.makedirs(src)
    archive = subprocess.Popen(["git", "-C", SB_DIR, "archive", SB_REF], stdout=subprocess.PIPE)
    tar = subprocess.run(["tar", "-x", "-C", src], stdin=archive.stdout)
    archive.stdout.close()
    if archive.wait() != 0 or tar.returncode != 0:
        die("cannot extract %s from %s" % (SB_REF, SB_DIR))
    return src


def jar_path(art):
    return os.path.join(maven.CACHE, art.path())


def read_lock():
    lock = {}
    if os.path.exists(LOCK):
        with open(LOCK) as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith("#"):
                    sha, coord = line.split(None, 1)
                    lock[coord] = sha
    return lock


def fetch_jars(artifacts):
    """Download every jar and check it against the lock file (written if absent)."""
    lock = read_lock()
    fresh = not lock
    arts = sorted({a.coord: a for a in artifacts if a.has_jar}.values(), key=lambda a: a.coord)

    def get(a):
        path = maven.fetch(a.path())
        if not path:
            raise RuntimeError("no jar for %s" % a.coord)
        return a.coord, maven.sha256(path)

    got = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
        for coord, digest in pool.map(get, arts):
            got[coord] = digest
    bad = [c for c, d in got.items() if lock and c in lock and lock[c] != d]
    if bad:
        die("checksum mismatch against %s: %s" % (LOCK, ", ".join(bad[:5])))
    if fresh or any(c not in lock for c in got):
        with open(LOCK, "w") as f:
            f.write("# sha256 of every jar the Spring Boot %s smoke uses\n" % SB_VERSION)
            for c in sorted(got):
                f.write("%s  %s\n" % (got[c], c))
        log("Wrote %s (%d jars)" % (LOCK, len(got)))
    return arts


# ----------------------------------------------------------------------------
# Compiling

class Out:
    """Where the output of a compiler for a project's source sets lives."""

    def __init__(self, work, compiler):
        self.root = os.path.join(work, "out", compiler)

    def dir(self, project, sset):
        return os.path.join(self.root, project.name, sset)


class Compiler:
    def __init__(self, name, work):
        self.name = name
        self.exe = JAVAC if name == "javac" else GENESIS
        self.out = Out(work, name)
        self.work = work
        self.version = model.gradle_properties(os.path.join(work, "src")).get("version", SB_VERSION)
        self.results = {}       # (project path, source set) -> dict
        self.lock = threading.Lock()

    def compile(self, project, sset, projs, fixtures, arts):
        """Compile project/src/<sset>/java; the result dict is recorded."""
        key = (project.path, sset)
        sources = []
        # The configuration modules also compile a shaded copy of org.json, from src/json-shade.
        dirs = [os.path.join(project.dir, "src", sset, "java")]
        if sset == "main":
            dirs.append(os.path.join(project.dir, "src", "json-shade", "java"))
        for d in dirs:
            for root, _dirs, files in os.walk(d):
                sources += [os.path.join(root, f) for f in sorted(files) if f.endswith(".java")]
        # Java templates (src/<set>/javaTemplates) are expanded with the project version,
        # as Spring Boot's own build does into build/generated-sources.
        templates = os.path.join(project.dir, "src", sset, "javaTemplates")
        for root, _dirs, files in os.walk(templates):
            for f in sorted(files):
                if not f.endswith(".java"):
                    continue
                rel = os.path.relpath(os.path.join(root, f), templates)
                dest = os.path.join(self.work, "generated", self.name, project.name, sset, rel)
                os.makedirs(os.path.dirname(dest), exist_ok=True)
                with open(os.path.join(root, f), encoding="utf-8") as fin:
                    text = fin.read().replace("${springBootVersion}", self.version)
                with open(dest, "w", encoding="utf-8") as fout:
                    fout.write(text)
                sources.append(dest)
        if not sources:
            return self._record(key, "none")
        # A dependency that did not compile leaves nothing to compile against.
        for p in projs:
            r = self.results.get((p.path, "main"))
            if r and r["status"] not in ("ok", "none"):
                return self._record(key, "skipped", "needs %s" % p.path)
        for p in fixtures:
            r = self.results.get((p.path, "testFixtures"))
            if r and r["status"] not in ("ok", "none"):
                return self._record(key, "skipped", "needs testFixtures of %s" % p.path)

        out = self.out.dir(project, sset)
        shutil.rmtree(out, ignore_errors=True)
        os.makedirs(out)
        cp = []
        for p in projs:
            d = self.out.dir(p, "main")
            if os.path.isdir(d) and not (p is project and sset == "main"):
                cp.append(d)
        cp += [self.out.dir(p, "testFixtures") for p in fixtures
               if os.path.isdir(self.out.dir(p, "testFixtures"))]
        cp += [jar_path(a) for a in arts if a.has_jar]
        argfile = os.path.join(self.work, "args", self.name, "%s.%s.args" % (project.name, sset))
        os.makedirs(os.path.dirname(argfile), exist_ok=True)
        with open(argfile, "w") as f:
            flags = ["-d", out] + (["-cp", os.pathsep.join(cp)] if cp else [])
            for a in flags + ["--release", RELEASE, "-parameters", "-proc:none", "-g", "-nowarn",
                              "-encoding", "UTF-8"] + sources:
                f.write(a + "\n")
        logfile = os.path.join(self.work, "logs", self.name, "%s.%s.compile.log" % (project.name, sset))
        os.makedirs(os.path.dirname(logfile), exist_ok=True)
        status, secs = run([self.exe, "@" + argfile], logfile)
        if status != 0:
            return self._record(key, "failed", "see %s" % logfile, secs, logfile)
        res = os.path.join(project.dir, "src", sset, "resources")
        if os.path.isdir(res):
            shutil.copytree(res, out, dirs_exist_ok=True)
        return self._record(key, "ok", "", secs, logfile)

    def _record(self, key, status, note="", secs=0.0, logfile=None):
        r = {"status": status, "note": note, "seconds": secs, "log": logfile}
        with self.lock:
            self.results[key] = r
        return r


def schedule(build, comp, paths):
    """Compile the main source sets in dependency order, independent projects together."""
    order = build.compile_order(paths)
    done = set()
    pending = list(order)
    with concurrent.futures.ThreadPoolExecutor(max_workers=JOBS) as pool:
        while pending:
            ready = []
            for path in pending:
                deps = [d.path for d in build._projects_of(build.projects[path],
                                                          model.MAIN_COMPILE + ("runtimeOnly",))
                        if d.path in set(order)]
                if all(d in done for d in deps):
                    ready.append(path)
            if not ready:
                ready = pending[:1]     # a cycle (the build forbids them): just go on
            futures = []
            for path in ready:
                p = build.projects[path]
                projs, arts = build.main_compile(p)
                futures.append(pool.submit(comp.compile, p, "main", projs, [], arts))
            for fu in futures:
                fu.result()
            for path in ready:
                pending.remove(path)
                done.add(path)


def compile_tests(build, comp, needed, selected):
    """testFixtures of every needed project (a fixture after those it needs), then the
    tests of the selected projects."""
    fixture_paths = [p for p in needed if build.projects[p].has_source("testFixtures")]
    plans = {}
    for path in fixture_paths:
        projs, arts, fixtures = build.fixtures_compile(build.projects[path])
        plans[path] = (projs, fixtures, arts)
    pending = list(fixture_paths)
    done = set()
    with concurrent.futures.ThreadPoolExecutor(max_workers=JOBS) as pool:
        while pending:
            ready = [x for x in pending
                     if all(f.path in done or f.path not in plans or f.path == x
                            for f in plans[x][1])]
            if not ready:
                ready = pending[:1]
            futs = [pool.submit(comp.compile, build.projects[x], "testFixtures", *plans[x])
                    for x in ready]
            for fu in futs:
                fu.result()
            for x in ready:
                pending.remove(x)
                done.add(x)
        futs = []
        for path in selected:
            p = build.projects[path]
            if p.has_source("test"):
                projs, arts, fixtures = build.test_compile(p)
                futs.append(pool.submit(comp.compile, p, "test", projs, fixtures, arts))
        for fu in futs:
            fu.result()


# ----------------------------------------------------------------------------
# Testing

def run_tests(build, comp, p, console):
    """Run a project's tests; {test id: 'passed' | 'failed' | 'skipped'} or an error string."""
    r = comp.results.get((p.path, "test"))
    if not r or r["status"] != "ok":
        return None
    projs, arts, fixtures = build.test_runtime(p)
    cp = [comp.out.dir(p, "test")]
    cp += [comp.out.dir(f, "testFixtures") for f in fixtures
           if os.path.isdir(comp.out.dir(f, "testFixtures"))]
    cp += [comp.out.dir(x, "main") for x in projs if os.path.isdir(comp.out.dir(x, "main"))]
    cp += [jar_path(a) for a in arts if a.has_jar]
    cp += [jar_path(a) for a in console if a.has_jar]
    reports = os.path.join(comp.work, "reports", comp.name, p.name)
    shutil.rmtree(reports, ignore_errors=True)
    os.makedirs(reports)
    logfile = os.path.join(comp.work, "logs", comp.name, "%s.test.log" % p.name)
    cmd = [JAVA, "-Xmx1536m", "-Djava.awt.headless=true", "-cp", os.pathsep.join(cp),
           "org.junit.platform.console.ConsoleLauncher", "execute", "--disable-banner",
           "--disable-ansi-colors", "--details=none", "--reports-dir", reports,
           "--scan-classpath", comp.out.dir(p, "test")]
    status, secs = run(cmd, logfile, timeout=TEST_TIMEOUT, cwd=os.path.join(comp.work, "run"))
    results = {}
    for name in sorted(os.listdir(reports)):
        if not name.endswith(".xml"):
            continue
        try:
            root = ET.parse(os.path.join(reports, name)).getroot()
        except ET.ParseError:
            continue
        for tc in root.iter("testcase"):
            ident = "%s.%s" % (tc.get("classname"), tc.get("name"))
            if tc.find("failure") is not None or tc.find("error") is not None:
                results[ident] = "failed"
            elif tc.find("skipped") is not None:
                results[ident] = "skipped"
            else:
                results[ident] = "passed"
    if not results and status != 0:
        return "no results (exit %d, see %s)" % (status, logfile)
    return results


# ----------------------------------------------------------------------------

def main():
    if not JAVA_HOME or not os.path.exists(JAVAC):
        die("JAVA_HOME must name a JDK 25 (it needs javac and, for genesis, jmods)")
    if not os.path.isdir(os.path.join(JAVA_HOME, "jmods")) and "genesis" in COMPILERS:
        die("%s has no jmods directory, which genesis needs (see the README)" % JAVA_HOME)
    if "genesis" in COMPILERS and not os.access(GENESIS, os.X_OK):
        die("genesis not found at %s (build it or set GENESIS)" % GENESIS)
    if not os.path.isdir(os.path.join(SB_DIR, ".git")):
        die("%s is not a git checkout of spring-boot (set SB_DIR)" % SB_DIR)

    work = tempfile.mkdtemp(prefix="genesis-spring-boot.")
    os.makedirs(os.path.join(work, "run"))
    try:
        return smoke(work)
    finally:
        if os.environ.get("SB_KEEP"):
            log("Keeping work directory: %s" % work)
        else:
            shutil.rmtree(work, ignore_errors=True)


def smoke(work):
    log("=== Spring Boot smoke: %s ===" % SB_REF)
    log("Work directory: %s" % work)
    log("Compilers:      %s" % ", ".join(COMPILERS))
    src = extract_source(work)
    build = model.Build(src, SB_VERSION)

    selected = [p.path for p in build.projects.values()
                if not p.path.startswith(":starter:") and p.has_source("main")
                and (PROJECTS_RE is None or PROJECTS_RE.search(p.path))]
    selected = build.compile_order(selected)

    # A project whose dependencies cannot be resolved (from Central and the
    # repositories above) is left out, with whatever needs it.
    unresolved = {}
    for path in list(selected):
        try:
            p = build.projects[path]
            build.main_compile(p)
            build.test_compile(p)
            build.test_runtime(p)
        except RuntimeError as e:
            unresolved[path] = str(e)
    if unresolved:
        gone = set(unresolved)
        changed = True
        while changed:
            changed = False
            for path in selected:
                if path in gone:
                    continue
                p = build.projects[path]
                needs = [d.path for d in build._projects_of(p, model.MAIN_COMPILE + ("runtimeOnly",))]
                if any(n in gone for n in needs):
                    gone.add(path)
                    unresolved[path] = "needs an unresolvable project"
                    changed = True
        selected = [x for x in selected if x not in gone]
        log("Left out %d project(s) with unresolvable dependencies:" % len(gone))
        for path in sorted(gone)[:20]:
            log("  %s: %s" % (path, unresolved[path]))

    # The selected projects, and every project they need to compile and run.
    needed = set(selected)
    arts = []
    todo = list(selected)
    while todo:
        p = build.projects[todo.pop()]
        projs, a = build.main_compile(p)
        arts += a
        found = list(projs)
        if p.path in selected:
            for fn in (build.test_compile, build.test_runtime):
                ps, a, fx = fn(p)
                arts += a
                found += list(ps) + list(fx)
            arts += build.test_runtime(p)[1]
        for q in found:
            if q.path not in needed and q.has_source("main"):
                needed.add(q.path)
                todo.append(q.path)
            elif q.path not in needed and not q.path.startswith(":starter:"):
                needed.add(q.path)
    paths = build.compile_order([x for x in needed if build.projects[x].has_source("main")])
    log("Projects:       %d selected, %d to compile (of %d)" % (len(selected), len(paths), len(build.projects)))

    # Every jar any of them needs.
    for path in paths:
        p = build.projects[path]
        arts += build.main_compile(p)[1]
    managed = build.managed
    console = maven.resolve([maven.Dep("org.junit.platform", "junit-platform-console")], managed, True)
    arts += console
    arts = fetch_jars(arts)
    log("Jars:           %d" % len(arts))
    log()

    comps = {}
    for name in COMPILERS:
        comp = Compiler(name, work)
        comps[name] = comp
        start = time.time()
        schedule(build, comp, paths)
        main_ok = sum(1 for p in paths if comp.results[(p, "main")]["status"] == "ok")
        log("%-8s main compiled: %d of %d projects (%.0fs)" % (name, main_ok, len(paths), time.time() - start))
        start = time.time()
        compile_tests(build, comp, paths, selected)
        t_ok = sum(1 for p in selected if comp.results.get((p, "test"), {}).get("status") == "ok")
        t_all = sum(1 for p in selected if build.projects[p].has_source("test"))
        log("%-8s tests compiled: %d of %d projects (%.0fs)" % (name, t_ok, t_all, time.time() - start))

    tests = {}
    if not os.environ.get("SB_NO_TESTS"):
        for name, comp in comps.items():
            tests[name] = {}
            start = time.time()
            with concurrent.futures.ThreadPoolExecutor(max_workers=max(1, JOBS // 2)) as pool:
                futs = {path: pool.submit(run_tests, build, comp, build.projects[path], console)
                        for path in selected}
                for path, fu in futs.items():
                    tests[name][path] = fu.result()
            total = sum(len(v) for v in tests[name].values() if isinstance(v, dict))
            log("%-8s tests run: %d (%.0fs)" % (name, total, time.time() - start))

    return report(build, paths, selected, comps, tests)


def _error_lines(logfile):
    try:
        with open(logfile, errors="replace") as f:
            return [line.rstrip() for line in f if "error" in line]
    except OSError:
        return []


def count_errors(logfile):
    return len(_error_lines(logfile))


def first_errors(logfile, n):
    """The first n error lines of a compile log, without the work directory prefix."""
    out = []
    for line in _error_lines(logfile)[:n]:
        out.append(re.sub(r"^.*?/src/", "", line)[:200])
    return out


def report(build, paths, selected, comps, tests):
    log()
    log("=== Results ===")
    base = comps.get("javac")
    gen = comps.get("genesis")
    problems = []
    if base and gen:
        for path in paths:
            for sset in ("main", "testFixtures", "test"):
                b = base.results.get((path, sset))
                g = gen.results.get((path, sset))
                if b and g and b["status"] == "ok" and g["status"] != "ok":
                    if g["status"] == "failed":
                        problems.append("%s %s: genesis failed (%d errors)" %
                                        (path, sset, count_errors(g["log"])))
                        problems += ["      " + line for line in first_errors(g["log"], 3)]
                    else:
                        problems.append("%s %s: genesis %s: %s" % (path, sset, g["status"], g["note"]))
        for path in selected:
            tb, tg = tests.get("javac", {}).get(path), tests.get("genesis", {}).get(path)
            if isinstance(tb, dict) and tg is None:
                continue
            if isinstance(tb, dict) and isinstance(tg, str):
                problems.append("%s tests: genesis %s" % (path, tg))
            if isinstance(tb, dict) and isinstance(tg, dict):
                for ident, outcome in sorted(tb.items()):
                    if outcome == "passed" and tg.get(ident) != "passed":
                        problems.append("%s: %s passes on javac, %s on genesis" %
                                        (path, ident, tg.get(ident, "is missing")))
    log("Baseline (javac) failures are not counted against genesis.")
    for name, comp in comps.items():
        counts = {}
        for (_path, _sset), r in comp.results.items():
            counts[r["status"]] = counts.get(r["status"], 0) + 1
        log("%-8s source sets: %s" % (name, ", ".join("%s %d" % kv for kv in sorted(counts.items()))))
    for name, per in tests.items():
        passed = sum(1 for v in per.values() if isinstance(v, dict) for o in v.values() if o == "passed")
        failed = sum(1 for v in per.values() if isinstance(v, dict) for o in v.values() if o == "failed")
        log("%-8s tests: %d passed, %d failed" % (name, passed, failed))
    log()
    if problems:
        log("%d problem(s) on genesis that javac does not have:" % len(problems))
        for line in problems[:200]:
            log("  " + line)
        if len(problems) > 200:
            log("  ... and %d more" % (len(problems) - 200))
        log()
        log("SMOKE FAILED")
        return 1
    log("SMOKE PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
