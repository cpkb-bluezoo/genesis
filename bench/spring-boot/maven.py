#!/usr/bin/env python3
#
# maven.py - the little of Maven that the Spring Boot smoke needs
#
# Copyright (C) 2026 Chris Burdess <dog@gnu.org>
#
# This file is part of genesis; see COPYING for the license terms.
#
# Fetches poms and jars from Maven Central into a local cache, builds
# effective poms (parent chain, properties, imported BOMs) and resolves the
# transitive dependencies of an artifact. It is not a Maven: no profiles, no
# version ranges, nearest-wins mediation, one repository. That is enough for
# the artifacts a Spring Boot build depends on, and it runs without a build
# tool.
#
# Environment:
#   SB_REPOS   comma separated repository roots (default: Maven Central, then
#              the Shibboleth repository for OpenSAML)
#   SB_CACHE   local cache directory, laid out like a Maven repository
#              (default: ~/.cache/genesis-bench/maven)

import hashlib
import os
import re
import sys
import time
import urllib.error
import urllib.request
import xml.etree.ElementTree as ET

# Maven Central, then the repositories Spring Boot's own build adds for the few
# artifacts that Central does not have (OpenSAML).
REPOS = [r.rstrip("/") for r in os.environ.get(
    "SB_REPOS", "https://repo.maven.apache.org/maven2,https://build.shibboleth.net/maven/releases").split(",")]
CACHE = os.environ.get("SB_CACHE", os.path.expanduser("~/.cache/genesis-bench/maven"))


def repo_path(group, artifact, version, classifier=None, ext="jar"):
    name = "%s-%s%s.%s" % (artifact, version, "-" + classifier if classifier else "", ext)
    return "%s/%s/%s/%s" % (group.replace(".", "/"), artifact, version, name)


def fetch(path):
    """The local file for a repository path, downloading it if needed.
    None if no repository has it."""
    local = os.path.join(CACHE, path)
    missing = local + ".missing"
    if os.path.exists(local) and os.path.getsize(local) > 0:
        return local
    if os.path.exists(missing):
        return None
    os.makedirs(os.path.dirname(local), exist_ok=True)
    for repo in REPOS:
        url = repo + "/" + path
        last = None
        for attempt in range(4):
            try:
                req = urllib.request.Request(url, headers={"User-Agent": "genesis-bench"})
                with urllib.request.urlopen(req, timeout=60) as resp:
                    data = resp.read()
                tmp = "%s.tmp.%d" % (local, os.getpid())
                with open(tmp, "wb") as f:
                    f.write(data)
                os.replace(tmp, local)
                return local
            except urllib.error.HTTPError as e:
                if e.code == 404:
                    last = None
                    break
                last = e
            except (urllib.error.URLError, OSError) as e:
                last = e
            time.sleep(1 + attempt * 2)
        if last is not None:
            raise RuntimeError("cannot fetch %s: %s" % (url, last))
    # Remember the absence, so a missing optional file is asked for once.
    open(missing, "wb").close()
    return None


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def version_key(v):
    """Sort key for a version string: numbers numerically, words below numbers."""
    parts = re.split(r"[.\-_+]", v)
    key = []
    for p in parts:
        key.append((1, int(p), "") if p.isdigit() else (0, 0, p.lower()))
    return key


# ----------------------------------------------------------------------------
# Poms

class Dep:
    __slots__ = ("group", "artifact", "version", "scope", "optional", "type",
                 "classifier", "exclusions")

    def __init__(self, group, artifact, version=None, scope="compile", optional=False,
                 type="jar", classifier=None, exclusions=()):
        self.group = group
        self.artifact = artifact
        self.version = version
        self.scope = scope
        self.optional = optional
        self.type = type
        self.classifier = classifier
        self.exclusions = tuple(exclusions)

    @property
    def key(self):
        return (self.group, self.artifact)


def _strip(root):
    for el in root.iter():
        if isinstance(el.tag, str) and "}" in el.tag:
            el.tag = el.tag.split("}", 1)[1]
    return root


def _text(el, name, default=None):
    c = el.find(name)
    return c.text.strip() if c is not None and c.text and c.text.strip() else default


class Pom:
    """An effective pom: what Maven would compute from the parent chain and imports."""

    _cache = {}

    def __init__(self, group, artifact, version):
        self.group = group
        self.artifact = artifact
        self.version = version
        self.packaging = "jar"
        self.props = {}
        self.managed = {}      # (group, artifact, type, classifier) -> Dep
        self.deps = []
        self.exists = False

    @classmethod
    def get(cls, group, artifact, version):
        key = (group, artifact, version)
        if key not in cls._cache:
            cls._cache[key] = None   # a cycle in the imports would otherwise recurse
            cls._cache[key] = cls._load(group, artifact, version)
        return cls._cache[key]

    @classmethod
    def _load(cls, group, artifact, version):
        pom = cls(group, artifact, version)
        path = fetch(repo_path(group, artifact, version, ext="pom"))
        if not path:
            return pom
        pom.exists = True
        root = _strip(ET.parse(path).getroot())

        parent = None
        p = root.find("parent")
        if p is not None:
            parent = cls.get(_text(p, "groupId"), _text(p, "artifactId"), _text(p, "version"))
        pom.group = _text(root, "groupId") or (parent.group if parent else group)
        pom.version = _text(root, "version") or (parent.version if parent else version)
        pom.packaging = _text(root, "packaging", "jar")
        if parent:
            pom.props.update(parent.props)
            pom.managed.update(parent.managed)
            pom.deps.extend(parent.deps)
        props = root.find("properties")
        if props is not None:
            for el in props:
                if isinstance(el.tag, str):
                    pom.props[el.tag] = (el.text or "").strip()
        for name, value in (("project.groupId", pom.group), ("project.artifactId", artifact),
                            ("project.version", pom.version), ("pom.version", pom.version),
                            ("version", pom.version), ("groupId", pom.group)):
            pom.props[name] = value
        if parent:
            pom.props["project.parent.version"] = parent.version
            pom.props["parent.version"] = parent.version
            pom.props["project.parent.groupId"] = parent.group

        def sub(s):
            if s is None:
                return None
            for _ in range(10):
                n = re.sub(r"\$\{([^}]+)\}", lambda m: pom.props.get(m.group(1), m.group(0)), s)
                if n == s:
                    break
                s = n
            return s

        def read_dep(d):
            excl = []
            ex = d.find("exclusions")
            if ex is not None:
                for e in ex.findall("exclusion"):
                    excl.append((sub(_text(e, "groupId", "*")), sub(_text(e, "artifactId", "*"))))
            return Dep(sub(_text(d, "groupId")), sub(_text(d, "artifactId")),
                       sub(_text(d, "version")), sub(_text(d, "scope")),
                       sub(_text(d, "optional", "false")) == "true",
                       sub(_text(d, "type", "jar")), sub(_text(d, "classifier")), excl)

        own = {}
        dm = root.find("dependencyManagement/dependencies")
        if dm is not None:
            for d in dm.findall("dependency"):
                dep = read_dep(d)
                if dep.scope == "import" and dep.type == "pom":
                    imp = cls.get(dep.group, dep.artifact, dep.version)
                    for k, v in imp.managed.items():
                        own.setdefault(k, v)
                else:
                    own[(dep.group, dep.artifact, dep.type, dep.classifier)] = dep
        pom.managed.update(own)

        deps = root.find("dependencies")
        if deps is not None:
            for d in deps.findall("dependency"):
                dep = read_dep(d)
                m = pom.managed_for(dep)
                if m:
                    if not dep.version:
                        dep.version = m.version
                    if not dep.scope:
                        dep.scope = m.scope
                    if not dep.exclusions:
                        dep.exclusions = m.exclusions
                dep.scope = dep.scope or "compile"
                pom.deps.append(dep)
        return pom

    def managed_for(self, dep):
        return (self.managed.get((dep.group, dep.artifact, dep.type, dep.classifier))
                or self.managed.get((dep.group, dep.artifact, "jar", None)))


# ----------------------------------------------------------------------------
# Resolution

class Artifact:
    __slots__ = ("group", "artifact", "version", "classifier", "has_jar")

    def __init__(self, group, artifact, version, classifier=None, has_jar=True):
        self.group = group
        self.artifact = artifact
        self.version = version
        self.classifier = classifier
        self.has_jar = has_jar

    @property
    def coord(self):
        c = "%s:%s:%s" % (self.group, self.artifact, self.version)
        return c + ":" + self.classifier if self.classifier else c

    def path(self):
        return repo_path(self.group, self.artifact, self.version, self.classifier)


def resolve(roots, managed, runtime):
    """The transitive closure of the root dependencies.

    roots     list of Dep (version may be None: then `managed` gives it)
    managed   dict (group, artifact) -> version, which overrides every version
    runtime   True for the runtime classpath (compile and runtime scopes),
              False for the compile classpath (compile scope only, below the roots)
    Returns a list of Artifact in discovery order."""
    chosen = {}
    order = []
    queue = []
    for r in roots:
        queue.append((r, frozenset(r.exclusions), True))
    while queue:
        dep, excl, is_root = queue.pop(0)
        if dep.type == "pom" and not is_root and dep.scope not in ("compile", "runtime"):
            continue
        if any((g in ("*", dep.group)) and (a in ("*", dep.artifact)) for g, a in excl):
            continue
        key = (dep.group, dep.artifact, dep.classifier)
        if key in chosen:
            continue
        version = managed.get((dep.group, dep.artifact)) or dep.version
        if not version:
            raise RuntimeError("no version for %s:%s" % (dep.group, dep.artifact))
        pom = Pom.get(dep.group, dep.artifact, version)
        if not pom.exists:
            raise RuntimeError("no pom for %s:%s:%s" % (dep.group, dep.artifact, version))
        art = Artifact(dep.group, dep.artifact, version, dep.classifier,
                       has_jar=(pom.packaging != "pom" and dep.type != "pom"))
        chosen[key] = art
        order.append(art)
        for d in pom.deps:
            if d.optional or d.scope not in (("compile", "runtime") if runtime else ("compile",)):
                continue
            if d.type == "test-jar":
                continue
            queue.append((d, excl | frozenset(d.exclusions), False))
    return order


if __name__ == "__main__":
    # maven.py group:artifact:version [runtime]  - print the resolved closure
    g, a, v = sys.argv[1].split(":")
    for art in resolve([Dep(g, a, v)], {}, len(sys.argv) > 2):
        print(art.coord, "" if art.has_jar else "(pom)")
