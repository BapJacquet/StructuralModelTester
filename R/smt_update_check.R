#' Default GitHub slug for update checks
#' @keywords internal
smt_github_repo_default <- function() "BapJacquet/StructuralModelTester"

#' Run a git command in a directory (quiet, timed when R supports it)
#'
#' Uses `setwd(root)` instead of `git -C` so paths with spaces stay reliable
#' under `system2(..., timeout = )` on Unix.
#' @keywords internal
smt_git_run <- function(args, root, timeout_sec = 8) {
  git <- Sys.which("git")
  if (!nzchar(git)) {
    return(list(ok = FALSE, out = character(), code = 127L))
  }
  root <- as.character(root %||% "")
  if (!nzchar(root) || !dir.exists(root)) {
    return(list(ok = FALSE, out = character(), code = 1L))
  }
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  timeout_sec <- as.numeric(timeout_sec)[1]
  if (!is.finite(timeout_sec) || timeout_sec <= 0) timeout_sec <- 8
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(root)
  # Suppress the console warning R prints for non-zero status; callers use $ok.
  res <- tryCatch(
    suppressWarnings({
      if (getRversion() >= "4.2") {
        system2(git, args, stdout = TRUE, stderr = TRUE, timeout = timeout_sec)
      } else {
        system2(git, args, stdout = TRUE, stderr = TRUE)
      }
    }),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(ok = FALSE, out = conditionMessage(res), code = 1L))
  }
  code <- as.integer(attr(res, "status") %||% 0L)
  if (length(code) != 1L || is.na(code)) code <- 0L
  list(ok = identical(code, 0L), out = as.character(res), code = code)
}

#' True if `root` is a git working tree
#' @keywords internal
smt_is_git_worktree <- function(root) {
  r <- smt_git_run(c("rev-parse", "--is-inside-work-tree"), root, timeout_sec = 3)
  isTRUE(r$ok) && any(grepl("^true$", trimws(r$out), ignore.case = TRUE))
}

#' Parse owner/repo from a git remote URL
#' @keywords internal
smt_parse_github_slug <- function(url) {
  url <- as.character(url %||% "")[1]
  if (!nzchar(url)) return(NA_character_)
  url <- sub("\\.git$", "", url)
  m <- regexec("github\\.com[:/]+([^/]+)/([^/]+)$", url, ignore.case = TRUE)
  hit <- regmatches(url, m)[[1]]
  if (length(hit) >= 3L) {
    return(paste0(hit[[2]], "/", hit[[3]]))
  }
  NA_character_
}

#' GitHub slug from origin remote, else package default
#' @keywords internal
smt_github_repo_slug <- function(root = "") {
  if (nzchar(root) && smt_is_git_worktree(root)) {
    r <- smt_git_run(c("config", "--get", "remote.origin.url"), root, timeout_sec = 3)
    if (isTRUE(r$ok) && length(r$out)) {
      slug <- smt_parse_github_slug(r$out[[1]])
      if (!is.na(slug) && nzchar(slug)) return(slug)
    }
  }
  smt_github_repo_default()
}

#' Fetch DESCRIPTION Version from GitHub raw content
#' @keywords internal
smt_remote_description_version <- function(slug, ref = "main", timeout_sec = 6) {
  slug <- as.character(slug %||% "")[1]
  if (!nzchar(slug)) return(NA_character_)
  url <- sprintf(
    "https://raw.githubusercontent.com/%s/%s/DESCRIPTION",
    slug, as.character(ref %||% "main")[1]
  )
  txt <- tryCatch(
    {
      con <- url(url, open = "rb")
      on.exit(close(con), add = TRUE)
      # Best-effort short read; socket timeouts vary by platform
      rawToChar(readBin(con, what = "raw", n = 8192L))
    },
    error = function(e) ""
  )
  if (!nzchar(txt)) return(NA_character_)
  # DESCRIPTION is DCF — Version: line is enough
  m <- regexec("(?m)^Version:\\s*(\\S+)", txt, perl = TRUE)
  hit <- regmatches(txt, m)[[1]]
  if (length(hit) >= 2L) hit[[2]] else NA_character_
}

#' Compare two package versions; TRUE if remote is strictly newer
#' @keywords internal
smt_version_is_newer <- function(remote, local) {
  remote <- as.character(remote %||% "")[1]
  local <- as.character(local %||% "")[1]
  if (!nzchar(remote) || !nzchar(local)) return(FALSE)
  tryCatch(
    utils::compareVersion(remote, local) > 0,
    error = function(e) FALSE
  )
}

#' Check whether a newer package revision is available on git / GitHub
#'
#' Prefer a local clone: `git fetch` then count commits behind `origin/<branch>`
#' (fallback `origin/main`). If there is no git worktree (installed package),
#' compare the local DESCRIPTION version to `main` on GitHub.
#'
#' Always fails soft (returns `update_available = FALSE`) when offline / no git.
#'
#' @return A list with at least `update_available` (logical) and `message`
#'   (character). Extra fields: `source`, `behind`, `local_version`,
#'   `remote_version`, `branch`, `repo`.
#' @keywords internal
smt_update_check <- function(timeout_sec = 8,
                             root = NULL,
                             local_version = NULL) {
  empty <- function(msg = "", ...) {
    c(
      list(
        update_available = FALSE,
        message = as.character(msg %||% "")[1],
        source = NA_character_,
        behind = 0L,
        local_version = as.character(local_version %||% smt_package_version())[1],
        remote_version = NA_character_,
        branch = NA_character_,
        repo = smt_github_repo_default()
      ),
      list(...)
    )
  }

  if (is.null(local_version) || !nzchar(as.character(local_version)[1])) {
    local_version <- smt_package_version()
  }
  local_version <- as.character(local_version)[1]

  if (is.null(root) || !nzchar(as.character(root)[1])) {
    root <- tryCatch(smt_find_pkg_root(), error = function(e) "")
  }
  root <- as.character(root %||% "")[1]
  repo <- smt_github_repo_slug(root)

  # --- Path A: local git clone ---
  if (nzchar(root) && smt_is_git_worktree(root)) {
    br <- smt_git_run(c("rev-parse", "--abbrev-ref", "HEAD"), root, timeout_sec = 3)
    branch <- if (isTRUE(br$ok) && length(br$out)) {
      trimws(br$out[[1]])
    } else {
      "main"
    }
    if (identical(branch, "HEAD")) branch <- "main"

    fetch <- smt_git_run(
      c(
        "-c", "http.lowSpeedLimit=1000",
        "-c", paste0("http.lowSpeedTime=", max(3L, as.integer(timeout_sec))),
        "fetch", "--quiet", "--no-tags", "origin"
      ),
      root,
      timeout_sec = timeout_sec
    )
    # Even if fetch fails (offline), still try comparing to existing origin/* tips
    candidates <- unique(c(paste0("origin/", branch), "origin/main", "origin/master"))
    remote_ref <- NA_character_
    behind <- NA_integer_
    for (ref in candidates) {
      exists <- smt_git_run(
        c("rev-parse", "--verify", "--quiet", ref),
        root,
        timeout_sec = 3
      )
      if (!isTRUE(exists$ok)) next
      cnt <- smt_git_run(
        c("rev-list", "--count", paste0("HEAD..", ref)),
        root,
        timeout_sec = 3
      )
      if (!isTRUE(cnt$ok) || !length(cnt$out)) next
      n <- suppressWarnings(as.integer(trimws(cnt$out[[1]])))
      if (!is.finite(n)) next
      remote_ref <- ref
      behind <- n
      break
    }

    remote_ver <- smt_remote_description_version(
      repo, ref = sub("^origin/", "", remote_ref %||% "main"),
      timeout_sec = min(6, timeout_sec)
    )

    if (is.finite(behind) && behind > 0L) {
      msg <- sprintf(
        "Git is %d commit%s behind %s. Pull / reinstall: remotes::install_github(\"%s\").",
        behind,
        if (behind == 1L) "" else "s",
        remote_ref,
        repo
      )
      if (!is.na(remote_ver) && smt_version_is_newer(remote_ver, local_version)) {
        msg <- paste0(msg, sprintf(" Remote package version: %s (local %s).", remote_ver, local_version))
      }
      return(list(
        update_available = TRUE,
        message = msg,
        source = "git",
        behind = as.integer(behind),
        local_version = local_version,
        remote_version = remote_ver,
        branch = branch,
        repo = repo,
        remote_ref = remote_ref,
        fetch_ok = isTRUE(fetch$ok)
      ))
    }

    if (!is.na(remote_ver) && smt_version_is_newer(remote_ver, local_version)) {
      return(list(
        update_available = TRUE,
        message = sprintf(
          "Version %s is on GitHub (you have %s). remotes::install_github(\"%s\").",
          remote_ver, local_version, repo
        ),
        source = "version",
        behind = 0L,
        local_version = local_version,
        remote_version = remote_ver,
        branch = branch,
        repo = repo,
        fetch_ok = isTRUE(fetch$ok)
      ))
    }

    return(list(
      update_available = FALSE,
      message = if (isTRUE(fetch$ok)) {
        "Up to date with origin."
      } else {
        "Could not reach origin (offline or no remote)."
      },
      source = "git",
      behind = if (is.finite(behind)) as.integer(behind) else 0L,
      local_version = local_version,
      remote_version = remote_ver,
      branch = branch,
      repo = repo,
      fetch_ok = isTRUE(fetch$ok)
    ))
  }

  # --- Path B: installed package (no .git) — compare DESCRIPTION versions ---
  remote_ver <- smt_remote_description_version(repo, ref = "main", timeout_sec = timeout_sec)
  if (is.na(remote_ver) || !nzchar(remote_ver)) {
    return(empty("Could not check GitHub for updates.", source = "version", repo = repo))
  }
  if (smt_version_is_newer(remote_ver, local_version)) {
    return(list(
      update_available = TRUE,
      message = sprintf(
        "Version %s is on GitHub (you have %s). remotes::install_github(\"%s\").",
        remote_ver, local_version, repo
      ),
      source = "version",
      behind = 0L,
      local_version = local_version,
      remote_version = remote_ver,
      branch = "main",
      repo = repo
    ))
  }
  empty("Up to date.", source = "version", repo = repo, remote_version = remote_ver)
}
