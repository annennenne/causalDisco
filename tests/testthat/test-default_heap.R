# ──────────────────────────────────────────────────────────────────────────────
# default_heap()
# ──────────────────────────────────────────────────────────────────────────────

test_that("default_heap respects option and env-var in priority order", {
  # ── 1. Neither option nor env-var  ───────────────────────────────
  withr::with_envvar(list(JAVA_HEAP_SIZE = NA), {
    withr::with_options(list(java.heap.size = NULL), {
      expect_equal(default_heap(), "2g") # fallback
    })
  })

  # ── 2. Option present  ───────────────────────────────────────────
  withr::with_envvar(list(JAVA_HEAP_SIZE = ""), {
    withr::with_options(list(java.heap.size = "8g"), {
      expect_equal(default_heap(), "8g") # option wins
    })
  })

  # ── 3. Only env-var present  ─────────────────────────────────────
  withr::with_envvar(list(JAVA_HEAP_SIZE = "6g"), {
    withr::with_options(list(java.heap.size = NULL), {
      expect_equal(default_heap(), "6g") # env-var when no option
    })
  })
})
