# Returns an integer number of gigabytes on the heap (rough calculation)
# Requires rjava to be initialized
if (verify_tetrad()$installed && verify_tetrad()$java_ok) {
  causalDisco:::current_heap_gb()
}
