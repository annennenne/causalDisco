# Returns an integer number of gigabytes on the heap (rough calculation)
# Requires rjava to be initialized
if (check_tetrad_install()$installed && check_tetrad_install()$java_ok) {
  causalDisco:::current_heap_gb()
}
