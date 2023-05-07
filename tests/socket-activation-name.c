/* NBD client library in userspace
 * Copyright Red Hat
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/* Test nbd_{set,get}_socket_activation_name API. */

#undef NDEBUG

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>

#include <libnbd.h>

#include "requires.h"

#define DEBUG_FILE "socket-activation-name.out"

int
main (int argc, char *argv[])
{
  struct nbd_handle *nbd;
  char *r;

  /* Test that this version of nbdkit supports -D nbdkit.environ=1
   * added in nbdkit 1.35.2.
   *
   * As a side-effect, also checks we have nbdkit, the null plugin and
   * a working grep command, all needed below.
   */
  requires ("libnbd_sentinel=42 "
            "nbdkit -v -D nbdkit.environ=1 null --run true 2>&1 | "
            "grep -sq 'debug.*libnbd_sentinel=42'");

  nbd = nbd_create ();
  if (nbd == NULL) {
    fprintf (stderr, "%s\n", nbd_get_error ());
    exit (EXIT_FAILURE);
  }

  /* Check firstly that it defaults to empty string. */
  r = nbd_get_socket_activation_name (nbd);
  assert (r != NULL);
  assert (strcmp (r, "") == 0);
  free (r);

  /* Run external nbdkit and check the LISTEN_FDNAMES environment
   * variable is set to "unknown".  We need to capture the debug
   * output of nbdkit, hence the journey through the shell.
   */
  unlink (DEBUG_FILE);
  char *cmd[] = {
    "sh", "-c",
    "exec 2> " DEBUG_FILE "\n"
    "exec nbdkit --exit-with-parent -v -D nbdkit.environ=1 null 1024\n",
    NULL
  };
  if (nbd_connect_systemd_socket_activation (nbd, cmd) == -1) {
    fprintf (stderr, "%s\n", nbd_get_error ());
    exit (EXIT_FAILURE);
  }

  /* Check the size is expected so we know we managed to connect to nbdkit. */
  assert (nbd_get_size (nbd) == 1024);

  nbd_close (nbd);

  /* nbdkit doesn't know anything about socket activation names, but
   * the LISTEN_FDNAMES environment variable should appear in the
   * debug output.
   */
  assert (system ("grep 'debug.*LISTEN_FDNAMES=unknown' " DEBUG_FILE) == 0);
  unlink (DEBUG_FILE);

  /* Test again with a specific name. */
  nbd = nbd_create ();
  if (nbd == NULL) {
    fprintf (stderr, "%s\n", nbd_get_error ());
    exit (EXIT_FAILURE);
  }
  /* Check we can set it to something and read that back. */
  assert (nbd_set_socket_activation_name (nbd, "hello") == 0);
  r = nbd_get_socket_activation_name (nbd);
  assert (r != NULL);
  assert (strcmp (r, "hello") == 0);
  free (r);

  /* Run external nbdkit again (same command as above). */
  if (nbd_connect_systemd_socket_activation (nbd, cmd) == -1) {
    fprintf (stderr, "%s\n", nbd_get_error ());
    exit (EXIT_FAILURE);
  }

  assert (nbd_get_size (nbd) == 1024);

  nbd_close (nbd);

  /* Check LISTEN_FDNAMES was set to the known value. */
  assert (system ("grep 'debug.*LISTEN_FDNAMES=hello' " DEBUG_FILE) == 0);
  unlink (DEBUG_FILE);

  exit (EXIT_SUCCESS);
}
