/* NBD client library in userspace
 * Copyright (C) 2013-2019 Red Hat Inc.
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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <unistd.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "internal.h"

/* For all connections in the CREATED state, start connecting them to
 * a Unix domain socket.  Wait until at least one is in the READY
 * state.
 */
int
nbd_connect_unix (struct nbd_handle *h, const char *sockpath)
{
  size_t i;
  struct sockaddr_un sun;
  socklen_t len;
  bool started;

  sun.sun_family = AF_UNIX;
  memset (sun.sun_path, 0, sizeof (sun.sun_path));
  strncpy (sun.sun_path, sockpath, sizeof (sun.sun_path) - 1);
  len = sizeof (sun.sun_family) + strlen (sun.sun_path) + 1;

  started = false;
  for (i = 0; i < h->multi_conn; ++i) {
    if (h->conns[i]->state == STATE_CREATED) {
      if (h->conns[i]->fd >= 0)
        close (h->conns[i]->fd);
      h->conns[i]->fd =
        socket (AF_UNIX, SOCK_STREAM|SOCK_NONBLOCK|SOCK_CLOEXEC, 0);
      if (h->conns[i]->fd == -1) {
        set_error (errno, "socket");
        return -1;
      }

      if (nbd_aio_connect (h->conns[i], (struct sockaddr *) &sun, len) == -1)
        return -1;
      started = true;
    }
  }

  if (!started) {
    set_error (0, "nbd_connect_unix: no connections in this handle were in the created state, this is likely to be caused by a programming error in the calling program");
    return -1;
  }

  for (;;) {
    bool connected = false;

    /* Are any connected? */
    for (i = 0; i < h->multi_conn; ++i) {
      if (nbd_aio_is_ready (h->conns[i])) {
        connected = true;
        break;
      }
    }

    if (connected)
      break;

    if (nbd_aio_poll (h, -1) == -1)
      return -1;
  }

  return 0;
}

int
nbd_aio_connect (struct nbd_connection *conn,
                 const struct sockaddr *addr, socklen_t len)
{
  memcpy (&conn->connaddr, addr, len);
  conn->connaddrlen = len;

  return nbd_internal_run (conn->h, conn, cmd_connect);
}
