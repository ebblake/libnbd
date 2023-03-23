/* nbd client library in userspace: state machine
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

/* State machine related to connecting with systemd socket activation. */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "internal.h"

/* This is baked into the systemd socket activation API. */
#define FIRST_SOCKET_ACTIVATION_FD 3

/* == strlen ("LISTEN_PID=") | strlen ("LISTEN_FDS=") */
#define PREFIX_LENGTH 11

extern char **environ;

/* Prepare environment for calling execvp when doing systemd socket
 * activation.  Takes the current environment and copies it.  Removes
 * any existing LISTEN_PID or LISTEN_FDS and replaces them with new
 * variables.  env[0] is "LISTEN_PID=..." which is filled in by
 * CONNECT_SA.START, and env[1] is "LISTEN_FDS=1".
 */
static int
prepare_socket_activation_environment (string_vector *env)
{
  char *p;
  size_t i;

  *env = (string_vector)empty_vector;

  /* Reserve slots env[0] and env[1]. */
  p = strdup ("LISTEN_PID=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
  if (p == NULL)
    goto err;
  if (string_vector_append (env, p) == -1) {
    free (p);
    goto err;
  }
  p = strdup ("LISTEN_FDS=1");
  if (p == NULL)
    goto err;
  if (string_vector_append (env, p) == -1) {
    free (p);
    goto err;
  }

  /* Append the current environment, but remove LISTEN_PID, LISTEN_FDS. */
  for (i = 0; environ[i] != NULL; ++i) {
    if (strncmp (environ[i], "LISTEN_PID=", PREFIX_LENGTH) != 0 &&
        strncmp (environ[i], "LISTEN_FDS=", PREFIX_LENGTH) != 0) {
      char *copy = strdup (environ[i]);
      if (copy == NULL)
        goto err;
      if (string_vector_append (env, copy) == -1) {
        free (copy);
        goto err;
      }
    }
  }

  /* The environ must be NULL-terminated. */
  if (string_vector_append (env, NULL) == -1)
    goto err;

  return 0;

 err:
  set_error (errno, "malloc");
  string_vector_empty (env);
  return -1;
}

STATE_MACHINE {
 CONNECT_SA.START:
  enum state next;
  char *tmpdir;
  char *sockpath;
  int s;
  struct sockaddr_un addr;
  struct execvpe execvpe_ctx;
  string_vector env;
  pid_t pid;

  assert (!h->sock);
  assert (h->argv.ptr);
  assert (h->argv.ptr[0]);

  next = %.DEAD;

  /* Use /tmp instead of TMPDIR because we must ensure the path is
   * short enough to store in the sockaddr_un.  On some platforms this
   * may cause problems so we may need to revisit it.  XXX
   */
  tmpdir = strdup ("/tmp/libnbdXXXXXX");
  if (tmpdir == NULL) {
    set_error (errno, "strdup");
    goto done;
  }

  if (mkdtemp (tmpdir) == NULL) {
    set_error (errno, "mkdtemp");
    goto free_tmpdir;
  }

  if (asprintf (&sockpath, "%s/sock", tmpdir) == -1) {
    set_error (errno, "asprintf");
    goto rmdir_tmpdir;
  }

  s = nbd_internal_socket (AF_UNIX, SOCK_STREAM, 0, false);
  if (s == -1) {
    set_error (errno, "socket");
    goto free_sockpath;
  }

  addr.sun_family = AF_UNIX;
  memcpy (addr.sun_path, sockpath, strlen (sockpath) + 1);
  if (bind (s, (struct sockaddr *) &addr, sizeof addr) == -1) {
    set_error (errno, "bind: %s", sockpath);
    goto close_socket;
  }

  if (listen (s, SOMAXCONN) == -1) {
    set_error (errno, "listen");
    goto unlink_sockpath;
  }

  if (nbd_internal_execvpe_init (&execvpe_ctx, h->argv.ptr[0], h->argv.len) ==
      -1) {
    set_error (errno, "nbd_internal_execvpe_init");
    goto unlink_sockpath;
  }

  if (prepare_socket_activation_environment (&env) == -1)
    /* prepare_socket_activation_environment() calls set_error() internally */
    goto uninit_execvpe;

  pid = fork ();
  if (pid == -1) {
    set_error (errno, "fork");
    goto empty_env;
  }

  if (pid == 0) {         /* child - run command */
    if (s != FIRST_SOCKET_ACTIVATION_FD) {
      if (dup2 (s, FIRST_SOCKET_ACTIVATION_FD) == -1) {
        nbd_internal_fork_safe_perror ("dup2");
        _exit (126);
      }
      if (close (s) == -1) {
        nbd_internal_fork_safe_perror ("close");
        _exit (126);
      }
    }
    else {
      /* We must unset CLOEXEC on the fd.  (dup2 above does this
       * implicitly because CLOEXEC is set on the fd, not on the
       * socket).
       */
      int flags = fcntl (s, F_GETFD, 0);
      if (flags == -1) {
        nbd_internal_fork_safe_perror ("fcntl: F_GETFD");
        _exit (126);
      }
      if (fcntl (s, F_SETFD, (int)(flags & ~(unsigned)FD_CLOEXEC)) == -1) {
        nbd_internal_fork_safe_perror ("fcntl: F_SETFD");
        _exit (126);
      }
    }

    char buf[32];
    const char *v =
      nbd_internal_fork_safe_itoa ((long) getpid (), buf, sizeof buf);
    strcpy (&env.ptr[0][PREFIX_LENGTH], v);

    /* Restore SIGPIPE back to SIG_DFL. */
    if (signal (SIGPIPE, SIG_DFL) == SIG_ERR) {
      nbd_internal_fork_safe_perror ("signal");
      _exit (126);
    }

    (void)nbd_internal_fork_safe_execvpe (&execvpe_ctx, &h->argv, env.ptr);
    nbd_internal_fork_safe_perror (h->argv.ptr[0]);
    if (errno == ENOENT)
      _exit (127);
    else
      _exit (126);
  }

  /* Parent -- we're done; commit. */
  h->sact_tmpdir = tmpdir;
  h->sact_sockpath = sockpath;
  h->pid = pid;

  h->connaddrlen = sizeof addr;
  memcpy (&h->connaddr, &addr, h->connaddrlen);
  next = %^CONNECT.START;

  /* fall through, for releasing the temporaries */

empty_env:
  string_vector_empty (&env);

uninit_execvpe:
  nbd_internal_execvpe_uninit (&execvpe_ctx);

unlink_sockpath:
  if (next == %.DEAD)
    unlink (sockpath);

close_socket:
  close (s);

free_sockpath:
  if (next == %.DEAD)
    free (sockpath);

rmdir_tmpdir:
  if (next == %.DEAD)
    rmdir (tmpdir);

free_tmpdir:
  if (next == %.DEAD)
    free (tmpdir);

done:
  SET_NEXT_STATE (next);
  return 0;
} /* END STATE MACHINE */
