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

/* State machine for parsing structured reply chunk payloads from the server. */

#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>

/* Structured reply must be completely inside the bounds of the
 * requesting command.
 */
static bool
structured_reply_in_bounds (uint64_t offset, uint32_t length,
                            const struct command *cmd)
{
  if (offset < cmd->offset ||
      offset >= cmd->offset + cmd->count ||
      offset + length > cmd->offset + cmd->count) {
    set_error (0, "range of structured reply is out of bounds, "
               "offset=%" PRIu64 ", cmd->offset=%" PRIu64 ", "
               "length=%" PRIu32 ", cmd->count=%" PRIu32 ": "
               "this is likely to be a bug in the NBD server",
               offset, cmd->offset, length, cmd->count);
    return false;
  }

  return true;
}

/* Return true if payload length of block status reply is valid.
 */
static bool
bs_reply_length_ok (uint16_t type, uint32_t length)
{
  /* TODO support 64-bit replies */
  size_t prefix_len = sizeof (struct nbd_chunk_block_status_32);
  size_t extent_len = sizeof (struct nbd_block_descriptor_32);
  assert (type == NBD_REPLY_TYPE_BLOCK_STATUS);

  /* At least one descriptor is needed after id prefix */
  if (length < prefix_len + extent_len)
    return false;

  /* There must be an integral number of extents */
  length -= prefix_len;
  if (length % extent_len != 0)
    return false;

  return true;
}

STATE_MACHINE {
 REPLY.CHUNK_REPLY.START:
  struct command *cmd = h->reply_cmd;
  uint16_t flags, type;
  uint32_t length;

  flags = be16toh (h->sbuf.reply.hdr.structured.flags);
  type = be16toh (h->sbuf.reply.hdr.structured.type);
  length = be32toh (h->sbuf.reply.hdr.structured.length);

  /* Reject a server that replies with too much information, but don't
   * reject a single structured reply to NBD_CMD_READ on the largest
   * size we were willing to send. The most likely culprit is a server
   * that replies with block status with way too many extents, but any
   * oversized reply is going to take long enough to resync that it is
   * not worth keeping the connection alive.
   */
  if (length > MAX_REQUEST_SIZE + sizeof h->sbuf.reply.payload.offset_data) {
    set_error (0, "invalid server reply length %" PRIu32, length);
    SET_NEXT_STATE (%.DEAD);
    return 0;
  }

  /* Skip an unexpected structured reply, including to an unknown cookie. */
  if (cmd == NULL || !h->structured_replies)
    goto resync;
  h->payload_left = length;

  switch (type) {
  case NBD_REPLY_TYPE_NONE:
    if (length != 0 || !(flags & NBD_REPLY_FLAG_DONE))
      goto resync;
    SET_NEXT_STATE (%FINISH);
    break;

  case NBD_REPLY_TYPE_OFFSET_DATA:
    /* The spec states that 0-length requests are unspecified, but
     * 0-length replies are broken. Still, it's easy enough to support
     * them as an extension, so we use < instead of <=.
     */
    if (cmd->type != NBD_CMD_READ ||
        length < sizeof h->sbuf.reply.payload.offset_data)
      goto resync;
    h->rbuf = &h->sbuf.reply.payload.offset_data;
    h->rlen = sizeof h->sbuf.reply.payload.offset_data;
    h->payload_left -= h->rlen;
    SET_NEXT_STATE (%RECV_OFFSET_DATA);
    break;

  case NBD_REPLY_TYPE_OFFSET_HOLE:
    if (cmd->type != NBD_CMD_READ ||
        length != sizeof h->sbuf.reply.payload.offset_hole)
      goto resync;
    h->rbuf = &h->sbuf.reply.payload.offset_hole;
    h->rlen = sizeof h->sbuf.reply.payload.offset_hole;
    h->payload_left -= h->rlen;
    SET_NEXT_STATE (%RECV_OFFSET_HOLE);
    break;

  case NBD_REPLY_TYPE_BLOCK_STATUS:
    if (cmd->type != NBD_CMD_BLOCK_STATUS ||
        !h->meta_valid || h->meta_contexts.len == 0 ||
        !bs_reply_length_ok (type, length))
      goto resync;
    assert (CALLBACK_IS_NOT_NULL (cmd->cb.fn.extent));
    /* Start by reading the context ID. */
    h->rbuf = &h->sbuf.reply.payload.bs_hdr_32;
    h->rlen = sizeof h->sbuf.reply.payload.bs_hdr_32;
    SET_NEXT_STATE (%RECV_BS_HEADER);
    break;

  default:
    if (NBD_REPLY_TYPE_IS_ERR (type)) {
      /* Any payload shorter than uint32_t cannot even carry an errno
       * value; anything longer, even if it is not long enough to be
       * compliant, will favor the wire error over EPROTO during more
       * length checks in RECV_ERROR_MESSAGE and RECV_ERROR_TAIL.
       */
      if (length < sizeof h->sbuf.reply.payload.error.error.error)
        goto resync;
      h->rbuf = &h->sbuf.reply.payload.error.error;
      h->rlen = MIN (length, sizeof h->sbuf.reply.payload.error.error);
      SET_NEXT_STATE (%RECV_ERROR);
    }
    else
      goto resync;
    break;
  }
  return 0;

 resync:
  h->rbuf = NULL;
  h->rlen = h->payload_left;
  h->payload_left = 0;
  SET_NEXT_STATE (%RESYNC);
  return 0;

 REPLY.CHUNK_REPLY.RECV_ERROR:
  struct command *cmd = h->reply_cmd;
  uint32_t length, msglen, error;

  switch (recv_into_rbuf (h)) {
  case -1: SET_NEXT_STATE (%.DEAD); return 0;
  case 1:
    save_reply_state (h);
    SET_NEXT_STATE (%.READY);
    return 0;
  case 0:
    length = h->payload_left;
    h->payload_left -= MIN (length, sizeof h->sbuf.reply.payload.error.error);
    assert (length >= sizeof h->sbuf.reply.payload.error.error.error);
    assert (cmd);

    if (length < sizeof h->sbuf.reply.payload.error.error)
      goto resync;

    msglen = be16toh (h->sbuf.reply.payload.error.error.len);
    if (msglen > h->payload_left ||
        msglen > sizeof h->sbuf.reply.payload.error.msg)
      goto resync;

    h->rbuf = h->sbuf.reply.payload.error.msg;
    h->rlen = msglen;
    h->payload_left -= h->rlen;
    SET_NEXT_STATE (%RECV_ERROR_MESSAGE);
  }
  return 0;

 resync:
  /* Favor the error packet's errno over RESYNC's EPROTO. */
  error = be32toh (h->sbuf.reply.payload.error.error.error);
  if (cmd->error == 0)
    cmd->error = nbd_internal_errno_of_nbd_error (error);
  h->rbuf = NULL;
  h->rlen = h->payload_left;
  h->payload_left = 0;
  SET_NEXT_STATE (%RESYNC);
  return 0;

 REPLY.CHUNK_REPLY.RECV_ERROR_MESSAGE:
  uint32_t msglen;
  uint16_t type;

  switch (recv_into_rbuf (h)) {
  case -1: SET_NEXT_STATE (%.DEAD); return 0;
  case 1:
    save_reply_state (h);
    SET_NEXT_STATE (%.READY);
    return 0;
  case 0:
    msglen = be16toh (h->sbuf.reply.payload.error.error.len);
    type = be16toh (h->sbuf.reply.hdr.structured.type);

    if (msglen)
      debug (h, "structured error server message: %.*s", (int)msglen,
             h->sbuf.reply.payload.error.msg);

    /* Special case two specific errors; silently ignore tail for all others */
    h->rbuf = NULL;
    h->rlen = h->payload_left;
    switch (type) {
    case NBD_REPLY_TYPE_ERROR:
      if (h->payload_left != 0)
        debug (h, "ignoring unexpected slop after error message, "
               "the server may have a bug");
      break;
    case NBD_REPLY_TYPE_ERROR_OFFSET:
      if (h->payload_left != sizeof h->sbuf.reply.payload.error.offset)
        debug (h, "unable to safely extract error offset, "
               "the server may have a bug");
      else
        h->rbuf = &h->sbuf.reply.payload.error.offset;
      break;
    }
    h->payload_left = 0;
    SET_NEXT_STATE (%RECV_ERROR_TAIL);
  }
  return 0;

 REPLY.CHUNK_REPLY.RECV_ERROR_TAIL:
  struct command *cmd = h->reply_cmd;
  uint32_t error;
  uint16_t type;

  switch (recv_into_rbuf (h)) {
  case -1: SET_NEXT_STATE (%.DEAD); return 0;
  case 1:
    save_reply_state (h);
    SET_NEXT_STATE (%.READY);
    return 0;
  case 0:
    error = be32toh (h->sbuf.reply.payload.error.error.error);
    type = be16toh (h->sbuf.reply.hdr.structured.type);

    assert (cmd); /* guaranteed by CHECK */

    /* The spec requires the server to send a non-zero error */
    error = nbd_internal_errno_of_nbd_error (error);
    if (error == 0) {
      debug (h, "server forgot to set error; using EPROTO");
      error = EPROTO;
    }

    /* Sanity check that any error offset is in range, then invoke
     * user callback if present.  Ignore the offset if it was bogus.
     */
    if (type == NBD_REPLY_TYPE_ERROR_OFFSET && h->rbuf) {
      uint64_t offset = be64toh (h->sbuf.reply.payload.error.offset);
      if (structured_reply_in_bounds (offset, 0, cmd) &&
          cmd->type == NBD_CMD_READ &&
          CALLBACK_IS_NOT_NULL (cmd->cb.fn.chunk)) {
        int scratch = error;

        /* Different from successful reads: inform the callback about the
         * current error rather than any earlier one. If the callback fails
         * without setting errno, then use the server's error below.
         */
        if (CALL_CALLBACK (cmd->cb.fn.chunk,
                           (char *)cmd->data + (offset - cmd->offset),
                           0, offset, LIBNBD_READ_ERROR,
                           &scratch) == -1)
          if (cmd->error == 0)
            cmd->error = scratch;
      }
      else
        debug (h, "no use for error offset %" PRIu64, offset);
    }

    /* Preserve first error encountered */
    if (cmd->error == 0)
      cmd->error = error;

    SET_NEXT_STATE (%FINISH);
  }
  return 0;

 REPLY.CHUNK_REPLY.RECV_OFFSET_DATA:
  struct command *cmd = h->reply_cmd;
  uint64_t offset;

  switch (recv_into_rbuf (h)) {
  case -1: SET_NEXT_STATE (%.DEAD); return 0;
  case 1:
    save_reply_state (h);
    SET_NEXT_STATE (%.READY);
    return 0;
  case 0:
    offset = be64toh (h->sbuf.reply.payload.offset_data.offset);

    assert (cmd); /* guaranteed by CHECK */
    assert (cmd->data && cmd->type == NBD_CMD_READ);

    /* Is the data within bounds? */
    if (! structured_reply_in_bounds (offset, h->payload_left, cmd)) {
      SET_NEXT_STATE (%.DEAD);
      return 0;
    }
    if (cmd->data_seen <= cmd->count)
      cmd->data_seen += h->payload_left;
    /* Now this is the byte offset in the read buffer. */
    offset -= cmd->offset;

    /* Set up to receive the data directly to the user buffer. */
    h->rbuf = (char *)cmd->data + offset;
    h->rlen = h->payload_left;
    SET_NEXT_STATE (%RECV_OFFSET_DATA_DATA);
  }
  return 0;

 REPLY.CHUNK_REPLY.RECV_OFFSET_DATA_DATA:
  struct command *cmd = h->reply_cmd;
  uint64_t offset;

  switch (recv_into_rbuf (h)) {
  case -1: SET_NEXT_STATE (%.DEAD); return 0;
  case 1:
    save_reply_state (h);
    SET_NEXT_STATE (%.READY);
    return 0;
  case 0:
    offset = be64toh (h->sbuf.reply.payload.offset_data.offset);

    assert (cmd); /* guaranteed by CHECK */
    if (CALLBACK_IS_NOT_NULL (cmd->cb.fn.chunk)) {
      int error = cmd->error;

      if (CALL_CALLBACK (cmd->cb.fn.chunk,
                         (char *)cmd->data + (offset - cmd->offset),
                         h->payload_left, offset,
                         LIBNBD_READ_DATA, &error) == -1)
        if (cmd->error == 0)
          cmd->error = error ? error : EPROTO;
    }
    h->payload_left = 0;

    SET_NEXT_STATE (%FINISH);
  }
  return 0;

 REPLY.CHUNK_REPLY.RECV_OFFSET_HOLE:
  struct command *cmd = h->reply_cmd;
  uint64_t offset;
  uint32_t length;

  switch (recv_into_rbuf (h)) {
  case -1: SET_NEXT_STATE (%.DEAD); return 0;
  case 1:
    save_reply_state (h);
    SET_NEXT_STATE (%.READY);
    return 0;
  case 0:
    offset = be64toh (h->sbuf.reply.payload.offset_hole.offset);
    length = be32toh (h->sbuf.reply.payload.offset_hole.length);

    assert (cmd); /* guaranteed by CHECK */
    assert (cmd->data && cmd->type == NBD_CMD_READ);

    /* Is the data within bounds? */
    if (! structured_reply_in_bounds (offset, length, cmd)) {
      SET_NEXT_STATE (%.DEAD);
      return 0;
    }
    if (cmd->data_seen <= cmd->count)
      cmd->data_seen += length;
    /* Now this is the byte offset in the read buffer. */
    offset -= cmd->offset;

    /* The spec states that 0-length requests are unspecified, but
     * 0-length replies are broken. Still, it's easy enough to support
     * them as an extension, and this works even when length == 0.
     */
    if (!cmd->initialized)
      memset ((char *)cmd->data + offset, 0, length);
    if (CALLBACK_IS_NOT_NULL (cmd->cb.fn.chunk)) {
      int error = cmd->error;

      if (CALL_CALLBACK (cmd->cb.fn.chunk,
                         (char *)cmd->data + offset, length,
                         cmd->offset + offset,
                         LIBNBD_READ_HOLE, &error) == -1)
        if (cmd->error == 0)
          cmd->error = error ? error : EPROTO;
    }

    SET_NEXT_STATE (%FINISH);
  }
  return 0;

 REPLY.CHUNK_REPLY.RECV_BS_HEADER:
  struct command *cmd = h->reply_cmd;
  uint16_t type;

  switch (recv_into_rbuf (h)) {
  case -1: SET_NEXT_STATE (%.DEAD); return 0;
  case 1:
    save_reply_state (h);
    SET_NEXT_STATE (%.READY);
    return 0;
  case 0:
    type = be16toh (h->sbuf.reply.hdr.structured.type);

    assert (cmd); /* guaranteed by CHECK */
    assert (cmd->type == NBD_CMD_BLOCK_STATUS);
    assert (bs_reply_length_ok (type, h->payload_left));
    STATIC_ASSERT (sizeof (struct nbd_block_descriptor_32) ==
                   2 * sizeof *h->bs_entries,
                   _block_desc_is_multiple_of_bs_entry);
    h->payload_left -= sizeof h->sbuf.reply.payload.bs_hdr_32;
    assert (h->payload_left % sizeof (struct nbd_block_descriptor_32) == 0);
    h->bs_count = h->payload_left / sizeof (struct nbd_block_descriptor_32);

    free (h->bs_entries);
    h->bs_entries = malloc (h->payload_left);
    if (h->bs_entries == NULL) {
      SET_NEXT_STATE (%.DEAD);
      set_error (errno, "malloc");
      return 0;
    }
    h->rbuf = h->bs_entries;
    h->rlen = h->payload_left;
    h->payload_left = 0;
    SET_NEXT_STATE (%RECV_BS_ENTRIES);
  }
  return 0;

 REPLY.CHUNK_REPLY.RECV_BS_ENTRIES:
  struct command *cmd = h->reply_cmd;
  size_t i;
  uint32_t context_id;

  switch (recv_into_rbuf (h)) {
  case -1: SET_NEXT_STATE (%.DEAD); return 0;
  case 1:
    save_reply_state (h);
    SET_NEXT_STATE (%.READY);
    return 0;
  case 0:
    assert (cmd); /* guaranteed by CHECK */
    assert (cmd->type == NBD_CMD_BLOCK_STATUS);
    assert (CALLBACK_IS_NOT_NULL (cmd->cb.fn.extent));
    assert (h->bs_count && h->bs_entries);
    assert (h->meta_valid);

    /* Look up the context ID. */
    context_id = be32toh (h->sbuf.reply.payload.bs_hdr_32.context_id);
    for (i = 0; i < h->meta_contexts.len; ++i)
      if (context_id == h->meta_contexts.ptr[i].context_id)
        break;

    if (i < h->meta_contexts.len) {
      int error = cmd->error;
      const char *name = h->meta_contexts.ptr[i].name;

      /* Need to byte-swap the entries returned, but apart from that
       * we don't validate them.  Yes, our 32-bit public API foolishly
       * tracks the number of uint32_t instead of block descriptors;
       * see _block_desc_is_multiple_of_bs_entry above.
       */
      for (i = 0; i < h->bs_count * 2; ++i)
        h->bs_entries[i] = be32toh (h->bs_entries[i]);

      /* Call the caller's extent function.  */
      if (CALL_CALLBACK (cmd->cb.fn.extent, name, cmd->offset,
                         h->bs_entries, h->bs_count * 2, &error) == -1)
        if (cmd->error == 0)
          cmd->error = error ? error : EPROTO;
    }
    else
      /* Emit a debug message, but ignore it. */
      debug (h, "server sent unexpected meta context ID %" PRIu32,
             context_id);

    SET_NEXT_STATE (%FINISH);
  }
  return 0;

 REPLY.CHUNK_REPLY.RESYNC:
  struct command *cmd = h->reply_cmd;
  uint16_t type;
  uint32_t length;

  assert (h->rbuf == NULL);
  switch (recv_into_rbuf (h)) {
  case -1: SET_NEXT_STATE (%.DEAD); return 0;
  case 1:
    save_reply_state (h);
    SET_NEXT_STATE (%.READY);
    return 0;
  case 0:
    /* If this reply is to an unknown command, FINISH_COMMAND will
     * diagnose and ignore the server bug.  Otherwise, ensure the
     * pending command sees a failure of EPROTO if it does not already
     * have an error.
     */
    if (cmd == NULL) {
      SET_NEXT_STATE (%^FINISH_COMMAND);
      return 0;
    }
    type = be16toh (h->sbuf.reply.hdr.structured.type);
    length = be32toh (h->sbuf.reply.hdr.structured.length);
    debug (h, "unexpected reply type %u or payload length %" PRIu32
           " for cookie %" PRIu64 " and command %" PRIu32
           ", this is probably a server bug",
           type, length, cmd->cookie, cmd->type);
    if (cmd->error == 0)
      cmd->error = EPROTO;
    SET_NEXT_STATE (%FINISH);
  }
  return 0;

 REPLY.CHUNK_REPLY.FINISH:
  uint16_t flags;

  assert (h->payload_left == 0);
  flags = be16toh (h->sbuf.reply.hdr.structured.flags);
  if (flags & NBD_REPLY_FLAG_DONE) {
    SET_NEXT_STATE (%^FINISH_COMMAND);
  }
  else {
    h->reply_cmd = NULL;
    SET_NEXT_STATE (%.READY);
  }
  return 0;

} /* END STATE MACHINE */
