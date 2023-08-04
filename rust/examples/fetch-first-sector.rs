//! This example shows how to connect to an NBD server
//! and fetch and print the first sector (usually the
//! boot sector or partition table or filesystem
//! superblock).
//!
//! You can test it with nbdkit like this:
//!
//!     nbdkit -U - floppy . \
//!       --run 'cargo run --example fetch-first-sector -- $unixsocket'
//!
//! The nbdkit floppy plugin creates an MBR disk so the
//! first sector is the partition table.

use pretty_hex::pretty_hex;
use std::env;

fn main() -> anyhow::Result<()> {
    let nbd = libnbd::Handle::new()?;

    let args = env::args_os().collect::<Vec<_>>();
    if args.len() != 2 {
        anyhow::bail!("Usage: {:?} socket", args[0]);
    }
    let socket = &args[1];

    // Connect to the NBD server over a
    // Unix domain socket.
    nbd.connect_unix(socket)?;

    // Read the first sector synchronously.
    let mut buf = [0; 512];
    nbd.pread(&mut buf, 0, None)?;

    // Print the sector in hexdump like format.
    print!("{}", pretty_hex(&buf));

    Ok(())
}
