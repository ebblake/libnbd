//! This example shows how to connect to an NBD
//! server and read the size of the disk.
//!
//! You can test it with nbdkit like this:
//!
//!     nbdkit -U - memory 1M \
//!       --run 'cargo run --example get-size -- $unixsocket'

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

    // Read the size in bytes and print it.
    let size = nbd.get_size()?;
    println!("{:?}: size = {size} bytes", socket);

    Ok(())
}
