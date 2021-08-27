pub fn line(idx: u64, source: []const u8) u64 {
    var count: u64 = 1;
    for (source[0..idx]) |c| {
        switch (c) {
            '\n' => count += 1,
            else => {},
        }
    }
    // check in case current is on a newline
    if (source[idx - 1] == '\n') {
        count -= 1;
    }
    return count;
}
