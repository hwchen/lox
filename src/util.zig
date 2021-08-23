pub fn line(idx: u64, source: []const u8) u64 {
    var count: u64 = 1;
    for (source[0..idx]) |c| {
        switch (c) {
            '\n' => count += 1,
            else => {},
        }
    }
    return count;
}
