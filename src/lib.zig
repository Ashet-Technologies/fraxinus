const std = @import("std");

pub const Color = enum(u3) {
    black = 0b000, // 0bRGB
    blue = 0b001,
    green = 0b010,
    cyan = 0b011,
    red = 0b100,
    magenta = 0b101,
    yellow = 0b110,
    white = 0b111,
};

pub const Char = packed struct(u32) {
    codepoint: u24,
    attributes: Attributes,
};

pub const Attributes = packed struct(u8) {
    foreground: Color,
    background: Color,
    blink: bool,
    underline: bool,
};

pub const VirtualTerminal = struct {
    page: Page, // content of the screen
    state: State = .{}, // current cursor state
    dirty_lines: std.DynamicBitSet, // set of lines that have been changed in the terminal

    pub fn init(allocator: std.mem.Allocator, width: u15, height: u15) !VirtualTerminal {
        var page = try Page.alloc(allocator, width, height);
        errdefer page.free(allocator);

        var dirty_lines = try std.DynamicBitSet.initEmpty(allocator, height);
        errdefer dirty_lines.deinit();

        return VirtualTerminal{
            .page = page,
            .dirty_lines = dirty_lines,
        };
    }

    pub fn deinit(vt: *VirtualTerminal, allocator: std.mem.Allocator) void {
        vt.page.free(allocator);
        vt.dirty_lines.deinit();
        vt.* = undefined;
    }

    pub fn write(vt: *VirtualTerminal, string: []const u8) void {
        var proc = Processor.new(vt);
        proc.write(string);
    }

    pub fn execute(vt: *VirtualTerminal, cmd: Command) void {
        var proc = Processor.new(vt);
        proc.execute(cmd);
    }

    pub fn markTerminalClean(vt: *VirtualTerminal) void {
        vt.dirty_lines.setRangeValue(.{ .start = 0, .end = vt.dirty_lines.count() }, false);
    }

    pub fn markLineDirty(vt: *VirtualTerminal, line: usize) void {
        std.debug.assert(line < vt.page.height);
    }

    pub fn setCursor(vt: *VirtualTerminal, point: Point) void {
        vt.state.cursor.row = @min(vt.page.height -| 1, point.row);
        vt.state.cursor.column = @min(vt.page.width -| 1, point.column);
    }

    pub fn setAttributes(vt: *VirtualTerminal, attributes: Attributes) void {
        vt.state.current_attributes = attributes;
    }
};

pub const State = struct {
    current_attributes: Attributes = .{
        .foreground = .white,
        .background = .black,
        .blink = false,
        .underline = false,
    },
    cursor: Point = Point.new(0, 0),
};

pub const Processor = struct {
    vt: *VirtualTerminal,

    pub fn new(vt: *VirtualTerminal) Processor {
        return .{ .vt = vt };
    }

    pub fn write(proc: *Processor, string: []const u8) void {
        var view = std.unicode.Utf8View.init(string) catch @panic("TODO: Implement partial utf-8 sequencing");
        var iter = view.iterator();

        const column = &proc.vt.state.cursor.column;
        const row = &proc.vt.state.cursor.row;

        while (iter.nextCodepoint()) |codepoint| {
            const char = proc.makeChar(codepoint);

            const index = proc.vt.page.stride * (row.*) + (column.*);
            proc.vt.page.chars[index] = char;

            column.* += 1;

            if (proc.vt.state.cursor.column >= proc.vt.page.width) {
                proc.vt.state.cursor.column = 0;
                proc.feedLine();
            }
        }
    }

    pub fn execute(proc: *Processor, cmd: Command) void {
        switch (cmd) {
            .set_foreground => |color| proc.vt.state.current_attributes.foreground = color,
            .set_background => |color| proc.vt.state.current_attributes.background = color,
            .set_cursor => |point| proc.vt.setCursor(point),
            .carriage_return => proc.vt.state.cursor.column = 0,
            .line_feed => proc.feedLine(),
            .new_line => {
                proc.execute(.line_feed);
                proc.execute(.carriage_return);
            },
            .clear_line => {
                proc.vt.state.cursor.column = 0;
                @memset(proc.vt.page.rowMut(proc.vt.state.cursor.row), proc.makeChar(' '));
            },
            .clear_screen => {
                proc.vt.page.clear(proc.makeChar(' '));
                proc.vt.state.cursor = Point.zero;
            },
        }
    }

    fn feedLine(proc: *Processor) void {
        proc.vt.state.cursor.row += 1;
        if (proc.vt.state.cursor.row >= proc.vt.page.height) {
            proc.vt.page.scroll(1, proc.makeChar(' '));
            proc.vt.state.cursor.row -= 1;
            std.debug.assert(proc.vt.state.cursor.row < proc.vt.page.height);
        }
    }

    fn makeChar(proc: *Processor, codepoint: u24) Char {
        return Char{
            .codepoint = codepoint,
            .attributes = proc.vt.state.current_attributes,
        };
    }
};

pub const Page = struct {
    chars: [*]Char,
    stride: usize,
    width: u16,
    height: u16,
    max_width: u16,
    max_height: u16,

    pub fn alloc(allocator: std.mem.Allocator, max_width: u16, max_height: u16) !Page {
        return Page{
            .chars = (try allocator.alloc(Char, @as(usize, max_width) * max_height)).ptr,
            .stride = max_width,

            .width = max_width,
            .height = max_height,

            .max_width = max_width,
            .max_height = max_height,
        };
    }

    pub fn free(page: *Page, allocator: std.mem.Allocator) void {
        allocator.free(page.chars[0 .. page.stride * page.max_height]);
        page.* = undefined;
    }

    pub fn resize(page: *Page, width: u16, height: u16) void {
        page.width = @min(width, page.max_width);
        page.height = @min(height, page.max_height);
    }

    pub fn clear(page: *Page, char: Char) void {
        for (0..page.height) |y| {
            @memset((page.chars + page.stride * y)[0..page.width], char);
        }
    }

    pub fn scroll(page: *Page, amount: usize, fill_char: Char) void {
        if (amount == 0)
            return;
        if (amount >= page.height) {
            page.clear(fill_char);
            return;
        }

        for (amount..page.height) |src_y| {
            const dst_y = src_y - amount;
            @memcpy(page.rowMut(dst_y), page.row(src_y));
        }
        for (page.height - amount..page.height) |y| {
            @memset(page.rowMut(y), fill_char);
        }
     }

    pub fn rowMut(page: *Page, y: usize) []Char {
        const stride = @as(usize, page.stride);
        return page.chars[stride * (y + 0) .. stride * (y + 1)];
    }

    pub fn row(page: *const Page, y: usize) []const Char {
        const stride = @as(usize, page.stride);
        return page.chars[stride * (y + 0) .. stride * (y + 1)];
    }
};

pub const Point = struct {
    pub const zero = new(0, 0);

    row: u16,
    column: u16,

    pub fn new(x: u16, y: u16) Point {
        return .{ .row = y, .column = x };
    }
};

pub const Command = union(enum) {
    set_foreground: Color,
    set_background: Color,
    set_cursor: Point,
    carriage_return,
    line_feed,
    new_line,
    clear_line,
    clear_screen,
};

pub const StreamEvent = union(enum) {
    text: []const u8,
    command: Command,
};

/// https://en.wikipedia.org/wiki/ANSI_escape_code
pub const AnsiDecoder = struct {
    intermediate_buffer: std.BoundedArray(u8, 32) = .{}, // max limit

    pub const FeedResult = struct { count: usize, event: ?StreamEvent };
    pub fn feed(decoder: *AnsiDecoder, data: []const u8) FeedResult {
        _ = decoder;
        // TODO: Properly implement ansi decoding here
        return FeedResult{
            .event = .{ .text = data },
            .count = data.len,
        };
    }
};

pub fn DecodingTerminal(comptime Decoder: type) type {
    return struct {
        const DT = @This();

        decoder: Decoder = .{},
        terminal: VirtualTerminal,

        pub fn init(allocator: std.mem.Allocator, width: u15, height: u15) !DT {
            return DT{
                .decoder = .{},
                .terminal = try VirtualTerminal.init(allocator, width, height),
            };
        }

        pub fn deinit(dt: *DT, allocator: std.mem.Allocator) void {
            dt.terminal.deinit(allocator);
            dt.* = undefined;
        }

        pub fn feed(dt: *DT, string: []const u8) void {
            var offset: usize = 0;
            while (offset < string.len) {
                const element = dt.decoder.feed(string[offset..]);
                if (element.event) |event| {
                    switch (event) {
                        .text => |val| dt.terminal.write(val),
                        .command => |val| dt.terminal.execute(val),
                    }
                }
                offset += element.count;
            }
        }
    };
}

test "basic instance creation/destruction" {
    const Term = DecodingTerminal(AnsiDecoder);

    var term = try Term.init(std.testing.allocator, 80, 25);
    defer term.deinit(std.testing.allocator);

    term.feed("");

    term.terminal.page.clear(Char{
        .codepoint = ' ',
        .attributes = .{
            .foreground = .red,
            .background = .blue,
            .blink = false,
            .underline = false,
        },
    });
    term.terminal.page.resize(40, 10);
}

test "clear command" {
    var term = try VirtualTerminal.init(std.testing.allocator, 80, 25);
    defer term.deinit(std.testing.allocator);

    term.execute(.clear_screen);

    for (0..term.page.height) |y| {
        const row = term.page.row(y);
        for (row) |c| {
            try std.testing.expect(c.codepoint == ' ');
        }
    }
}

test "write basic text" {
    var term = try VirtualTerminal.init(std.testing.allocator, 80, 25);
    defer term.deinit(std.testing.allocator);

    term.execute(.clear_screen);

    const string = "Hello, World!";

    term.write(string);

    for (string, term.page.row(0)[0..string.len]) |expected, actual| {
        try std.testing.expectEqual(@as(u24, expected), actual.codepoint);
    }
    try std.testing.expectEqual(@as(u24, ' '), term.page.row(0)[string.len].codepoint);
}

test "cursor modifications" {
    var term = try VirtualTerminal.init(std.testing.allocator, 80, 25);
    defer term.deinit(std.testing.allocator);

    term.execute(.clear_screen);

    try std.testing.expectEqual(@as(u16, 0), term.state.cursor.column);
    try std.testing.expectEqual(@as(u16, 0), term.state.cursor.row);

    term.write("12345");

    try std.testing.expectEqual(@as(u16, 5), term.state.cursor.column);
    try std.testing.expectEqual(@as(u16, 0), term.state.cursor.row);

    term.write("123");

    try std.testing.expectEqual(@as(u16, 8), term.state.cursor.column);
    try std.testing.expectEqual(@as(u16, 0), term.state.cursor.row);

    term.execute(.line_feed);
    term.execute(.line_feed);

    try std.testing.expectEqual(@as(u16, 8), term.state.cursor.column);
    try std.testing.expectEqual(@as(u16, 2), term.state.cursor.row);

    term.execute(.carriage_return);

    try std.testing.expectEqual(@as(u16, 0), term.state.cursor.column);
    try std.testing.expectEqual(@as(u16, 2), term.state.cursor.row);

    term.write("123");

    try std.testing.expectEqual(@as(u16, 3), term.state.cursor.column);
    try std.testing.expectEqual(@as(u16, 2), term.state.cursor.row);

    term.execute(.new_line);

    try std.testing.expectEqual(@as(u16, 0), term.state.cursor.column);
    try std.testing.expectEqual(@as(u16, 3), term.state.cursor.row);

    term.execute(.{ .set_cursor = Point.new(1, 2) });

    try std.testing.expectEqual(@as(u16, 1), term.state.cursor.column);
    try std.testing.expectEqual(@as(u16, 2), term.state.cursor.row);

    term.write("123");

    try std.testing.expectEqual(@as(u16, 4), term.state.cursor.column);
    try std.testing.expectEqual(@as(u16, 2), term.state.cursor.row);

    for ("123", term.page.row(2)[1..4]) |e, a| {
        try std.testing.expectEqual(@as(u24, e), a.codepoint);
    }

    term.execute(.clear_line);

    for ("     ", term.page.row(2)[0..5]) |e, a| {
        try std.testing.expectEqual(@as(u24, e), a.codepoint);
    }

    try std.testing.expectEqual(@as(u16, 0), term.state.cursor.column);
    try std.testing.expectEqual(@as(u16, 2), term.state.cursor.row);
}

fn rewriteAttribute(original: Attributes, comptime field: std.meta.FieldEnum(Attributes), value: std.meta.fieldInfo(Attributes, field).type) Attributes {
    var changed = original;
    @field(changed, @tagName(field)) = value;
    return changed;
}

test "attribute propagation" {
    var term = try VirtualTerminal.init(std.testing.allocator, 80, 25);
    defer term.deinit(std.testing.allocator);

    const base_attribute = Attributes{
        .foreground = .white,
        .background = .black,
        .blink = false,
        .underline = false,
    };

    term.setAttributes(base_attribute);

    term.write("abc");

    for (term.page.row(0)[0..3]) |char| {
        try std.testing.expectEqual(base_attribute, char.attributes);
    }

    term.execute(.{ .set_foreground = .red });

    const red_attribute = rewriteAttribute(base_attribute, .foreground, .red);

    term.write("def");

    for (term.page.row(0)[3..6]) |char| {
        try std.testing.expectEqual(red_attribute, char.attributes);
    }

    term.execute(.{ .set_background = .blue });

    const red_blue_attribute = rewriteAttribute(red_attribute, .background, .blue);

    term.write("ghi");

    for (term.page.row(0)[6..9]) |char| {
        try std.testing.expectEqual(red_blue_attribute, char.attributes);
    }
}

test "scrolling" {
    var term = try VirtualTerminal.init(std.testing.allocator, 100, 4);
    defer term.deinit(std.testing.allocator);

    const H = struct {
        fn expectLines(vt: *VirtualTerminal, lines: []const []const u8) !void {
            try std.testing.expectEqual(lines.len, vt.page.height);
            for (lines, 0..) |line, y| {
                const row = vt.page.row(y);

                for (line, row[0..line.len]) |e, a| {
                    try std.testing.expectEqual(@as(u24, e), a.codepoint);
                }
                for (row[line.len..]) |a| {
                    try std.testing.expectEqual(@as(u24, ' '), a.codepoint);
                }
            }
        }
    };

    term.execute(.clear_screen);

    try H.expectLines(&term, &.{ "", "", "", "" });

    term.write("line 1");
    term.execute(.new_line);

    try H.expectLines(&term, &.{ "line 1", "", "", "" });

    term.write("line 2");
    term.execute(.new_line);

    try H.expectLines(&term, &.{ "line 1", "line 2", "", "" });

    term.write("line 3");
    term.execute(.new_line);

    try H.expectLines(&term, &.{ "line 1", "line 2", "line 3", "" });

    term.write("line 4");

    try H.expectLines(&term, &.{ "line 1", "line 2", "line 3", "line 4" });

    term.execute(.new_line);

    try H.expectLines(&term, &.{ "line 2", "line 3", "line 4", "" });
}

test "auto line-wrap" {
    var term = try VirtualTerminal.init(std.testing.allocator, 10, 4);
    defer term.deinit(std.testing.allocator);

    const H = struct {
        fn expectLines(vt: *VirtualTerminal, lines: []const []const u8) !void {
            try std.testing.expectEqual(lines.len, vt.page.height);
            for (lines, 0..) |line, y| {
                const row = vt.page.row(y);

                for (line, row[0..line.len]) |e, a| {
                    try std.testing.expectEqual(@as(u24, e), a.codepoint);
                }
                for (row[line.len..]) |a| {
                    try std.testing.expectEqual(@as(u24, ' '), a.codepoint);
                }
            }
        }
    };

    term.execute(.clear_screen);

    try H.expectLines(&term, &.{ "", "", "", "" });

    term.write("01234");

    try H.expectLines(&term, &.{ "01234", "", "", "" });

    term.write("56789");

    try H.expectLines(&term, &.{ "0123456789", "", "", "" });

    term.write("abc");

    try H.expectLines(&term, &.{ "0123456789", "abc", "", "" });

    term.write("defghi");

    try H.expectLines(&term, &.{ "0123456789", "abcdefghi", "", "" });

    term.write("+-*/");

    try H.expectLines(&term, &.{ "0123456789", "abcdefghi+", "-*/", "" });
}
