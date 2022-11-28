const std = @import("std");
const Allocator = std.mem.Allocator;

const Char = extern struct {
    char: u16,
    flags: Attributes,
};

const Attributes = packed struct(u16) {
    fg: Color = .default,
    bg: Color = .default,
    bold: bool = false,
    dim: bool = false,
    underline: bool = false,
    blink: bool = false,
    reverse: bool = false,
    invisible: bool = false,
    _pad: u2 = 0,
};

const Color = enum(u4) {
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    default,
};

const Point = extern struct {
    row: u16,
    column: u16,

    fn new(r: u16, c: u16) Point {
        return Point{ .row = r, .column = c };
    }
};

const Page = extern struct {
    chars: [*]Char,
    size: Point,
    max_size: Point,

    fn create(allocator: Allocator, size: Point, max_size: Point) !Page {
        if (size.row > max_size.row or
            size.column > max_size.column) return error.SizeTooBig;
        const chars = try allocator.alloc(Char, max_size.column * max_size.row);
        return Page{
            .chars = chars.ptr,
            .size = size,
            .max_size = max_size,
        };
    }

    fn resize(self: *Page, new_size: Point) !void {
        if (new_size.row > self.max_size.row or
            new_size.column > self.max_size.column) return error.SizeTooBig;
        self.size = new_size;
    }

    fn free(self: *Page, allocator: Allocator) void {
        allocator.free(self.chars[0 .. self.max_size.column * self.max_size.row]);
    }
};

const Terminal = struct {
    page: Page,
    dirty_lines: std.DynamicBitSetUnmanaged,
    cursor: Point,

    fn open(allocator: Allocator, size: Point, max_size: Point) !*Terminal {
        var terminal = try allocator.create(Terminal);
        terminal.dirty_lines = try std.DynamicBitSetUnmanaged.initEmpty(allocator, max_size.row);
        errdefer terminal.dirty_lines.deinit(allocator);
        terminal.page = try Page.create(allocator, size, max_size);
        terminal.cursor = Point.new(0, 0);
        return terminal;
    }

    fn close(self: *Terminal, allocator: Allocator) void {
        self.page.free(allocator);
        self.dirty_lines.deinit(allocator);
        allocator.destroy(self);
    }

    fn resize(self: *Terminal, new_size: Point) !void {
        if (self.page.size.row > new_size.row) {
            self.dirty_lines.setRangeValue(.{
                .start = self.page.size.row,
                .end = new_size.row,
            }, false);
        }
        try self.page.resize(new_size);

        self.fixcursor();
    }

    fn fixcursor(self: *Terminal) void {
        if (self.cursor.row > self.page.size.row - 1) {
            self.cursor.row = self.page.size.row - 1;
        }
        if (self.cursor.column > self.page.size.column - 1) {
            self.cursor.column = self.page.size.column - 1;
        }
    }

    fn clean(self: *Terminal) void {
        self.dirty_lines.setRangeValue(.{
            .start = 0,
            .end = self.dirty_lines.bit_length,
        }, false);
    }
    fn reset(self: *Terminal) void {}

    fn write(self: *Terminal, data: []const u8) void {}
};

test "Page create and free" {
    const alloc = std.testing.allocator;
    var page = try Page.create(alloc, .{ .row = 1, .column = 10 }, .{ .row = 2, .column = 11 });
    page.free(alloc);
}
