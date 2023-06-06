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
        _ = proc;
        _ = string;
    }

    pub fn execute(proc: *Processor, cmd: Command) void {
        _ = proc;
        _ = cmd;
    }
};

pub const Page = struct {
    chars: [*]Char,
    stride: usize,
    width: u16,
    height: u16,
    max_width: u16,
    max_height: u16,

    pub fn alloc(allocator: std.mem.Allocator, max_width: usize, max_height: usize) !Page {
        return Page{
            .chars = (try allocator.alloc(Char, max_width * max_height)).ptr,
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

    pub fn resize(page: *Page, width: usize, height: usize) void {
        page.width = @min(width, page.max_width);
        page.height = @min(height, page.max_height);
    }

    pub fn clear(page: *Page, char: Char) void {
        for (0..page.height) |y| {
            @memset((page.chars + page.stride * y)[0..page.width], char);
        }
    }
};

pub const Point = struct {
    row: u16,
    column: u16,

    pub fn new(x: usize, y: usize) Point {
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
    intermediate_buffer: std.BoundedArray(u8, 32), // max limit

    const FeedResult = struct { count: usize, event: ?StreamEvent };
    pub fn feed(decoder: *AnsiDecoder, data: []const u8) FeedResult {
        _ = decoder;
        _ = data;
    }
};
