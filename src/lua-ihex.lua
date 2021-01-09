--- Intel Hex encoder/decoder.
--
-- An opinionated, mostly-feature-complete,
-- probably-not-that-buggy Intel Hex encoding
-- and decoding utility library for Lua.
--
-- `lua-ihex` only has one dependency: `bit32`.
-- Support for using `lua-ihex` without `bit32`
-- may be added in the future.
--
-- @module lua-ihex
-- @author sci4me
-- @license MIT
-- @copyright Scitoshi Nakayobro 2021

local bit = require "bit32"

local HEX_DECODE_LUT = {
    ['0'] = 0x0, ['1'] = 0x1, ['2'] = 0x2, ['3'] = 0x3,
    ['4'] = 0x4, ['5'] = 0x5, ['6'] = 0x6, ['7'] = 0x7,
    ['8'] = 0x8, ['9'] = 0x9, ['a'] = 0xA, ['A'] = 0xA,
    ['b'] = 0xB, ['B'] = 0xB, ['c'] = 0xC, ['C'] = 0xC,
    ['d'] = 0xD, ['D'] = 0xD, ['e'] = 0xE, ['E'] = 0xE,
    ['f'] = 0xF, ['F'] = 0xF
}

local REC_DATA                     = 0x00
local REC_EOF                      = 0x01
local REC_EXTENDED_SEGMENT_ADDRESS = 0x02
local REC_START_SEGMENT_ADDRESS    = 0x03
local REC_EXTENDED_LINEAR_ADDRESS  = 0x04
local REC_START_LINEAR_ADDRESS     = 0x05

local function is_int(x)
    assert(type(x) == "number", "expected number, got " .. type(x))
    return x == math.floor(x)
end

local function is_newline(c)
    return c == '\r' or c == '\n'
end

--- Default decoding options to be used
-- if no options are specified for the
-- `decode` function.
-- @table DEFAULT_DECODE_OPTIONS
-- @field[opt=false] skipNonColonLines If set to true, `decode`
-- will ignore lines that do not begin with a `:`
-- @field[opt=false] allowOverwrite If set to true, `decode`
-- will not raise an error if the parsed Intel Hex
-- data specifies a data byte at the same address
-- multiple times.
-- @field[opt=true] allowExtendedSegmentAddress If set to true,
-- `decode` will not raise an error if the parsed
-- Intel Hex data contains an extended segment address
-- record.
-- @field[opt=false] allowStartSegmentAddress If set to true,
-- `decode` will not raise an error if the parsed
-- Intel Hex data contains a start segment address
-- record.
-- @field[opt=true] allowExtendedLinearAddress If set to true,
-- `decode` will not raise an error if the parsed
-- Intel Hex data contains an extended linear address
-- record.
-- @field[opt=false] allowStartLinearAddress If set to true,
-- `decode` will not raise an error if the parsed
-- Intel Hex data contains a start linear address
-- record.
local DEFAULT_DECODE_OPTIONS = {
    skipNonColonLines              = false,
    allowOverwrite                 = false,
    allowExtendedSegmentAddress    = true,
    allowStartSegmentAddress       = false,
    allowExtendedLinearAddress     = true,
    allowStartLinearAddress        = false
}

--- Default encoding options to be used
-- if no options are specified for the
-- `encode` function.
-- @table DEFAULT_ENCODE_OPTIONS
-- @field[opt=0x20] bytesPerLine The maximum
-- number of bytes to be encoded per line.
-- @field[opt=true] upperCaseHex If set to true,
-- `encode` will use upper-case hex digits.
-- @field[opt=false] crlf If set to true,
-- `encode` will use Windows-style line breaks
-- (`\r\n`). Otherwise, the correct style of line
-- break will be used (`\n`).
-- @field[opt=false] lineBreaksAtEndOfFile If set
-- to true, `encode` will insert a line break after
-- the last line (EOF record).
local DEFAULT_ENCODE_OPTIONS = {
    bytesPerLine                   = 0x20,
    upperCaseHex                   = true,
    crlf                           = false,
    lineBreakAtEndOfFile           = false
}

--- Decode an Intel Hex encoded string to a byte array.
-- The returned table is an array of decoded bytes,
-- which may or may not be contiguous. Intel Hex allows
-- for specifying disjoint blocks of data, therefore,
-- there may be indices between blocks of valid data
-- that have the value `nil`.
--
-- The first index that may contain data is `0`. Since
-- Lua made the mistake of using `1`-based indexing,
-- the length of the table will be off by one.
-- Therefore, the resultant table will also contain
-- the field `count` which contains the number of data
-- bytes specified by the input string.
--
-- This function allows an optional second argument
-- which must be a table containing options specifying
-- how the input string should be interpreted.
-- If no second argument is specified, the default
-- options (`DEFAULT_DECODE_OPTIONS`) will be used.
--
-- This function can optionally handle the
-- start segment address and start linear address
-- Intel Hex record types.
--
-- If start segment address records are enabled in
-- the options table and the input string contains
-- such a record, the table indices `CS` and `IP` will
-- be set.
-- To enable start segment address records, set
-- `allowStartSegmentAddress` to true in the options
-- table.
--
-- If start linear address records are enabled in
-- the options table and the input string contains
-- such a record, the table index `EIP` will be set.
-- To enable start linear address records, set
-- `allowStartLinearAddress` to true in the options
-- table.
--
-- @tparam string str A string containing data encoded
-- in the Intel Hex format.
-- @tparam[opt] table options A table specifying how the input
-- string should be interpreted. see @{DEFAULT_DECODE_OPTIONS}
-- @treturn table A table containing each data byte specified
-- by the input string, indexed by its respective numeric address.
-- This table also contains the field `count` which is the
-- total number of data bytes decoded from the input string.
local function decode(str, options)
    if options then
        if type(options) ~= "table" then
            error("expected table, got " .. type(options))
        end
    else
        options = {}
    end

    local skipNonColonLines = options.skipNonColonLines and options.skipNonColonLines or DEFAULT_DECODE_OPTIONS.skipNonColonLines
    local allowOverwrite = options.allowOverwrite and options.allowOverwrite or DEFAULT_DECODE_OPTIONS.allowOverwrite
    local allowExtendedSegmentAddress = options.allowExtendedSegmentAddress and options.allowExtendedSegmentAddress or DEFAULT_DECODE_OPTIONS.allowExtendedSegmentAddress
    local allowStartSegmentAddress = options.allowStartSegmentAddress and options.allowStartSegmentAddress or DEFAULT_DECODE_OPTIONS.allowStartSegmentAddress
    local allowExtendedLinearAddress = options.allowExtendedLinearAddress and options.allowExtendedLinearAddress or DEFAULT_DECODE_OPTIONS.allowExtendedLinearAddress
    local allowStartLinearAddress = options.allowStartLinearAddress and options.allowStartLinearAddress or DEFAULT_DECODE_OPTIONS.allowStartLinearAddress

    local index = 1
    local len   = str:len()
    local sum   = 0

    local function more()
        return index <= len
    end

    local function peek()
        return str:sub(index, index)
    end

    local function next()
        assert(more(), "unexpected end of data")
        local c = peek()
        index = index + 1
        return c
    end

    local function skip_newline()
        local a = next()
        if not is_newline(a) then
            error("expected line feed or carriage return")
        end
        if a == '\r' and peek() == '\n' then
            next()
        end
    end

    local function u1()
        local chi = next()
        local clo = next()
        local hi = HEX_DECODE_LUT[chi]
        local lo = HEX_DECODE_LUT[clo]
        if not hi then
            error("expected hex digit, got '" .. tostring(chi) .. "'")
        end
        if not lo then
            error("expected hex digit, got '" .. tostring(clo) .. "'")
        end
        local x = 16 * hi + lo
        sum = sum + x
        return x
    end

    local function u2()
        local hi = u1()
        local lo = u1()
        return 256 * hi + lo
    end

    local function verify_checksum()
        local computed_checksum = bit.band(0x100 - sum, 0xFF)
        local checksum = u1()
        if computed_checksum ~= checksum then
            error(string.format("bad checksum; expected %02X, got %02X", checksum, computed_checksum))
        end
    end

    local result = {}
    local count = 0
    local base = 0
    local eof = false

    while more() do
        local skip = false
        local sc = next()
        if sc ~= ':' then
            print(skipNonColonLines)
            if skipNonColonLines then
                while not is_newline(peek()) do
                    next()
                end
                skip_newline()
                skip = true
            else
                if sc == '\r' then
                    sc = "\\r"
                elseif sc == '\n' then
                    sc = "\\n"
                end
                error("expected : at beginning of line, got '" .. sc .. "'")
            end
        end

        if not skip then
            sum = 0
            local nbytes = u1()
            local addr = u2()
            local type = u1()

            if type == REC_DATA then
                for i = 0, nbytes-1 do
                    local actual_addr = base + bit.band(addr + i, 0xFFFF)
                    local x = u1()
                    if not allowOverwrite and result[actual_addr] then
                        error(string.format("unexpected overwrite at address %08X", actual_addr))
                    end
                    result[actual_addr] = x
                    count = count + 1
                end
                verify_checksum()
            elseif type == REC_EOF then
                assert(u1() == 0xFF)
                eof = true
                break
            elseif type == REC_EXTENDED_SEGMENT_ADDRESS then
                assert(allowExtendedSegmentAddress, "unexpected extended segment address record")
                assert(nbytes == 2, "extended segment address record must contain 2 bytes, got " .. nbytes)
                base = bit.lshift(u2(), 4)
                verify_checksum()
            elseif type == REC_START_SEGMENT_ADDRESS then
                assert(allowStartSegmentAddress, "unexpected start segment address record")
                assert(addr == 0, string.format("start segment address record address must be 0x0000, got %04X", addr))
                assert(nbytes == 4, "start segment address record must contain 4 bytes, got " .. nbytes)
                result.CS = u2()
                result.IP = u2()
                verify_checksum()
            elseif type == REC_EXTENDED_LINEAR_ADDRESS then
                assert(allowExtendedLinearAddress, "unexpected extended linear address record")
                assert(nbytes == 2, "extended linear address record must contain 2 bytes, got " .. nbytes)
                base = bit.lshift(u2(), 16)
                verify_checksum()
            elseif type == REC_START_LINEAR_ADDRESS then
                assert(allowStartLinearAddress, "unexpected start linear address record")
                assert(addr == 0, string.format("start linear address record address must be 0x0000, got %04X", addr))
                assert(nbytes == 4, "start linear address record must contain 4 bytes, got " .. nbytes)
                result.EIP = bit.lshift(u2(), 16) + u2()
                verify_checksum()
            else
                error(string.format("unexpected record type %02X", type))
            end

            skip_newline()
        end
    end

    if not eof then
        assert(more(), "unexpected end of data; expected EOF record")
    end

    result.count = count

    return result
end

--- Encode a byte array in the Intel Hex format.
-- Unlike the `decode` function, `encode` uses `1`-based indexing!
-- In the future, support may be added to allow for the correct
-- indexing style to be used.
--
-- This function will encode a byte array starting at index `1`
-- and ending at index `#data` in the Intel Hex format and return
-- it as a string.
--
-- Currently `lua-ihex` does not support encoding disjoint blocks
-- of data, as well as start segment address and start linear address
-- records. Support for these features may be added in the future.
--
-- This function allows an optional second argument which must be
-- a table containing options specifying how the input data
-- should be encoded.
-- If no second argument is specified, the default options
-- (`DEFAULT_ENCODE_OPTIONS`) will be used.
--
-- @tparam table data An array of unsigned bytes to encode.
-- @tparam[opt] table options A table specifying how the input
-- data should be encoded.
-- @treturn string The Intel Hex encoded representation of the
-- input byte array.
local function encode(data, options)
    if type(data) ~= "table" then
        error("expected table, got " .. type(data))
    end

    if options then
        if type(options) ~= "table" then
            error("expected table, got " .. type(options))
        end
    else
        options = {}
    end

    local bytesPerLine         = options.bytesPerLine and options.bytesPerLine or DEFAULT_ENCODE_OPTIONS.bytesPerLine
    local upperCaseHex         = options.upperCaseHex and options.upperCaseHex or DEFAULT_ENCODE_OPTIONS.upperCaseHex
    local crlf                 = options.crlf and options.crlf or DEFAULT_ENCODE_OPTIONS.crlf
    local lineBreakAtEndOfFile = options.lineBreakAtEndOfFile and options.lineBreakAtEndOfFile or DEFAULT_ENCODE_OPTIONS.lineBreakAtEndOfFile

    if not is_int(bytesPerLine) then
        error("bytesPerLine must be an integer, got " .. tostring(bytesPerLine))
    end
    if bytesPerLine < 1 then
        error("bytesPerLine must be >= 1")
    end
    if bytesPerLine > 255 then
        error("bytesPerLine must be <= 255")
    end

    local result = {}
    local sum = 0
    local index = 1
    local bytesLeft = #data
    local addr = 0

    local function emit(x)
        assert(type(x) == "string")
        result[#result + 1] = x
    end

    local u1
    if upperCaseHex then
        u1 = function(x)
            assert(is_int(x))
            assert(x >= 0)
            assert(x <= 255)
            emit(string.format("%02X", x))
            sum = sum + x
        end
    else
        u1 = function(x)
            assert(is_int(x))
            assert(x >= 0)
            assert(x <= 255)
            emit(string.format("%02x", x))
            sum = sum + x
        end
    end

    local function u2(x)
        assert(is_int(x))
        u1(bit.rshift(bit.band(x, 0xFF00), 8))
        u1(bit.band(x, 0xFF))
    end

    local function write_checksum()
        u1(bit.band(0x100 - sum, 0xFF))
        sum = 0
    end

    local function line_break()
        if crlf then
            emit("\r\n")
        else
            emit("\n")
        end
    end

    while bytesLeft > 0 do
        emit(':')

        local nbytes = math.min(bytesPerLine, bytesLeft)
        u1(nbytes)

        -- @todo write extended linear address record if needed! otherwise
        -- this function will only be able to encode up to 64k bytes!

        u2(addr)
        
        u1(REC_DATA)
        for _ = 1, nbytes do
            local x = data[index]
            index = index + 1
            u1(x)
        end
        bytesLeft = bytesLeft - nbytes
        addr = addr + nbytes

        write_checksum()

        line_break()
    end

    emit(":00000001FF")

    if lineBreakAtEndOfFile then
        line_break()
    end

    return table.concat(result)
end

-- NOTE: Workaround to get LDoc not to complain about this; not sure what
-- the proper way to fix this would be; can't find an answer online. :/
local _LICENSE = [[
    MIT License

    Copyright (c) 2021 Scitoshi Nakayobro

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
]]

--- @export DEFAULT_DECODE_OPTIONS
-- @export DEFAULT_ENCODE_OPTIONS
-- @export decode
-- @export encode
return {
    _DESCRIPTION = "Intel Hex encoder/decoder",
    _URL = "https://github.com/sci4me/lua-ihex",
    _VERSION = "lua-ihex 0.1.0",
    _LICENSE = _LICENSE,
    DEFAULT_DECODE_OPTIONS = DEFAULT_DECODE_OPTIONS,
    DEFAULT_ENCODE_OPTIONS = DEFAULT_ENCODE_OPTIONS,
    decode                 = decode,
    encode                 = encode
}