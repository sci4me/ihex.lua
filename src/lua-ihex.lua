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

-- TODO: Don't know how this slipped my mind: can't we
-- use string.pack/string.unpack if they're available?
-- I don't _actually_ know, gotta double-check the
-- reference; I haven't really used that API.
--
-- Okay actually, probably not? I guess. :/
-- Unless there's a way (that would be worth it)
-- that I'm not thinking of right now. Hmh.
--
-- I mean, we could use it I guess, for encoding
-- binary data to/from strings, it's just that
-- it won't handle the ASCII conversion for us.
-- So, I'm not sure if it would really provied
-- any benefit at that point. Will have to mess
-- around with it!
--              - sci4me, 1/9/21

local bit
if jit then
    bit = require "bit"
else
    bit = require "bit32"
end

local band   = bit.band
local lshift = bit.lshift
local rshift = bit.rshift
local tohex  = bit.tohex
local strfmt = string.format
local substr = string.sub
local min    = math.min

-- TODO: @Speed instead of converting the hex digits to a number and comparing
-- with these constants, as numbers, just compare them as strings! Duh!
local REC_DATA                     = 0x00
local REC_EOF                      = 0x01
local REC_EXTENDED_SEGMENT_ADDRESS = 0x02
local REC_START_SEGMENT_ADDRESS    = 0x03
local REC_EXTENDED_LINEAR_ADDRESS  = 0x04
local REC_START_LINEAR_ADDRESS     = 0x05

local function option(opts, name, defaults)
    return opts[name] and opts[name] or defaults[name]
end

-- TODO: Add an option to default the values inbetween
-- disjoint blocks of data to 0 or perhaps to any byte.

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
        assert(type(options) == "table", "expected table, got " .. type(options))
    else
        options = {}
    end

    local skipNonColonLines           = option(options, "skipNonColonLines", DEFAULT_DECODE_OPTIONS)
    local allowOverwrite              = option(options, "allowOverwrite", DEFAULT_DECODE_OPTIONS)
    local allowExtendedSegmentAddress = option(options, "allowExtendedSegmentAddress", DEFAULT_DECODE_OPTIONS)
    local allowStartSegmentAddress    = option(options, "allowStartSegmentAddress", DEFAULT_DECODE_OPTIONS)
    local allowExtendedLinearAddress  = option(options, "allowExtendedLinearAddress", DEFAULT_DECODE_OPTIONS)
    local allowStartLinearAddress     = option(options, "allowStartLinearAddress", DEFAULT_DECODE_OPTIONS)

    local index = 1
    local len   = str:len()
    local sum   = 0

    local function next()
        if index > len then error("unexpected end of data") end
        local c = substr(str, index, index)
        index = index + 1
        return c
    end

    local function skip_newline()
        local a = next()
        assert(a == '\r' or a == '\n', "expected line feed or carriage return")
        if a == '\r' and substr(str, index, index) == '\n' then
            index = index + 1
        end
    end

    local function u1()
        if index + 1 > len then error("unexpected end of data") end
        local y = tonumber(substr(str, index, index + 1), 16)
        index = index + 2
        sum = sum + y
        return y
    end

    local function u2()
        -- TODO: Can this somehow be further optimized?
        if index + 3 > len then error("unexpected end of data") end
        local hi = tonumber(substr(str, index, index + 1), 16)
        local lo = tonumber(substr(str, index + 2, index + 3), 16)
        index = index + 4
        sum = sum + hi + lo
        return lshift(hi, 8) + lo
    end

    local function verify_checksum()
        local computed_checksum = band(0x100 - sum, 0xFF)
        local checksum = u1()
        if computed_checksum ~= checksum then
            error(strfmt("bad checksum; expected %02X, got %02X", checksum, computed_checksum))
        end
    end

    local result = {}
    local count = 0
    local base = 0
    local eof = false

    while index <= len do
        local skip = false
        local sc = next()
        if sc ~= ':' then
            if skipNonColonLines then
                sc = substr(str, index, index)
                while index <= len and not (sc == '\r' or sc == '\n') do
                    index = index + 1
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
            -- TODO: Would be interesting to see how the profiling results
            -- compare between doing u1, u2, u1 vs. doing u4 and then using
            -- bit to pull the separate data fields out of the 32-bit number.
            local nbytes = u1()
            local addr = u2()
            local type = u1()

            if type == REC_DATA then
                -- TODO: Optimize this loop
                for i = 0, nbytes-1 do
                    local actual_addr = base + band(addr + i, 0xFFFF)
                    local x = u1()
                    if not allowOverwrite and result[actual_addr] then
                        error(strfmt("unexpected overwrite at address %08X", actual_addr))
                    end
                    result[actual_addr] = x
                    count = count + 1
                end
            elseif type == REC_EOF then
                local checksum = u1()
                assert(checksum == 0xFF, "expected checksum of EOF record to be 0xFF, got " .. checksum)
                eof = true
                break
            elseif type == REC_EXTENDED_SEGMENT_ADDRESS then
                assert(allowExtendedSegmentAddress, "unexpected extended segment address record")
                assert(nbytes == 2, "extended segment address record must contain 2 bytes, got " .. nbytes)
                base = lshift(u2(), 4)
            elseif type == REC_START_SEGMENT_ADDRESS then
                assert(allowStartSegmentAddress, "unexpected start segment address record")
                assert(addr == 0, strfmt("start segment address record address must be 0x0000, got %04X", addr))
                assert(nbytes == 4, "start segment address record must contain 4 bytes, got " .. nbytes)
                result.CS = u2()
                result.IP = u2()
            elseif type == REC_EXTENDED_LINEAR_ADDRESS then
                assert(allowExtendedLinearAddress, "unexpected extended linear address record")
                assert(nbytes == 2, "extended linear address record must contain 2 bytes, got " .. nbytes)
                base = lshift(u2(), 16)
            elseif type == REC_START_LINEAR_ADDRESS then
                assert(allowStartLinearAddress, "unexpected start linear address record")
                assert(addr == 0, strfmt("start linear address record address must be 0x0000, got %04X", addr))
                assert(nbytes == 4, "start linear address record must contain 4 bytes, got " .. nbytes)
                result.EIP = lshift(u2(), 16) + u2()
            else
                error(strfmt("unexpected record type %02X", type))
            end

            verify_checksum()
            skip_newline()
        end
    end

    assert(eof, "unexpected end of data; expected EOF record")

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
        assert(type(options) == "table", "expected table, got " .. type(options))
    else
        options = {}
    end

    local bytesPerLine         = option(options, "bytesPerLine", DEFAULT_ENCODE_OPTIONS)
    local upperCaseHex         = option(options, "upperCaseHex", DEFAULT_ENCODE_OPTIONS)
    local crlf                 = option(options, "crlf", DEFAULT_ENCODE_OPTIONS)
    local lineBreakAtEndOfFile = option(options, "lineBreakAtEndOfFile", DEFAULT_ENCODE_OPTIONS)

    local line_break = crlf and "\r\n" or "\n"

    assert(bytesPerLine % 1 == 0, "bytesPerLine must be an integer, got " .. tostring(bytesPerLine))
    assert(bytesPerLine >= 1, "bytesPerLine must be >= 1")
    assert(bytesPerLine <= 255, "bytesPerLine must be <= 255")

    local result = {}
    local sum = 0
    local index = 1
    local bytesLeft = #data
    local addr = 0
    local upper = 0

    local u1, u2, u1format, u2format
    if jit then
        u1format = upperCaseHex and -2 or 2
        u2format = upperCaseHex and -4 or 4

        u1 = function(x)
            result[#result + 1] = tohex(x, u1format)
            sum = sum + x
        end

        u2 = function(x)
            result[#result + 1] = tohex(x, u2format)
            local hi = rshift(band(x, 0xFF00), 8)
            local lo = band(x, 0xFF)
            sum = sum + hi + lo
        end
    else
        u1format = upperCaseHex and "%02X" or "%02x"
        u2format = upperCaseHex and "%04X" or "%04x"
        
        u1 = function(x)
            result[#result + 1] = strfmt(u1format, x)
            sum = sum + x
        end

        u2 = function(x)
            result[#result + 1] = strfmt(u2format, x)
            local hi = rshift(band(x, 0xFF00), 8)
            local lo = band(x, 0xFF)
            sum = sum + hi + lo
        end
    end

    while bytesLeft > 0 do
        local up = rshift(band(addr, 0xFFFF0000), 16)
        if up ~= upper then
            upper = up

            result[#result + 1] = ':020000'
            sum = sum + 2
            u1(REC_EXTENDED_LINEAR_ADDRESS)
            u2(up)
            u1(band(0x100 - sum, 0xFF))
            sum = 0
            result[#result + 1] = line_break
        end

        result[#result + 1] = ':'

        local nbytes = min(bytesPerLine, bytesLeft)
        u1(nbytes)
        u2(band(addr, 0xFFFF))
        u1(REC_DATA)
        -- TODO: Optimize this loop
        for _ = 1, nbytes do
            local x = data[index]
            if x % 1 ~= 0 then error("expected integer, got float") end
            if x < 0 or x > 0xFF then error("expected number between [0,FF], got " .. x) end
            index = index + 1
            u1(x)
        end
        bytesLeft = bytesLeft - nbytes
        addr = addr + nbytes

        u1(band(0x100 - sum, 0xFF))
        sum = 0
        result[#result + 1] = line_break
    end

    result[#result + 1] = ":00000001FF"

    if lineBreakAtEndOfFile then
        result[#result + 1] = line_break
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
    _DESCRIPTION           = "Intel Hex encoder/decoder",
    _URL                   = "https://github.com/sci4me/lua-ihex",
    _VERSION               = "lua-ihex 0.1.1",
    _LICENSE               = _LICENSE,
    DEFAULT_DECODE_OPTIONS = DEFAULT_DECODE_OPTIONS,
    DEFAULT_ENCODE_OPTIONS = DEFAULT_ENCODE_OPTIONS,
    decode                 = decode,
    encode                 = encode
}