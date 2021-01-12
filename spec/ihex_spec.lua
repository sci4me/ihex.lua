local bit = require "bit32"

math.randomseed(os.time())

-- TODO: more thorough test suite!

describe("ihex.lua", function()
    local ihex

    setup(function()
        ihex = require "ihex"
    end)

    teardown(function()
        ihex = nil
    end)

    it("loads", function()
        assert(type(ihex) == "table", "expected table, got " .. type(ihex))
        assert(type(ihex.DEFAULT_DECODE_OPTIONS) == "table", "expected table, got " .. type(ihex.DEFAULT_DECODE_OPTIONS))
        assert(type(ihex.DEFAULT_ENCODE_OPTIONS) == "table", "expected table, got " .. type(ihex.DEFAULT_ENCODE_OPTIONS))
        assert(type(ihex.decode) == "function", "expected function, got " .. type(ihex.decode))
        assert(type(ihex.encode) == "function", "expected function, got " .. type(ihex.encode))
    end)

    local N_TEST_FILES = 5

    describe("decodes generated test files correctly", function()
        for i = 1, N_TEST_FILES do
            it("test_" .. i, function()
                local hex_path = "spec/test_files/test_" .. i .. ".hex"
                local bin_path = "spec/test_files/test_" .. i .. ".bin"
                
                local hex_fh = io.open(hex_path, "r")
                if not hex_fh then
                    error("file not found: " .. hex_path)
                end
                local hex = hex_fh:read("*a")
                hex_fh:close()

                local result = ihex.decode(hex)

                local bin_fh = io.open(bin_path, "rb")
                if not bin_fh then
                    error("file not found: " .. bin_path)
                end
                local n = 0
                while true do
                    local x = bin_fh:read(1)
                    if not x then
                        assert(n == result.count, "expected " .. n .. " bytes, got " .. #result)
                        break
                    end
                    assert(result[n] == string.byte(x))
                    n = n + 1
                end
            end)
        end
    end)

    describe("encodes generated test files correctly", function()
        for i = 1, N_TEST_FILES do
            it("test_" .. i, function()
                local hex_path = "spec/test_files/test_" .. i .. ".hex"
                local bin_path = "spec/test_files/test_" .. i .. ".bin"

                local bin_fh = io.open(bin_path, "rb")
                local data = {}
                while true do
                    local x = bin_fh:read(1)
                    if not x then
                        break
                    end
                    data[#data + 1] = string.byte(x)
                end
                bin_fh:close()

                local hex = ihex.encode(data, { lineBreakAtEndOfFile = true })

                local hex_fh = io.open(hex_path, "r")
                local expected = hex_fh:read("*a")
                hex_fh:close()

                assert(hex == expected)
            end)
        end
    end)

    it("encodes large data correctly", function()
        local N = 0x30000

        local data = {}
        for i = 1, N do
            data[i] = bit.band(i, 0xFF)
        end
        local hex = ihex.encode(data)
        local bin = ihex.decode(hex)
        for i = 1, N do
            local x = bin[i - 1]
            local y = bit.band(i, 0xFF)
            assert(x == y, string.format("expected %02X at address %08X, got %02X", y, i, x))
        end
    end)
end)