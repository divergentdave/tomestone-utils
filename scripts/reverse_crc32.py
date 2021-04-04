#!/usr/bin/env python3
import z3

POLYNOMIAL = 0xEDB88320


def jamcrc(data):
    data_size = data.size()
    size = max(data_size, 32)
    if data_size < 32:
        data = z3.Concat(z3.BitVecVal(0, 32 - data_size), data)
    one = z3.BitVecVal(1, size)
    polynomial = z3.BitVecVal(POLYNOMIAL, size)
    crc = 0xffffffff
    for i in range(0, data_size, 8):
        crc = crc ^ (z3.LShR(data, i) & 0xff)
        for _ in range(8):
            shifted = z3.LShR(crc, 1)
            crc = z3.If(
                crc & one == one,
                shifted ^ polynomial,
                shifted
            )
    return crc


def bitvec_to_bytes(model, bv):
    output = bytearray()
    for i in range(0, bv.size(), 8):
        byte = z3.Extract(i + 7, i, bv)
        output.append(model.eval(byte).as_long())
    return bytes(output)


def gen_matches(prefix, suffix, byte_size, target):
    bit_size = byte_size * 8
    s = z3.Solver()
    data = z3.BitVec("data", bit_size)
    for i, char in enumerate(prefix):
        s.add(z3.Extract(i * 8 + 7, i * 8, data) == char)
    for i in range(byte_size - len(suffix), byte_size):
        char = suffix[i - (byte_size - len(suffix))]
        s.add(z3.Extract(i * 8 + 7, i * 8, data) == char)
    for i in range(byte_size):
        byte = z3.Extract(i * 8 + 7, i * 8, data)
        s.add(z3.Or(
            z3.And(byte >= 0x61, byte <= 0x7a),
            byte == 0x2d,
            byte == 0x2e,
            # byte == 0x2f,
            z3.And(byte >= 0x30, byte <= 0x39),
            byte == 0x5f,
        ))
    s.add(jamcrc(data) == target)
    while s.check() == z3.sat:
        m = s.model()
        yield bitvec_to_bytes(m, data)
        s.add(m[data] != data)


def test():
    exd_int = ord("e") | (ord("x") << 8) | (ord("d") << 16)
    exd_bvv = z3.BitVecVal(exd_int, 24)
    expected_crc = 0xe39b7999
    actual_crc = z3.simplify(jamcrc(exd_bvv)).as_long()
    assert actual_crc == expected_crc


def main():
    test()

    prefix = b"fesbkc002btl_031"
    suffix = b".luab"
    max_length = 23
    target = 0x0481e34a
    for length in range(max(1, len(prefix) + len(suffix)), max_length + 1):
        for match in gen_matches(prefix, suffix, length, target):
            print(match.decode("ascii"))


if __name__ == "__main__":
    main()
