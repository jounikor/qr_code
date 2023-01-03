#
# (c) 2023 Jouni Korhonen
#
#

class galois_field_256(object):
    """
    There are excellent articles about Galois Fields:
    https://en.wikipedia.org/wiki/Finite_field
    https://zavier-henry.medium.com/an-introductory-walkthrough-for-encoding-qr-codes-5a33e1e882b5
    https://www.thonky.com/qr-code-tutorial/error-correction-coding
    """
    def __init__(self):
        self.gf_to_ex = bytearray(256)
        self.ex_to_gf = bytearray(256)
        
        for n in range(256):
            if (n == 0):
                gf = 1
            else:
                gf = gf * 2

            if (gf >= 256):
                gf = gf % 256
                gf = gf ^ (285 - 256)

            self.ex_to_gf[n] = gf

        for n in range(1,255):
            self.gf_to_ex[self.ex_to_gf[n]] = n

        #for n in range(256):
        #    print(n,self.ex_to_gf[n],self.gf_to_ex[n])


    #
    def add_sub(self,a : int, b : int) -> int:
        return a ^ b

    #
    def mul(self, a : int, b : int) -> int:
        n = (self.gf_to_ex[a] + self.gf_to_ex[b]) % 255
        return self.ex_to_gf[n]

    #
    def div(self,a : int, b : int) -> int:
        n = (self.gf_to_ex[a] - self.gf_to_ex[b]) % 255
        return self.ex_to_gf[n]

    #
    def e2g(self, a, inplace : bool=True) -> int:
        if (type(a) is int):
            return self.ex_to_gf[a]

        if (inplace):
            for i in range(len(a)):
                a[i] = self.ex_to_gf[a[i]]
            return a
        else:
            return bytearray(map(lambda n:self.ex_to_gf[n],a))

    #
    def g2e(self, a , inplace : bool=True):
        if (type(a) is int):
            return self.gf_to_ex[a]

        if (inplace):
            for i in range(len(a)):
                a[i] = self.gf_to_ex[a[i]]
            return a
        else:
            return bytearray(map(lambda n:self.gf_to_ex[n],a))


#
# 3-L generator polynomial
# a^0x15+ a^8x14+ a^183x13+ a^61x12+ a^91x11+ a^202x10+ a^37x9 +
# a^51x8 + a^58x7 + a^58x6 + a^237x5 + a^140x4 + a^124x3 + a^5 x2 +
# a^99x+ a^105
#
# Represented in gf(256), where a=2
#   1, 29,196,111,163,112, 74,
#  10,105,105,139,132,151, 32
# 134,26  
#



class generator(object):
    """
    The generator polynomial is created by multiplying
    together a**0x-a**0 through a**0x-a**(n-1), where
    n is the number of error codewords to be generated
    and a = 2

    For more information see:
    https://www.thonky.com/qr-code-tutorial/how-create-generator-polynomial

    """

    # x[01-9]{1,3} \+ α
    generators_ = {
        "gen_7":bytes([87,229,146,149,238,102,21]),
        "gen_10":bytes([251,67,46,61,118,70,64,94,32,45]),
        "gen_13":bytes([74,152,176,100,86,100,106,104,130,218,206,140,78]),
        "gen_15":bytes([8,183,61,91,202,37,51,58,58,237,140,124,5,99,105]),
        "gen_16":bytes([120,104,107,109,102,161,76,3,91,191,147,169,182,194,225,120]),
        "gen_17":bytes([43,139,206,78,43,239,123,206,214,147,24,99,150,39,243,163,136]),
        "gen_18":bytes([215,234,158,94,184,97,118,170,79,187,152,148,252,179,5,98,96,153]),
        "gen_20":bytes([17,60,79,50,61,163,26,187,202,180,221,225,83,239,156,164,212,212,188,190]),
        "gen_22":bytes([210,171,247,242,93,230,14,109,221,53,200,74,8,172,98,80,219,134,160,105,165,231]),
        "gen_24":bytes([229,121,135,48,211,117,251,126,159,180,169,152,192,226,228,218,111,0,117,232,87,96,227,21]),
        "gen_26":bytes([173,125,158,2,103,182,118,17,145,201,111,28,165,53,161,21,245,142,13,102,48,227,153,145,218,70]),
        "gen_28":bytes([168,223,200,104,224,234,108,180,110,190,195,147,205,27,232,201,21,43,245,87,42,195,212,119,242,37,9,123]),
        "gen_30":bytes([41,173,145,152,216,31,179,182,50,48,110,86,239,96,222,125,42,173,226,193,224,130,156,37,251,216,238,40,192,180])
    }

    @staticmethod
    def get_by_name(name : str) -> bytearray:
        if (name in generator.generators_):
            return generator.generators_[name]
        else:
            return None

    @staticmethod
    def get_by_ecc_codewords(ecw : int) -> bytearray:
        if (ecw not in [7,10,13,15,16,17,18,20,22,24,26,28,30]):
            raise ValueError(f"Unsupported generator size {ecw}")

        return generator.get_by_name(f"gen_{ecw}")

    def __init__(self):
        pass


class polydiv(galois_field_256):
    def __init__(self):
        super().__init__()

    #
    # poly in an integer coefficient form
    # gen  in an exponent form
    #
    #
    #
    # returns ECC size of generator in integer coefficient form
    #
    def polydiv(self, poly : bytearray, gen : bytes) -> bytearray:

        genlen = len(gen)
        polylen = len(poly)

        # bytearray zeroes each index by default
        p = bytearray(polylen+genlen)

        # bytearray takes care of overrunning the index..
        for n in range(polylen):
            p[n] = poly[n]

        n = 0

        while (n < polylen):
            ex = self.g2e(p[n])

            # The first coefficient will be zero in any case
            n += 1

            for m in range(genlen):
                g = self.e2g(ex + gen[m] if ex + gen[m] < 256 else ex + gen[m] - 255)
                r = p[n+m] ^ g
                p[n+m] = r

        return p[n:n+genlen]

    def polydiv2(self, poly : bytearray, index : int, polylen : int, gen : bytes) -> bytearray:

        genlen = len(gen)

        # bytearray zeroes each index by default
        p = bytearray(polylen+genlen)

        # bytearray takes care of overrunning the index..

        if (len(poly) < index+polylen):
            to_copy = len(poly) - index
        else:
            to_copy = polylen

        for n in range(to_copy):
            p[n] = poly[index+n]

        n = 0

        while (n < polylen):
            ex = self.g2e(p[n])

            # The first coefficient will be zero in any case
            n += 1

            for m in range(genlen):
                g = self.e2g(ex + gen[m] if ex + gen[m] < 256 else ex + gen[m] - 255)
                r = p[n+m] ^ g
                p[n+m] = r

        return p[n:n+genlen]
