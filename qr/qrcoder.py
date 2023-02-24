#
# Build QR-Code.. done for self education purposes and therefore
# code efficiency or compactness has not been a design target.

import numpy as np
from . import phrasecoder
from . import galois
from . import qrpenalty
import sys

class encode(object):
    #
    QR_BLACK = 0        # final pixel in black
    QR_WHITE = 255      # final pixel in white
    QR_WHITEZ = 255      # for debugging reserved areas
    QR_UNUSED = 64       # temporary pixel not in use
    QR_OTHER = 128

    # ECC levels
    QR_ECC_L = 'L'
    QR_ECC_M = 'M'
    QR_ECC_Q = 'Q'
    QR_ECC_H = 'H'

    #
    QR_MAX_VERSION = 40

    # 
    QR_DIR_UP = 0
    QR_DIR_DOWN = 1

    QR_STM_LR   = 0     # lower right
    QR_STM_LL   = 1     # lower left
    QR_STM_UL   = 2     # upper left
    QR_STM_UR   = 3     # upper right

    # Internal class for grouping QR-code encoding information.
    class eccInfo(object):
        def __init__(self, ver, dc, ec, g1b, g1dc, g2b, g2dc, lev):
            self.data_codewords = dc
            self.ecc_codewords = ec
            self.group1blocks = g1b
            self.group1data_codewords = g1dc
            self.group2blocks = g2b
            self.group2data_codewords = g2dc
            self.version = ver
            self.ecc_level = lev

        def get_num_row(self):
            return self.group1blocks+self.group2blocks

        def get_max_col(self):
            return self.group1data_codewords if self.group1data_codewords > self.group2data_codewords else self.group2data_codewords

        def get_total_codewords(self):
            return self.group1data_codewords*self.group2blocks + self.group2data_codewords*self.group2blocks

    # finder pattern
    finder_ =   np.array(
                [QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK,
                 QR_BLACK,QR_WHITE,QR_WHITE,QR_WHITE,QR_WHITE,QR_WHITE,QR_BLACK,
                 QR_BLACK,QR_WHITE,QR_BLACK,QR_BLACK,QR_BLACK,QR_WHITE,QR_BLACK,
                 QR_BLACK,QR_WHITE,QR_BLACK,QR_BLACK,QR_BLACK,QR_WHITE,QR_BLACK,
                 QR_BLACK,QR_WHITE,QR_BLACK,QR_BLACK,QR_BLACK,QR_WHITE,QR_BLACK,
                 QR_BLACK,QR_WHITE,QR_WHITE,QR_WHITE,QR_WHITE,QR_WHITE,QR_BLACK,
                 QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK],
                 np.uint8).reshape(7,7)

    # alignment pattern
    alignment_ =np.array(
                [QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK,
                 QR_BLACK,QR_WHITE,QR_WHITE,QR_WHITE,QR_BLACK,
                 QR_BLACK,QR_WHITE,QR_BLACK,QR_WHITE,QR_BLACK,
                 QR_BLACK,QR_WHITE,QR_WHITE,QR_WHITE,QR_BLACK,
                 QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK,QR_BLACK],np.uint8).reshape(5,5)

    # Starting from version 2
    alignment_loc_ = [
            [6,  18],  #2
            [6,  22],
            [6,  26],
            [6,  30],
            [6,  34],
            [6,  22,38],  #7
            [6,  24,42],
            [6,  26,46],
            [6,	28,	50],
            [6,	30,	54],
            [6,	32,	58],
            [6,	34,	62],
            [6,	26,	46,	66],  #14
            [6,	26,	48,	70],
            [6,	30,	54,	78],
            [6,	26,	50,	74],
            [6,	30,	56,	82],
            [6,	30,	58,	86],
            [6,	34,	62,	90],
            [6,	28,	50,	72,	94],	#21		
            [6,	26,	50,	74,	98],				
            [6,	30,	54,	78,	102],				
            [6,	28,	54,	80,	106],			
            [6,	32,	58,	84,	110],				
            [6,	30,	58,	86,	114],				
            [6,	34,	62,	90,	118],		
            [6,	26,	50,	74,	98,	122],	#28
            [6,	30,	54,	78,	102,126],	
            [6,	26,	52,	78,	104,130],	
            [6,	30,	56,	82,	108,134],	
            [6,	34,	60,	86,	112,138],
            [6,	30,	58,	86,	114,142],
            [6,	34,	62,	90,	118,146],
            [6,	24,	50,	76,	102,128,154],
            [6,	30,	54,	78,	102,126,150],    #35
            [6,	28,	54,	80,	106,132,158],
            [6,	32,	58,	84,	110,136,162],
            [6,	26,	54,	82,	110,138,166],
            [6,	30,	58,	86,	114,142,170]]

    ecc_table_ = {
        # "version-level",data codewords,ecc codewords, grp1 # blks, gpr1 # data codewords, grp2 # blks, gpr2 # data codewords
        "1-L":eccInfo(1,19,7,1,19,0,0,0b01),
        "1-M":eccInfo(1,16,10,1,16,0,0,0b00),
        "1-Q":eccInfo(1,13,13,1,13,0,0,0b11),
        "1-H":eccInfo(1,9,17,1,9,0,0,0b10),
        "2-L":eccInfo(2,34,10,1,34,0,0,0b01),
        "2-M":eccInfo(2,28,16,1,28,0,0,0b00),
        "2-Q":eccInfo(2,22,22,1,22,0,0,0b11),
        "2-H":eccInfo(2,16,28,1,16,0,0,0b10),
        "3-L":eccInfo(3,55,15,1,55,0,0,0b01),  # 29x29 
        "3-M":eccInfo(3,44,26,1,44,0,0,0b00),
        "3-Q":eccInfo(3,34,18,2,17,0,0,0b11),
        "3-H":eccInfo(3,26,22,2,13,0,0,0b10),
        "4-L":eccInfo(4,80,20,1,80,0,0,0b01),
        "4-M":eccInfo(4,64,18,2,32,0,0,0b00),
        "4-Q":eccInfo(4,48,26,2,24,0,0,0b11),
        "4-H":eccInfo(4,36,16,4,9,0,0,0b10),
        "5-L":eccInfo(5,108,26,1,108,0,0,0b01),
        "5-M":eccInfo(5,86,24,2,43,0,0,0b00),
        "5-Q":eccInfo(5,62,18,2,15,2,16,0b11),
        "5-H":eccInfo(5,46,22,2,11,2,12,0b10),
        "6-L":eccInfo(6,136,18,2,68,0,0,0b01),
        "6-M":eccInfo(6,108,16,4,27,0,0,0b00),
        "6-Q":eccInfo(6,76,24,4,19,0,0,0b11),
        "6-H":eccInfo(6,60,28,4,15,0,0,0b10),
        "7-L":eccInfo(7,156,20,2,78,0,0,0b01),
        "7-M":eccInfo(7,124,18,4,31,0,0,0b00),
        "7-Q":eccInfo(7,88,18,2,14,4,15,0b11),
        "7-H":eccInfo(7,66,26,4,13,1,14,0b10),
        "8-L":eccInfo(8,194,24,2,97,0,0,0b01),
        "8-M":eccInfo(8,154,22,2,38,2,39,0b00),
        "8-Q":eccInfo(8,110,22,4,18,2,19,0b11),
        "8-H":eccInfo(8,86,26,4,14,2,15,0b10),
        "9-L":eccInfo(9,232,30,2,116,0,0,0b01),
        "9-M":eccInfo(9,182,22,3,36,2,37,0b00),
        "9-Q":eccInfo(9,132,20,4,16,4,17,0b11),
        "9-H":eccInfo(9,100,24,4,12,4,13,0b10),
        "10-L":eccInfo(10,274,18,2,68,2,69,0b01),
        "10-M":eccInfo(10,216,26,4,43,1,44,0b00),
        "10-Q":eccInfo(10,154,24,6,19,2,20,0b11),
        "10-H":eccInfo(10,122,28,6,15,2,16,0b10),
        "11-L":eccInfo(11,324,20,4,81,0,0,0b01),
        "11-M":eccInfo(11,254,30,1,50,4,51,0b00),
        "11-Q":eccInfo(11,180,28,4,22,4,23,0b11),
        "11-H":eccInfo(11,140,24,3,12,8,13,0b10),
        "12-L":eccInfo(12,370,24,2,92,2,93,0b01),
        "12-M":eccInfo(12,290,22,6,36,2,37,0b00),
        "12-Q":eccInfo(12,206,26,4,20,6,21,0b11),
        "12-H":eccInfo(12,158,28,7,14,4,15,0b10),
        "13-L":eccInfo(13,428,26,4,107,0,0,0b01),
        "13-M":eccInfo(13,334,22,8,37,1,38,0b00),
        "13-Q":eccInfo(13,244,24,8,20,4,21,0b11),
        "13-H":eccInfo(13,180,22,12,11,4,12,0b10),
        "14-L":eccInfo(14,461,30,3,115,1,116,0b01),
        "14-M":eccInfo(14,365,24,4,40,5,41,0b00),
        "14-Q":eccInfo(14,261,20,11,16,5,17,0b11),
        "14-H":eccInfo(14,197,24,11,12,5,13,0b10),
        "15-L":eccInfo(15,523,22,5,87,1,88,0b01),
        "15-M":eccInfo(15,415,24,5,41,5,42,0b00),
        "15-Q":eccInfo(15,295,30,5,24,7,25,0b11),
        "15-H":eccInfo(15,223,24,11,12,7,13,0b10),
        "16-L":eccInfo(16,589,24,5,98,1,99,0b01),
        "16-M":eccInfo(16,453,28,7,45,3,46,0b00),
        "16-Q":eccInfo(16,325,24,15,19,2,20,0b11),
        "16-H":eccInfo(16,253,30,3,15,13,16,0b10),
        "17-L":eccInfo(17,647,28,1,107,5,108,0b01),
        "17-M":eccInfo(17,507,28,10,46,1,47,0b00),
        "17-Q":eccInfo(17,367,28,1,22,15,23,0b11),
        "17-H":eccInfo(17,283,28,2,14,17,15,0b10),
        "18-L":eccInfo(18,721,30,5,120,1,121,0b01),
        "18-M":eccInfo(18,563,26,9,43,4,44,0b00),
        "18-Q":eccInfo(18,397,28,17,22,1,23,0b11),
        "18-H":eccInfo(18,313,28,2,14,19,15,0b10),
        "19-L":eccInfo(19,795,28,3,113,4,114,0b01),
        "19-M":eccInfo(19,627,26,3,44,11,45,0b00),
        "19-Q":eccInfo(19,445,26,17,21,4,22,0b11),
        "19-H":eccInfo(19,341,26,9,13,16,14,0b10),
        "20-L":eccInfo(20,861,28,3,107,5,108,0b01),
        "20-M":eccInfo(20,669,26,3,41,13,42,0b00),
        "20-Q":eccInfo(20,485,30,15,24,5,25,0b11),
        "20-H":eccInfo(20,385,28,15,15,10,16,0b10),
        "21-L":eccInfo(21,932,28,4,116,4,117,0b01),
        "21-M":eccInfo(21,714,26,17,42,0,0,0b00),
        "21-Q":eccInfo(21,512,28,17,22,6,23,0b11),
        "21-H":eccInfo(21,406,30,19,16,6,17,0b10),
        "22-L":eccInfo(22,1006,28,2,111,7,112,0b01),
        "22-M":eccInfo(22,782,28,17,46,0,0,0b00),
        "22-Q":eccInfo(22,568,30,7,24,16,25,0b11),
        "22-H":eccInfo(22,442,24,34,13,0,0,0b10),
        "23-L":eccInfo(23,1094,30,4,121,5,122,0b01),
        "23-M":eccInfo(23,860,28,4,47,14,48,0b00),
        "23-Q":eccInfo(23,614,30,11,24,14,25,0b11),
        "23-H":eccInfo(23,464,30,16,15,14,16,0b10),
        "24-L":eccInfo(24,1174,30,6,117,4,118,0b01),
        "24-M":eccInfo(24,914,28,6,45,14,46,0b00),
        "24-Q":eccInfo(24,664,30,11,24,16,25,0b11),
        "24-H":eccInfo(24,514,30,30,16,2,17,0b10),
        "25-L":eccInfo(25,1276,26,8,106,4,107,0b01),
        "25-M":eccInfo(25,1000,28,8,47,13,48,0b00),
        "25-Q":eccInfo(25,718,30,7,24,22,25,0b11),
        "25-H":eccInfo(25,538,30,22,15,13,16,0b10),
        "26-L":eccInfo(26,1370,28,10,114,2,115,0b01),
        "26-M":eccInfo(26,1062,28,19,46,4,47,0b00),
        "26-Q":eccInfo(26,754,28,28,22,6,23,0b11),
        "26-H":eccInfo(26,596,30,33,16,4,17,0b10),
        "27-L":eccInfo(27,1468,30,8,122,4,123,0b01),
        "27-M":eccInfo(27,1128,28,22,45,3,46,0b00),
        "27-Q":eccInfo(27,808,30,8,23,26,24,0b11),
        "27-H":eccInfo(27,628,30,12,15,28,16,0b10),
        "28-L":eccInfo(28,1531,30,3,117,10,118,0b01),
        "28-M":eccInfo(28,1193,28,3,45,23,46,0b00),
        "28-Q":eccInfo(28,871,30,4,24,31,25,0b11),
        "28-H":eccInfo(28,661,30,11,15,31,16,0b10),
        "29-L":eccInfo(29,1631,30,7,116,7,117,0b01),
        "29-M":eccInfo(29,1267,28,21,45,7,46,0b00),
        "29-Q":eccInfo(29,911,30,1,23,37,24,0b11),
        "29-H":eccInfo(29,701,30,19,15,26,16,0b10),
        "30-L":eccInfo(30,1735,30,5,115,10,116,0b01),
        "30-M":eccInfo(30,1373,28,19,47,10,48,0b00),
        "30-Q":eccInfo(30,985,30,15,24,25,25,0b11),
        "30-H":eccInfo(30,745,30,23,15,25,16,0b10),
        "31-L":eccInfo(31,1843,30,13,115,3,116,0b01),
        "31-M":eccInfo(31,1455,28,2,46,29,47,0b00),
        "31-Q":eccInfo(31,1033,30,42,24,1,25,0b11),
        "31-H":eccInfo(31,793,30,23,15,28,16,0b10),
        "32-L":eccInfo(32,1955,30,17,115,0,0,0b01),
        "32-M":eccInfo(32,1541,28,10,46,23,47,0b00),
        "32-Q":eccInfo(32,1115,30,10,24,35,25,0b11),
        "32-H":eccInfo(32,845,30,19,115,35,16,0b10),
        "33-L":eccInfo(33,2071,30,17,115,1,116,0b01),
        "33-M":eccInfo(33,1631,28,14,46,21,47,0b00),
        "33-Q":eccInfo(33,1171,30,29,24,19,25,0b11),
        "33-H":eccInfo(33,901,30,11,15,46,16,0b10),
        "34-L":eccInfo(34,2191,30,13,115,6,116,0b01),
        "34-M":eccInfo(34,1725,28,14,46,23,47,0b00),
        "34-Q":eccInfo(34,1231,30,44,24,7,25,0b11),
        "34-H":eccInfo(34,961,30,59,16,1,17,0b10),
        "35-L":eccInfo(35,2306,30,12,121,7,122,0b01),
        "35-M":eccInfo(35,1812,28,12,47,26,48,0b00),
        "35-Q":eccInfo(35,1286,30,39,24,14,25,0b11),
        "35-H":eccInfo(35,986,30,22,15,41,16,0b10),
        "36-L":eccInfo(36,2434,30,6,121,14,122,0b01),
        "36-M":eccInfo(36,1914,28,6,47,34,48,0b00),
        "36-Q":eccInfo(36,1354,30,46,24,10,25,0b11),
        "36-H":eccInfo(36,1054,30,2,15,64,16,0b10),
        "37-L":eccInfo(37,2566,30,17,122,4,123,0b01),
        "37-M":eccInfo(37,1992,28,29,46,14,47,0b00),
        "37-Q":eccInfo(37,1426,30,49,24,10,25,0b11),
        "37-H":eccInfo(37,1096,30,24,15,46,16,0b10),
        "38-L":eccInfo(38,2702,30,4,122,18,123,0b01),
        "38-M":eccInfo(38,2102,28,13,46,32,47,0b00),
        "38-Q":eccInfo(38,1502,30,48,24,14,25,0b11),
        "38-H":eccInfo(38,1142,30,42,15,32,16,0b10),
        "39-L":eccInfo(39,2812,30,20,117,4,118,0b01),
        "39-M":eccInfo(39,2216,28,40,47,7,48,0b00),
        "39-Q":eccInfo(39,1582,30,43,24,22,25,0b11),
        "39-H":eccInfo(39,1222,30,10,15,67,16,0b10),
        "40-L":eccInfo(40,2956,30,19,118,6,1196,0b01),
        "40-M":eccInfo(40,2334,28,18,47,31,486,0b00),
        "40-Q":eccInfo(40,1666,30,34,24,34,256,0b11),
        "40-H":eccInfo(40,1276,30,20,15,61,16,0b10)}

    # Contributed by https://www.thonky.com/qr-code-tutorial/format-version-tables
    # Lazy enough not to calculate them runtime..
    format_tab_ = (
        "101010000010010",  # M = 0, mask 0
        "101000100100101",
        "101111001111100",
        "101101101001011",
        "100010111111001",
        "100000011001110",
        "100111110010111",
        "100101010100000",  # mask 7
        "111011111000100",  # L = 1, mask 0
        "111001011110011",
        "111110110101010",
        "111100010011101",
        "110011000101111",
        "110001100011000",
        "110110001000001",
        "110100101110110",
        "001011010001001",  # H = 2, mask 0
        "001001110111110",
        "001110011100111",
        "001100111010000",
        "000011101100010",
        "000001001010101",
        "000110100001100",
        "000100000111011",
        "011010101011111",  # Q = 3, mask 0
        "011000001101000",
        "011111100110001",
        "011101000000110",
        "010010010110100",
        "010000110000011",
        "010111011011010",
        "010101111101101",
        "111111111111111")

    # Contributed by https://www.thonky.com/qr-code-tutorial/format-version-tables
    # Lazy enough not to calculate them runtime..
    version_tab_ = (
        "000111110010010100",   # version 7.. these are in not so good order but..
        "001000010110111100",
        "001001101010011001",
        "001010010011010011",
        "001011101111110110",
        "001100011101100010",
        "001101100001000111",
        "001110011000001101",
        "001111100100101000",
        "010000101101111000",
        "010001010001011101",
        "010010101000010111",
        "010011010100110010",
        "010100100110100110",
        "010101011010000011",
        "010110100011001001",
        "010111011111101100",
        "011000111011000100",
        "011001000111100001",
        "011010111110101011",
        "011011000010001110",
        "011100110000011010",
        "011101001100111111",
        "011110110101110101",
        "011111001001010000",
        "100000100111010101",
        "100001011011110000",
        "100010100010111010",
        "100011011110011111",
        "100100101100001011",
        "100101010000101110",
        "100110101001100100",
        "100111010101000001")   # version 40

    # number of bits to pad per QR-code version
    padding_tab_ = (
        -1,0,7,7,7,7,7,0,0,0,0,0,0,0,3,3,3,3,3,3,3,4,4,
        4,4,4,4,4,3,3,3,3,3,3,3,0,0,0,0,0,0)

    @staticmethod
    def get_dimension_by_version(version:int)->int:
        if (version < 1 or version > 40):
            raise ValueError("Version can be between 1 to 40")

        return 21 + (version - 1) * 4

    @staticmethod
    def get_eccInfo(version_level:str)->eccInfo:
        if (version_level not in encode.ecc_table_):
            return None

        ei =  encode.ecc_table_[version_level]

        # Currently we only support 1 group 1 block..
        #if (ei.group1blocks > 1 or ei.group2blocks > 0):
        #    raise NotImplementedError("Number or Group 1 or 2 blocks not supported")

        return ei

    @staticmethod
    def find_version_level(lenght:int,level:str=QR_ECC_L)->str:
        # brute force search.. could probably use binary search
        for i in range(1,encode.QR_MAX_VERSION+1):
            key = f"{i}-{level}"

            if (key in encode.ecc_table_):
                ecc = encode.ecc_table_[key]

                if (ecc.data_codewords >= lenght):
                    return key
            else:
                break

        return None

    #
    def prep_finder_patterns(self):
        # also include separators around finder patterns.
        d = self.get_dimension()
        
        # upper left
        self.qr[0:7,0:7]   = encode.finder_
        # upper right
        self.qr[0:7,d-7:d] = encode.finder_
        # lower left
        self.qr[d-7:d,0:7] = encode.finder_

    #
    def prep_timing_patterns(self):
        v = self.ecc_info.version
        d = self.get_dimension()

        # seveth row
        pixel = encode.QR_BLACK
        for n in range(8,d-8):
            self.qr[6,n] = pixel
            pixel = encode.QR_BLACK if pixel == encode.QR_WHITE else encode.QR_WHITE

        # seveth column
        pixel = encode.QR_BLACK
        for n in range(8,d-8):
            self.qr[n,6] = pixel
            pixel = encode.QR_BLACK if pixel == encode.QR_WHITE else encode.QR_WHITE

        # dark module
        self.qr[4*v+9,8] = encode.QR_BLACK


    #
    def prep_separators(self):
        d = self.get_dimension()

        # rows
        for n in range(0,8):
            self.qr[7,n]     = encode.QR_WHITE
            self.qr[7,d-n-1] = encode.QR_WHITE
            self.qr[d-8,n]   = encode.QR_WHITE

        # columns
        for n in range(0,8):
            self.qr[n,7]     = encode.QR_WHITE
            self.qr[d-n-1,7] = encode.QR_WHITE
            self.qr[n,d-8]   = encode.QR_WHITE

    #
    def prep_alignment_patterns(self):
        v = self.ecc_info.version
        
        if (v < 2):
            return 

        # version 2 or greater..
        loc = encode.alignment_loc_[v-2]
        loc_len = len(loc)

        # special case 1st row
        yp = loc[0] - 2
        
        for n in range(1,loc_len-1):
            xp = loc[n] - 2
            self.qr[yp:yp+5,xp:xp+5] = encode.alignment_

        # special case last row
        yp = loc[loc_len-1] - 2

        for n in range(1,loc_len):
            xp = loc[n] - 2
            self.qr[yp:yp+5,xp:xp+5] = encode.alignment_

        # generic case if version > 2
        for y in range(1,loc_len-1):
            yp = loc[y] - 2

            for x in range(loc_len):
                xp = loc[x] - 2
                self.qr[yp:yp+5,xp:xp+5] = encode.alignment_

    # version information
    def insert_version(self,prep=True):
        v = self.ecc_info.version
        d = self.get_dimension()

        # For QR code versions greater or equal to 7
        if (v >= 7):

            if (prep):
                # jusr reserve space..
                black = encode.QR_WHITE
            else:
                black = encode.QR_BLACK

            lower_left = np.zeros([3,6],dtype=np.uint8)
            upper_rght = np.zeros([6,3],dtype=np.uint8)
 
            # prepare lower left
            i = 18
            for x in range(6):
                for y in range(3):
                    i = i - 1

                    if (encode.version_tab_[v-7][i] == '1'):
                        lower_left[y,x] = black
                    else:
                        lower_left[y,x] = encode.QR_WHITE

            self.qr[d-11:d-8,0:6] = lower_left

            # prepare upper right
            i = 18
            for y in range(6):
                for x in range(3):
                    i = i - 1

                    if (encode.version_tab_[v-7][i] == '1'):
                        upper_rght[y,x] = black
                    else:
                        upper_rght[y,x] = encode.QR_WHITE

            self.qr[0:6,d-11:d-8] = upper_rght

    def insert_level_mask(self, mask=None):
        if (mask is None):
            fmt = "000000000000000"
            #fmt = "111111111111111"
        else:
            fmt = encode.format_tab_[self.ecc_info.ecc_level * 8 + mask]

        d = self.get_dimension()

        # horizontal level + mask string
        i = 0

        for n in range(6):
            # 0 to 5
            self.qr[8,n] = encode.QR_BLACK if fmt[i] == '1' else encode.QR_WHITEZ
            i = i + 1

        # 6 and 7
        self.qr[8,7] = encode.QR_BLACK if fmt[i] == '1' else encode.QR_WHITEZ
        self.qr[8,8] = encode.QR_BLACK if fmt[i+1] == '1' else encode.QR_WHITEZ

        # 8 to 14
        for n in range(d-8,d):
            i = i + 1
            self.qr[8,n] = encode.QR_BLACK if fmt[i] == '1' else encode.QR_WHITEZ

        # vertical level + mask string
        
        for n in range(0,6):
            self.qr[n,8] = encode.QR_BLACK if fmt[i] == '1' else encode.QR_WHITEZ
            i = i - 1

        self.qr[7,8] = encode.QR_BLACK if fmt[i] == '1' else encode.QR_WHITEZ
        i = i - 1

        for n in range(d-7,d):
            i = i - 1
            self.qr[n,8] = encode.QR_BLACK if fmt[i] == '1' else encode.QR_WHITEZ

    #
    def reset_cwd_ewd(self):
        d = self.get_dimension()
        self.cwd_x_ = d - 1
        self.cwd_y_ = d - 1

    #
    def get_version_level(self):
        return self.ecc_info.version,self.ecc_info.ecc_level

    def calc_code_ecc_arrays(self, phrase : bytearray):
        ei = self.ecc_info

        gen = galois.generator.get_by_ecc_codewords(ei.ecc_codewords)
        div = galois.polydiv()

        ewds = []
        cwds = []

        # divide phrase into group1 blocks.. group1 is always >= 1
        stp = ei.group1data_codewords

        for n in range(ei.group1blocks):
            ecc = div.polydiv2(phrase,n*stp,stp,gen)
            ewds.append(ecc)
            cwds.append(bytearray(phrase[n*stp:(n+1)*stp]))

        # divide phrase into group2 blocks.. group2 may be 0 i.e. non-existent
        sta = ei.group1blocks*stp
        stp = ei.group2data_codewords

        for n in range(ei.group2blocks):
            ecc = div.polydiv2(phrase,sta+n*stp,stp,gen)
            ewds.append(ecc)
            cwds.append(bytearray(phrase[sta+n*stp:sta+(n+1)*stp]))

        return cwds,ewds
   
    #
    def interleave_code_ecc_arrays(self,cwds : bytearray, ewds : bytearray) -> bytearray:
        dst = bytearray()

        # data
        for cols in range(self.ecc_info.get_max_col()):
            for rows in range(self.ecc_info.get_num_row()):
                if (cols < len(cwds[rows])):
                    dst.append(cwds[rows][cols])
                    
        # ecc
        for cols in range(self.ecc_info.ecc_codewords):
            for rows in range(self.ecc_info.get_num_row()):
                dst.append(ewds[rows][cols])

        # map to array of bytes to zeroes and ones.. tedious.. lookup would be nice :D
        bin_array = bytearray()
        
        for byte in dst:
            bin_array.append(encode.QR_BLACK if (byte & 0x80) else encode.QR_WHITE)
            bin_array.append(encode.QR_BLACK if (byte & 0x40) else encode.QR_WHITE)
            bin_array.append(encode.QR_BLACK if (byte & 0x20) else encode.QR_WHITE)
            bin_array.append(encode.QR_BLACK if (byte & 0x10) else encode.QR_WHITE)
            bin_array.append(encode.QR_BLACK if (byte & 0x08) else encode.QR_WHITE)
            bin_array.append(encode.QR_BLACK if (byte & 0x04) else encode.QR_WHITE)
            bin_array.append(encode.QR_BLACK if (byte & 0x02) else encode.QR_WHITE)
            bin_array.append(encode.QR_BLACK if (byte & 0x01) else encode.QR_WHITE)

        # add padding if needed..
        padding = encode.padding_tab_[self.ecc_info.version]

        while (padding > 0):
            bin_array.append(encode.QR_WHITE)
            padding -= 1

        return bin_array

    #
    def init_pos_(self):
        self.y = self.dimension - 1
        self.x = self.dimension - 1
        self.dir = encode.QR_DIR_UP
        self.stm = 0
        return self.y, self.x

    def next_pos_(self):
        self.stm = (self.stm + 1) % 4

        if (self.dir == encode.QR_DIR_UP):
            if (self.stm == 1):
                y,x = self.y,self.x-1
            elif (self.stm == 2):
                self.y -= 1
                y,x = self.y,self.x
            elif (self.stm == 3):
                y,x = self.y,self.x-1
            else:
                self.y -= 1
                y,x = self.y,self.x
        else:
            if (self.stm == 1):
                y,x = self.y,self.x-1
            elif (self.stm == 2):
                self.y += 1
                y,x = self.y,self.x
            elif (self.stm == 3):
                y,x = self.y,self.x-1
            else:
                self.y += 1
                y,x = self.y,self.x

        # reached the top?
        if (y < 0):
            self.dir = encode.QR_DIR_DOWN
            self.y = 0
            self.x = self.x - 2
            y,x = self.y,self.x

        # reached the bottom?
        if (y >= self.dimension):
            self.dir = encode.QR_DIR_UP
            self.y = self.dimension - 1
            self.x = self.x - 2
            y,x = self.y,self.x

        # check for vertical timing pattern
        if (self.x == 6):
            self.x = 5
            x = self.x

        return y,x

    #
    def encode_layout(self, bin_array : bytearray):
        # lower right corner as the starting pos
        y,x = self.init_pos_()

        for bit in bin_array:
            bit_unset = True
    
            while (bit_unset):
                if (self.qr_msk[y,x] == encode.QR_UNUSED):
                    if (bit == 0x00):
                        self.qr[y,x] = encode.QR_BLACK
                    else: 
                        self.qr[y,x] = encode.QR_WHITE
                    
                    bit_unset = False
                    
                y,x = self.next_pos_()

        # qr_rst is used to reset back to non-masked version..
        for row in range(self.dimension):
            for col in range(self.dimension):
                self.qr_rst[row,col] = self.qr[row,col]

    def encode_mask(self, mask : int):
        # reset to inital state
        for row in range(self.get_dimension()):
            for col in range(self.get_dimension()):
                self.qr[row,col] = self.qr_rst[row,col]

        for row in range(self.get_dimension()):
            for col in range(self.get_dimension()):
                flip = False

                if (mask == 0):
                    # (row + column) mod 2 == 0
                    if ((row + col) % 2 == 0):
                        flip = True

                elif (mask == 1):
                    # (row) mod 2 == 0
                    if (row % 2 == 0):
                        flip = True
    
                elif (mask == 2):
                    # (column) mod 3 == 0
                    if (col % 3 == 0):
                        flip = True

                elif (mask == 3):
                    # (row + column) mod 3 == 0
                    if ((row + col) % 3 == 0):
                        flip = True

                elif (mask == 4):
                    # ( floor(row / 2) + floor(column / 3) ) mod 2 == 0
                    if ((row // 2 + col // 3) % 2 == 0):
                        flip = True

                elif (mask == 5):
                    # ((row * column) mod 2) + ((row * column) mod 3) == 0
                    if (((row * col) % 2) + ((row * col) % 3) == 0):
                        flip = True

                elif (mask == 6):
                    # ( ((row * column) mod 2) + ((row * column) mod 3) ) mod 2 == 0
                    if ((((row * col) % 2) + ((row * col) % 3)) % 2 == 0):
                        flip = True

                else:   # (mask == 7):
                    # ( ((row + column) mod 2) + ((row * column) mod 3) ) mod 2 == 0
                    if ((((row + col) % 2) + ((row * col) % 3)) % 2 == 0):
                        flip = True

                if (flip == True):
                    #print(f"{self.qr_msk[row,col]}, {self.qr[row,col]}",end="")
                    if (self.qr_msk[row,col] == encode.QR_UNUSED):
                        self.qr[row,col] ^= 0xff

                    #print(f", {self.qr[row,col]}")

    def get_dimension(self):
        return self.dimension

    #def get_tmp_qr(self):
    #    return self.qr_tmp

    #
    def get_qr(self):
        return self.qr

    #
    def get_mask(self):
        return self.mask

    #
    def __init__(self,version_level : str):
        self.version_level = version_level

        # Prepare for encoding, get generator, etc        
        self.ecc_info = encode.get_eccInfo(version_level)

        # Reserve 2-dimensional space for QR code "graphics"
        d = encode.get_dimension_by_version(self.ecc_info.version)
        self.qr = np.full((d,d),encode.QR_UNUSED,dtype=np.uint8,order='C')
        self.qr_rst = np.full((d,d),encode.QR_UNUSED,dtype=np.uint8,order='C')
        self.dimension = d
        self.mask = -1
        self.pe = qrpenalty.penalty(self.qr,self.dimension)

        # Build basic layout..
        self.prep_finder_patterns()
        self.prep_separators()
        self.prep_timing_patterns()
        self.prep_alignment_patterns()
        self.insert_version()       # just reserve
        self.insert_level_mask()    # just reserve

        # Going to be static..
        self.qr_msk = self.qr.copy()

    def generate_qr_code(self,phrase : str, mode_ : int=phrasecoder.encode.QR_MODE_ALPHANUMERIC):
        pc = phrasecoder.encode(mode=mode_,version=self.ecc_info.version)
        ph = pc.encode_phrase(phrase,self.ecc_info.data_codewords)
        dat,ecc = self.calc_code_ecc_arrays(ph)

        print("code words")

        for n in dat[0]:
            print(f"{n:02x},",end="")

        print("ECC")
        for n in ecc[0]:
            print(f"{n:02x},",end="")


        print()




        res = self.interleave_code_ecc_arrays(dat,ecc)
    
        self.encode_layout(res)

        lowest_mask = -1
        lowest_penalty = sys.maxsize

        for mask in range(8):
            self.encode_mask(mask)

            a = self.pe.calc_rule1()
            b = self.pe.calc_rule2()
            c = self.pe.calc_rule3()
            d = self.pe.calc_rule4()
            
            if (a+b+c+d < lowest_penalty):
                lowest_mask = mask
                lowest_penalty = a+b+c+d

            self.mask = lowest_mask
            self.encode_mask(lowest_mask)
            self.insert_level_mask(lowest_mask)
            self.insert_version(False)

        return self.qr

#
#
#if (__name__ == "__main__"):   
#
#    #
#    qr = encode("3-L")
#    qr_code = qr.generate_qr_code("https://board.esxdos.org/")
#    print(qr.get_mask())
#
#    #
#    ima = im.fromarray(qr_code, mode="P")
#    ima.show()




