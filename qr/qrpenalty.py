import numpy as np

class penalty(object):
    QR_BLACK = 0
    QR_WHITE = 255

    # match and next tables for rule 3
    rule3_1_pat = (0x00,0xff,0x00,0x00,0x00,0xff,0x00,0xff,0xff,0xff,0xff)
    #rule3_1_pat = (0xff,0x00,0xff,0xff,0xff,0x00,0xff,0x00,0x00,0x00,0x00)
    rule3_1_next = (1,1,3,2,1,2,1,4,3,2,1)

    rule3_2_pat = (0xff,0xff,0xff,0xff,0x00,0xff,0x00,0x00,0x00,0xff,0x00)
    #rule3_2_pat = (0x00,0x00,0x00,0x00,0xff,0x00,0xff,0xff,0xff,0x00,0xff)
    rule3_2_next = (4,3,2,1,2,1,3,2,1,1,1)


    def __init__(self, qr, dim):
        # size of the qr code
        self.d = dim
        self.qr = qr

    #
    def calc_rule1(self) -> int:
        # 5 consequtive pixels of the same color = 3 penalty point.
        # After 5 each additional pixel of the same color add 1 penalty point.
        # Do check for each row and column.

        penalty_rule1 = 0

        # horizontal penalty
        for row in range(self.d):
            consequtive = 0
            prev = 0

            for next in self.qr[row]:
                assert(next != 0 or next != 255)

                if (consequtive == 0):
                    consequtive = 1
                    prev = next
                else:
                    if (prev == next):
                        consequtive += 1 
                    else:
                        prev = next
                        consequtive = 0

                if (consequtive == 5):
                    penalty_rule1 += 3
                elif (consequtive > 5):
                    penalty_rule1 += 1

        # vertical penalty
        for col in range(self.d):
            consequtive = 0
            prev = 0

            for row in range(self.d):
                next = self.qr[row,col]
                assert(next != 0 or next != 255)

                if (consequtive == 0):
                    consequtive = 1
                    prev = next
                else:
                    if (prev == next):
                        consequtive += 1 
                    else:
                        prev = next
                        consequtive = 0

                if (consequtive == 5):
                    penalty_rule1 += 3
                elif (consequtive > 5):
                    penalty_rule1 += 1
        
        return penalty_rule1


    #
    def calc_rule2(self) -> int:
        # 3x(m-1 x n-1) blocks.. over the entire qr-c0de
        # brute force search using 2x2 blocks
        penalty_rule2 = 0

        for row in range(self.d - 1):
            for col in range(self.d - 1):
                if (self.qr[row,col] == self.qr[row,col+1] and
                    self.qr[row,col] == self.qr[row+1,col] and
                    self.qr[row+1,col] == self.qr[row+1,col+1]):
                    penalty_rule2 += 3

        return penalty_rule2


    #
    def calc_rule3_horiz_(self, pat, nxt) -> int:
        penalty_rule3 = 0

        for row in range(self.d):
            n = 0
            x = 0

            while (x < self.d - 11):
                if (pat[n] != self.qr[row,x+n]):
                    x += nxt[n]
                    n = 0
                else:
                    n += 1

                if (n == len(pat)):
                    x += n
                    penalty_rule3 += 40
                    n = 0
        
        return penalty_rule3

    def calc_rule3_vert_(self, pat, nxt) -> int:
        penalty_rule3 = 0

        for col in range(self.d):
            n = 0
            y = 0

            while (y < self.d - 11):
                if (pat[n] != self.qr[y+n,col]):
                    y += nxt[n]
                    n = 0
                else:
                    n += 1

                if (n == len(pat)):
                    y += n
                    penalty_rule3 += 40
                    n = 0
        
        return penalty_rule3

    def calc_rule3(self) -> int:
        # look for 0X000X0XXXX or XXXX0X000X0 patterns, where X = 255
        # slightly optomized search based on "next" table to advance to the
        # next possible seacrh position based on the search so far..

        # horizontal
        penalty_rule3 = self.calc_rule3_horiz_(penalty.rule3_1_pat, penalty.rule3_1_next)
        penalty_rule3 += self.calc_rule3_horiz_(penalty.rule3_2_pat, penalty.rule3_2_next)
 
        # vertical
        penalty_rule3 += self.calc_rule3_vert_(penalty.rule3_1_pat, penalty.rule3_1_next)
        penalty_rule3 += self.calc_rule3_vert_(penalty.rule3_2_pat, penalty.rule3_2_next)

        return penalty_rule3


    #
    def calc_rule4(self) -> int:
        penalty_rule4 = 0
        total = self.d * self.d
        black = 0

        for row in range(self.d):
            for col in range(self.d):
                if self.qr[row,col] == 0:
                    black += 1

        percent = round(black / total * 100)
        prev = percent - percent % 5
        next = prev + 5
        low = abs(prev-50) // 5
        hgh = abs(next-50) // 5
        penalty_rule4 = min(low,hgh) * 10
        return penalty_rule4
