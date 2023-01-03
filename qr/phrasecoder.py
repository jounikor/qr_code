#
# Handle encoding of phrases and dividing the content into
# groups & blocks.
#

class encode(object):
    """
    This class implements encoding of arbitrary phrases using one of the
    supported QR-code encoding modes.
    """

    # Modes..
    QR_MODE_NONE = 0b0000
    QR_MODE_NUMERIC = 0b0001
    QR_MODE_ALPHANUMERIC = 0b0010
    QR_MODE_BYTE = 0b0100
    QR_MODE_KANJI = 0b1000

    # Mapping of modes on each version to maximum length in bits
    level_1_9_size_tab_   = [0,10, 9,0, 8,0,0,0, 8]
    level_10_26_size_tab_ = [0,12,11,0,16,0,0,0,10]
    level_27_40_size_tab_ = [0,14,13,0,16,0,0,0,12]

    #
    alphanumeric_map_ = {
        "0":0,"1":1,"2":2,"3":3,"4":4,"5":5,"6":6,"7":7,"8":8,"9":9,
        "A":10,"B":11,"C":12,"D":13,"E":14,"F":15,"G":16,"H":17,"I":18,
        "J":19,"K":20,"L":21,"M":22,"N":23,"O":24,"P":25,"Q":26,"R":27,
        "S":28,"T":29,"U":30,"V":31,"W":32,"X":33,"Y":34,"Z":35, 
        " ":36,"$":37,"%":38,"*":39,"+":40,"-":41,".":42,"/":43,":":44
    }

    #
    def __init__(self,**kwargs):
        """
        **kwargs:
        ---------
        mode : int
            QR-code phrase encoding mode. Can be on of the following:
                QR_MODE_ALPHANUMERIC
                QR_MODE_NUMERIC
                QR_MODE_BYTE
                QR_MODE_KANJI
        version : int
            QR-code version between 1 and 40.
        """
        self.mode = encode.QR_MODE_ALPHANUMERIC
        self.version = 1

        if ("mode" in kwargs):
            self.mode = kwargs["mode"]

        if ("version" in kwargs):
            self.version = kwargs["version"]

        if (self.version < 10 and self.version > 0):
            self.size_bits = encode.level_1_9_size_tab_[self.mode]
        elif (self.version < 27):
            self.size_bits = encode.level_10_26_size_tab_[self.mode]
        elif (self.version <= 40):
            self.size_bits = encode.level_27_40_size_tab_[self.mode]
        else:
            raise ValueError(f"Invalid QR-code version {self.version}")

        if (self.mode == encode.QR_MODE_ALPHANUMERIC):
            self.encode_phrase = self.encode_alphanumeric_
        elif (self.mode == encode.QR_MODE_BYTE):
            self.encode_phrase = self.encode_byte_
        elif (self.mode == encode.QR_MODE_NUMERIC):
            self.encode_phrase = self.encode_numeric_
        elif (self.mode == encode.QR_MODE_KANJI):
            self.encode_phrase = self.encode_kanji_
        else:
            raise ValueError(f"Invalid QR-code mode {bin(self.mode)}")

    #
    def encode_with_trailer_(self, bin_str : str, max_len : int) -> bytearray:
        """Internal method for adding trailing zeros and padding.

            Parameters:
            -----------
            bin_str : str
                Input phrase as a binary string.
            max_len : int
                Maximum length of the encoded phrase (in octets).
            
            Raises:
            -------
            ValueError
                If the encoded phrase is longer than maximum codewords allowed by QR-code.
            
            Return:
            -------
                bytearray containing the encoded phrase.
        """
        # terminating zeroes
        if (len(bin_str)+4 <= max_len*8):
            bin_str = bin_str + "0000"
        else:
            bin_str = bin_str + "0000"[0:max_len*8 - len(bin_str)]
            print("non 4 termination")

        # align to 8 bits
        bin_str = bin_str + "00000000"[0:-len(bin_str) % 8]
        # convert to bytearray
        encoded = bytearray([int(bin_str[i:i+8],2) for i in range(0,len(bin_str),8)])

        pad = 0b11101100
        pos = len(encoded) 

        if (pos > max_len):
            raise ValueError("Phrase to be encoded is too long for QR-code version and mode")

        while (pos < max_len):
            encoded.append(pad)
            pad = pad ^ 0b11111101
            pos += 1

        return encoded

    #
    def encode_preamble_(self, length : int) -> str:
        """Internal method for creating the preample with the mode and phrase length.

            Parameters:
            -----------
            length : int
                The length of the phrase to be encoded.
                        
            Return:
            -------
                str containing the encoded mode and length in binary format.
        """

        return f"{self.mode:04b}{length:0{self.size_bits}b}"

    #
    def encode_byte_(self,phrase : str, max_len : int) -> bytearray:
        """Internal method for encoding a phrase in byte mode.

            Parameters:
            -----------
            phrase : str
                Input phrase to be encoded.
            max_len : int
                The maximum length of the codewords for this QR-code.
                        
            Return:
            -------
                bytearray containing the entire encoded phrase with preample, trailer and padding.
        """
        # Mode and phrase length
        bin_str = self.encode_preamble_(len(phrase))
        tmp = phrase.encode("UTF-8")

        for n in range(len(phrase)):
            bin_str = bin_str + f"{tmp[n]:08b}"

        return self.encode_with_trailer_(bin_str,max_len)

    #
    def encode_alphanumeric_(self,phrase,max_len)->bytearray:
        if (type(phrase) is not str):
            raise TypeError(f"Input phrase must be string got '{type(phrase)}'")
        
        # start encoding..
        n = 0

        # Should check if the phrase fits before starting to encode:
        # B = 4 + C + 11(D DIV 2) + 6(D MOD 2)

        # Mode and phrase length
        bin_str = self.encode_preamble_(len(phrase))
        
        # encode payload
        while (n+1 < len(phrase)):
            upper = phrase[n].upper()
            lower = phrase[n+1].upper()

            if (upper not in encode.alphanumeric_map_ or lower not in encode.alphanumeric_map_):
                raise ValueError(f"Illegal characters in the input phrase")

            alnum = encode.alphanumeric_map_[upper] * 45 + encode.alphanumeric_map_[lower]
            #print(f"{upper},{lower},{alnum:04x},{alnum:011b}")
            bin_str = bin_str + f"{alnum:011b}"
            n = n + 2
        else:
            if (n < len(phrase)):
                if (phrase[n].upper() not in self.alphanumeric_map_):
                    raise ValueError(f"Illegal characters in the input phrase")
                
                bin_str = bin_str + f"{encode.alphanumeric_map_[phrase[n].upper()]:06b}"

        #print(bin_str)
        return self.encode_with_trailer_(bin_str,max_len)

    #
    def encode_numeric_(self,phrase,max_len)->bytearray:
        if (type(phrase) is not str):
            raise TypeError(f"Input phrase must be string got '{type(phrase)}'")
        
        # start encoding..
        n = 0

        # Mode and phrase length
        bin_str = self.encode_preamble_(len(phrase))
        
        # encode payload
        r = len(phrase) % 3
        n = 0

        while (n + 2 < len(phrase)):
            num = int(phrase[n:n+3])            
            bin_str = bin_str + f"{num:010b}"
            n += 3

        if (r):
            num = int(phrase[n:])

            if (r == 1):
                bits = 4
            else:
                bits = 7

            bin_str = bin_str + f"{num:0{bits}b}"


        return self.encode_with_trailer_(bin_str,max_len)
    
    #
    def encode_kanji_(self,phrase,max_len)->bytearray:
        raise NotImplementedError("Phrase encoding in Kanji not implemented")

    #
    def encode_phrase(self,phrase,max_len)->bytearray:
        # place holder
        raise NotImplementedError("Something went wrong..")
 


