from qr import qrcoder as q
from PIL import Image as im



qr = q.encode("3-L")
#qr_code = qr.generate_qr_code("https://board.esxdos.org/")
#print(qr.get_mask())

#ima = im.fromarray(qr_code, mode="P")
#ima.show()

qr_code = qr.generate_qr_code("http://www.deadcoderssociety.net/")
#print(qr.get_mask())

ima = im.fromarray(qr_code, mode="P")
ima.show()

