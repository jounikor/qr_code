ASM = sjasmplus 

EXEC = qrcoder
SRC = qrcoder.asm
MAP = $(EXEC).map
SLD = $(EXEC).sld
#OBJ = $(SRC:.cc=.o)
ASMFLAGS = --sld=$(SLD) --sym=$(MAP) --fullpath -DDOTCMD

all: $(EXEC)

$(EXEC): $(SRC)
	$(ASM) $(ASMFLAGS) $(SRC)

clean:
	rm -rf $(MAP) $(SLD)
