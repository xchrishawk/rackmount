# -- Variables --

TARGET		:= rackmount
SRC_DIR		:= src
MAIN_MODULE	:= main
RACO		:= raco

# -- Rules --

.PHONY : all clean rebuild

all : $(TARGET)

clean :
	$(RM) $(TARGET)

rebuild : clean all

$(TARGET) : $(SRC_DIR)/*.rkt
	$(RACO) exe -o $@ $(SRC_DIR)/$(MAIN_MODULE).rkt
