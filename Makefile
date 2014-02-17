

SRC=$(PWD)/src/
EBIN=$(PWD)/ebin/
INCLUDE=$(PWD)/include/


all: compile 

compile: 
	erlc -o $(EBIN) -I $(INCLUDE) $(SRC)*.erl

clean:
	@rm -rf $(EBIN)*.beam
