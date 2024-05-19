##
## EPITECH PROJECT, 2021
## HAL
## File description:
## HAL
##

BINARY_PATH 	:=	$(shell stack path --local-install-root)
NAME 			= 	hal

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re