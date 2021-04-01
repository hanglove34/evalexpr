##
## EPITECH PROJECT, 2018
## MAKEFILE
## File description:
## Makefile
##

SRC	=   app/Main.hs

NAME	=	funEvalExpr

all:	$(NAME)

$(NAME):	$(SRC)
		stack build --copy-bins --local-bin-path .
		mv funEvalExpr-exe $(NAME)

clean:
	rm -rf .stack-work funEvalExpr.cabal
	rm -rf $(NAME)

fclean: clean
	$(RM) $(NAME)

re: fclean all
