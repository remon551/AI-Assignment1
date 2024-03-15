:- consult(data).
%%%%%%%%%%%%%helpers%%%%%%%%%%%%%%%%%%%%
append_order([], L, L).
append_order([H|T], L2, [H|NT]):-
	append_order(T, L2, NT).

is_in(_, []) :- false.
is_in(Item, [Item|_]) :- !.
is_in(Item, [_|T]) :- 
    is_in(Item, T).


list_length(List, Length):-
	list_length(List, 0, Length).
list_length([], TempLength, TempLength).
list_length([_|T], TempLength, Length):-
	TempLength1 is TempLength + 1,
	list_length(T, TempLength1, Length).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	



%%%%%%%%first problem%%%%%%%%%%%%%%%%%%%
list_orders(CustomerName, Orders) :- 
    customer(CustomerID, CustomerName),
    collect_orders(CustomerID, [], Orders), !.

collect_orders(CustomerID, TempList,Orders):-
	order(CustomerID, OrderID, Items),
	\+ is_in(order(CustomerID, OrderID, Items), TempList),
	collect_orders(CustomerID, [order(CustomerID, OrderID, Items)|TempList], Orders).
	
collect_orders(_, TempList, TempList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%second problem%%%%%%%%%%%%%%%%%%%
countOrdersOfCustomer(CustomerName, Number) :- 
    customer(CustomerID, CustomerName),
    countOrdersOfCustomer(CustomerID, [], 0, Number), !.

countOrdersOfCustomer(CustomerID, TempList, TempNumber,Number):-
	order(CustomerID, OrderID, Items),
	\+ is_in(order(CustomerID, OrderID, Items), TempList),
	TempNumber2 is TempNumber+1,
	countOrdersOfCustomer(CustomerID, [order(CustomerID, OrderID, Items)|TempList], TempNumber2, Number).
	
countOrdersOfCustomer(_, _,TempNumber, TempNumber).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%third problem%%%%%%%%%%%%%%%%%%%
getItemsInOrderById(CustomerName, OrderID, Items) :- 
	customer(CustomerID, CustomerName),
	order(CustomerID, OrderID, Items).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%forth problem%%%%%%%%%%%%%%%%%%%
getNumOfItems(CustomerName, OrderID, Length) :- 
	customer(CustomerID, CustomerName),
	order(CustomerID, OrderID, Items),
	list_length(Items, Length).	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
