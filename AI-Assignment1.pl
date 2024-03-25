:- consult(data).
%%%%%%%%%%%%%helpers%%%%%%%%%%%%%%%%%%%%
% appends order to a given list %
append_order([], L, L).
append_order([H|T], L2, [H|NT]):-
	append_order(T, L2, NT).

% checks if item is in list %
is_in(_, []) :- false.
is_in(Item, [Item|_]) :- !.
is_in(Item, [_|T]) :- 
    is_in(Item, T).

% get the length of a list. %
list_length(List, Length):-
	list_length(List, 0, Length).
list_length([], TempLength, TempLength).
list_length([_|T], TempLength, Length):-
	TempLength1 is TempLength + 1,
	list_length(T, TempLength1, Length).

% Check If The Item Is Boycotted. %
isBoycotted(Item) :-
    item(Item, Company, _)
    , boycott_company(Company, _).


% Remove The Boycotted Items From The Item List. %
removeBoycottItems([], []). % Base Case %

removeBoycottItems([Item|Rest], NewItemList) :-  % Boycott Item %
        isBoycotted(Item)
        , removeBoycottItems(Rest, NewItemList).


removeBoycottItems([Item|Rest], NewItemList) :-  % Not Boycott Item %
        \+ isBoycotted(Item),
        removeBoycottItems(Rest, RemainingList),
        NewItemList = [Item|RemainingList].


% Replace Boycotted Items From The Item List. %
replaceBoycottItems([], []). % Base case %

replaceBoycottItems([Item|Rest], NewItemList) :- % Boycott Item %
    isBoycotted(Item),
    alternative(Item, AlternativeItem),
    replaceBoycottItems(Rest, RemainingList),
    NewItemList = [AlternativeItem|RemainingList].


replaceBoycottItems([Item|Rest], NewItemList) :- % Not Boycott Item %
    \+ isBoycotted(Item),
    replaceBoycottItems(Rest, RemainingList),
    NewItemList = [Item|RemainingList].


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	



%%%%%%%%%%%1st problem%%%%%%%%%%%%%%%%%%%
list_orders(CustomerName, Orders) :- 
    customer(CustomerID, CustomerName),
    collect_orders(CustomerID, [], Orders), !.

collect_orders(CustomerID, TempList,Orders):-
	order(CustomerID, OrderID, Items),
	\+ is_in(order(CustomerID, OrderID, Items), TempList),
	collect_orders(CustomerID, [order(CustomerID, OrderID, Items)|TempList], Orders).
	
collect_orders(_, TempList, TempList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%2nd problem%%%%%%%%%%%%%%%%%%%
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



%%%%%%%%%%%%3rd problem%%%%%%%%%%%%%%%%%%%
getItemsInOrderById(CustomerName, OrderID, Items) :- 
	customer(CustomerID, CustomerName),
	order(CustomerID, OrderID, Items).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%4th problem%%%%%%%%%%%%%%%%%%%
getNumOfItems(CustomerName, OrderID, Length) :- 
	customer(CustomerID, CustomerName),
	order(CustomerID, OrderID, Items),
	list_length(Items, Length).	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%5th problem%%%%%%%%%%%%%%%%%%%
calcPriceOfOrder(CustomerName, OrderID, TotalPrice) :-
    customer(CustomerID, CustomerName),
    order(CustomerID, OrderID, Items),
    calculateItemsPrice(Items, OrderTotal),
    TotalPrice is OrderTotal.

calculateItemsPrice([], 0).
calculateItemsPrice([ItemID|Rest], Total) :-
    item(ItemID, _, Price),
    calculateItemsPrice(Rest, RemainingTotal),
    Total is Price + RemainingTotal.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%6th problem%%%%%%%%%%%%%%%%%%%
isBoycott(Item) :-
    item(Item, Company, _)
    , boycott_company(Company, _).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%7th problem%%%%%%%%%%%%%%%%%%%
whyToBoycott(CompanyOrItemName,Justification):-
    boycott_company(CompanyOrItemName, Justification)
    ; item(CompanyOrItemName, CompanyName, _)
    , boycott_company(CompanyName, Justification).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%8th problem%%%%%%%%%%%%%%%%%%%%
removeBoycottItemsFromAnOrder(Username, OrderID, NewItemList) :-
    customer(CustomerID, Username)
    , order(CustomerID, OrderID, ItemList),
    removeBoycottItems(ItemList, NewItemList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%9th problem%%%%%%%%%%%%%%%%%%
replaceBoycottItemsFromAnOrder(Username, OrderID, NewItemList) :-
    customer(CustomerID, Username),
    order(CustomerID, OrderID, ItemList),
    replaceBoycottItems(ItemList, NewItemList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%10th problem%%%%%%%%%%%%%%%%%%%
calcPriceAfterReplacingBoycottItemsFromAnOrder(Username, OrderID, NewList, TotalPrice):-
    replaceBoycottItemsFromAnOrder(Username,OrderID, NewList),
    calculateItemsPrice(NewList, TotalPrice).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%11th problem%%%%%%%%%%%%%%%%%
getTheDifferenceInPriceBetweenItemAndAlternative(Item, AlternativeItem, DiffPrice):-
    alternative(Item, AlternativeItem), !,
    item(Item, _, ItemPrice),
    item(AlternativeItem, _, AlternativeItemPrice),
    DiffPrice is ItemPrice - AlternativeItemPrice.

getTheDifferenceInPriceBetweenItemAndAlternative(Item, Item, 0).


%%%%%%%%%%%%12th problem%%%%%%%%%%%%%%%%%
:- dynamic(item/3). % Declare item/3 as dynamic

% Insert a new item
add_item(Item, Company, Price) :-
    assert(item(Item, Company, Price)).

% Remove an item
remove_item(Item, Company, Price) :-
    retract(item(Item, Company, Price)).

% Insert a new alternative
insertAlternative(Item1, Item2) :-
    assert(alternative(Item1, Item2)).

% Remove an alternative
removeAlternative(Item1, Item2) :-
    retract(alternative(Item1, Item2)),
    retract(alternative(Item2, Item1)). % Remove bidirectional association

% Insert a new boycotted company
insertBoycott(Company) :-
    item(Item, Company, Price),
    assert(boycott_company(Company)).

% Remove a boycotted company
removeBoycott(Company) :-
    item(Item, Company, Price),
    retract(boycott_company(Company)).

