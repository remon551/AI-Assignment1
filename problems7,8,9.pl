:- consult(data).

% ================== Helpers =========================== %
%
% 1- Check If The Item Is Boycotted. %
isBoycotted(Item) :-
    item(Item, Company, _)
    , boycott_company(Company, _).

% ====================================================== %
%
% 2- Remove The Boycotted Items From The Item List. %
removeBoycottItems([], []). % Base Case %

removeBoycottItems([Item|Rest], NewItemList) :-  % Boycott Item %
        isBoycotted(Item)
        , removeBoycottItems(Rest, NewItemList).


removeBoycottItems([Item|Rest], NewItemList) :-  % Not Boycott Item %
        \+ isBoycotted(Item),
        removeBoycottItems(Rest, RemainingList),
        NewItemList = [Item|RemainingList].

% ====================================================== %
%
% 3- Replace Boycotted Items From The Item List. %
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

% ====================================================== %
% ================== Problems =========================== %
%
% Problem Number 7%
whyToBoycott(CompanyOrItemName,Justification):-
    boycott_company(CompanyOrItemName, Justification)
    ; item(CompanyOrItemName, CompanyName, _)
    , boycott_company(CompanyName, Justification).

% ====================================================== %
%
% Problem Number 8%
removeBoycottItemsFromAnOrder(Username, OrderID, NewItemList) :-
    customer(CustomerID, Username)
    , order(CustomerID, OrderID, ItemList),
    removeBoycottItems(ItemList, NewItemList).

% ====================================================== %
%
% Problem Number 9%
replaceBoycottItemsFromAnOrder(Username, OrderID, NewItemList) :-
    customer(CustomerID, Username)
    , order(CustomerID, OrderID, ItemList),
    replaceBoycottItems(ItemList, NewItemList).







