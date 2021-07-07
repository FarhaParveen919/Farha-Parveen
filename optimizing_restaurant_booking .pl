/* All libraries used by this code. */
:- use_module( [library(clpfd),library(lists)] ).
:- expects_dialect( sicstus ).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- set_prolog_flag(double_quotes, chars).
:- use_module([library(pcre)]).
:- set_prolog_flag(double_quotes, chars).

/* Delay the order of execution of a predicate that set the allocated order variables : 
1. Order Number, 2. Order Start Time, 3. Boolean Variable that determines whether a table is occupied or not, e.g. [1, 0, 0] indicates that the order occupies only the first table that can accomodate 2 people, 
4. Order Duration (1 for theatre menu and 2 for standard menu) allocated to the Order, 5. Number of People, 6. Date in the format (Day,Month) */
:- block allocated_order( -,-,-,-,-, -).


%%%%%%%%%%%% --------------------------------------------------------------------------------------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% This code parses all the restaurant orders based on natural language processing  with DCG, allocates the tables in the restaurant as per the order,
%%%%% so that no two orders occupy the same table, at a given time, and prints out the orders and table allocation in a human readable format with DCG.
%%%%% To run the code call the predicate :  read_allocate_and_print_orders(AllocatedOrderList). 
%%%%% The testing of the code is described in the last part of the document, and shows different input orders that are parsed, allocated and printed by the code. 
/*Note that a list of allocated_order(OrderNo, OrderStartTime, OrderTableOccupation, OrderDuration, OrderPeople, OrderDate) is the final solution of the problem*/
%%%%%%%%%%%% --------------------------------------------------------------------------------------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% The following predicate contains the details of the orders.
%%% The format is as follows :
%%% all_orders([Order1, Order2, Order3, Order4,.... OrderN])
%%% Each order is a list of words, as per the given preprocessed  Prolog version.
%%% Note : I have commented out this section here. The last part of the document populates all_orders(...), for different test cases.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
all_orders(
    [[table,for,2,at,20,':',00,on,18,march],
    [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],
    [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,'/',03],
    [can,i,book,a,table,at,8,pm,for,2,people,on,the,19,th,of,march,for,the,standard,menu,please]
    [reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu]
    [9,people,on,18,th,of,march]
    [book,6,of,us,in,on,18,march,at,20,':',00],
    [reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]
    ]
).*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% The following predicate sets the details of the tables in the restaurant and the number of people that can be fit into the table.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** table(+TableID : integer, +TableCapacity : Integer)
* @param TableID : Table ID,
* @param TableCapacity : Table Capacity
*/

table(1,2).
table(2,3).
table(3,4).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% The following predicate maps the month onto its respective integer.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** month(+MonthName : Term, -MonthNo : Integer)
* @param MonthName : Month Name,
* @param MonthNo : Month Number
*/


month(january,1).
month(february,2).
month(march,3).
month(april,4).
month(may,5).
month(june,6).
month(july,7).
month(august,8).
month(september,9).
month(october,10).
month(november,11).
month(december,12).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% The following predicate maps the menu type onto its duration in hours.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** menu_type(+MenuType : Term, +Duration: Integer)
* @param MenuType : Menu Type on whether it is a standard or theatre menu.
* @param Duration : Duration in hours for the menu.  
*/

menu_type(standard,2).
menu_type(theatre,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Implementing NLP Methodology using DCG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following predicate reads all orders from the predicate all_orders, counts the number of orders, and parses them.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
* read_all_orders(-ParsedOrders : List)
* True if all the orders can be read and parsed, and their details can be populated in the list ParsedOrders.  

* @param ParsedOrders : List of order(_, Time, Hours, People, (Day,Month)), containing the details of each order.
*/

read_all_orders(ParsedOrders):-
    all_orders(PreprocessedOrders),
    length(PreprocessedOrders, NumberOfOrders),
    findall(N, between(1, NumberOfOrders, N), OrderNumberList),
    parse_orders(PreprocessedOrders, ParsedOrders,OrderNumberList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following predicate parses all orders (in their String Format) and populates a list containing details of each order
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
* parse_orders(+OrdersInStringListFomat: list of lists, -ParsedOrders : list, +OrderNumbers : List)
* True if all the orders can be parsed and their details can be populated in a list ParsedOrders.  

* @param OrdersInStringListFomat : List of "lists", where each "list" correspond to one order.
* @param ParsedOrders : List of order(_, Time, Hours, People, (Day,Month)), containing the details of each order.
* @param OrderNumbers : List of order numbers associated with the orders.
*/

parse_orders([],[],[]).
parse_orders([HeadOrderStringListFormat | TailOrdersStringListFormat], [order(OrderNo, Time, Hours, People, (Day,Month)) | TailOrdersInRequiredFormat], [OrderNo|TailOrdersNo]):-
    parse_one_order(HeadOrderStringListFormat,order(OrderNo, Time, Hours, People, (Day,Month))),
    parse_orders(TailOrdersStringListFormat, TailOrdersInRequiredFormat,TailOrdersNo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following predicate parses one order and estimates its parameters such as order time, number of hours in the order based on menu type
%%%%%% number of people and date
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
* parse_one_order(+OrderInStringListFomat: list, -order(_, Time: Integer, Hours: Integer, People : Integer, (Day: Integer,Month : Integer)))
* True if all the terms in the StringList (i.e. Strings) can be converted to atoms, and the results are in the variable AtomList.   

* @param OrderInStringListFomat : List containing all the strings in one order.
* @param order(_, Time, Hours, People, (Day,Month)): term containing details of the order.
*/

parse_one_order(OrderInStringListFomat,order(_, Time, Hours, People, (Day,Month))):-
    convert_to_atoms(OrderInStringListFomat, OrderInAtomicListFomat),
    estimate_parameters(OrderInAtomicListFomat,People,Time,(Day,Month),Menu),
    menu_type(Menu, Hours).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following predicate convert a list of strings(from each order in the predicate all_orders) to a list of atoms for processing by DCG. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
* convert_to_atoms(+StringList: list, -AtomList: List)
* True if all the the terms in the StringList (i.e. Strings) can be converted to atoms, and the results are in the variable AtomList.   

* @param StringList : List containing all the strings in one order.
* @param AtomList: List containing the correponding atoms in the order, based on conversion from string to atom.
*/

convert_to_atoms([],[]).
convert_to_atoms([HeadString|TailStrings],[HeadAtom|TailAtoms]):-
    atom_string(HeadAtom, HeadString),
    convert_to_atoms(TailStrings, TailAtoms).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following predicate estimate the parameters of the order, based on the sentence represented as list of atoms. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
* estimate_parameters(+List: atomlist, -N: Integer, -T: Integer, -D: Integer, -Menu: Term)
* True if a sentence (i.e. order) given by a list of atoms can be parsed into the number of people in the order, order time, order date and order menu 

* @param List : Atom list containing the sentence (i.e. the order in preprocessed prolog format)
* @param N: Number of people in the order
* @param T : Order Time.
* @param (D,M) : Day (D) and Month (M) of the order.
* @param Menu : Menu type of the order, i.e. standard or theatre.
*/

/*True if the sentence (represented by the atomlist) specifies all infomation including number of people, order time, order date and menu*/
estimate_parameters(List,N,T,(D,M),Menu):-
    phrase(sentence(N,T,(D,M)),List,[]),
    phrase(sentence_menu(Menu),List,[]).

/*True if the sentence (represented by the atomlist) specifies only the number of people, date and menu. 
The time is then considered variable.*/
estimate_parameters(List,N,T,(D,M),Menu):-
    \+ phrase(sentence(N,T,(D,M)),List,[]),
    phrase(sentence_short(N,(D,M)),List,[]),
    %T in 19..23,
    phrase(sentence_menu(Menu),List,[]).


/*True if the sentence (represented by the atomlist) specifies only number of people, order time and order date. 
Menu is not specified in the sentence, and will therefore be considered as standard. */
estimate_parameters(List,N,T,(D,M),Menu):-
    phrase(sentence(N,T,(D,M)),List,[]),
    \+ phrase(sentence_menu(Menu),List,[]),
    Menu = standard.

/*True if the sentence (represented by the atomlist) specifies only number of people and date. 
Menu is not specified in the sentence, and will therefore be considered as standard. Time is not specified and considered as variable.*/
estimate_parameters(List,N,T,(D,M),Menu):-
    \+ phrase(sentence(N,T,(D,M)),List,[]),
    phrase(sentence_short(N,(D,M)),List,[]),
    %T in 19..23,
    \+ phrase(sentence_menu(Menu),List,[]),
    Menu = standard.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following DCG commands are used by the above predicates to parse an order
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*The following command is used to denote any set of literals before or after specific literals in DCG commands */
... --> [] | [_], ... .


/*The following DCG commands is used to parse a sentence with number of people in the order, order time and order date*/
/*Depending on the arrangement of people, time and date in the sentence, different permutations are implemented. */
sentence(N,T,(D,M)) --> ...,number_phrase(N),...,time_phrase(T),..., date_phrase(D,M), ... .
sentence(N,T,(D,M)) --> ...,number_phrase(N),...,date_phrase(D,M), ..., time_phrase(T), ... .
sentence(N,T,(D,M)) --> ..., date_phrase(D,M),...,number_phrase(N),..., time_phrase(T), ... .
sentence(N,T,(D,M)) --> ..., date_phrase(D,M),...,time_phrase(T),..., number_phrase(N), ... .
sentence(N,T,(D,M)) --> ..., time_phrase(T),...,date_phrase(D,M), ..., number_phrase(N),... .
sentence(N,T,(D,M)) --> ..., time_phrase(T),...,number_phrase(N),...,date_phrase(D,M), ... .

/*The following DCG commands correspond to parsing a sentence only if  number of people and date are given.*/
sentence_short(N,(D,M)) --> ...,number_phrase(N),..., date_phrase(D,M), ... .
sentence_short(N,(D,M)) --> ...,date_phrase(D,M), ..., number_phrase(N), ... .

/*The following DCG command correspond to parsing a sentence to obtain the menu type in the order*/
sentence_menu(Menu) --> ..., menu_phrase(Menu), ... .


/*The phrase associated with the menu for the order in a sentence*/
menu_phrase(Menu) --> [Menu], [menu], {menu_type(Menu,_)}.

/*The phrase associated with  the number of people in the order, in a sentence*/
number_phrase(N) --> conjunction,number(N).
number_phrase(N) --> number(N),conjunction.
number_phrase(N) --> number(N),noun.

/*The phrase associated with the order time, in a sentence*/
time_phrase(T) --> preposition, time(T).

/*The phrase associated with the order date, in a sentence*/
date_phrase(D,M) --> date_preposition, date(D,M).
date_phrase(D,M) --> date_preposition,determiner, date(D,M).


/*The connecting words such as determiner, conjunction, noun and preoposition for the phrases in a sentence*/
/*Specific category for time and date preposition and conjunction.*/
determiner -->[the].

conjunction -->[for] | [of].

noun --> [people].

preposition --> [at].

time_preposition_12_hrs --> [oclock] | [pm].
time_preposition_24_hrs --> [':'].

date_conjunction --> ['/'].
date_preposition -->  [on].
ordinal --> [th].


/*DCG commands to extract day and month of the order in any given format (e.g. march 18, 18 march,  18/03,  march 18th, 19th of march  etc.)*/

/*e.g. march 18, 18 march*/
date(Day,Month) --> [Atom1], [Atom2], 
                                                {
                                                   call(is_date([Atom1, Atom2], Day, Month))
                                                }.


/*e.g. 18/03*/
date(Day,Month) --> [Atom1],date_conjunction, [Atom2],
                                                {
                                                   call(is_date([Atom1, Atom2], Day, Month))
                                                }.

/*e.g. March 18th*/
date(Day,Month) --> [Atom1], [Atom2], ordinal,
                                                {
                                                   call(is_date([Atom1, Atom2], Day, Month))
                                                }.

/*e.g. 19th of March*/
date(Day,Month) --> [Atom1], ordinal, conjunction, [Atom2],
                                                {    
                                                   call(is_date([Atom1, Atom2], Day, Month))
                                                }.



/*DCG commands to extract time of the order in any given format (e.g. 9 pm, 7 oclock, 20:00)*/

/*e.g. 9 pm, 7 oclock */
time(T) --> [TimeAtom],time_preposition_12_hrs,
                                                {
                                                   call(is_time_12hrs(TimeAtom,T))
                                                }.

/*e.g. 20:00 */
time(T) --> [TimeAtom], time_preposition_24_hrs,
                                                {
                                                   call(is_time_24hrs(TimeAtom,T))
                                                }.



/*DCG command to extract number of people in the order*/
number(N) --> [NumberAtom], 
                                                {
                                                  call(atom_number(NumberAtom, N)), N > 0
                                                }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following predicates estimate the time in 24 hours, given an atom parsed by DCG .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
* is_time_12_hrs(+TimeAtom :atom, -Time24Hours :integer)
* True if the atom corresponding to time (in 12 hr format), can be mapped onto a time in 24 hour format such that the time is 
* is between 19 (restaurant opening time) and 23 hrs (restaurant closing time )

* @param TimeAtom : Atom correponding to time (in 12 hr format) in the DCG parser
* @param Time24Hours : The time of the order in 24 hrs. 
*/

is_time_12hrs(TimeAtom, Time24Hours):-
    atom_number(TimeAtom, Number),
    Time24Hours is Number + 12,
    Time24Hours in 19..23.


/**
* is_time_24_hrs(+TimeAtom :atom, -Time24Hours :integer)
* True if the atom corresponding to time (in 24 hr format), can be mapped onto a time in 24 hour format such that the time is 
* is between 19 (restaurant opening time) and 23 hrs (restaurant closing time )

* @param TimeAtom : Atom correponding to time (in 24 hr format) in the DCG parser
* @param Time24Hours : The time of the order in 24 hrs. 
*/

is_time_24hrs(TimeAtom, Time24Hours):-
    atom_number(TimeAtom, Time24Hours),
    Time24Hours in 19..23.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following predicates estimate the date with Day and month, given two atoms parsed by DCG in the date phrase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/**
* is_date(+[Atom1 :atom, Atom2 :atom], -Day :Integer, -Month : Integer)
* True if the two atoms (corresponding to the date phrase in the order) can be mapped onto their respective day and month 

* @param Atom 1: Atom correponding to the day or month
* @param Atom 2: Atom correponding to the month or day
* @param Day : The day of the order.
* @param Month : The month of the order. 
*/

/*True if both atoms in the date are numbers. e.g. 18, 3 when parsing 18/03*/
is_date([Atom1, Atom2], Day, Month):-
    atom_number(Atom2, Month),
    atom_number(Atom1, Day).

/*True if first atom in the date correspond to day, and second atom correspond to month (as string) e.g. 18 march*/
is_date([Atom1, Atom2], Day, Month):-
    month(Atom2, Month),
    atom_number(Atom1, Day).
    
/*True if first atom in the date correspond to month (as string), and second atom correspond to day e.g. march 18*/
is_date([Atom1, Atom2], Day, Month):-
    month(Atom1, Month),
    atom_number(Atom2, Day).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Constrain the System for Restaurant Booking   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following predicate is used to constrain the parsed orders as per the criteria given in the question,
%%%%%% and estimate the order time (if not specified) and allocate appropriate tables to each order. The output of this predicate
%%%%%% is a list (AllocatedOrderList) containing details (order number, time, occupied tables, duration, people, date) of each order.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/**
* allocate_orders(+ParsedOrderList:List, -AllocatedOrderList: List)
* True if we can constrain the parsed orders as per the requirements in the question, and estimate the order time (if not specified in the order) and allocate tables to each order.

* @param ParsedOrderList: List containing details of the parsed order including OrderNo, OrderTime (variable if not specified), Duration, OrderPeople and OrderDate
* @param AllocatedOrderList: List containing details of each order including OrderNo, OrderTime, TableOccupationList, Duration, OrderPeople and OrderDate
*/ 
								   
allocate_orders(ParsedOrderList, AllocatedOrderList) :-
    length(ParsedOrderList,NumberOfOrders),
    length(AllocatedOrderList, NumberOfOrders ),
    length(OrderNumberList, NumberOfOrders),
    all_different(OrderNumberList),
    constrain_each_order(OrderNumberList, Variables,ParsedOrderList, AllocatedOrderList),
    constraint_orders_tables_non_overlapping(AllocatedOrderList),
    flatten(Variables, VariablesFlattened),									  
	labeling( [ff], VariablesFlattened). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following predicate is used to to constain one order, as per the question requirements (1. restaurant opening and closing 
%%%%%% time, 2. order date, 3. order month, 4. duration of the order, and 5. allocating tables to each order, so that the number of people 
%%%%% in the order can be accomodated as per the capacity of the tables).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
* constrain_each_order(+OrderNos : List, -Variables : List, +ParsedOrderList: List,   -AllocatedOrderList: List )
* True if the order can be constrained as per the above 5 requirements in the question.


* @param OrderNos: List containing order numbers. e.g [1,2,3,4,5]
* @param Variables : List containing the parameters to be optimized. The parameters include 
                     1. OrderTime (if it has not been specified in the order) 
                     2. TableOccupationList indicating the occupation of tables in the order. e.g. [1,0,1] indicates that the tables 1 and 3 are occupied.  
                     corresponding to the orders.
* @param ParsedOrderList : List containing details of the orders, as parsed using DCG. 
* @param AllocatedOrderList : List containing details including ORderNo, OrderTime, TableOccupationList, Duration, OrderPeople and OrderDate
*/   

constrain_each_order( [], [], [], [] ). 
constrain_each_order( [OrderNo|OrderNoTail],[OrderTime,TableOccupationList|Variables],[order(OrderNo, OrderTime, Hour, People, (Day, Month)) | ParsedOrderTail], [allocated_order(OrderNo,OrderTime,TableOccupationList,Hour,People, (Day, Month))| AllocatedOrderTail]) :-
                                                           OrderTime #>= 19,
                                                           Hour in 1..2,
                                                           OrderTime + Hour #=< 23,
                                                           Day in 1..31,
                                                           Month in 1..12,
                                                           constrain_table_allocation(TableOccupationList, People), 
                                                           constrain_each_order(OrderNoTail, Variables, ParsedOrderTail, AllocatedOrderTail).               




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following two predicates are used to constraint table allocation, such that the total number of people in the order,
%%%%%% is less than sum of occupancies of the tables to which they are allocated. As a result, all the people in the order can be fit into the given set of tables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/**
* constrain_table_allocation(-TableOccupationList : List, +People : Integer)
* True if table's can be allocated to the order, such that the capacicity of the occupied tables exceed the number of people in the order.

* @param TableOccupationList : List of size equal to number (i.e. 3) of tables. The list indicates whether the table is occupied for the order.
                               This list can contain only zero's or ones. 
                               e.g. [1,0,1] indicates that the tables 1 and 3 are occupied.
* @param People : Number of people in the order
*/   

constrain_table_allocation(TableOccupationList,OrderPeople):-
    findall(X, table(X,_),TableList),
    constrain_table_allocation_(TableList, TableOccupationList, NumChairs),
    NumChairs #>= OrderPeople.


/**
* constrain_table_allocation_(+TableList : List, ?TableOccupationList : List, -NumChairs : Integer)
* True if the capacity of the occupied tables can be estimated, given a list (TableOccupationList) 
* which indicates whether the table is occupied or not by the order.


* @param Table List: List of given tables. e.g. [1,2,3]
* @param TableOccupationList : List containing zeros or ones, indicating whether the tables are occupied. e.g. [1,0,1] indicates that the tables 1 and 3 are occupied for the order.
* @param NumChairs : Total Occupancies of the tables filled by the order.  
*/

constrain_table_allocation_([],[],0).
constrain_table_allocation_([HTable | TTable], [HTableOccupationList | TTableOccupationList], NumChairs):-
    constrain_table_allocation_(TTable, TTableOccupationList,M),
    NumChairs #= HTableOccupationList*TableCapacity + M,
    HTableOccupationList in 0..1,
    table(HTable,TableCapacity).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% The following two predicates are used to ensure that the allocated tables associated with any two orders do not 
%%%%%% overlap in time. i.e. at a particular day and time, two orders should not involve the same table. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/**
* constraint_orders_tables_non_overlapping(?AllocatedOrderList : List)
* True if we can ensure the table allocation associated with any two-orders  do not overlap in time, i.e. at a particular day and time, two orders should not involve the same table. 


* @param AllocatedOrderList: List containing details of each order including OrderNo, OrderTime, TableOccupationList, Duration, OrderPeople and OrderDate
*/  

/*Note : This part of the code has been inspired from the following website, which was used for an entirely different problem. 
The code uses maplist to ensure that any two permutations don't have the same tables occupied at a time. */


%% constraint_orders_tables_non_overlapping this predicate  prevent over-lapping or sharing of tables by different group of people.

constraint_orders_tables_non_overlapping([]).  
constraint_orders_tables_non_overlapping([HeadOrder | TailOrders]) :-
    maplist(tables_non_overlapping(HeadOrder), TailOrders),
    constraint_orders_tables_non_overlapping(TailOrders).


/**
* tables_non_overlapping(+AllocatedOrder1: term,  +AllocatedOrder2: term)
* True if the two allocated orders  do not involve the same table at a time and date.


* @param AllocatedOrder1: Term containing details (order no, time, occupied tables, duration, people, date) of the first order.
* @param AllocatedOrder2: Term containing details (order no, time, occupied tables, duration, people, date) of the second order.
*/  


tables_non_overlapping(allocated_order(_,_,[],_,_,(_,_)),allocated_order(_,_,[],_,_,(_,_))).
tables_non_overlapping(allocated_order(_,OrderTime1, [HeadOrder1TableOccupationList | TailOrder1TableOccupationList], Hour1, _, (Day1, Month1)), 
                allocated_order(_,OrderTime2, [HeadOrder2TableOccupationList | TailOrder2TableOccupationList], Hour2, _, (Day2, Month2))) :-
    (Day1 #= Day2) #/\ (Month1 #= Month2) #/\ (OrderTime2 #>= OrderTime1) #/\ (OrderTime2 #< OrderTime1 + Hour1) #/\ (HeadOrder1TableOccupationList #= 1)   #==> (HeadOrder2TableOccupationList #= 0),
    (Day1 #= Day2) #/\ (Month1 #= Month2) #/\ (OrderTime2 #>= OrderTime1) #/\ (OrderTime2 #< OrderTime1 + Hour1) #/\ (HeadOrder2TableOccupationList #= 1) #==> (HeadOrder1TableOccupationList #= 0),
    (Day1 #= Day2) #/\ (Month1 #= Month2) #/\ (OrderTime1 #>= OrderTime2) #/\ (OrderTime1 #< OrderTime2 + Hour2) #/\ (HeadOrder1TableOccupationList #= 1)   #==> (HeadOrder2TableOccupationList #= 0),
    (Day1 #= Day2) #/\ (Month1 #= Month2) #/\ (OrderTime1 #>= OrderTime2) #/\ (OrderTime1 #< OrderTime2 + Hour2) #/\ (HeadOrder2TableOccupationList #= 1) #==> (HeadOrder1TableOccupationList #= 0),
    tables_non_overlapping(allocated_order(_,OrderTime1, TailOrder1TableOccupationList, Hour1, _, (Day1, Month1)),allocated_order(_,OrderTime2, TailOrder2TableOccupationList, Hour2, _, (Day2, Month2))).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Displaying the results using DCG   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/**
DCG instructions for all the orders indicating the order number, number of people ,  list of tables that is occupied by the order, order time, order duration, and order date. 
*/

dcg_commands([]) --> [].
dcg_commands([allocated_order(OrderNo,OrderTime, TableOccupationList,OrderHours,OrderPeople,(OrderDay,OrderMonth)) |TailOrdersDCG]) --> 
    ['Order Number ', OrderNo, ' consisting of ', OrderPeople, ' people should have them occupying table numbers ', OrderTables, ' at ', 
    OrderTime, ':00', ' for ',OrderHours, ' hours ', 'on ', OrderDay, '/', OrderMonth, '.', '\n'],
    { findall(Index,nth1(Index,TableOccupationList,1),OrderTables) },
    dcg_commands(TailOrdersDCG).


/**
* print_dcg_instructions_on_screen(DCGInstructions :list)

* True if all the DCG instructions are printed on the screen. 

* @param DCGInstructions:  List of Instructions to be printed on the screen. 
*/

print_dcg_instructions_on_screen([]).
print_dcg_instructions_on_screen([HeadInstruction|TailInstructions]) :-
    format('~w', [HeadInstruction]),
    print_dcg_instructions_on_screen(TailInstructions).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main of the program %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
* read_allocate_and_print_orders(-AllocatedOrderList :list)

* True if all the orders can be read and parsed via DCG, allocated to the required tables and time as per the constraint provided in the question
      and printed via DCG. . 

* @param AllocatedOrderList:  List containing details of each order including OrderNo, OrderTime, TableOccupationList, Duration, OrderPeople and OrderDate
*/

read_allocate_and_print_orders(AllocatedOrderList):-
    read_all_orders(ParsedOrderList),
    allocate_orders(ParsedOrderList, AllocatedOrderList),
    dcg_commands(AllocatedOrderList, DCGInstructions,[]),
    print_dcg_instructions_on_screen(DCGInstructions).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Testing the code    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% To test the code, do the following : 
%%%% Step 1 : Specify the list all_orders(), as per one of the following 8 test cases by uncommenting the specific case, and commenting all test other cases.
%%%% Step 2 : call the predicate read_allocate_and_print_orders(AllocatedOrderList)
%%%% Output will be then shown in terminal. This output is also shown in the file for verification. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Test Case 1 : Take the first 4 orders given in the question. 
%%%% To try this case, uncomment all_order(...) below and comment all other all_orders(...).
%%%% The corresponding terminal output as well as its description is also shown.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all_orders(
    [[table,for,2,at,20,':',00,on,18,march],
    [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],
    [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,'/',03],
    [can,i,book,a,table,at,8,pm,for,2,people,on,the,19,th,of,march,for,the,standard,menu,please]
    %[reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu]
    %[9,people,on,18,th,of,march]
    %[book,6,of,us,in,on,18,march,at,20,':',00],
    %[reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]
    ]
).

/* Terminal Output : 

?- read_allocate_and_print_orders(AllocatedOrderList).
Order Number 1 consisting of 2 people should have them occupying table numbers [3] at 20:00 for 2 hours on 18/3.
Order Number 2 consisting of 3 people should have them occupying table numbers [3] at 19:00 for 1 hours on 18/3.
Order Number 3 consisting of 5 people should have them occupying table numbers [1,2] at 20:00 for 2 hours on 18/3.
Order Number 4 consisting of 2 people should have them occupying table numbers [3] at 20:00 for 2 hours on 19/3.
AllocatedOrderList = [allocated_order(1,20,[0,0,1],2,2,(18,3)),allocated_order(2,19,[0,0,1],1,3,(18,3)),allocated_order(3,20,[1,1,0],2,5,(18,3)),allocated_order(4,20,[0,0,1],2,2,(19,3))]

*/

/* Description on the output : Note the following. 
   a. All the orders are parsed correctly with respect to date, number of people.
   b. If time is not specified, the time is parsed correctly (e.g. Order No 1, 3 and 4). Else a time is allocated (e.g. Order No : 2). and we can see that the orders are parsed correctly
   c. The number of hours dedicated for each order is 1 (correpsonding to theatre booking, e.g. order no 2) and 2 (corresponding to standard menu, e.g. order no 4)
   d. If the menu type is not specified, the default  menu is standard type and number of hours is 2 (e.g. order no 1 and 3) 
   e. The code is able to constraint all the orders such that no two tables are occupied by the same order at a given time. 
   f. The capacity of the tables occupied per order is greater than the number of people in the order. Note : Capacity of Table1 is 2, Table2 is 3 and Table3 is 4. 
   All these results are consistent with the results in the question. 
   Note : Order Numbers 1 and 3 will require all the tables (1,2 and 3) to be occupled between 20:00 and 22:00 on March 18. 
   Hence any further standard order lasting two hours on March 18 cannot be added, as the restaurant opens at 19:00 and closes at 23:00. 
   This will be evident in the next testcase where the read_allocate_and_print_orders(AllocatedOrderList) will return false.    
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Test Case 2 : Take the first 5 orders orders in the question. 
%%%% To try this case, uncomment all_order(...) below and comment all other all_orders(...).
%%%% The corresponding terminal output as well as its description is also shown.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
all_orders(
    [[table,for,2,at,20,':',00,on,18,march],
    [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],
    [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,'/',03],
    [can,i,book,a,table,at,8,pm,for,2,people,on,the,19,th,of,march,for,the,standard,menu,please],
    [reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu]
    %[9,people,on,18,th,of,march],
    %[book,6,of,us,in,on,18,march,at,20,':',00],
    %[reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]
    ]
).*/

/* Terminal Output : 
?- read_allocate_and_print_orders(AllocatedOrderList).
false.*/

/* Description on the output : 
   As per the previous test case, Order Numbers 1 and 3 will require all the tables (1,2 and 3) to be occupied between 20:00 and 22:00. 
   Hence any standard order (i.e. the 5th order) lasting two hours on March 18 cannot be added, as the restaurant opens at 19:00 and closes at 23:00. 
   Hence, we get false. Adding other orders that are commented out (e.g. [9,people,on,18,th,of,march], [book,6,of,us,in,on,18,march,at,20,':',00], 
   [reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]) will also return false due to the above reason.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Test Case 3 : Take all the 8 orders in the question. 
%%%% To try this case, uncomment all_order(...) below and comment all other all_orders(...). 
%%%% The corresponding terminal output as well as its description is also shown.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
all_orders(
    [[table,for,2,at,20,':',00,on,18,march],
    [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],
    [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,'/',03],
    [can,i,book,a,table,at,8,pm,for,2,people,on,the,19,th,of,march,for,the,standard,menu,please],
    [reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu],
    [9,people,on,18,th,of,march],
    [book,6,of,us,in,on,18,march,at,20,':',00],
    [reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]
    ]
).*/

/* Terminal Output 
?- read_allocate_and_print_orders(AllocatedOrderList).
false.
*/

/* Description on the output : 
   As per  test case 1, Order Numbers 1 and 3 will require all the tables (1,2 and 3) to be occupied between 20:00 and 22:00. 
   Hence adding any standard order (i.e. the 5th order, 6th or 7th or 8th order or all of them) lasting two hours on March 18 cannot be 
   added as the restaurant opens at 19:00 and closes at 23:00. Hence, we get false.  
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Test Case 4 : Take all the 8 orders in the question, but modify them to be on different dates (11 March to 18 March).
%%%% To try this case, uncomment all_order(...) below and comment all other all_orders(...).
%%%% The corresponding terminal output as well as its description is also shown.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
all_orders(
    [[table,for,2,at,20,':',00,on,11,march],
    [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,12,th],
    [we,would,like,a,table,for,5,preferably,at,8,pm,on,13,'/',03],
    [can,i,book,a,table,at,8,pm,for,2,people,on,the,14,th,of,march,for,the,standard,menu,please],
    [reserve,us,a,table,on,march,15,for,a,party,of,4,for,the,standard,menu],
    [9,people,on,16,th,of,march],
    [book,6,of,us,in,on,17,march,at,20,':',00],
    [reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]
    ]
).*/


/* Terminal Output
?- read_allocate_and_print_orders(AllocatedOrderList).
Order Number 1 consisting of 2 people should have them occupying table numbers [3] at 20:00 for 2 hours on 11/3.
Order Number 2 consisting of 3 people should have them occupying table numbers [3] at 19:00 for 1 hours on 12/3.
Order Number 3 consisting of 5 people should have them occupying table numbers [2,3] at 20:00 for 2 hours on 13/3.
Order Number 4 consisting of 2 people should have them occupying table numbers [3] at 20:00 for 2 hours on 14/3.
Order Number 5 consisting of 4 people should have them occupying table numbers [3] at 19:00 for 2 hours on 15/3.
Order Number 6 consisting of 9 people should have them occupying table numbers [1,2,3] at 19:00 for 2 hours on 16/3.
Order Number 7 consisting of 6 people should have them occupying table numbers [2,3] at 20:00 for 2 hours on 17/3.
Order Number 8 consisting of 7 people should have them occupying table numbers [2,3] at 19:00 for 2 hours on 18/3.
AllocatedOrderList = [allocated_order(1,20,[0,0,1],2,2,(11,3)),allocated_order(2,19,[0,0,1],1,3,(12,3)),allocated_order(3,20,[0,1,1],2,5,(13,3)),allocated_order(4,20,[0,0,1],2,2,(14,3)),allocated_order(5,19,[0,0,1],2,4,(15,3)),allocated_order(6,19,[1,1,1],2,9,(16,3)),allocated_order(7,20,[0,1,1],2,6,(17,3)),allocated_order(8,19,[0,1,1],2,7,(18,3))] 
*/


/* Description on the output : 
   a. All the 8 orders can be parsed correctly in terms of date, time, number of people and hours.  
   c. All the orders can be constrained, as they are on different dates.
   b. The table capacity of Table1 is 2, Table2  is 3 and Table3 is 4. 
      Order Number 6 consisting of 9 people should occupy all the tables [1,2,3] and is observed in the DCG output above.
      Similarly Order Number 8 consisting of 8 people should occupy tables [2,3] and is also observed in the DCG output above.
      Similarly, for all the other cases, we can see that all the people in the order can be fit into the tables allocated to that order. 
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Test Case 5 : Take all the 8 orders in the question, but modify them to be on different dates (11 March to 18 March).
%%%%  Also consider order number 6 to consist of 10 people rather than 9.
%%%% To try this case, uncomment all_order(...) below and comment all other all_orders(...).
%%%% The corresponding terminal output as well as its description is also shown.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
all_orders(
    [[table,for,2,at,20,':',00,on,11,march],
    [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,12,th],
    [we,would,like,a,table,for,5,preferably,at,8,pm,on,13,'/',03],
    [can,i,book,a,table,at,8,pm,for,2,people,on,the,14,th,of,march,for,the,standard,menu,please],
    [reserve,us,a,table,on,march,15,for,a,party,of,4,for,the,standard,menu],
    [10,people,on,16,th,of,march],
    [book,6,of,us,in,on,17,march,at,20,':',00],
    [reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]
    ]
). */

/* Terminal Output
?- read_allocate_and_print_orders(AllocatedOrderList).
false.
*/

/* Description on the output :  
   The table capacity of Table1 is 2, Table2  is 3 and Table3 is 4. 
   So, each order can accomodate a maximum of 9 people. Order number 6 contains 10 people and will hence return false
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Test Case 6 : Take all the 8 orders in the question, but modify all of them to ensure that all the orders can be allocated to the their tables accordingly on March 18.
%%%% To try this case, uncomment all_order(...) below and comment all other all_orders(...).
%%%% The corresponding terminal output as well as its description is also shown.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
all_orders(
    [[table,for,2,at,19,':',00,on,18,march],
     [please,can,we,have,a,table,for,2,for,the,theatre,menu,on,march,18,th],
     [we,would,like,a,table,for,7,preferably,at,7,pm,on,18,'/',03],
     [can,i,book,a,table,at,10,pm,for,2,people,on,the,18,th,of,march,for,the,theatre,menu,please],
     [reserve,us,a,table,on,march,18,for,a,party,of,3,for,the,theatre,menu],
     [3,people,on,18,th,of,march, for, the, theatre, menu],
     [book,4,of,us,in,on,18,march,for,theatre,menu, at,22,':',00],
     [reservation,for,4,on,march,18,preferably,for,theatre,menu,at,9,oclock]
    ]
).*/

/* Terminal Output
?- read_allocate_and_print_orders(AllocatedOrderList).
Order Number 1 consisting of 2 people should have them occupying table numbers [1] at 19:00 for 2 hours on 18/3.
Order Number 2 consisting of 2 people should have them occupying table numbers [1] at 21:00 for 1 hours on 18/3.
Order Number 3 consisting of 7 people should have them occupying table numbers [2,3] at 19:00 for 2 hours on 18/3.
Order Number 4 consisting of 2 people should have them occupying table numbers [1] at 22:00 for 1 hours on 18/3.
Order Number 5 consisting of 3 people should have them occupying table numbers [2] at 21:00 for 1 hours on 18/3.
Order Number 6 consisting of 3 people should have them occupying table numbers [2] at 22:00 for 1 hours on 18/3.
Order Number 7 consisting of 4 people should have them occupying table numbers [3] at 22:00 for 1 hours on 18/3.
Order Number 8 consisting of 4 people should have them occupying table numbers [3] at 21:00 for 1 hours on 18/3.
AllocatedOrderList = [allocated_order(1,19,[1,0,0],2,2,(18,3)),allocated_order(2,21,[1,0,0],1,2,(18,3)),allocated_order(3,19,[0,1,1],2,7,(18,3)),allocated_order(4,22,[1,0,0],1,2,(18,3)),allocated_order(5,21,[0,1,0],1,3,(18,3)),allocated_order(6,22,[0,1,0],1,3,(18,3)),allocated_order(7,22,[0,0,1],1,4,(18,3)),allocated_order(8,21,[0,0,1],1,4,(18,3))] 
*/

/* Description of Output : 
    Note : I have modified the orders such that all the orders are on March 18, and their time and allocaated tables can be constraint
    as per the requirements of the question. (Restaurant opening and closing times, no two orders sharing the same table at a given time, and tables allocated to
    accomodate all people in the order)
    This works perfectly fine as shown in the above output. 
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Summary of All Test cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The results from all the test cases indicate that :
%% a. code is capable of parsing all the orders with DCG properly, 
%% b. constraining them to ensure that  the tables are allocated to seat all people in the order
%% such that no two orders can occupy the same table at a given time and date.  
%% c. the results are printed using DCG on the terminal. 

