herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(limpiarTecho, [escoba, pala]).
herramientasRequeridas(cortarPasto, [bordedadora]).
herramientasRequeridas(limpiarBanio, [sopapa, trapeador]).
herramientasRequeridas(encerarPisos, [lustradpesora, cera, aspiradora(300)]).

persona(egon,aspiradora(200)).
persona(egon,trapeador).
persona(peter,trapeador).
persona(winston,varitaDeNeutrones).

tieneHerramientaRequerida(Persona,HerramientaRequerida):-
    persona(Persona,HerramientaRequerida).    

tieneHerramientaRequerida(Persona,aspiradora(Potenciarequerida)):-
    persona(Persona,aspiradora(Potencia)),
    Potenciarequerida is Potencia.

puedeRealizarTarea(Persona,Tarea):-
    herramientasRequeridas(Tarea,_),
    persona(Persona,varitaDeNeutrones).

puedeRealizarTarea(Persona,Tarea):-
    herramientasRequeridas(Tarea,ListaHerramientasRequeridas),
    persona(Persona,_),
    forall(member(HerramientaRequerida, ListaHerramientasRequeridas),tieneHerramientaRequerida(Persona,HerramientaRequerida)).

%con member(HerramientaRequerida, ListaHerramientasRequeridas) ligo una herramienta de la lista.
%costo(Tarea,costoMetroCuadrado).
costo(limpiarBanio,3).
costo(limpiarTecho,5).
costo(ordenarCuarto,2).
costo(cortarPasto,6).
costo(encerarPisos,6).

tareaPedida(ana,limpiarTecho,60).
tareaPedida(ana,limpiarBanio,10).
tareaPedida(martu,ordenarCuarto,10).
tareaPedida(martu,limpiarBanio,20).
tareaPedida(martu,encerarPisos,70).
tareaPedida(cata,cortarPasto,40).
tareaPedida(afro,cortarPasto,60).
tareaPedida(afro,limpiarTecho,50).
tareaPedida(afro,limpiarBanio,20).
  
criterioPersona(ray,Cliente):-
    tareaPedida(Cliente,_,_),
    forall(tareaPedida(Cliente,Tarea,_),Tarea \= limpiarTecho).

%en ninguna de las tareas del pedido tiene que estar limpiar techo.

criterioPersona(winston,Cliente):-
    cuentaTotal(Cliente,CostoTotal),
    CostoTotal > 500.

criterioPersona(egon,Cliente):-
    not(tieneTareasComplejas(Cliente)).

tieneTareasComplejas(Cliente):-
    tareaPedida(Cliente,_,_),
    forall(tareaPedida(Cliente,Tarea,_),esTareaCompleja(Tarea)).

esTareaCompleja(Tarea):-
    herramientasRequeridas(Tarea,Herramientas),
    length(Herramientas,CantHerramientas),
    CantHerramientas < 2.

esTareaCompleja(limpiarTecho).

aceptaPedido(Persona,Cliente):-
    tareaPedida(Cliente,_,_), 
    satisfaceNecesidadesPedido(Persona,Cliente),
    estaDispuestoAHacerlo(Persona,Cliente).

aceptaPedido(peter,Cliente):- %acepta cualquier pedido
    tareaPedida(Cliente,_,_).

satisfaceNecesidadesPedido(Persona,Cliente):-
    persona(Persona,_),
    tareaPedida(Cliente,_,_),
    forall(tareaPedida(Cliente,Tarea,_),puedeRealizarTarea(Persona,Tarea)).

cuentaTotal(Cliente,CostoTotal):-
    findall(CostoDeLaTarea,(tareaPedida(Cliente,Tarea,Metros),costoDeLaTarea(Tarea,Metros,CostoDeLaTarea)),ListaDeCostosTareas),
    sumlist(ListaDeCostosTareas,CostoTotal).

costoDeLaTarea(Tarea,Metros,CostoDeLaTarea):-
    costo(Tarea,Costo),
    CostoDeLaTarea is (Costo * Metros).
    
estaDispuestoAHacerlo(Persona,Cliente):-
    persona(Persona,_),
    tareaPedida(Cliente,_,_),
    criterioPersona(Persona,Cliente).


/*
para agregar una posible solucion para ordenar el cuarto sin usar una aspiradora haria lo siguiente:
herramientasRequeridas(ordenarCuarto, [escoba, trapeador, plumero]).

esto me permite no cambiar el predicado de puede realizar tarea (y en consecuencia cualuqier otro que lo use) ya que con el forall
me fijo que la persona tenga todas las herramientas necesarias para dicha tarea. Entonces en el caso de tener que ordenar cuarto
la persona que tenga escoba ,trapeador y plumero matchea por el caso agregado anteriormente y devuelve que el puede realizar la 
tarea.

*/