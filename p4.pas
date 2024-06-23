program Sudoku; //pide nombre

uses crt;

const
TAMANIO_TABLERO= 9;
  CELDA_VACIA= 0;
  VALOR_MINIMO= 1;
  VALOR_MAXIMO= 9;
  PISTAS= 17;                

type
  TTablero = array[1..TAMANIO_TABLERO, 1..TAMANIO_TABLERO] of integer;  

var
  tablero: TTablero;                       
  fila, columna, numero: integer;          
  celdasVacias: integer;                   
  nombre: string;                        

procedure InicializarTablero;
var
  i, j, k: integer;
begin
  for i := 1 to TAMANIO_TABLERO do
    for j := 1 to TAMANIO_TABLERO do
      tablero[i, j] := TAMANIO_TABLERO;

 
  Randomize;
  for k := 1 to PISTAS do
  begin
    repeat
      i := Random(TAMANIO_TABLERO) + 1;
      j := Random(TAMANIO_TABLERO) + 1;
    until tablero[i, j] =CELDA_VACIA;

  
    numero := Random(VALOR_MAXIMO - VALOR_MINIMO + 1) + VALOR_MINIMO;
    tablero[i, j] := numero;
  end;

  
  celdasVacias := TAMANIO_TABLERO * TAMANIO_TABLERO;
  for i := 1 to TAMANIO_TABLERO do
    for j := 1 to TAMANIO_TABLERO do
      if tablero[i, j] = CELDA_VACIA then
        Dec(celdasVacias);
end;

procedure MostrarTablero;
var
  i, j: integer;
begin
  for i := 1 to TAMANIO_TABLERO do
  begin
    for j := 1 to TAMANIO_TABLERO do
      Write(tablero[i, j]:2);
    WriteLn;
  end;
end;

procedure IngresarNumero;
begin
  Writeln('Ingrese fila (1-9): ');
  ReadLn(fila);
  Writeln('Ingrese columna (1-9): ');
  ReadLn(columna);
  Writeln('Ingrese numero (1-9): ');
  ReadLn(numero);

  if (fila < VALOR_MINIMO) or (fila > VALOR_MAXIMO) or (columna < VALOR_MINIMO) or (columna > VALOR_MAXIMO) then
  begin
    WriteLn('Coordenadas invalidas.');
  end
  else if tablero[fila, columna] <> CELDA_VACIA then
  begin
    WriteLn('Celda ocupada.');
  end
  else
  begin
    tablero[fila, columna] := numero;
    Dec(celdasVacias);
  end;
end;

procedure VerificarVictoria;
begin
  if celdasVacias = 0 then
  begin
    WriteLn('¡Ganaste, ', nombre, '!');
    MostrarTablero;
  end;
end;

begin
  Randomize;

  Write('Ingrese su nombre: ');
  ReadLn(nombre);

  WriteLn('Bienvenido al juego de Sudoku, ', nombre, '');

  InicializarTablero;
  MostrarTablero;

  while celdasVacias > 0 do
  begin
    IngresarNumero;
    MostrarTablero;
    VerificarVictoria;
  end;
end.
