program Sudoku; //base 

uses crt;

const
  N = 9;                                  
  VACIO = 0;                             
  MIN = 1;                              
  MAX = N;                             
  NUM_INICIALES = 17;                 

type
  TTablero = array[1..N, 1..N] of integer;  

var
  tablero: TTablero;                        
  fila, columna, numero: integer;         
  celdasVacias: integer;                 
procedure InicializarTablero;
var
  i, j, k: integer;

begin
  for i := 1 to N do
    for j := 1 to N do
      tablero[i, j] := VACIO;

  
  Randomize;
  for k := 1 to NUM_INICIALES do
  begin
    repeat
      i := Random(N) + 1;
      j := Random(N) + 1;
    until tablero[i, j] = VACIO;

    
    numero := Random(MAX - MIN + 1) + MIN;
    tablero[i, j] := numero;
  end;

 
  celdasVacias := N * N;
  for i := 1 to N do
    for j := 1 to N do
      if tablero[i, j] = VACIO then
        Dec(celdasVacias);
end;

procedure MostrarTablero;
var
  i, j: integer;
begin
  for i := 1 to N do
  begin
    for j := 1 to N do
      Write(tablero[i, j]:2);
    WriteLn;
  end;
end;

procedure IngresarNumero;
begin
  Write('Ingrese fila (1-9): ');
  ReadLn(fila);
  Write('Ingrese columna (1-9): ');
  ReadLn(columna);
  Write('Ingrese número (1-9): ');
  ReadLn(numero);

  if (fila < MIN) or (fila > MAX) or (columna < MIN) or (columna > MAX) then
  begin
    WriteLn('Coordenadas inválidas.');
  end
  else if tablero[fila, columna] <> VACIO then
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
    WriteLn('¡Ganaste!');
    MostrarTablero;
  end;
end;

begin
  Randomize;

  InicializarTablero;
  MostrarTablero;

  while celdasVacias > 0 do
  begin
    IngresarNumero;
    MostrarTablero;
    VerificarVictoria;
  end;
end.
