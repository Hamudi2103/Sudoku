program JuegoSudoku; // writeln fila y columna invertidos 

uses crt;

const
  TAMANIO_TABLERO = 9;
  CELDA_VACIA = 0;
  VALOR_MINIMO = 1;
  VALOR_MAXIMO = 9;
  PISTAS = 17;

type
  TTableroSudoku = array[1..TAMANIO_TABLERO, 1..TAMANIO_TABLERO] of integer;

var
  tablero: TTableroSudoku;
  fila, columna, numero: integer;
  celdasVacias: integer;
  nombreJugador: string;

procedure InicializarTablero;
var
  i, j, k: integer;
  numValido: boolean;

  function EsNumeroValido(num, fila, columna: integer): boolean;
  var
    x: integer;
  begin
    for x := 1 to TAMANIO_TABLERO do
    begin
      if (tablero[fila, x] = num) or (tablero[x, columna] = num) then
      begin
        EsNumeroValido := False;
        Exit;
      end;
    end;
    EsNumeroValido := True;
  end;

begin
  for i := 1 to TAMANIO_TABLERO do
    for j := 1 to TAMANIO_TABLERO do
      tablero[i, j] := CELDA_VACIA;

  Randomize;
  for k := 1 to PISTAS do
  begin
    repeat
      i := Random(TAMANIO_TABLERO) + 1;
      j := Random(TAMANIO_TABLERO) + 1;
    until tablero[i, j] = CELDA_VACIA;

    repeat
      numValido := True;
      numero := Random(VALOR_MAXIMO - VALOR_MINIMO + 1) + VALOR_MINIMO;
      if not EsNumeroValido(numero, i, j) then
        numValido := False;
    until numValido;

    tablero[i, j] := numero;
  end;

  celdasVacias := TAMANIO_TABLERO * TAMANIO_TABLERO;
  for i := 1 to TAMANIO_TABLERO do
    for j := 1 to TAMANIO_TABLERO do
      if tablero[i, j] <> CELDA_VACIA then
        Dec(celdasVacias);
end;

procedure MostrarTablero;
var
  i, j: integer;
begin
  for i := 1 to TAMANIO_TABLERO do
  begin
    if (i - 1) mod 3 = 0 then
      WriteLn(' -----------------------');
    for j := 1 to TAMANIO_TABLERO do
    begin
      if (j - 1) mod 3 = 0 then
        Write(' |');
      if tablero[i, j] = CELDA_VACIA then
        Write('  ')
      else
        Write(' ', tablero[i, j]);
    end;
    WriteLn(' |');
  end;
  WriteLn(' -----------------------');
end;

procedure IngresarNumero;
begin
  WriteLn('Ingrese columna (1-9): ');
  ReadLn(columna);
  WriteLn('Ingrese fila (1-9): ');
  ReadLn(fila);
  WriteLn('Ingrese número (1-9): ');
  ReadLn(numero);

  if (fila < VALOR_MINIMO) or (fila > VALOR_MAXIMO) or (columna < VALOR_MINIMO) or (columna > VALOR_MAXIMO) then
  begin
    WriteLn('Coordenadas inválidas.');
  end
  else if tablero[fila, columna] <> CELDA_VACIA then
  begin
    WriteLn('La celda ya está ocupada.');
  end
  else if (numero < VALOR_MINIMO) or (numero > VALOR_MAXIMO) then
  begin
    WriteLn('Número inválido.');
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
    WriteLn('¡Felicitaciones, ', nombreJugador, '!');
    MostrarTablero;
  end;
end;

begin
  Randomize;

  Write('Ingrese su nombre: ');
  ReadLn(nombreJugador);

  WriteLn('Bienvenido al Sudoku, ', nombreJugador);

  InicializarTablero;
  MostrarTablero;

  while celdasVacias > 0 do
  begin
    IngresarNumero;
    MostrarTablero;
    VerificarVictoria;
  end;
end.

