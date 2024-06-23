program JuegoSudoku;  // numeros para identificar filas

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
  juegoTerminado: boolean;

procedure IniciarTablero;
var
  i, j, k: integer;
  numValido: boolean;

  function EsNumeroValido(num, fila, columna: integer): boolean;
  var
    x, y, inicioFila, inicioColumna: integer;
  begin
    for x := 1 to TAMANIO_TABLERO do
    begin
      if (tablero[fila, x] = num) or (tablero[x, columna] = num) then
      begin
        EsNumeroValido := False;
        Exit;
      end;
    end;

    inicioFila := (fila - 1) div 3 * 3 + 1;
    inicioColumna := (columna - 1) div 3 * 3 + 1;
    for x := inicioFila to inicioFila + 2 do
      for y := inicioColumna to inicioColumna + 2 do
        if tablero[x, y] = num then
        begin
          EsNumeroValido := False;
          Exit;
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
  ClrScr;

  WriteLn('Bienvenido al Sudoku, ', nombreJugador);

Writeln (' ');

  for i := 1 to TAMANIO_TABLERO do
  begin
    if (i - 1) mod 3 = 0 then
      WriteLn('    -----------------------');
    Write('f', i:1, ' ');
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
  WriteLn('    -----------------------');
end;

procedure IngresarNumero;
var
  input: string;
  code: integer;
  i, x, y, inicioFila, inicioColumna: integer;
begin
  repeat
    WriteLn('Ingrese columna (1-9): ');
    ReadLn(input);
    Val(input, columna, code);
    if (code <> 0) or (columna < VALOR_MINIMO) or (columna > VALOR_MAXIMO) then
      WriteLn(' Por favor, ingrese un numero del 1 al 9.');
  until (code = 0) and (columna >= VALOR_MINIMO) and (columna <= VALOR_MAXIMO);

  repeat
    WriteLn('Ingrese fila (1-9): ');
    ReadLn(input);
    Val(input, fila, code);
    if (code <> 0) or (fila < VALOR_MINIMO) or (fila > VALOR_MAXIMO) then
      WriteLn('Por favor, ingrese un numero del 1 al 9.');
  until (code = 0) and (fila >= VALOR_MINIMO) and (fila <= VALOR_MAXIMO);

  repeat
    WriteLn('Ingrese numero (1-9): ');
    ReadLn(input);
    Val(input, numero, code);
    if (code <> 0) or (numero < VALOR_MINIMO) or (numero > VALOR_MAXIMO) then
      WriteLn(' Por favor, ingrese un numero del 1 al 9.');
  until (code = 0) and (numero >= VALOR_MINIMO) and (numero <= VALOR_MAXIMO);

  if tablero[fila, columna] <> CELDA_VACIA then
  begin
    WriteLn('La celda ya esta ocupada. Intente otra vez.');
  end
  else
  begin
    for i := 1 to TAMANIO_TABLERO do
    begin
      if (tablero[fila, i] = numero) or (tablero[i, columna] = numero) then
      begin
        WriteLn('Numero repetido en la fila o columna. Perdiste');
        juegoTerminado := True;
        Exit;
      end;
    end;

    inicioFila := ((fila - 1) div 3) * 3 + 1;
    inicioColumna := ((columna - 1) div 3) * 3 + 1;
    for x := inicioFila to inicioFila + 2 do
      for y := inicioColumna to inicioColumna + 2 do
        if tablero[x, y] = numero then
        begin
          WriteLn('Numero repetido en la fila, columna o en el cuadro. Perdiste');
          juegoTerminado := True;
          Exit;
        end;

    tablero[fila, columna] := numero;
    Dec(celdasVacias);
  end;
end;

procedure VerificarVictoria;
begin
  if celdasVacias = 0 then
  begin
    WriteLn('Felicitaciones ', nombreJugador, ', ganaste');
    MostrarTablero;
    juegoTerminado := True;
  end;
end;

function NombreValido(nombre: string): boolean;
var
  i: integer;
begin
  NombreValido := True;
  if nombre = '' then
  begin
    NombreValido := False;
    Exit;
  end;

  for i := 1 to Length(nombre) do
  begin
    if nombre[i] in ['0'..'9'] then
    begin
      NombreValido := False;
      Exit;
    end;
  end;
end;

begin
  Randomize;

  repeat
    Write('Ingrese su nombre: ');
    ReadLn(nombreJugador);
    if not NombreValido(nombreJugador) then
      WriteLn('Nombre invalido. Por favor Sea serio.');
  until NombreValido(nombreJugador);

  IniciarTablero;
  MostrarTablero;

  juegoTerminado := False;

  while not juegoTerminado do
  begin
    IngresarNumero;
    if not juegoTerminado then
    begin
      MostrarTablero;
      VerificarVictoria;
    end;
  end;
  WriteLn('Juego terminado. Gracias por jugar.');
end.
