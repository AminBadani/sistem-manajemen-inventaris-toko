PROGRAM SistemManjemenInventarisToko;
USES crt;
TYPE
  Barang = record
    id_barang: longint;
    nama: string;
    stok: integer;
    harga: real;
  end;

VAR 
  DataBarang: array [0..5] of Barang;
  i: integer;

  idBaru, stokBaru: integer;
  namaBaru: string;
  hargaBaru: real;

BEGIN
  ClrScr;

  for i := 0 to 2 do 
    begin
      Writeln('---- Tambah barang baru ----');
      Write('Masukkan id barang baru: ');
      ReadLn(idBaru);

      Write('Masukkan nama barang baru: ');
      ReadLn(namaBaru);
      
      Write('Masukkan stok barang baru: ');
      ReadLn(stokBaru);

      Write('Masukkan harga barang baru: ');
      ReadLn(hargaBaru);

      DataBarang[i].id_barang := idBaru;
      DataBarang[i].nama := namaBaru;
      DataBarang[i].stok := stokBaru;
      DataBarang[i].harga := hargaBaru;
    end;

  
END.