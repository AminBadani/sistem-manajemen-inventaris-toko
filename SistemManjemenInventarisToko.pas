{$MODE DELPHI}
PROGRAM SistemManjemenInventarisToko;
USES 
  crt, fpjson, jsonparser, sysutils, classes, jsonconf;

VAR
  DataBarang: TJSONData; // Untuk menampung semua data barang yang ada di dalam file json
  KeluarProgram: Boolean = false;  // Untuk mengeluarkan pengguna dari program

  PilihMenu: integer; // Untuk menampung pilihan pengguna berdasarkan menu utama yang ada
  PilihMenuBarang: integer; // Untuk menampung pilihan ketika tambah, edit, dan hapus

// Membuat file json yang masih kosong
procedure BuatJSON(namaFile: string);
var
  DataBarangBaru: TJSONConfig; // Untuk membuat file json
begin
  DataBarangBaru := TJSONConfig.Create(nil);

  try
    DataBarangBaru.Formatted := True;
    DataBarangBaru.Filename := namaFile;
  finally
    DataBarangBaru.Free;
  end;
end;

// Untuk membaca semua data barang yang ada di dalam file json
function BacaJSON(const namaFile: string): TJSONData;
var 
  // Menyimpan isi dari file secara utuh yang masih berupa string 
  // Menggunakan memory stream untuk mempercepat ketika membaca file
  IsiFile: TMemoryStream; 
  IsiFileJSON: TJSONData; // Untuk menyimpan isi dari file dalam bentuk json
begin
  IsiFile := TMemoryStream.Create;
  try
    try
      IsiFile.loadFromFile(namaFile);
      IsiFileJSON := GetJSON(IsiFile);
    except
      on E:Exception do
        writeln('File ', namaFile, ' could not be found');
    end;
  finally
    IsiFile.free;
  end;

  exit(IsiFileJSON);
end;

// Menampilkan semua data barang ke terminal
procedure TampilkanSemuaBarang(list_data_barang: TJSONData);
var
  DetailBarang: TJSONData; // Untuk menampung satu barang beserta properti-nya
  Barang: TJSONObject;  // Untuk menampung properti spesifik dari barang
  i: integer;
begin 
  ClrScr;

  writeln('------ Data semua barang ------');
  for i := 0 to list_data_barang.Count - 1 do
    begin
      DetailBarang := list_data_barang.Items[i];
      Barang := DetailBarang as TJSONObject;

      writeln('ID barang: ', Barang.Get('id'));
      writeln('Nama barang: ', Barang.Get('nama'));
      writeln('Stok barang: ', Barang.Get('stok'));
      writeln('Harga barang: ', Barang.Get('harga'), sLineBreak);
    end;
end;

BEGIN
  if not FileExists('dataBarang.json') then
    CreateJSON('dataBarang.json');

  repeat
    ClrScr;

    writeln('------ Sistem Manajemen Inventaris Toko');
    writeln('1. Lihat semua barang');
    writeln('2. Keluar dari program');

    write(sLineBreak, 'Pilih menu ke: ');
    readln(PilihMenu);

    if (PilihMenu = 1) then begin
        DataBarang := BacaJSON('dataBarang.json');
        TampilkanSemuaBarang(DataBarang);

        writeln('------ Pilih Menu ------');
        writeln('1. Tambah data barang');
        writeln('2. Edit data barang');
        writeln('3. Hapus data barang');

        write('Masukkan pilihan: ');
        readln(PilihMenuBarang);
    end
    else if (PilihMenu = 2) then begin
      writeln('Bye-bye');
      KeluarProgram := True;
    end
    else
      writeln('Hello world');
    
  until KeluarProgram = True;
END.