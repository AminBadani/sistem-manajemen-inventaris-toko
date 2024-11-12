{$MODE DELPHI}
PROGRAM SistemManjemenInventarisToko;
USES 
  crt, fpjson, jsonparser, sysutils, classes, jsonconf;

VAR
  dataBarang: TJSONData; // Untuk menampung semua data barang yang ada di dalam file json
  keluarProgram: Boolean = false;  // Untuk mengeluarkan pengguna dari program

  pilihMenu: integer; // Untuk menampung pilihan pengguna berdasarkan menu utama yang ada
  pilihMenuBarang: integer; // Untuk menampung pilihan ketika tambah, edit, dan hapus

// Membuat file json baru
procedure BuatJSON(nama_file: string);
var
  fileJsonBaru: TJSONConfig; // Untuk membuat file json
begin
  fileJsonBaru := TJSONConfig.Create(nil);

  try
    fileJsonBaru.Formatted := True;
    fileJsonBaru.Filename := nama_file;

    fileJsonBaru.SetValue('/0/id', 1);
    fileJsonBaru.SetValue('/0/nama', 'Sabun cuci');
    fileJsonBaru.SetValue('/0/merk', 'Lifebuoy');
    fileJsonBaru.SetValue('/0/stok', 20);
    fileJsonBaru.SetValue('/0/harga', 10000);
  finally
    fileJsonBaru.Free;
  end;
end;

// Untuk membaca semua data barang yang ada di dalam file json
function BacaJSON(nama_file: string): TJSONData;
var 
  // Menyimpan isi dari file secara utuh yang masih berupa string 
  // Menggunakan memory stream untuk mempercepat ketika membaca file
  isiFile: TMemoryStream; 
  isiFileJSON: TJSONData; // Untuk menyimpan isi dari file dalam bentuk json
begin
  isiFile := TMemoryStream.Create;
  try
    try
      isiFile.loadFromFile(nama_file);
      isiFileJSON := GetJSON(isiFile);
    except
      on E:Exception do
        writeln('File ', nama_file, ' tidak ditemukan');
    end;
  finally
    isiFile.Free;
  end;

  exit(isiFileJSON);
end;

// Menampilkan semua data barang ke terminal
procedure TampilkanSemuaBarang(list_data_barang: TJSONData);
var
  detailBarang: TJSONData; // Untuk menampung satu barang beserta properti-nya
  barang: TJSONObject;  // Untuk menampung properti spesifik dari barang
  i: integer;
begin 
  ClrScr;

  try
    writeln('------ Data semua barang ------');
    for i := 0 to list_data_barang.Count - 1 do begin
      detailBarang := list_data_barang.Items[i];
      barang := detailBarang as TJSONObject;

      writeln('Nama barang: ', barang.Get('nama'));
      writeln('Merk barang: ', barang.Get('merk'));
      writeln('Stok barang: ', barang.Get('stok'));
      writeln('Harga barang: ', barang.Get('harga'), sLineBreak);
    end;  
  finally
    detailBarang.Free;
  end;
end;

// Menambah barang baru ke dalam file json
procedure TambahBarangBaru(nama_file: string);
var
  // Variabel untuk mengambil dan menyimpan data lama
  fileJsonLama: TJSONConfig;
  dataBarangLama: TJSONData;
  barangLama: TJSONObject;

  // Variabel untuk menampung input data barang baru oleh user
  namaBarangBaru: string;
  merkBarangBaru: string;
  stokBarangBaru: integer;
  hargaBarangBaru: longint;

  temporaryString: string; // Untuk menampung string sementara dari path json
  i: integer;

begin
  ClrScr;
  fileJsonLama := TJSONConfig.Create(nil); // Membuat komponen TJSONConifg

  // Membaca input pengguna
  writeln('------ Tambah barang baru ------');
  write('Masukkan nama barang baru: ');
  readln(namaBarangBaru);
  write('Masukkan merk barang baru: ');
  readln(merkBarangBaru);
  write('Masukkan stok barang baru: ');
  readln(stokBarangBaru);
  write('Masukkan harga barang baru: ');
  readln(hargaBarangBaru);

  try
    // Konfigurasi file json dan nama file
    fileJsonLama.Formatted := True;
    fileJsonLama.Filename := nama_file;

    // Membaca file json yang berisi data lama
    dataBarangLama := BacaJSON(nama_file);

    // Menimpa isi dari file json lama dengan barang sebelumnya
    for i := 0 to dataBarangLama.Count - 1 do
      begin
        // Membaca data barang lama berdasarkan yang ada di dalam file json
        barangLama := dataBarangLama.Items[i] as TJSONObject;

        // Menimpa isi file lama dengan data barang sebelumnya
        temporaryString := '/' + IntToStr(i) + '/id';
        fileJsonLama.SetValue(temporaryString, barangLama.Integers['id']);

        temporaryString := '/' + IntToStr(i) + '/nama';
        fileJsonLama.SetValue(temporaryString, barangLama.Get('nama', ''));

        temporaryString := '/' + IntToStr(i) + '/merk';
        fileJsonLama.SetValue(temporaryString, barangLama.Get('merk', ''));

        temporaryString := '/' + IntToStr(i) + '/stok';
        fileJsonLama.SetValue(temporaryString, barangLama.Integers['stok']);

        temporaryString := '/' + IntToStr(i) + '/harga';
        fileJsonLama.SetValue(temporaryString, barangLama.Integers['harga']);
      end;

    // Memasukkan data barang baru pada file json
    temporaryString := '/' + IntToStr(dataBarangLama.Count) + '/id';
    fileJsonLama.SetValue(temporaryString, dataBarangLama.Count + 1);
    
    temporaryString := '/' + IntToStr(dataBarangLama.Count) + '/nama';
    fileJsonLama.SetValue(temporaryString, namaBarangBaru);

    temporaryString := '/' + IntToStr(dataBarangLama.Count) + '/merk';
    fileJsonLama.SetValue(temporaryString, merkBarangBaru);

    temporaryString := '/' + IntToStr(dataBarangLama.Count) + '/stok';
    fileJsonLama.SetValue(temporaryString, stokBarangBaru);

    temporaryString := '/' + IntToStr(dataBarangLama.Count) + '/harga';
    fileJsonLama.SetValue(temporaryString, hargaBarangBaru);
  finally
    write('Barang berhasil ditambahkan (tekan keyboard untuk melanjutkan) ');
    readkey;

    fileJsonLama.Free;
    dataBarangLama.Free;
  end;
end;

BEGIN
  if not FileExists('dataBarang.json') then begin
    BuatJSON('dataBarang.json');
  end;

  repeat
    ClrScr;

    writeln('------ Sistem Manajemen Inventaris Toko');
    writeln('1. Lihat semua barang');
    writeln('2. Keluar dari program');

    write(sLineBreak, 'Pilih menu ke: ');
    readln(pilihMenu);

    if (pilihMenu = 1) then begin
      repeat
          dataBarang := BacaJSON('dataBarang.json');
          TampilkanSemuaBarang(dataBarang);

          writeln('------ Pilih Menu ------');
          writeln('1. Tambah data barang');
          writeln('2. Edit data barang');
          writeln('3. Hapus data barang');
          writeln('4. Kembali ke menu utama');

          write('Masukkan pilihan: ');
          readln(pilihMenuBarang);

          if (pilihMenuBarang = 1) then begin
            TambahBarangBaru('dataBarang.json');
          end;
      until (pilihMenuBarang = 4)
    end
    else if (pilihMenu = 2) then begin
      writeln('Bye-bye');
      keluarProgram := True;
    end
    else
      writeln('Hello world');
    
  until keluarProgram = True;
END.