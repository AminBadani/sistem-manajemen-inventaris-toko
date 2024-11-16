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

      writeln('ID barang: ', barang.Get('id'));
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

  // Variabel untuk menampung input data barang baru oleh user
  namaBarangBaru: string;
  merkBarangBaru: string;
  stokBarangBaru: integer;
  hargaBarangBaru: longint;

  temporaryString: string; // Untuk menampung string sementara dari path json
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

procedure HapusBarang(detail_barang: TJSONObject; nama_file: string);
var
  fileJsonLama: TJSONConfig;
  dataBarang: TJSONData;
  pathDataBarang: TJSONData;

  yakinMenghapus: char;
begin
  write('Apakah anda yakin ingin menghapus ', detail_barang.Get('nama'), ' (Y/N) ');
  readln(yakinMenghapus);

  if (UpperCase(yakinMenghapus) <> 'Y') then begin
    write('Hapus data dibatalkan (tekan keyboard untuk melanjutkan)');
    readkey;
    exit
  end;

  fileJsonLama := TJSONConfig.Create(nil);

  try
    fileJsonLama.Formatted := True;
    fileJsonLama.Filename := nama_file;

    dataBarang := BacaJSON(nama_file);
    pathDataBarang := dataBarang.GetPath(IntToStr(detail_barang.Integers['id'] - 1));

    if (pathDataBarang <> nil) then begin
      fileJsonLama.DeletePath(IntToStr(detail_barang.Integers['id'] - 1));
    end;
  finally
    write('Barang ', detail_barang.Get('nama'), ' berhasil dihapus (tekan keyboard untuk melanjutkan) ');
    readkey;

    fileJsonLama.Free;
  end;
end;

// Mencari barang berdasarkan id
procedure CariBarang();
var
  listDataBarang: TJSONData;
  detailBarang: TJSONObject;
  idBarang: integer;
  pilihMenuDetail: integer;

  i: integer;
begin
  write('Masukkan ID barang yang dicari: ');
  readln(idBarang);

  listDataBarang := BacaJSON('dataBarang.json');

  for i := 0 to listDataBarang.Count - 1 do begin
    detailBarang := listDataBarang.Items[i] as TJSONObject;

    if (detailBarang.Integers['id'] = idBarang) then begin
      ClrScr;
      writeln('------ Detail barang ------');
      writeln('ID barang: ', detailBarang.Get('id'));
      writeln('Nama barang: ', detailBarang.Get('nama'));
      writeln('Merk barang: ', detailBarang.Get('merk'));
      writeln('Stok barang: ', detailBarang.Get('stok'));
      writeln('Harga barang: ', detailBarang.Get('harga'), sLineBreak);
      
      writeln('------ Menu detail barang ------');
      writeln('1. Edit data barang');
      writeln('2. Hapus data barang');
      writeln('3. Kembali ke menu utama');

      write('Masukkan pilihan: ');
      readln(pilihMenuDetail);

      if (pilihMenuDetail = 2) then begin
        HapusBarang(detailBarang, 'dataBarang.json');
      end;

      exit;
    end;
  end;

  write('Barang tidak ditemukan (tekan keyboard untuk melanjutkan) ');
  readkey;
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
          writeln('4. Cari data barang');
          writeln('5. Kembali ke menu utama');

          write('Masukkan pilihan: ');
          readln(pilihMenuBarang);

          if (pilihMenuBarang = 1) then TambahBarangBaru('dataBarang.json')
          else if (pilihMenuBarang = 4) then CariBarang();
      
      until (pilihMenuBarang = 5)
    end
    else if (pilihMenu = 2) then begin
      writeln('Bye-bye');
      keluarProgram := True;
    end
    else
      writeln('Hello world');
    
  until keluarProgram = True;
END.