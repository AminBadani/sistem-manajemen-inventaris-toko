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
  // Variabel untuk mengambil data lama dan menyimpan data baru
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

// Mengedit salah satu atribut barang 
procedure EditBarang(detail_barang: TJSONObject; nama_file: string);
var
  fileJsonLama: TJSONConfig; // Menampung file json yang diakses
  pilihMenuEdit: integer;  // Menampung pilihan edit berdasarkan property atau atribut barang

  temporaryString: string; // Menampung path dari json yang di edit

  // Variabel untuk menampung isi propery yang baru
  namaBarangBaru: string; 
  merkBarangBaru: string;
  stokBarangBaru: integer;
  hargaBarangBaru: longint;
begin

  //  Input pilihan properti yang ingin diedit
  write('Pilih property yang ingin di edit (1 = nama, 2 = merk, 3 = stok, 4 = harga): ');
  readln(pilihMenuEdit);
  
  fileJsonLama := TJSONConfig.Create(nil);

  try

    // Konfigurasi file json yang diakses
    fileJsonLama.Formatted := True;
    fileJsonLama.Filename := nama_file;

    // Menggunakan pengkondisian untuk memilih propery yang diedit
    if (pilihMenuEdit = 1) then begin
      write('Masukkan nama barang baru: ');
      readln(namaBarangBaru);

      // Mengupdate isi dari property nama dengan nama baru
      temporaryString := '/' + IntToStr(detail_barang.Integers['id']) + '/nama';
      fileJsonLama.SetValue(temporaryString, namaBarangBaru);
      write('Nama barang berhasil di edit (tekan keyboard untuk melanjutkan) ');
      readkey;

    end else if (pilihMenuEdit = 2) then begin
      write('Masukkan merk barang baru: ');
      readln(merkBarangBaru);

      // Mengupdate isi dari property merk dengan merk baru
      temporaryString := '/' + IntToStr(detail_barang.Integers['id']) + '/merk';
      fileJsonLama.SetValue(temporaryString, merkBarangBaru);
      write('Merk barang berhasil di edit (tekan keyboard untuk melanjutkan) ');
      readkey;

    end else if (pilihMenuEdit = 3) then begin
      write('Masukkan harga barang baru: ');
      readln(stokBarangBaru);

      // Mengupdate isi dari property stok dengan stok baru
      temporaryString := '/' + IntToStr(detail_barang.Integers['id']) + '/stok';
      fileJsonLama.SetValue(temporaryString, stokBarangBaru);
      write('Stok barang berhasil di edit (tekan keyboard untuk melanjutkan) ');
      readkey;

    end else if (pilihMenuEdit = 4) then begin
      write('Masukkan harga barang baru: ');
      readln(hargaBarangBaru);

      // Mengupdate isi dari property harga dengan harga baru
      temporaryString := '/' + IntToStr(detail_barang.Integers['id']) + '/harga';
      fileJsonLama.SetValue(temporaryString, hargaBarangBaru);
      write('Harga barang berhasil di edit (tekan keyboard untuk melanjutkan) ');
      readkey;

    end else begin
      write('Pilihan tidak ada');
      readkey;
      exit;

    end;
  finally
    fileJsonLama.Free;
  end;
end;

// Menghapus data barang berdasarkan id
procedure HapusBarang(detail_barang: TJSONObject; nama_file: string);
var
  // Mengambil file json lama
  fileJsonLama: TJSONConfig;
  dataBarang: TJSONData;
  pathDataBarang: TJSONData;

  yakinMenghapus: char; // Konfirmasi penghapusan
begin 

  // Konfirmasi yakin ingin menghapus
  write('Apakah anda yakin ingin menghapus barang: ', detail_barang.Get('nama'), ' (Y/N) ');
  readln(yakinMenghapus);

  if (UpperCase(yakinMenghapus) <> 'Y') then begin
    write('Hapus barang dibatalkan (tekan keyboard untuk melanjutkan) ');
    readkey;
    exit
  end;

  fileJsonLama := TJSONConfig.Create(nil);

  try
    fileJsonLama.Formatted := True;
    fileJsonLama.Filename := nama_file;

    // Membaca semua data yang ada didalam file json
    dataBarang := BacaJSON(nama_file);
    // Mengambil path berdasarkan id
    pathDataBarang := dataBarang.GetPath(IntToStr(detail_barang.Integers['id']));

    // Hapus path pada file json jika ditemukan
    if (pathDataBarang <> nil) then begin
      fileJsonLama.DeletePath(IntToStr(detail_barang.Integers['id']));
    end;
  finally
    write('Barang ', detail_barang.Get('nama'), ' berhasil dihapus (tekan keyboard untuk melanjutkan) ');
    readkey;

    fileJsonLama.Free;
  end;
end;

// Mencari barang berdasarkan id
procedure DetailBarang();
var
  listDataBarang: TJSONData;
  detailBarang: TJSONObject;
  idBarang: integer;
  pilihMenuDetail: integer;

  i: integer;
begin
  write('Masukkan ID barang: ');
  readln(idBarang);

  repeat
    listDataBarang := BacaJSON('dataBarang.json');

    for i := 0 to listDataBarang.Count - 1 do begin
      detailBarang := listDataBarang.Items[i] as TJSONObject;

      if (detailBarang.Integers['id'] = idBarang) then begin
          ClrScr;
          detailBarang := listDataBarang.Items[i] as TJSONObject;

          writeln('------ Detail barang ------');
          writeln('ID barang: ', detailBarang.Get('id'));
          writeln('Nama barang: ', detailBarang.Get('nama'));
          writeln('Merk barang: ', detailBarang.Get('merk'));
          writeln('Stok barang: ', detailBarang.Get('stok'));
          writeln('Harga barang: ', detailBarang.Get('harga'), sLineBreak);
          
          writeln('------ Menu detail barang ------');
          writeln('1. Edit data barang');
          writeln('2. Hapus data barang');
          writeln('3. Kembali ke list semua barang');

          write('Masukkan pilihan: ');
          readln(pilihMenuDetail);

          if (pilihMenuDetail = 1) then begin
            EditBarang(detailBarang, 'dataBarang.json');
          end else if (pilihMenuDetail = 2) then begin
            HapusBarang(detailBarang, 'dataBarang.json');
            exit
          end else if (pilihMenuDetail = 3) then begin
            exit;
          end else begin
            writeln('Pilihan tidak ada');
            readkey;
          end;
      end;
    end;
  until (pilihMenuDetail = 3);

  write('Barang tidak ditemukan (tekan keyboard untuk melanjutkan) ');
  readkey;
end;

procedure CariBarangKeyword();
var 
  listDataBarang: TJSONData;
  detailBarang: TJSONObject;
  kataKunci: string;

  i: integer;
begin
  write('Masukkan kata kunci pencarian (nama atau merk): ');
  readln(kataKunci);

  ClrScr;
  writeln('------ Cari barang ------');
  
  kataKunci := LowerCase(kataKunci);
  listDataBarang := BacaJSON('dataBarang.json');

  for i := 0 to listDataBarang.Count - 1 do begin

    detailBarang := listDataBarang.Items[i] as TJSONObject;

    if (( Pos(kataKunci, LowerCase(detailBarang.Get('nama'))) > 0 ) or ( Pos(kataKunci, LowerCase(detailBarang.Get('merk'))) > 0 )) then begin
      writeln('ID barang: ', detailBarang.Get('id'));
      writeln('Nama barang: ', detailBarang.Get('nama'));
      writeln('Merk barang: ', detailBarang.Get('merk'));
      writeln('Stok barang: ', detailBarang.Get('stok'));
      writeln('Harga barang: ', detailBarang.Get('harga'), sLineBreak);
    end;
  end;

  listDataBarang.Free;
  readkey;
end;

BEGIN
  if not FileExists('dataBarang.json') then begin
    BuatJSON('dataBarang.json');
  end;

  repeat
    ClrScr;

    writeln('------ Sistem Manajemen Inventaris Toko ------');
    writeln('1. Lihat semua barang');
    writeln('2. Keluar dari program');

    write(sLineBreak, 'Pilih menu ke: ');
    readln(pilihMenu);

    if (pilihMenu = 1) then begin
      repeat
          dataBarang := BacaJSON('dataBarang.json');
          TampilkanSemuaBarang(dataBarang);

          writeln('------ Pilih Menu ------');
          writeln('1. Tambah data barang baru');
          writeln('2. Cari barang berdasarkan kata kunci (nama atau merk)');
          writeln('3. Lihat detail barang (berdasarkan id)');
          writeln('4. Kembali ke menu utama');

          write('Masukkan pilihan: ');
          readln(pilihMenuBarang);

          if (pilihMenuBarang = 1) then TambahBarangBaru('dataBarang.json')
          else if (pilihMenuBarang = 2) then CariBarangKeyword()
          else if (pilihMenuBarang = 3) then DetailBarang()
      until (pilihMenuBarang = 4)

    end else if (pilihMenu = 2) then begin
      writeln('Bye-bye');
      keluarProgram := True;

    end else begin
      writeln('Pilihan tidak ada');
      readkey;

    end;
    
  until keluarProgram = True;
END.