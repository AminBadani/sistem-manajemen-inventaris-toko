{$MODE DELPHI}

program MainSQLite;

uses  
  crt, sysutils, sqldb, sqlite3conn;

var 
  sqlite3: TSQLite3Connection; // Untuk menyimpan konfigurasi koneksi ke database
  dbTransaction: TSQLTransaction; // Untuk menyimpan konfigurasi transaction berdasarkan database
  dbQuery: TSQLQuery; // Untuk mengambil record dari database

procedure BukaDatabase(nama_database: string);
var 
  sqlQuery: string;

begin

  // Membuat komponen 
  sqlite3       := TSQLite3Connection.Create(nil); // Menyiapkan koneksi ke database
  dbTransaction := TSQLTransaction.Create(nil); // Menyiapkan transaction untuk dapat mengakses dan memodifikasi isi database 
  dbQuery       := TSQLQuery.Create(nil); // Menyiapkan komponen untuk SQL command

  // Menkonfigurasi database
  sqlite3.Transaction     := dbTransaction; // Mengatur koneksi ke transaction yang digunakan
  sqlite3.DatabaseName    := nama_database; // Mengatur nama database sesuai paremeter nama_database
  
  // Menyiapkan komponen
  dbTransaction.Database  := sqlite3; // Mengatur database untuk transaction
  dbQuery.Transaction   := dbTrans; // Mengatur transaction untuk query
  dbQuery.Database      := sqlite3; // Mengatur database yang digunakan untuk menjalankan query

  // Mengecek apakah file sudah ada
  if (not FileExists(nama_database)) then
    begin

      // buat database baru
      try
        
        // Membuka koneksi ke database
        sqlite3.Open;

        // Mengaktifkan transaction agar dapat mengakses isi database
        dbTransaction.Active := true;

        // Menjalankan atau mengeksekusi (execute) perintah berdasarkan query
        sqlQuery := 'CREATE TABLE barang (id INTEGER PRIMARY KEY AUTOINCREMENT, nama_barang TEXT, stok INTEGER, harga integer);';
        sqlite3.ExecuteDirect(sqlQuery);

        sqlQuery := 'INSERT INTO barang (nama_barang, stok, harga) VALUES ("Sabun mandi", 10, 15000);';
        sqlite3.ExecuteDirect(sqlQuery);

        // Menyimpan perubahan berdasarkan query yang sudah dieksekusi (executed)
        dbTransaction.Commit;
        writeln('Database berhasil dibuat');

      except
        on E: Exception do
        begin
          sqlite3.Close;
          writeln('Error setup database?! ', E.Message);
        end;
      end;

    end;
end;

procedure TutupDatabase;
begin
  // Menutup koneksi dan melepaskan "sumber daya"
  sqlite3.Close;
  sqlite3.Free;
  dbTransaction.Free;
end;

procedure TambahBarang(nama_barang: string; stok_barang: integer; harga_barang: longint);
var 
  queryTambahBarang: string;

begin
  try
    queryTambahBarang := 'INSERT INTO barang (nama_barang, stok, harga) VALUES("' + nama_barang + '", ' + IntToStr(stok_barang) + ', ' + IntToStr(harga_barang) + ');' ;
    sqlite3.ExecuteDirect(queryTambahBarang);

    // Menyimpan perubahan berdasarkan query yang sudah dieksekusi (executed)
    dbTransaction.Commit;
    writeln('Tambah barang berhasil');
  except
    on E: Exception do
    begin
      TutupDatabase;
      writeln('Error tambah barang?! ', E.Message)
    end;
  end;
end;

procedure EditBarang(id_barang: integer);
var 
  queryEditBarang: String

begin
  try

  except
    on E: Exception do
    begin
      TutupDatabase;
      writeln('Error edit barang?! ', E.Message)  
    end;
  end;
end;

procedure DetailBarang(id_barang: integer);
begin
  
end;

begin
  BukaDatabase('test.db');
  TambahBarang('Sikat gigi', 20, 22000);
end.