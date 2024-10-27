{$MODE DELPHI}

program MainSQLite;

uses  
  crt, sysutils, sqldb, sqlite3conn;

var 
  sqlite3: TSQLite3Connection; // Untuk menyimpan konfigurasi koneksi ke database
  dbTransaction: TSQLTransaction; // Untuk menyimpan konfigurasi transaction berdasarkan database
  dbQuery: TSQLQuery; { Masih belum terpakai }

procedure BukaDatabase(namaDatabase: string);
var 
  sqlQuery: string;

begin

  // Membuat komponen 
  sqlite3       := TSQLite3Connection.Create(nil); // Menyiapkan koneksi ke database
  dbTransaction := TSQLTransaction.Create(nil); // Menyiapkan transaction untuk dapat mengakses dan memodifikasi isi database 

  // Mengatur komponen
  sqlite3.Transaction     := dbTransaction; // Mengatur koneksi ke transaction yang digunakan
  sqlite3.DatabaseName    := namaDatabase; // Mengatur nama database sesuai paremeter namaDatabase
  dbTransaction.Database  := sqlite3; // Mengatur database untuk transaction

  // Mengecek apakah file sudah ada
  if (not FileExists(namaDatabase)) then
    begin

      // buat database baru
      try

        // Membuka koneksi berdasarkan konfigurasi yang dilakukan sebelumnya
        sqlite3.Open;
        // Mengaktifkan transaction agar dapat mengakses isi database
        dbTransaction.Active := true;

        // Menjalankan atau mengeksekusi (execute) perintah berdasarkan query
        sqlQuery := 'CREATE TABLE barang (id INTEGER PRIMARY KEY AUTOINCREMENT, nama_barang TEXT, stok INTEGER, harga TEXT);';
        sqlite3.ExecuteDirect(sqlQuery);

        sqlQuery := 'INSERT INTO barang (nama_barang, stok, harga) VALUES ("Sabun mandi", 10, "15.000");';
        sqlite3.ExecuteDirect(sqlQuery);

        // Menyimpan perubahan berdasarkan query yang sudah dieksekusi (executed)
        dbTransaction.Commit;
        writeln('Database berhasil dibuat');
      except
        begin
          sqlite3.Close;
          writeln('Error ?!');
        end;
      end;

    end;
  
  // Menutup koneksi dan melepaskan "sumber daya"
  sqlite3.Close;
  sqlite3.Free;
  dbTransaction.Free;
end;

begin
  BukaDatabase('test.db');
end.