# Algebirc: Tesseract Architecture & Future Concepts
**Status**: Research Incubator / Concept Draft
**Related Topics**: Honey Encryption, Plausible Deniability, Post-Quantum Tarpits, SMT Oracle Defeat

---

## 1. The "Tesseract" Concept (Closed Mirror Room)
Dalam kriptografi standar, alat pembobol otomatis (seperti Z3 SMT Solver atau Hashcat) sangat bergantung pada fenomena **Oracle of Correctness** (Peramal Kebenaran). Sistem biasanya merespons dengan *syntax error*, *padding failure*, atau *invalid MAC* saat diberikan *password* yang salah, sehingga *brute-forcer* bisa langsung membuang tebakan tersebut dan beralih ke tebakan berikutnya.

**Solusi Tesseract**: 
Algebirc akan dikembangkan untuk menghancurkan *Oracle of Correctness* ini sepenuhnya. Jika mesin menerima *Key* atau *Password* yang salah, matriks Isogeny tidak akan memuntahkan *error*. Sebaliknya, ia akan memantulkan **Decoy Output** (Program Jebakan) yang tetap lolos verifikasi kompilasi GHC (Plausible Deniability) atau justru memanipulasi *graph traversal* menjadi **Isogeny Tarpit**.

---

## 2. Peta Jalan Implementasi (Future Roadmap)

### A. Non-Deterministic Honey-Pot Decryption via Isogeny Mapping
Kita akan mendesain mesin dekripsi agar mampu memuntahkan *Plaint-text* yang Valid (namun salah) secara sintaksis.
- **Mekanisme**: Memanipulasi sisa *toxic padding* atau menyisipkan *grammar generator* di dalam alur deobfuscation. Jika dekripsi matriks gagal mencapai target *hash* asli (mengindikasikan *password* salah), mesin akan menggeser lintasan dekripsinya untuk menghasilkan kode sumber berupa fungsi buatan tak bermakna (*dummy function*) yang *compile-able*.
- **Efek (Human-in-the-loop Exhaustion)**: SMT Solver akan melaporkan bahwa seluruh jutaan tebakan passwordnya bernilai "Benar". Peretas secara manual (manusia) harus menguji jutaan program pantulan tersebut untuk mencari mana logika bisnis yang asli, yang secara waktu dan tenaga mustahil dilakukan.

### B. Dimensional Disorientation (Isogeny Tarpits)
Memanfaatkan sifat matematis dari Supersingular Isogeny Graphs (graf gunung berapi / *volcano graphs*).
- **Mekanisme**: Jika *password* yang dimasukkan salah, lintasan *Richelot Walk* akan sengaja disesatkan (*misrouted*) ke sebuah sub-graf melingkar (*infinite loop / cycle*). 
- **Efek (Resource Exhaustion)**: CPU dan memori RAM peretas akan terkunci memproses pemetaan matriks tanpa henti. Celah ini akan menghabiskan (*drain*) tenaga komputasi musuh tanpa pernah memberikan sinyal bahwa mesin mereka sedang dijebak. Proses dekripsi tidak pernah selesai, memenangkan waktu untuk pihak bertahan.

### C. Deniable Encryption (Anti-Interogasi)
Mengadopsi filosofi *TrueCrypt/VeraCrypt* langsung di level polinomial.
- **Mekanisme**: Satu blok polinomial menyimpan dua *state* yang sah. *Password A* membuka program decoy (umpan), sementara *Password B* membuka program rahasia.
- **Efek**: Jika pemilik kode tertangkap dan dipaksa menyerahkan kunci ekskusi di bawah tekanan (*rubber-hose cryptanalysis*), mereka bisa menyerahkan *Decoy Password*. Secara matematis, mustahil membuktikan ada program lain yang tersembunyi di balik persamaan polinomial yang sama.

---

## 3. Rencana Integrasi Akademik
Jika/Ketika fitur ini mulai memasuki fase purwarupa, materi ini akan dipindahkan ke **`part4_security.tex`** di dalam *paper* Algebirc, mengokohkan argumen bahwa Algebirc bukan sekadar pelindung logika (*logic obstructor*), melainkan **Weaponized Cryptography** (kriptografi yang dipersenjatai) untuk perlawanan asimetris terhadap alat analisis otomatis.
