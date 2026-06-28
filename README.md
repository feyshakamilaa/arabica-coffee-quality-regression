# Analisis Pengaruh Cacat Biji dan Kadar Air terhadap Skor Kualitas Kopi Arabika Menggunakan Regresi Linear Berganda

Repository ini berisi kode program, dataset, dan hasil analisis yang digunakan dalam penelitian mengenai pengaruh cacat biji dan kadar air terhadap skor kualitas kopi Arabika menggunakan metode regresi linear berganda.

## Deskripsi

Penelitian ini bertujuan untuk menganalisis pengaruh:

- Category One Defects
- Category Two Defects
- Moisture Percentage

terhadap Total Cup Points sebagai indikator kualitas kopi Arabika.

Analisis dilakukan menggunakan perangkat lunak R dengan metode regresi linear berganda.

## Struktur Repository

```
arabica-coffee-quality-regression/
│
├── Dataset.zip
│
├── script R.R
│
├── output/
│   ├── grafik_kadar_air_skor_kualitas.png
│   ├── grafik_cacat_kategori_1_skor_kualitas.png
│   ├── grafik_cacat_kategori_2_skor_kualitas.png
│   ├── grafik_distribusi_skor_kualitas.png
│   ├── tabel_statistik_deskriptif.csv
│   ├── tabel_hasil_regresi.csv
│   ├── tabel_uji_f.csv
│   ├── tabel_uji_t.csv
│   └── tabel_vif.csv
│
├── README.md
├── LICENSE (opsional)
└── .gitignore (opsional)
```

## Dataset

Dataset yang digunakan berasal dari Coffee Quality Database (Arabica) yang dipublikasikan oleh Coffee Quality Institute (CQI).

## Tools

- R
  
## Metode Analisis

Tahapan analisis meliputi:

1. Import dataset
2. Pembersihan data
3. Statistik deskriptif
4. Analisis korelasi
5. Regresi linear berganda
6. Uji asumsi klasik
7. Uji t
8. Uji F
9. Koefisien determinasi (R²)
10. Visualisasi hasil

## Output

Repository ini menghasilkan beberapa output berupa:

- Grafik hubungan antarvariabel
- Histogram distribusi data
- Tabel statistik deskriptif
- Tabel hasil regresi
- Tabel uji t
- Tabel uji F
- Tabel VIF

## Penulis

- Hafshah
- Vincent Berwyn Cahyono
- Feysha Kamila Pracilya
- Faza Nur Aulia Suraya

Program Studi Sains Data  
Universitas Telkom Surabaya
