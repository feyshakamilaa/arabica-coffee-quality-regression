# ============================================================
# Judul:
# Analisis Pengaruh Cacat Biji dan Kadar Air terhadap Skor Kualitas Kopi Arabika
# Menggunakan Regresi Linear Berganda
# ============================================================

# ============================================================
# 0. Persiapan Package
# ============================================================

packages <- c(
  "tidyverse",
  "broom",
  "car",
  "lmtest",
  "knitr",
  "ggplot2"
)

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(packages, install_if_missing))


# ============================================================
# 1. Membaca Dataset
# ============================================================

# Pastikan file archive (6).zip berada di folder kerja RStudio
zip_file <- "archive (6).zip"

if (file.exists(zip_file)) {
  unzip(zip_file, exdir = "data_kopi")
} else {
  stop("File archive (6).zip tidak ditemukan. Pastikan file berada di working directory RStudio.")
}

# Membaca file CSV hasil ekstraksi
csv_file <- "data_kopi/df_arabica_clean.csv"

if (file.exists(csv_file)) {
  data_kopi <- read.csv(csv_file)
} else {
  stop("File df_arabica_clean.csv tidak ditemukan setelah proses unzip.")
}

# Melihat beberapa data awal
head(data_kopi)

# Melihat struktur dataset
str(data_kopi)

# Melihat jumlah baris dan kolom dataset
dim(data_kopi)

# Melihat nama kolom
names(data_kopi)


# ============================================================
# 2. Seleksi Variabel Penelitian
# ============================================================

# Variabel yang digunakan:
# Y  = Total Cup Points
# X1 = Category One Defects
# X2 = Category Two Defects
# X3 = Moisture Percentage

data_regresi <- data.frame(
  skor_kualitas = data_kopi$Total.Cup.Points,
  cacat_kategori_1 = data_kopi$Category.One.Defects,
  cacat_kategori_2 = data_kopi$Category.Two.Defects,
  kadar_air = data_kopi$Moisture.Percentage
)

# Melihat data hasil seleksi variabel
head(data_regresi)

# Mengecek struktur data_regresi
str(data_regresi)


# ============================================================
# 3. Pemeriksaan Kualitas Data
# ============================================================

# Mengecek missing value pada setiap variabel
missing_value <- colSums(is.na(data_regresi))
missing_value

# Menghapus data yang memiliki missing value pada variabel penelitian
data_regresi <- na.omit(data_regresi)

# Mengecek jumlah data setelah missing value dihapus
nrow(data_regresi)

# Mengecek data duplikat
jumlah_duplikat <- sum(duplicated(data_regresi))
jumlah_duplikat

# Menghapus data duplikat jika ada
data_regresi <- data_regresi[!duplicated(data_regresi), ]

# Mengecek kembali dimensi data
dim(data_regresi)


# ============================================================
# 4. Statistik Deskriptif
# ============================================================

statistik_deskriptif <- data_regresi %>%
  summarise(
    n = n(),
    
    mean_skor = mean(skor_kualitas),
    sd_skor = sd(skor_kualitas),
    min_skor = min(skor_kualitas),
    max_skor = max(skor_kualitas),
    
    mean_cacat_kat1 = mean(cacat_kategori_1),
    sd_cacat_kat1 = sd(cacat_kategori_1),
    min_cacat_kat1 = min(cacat_kategori_1),
    max_cacat_kat1 = max(cacat_kategori_1),
    
    mean_cacat_kat2 = mean(cacat_kategori_2),
    sd_cacat_kat2 = sd(cacat_kategori_2),
    min_cacat_kat2 = min(cacat_kategori_2),
    max_cacat_kat2 = max(cacat_kategori_2),
    
    mean_kadar_air = mean(kadar_air),
    sd_kadar_air = sd(kadar_air),
    min_kadar_air = min(kadar_air),
    max_kadar_air = max(kadar_air)
  )

statistik_deskriptif

# Tabel deskriptif dalam format lebih rapi
tabel_deskriptif <- data_regresi %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variabel",
    values_to = "Nilai"
  ) %>%
  group_by(Variabel) %>%
  summarise(
    N = n(),
    Minimum = min(Nilai),
    Maksimum = max(Nilai),
    Mean = mean(Nilai),
    Median = median(Nilai),
    SD = sd(Nilai)
  )

kable(tabel_deskriptif, digits = 3, caption = "Statistik Deskriptif Variabel Penelitian")


# ============================================================
# 5. Korelasi Antarvariabel
# ============================================================

korelasi <- cor(data_regresi)
korelasi

kable(korelasi, digits = 3, caption = "Matriks Korelasi Antarvariabel Penelitian")


# ============================================================
# 6. Model Regresi Linear Berganda
# ============================================================

model <- lm(
  skor_kualitas ~ cacat_kategori_1 + cacat_kategori_2 + kadar_air,
  data = data_regresi
)

# Ringkasan model regresi
summary(model)

# Tabel koefisien regresi yang lebih rapi
tabel_regresi <- tidy(model)

kable(tabel_regresi, digits = 4, caption = "Hasil Regresi Linear Berganda")


# ============================================================
# 7. Persamaan Regresi
# ============================================================

koef <- coef(model)

cat("Persamaan regresi:\n")
cat("Y =", round(koef[1], 4),
    "+", round(koef["cacat_kategori_1"], 4), "X1",
    "+", round(koef["cacat_kategori_2"], 4), "X2",
    "+", round(koef["kadar_air"], 4), "X3\n")

cat("\nKeterangan:\n")
cat("Y  = Skor kualitas kopi Arabika\n")
cat("X1 = Cacat biji kategori 1\n")
cat("X2 = Cacat biji kategori 2\n")
cat("X3 = Kadar air kopi\n")


# ============================================================
# 8. Uji Simultan atau Uji F
# ============================================================

uji_f <- anova(model)
uji_f

kable(uji_f, digits = 4, caption = "Hasil Uji Simultan Model Regresi")


# ============================================================
# 9. Uji Parsial atau Uji t
# ============================================================

uji_t <- summary(model)$coefficients
uji_t

kable(uji_t, digits = 4, caption = "Hasil Uji Parsial Koefisien Regresi")


# ============================================================
# 10. Koefisien Determinasi
# ============================================================

hasil_model <- summary(model)

r_squared <- hasil_model$r.squared
adjusted_r_squared <- hasil_model$adj.r.squared

cat("Nilai R-squared:", round(r_squared, 4), "\n")
cat("Nilai Adjusted R-squared:", round(adjusted_r_squared, 4), "\n")

cat("Interpretasi: Variabel cacat biji kategori 1, cacat biji kategori 2, dan kadar air mampu menjelaskan variasi skor kualitas kopi Arabika sebesar",
    round(r_squared * 100, 2), "%.\n")


# ============================================================
# 11. Uji Asumsi Regresi
# ============================================================

# 11.1 Uji Normalitas Residual
residual_model <- residuals(model)

uji_normalitas <- shapiro.test(residual_model)
uji_normalitas

# 11.2 Uji Multikolinearitas
vif_model <- vif(model)
vif_model

# 11.3 Uji Heteroskedastisitas
uji_heteroskedastisitas <- bptest(model)
uji_heteroskedastisitas

# 11.4 Uji Autokorelasi
# Catatan: uji ini lebih umum untuk data runtut waktu.
# Pada data cross-section, hasilnya hanya sebagai informasi tambahan.
uji_autokorelasi <- dwtest(model)
uji_autokorelasi

# 11.5 Diagnostic Plot Model
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))


# ============================================================
# 12. Visualisasi Data
# ============================================================

# 12.1 Hubungan kadar air dan skor kualitas kopi
grafik_kadar_air <- ggplot(data_regresi, aes(x = kadar_air, y = skor_kualitas)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Moisture Percentage and Arabica Coffee Quality Score",
    x = "Moisture Percentage (%)",
    y = "Total Cup Points"
  ) +
  theme_minimal()

grafik_kadar_air

# 12.2 Hubungan cacat kategori 1 dan skor kualitas kopi
grafik_cacat_1 <- ggplot(data_regresi, aes(x = cacat_kategori_1, y = skor_kualitas)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Category One Defects and Arabica Coffee Quality Score",
    x = "Category One Defects",
    y = "Total Cup Points"
  ) +
  theme_minimal()

grafik_cacat_1

# 12.3 Hubungan cacat kategori 2 dan skor kualitas kopi
grafik_cacat_2 <- ggplot(data_regresi, aes(x = cacat_kategori_2, y = skor_kualitas)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Category Two Defects and Arabica Coffee Quality Score",
    x = "Category Two Defects",
    y = "Total Cup Points"
  ) +
  theme_minimal()

grafik_cacat_2

# 12.4 Distribusi skor kualitas kopi
grafik_distribusi_skor <- ggplot(data_regresi, aes(x = skor_kualitas)) +
  geom_histogram(bins = 30, alpha = 0.8) +
  labs(
    title = "Distribution of Arabica Coffee Quality Scores",
    x = "Total Cup Points",
    y = "Frequency"
  ) +
  theme_minimal()

grafik_distribusi_skor


# ============================================================
# 13. Menyimpan Output Gambar
# ============================================================

ggsave("grafik_kadar_air_skor_kualitas.png", grafik_kadar_air, width = 7, height = 5, dpi = 300)
ggsave("grafik_cacat_kategori_1_skor_kualitas.png", grafik_cacat_1, width = 7, height = 5, dpi = 300)
ggsave("grafik_cacat_kategori_2_skor_kualitas.png", grafik_cacat_2, width = 7, height = 5, dpi = 300)
ggsave("grafik_distribusi_skor_kualitas.png", grafik_distribusi_skor, width = 7, height = 5, dpi = 300)


# ============================================================
# 14. Menyimpan Output Tabel
# ============================================================

write.csv(tabel_deskriptif, "tabel_statistik_deskriptif.csv", row.names = FALSE)
write.csv(tabel_regresi, "tabel_hasil_regresi.csv", row.names = FALSE)
write.csv(as.data.frame(uji_f), "tabel_uji_f.csv", row.names = TRUE)
write.csv(as.data.frame(uji_t), "tabel_uji_t.csv", row.names = TRUE)
write.csv(as.data.frame(vif_model), "tabel_vif.csv", row.names = TRUE)


# ============================================================
# 15. Interpretasi Otomatis Singkat
# ============================================================

cat("\n============================================================\n")
cat("RINGKASAN HASIL ANALISIS\n")
cat("============================================================\n")

cat("Jumlah data yang dianalisis:", nrow(data_regresi), "\n")
cat("R-squared:", round(r_squared, 4), "\n")
cat("Adjusted R-squared:", round(adjusted_r_squared, 4), "\n")

cat("\nInterpretasi umum:\n")
cat("Model regresi linear berganda digunakan untuk menganalisis pengaruh cacat biji kategori 1, cacat biji kategori 2, dan kadar air terhadap skor kualitas kopi Arabika.\n")
cat("Nilai R-squared menunjukkan proporsi variasi skor kualitas kopi yang dapat dijelaskan oleh ketiga variabel independen tersebut.\n")
cat("Nilai signifikansi pada uji t digunakan untuk melihat pengaruh parsial masing-masing variabel, sedangkan uji F digunakan untuk melihat signifikansi model secara simultan.\n")