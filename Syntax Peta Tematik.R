

### 19 Kelompok ###
library(sf)
library(ggplot2)
library(ggspatial)
library(openxlsx)
library(dplyr)

map_data <- st_read("C:/Users/Dissa/Documents/Bismillah/gadm41_IDN_3/gadm41_IDN_3.shp")

# Filter data untuk wilayah kabupaten Bandung
kb_boundary <- map_data %>% filter(NAME_1 == "Jawa Barat", NAME_2 == 'Bandung')

### VARIABEL 19 KELOMPOK ###
wilayah_kelompok1 <- c("Bojongsoang", "Cangkuang", "Cikancung",
                       "Majalaya", "Margahayu", "Pameungpeuk")
wilayah_kelompok2 <- c("Pangalengan", "Rancabali")
wilayah_kelompok3 <- c("Kertasari")
wilayah_kelompok4 <- c("Cimaung", "Ibun")
wilayah_kelompok5 <- c("Cilengkrang")
wilayah_kelompok6 <- c("Cimenyan")
wilayah_kelompok7 <- c("Nagreg", "Pacet")
wilayah_kelompok8 <- c("Margaasih")
wilayah_kelompok9 <- c("Pasirjambu", "Soreang")
wilayah_kelompok10 <- c("Banjaran")
wilayah_kelompok11 <- c("Cileunyi", "Solokan Jeruk")
wilayah_kelompok12 <- c("Baleendah")
wilayah_kelompok13 <- c("Dayeuhkolot", "Rancaekek")
wilayah_kelompok14 <- c("Ciparay", "Kutawaringin")
wilayah_kelompok15 <- c("Ciwidey")
wilayah_kelompok16 <- c("Cicalengka")
wilayah_kelompok17 <- c("Katapang")
wilayah_kelompok18 <- c("Arjasari")
wilayah_kelompok19 <- c("Paseh")

# Mapping ke dataframe
kelompok1 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok1)
kelompok2 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok2)
kelompok3 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok3)
kelompok4 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok4)
kelompok5 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok5)
kelompok6 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok6)
kelompok7 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok7)
kelompok8 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok8)
kelompok9 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok9)
kelompok10 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok10)
kelompok11 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok11)
kelompok12 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok12)
kelompok13 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok13)
kelompok14 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok14)
kelompok15 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok15)
kelompok16 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok16)
kelompok17 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok17)
kelompok18 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok18)
kelompok19 <- kb_boundary %>% filter(NAME_3 %in% wilayah_kelompok19)

# Peta tanpa keterangan warna
# Peta tanpa keterangan warna
ggplot() +
  geom_sf(data = kelompok1, fill = "gray90", alpha = 1) +
  geom_sf(data = kelompok2, fill = "#FFB3B3", alpha = 1) +
  geom_sf(data = kelompok3, fill = "#FF1666", alpha = 1) +
  geom_sf(data = kelompok4, fill = "#CC0000", alpha = 1) +
  geom_sf(data = kelompok5, fill = "darkred", alpha = 1) +
  geom_sf(data = kelompok6, fill = "#FFD9B3", alpha = 1) +
  geom_sf(data = kelompok7, fill = "#FFA64D", alpha = 1) +
  geom_sf(data = kelompok8, fill = "#DC5200", alpha = 1) +
  geom_sf(data = kelompok9, fill = "#FFFFB3", alpha = 1) +
  geom_sf(data = kelompok10, fill = "yellow", alpha = 1) +
  geom_sf(data = kelompok11, fill = "#B3FFB3", alpha = 1) +
  geom_sf(data = kelompok12, fill = "#008000", alpha = 1) +
  geom_sf(data = kelompok13, fill = "#B3E0FF", alpha = 1) +
  geom_sf(data = kelompok14, fill = "#4DA6FF", alpha = 1) +
  geom_sf(data = kelompok15, fill = "#004080", alpha = 1) +
  geom_sf(data = kelompok16, fill = "#E0B3FF", alpha = 1) +
  geom_sf(data = kelompok17, fill = "purple", alpha = 1) +
  geom_sf(data = kelompok18, fill = "#8000fc", alpha = 1) +
  geom_sf(data = kelompok19, fill = "#400090", alpha = 1) +
  geom_sf_text(data = kb_boundary, aes(geometry = st_centroid(geometry), label = NAME_3),
               color = "black", size = 2.3, fontface = "bold") +
  theme_minimal() +
  theme(legend.position = "none") + # Menghilangkan legenda
  labs(
    title = "Peta Kabupaten Bandung",
    subtitle = "Karakteristik Pola Konsumsi dan Produksi yang Perlu Peningkatan"
  )




# Keterangan Warna Saja
library(ggplot2)
library(cowplot)

# Data kategori yang benar
kategori_labels <- c(
  "Tidak memiliki permasalahan pola konsumsi dan produksi",
  "Tempat pembuangan sampah bermasalah",
  "Tempat pembuangan sampah bermasalah, Tidak ada kegiatan daur ulang sampah/limbah",
  "Tempat pembuangan sampah bermasalah, Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah",
  "Tempat pembuangan sampah bermasalah, Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai",
  "Tidak memiliki TPS",
  "Tidak memiliki TPS, Tempat pembuangan sampah bermasalah",
  "Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai",
  "Tidak ada kegiatan daur ulang sampah/limbah",
  "Tidak ada kegiatan daur ulang sampah/limbah, Rasio sarana pendidikan tergolong rendah",
  "Ada pencemaran limbah sungai",
  "Ada pencemaran limbah sungai, Tidak ada RPJM desa",
  "Rasio tempat peribadatan tergolong rendah",
  "Rasio tempat peribadatan tergolong rendah, Rasio pasar tergolong rendah",
  "Rasio tempat peribadatan tergolong rendah, Tempat pembuangan sampah bermasalah, Tidak memiliki TPS",
  "Rasio sarana pendidikan tergolong rendah",
  "Rasio sarana pendidikan tergolong rendah, Ada pencemaran limbah sungai",
  "Rasio sarana pendidikan tergolong rendah, Tempat pembuangan sampah bermasalah, Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah",
  "Rasio sarana pendidikan tergolong rendah, Rasio tempat peribadatan tergolong rendah, Tempat pembuangan sampah bermasalah, Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah"
)

data_kategori <- data.frame(Kategori = factor(kategori_labels, levels = kategori_labels))

# Plot legenda
legend_plot <- ggplot(data_kategori, aes(x = Kategori, fill = Kategori)) +
  geom_bar() +
  scale_fill_manual(
    name = "Keterangan",
    values = c(
      "Tidak memiliki permasalahan pola konsumsi dan produksi" = "gray90",
      "Tempat pembuangan sampah bermasalah" = "#FFB3B3",
      "Tempat pembuangan sampah bermasalah, Tidak ada kegiatan daur ulang sampah/limbah" = "#FF1666",
      "Tempat pembuangan sampah bermasalah, Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah" = "#CC0000",
      "Tempat pembuangan sampah bermasalah, Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai" = "darkred",
      "Tidak memiliki TPS" = "#FFD9B3",
      "Tidak memiliki TPS, Tempat pembuangan sampah bermasalah" = "#FFA64D",
      "Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai" = "#DC5200",
      "Tidak ada kegiatan daur ulang sampah/limbah" = "#FFFFB3",
      "Tidak ada kegiatan daur ulang sampah/limbah, Rasio sarana pendidikan tergolong rendah" = "yellow",
      "Ada pencemaran limbah sungai" = "#B3FFB3",
      "Ada pencemaran limbah sungai, Tidak ada RPJM desa" = "#008000",
      "Rasio tempat peribadatan tergolong rendah" = "#B3E0FF",
      "Rasio tempat peribadatan tergolong rendah, Rasio pasar tergolong rendah" = "#4DA6FF",
      "Rasio tempat peribadatan tergolong rendah, Tempat pembuangan sampah bermasalah, Tidak memiliki TPS" = "#004080",
      "Rasio sarana pendidikan tergolong rendah" = "#E0B3FF",
      "Rasio sarana pendidikan tergolong rendah, Ada pencemaran limbah sungai" = "purple",
      "Rasio sarana pendidikan tergolong rendah, Tempat pembuangan sampah bermasalah, Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah" = "#8000fc",
      "Rasio sarana pendidikan tergolong rendah, Rasio tempat peribadatan tergolong rendah, Tempat pembuangan sampah bermasalah, Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah" = "#400090"
    ),
    breaks = kategori_labels,
    guide = guide_legend(override.aes = list(colour = "black", size = 4)) # Menambahkan outline hitam
  ) +
  theme_void() +
  theme(legend.position = "right") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10), # Ukuran teks keterangan lebih kecil
    legend.title = element_text(size = 12, face = "bold"), # Judul legenda lebih besar dan tebal
    legend.key.width = unit(0.8, "cm"), # Menyesuaikan lebar kotak legenda
    legend.key.height = unit(0.8, "cm") # Menyesuaikan tinggi kotak legenda
  )

legend <- cowplot::get_legend(legend_plot)
cowplot::ggdraw(legend)





