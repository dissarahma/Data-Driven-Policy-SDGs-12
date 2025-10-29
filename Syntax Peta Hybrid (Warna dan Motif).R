install.packages("ggplot2")
install.packages("ggpattern")
install.packages("sf")


### 19 Kelompok ###
library(sf)
library(ggpattern)
library(ggplot2)
library(ggspatial)
library(openxlsx)
library(dplyr)

map_data <- st_read("C:/Users/Dissa/Documents/Bismillah/gadm41_IDN_3/gadm41_IDN_3.shp")

# Filter data untuk wilayah kabupaten Bandung
kb_boundary <- map_data %>% filter(NAME_1 == "Jawa Barat", NAME_2 == 'Bandung')

### VARIABEL 19 KELOMPOK ###
wilayah_kelompok1 <- c("Cicalengka", "Cikancung", "Ciparay",
                       "Katapang", "Kutawaringin", "Pameungpeuk")
wilayah_kelompok2 <- c("Margahayu", "Rancaekek")
wilayah_kelompok3 <- c("Rancabali")
wilayah_kelompok4 <- c("Pangalengan")
wilayah_kelompok5 <- c("Kertasari")
wilayah_kelompok6 <- c("Ibun")
wilayah_kelompok7 <- c("Cimaung")
wilayah_kelompok8 <- c("Arjasari")
wilayah_kelompok9 <- c("Paseh")
wilayah_kelompok10 <- c("Cilengkrang")
wilayah_kelompok11 <- c("Cimenyan")
wilayah_kelompok12 <- c("Ciwidey", "Nagreg", "Pacet")
wilayah_kelompok13 <- c("Margaasih")
wilayah_kelompok14 <- c("Pasirjambu", "Soreang")
wilayah_kelompok15 <- c("Banjaran")
wilayah_kelompok16 <- c("Cileunyi","Dayeuhkolot", "Solokan Jeruk", "Majalaya")
wilayah_kelompok17 <- c("Cangkuang")
wilayah_kelompok18 <- c("Bojongsoang")
wilayah_kelompok19 <- c("Baleendah")


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
ggplot() +
  geom_sf(data = kelompok1, fill = "gray90", alpha = 1) +
  geom_sf_pattern(data = kelompok2,
                  aes(pattern = "stripe"),
                  fill = "gray90",
                  pattern_fill = "white",
                  pattern_color = "black",
                  pattern_density = 0.2,
                  pattern_spacing = 0.03,
                  pattern_angle = 45,
                  color = "black") +
  geom_sf(data = kelompok3, fill = "#FFB3B3", alpha = 1) +
  geom_sf_pattern(data = kelompok4,
                  aes(pattern = "crosshatch"),
                  fill = "#FFB3B3",
                  pattern_fill = "white",
                  pattern_color = "black",
                  pattern_density = 0.2,
                  pattern_spacing = 0.03,
                  pattern_angle = 45,
                  color = "black") +
  geom_sf_pattern(data = kelompok5,
                  aes(pattern = "stripe"),
                  fill = "#FF4C85",
                  pattern_fill = "white",
                  pattern_color = "black",
                  pattern_density = 0.2,
                  pattern_spacing = 0.03,
                  pattern_angle = 0,
                  color = "black") +
  geom_sf(data = kelompok6, fill = "#CC0000", alpha = 1) +
  geom_sf_pattern(data = kelompok7,
                  aes(pattern = "stripe"),
                  fill = "#CC0000",
                  pattern_fill = "white",
                  pattern_color = "black",
                  pattern_density = 0.2,
                  pattern_spacing = 0.03,
                  pattern_angle = 45,
                  color = "black") +
  geom_sf_pattern(data = kelompok8,
                  aes(pattern = "wave"),
                  fill = "#CC0000",
                  pattern_fill = "white",
                  pattern_color = "black",
                  pattern_density = 0.2,
                  pattern_spacing = 0.03,
                  pattern_angle = 45,
                  color = "black") +
  geom_sf_pattern(data = kelompok9,
                  aes(pattern = "crosshatch"),
                  fill = "#CC0000",
                  pattern_fill = "white",
                  pattern_color = "black",
                  pattern_density = 0.2,
                  pattern_spacing = 0.03,
                  pattern_angle = 45,
                  color = "black") +
  geom_sf_pattern(data = kelompok10,
                  aes(pattern = "crosshatch"),
                  fill = "darkred",
                  pattern_fill = "white",
                  pattern_color = "black",
                  pattern_density = 0.2,
                  pattern_spacing = 0.03,
                  pattern_angle = 45,
                  color = "black") +
  geom_sf(data = kelompok11, fill = "#B3E0FF", alpha = 1) +
  geom_sf(data = kelompok12, fill = "#4DA6FF", alpha = 1) +
  geom_sf(data = kelompok13, fill = "#004080", alpha = 1) +
  geom_sf(data = kelompok14, fill = "#FFFFB3", alpha = 1) +
  geom_sf_pattern(data = kelompok15,
                  aes(pattern = "wave"),
                  fill = "yellow",
                  pattern_fill = "white",
                  pattern_color = "black",
                  pattern_density = 0.2,
                  pattern_spacing = 0.03,
                  pattern_angle = 45,
                  color = "black") +
  geom_sf(data = kelompok16, fill = "#B3FFB3", alpha = 1) +
  geom_sf_pattern(data = kelompok17,
                  aes(pattern = "wave"),
                  fill = "#B3FFB3",
                  pattern_fill = "white",
                  pattern_color = "black",
                  pattern_density = 0.2,
                  pattern_spacing = 0.03,
                  pattern_angle = 45,
                  color = "black") +
  geom_sf_pattern(data = kelompok18,
                  aes(pattern = "stripe"),
                  fill = "#FF4C85",
                  pattern_fill = "white",
                  pattern_color = "black",
                  pattern_density = 0.2,
                  pattern_spacing = 0.03,
                  pattern_angle = 0,
                  color = "black") +
  geom_sf(data = kelompok19, fill = "#008000", alpha = 1) +
  geom_sf(
    data = kb_boundary,
    color = "black",
    linewidth = 0.5,  # Lebih tebal
    fill = NA
  )+
  geom_sf_label(
    data = kb_boundary,
    aes(geometry = st_centroid(geometry), label = NAME_3),
    stat = "sf_coordinates",
    color = "black",
    fill = "white",        # Warna kotaknya
    alpha = 1,           # Transparansi kotak
    size = 2.3,
    fontface = "bold"
  )+
  
  # Hilangkan legenda
  theme_minimal() +
  theme(
    legend.position = "none"
  )




#--- Plot Warna Kategori

library(ggplot2)
library(ggpattern)
library(cowplot)

# --- 1. Data warna dengan kategori yang benar
kategori_labels <- c(
  "Tidak memiliki permasalahan pola konsumsi dan produksi",
  "Tempat pembuangan sampah berupa drainase (got/selokan)",
  "Tempat pembuangan sampah berupa drainase (got/selokan), Tidak ada kegiatan daur ulang sampah/limbah",
  "Tempat pembuangan sampah berupa drainase (got/selokan), Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah",
  "Tempat pembuangan sampah berupa drainase (got/selokan), Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai",
  "Tidak memiliki TPS",
  "Tidak memiliki TPS, Tempat pembuangan sampah berupa drainase (got/selokan)",
  "Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai",
  "Tidak ada kegiatan daur ulang sampah/limbah",
  "Ada pencemaran limbah sungai",
  "Ada pencemaran limbah sungai, Tidak ada RPJM desa"
)

warna_kategori <- c(
  "Tidak memiliki permasalahan pola konsumsi dan produksi" = "gray90",
  "Tempat pembuangan sampah berupa drainase (got/selokan)" = "#FFB3B3",
  "Tempat pembuangan sampah berupa drainase (got/selokan), Tidak ada kegiatan daur ulang sampah/limbah" = "#FFAEC9",
  "Tempat pembuangan sampah berupa drainase (got/selokan), Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah" = "#CC0000",
  "Tempat pembuangan sampah berupa drainase (got/selokan), Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai" = "darkred",
  "Tidak memiliki TPS" = "#B3E0FF",
  "Tidak memiliki TPS, Tempat pembuangan sampah berupa drainase (got/selokan)" = "#4DA6FF",
  "Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai" = "#004080",
  "Tidak ada kegiatan daur ulang sampah/limbah" = "#FFFFB3",
  "Ada pencemaran limbah sungai" = "#B3FFB3",
  "Ada pencemaran limbah sungai, Tidak ada RPJM desa" = "#008000"
)

data_kategori <- data.frame(Kategori = factor(kategori_labels, levels = kategori_labels))

plot_dummy_warna <- ggplot(data_kategori, aes(x = Kategori, fill = Kategori)) +
  geom_bar() +
  scale_fill_manual(
    name = "Keterangan Warna",
    values = warna_kategori,
    guide = guide_legend(override.aes = list(colour = "black", size = 4))
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.8, "cm")
  )
legend_warna <- cowplot::get_legend(plot_dummy_warna)


# --- 2. Data dummy pattern legend
pattern_data <- data.frame(
  Motif = c("Rasio sarana pendidikan tergolong rendah", 
            "Rasio pasar tergolong rendah", 
            "Rasio tempat peribadatan  tergolong rendah",
            "Rasio pasar dan tempat peribadatan tergolong rendah"),
  Nilai = 1,
  pattern = c("stripe", "wave", "crosshatch", "wave"),
  density = c(0.2, 0.2, 0.2, 0.6)
)

plot_dummy_pattern <- ggplot(pattern_data, aes(x = Motif, y = Nilai, pattern = pattern)) +
  geom_bar_pattern(
    stat = "identity",
    fill = "white",
    colour = "black",
    pattern_fill = "black",
    pattern_density = pattern_data$density,  # Sesuaikan density tiap bar
    pattern_spacing = 0.05,
    pattern_key_scale_factor = 0.6
  ) +
  scale_pattern_manual(
    values = setNames(pattern_data$pattern, pattern_data$Motif),
    guide = guide_legend(title = "Keterangan Motif")
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.8, "cm")
  )

# --- 3. Gabungkan legend warna dan motif dengan jarak lebih rapat
final_legend <- cowplot::plot_grid(
  legend_warna, legend_motif,
  ncol = 1, align = "v",
  rel_heights = c(1, 0.7)  # Atur tinggi relatif agar lebih rapat
)

# Tambahkan pengaturan margin jika perlu
legend_warna <- cowplot::get_legend(plot_dummy_warna + theme(plot.margin = margin(0, 0, -10, 0)))
legend_motif <- cowplot::get_legend(plot_dummy_pattern + theme(plot.margin = margin(-10, 0, 0, 0)))

cowplot::ggdraw(final_legend)

# --- 3. Gabungkan legend warna dan motif
final_legend <- cowplot::plot_grid(
  legend_warna, legend_motif,
  ncol = 1, align = "v"
)

cowplot::ggdraw(final_legend)






























# Keterangan Warna Saja
library(ggplot2)
library(cowplot)

# Data kategori yang benar
kategori_labels <- c(
  "Tidak memiliki permasalahan pola konsumsi dan produksi",
  "Tempat pembuangan sampah berupa drainase (got/selokan)",
  "Tempat pembuangan sampah berupa drainase (got/selokan)", "Tidak ada kegiatan daur ulang sampah/limbah",
  "Tempat pembuangan sampah berupa drainase (got/selokan)", "Tidak memiliki TPS", "Tidak ada kegiatan daur ulang sampah/limbah",
  "Tempat pembuangan sampah berupa drainase (got/selokan), Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai",
  "Tidak memiliki TPS",
  "Tidak memiliki TPS, Tempat pembuangan sampah berupa drainase (got/selokan)",
  "Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai",
  "Tidak ada kegiatan daur ulang sampah/limbah",
  "Tidak ada kegiatan daur ulang sampah/limbah","Ada pencemaran limbah sungai",
  "Ada pencemaran limbah sungai",
  "Ada pencemaran limbah sungai, Tidak ada RPJM desa",
)

data_kategori <- data.frame(Kategori = factor(kategori_labels, levels = kategori_labels))

# Plot legenda
legend_plot <- ggplot(data_kategori, aes(x = Kategori, fill = Kategori)) +
  geom_bar() +
  scale_fill_manual(
    name = "Keterangan",
    values = c(
      "Tidak memiliki permasalahan pola konsumsi dan produksi" = "gray90",
      "Tempat pembuangan sampah berupa drainase (got/selokan)" = "#FFB3B3",
      "Tempat pembuangan sampah berupa drainase (got/selokan), Tidak ada kegiatan daur ulang sampah/limbah" = "#FFAEC9",
      "Tempat pembuangan sampah berupa drainase (got/selokan), Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah" = "#CC0000",
      "Tempat pembuangan sampah berupa drainase (got/selokan), Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai" = "darkred",
      "Tidak memiliki TPS" = "#B3E0FF",
      "Tidak memiliki TPS, Tempat pembuangan sampah berupa drainase (got/selokan)" = "#4DA6FF",
      "Tidak memiliki TPS, Tidak ada kegiatan daur ulang sampah/limbah, Ada pencemaran limbah sungai" = "#004080",
      "Tidak ada kegiatan daur ulang sampah/limbah" = "#FFFFB3",
      "Tidak ada kegiatan daur ulang sampah/limbah", "Ada pencemaran limbah sungai" = "yellow",
      "Ada pencemaran limbah sungai" = "#B3FFB3",
      "Ada pencemaran limbah sungai, Tidak ada RPJM desa" = "#008000"
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




# Data keterangan motif
pattern_legenda <- data.frame(
  Keterangan = c("TPS Bermasalah", "Tidak Ada Daur Ulang", "Pencemaran Limbah"),
  fill = rep("white", 3),  # Warna dasar (tidak relevan jika pakai pattern)
  pattern = c("stripe", "square", "crosshatch")
)

pattern_plot <- ggplot(pattern_legenda, aes(
  x = 1, y = Keterangan,
  fill = fill, pattern = pattern
)) +
  geom_tile_pattern(
    color = "black",
    width = 0.6,
    height = 0.6,
    pattern_fill = "black",
    pattern_density = 0.5,
    pattern_spacing = 0.05,
    pattern_key_scale_factor = 0.6
  ) +
  scale_pattern_identity() +
  scale_fill_identity() +
  labs(pattern = "Motif") +
  theme_void() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    plot.margin = margin(0, 0, 0, 10)
  )
# Gabungkan legend warna dan motif
final_legend <- cowplot::plot_grid(
  legend, pattern_plot,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 1)
)

cowplot::ggdraw(final_legend)
