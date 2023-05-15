

# bilirkisi_hesaplama_fonksiyonu <- function() {


# Load Libraries ----

library(readxl)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(formattable)

# Dosya Yükle ----

dosya_bilgiler <- read_excel("../Erdenerus_App/data/dosya_bilgiler.xlsx", sheet = "Sayfa1")

PR_TRH_2010 <- read_excel("../Erdenerus_App/data/TRH2010_son.xlsx", sheet = "Sayfa1")

Asgari_Tablo <- read_excel("../Erdenerus_App/data/Asgari_Ucret_Tablosu.xlsx", sheet = "Program")

Teminat_Limit_Tablosu <- read_excel("../Erdenerus_App/data/Teminat_Limit_Tablosu.xlsx", sheet = "Teminat")


# Dosya Bilgileri ----

dosya_no <- "123"
dosya <- dosya_bilgiler %>% filter(Dosya_No == dosya_no)


# Genel Bilgiler ----

Teknik_Faiz <- 0
pasif_donem_yas <- 60
faiz_oranı <- 9

Yasam_Tablosu <- "TRH-2010"

# Kişisel Bilgiler ----

Ad_Soyad <- dosya_bilgiler$Ad_Soyad
Cinsiyet <- dosya_bilgiler$Cinsiyet                         
Dogum_Tarihi <- as.Date(dosya_bilgiler$Dogum_Tarihi)
Gelir_Durumu <- dosya_bilgiler$Gelir
Kaza_Tarihi <- as.Date(dosya_bilgiler$Kaza_Tarihi)
Maluliyet_Oranı <- dosya_bilgiler$Maluliyet_Orani
Kusur_Oranı <- dosya_bilgiler$Kusur_Orani
Gecici_Maluliyet_sure <- dosya_bilgiler$Gecici_Maluliyet_sure
Kısmi_Odeme_Sayısı <- dosya_bilgiler$Kısmi_Odeme_Sayısı
Kısmi_Odeme_Tarihi_1 <- as.Date(dosya_bilgiler$Kısmi_Odeme_Tarihi_1)
Kısmi_Odeme_Tutarı_1 <- dosya_bilgiler$Kısmi_Odeme_Tutarı_1
Kısmi_Odeme_Tarihi_2 <- as.Date(dosya_bilgiler$Kısmi_Odeme_Tarihi_2)
Kısmi_Odeme_Tutarı_2 <- dosya_bilgiler$Kısmi_Odeme_Tutarı_2


# Bakıcı Gideri ----

Bakıcı_gideri <- dosya_bilgiler$Bakıcı_Gideri
Bakıcı_gideri_suresi <- dosya_bilgiler$`Bakıcı_Gideri_Süresi_(ay)`


# Genel Gelir Tablosu ----

Gelir_tablo <-  Asgari_Tablo %>%
  separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
  mutate(D_B = as.Date(Donem_Baslangic),
         D_S = as.Date(Donem_Son))


# Kaza Tarihi Teminat Limiti


Teminat_Limit_Tablosu <- Teminat_Limit_Tablosu %>% 
  separate(Donem, sep = "/", into = c("Donem_Baslangic", "Donem_Son")) %>%
  mutate(D_B = as.Date(Donem_Baslangic),
         D_S = as.Date(Donem_Son))

kaza_tarihi_teminat <- Teminat_Limit_Tablosu %>% 
  filter(D_S >= Kaza_Tarihi & Kaza_Tarihi > D_B)

kaza_tarihi_teminat_limiti <- as.numeric(format(kaza_tarihi_teminat$Teminat_Limiti, scientific = FALSE))

# 1-  YAS HESAPLAMALARI ----

Hesap_Tarihi <- Sys.Date()
Hesap_Tarihi_Sirket <- Kısmi_Odeme_Tarihi_1

kaza_tarihi_yas <- round(lubridate::time_length(difftime(Kaza_Tarihi, Dogum_Tarihi), "year"))  
hesap_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi, Dogum_Tarihi), "year")) 

sirket_odeme_tarihi_yas <- round(lubridate::time_length(difftime(Hesap_Tarihi_Sirket, Dogum_Tarihi), "year")) 


# 2- HESAPLAMADA KULLANILACAK TRH TABLOSU ----

## 2.1 Hesaplama Tarihine göre beklenen ömür ----

erkek_table <- PR_TRH_2010 %>% 
  select(Yas, Erkek)

kadın_table <- PR_TRH_2010 %>% 
  select(Yas, Kadın) 


hesaplama_tarihi_PR_TRH <- if (Cinsiyet == "Erkek") {
  filtered_PR_TRH <- erkek_table %>% 
    filter(Yas == hesap_tarihi_yas) %>% 
    select(Erkek)
} else {
  filtered_PR_TRH <- kadın_table %>% 
    filter(Yas == hesap_tarihi_yas) %>% 
    select(Kadın)
}


hesaplama_tarihi_beklenen_omur <- ifelse(Cinsiyet == "Erkek", hesaplama_tarihi_PR_TRH$Erkek, hesaplama_tarihi_PR_TRH$Kadın)
hesaplama_tarihi_beklenen_omur <- round(hesaplama_tarihi_beklenen_omur,digits = 2)


## 2.2 Şirket Ödeme Tarihine göre beklenen ömür ----

sirket_odeme_tarihi_PR_TRH <- if (Cinsiyet == "Erkek") {
  filtered_PR_TRH <- erkek_table %>% 
    filter(Yas == sirket_odeme_tarihi_yas) %>% 
    select(Erkek)
} else {
  filtered_PR_TRH <- kadın_table %>% 
    filter(Yas == sirket_odeme_tarihi_yas) %>% 
    select(Kadın)
}


sirket_odeme_beklenen_omur <- ifelse(Cinsiyet == "Erkek", sirket_odeme_tarihi_PR_TRH$Erkek, sirket_odeme_tarihi_PR_TRH$Kadın)
sirket_odeme_beklenen_omur <- round(sirket_odeme_beklenen_omur,digits = 2)



## 2.3 Pasif Dönem Yas- TRH Tablosu ----

hesaplama_tarihi_PR_TRH_pasif <- 
  if (Cinsiyet == "Erkek") {
    erkek_table %>% 
      filter(Yas == pasif_donem_yas)
  } else {
    kadın_table %>% 
      filter(Yas == pasif_donem_yas)
  }


hesap_tarihi_pasif_beklenen_omur <- ifelse(Cinsiyet == "Erkek", hesaplama_tarihi_PR_TRH_pasif$Erkek, hesaplama_tarihi_PR_TRH_pasif$Kadın)
hesap_tarihi_pasif_beklenen_omur <- round(hesap_tarihi_pasif_beklenen_omur,digits = 2)


# 3- PARAMATRE TABLOSU ----


parametre_tablosu <- data.frame(PARAMETRE = c("Dosya_No", "Ad-Soyad", "Kaza Tarihi Yas","Hesap Tarihi Yas", "Cinsiyet", "Kaza Tarihi", 
                                              "Maluliyet Oranı", "Teknik Faiz", "Yaşam Tablosu", "Kusur Oranı", "Geçici İş Göremezlik Süresi (ay)", 
                                              "Kısmi Ödeme Sayısı", "Ödeme Tarihi-1", "Ödeme Tutarı-1","Ödeme Tarihi-2", "Ödeme Tutarı-2"),
                                DEĞER = c(dosya_no, Ad_Soyad, kaza_tarihi_yas, hesap_tarihi_yas, Cinsiyet,as.character(Kaza_Tarihi), 
                                          Maluliyet_Oranı, Teknik_Faiz, Yasam_Tablosu, Kusur_Oranı, Gecici_Maluliyet_sure, 
                                          Kısmi_Odeme_Sayısı, as.character(Kısmi_Odeme_Tarihi_1), Kısmi_Odeme_Tutarı_1, as.character(Kısmi_Odeme_Tarihi_2), Kısmi_Odeme_Tutarı_2)
)

parametre_tablosu <- parametre_tablosu %>% filter(DEĞER != "none")

parametre_tablosu2 <- tibble(
  "Dosya_No" = dosya_no,
  "Ad-Soyad" = Ad_Soyad,
  "Yas" = kaza_tarihi_yas,
  "Hesap Tarihi Yas" = hesap_tarihi_yas,
  "Cinsiyet" = Cinsiyet, 
  "Kaza Tarihi" = as.character(Kaza_Tarihi),
  "Maluliyet Oranı" = Maluliyet_Oranı,
  "Teknik Faiz" = Teknik_Faiz,
  "Yaşam Tablosu" = Yasam_Tablosu,
  "Kusur Oranı" = Kusur_Oranı,
  "Geçici İş Göremezlik Süresi (ay)" = Gecici_Maluliyet_sure,
  "Kısmi Ödeme Sayısı" = Kısmi_Odeme_Sayısı,
  "Ödeme Tarihi-1" = Kısmi_Odeme_Tarihi_1,
  "Ödeme Tutarı-1" = Kısmi_Odeme_Tutarı_1,
  "Ödeme Tarihi-2" = Kısmi_Odeme_Tarihi_2,
  "Ödeme Tutarı-2" = Kısmi_Odeme_Tutarı_2
)

# 4- ŞİRKET ÖDEME TARİHİ İLE HESAPLAMA ----

## 4.1 Bilinen Dönem Hesaplaması ----

### 4.1.1 Tam Maluliyet Donemi Tablosu ----

maluliyet_tarihi_baslangıc <- Kaza_Tarihi
maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (Gecici_Maluliyet_sure * 30)

Maluliyet_Donemi_tablosu <- Gelir_tablo %>% 
  filter(D_S >= maluliyet_tarihi_baslangıc & maluliyet_tarihi_bitis > D_B)

Maluliyet_Donemi_tablosu$Donem_Baslangic[1] <- as.character(maluliyet_tarihi_baslangıc)
Maluliyet_Donemi_tablosu$Donem_Son[nrow(Maluliyet_Donemi_tablosu)] <- as.character(maluliyet_tarihi_bitis)


Maluliyet_Donemi_tazminat_hesaplama <- Maluliyet_Donemi_tablosu %>% 
  mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
         Donem_Sonu = as.Date(Donem_Son)) %>% 
  mutate(Kazanilan_Ay = signif(as.numeric(time_length(Donem_Sonu - Donem_Baslangici, "day")/30) , digits = 2)) %>%
  
  mutate(Kazanilan_Ay = case_when(
    Kazanilan_Ay >= 11.96 ~ round(Kazanilan_Ay, digits = 1),
    Kazanilan_Ay >= 5.9 ~ round(Kazanilan_Ay, digits = 0),
    Kazanilan_Ay >= 4.6 ~ plyr::round_any(Kazanilan_Ay, 0.01, f = ceiling),
    Kazanilan_Ay <= 2 ~ plyr::round_any(Kazanilan_Ay, 0.01, f = floor),
    TRUE ~ Kazanilan_Ay)) %>%
  
  mutate(Maluliyet_Oranı = 100) %>% 
  mutate(Kusur_Oranı = Kusur_Oranı) %>% 
  select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
  rename("Gelir" = Gelir_Durumu) %>% 
  mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
         Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>% 
  mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 

Tam_Maluliyet_Donemi_Tablosu <- Maluliyet_Donemi_tazminat_hesaplama  %>% 
  select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)

Tam_Maluliyet_Donemi_Tablosu

### 4.1.2 Tam Maluliyet Sonrası Dönem Tablosu ----


maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)


Maluliyet_Sonrasi_Donem_tablosu <- Gelir_tablo %>% 
  filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= Hesap_Tarihi_Sirket + 180)

hesaplama_tarihi_sirket_chr <- as.character(Hesap_Tarihi_Sirket)

Maluliyet_Sonrasi_Donem_tablosu$Donem_Baslangic[1] <- maluliyet_sonrası_donem_baslangıc_chr
Maluliyet_Sonrasi_Donem_tablosu$Donem_Son[nrow(Maluliyet_Sonrasi_Donem_tablosu)] <- hesaplama_tarihi_sirket_chr


Maluliyet_Sonrasi_Donem_tazminat_hesaplama <- Maluliyet_Sonrasi_Donem_tablosu %>% 
  mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
         Donem_Sonu = as.Date(Donem_Son)) %>% 
  mutate(Kazanilan_Ay = round(time_length(as.Date(Donem_Sonu) - as.Date(Donem_Baslangici), "month") , digits = 2)) %>% 
  
  mutate(Kazanilan_Ay = case_when(
    Kazanilan_Ay >= 11.96 ~ round(Kazanilan_Ay, digits = 1),
    Kazanilan_Ay >= 6.1 ~ plyr::round_any(Kazanilan_Ay, 0.1, f = ceiling),
    Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1, f = ceiling),
    Kazanilan_Ay >= 4.6 ~ plyr::round_any(Kazanilan_Ay, 0.01, f = ceiling),
    Kazanilan_Ay <= 2 ~ plyr::round_any(Kazanilan_Ay, 0.1, f = floor),
    TRUE ~ Kazanilan_Ay)) %>% 
  
  mutate(Maluliyet_Oranı = Maluliyet_Oranı) %>% 
  mutate(Kusur_Oranı = Kusur_Oranı) %>% 
  select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
  rename("Gelir" = Gelir_Durumu) %>%
  mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
         Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>%
  mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 


Maluliyet_Sonrasi_Donem_Tablosu <- Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
  select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)

Maluliyet_Sonrasi_Donem_Tablosu

### 4.1.3 Bilinen Dönem Tablosu ve Tazminatı ----

Bilinen_Donem_Tablosu <- bind_rows(Tam_Maluliyet_Donemi_Tablosu, Maluliyet_Sonrasi_Donem_Tablosu)

Bilinen_Donem_Tazminatı = sum(Bilinen_Donem_Tablosu$Donem_Tazminat)


## 4.2 Bilinmeyen Dönem Hesaplaması ----

### 4.2.1 Aktif Donem ----          

bilinen_son_donem_gelir <- Bilinen_Donem_Tablosu$Gelir[nrow(Bilinen_Donem_Tablosu)]


bilinmeyen_aktif_donem_tazminat <- (pasif_donem_yas-sirket_odeme_tarihi_yas) * bilinen_son_donem_gelir * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
bilinmeyen_aktif_donem_tazminat <- round(bilinmeyen_aktif_donem_tazminat,digits = 2)

sirket_odeme_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                          'Toplam Yıl' = (pasif_donem_yas-sirket_odeme_tarihi_yas),
                                                                          'Toplam Ay' = (pasif_donem_yas-sirket_odeme_tarihi_yas)*12,
                                                                          Gelir = bilinen_son_donem_gelir,
                                                                          'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                          'Kusur Oranı' = Kusur_Oranı,
                                                                          Toplam = bilinmeyen_aktif_donem_tazminat)

# format(son_donem_gelir, big.mark = ",", scientific = FALSE) 


### 4.2.2 Pasif Dönem ----

Asgari_Tablo_pasif_donem_sirket <- Gelir_tablo %>% filter (D_S >= Hesap_Tarihi_Sirket & Hesap_Tarihi_Sirket > D_B)
pasif_donem_geliri <- Asgari_Tablo_pasif_donem_sirket$Bekar


pasif_donem_beklenen_omur <- 
  
  if (sirket_odeme_tarihi_yas <= pasif_donem_yas) {
    
    (sirket_odeme_tarihi_yas + sirket_odeme_beklenen_omur - pasif_donem_yas) 
    
  } else {
    
    sirket_odeme_beklenen_omur
  }



bilinmeyen_pasif_donem_tazminat <- pasif_donem_beklenen_omur * pasif_donem_geliri * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100


sirket_odeme_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                          'Toplam Yıl' = pasif_donem_beklenen_omur,
                                                                          'Toplam Ay' = pasif_donem_beklenen_omur*12,
                                                                          Gelir = pasif_donem_geliri,
                                                                          'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                          'Kusur Oranı' = Kusur_Oranı,
                                                                          Toplam = bilinmeyen_pasif_donem_tazminat)




### 4.2.3 Bilinmeyen Dönem Toplam Tazminat Tablosu ----

sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu <- bind_rows(sirket_odeme_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                                   sirket_odeme_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)

sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu <- if(sirket_odeme_tarihi_yas > pasif_donem_yas) {
  sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
} else {
  sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu
}




bilinmeyen_donem_toplam_tazminat <- ifelse(sirket_odeme_tarihi_yas <= pasif_donem_yas, bilinmeyen_aktif_donem_tazminat + bilinmeyen_pasif_donem_tazminat, 
                                           bilinmeyen_pasif_donem_tazminat)



knitr::kable(sirket_odeme_tarihi_bilinmeyen_donem_tazminat_tablosu, caption = "Bilinmeyen Dönem Toplam Kazanç Tutarı")


### 4.2.4 Şirket Ödeme Tarihi Toplam Tazminat Tablosu ----

sirket_odeme_tarihi_toplam_tazminat_tablosu <- data.frame(Dönem = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam"),
                                                          Tazminat = c(Bilinen_Donem_Tazminatı,bilinmeyen_donem_toplam_tazminat,
                                                                       (Bilinen_Donem_Tazminatı + bilinmeyen_donem_toplam_tazminat))
                                                          )

knitr::kable( sirket_odeme_tarihi_toplam_tazminat_tablosu, caption = "Şirket Ödeme Tarihi itibari ile Toplam Tazminat")


### 4.2.5 Şirket Ödeme Tarihi Bakiye Tazminat Tablosu ----


sirket_odeme_tarihi_bakiye <- (Kısmi_Odeme_Tutarı_1 + Kısmi_Odeme_Tutarı_2) - (Bilinen_Donem_Tazminatı + bilinmeyen_donem_toplam_tazminat)


sirket_odeme_tarihi_bakiye_hesap_tablosu <- data.frame(Sonuc = c("Şirket Ödeme Tarihi İtibari ile Tazminat Tutarı", "Şirket Ödemesi",  "Bakiye"),
                                                       Tutar = c((Bilinen_Donem_Tazminatı + bilinmeyen_donem_toplam_tazminat),  
                                                                 (Kısmi_Odeme_Tutarı_1 + Kısmi_Odeme_Tutarı_2), 
                                                                 sirket_odeme_tarihi_bakiye)
                                                       )


# 5- HESAP TARİHİ İLE HESAPLAMA (AKTÜERYAL HESAPLAMA) ----

## 5.1 Hesap tarihi Bilinen Dönem Hesaplaması ----

### 5.1.1 Hesap tarihi Tam Maluliyet Donemi Tablosu ----


maluliyet_tarihi_baslangıc <- Kaza_Tarihi
maluliyet_tarihi_bitis <- (maluliyet_tarihi_baslangıc) + (Gecici_Maluliyet_sure * 30)

HT_Maluliyet_Donemi_tablosu <- Gelir_tablo %>% 
  filter(D_S >= maluliyet_tarihi_baslangıc & maluliyet_tarihi_bitis > D_B)

HT_Maluliyet_Donemi_tablosu$Donem_Baslangic[1] <- as.character(maluliyet_tarihi_baslangıc)
HT_Maluliyet_Donemi_tablosu$Donem_Son[nrow(HT_Maluliyet_Donemi_tablosu)] <- as.character(maluliyet_tarihi_bitis)

HT_Maluliyet_Donemi_tazminat_hesaplama <- HT_Maluliyet_Donemi_tablosu %>% 
  mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
         Donem_Sonu = as.Date(Donem_Son)) %>% 
  mutate(Kazanilan_Ay = signif(as.numeric(time_length(Donem_Sonu - Donem_Baslangici, "day")/30) , digits = 2)) %>%
  
  mutate(Kazanilan_Ay = case_when(
    Kazanilan_Ay >= 11.96 ~ round(Kazanilan_Ay, digits = 1),
    Kazanilan_Ay >= 5.9 ~ round(Kazanilan_Ay, digits = 0),
    Kazanilan_Ay >= 4.6 ~ plyr::round_any(Kazanilan_Ay, 0.01, f = ceiling),
    Kazanilan_Ay <= 2 ~ plyr::round_any(Kazanilan_Ay, 0.01, f = floor),
    TRUE ~ Kazanilan_Ay)) %>%
  
  mutate(Maluliyet_Oranı = 100) %>% 
  mutate(Kusur_Oranı = Kusur_Oranı) %>% 
  select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
  rename("Gelir" = Gelir_Durumu) %>% 
  mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
         Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>%
  mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 

HT_Tam_Maluliyet_Donemi_Tablosu <- HT_Maluliyet_Donemi_tazminat_hesaplama  %>% 
  select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)

HT_Tam_Maluliyet_Donemi_Tablosu

# HT_gecici_maluliyet_tazminat <- sum(HT_Tam_Maluliyet_Donemi_Tablosu$Donem_Tazminat)

### 5.1.2 Hesap tarihi Tam Maluliyet Sonrası Dönem Tablosu ----


maluliyet_sonrası_donem_baslangıc <- maluliyet_tarihi_bitis + 1
maluliyet_sonrası_donem_baslangıc_chr <- as.character(maluliyet_sonrası_donem_baslangıc)


HT_Maluliyet_Sonrasi_Donem_tablosu <- Gelir_tablo %>% 
  filter(D_S >= maluliyet_sonrası_donem_baslangıc & D_S <= Hesap_Tarihi + 360)

hesaplama_tarihi_chr <- as.character(Hesap_Tarihi)

HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Baslangic[1] <- maluliyet_sonrası_donem_baslangıc_chr
HT_Maluliyet_Sonrasi_Donem_tablosu$Donem_Son[nrow(HT_Maluliyet_Sonrasi_Donem_tablosu)] <- hesaplama_tarihi_chr


HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama <- HT_Maluliyet_Sonrasi_Donem_tablosu %>% 
  mutate(Donem_Baslangici = as.Date(Donem_Baslangic),
         Donem_Sonu = as.Date(Donem_Son)) %>%
  mutate(Kazanilan_Ay = signif(as.numeric(time_length(Donem_Sonu - Donem_Baslangici, "month") , digits = 2))) %>% 
  
  mutate(Kazanilan_Ay = case_when(
    Kazanilan_Ay >= 11.96 ~ round(Kazanilan_Ay, digits = 1),
    Kazanilan_Ay >= 11.7 ~  plyr::round_any(Kazanilan_Ay, 0.1, f = ceiling),
    Kazanilan_Ay >= 6 ~  plyr::round_any(Kazanilan_Ay, 1, f = floor),
    Kazanilan_Ay >= 5.9 ~  plyr::round_any(Kazanilan_Ay, 1, f = ceiling),
    Kazanilan_Ay >= 4 ~ plyr::round_any(Kazanilan_Ay, 0.1, f = ceiling),
    Kazanilan_Ay <= 2 ~ plyr::round_any(Kazanilan_Ay, 0.1, f = floor),
    TRUE ~ Kazanilan_Ay)) %>% 
  
  mutate(Maluliyet_Oranı = Maluliyet_Oranı) %>% 
  mutate(Kusur_Oranı = Kusur_Oranı) %>% 
  select(Gelir_Durumu, Kazanilan_Ay, Donem_Baslangic, Donem_Son, Maluliyet_Oranı, Kusur_Oranı) %>% 
  rename("Gelir" = Gelir_Durumu) %>% 
  mutate(Donem_Baslangic = format(as.Date(Donem_Baslangic), "%d/%m/%Y"),
         Donem_Son = format(as.Date(Donem_Son), "%d/%m/%Y")) %>%
  mutate(Donem_Tazminat = round(Kazanilan_Ay * Gelir * Maluliyet_Oranı/100 * Kusur_Oranı/100, digits = 2)) 


HT_Maluliyet_Sonrasi_Donem_Tablosu <- HT_Maluliyet_Sonrasi_Donem_tazminat_hesaplama %>% 
  select(Donem_Baslangic, Donem_Son, Kazanilan_Ay, Gelir, Maluliyet_Oranı, Kusur_Oranı, Donem_Tazminat)

HT_Maluliyet_Sonrasi_Donem_Tablosu

# HT_surekli_maluliyet_tazminat <- sum(HT_Maluliyet_Sonrasi_Donem_Tablosu$Donem_Tazminat)

### 5.1.3 Hesap Tarihi Bilinen Dönem Tablosu ve Tazminatı ----

HT_Bilinen_Donem_Tablosu <- bind_rows(HT_Tam_Maluliyet_Donemi_Tablosu, HT_Maluliyet_Sonrasi_Donem_Tablosu)

HT_Bilinen_Donem_Tazminatı = sum(HT_Bilinen_Donem_Tablosu$Donem_Tazminat)


# signif(as.numeric(time_length(as.Date("2022-07-01") - as.Date("2022-12-31"), "month") , digits = 2))


## 5.2 Hesap Tarihi Bilinmeyen Dönem Hesaplaması ----

### 5.2.1 Hesap Tarihi Aktif Donem ----          

ht_bilinen_son_donem_gelir <- HT_Bilinen_Donem_Tablosu$Gelir[nrow(HT_Bilinen_Donem_Tablosu)]

ht_bilinmeyen_aktif_donem_tazminat <- (pasif_donem_yas-hesap_tarihi_yas) * ht_bilinen_son_donem_gelir * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100
ht_bilinmeyen_aktif_donem_tazminat <- round(ht_bilinmeyen_aktif_donem_tazminat,digits = 2)

hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu <- data.frame(Dönem = "Aktif",
                                                                   'Toplam Yıl' = (pasif_donem_yas-hesap_tarihi_yas),
                                                                   'Toplam Ay' = (pasif_donem_yas-hesap_tarihi_yas)*12,
                                                                   Gelir = ht_bilinen_son_donem_gelir,
                                                                   'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                   'Kusur Oranı' = Kusur_Oranı,
                                                                   Toplam = ht_bilinmeyen_aktif_donem_tazminat)


# format(son_donem_gelir, big.mark = ",", scientific = FALSE) 


### 5.2.2 Hesap Tarihi Pasif Dönem ----

Asgari_Tablo_pasif_donem_ht <- Gelir_tablo %>% filter (D_S >= Hesap_Tarihi & Hesap_Tarihi > D_B)
ht_pasif_donem_geliri <- Asgari_Tablo_pasif_donem_ht$Bekar

ht_pasif_donem_beklenen_omur <- 
  
  if (hesap_tarihi_yas <= pasif_donem_yas) {
    
    (hesap_tarihi_yas + hesaplama_tarihi_beklenen_omur - pasif_donem_yas) 
    
  } else {
    
    hesaplama_tarihi_beklenen_omur
  }




ht_bilinmeyen_pasif_donem_tazminat <- ht_pasif_donem_beklenen_omur * ht_pasif_donem_geliri * 12 * Maluliyet_Oranı/100 * Kusur_Oranı/100


hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu <- data.frame(Dönem = "Pasif",
                                                                   'Toplam Yıl' = ht_pasif_donem_beklenen_omur,
                                                                   'Toplam Ay' = ht_pasif_donem_beklenen_omur*12,
                                                                   Gelir = ht_pasif_donem_geliri,
                                                                   'Maluliyet Oranı' =  Maluliyet_Oranı,
                                                                   'Kusur Oranı' = Kusur_Oranı,
                                                                   Toplam = ht_bilinmeyen_pasif_donem_tazminat)



### 5.2.3 Hesap Tarihi Bilinmeyen Dönem Toplam Tazminat Tablosu ----

hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- bind_rows(hesap_tarihi_bilinmeyen_aktif_donem_tazminat_tablosu, 
                                                            hesap_tarihi_bilinmeyen_pasif_donem_tazminat_tablosu)

hesap_tarihi_bilinmeyen_donem_tazminat_tablosu <- if(hesap_tarihi_yas > pasif_donem_yas) {
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu %>% filter (Dönem == "Pasif")
} else {
  hesap_tarihi_bilinmeyen_donem_tazminat_tablosu
}



ht_bilinmeyen_donem_toplam_tazminat <- ifelse(hesap_tarihi_yas <= pasif_donem_yas, ht_bilinmeyen_aktif_donem_tazminat + ht_bilinmeyen_pasif_donem_tazminat, 
                                              ht_bilinmeyen_pasif_donem_tazminat)
HT_Bilinmeyen_Donem_Tazminatı <- ht_bilinmeyen_donem_toplam_tazminat


knitr::kable(hesap_tarihi_bilinmeyen_donem_tazminat_tablosu, caption = "Bilinmeyen Dönem Toplam Kazanç Tutarı")


## 5.3 Toplam Tazminat ----

hesap_tarihi_toplam_tazminat_tablosu <- data.frame(Dönem = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam"),
                                                   Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, (HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı)))

knitr::kable(hesap_tarihi_toplam_tazminat_tablosu, caption = "Toplam Tazminat Tablosu")

Toplam_Tazminat <- HT_Bilinen_Donem_Tazminatı + HT_Bilinmeyen_Donem_Tazminatı


## 5.4 Şirket Ödemesinin Hesap Tarihli Değeri ----

### ŞİRKET ÖDEMELERİ 

odeme1_gecen_gun <- as.numeric(Hesap_Tarihi - parametre_tablosu2$`Ödeme Tarihi-1`)
odeme2_gecen_gun <- as.numeric(Hesap_Tarihi - parametre_tablosu2$`Ödeme Tarihi-2`)


odeme_hesaplama_degeri_1 <- if (Kısmi_Odeme_Tutarı_1 == "none") {0} else {Kısmi_Odeme_Tutarı_1 * (1 + (faiz_oranı/100) / 360 * odeme1_gecen_gun) }


odeme_hesaplama_degeri_2 <- if (Kısmi_Odeme_Tutarı_2 == "none") {0} else {Kısmi_Odeme_Tutarı_2 * (1 + (faiz_oranı/100) / 360 * odeme2_gecen_gun) }




Sirket_Odeme_Tablosu_1 <- tibble(
  "Ödeme Tarihi" = Kısmi_Odeme_Tarihi_1,
  "Hesaplama Tarihi" = as.character(format(ymd(Hesap_Tarihi),"%d/%m/%Y")),
  "Geçen Gün Sayısı" = odeme1_gecen_gun,
  "Ödenen Tutar" = if (Kısmi_Odeme_Tutarı_1 == "none") {0} else {Kısmi_Odeme_Tutarı_1},
  "Faiz Oranı" = scales::percent(faiz_oranı, scale = 1),
  "Hesap Tarihi Tutarı" = odeme_hesaplama_degeri_1
)

Sirket_Odeme_Tablosu_2 <- tibble(
  "Ödeme Tarihi" = Kısmi_Odeme_Tarihi_2,
  "Hesaplama Tarihi" = as.character(format(ymd(Hesap_Tarihi),"%d/%m/%Y")),
  "Geçen Gün Sayısı" = odeme2_gecen_gun,
  "Ödenen Tutar" = if (Kısmi_Odeme_Tutarı_2 == 0) {0} else {Kısmi_Odeme_Tutarı_2},
  "Faiz Oranı" = scales::percent(faiz_oranı, scale = 1),
  "Hesap Tarihi Tutarı" = odeme_hesaplama_degeri_2
)




Sirket_Odeme_Tablosu <- bind_rows(Sirket_Odeme_Tablosu_1, Sirket_Odeme_Tablosu_2) %>% 
  filter(`Ödenen Tutar` != 0)

if (Kısmi_Odeme_Sayısı >= 1) {
  knitr::kable(Sirket_Odeme_Tablosu, caption = "Şirket Ödeme Tablosu") } else {str_glue("Dosya kapsamında şirket ödemesi görülmemiştir.")}


toplam_kismi_odeme <- round(odeme_hesaplama_degeri_1 + odeme_hesaplama_degeri_2, digits = 2)

toplam_kismi_odeme <- formattable::currency(toplam_kismi_odeme, digits = 2, format = "f", big.mark = ".",decimal.mark = ",",  symbol = "")


if (Kısmi_Odeme_Sayısı >=1) {
  str_glue("Toplam Kısmi Ödeme: {toplam_kismi_odeme} TL") } else {str_glue(" ")}



# 6- BAKICI GİDERİ HESAPLAMASI ----


Bakici_Gideri_Baslangic_Tarihi <- Kaza_Tarihi
Bakici_Gideri_Bitis_Tarihi <- Kaza_Tarihi + Bakıcı_gideri_suresi

Bakici_Gideri_Donemi_filtered <- Gelir_tablo %>% 
  filter(D_S >= Bakici_Gideri_Baslangic_Tarihi & Bakici_Gideri_Bitis_Tarihi > D_B)

bakici_gideri_baslangic_tarihi_chr <- as.character(format(as.Date(Bakici_Gideri_Baslangic_Tarihi), "%d/%m/%Y"))
bakici_gideri_bitis_tarihi_chr <- as.character(format(as.Date(Bakici_Gideri_Bitis_Tarihi), "%d/%m/%Y"))

bakici_gideri_baslangic_tarihi_chr <- as.character(Bakici_Gideri_Baslangic_Tarihi)
bakici_gideri_bitis_tarihi_chr <- as.character(Bakici_Gideri_Bitis_Tarihi)


Bakici_Gideri_Donemi_filtered$Donem_Baslangic[1] <- bakici_gideri_baslangic_tarihi_chr
Bakici_Gideri_Donemi_filtered$Donem_Son[nrow(Bakici_Gideri_Donemi_filtered)] <- bakici_gideri_bitis_tarihi_chr


Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu <- Bakici_Gideri_Donemi_filtered %>% 
  mutate(Donem_Bas = as.Date(Donem_Baslangic),
         Donem_Sonu = as.Date(Donem_Son)) %>% 
  mutate(Kazanilan_Ay = signif(as.numeric(time_length(as.Date(Donem_Sonu) - as.Date(Donem_Bas), "day")/30) , digits = 2)) %>%
  mutate(Kazanilan_Ay = ifelse (Kazanilan_Ay >= 11.96 , round(Kazanilan_Ay, digits = 2), Kazanilan_Ay)) %>% 
  mutate(Kusur_Oranı = Kusur_Oranı) %>% 
  select(Donem_Bas, Donem_Son, Kazanilan_Ay, Brut, Kusur_Oranı) %>% 
  rename("Aylık_Bakıcı_Gideri" = Brut) %>% 
  mutate(Donem_Bakıcı_Gideri = round(Kazanilan_Ay * Aylık_Bakıcı_Gideri * Kusur_Oranı/100, digits = 2)) 
# mutate(Donem_Bakıcı_Gideri = round(Kazanilan_Ay * Aylık_Bakıcı_Gideri, digits = 2)) 


if (Bakıcı_gideri == "Var") {
  knitr::kable(Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu , caption = "Bakıcı Gideri Tablosu") } else {str_glue("Dosya kapsamında bakıcı gideri bulunmamaktadır.")}

Toplam_Bakici_gideri_hesaplama = sum(Bakici_Gideri_Donemi_kazanc_hesaplama_tablosu$Donem_Bakıcı_Gideri)


Toplam_Bakici_Gideri <- if (Bakıcı_gideri == "Var") { Toplam_Bakici_gideri_hesaplama } else { 0 }

if (Bakıcı_gideri == "Var") {
  str_glue("Toplam Bakıcı Gideri: {Toplam_Bakici_Gideri} TL") } else {str_glue(" ")} 


# 7- BAKİYE TAZMİNAT ve SON TABLO----

# bakiye_tazminat <- if(Toplam_Tazminat >= kaza_tarihi_teminat_limiti) {
#   kaza_tarihi_teminat_limiti
# } else {
#   Toplam_Tazminat - (Kısmi_Odeme_Tutarı_1 + Kısmi_Odeme_Tutarı_2)
# }


sirket_odeme_hesaplama <- if(Toplam_Tazminat >= kaza_tarihi_teminat_limiti) {
  Kısmi_Odeme_Tutarı_1 + Kısmi_Odeme_Tutarı_2
} else {
  toplam_kismi_odeme
}

bakiye_tazminat <- if(Toplam_Tazminat >= kaza_tarihi_teminat_limiti) {
  kaza_tarihi_teminat_limiti - sirket_odeme_hesaplama
} else {
  Toplam_Tazminat - sirket_odeme_hesaplama
}


# bakiye_tazminat <- round(bakiye_tazminat,digits = 2)


son_hesaplama_tablosu <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat", 
                                                 "Kısmi Ödeme", "Bakiye Tazminat Tutarı", "Bakıcı Gideri"),
                                    Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, Toplam_Tazminat, 
                                              (Kısmi_Odeme_Tutarı_1 + Kısmi_Odeme_Tutarı_2), bakiye_tazminat, Toplam_Bakici_Gideri)
)

knitr::kable(son_hesaplama_tablosu , caption = "Bakiye Tazminat Tablosu")


# }


##################################################################


gecici_son_hesaplama_tablosu <- data.frame(Tazminat = "Geçici İş Göremezlik Tazminatı",
                                           Tutar = HT_gecici_maluliyet_tazminat)


knitr::kable(gecici_son_hesaplama_tablosu , caption = "Geçici İş Göremezlik Tazminat Tablosu")


son_hesaplama_tablosu <- data.frame(Tazminat = c("Bilinen Dönem Tazminatı", "Bilinmeyen Dönem Tazminatı", "Toplam Tazminat",
                                                 "Kısmi Ödeme", "Bakiye Tazminat Tutarı"),
                                    Tutar = c(HT_Bilinen_Donem_Tazminatı, HT_Bilinmeyen_Donem_Tazminatı, Toplam_Tazminat,
                                              sirket_odeme_hesaplama, bakiye_tazminat)
)

knitr::kable(son_hesaplama_tablosu , caption = "Bakiye Tazminat Tablosu")


bakici_son_hesaplama_tablosu <- data.frame(Tazminat = "Bakıcı Gideri",
                                           Tutar =  Toplam_Bakici_Gideri)

knitr::kable(bakici_son_hesaplama_tablosu , caption = "Bakiye Tazminat Tablosu")



##################################################################

# bilirkisi_hesaplama_fonksiyonu()

Gecici_Maluliyet_surexx <- 3
Kaza_Tarihixx <- as.Date("2017-10-09")

maluliyet_tarihi_baslangıcxx <- as.Date(Kaza_Tarihixx)
maluliyet_tarihi_baslangıcxx 

maluliyet_tarihi_bitisxx <- (maluliyet_tarihi_baslangıcxx) + (Gecici_Maluliyet_surexx * 30)

maluliyet_tarihi_bitisxx



