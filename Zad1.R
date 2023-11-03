# a <- 11 / log(11)
# b <- 2 * a

# mx <- max(a, b)
# print(mx)

# #print(?tan)

# a <- c(0 : 50)

# sum(a)
# apropos("sum")

# setwd("dir")
# a <- "smartfon Huawei"
# save(a, file = "aa.RData")
# rm(a)
# print(exists("a"))
# load("aa.RData")
# print(a)
# setwd("../")

# library(grid)
# library(gridExtra)

# titanic <- read.csv("titanic.csv")
# tg <- tableGrob(titanic)
# grid.newpage()
# grid.draw(tg[1:10])

# w <- seq(300, 220, by = -8)
# a <- c(60:30)
# b <- c(30:50)
# d <- c(b, a)
# print(d)

nazwa <- c("Huawei P60 Pro 8",
           "Huawei nova Y61 4",
           "Huawei Nova 11i 8",
           "Huawei Nova 9 SE 8",
           "Huawei Nova 9 8",
           "Huawei Nova Y91 6",
           "Huawei Nova 11 Pro 8",
           "Huawei nova 10",
           "Huawei Nova 11 8",
           "Huawei Nova Y70 4")
wyswietlacz <- c(6.67, 6.52, 6.8, 6.78, 6.57, 6.95, 6.78, 6.67, 6.7, 6.75)
pamiec_ram <- c(8, 4, 8, 8, 8, 8, 8, 8, 8, 4)
pamiec_wbudowana <- c(256, 64, 128, 128, 128, 128, 256, 128, 256, 128)

# aparat_foto <- list(c(48, 48, 13),
#                     c(50, 2, 2),
#                     c(48, 2, 16),
#                     c(108, 8, 2, 2),
#                     c(50, 8, 2, 2),
#                     c(50, 2),
#                     c(50, 8),
#                     c(50, 8, 2),
#                     c(50, 8),
#                     c(48, 2, 5))

aparat_foto <- c(48, 50, 48, 108, 50, 50, 50, 50, 50, 48)
cena <- c(5499, 699, 1099, 1299, 1499, 1499, 2849, 1799, 2599, 839)
liczba_opini <- c(12, 2, 6, 7, 11, 8, 2, 6, NaN, 12)
smartfony <- data.frame(Nazwa = nazwa,
                        Wyswietlacz = wyswietlacz,
                        Pamiec_RAM = pamiec_ram,
                        Pamiec_wbudowana = pamiec_wbudowana,
                        Aparat_foto = aparat_foto,
                        Cena = cena,
                        Liczba_opini = liczba_opini)

srednia_cena <- mean(smartfony$Cena)
print(srednia_cena)

nowy_tel <- data.frame(Nazwa = "Huawei P50 Pro 8",
                       Wyswietlacz = 6.6,
                       Pamiec_RAM = 8,
                       Pamiec_wbudowana = 256,
                       Aparat_foto = 50,
                       Cena = 4499,
                       Liczba_opini = 8)
smartfony <- rbind(smartfony, nowy_tel)
print(mean(smartfony$Cena))

smartfony$Ocena_klientow <- as.factor(c(4.5, 4, 5, 5, 5, 5, 5, 5, NaN, 4.5, 4.5))
View(smartfony)

cena_na_ocene <- aggregate(Cena ~ Ocena_klientow, data = smartfony, FUN = mean)
print(cena_na_ocene)

nowe_nazwy <- c("Huawei Y5p",
                "Huawei Y6p",
                "Huawei P40 Lite E 4",
                "Huawei P smart 2021 4")
nowe_wysw <- c(5.45, 6.3, 6.39, 6.67)
nowe_ram <- c(2, 3, 4, 4)
nowe_wbud <- c(32, 64, 64, 128)

# nowe_apar <- list(c(8),
#                   c(13, 2, 5),
#                   c(48, 8, 2),
#                   c(48, 8, 2, 2))

nowe_apar <- c(8, 13, 48, 48)
nowe_cena <- c(299, 299, 499, 599)
nowe_l_op <- c(25, 24, 45, 28)
nowe_ocena <- c(5, 5, 4.5, 5)
nowe_tel <- data.frame(Nazwa = nowe_nazwy,
                       Wyswietlacz = nowe_wysw,
                       Pamiec_RAM = nowe_ram,
                       Pamiec_wbudowana = nowe_wbud,
                       Aparat_foto = I(nowe_apar),
                       Cena = nowe_cena,
                       Liczba_opini = nowe_l_op,
                       Ocena_klientow = nowe_ocena)

smartfony <- rbind(smartfony, nowe_tel)
View(smartfony)

library(plotrix)

helper <- as.numeric(as.character(smartfony$Ocena_klientow))
barplot(table(helper, useNA = "ifany"), ylim = c(0, 10))
legend("top", legend = "Stosunek liczby telefonów do ocen")

oceny <- table(smartfony$Ocena_klientow)
procent <- prop.table(oceny) * 100

pie3D(procent, labels = paste(names(procent), "(", round(procent, 2), "%)"),
      main = "Procentowy udział ocen klientów",
      explode = 0.1, col = rainbow(length(procent)))

fan.plot(procent, labels = paste(names(procent), "(", round(procent, 2), "%)"),
         main = "Procentowy udział ocen klientów",
         col = rainbow(length(procent)))

smartfony$Status_opinii <- cut(smartfony$Liczba_opini,
                               breaks = c(-Inf, 0, 50, 100, Inf),
                               labels = c("nie ma", "mniej 50 opinii",
                                          "50-100 opinii",
                                          "więcej 100 opinii"),
                               right = FALSE)

smartfony$Status_opinii <- as.factor(smartfony$Status_opinii)

status_opinii_count <- table(smartfony$Status_opinii)
status_opinii_procent <- prop.table(status_opinii_count) * 100

pie(status_opinii_procent, labels = paste(names(status_opinii_procent),
  "(", round(status_opinii_procent, 2), "%)"), main = "Procentowy udział statusu opinii")


for (i in 1:nrow(smartfony)) {
  zdanie <- paste(smartfony$Nazwa[i], " ma ocenę klientów ",
    smartfony$Ocena_klientow[i], " bo ma liczbę opinii ",
    smartfony$Liczba_opini[i], ".", sep = "")
  print(zdanie)
}

write.csv(smartfony, "smartfony_data.csv", row.names = FALSE)
smartfony_from_csv <- read.csv("smartfony_data.csv")
View(smartfony_from_csv)