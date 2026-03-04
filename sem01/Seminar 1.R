###############################################################################
# SEMINAR 1 SERII DE TIMP – ANALIZĂ ȘI PROGNOZĂ ÎN R
# Asist. univ. drd. Eduard Manta
#
# STRUCTURA SEMINARULUI:
# 1. Import date și obiecte de serie de timp
# 2. Vizualizare și explorare (trend, sezonalitate, volatilitate)
# 3. Autocorelare și dependență temporală
# 4. Medii mobile și trend
# 5. Metode de netezire exponențială:
#    - SES
#    - Holt
#    - Holt–Winters
#    - ETS
###############################################################################

library(fpp3)     # pentru date + vizualizare
library(fpp2)
library(forecast) # pentru SES, Holt, HW, ETS
library(readxl)

# DEFINIȚIE (teorie):
# O serie de timp este o succesiune de observații ordonate cronologic,
# măsurate la intervale regulate de timp (anual, trimestrial, lunar etc.).

###############################################################################
# 1. IMPORT DATE EXTERNE ȘI CONSTRUIREA UNEI SERII DE TIMP
###############################################################################

# TEORIE:
# O serie de timp trebuie să fie:
# - ordonată cronologic
# - observată la intervale regulate (anual, lunar etc.)

# În analiza de serii de timp, primul pas este importul datelor în format "tidy"
# (coloană de timp + coloană de valoare). Excel poate veni cu:
# - o singură coloană numerică (doar valori)
# - sau două coloane (Data, Valoare)
#
# Pentru modelare modernă (fpp3) e ideal să avem un tsibble cu index de timp.

z <- read_excel("C:/Users/40726/Desktop/Serii de timp 2022/Seminar 1/z.xlsx")
z

# Dacă z este un data.frame cu mai multe coloane, ts(z, ...) va crea o serie MULTIVARIATĂ
# (matrix ts). Dacă e o singură coloană, va fi univariate.
#
# Atenție: start=2000 și frequency=12 => interpretăm datele fiind LUNARE,
y_z <- ts(z, start = c(2000, 1), frequency = 12)
y_z

# frequency = 12 => 12 observații pe an (serie lunară)
# start = c(an, subperioadă) unde:
#   subperioadă = luna (1 = ianuarie, 12 = decembrie)
y_ts_month <- ts(
  c(123, 39, 78, 52, 110),
  start = c(2012, 1),  # 2012, luna 1 (ianuarie)
  frequency = 12
)

# avem doar 5 observații => seria acoperă 5 luni (ian–mai 2012)
y_ts_month

# frequency = 52 este o aproximare uzuală pentru date săptămânale
# start = c(an, săptămână)
y_ts_week <- ts(
  c(123, 39, 78, 52, 110),
  start = c(2003, 51),  # anul 2003, săptămâna 51
  frequency = 52
)

# Interpretare:
# prima observație = săpt. 51 / 2003
# următoarele = săptămânile următoare
y_ts_week


###############################################################################
# 2. VIZUALIZARE SI EXPLORARE A UNEI SERII DE TIMP (TREND, SEZONALITATE)
###############################################################################

# TEORIE:
# Graficul seriei de timp reprezintă prima etapă de analiză exploratorie
# și are rol diagnostic, permițând identificarea vizuală a principalelor
# componente și caracteristici ale procesului generator de date.
# Prin inspectarea graficului unei serii de timp putem evalua:
# - existența unui TREND: o mișcare sistematică de creștere sau scădere
#   a nivelului mediu al seriei pe termen lung;
# - existența SEZONALITĂȚII: variații regulate și repetitive la intervale
#   fixe de timp (anuale, lunare, săptămânale), determinate de factori
#   calendaristici sau comportamentali;
# - prezența OUTLIERILOR sau a ȘOCURILOR: observații atipice generate de
#   evenimente excepționale (crize, politici economice, conflicte, dezastre);
# - posibile SCHIMBĂRI DE REGIM sau rupturi structurale, caracterizate prin
#   modificări persistente ale nivelului, variabilității sau trendului seriei.
#
# Această analiză preliminară ghidează alegerea ulterioară a modelului
# (SES, Holt, Holt-Winters, ARIMA, ETS etc.) și a transformărilor necesare
# (diferențiere, logaritmare, detrending).
tail(ansett)

melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%   # selectăm ruta și clasa
  mutate(Passengers = Passengers / 1000)                  # scalare pentru lizibilitate

melsyd_economy

autoplot(melsyd_economy, Passengers) +
  labs(
    title = "Pasageri – clasa Economy",
    subtitle = "Ruta Melbourne–Sydney",
    y = "Pasageri ('000)"
  ) +
  theme_bw()

# INTERPRETARE:
# - șoc în 1989 (dispută industrială)
# - schimbare în 1992 (politici de preț)
# - creștere în 1991
# - sezonalitate: vârfuri la început de an (sărbători)


# TEORIE:
# autoplot pe un ts sau tsibble produce un grafic "time plot" standard.
autoplot(a10) +
  labs(y = "$ (milioane)", title = "Vânzările de înghețată") +
  theme_bw()
# INTERPRETARE:
# Trend clar crescător pe termen lung (creșterea nivelului mediu al vânzărilor).
# Sezonalitate anuală foarte puternică, cu vârfuri recurente (vara) și minime (iarna).
# Amplitudinea sezonalității crește în timp, sugerând un comportament multiplicativ.
# Serie nestationară (trend + sezonalitate)


# EXERCITII DE VIZUALIZARE
autoplot(hsales) +
  ggtitle("Sales of new one-family houses, USA") +
  xlab("Year") + ylab("Total sales") +
  theme_bw()
# INTERPRETARE:
# Nu există un trend clar și stabil pe termen lung.
# Fluctuații ciclice (posibile cicluri economice), dar fără sezonalitate evidentă.
# Volatilitate ridicată, cu episoade de creșteri și scăderi abrupte.
# Posibile schimbări de regim (boom / bust pe piața imobiliară).

autoplot(ustreas) +
  ggtitle("US Treasury Bill Contracts") +
  xlab("Day") + ylab("Price") +
  theme_bw()
# INTERPRETARE:
# Trend descrescător relativ lin.
# Fără sezonalitate evidentă.
# Variabilitate relativ mică => serie mai 'netedă'.
# Comportament tipic de serie financiară pe termen scurt.

autoplot(window(elec, start = 1980)) +
  ggtitle("Australian electricity production") +
  xlab("Year") + ylab("GWh") +
  theme_bw()
# INTERPRETARE:
# Trend crescător pronunțat.
# Sezonalitate anuală clară, bine definită.
# Sezonalitatea pare aproximativ aditivă (amplitudinea relativ constantă).
# Serie clar nestationară.

# TEORIE:
# diff() calculează diferențe de ordin 1: y_t - y_{t-1}
# - util pentru stabilizarea trendului (aproximativ) și analiza schimbărilor zilnice
autoplot(diff(goog)) +
  ggtitle("Daily changes in Google closing stock price") +
  ylab("$US") + xlab("Date") +
  theme_bw()
# INTERPRETARE:
# Nu există trend (valorile oscilează în jurul lui 0).
# Nu există sezonalitate.
# Volatilitate ridicată, cu spike-uri izolate (șocuri).
# Serie apropiată de zgomot alb, dar cu heteroscedasticitate (volatilitate variabilă).


# TEORIE:
# 1) ggseasonplot: arată fiecare an suprapus, ca să vedem pattern-ul sezonier.
# 2) polar=TRUE: același grafic, dar circular (util pentru sezonalitate anuală).
# 3) ggsubseriesplot: arată fiecare sub-perioadă (ex. lună) separat,
#    evidențiind medii și variabilitate.
ggseasonplot(a10, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: ice cream sales")
# INTERPRETARE:
# Fiecare linie reprezintă un an, iar pe axa X sunt lunile.
# Permite compararea aceluiași sezon (ex. ianuarie, iulie) între ani diferiți.
# Sezonalitate anuală foarte puternică, comună tuturor anilor.
# Minime recurente în lunile de iarnă (ianuarie–februarie).
# Vârfuri în lunile de vară (iunie–august).
# Nivelul general al vânzărilor crește de la un an la altul, indicând un trend crescător.
# Amplitudinea variațiilor sezoniere crește odată cu nivelul seriei, sugerând sezonalitate multiplicativă

ggseasonplot(a10, polar = TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: ice cream sales")
# INTERPRETARE:
# Aceleași informații ca seasonal plot, dar în format circular.
# Ajută la vizualizarea simetriei și intensității sezonalității.
# Forma spiralată indică creșterea nivelului seriei în timp.
# Distanța radială mai mare vara confirmă vârfurile sezoniere.
# Sezonalitatea este consistentă ca formă, dar cu amplitudine tot mai mare.
# Confirmă clar un comportament multiplicativ al sezonalității.

ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: ice cream sales") +
  theme_bw()
# INTERPRETARE:
# Fiecare panou reprezintă o lună.
# Linia orizontală albastră indică media lunară pe toată perioada.
# Se observă clar diferențe sistematice între luni:
#    valori medii mici în iarnă,
#    valori medii mari vara.
# Pentru fiecare lună există un trend crescător în timp, indicând că:
#    nu doar sezonul contează,
#    ci și creșterea nivelului general al seriei.
# Variabilitatea diferă între luni (mai mare vara).

# TEORIE:
# gg_season() e din feasts și lucrează natural cu tsibble.
# period = "day"/"week"/"year" controlează periodicitatea sezonalității.
vic_elec %>%
  gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand (Victoria): daily seasonality")
# INTERPRETARE:
# Se observă un pattern zilnic foarte clar al cererii.
# Minim nocturn (în jurul orelor 03:00–05:00), când activitatea economică și consumul casnic sunt reduse.
# Creștere rapidă dimineața, urmată de un vârf în a doua parte a zilei / seara (consum rezidențial + comercial).
# Forma este consistentă între zile, dar nivelul și amplitudinea diferă, sugerând influența altor factori (temperatură, zi lucrătoare vs weekend).

vic_elec %>%
  gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand (Victoria): weekly seasonality")
# INTERPRETARE:
# Se observă un pattern clar diferențiat pe zilele săptămânii.
# Cerere mai ridicată în zilele lucrătoare (luni–vineri).
# Reducere semnificativă în weekend, în special duminica.
# Structura zilnică (minim noaptea, maxim seara) se păstrează, dar amplitudinea este mai mică în weekend.

vic_elec %>%
  gg_season(Demand, period = "year") +
  labs(y = "MWh", title = "Electricity demand (Victoria): yearly seasonality")
# INTERPRETARE:
# Se observă variații sistematice pe parcursul anului.
# Niveluri mai ridicate în lunile de vară (cerere crescută pentru climatizare).
# Niveluri mai scăzute în anotimpurile intermediare.
# Pattern-ul este relativ stabil între ani, dar cu episoade extreme (vârfuri izolate), probabil asociate cu valuri de căldură sau evenimente excepționale.

# TEORIE:
# Serii multivariate: analizăm simultan evoluția mai multor variabile în timp.
# facets=TRUE separă graficele pe panouri, dar păstrează axa timpului comună.
autoplot(elecdemand[, c("Demand", "Temperature")], facets = TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand & temperature: Victoria (2014)")
# INTERPRETARE:
# Cererea de energie și temperatura prezintă dinamici diferite, dar corelate.
# Cererea are variații frecvente și vârfuri pronunțate (sezonalitate intrazilnică).
# Temperatura evoluează mai lent, cu un pattern sezonier anual (mai scăzută iarna, mai ridicată vara).
# Se observă perioade în care vârfurile de cerere coincid cu temperaturi extreme, sugerând o relație cauzală (climatizare / încălzire).
# Indică faptul că temperatura este un determinant important al cererii, dar 
# relația nu este instantaneu liniară și este mediată de sezonalitate și tipul zilei.


# TEORIE:
# Scatterplot = relație contemporană (la același moment t).
# Nu surprinde lag-uri; pentru asta folosim lag plot / cross-correlation.
qplot(Temperature, Demand, data = as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")
# INTERPRETARE:
# Relație neliniară între temperatură și cerere.
# Cerere minimă la temperaturi moderate (≈ 15–20°C).
# Creștere accentuată a cererii la temperaturi ridicate (aer condiționat).
# Ușoară creștere și la temperaturi foarte scăzute (încălzire).
# Dispersia mare indică influența altor factori (oră, zi a săptămânii, sezonalitate).
# Relația nu poate fi modelată adecvat printr-o regresie liniară simplă.

# TEORIE:
# ggpairs produce o matrice de scatterplot-uri + corelații între variabile.
# Atenție: pe serii de timp, corelațiile pot fi "spurioase" dacă există trend.
# (De aceea, uneori se lucrează cu diferențe sau detrending.)
GGally::ggpairs(as.data.frame(visnights[, 1:5]))
# INTERPRETARE:
# Corelații puternice și semnificative între anumite regiuni (ex. NSWMetro – NSWNthCo, NSWNthCo – NSWSthCo), sugerând:
# integrare regională, factori comuni de cerere.
# Corelații slabe sau nesemnificative între alte regiuni (ex. zone urbane vs zone izolate).
# Distribuțiile marginale diferă, indicând structuri de cerere distincte între regiuni.
# Evidențiază o structură spațială heterogenă a cererii de energie


###############################################################################
# 3. AUTOCORELAREA SI DEPENDENTA TEMPORALA
###############################################################################

# TEORIE:
# lag plot compară y_t cu y_{t-k}. Dacă există autocorelare,
# apar forme/structuri (nu un nor aleator).

# Notă: window() selectează o subperioadă dintr-un ts.
# start=c(1992,3) la frequency=4 (trimestrial) înseamnă trimestrul 3 din 1992.
beer2 <- window(ausbeer, start = c(1992, 3))
autoplot(beer2)
gglagplot(beer2)
# INTERPRETARE:
# Fiecare panou compară y_t cu y_t-k (k=lag)
# Punctele colorate pe trimestre evidențiază structura sezonieră.
# La lag-uri mici apar forme alungite/structurate, nu nori aleatori => dependență temporală.
# La lag-uri sezoniere (multipli de 4 pentru serie trimestrială) apar clustere distincte => sezonalitate puternică.
# Modelul nu este zgomot alb; există autocorelare și sezonalitate.

# TEORIE:
# ACF(k) = corelația dintre y_t și y_{t-k}.
# - dacă scade încet: posibil trend / non-staționaritate
# - vârfuri la multipli de sezon: sezonalitate (ex. la lag 12 pentru lunar)
ggAcf(beer2) + theme_bw()
# INTERPRETARE:
# Vârfuri semnificative la lag 4, 8, 12, 16.
# Alternanță de semne între lag-uri apropiate.
# Sezonalitate trimestrială clară (periodicitate = 4).
# Autocorelarea nu dispare rapid => seria este nestationară sezonier.

# Exemplu cu sezonalitate clară:
aelec <- window(elec, start = 1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")

# lag=48 (de ex.) permite să vedem mai multe lag-uri, util când avem sezonalitate anuală + substructuri
ggAcf(aelec, lag = 48) + theme_bw()
# INTERPRETARE:
# Autocorelare foarte mare la lag-uri mici, care scade lent.
# Vârfuri regulate la lag-uri sezoniere (ex. 12, 24, 36).
# Majoritatea barelor sunt peste limitele de semnificație.
# Trend pronunțat (scădere lentă a ACF).
# Sezonalitate anuală persistentă.
# Seria este clar nestationară (trend + sezonalitate).

# TEORIE:
# White noise (zgomot alb) = serie cu:
# - medie ~ 0
# - varianță constantă
# - fără autocorelare (ACF ~ 0 pentru lag>0)
#
# În practică, reziduurile unui model bun ar trebui să se apropie de white noise.

set.seed(30)         # pentru reproducibilitate: aceeași serie la fiecare rulare
y_wn <- ts(rnorm(50)) # 50 observații N(0,1)

autoplot(y_wn) + ggtitle("White noise")
# INTERPRETARE:
# Valorile oscilează aleator în jurul unei medii constante (≈ 0).
# Nu se observă trend sau sezonalitate.
# Variabilitatea este relativ constantă în timp.
# Apar fluctuații mari izolate, dar fără persistență
# Sugerează un proces aleator, fără structură sistematică.
ggAcf(y_wn)
# INTERPRETARE:
# Majoritatea coeficienților de autocorelare sunt apropiați de zero.
# Barele se află în interiorul limitelor de semnificație statistică.
# Lipsa unui pattern sistematic în ACF.
# Confirmă că seria este zgomot alb.

###############################################################################
# 4. MEDII MOBILE SI TREND
###############################################################################
# Media mobilă este o tehnică de netezire (smoothing) utilizată în analiza
# seriilor de timp pentru a reduce fluctuațiile aleatoare pe termen scurt
# și pentru a evidenția componenta de trend a seriei.
#
# Ideea de bază:
# Fiecare valoare netezită este calculată ca media aritmetică a unui număr
# fix de observații consecutive din jurul unui moment de timp t.
#
# Formal, o medie mobilă de ordin k este definită ca:
#   MA_t = (1/k) * (y_{t-(k-1)/2} + ... + y_t + ... + y_{t+(k-1)/2})
# pentru k impar.
#
# În acest seminar folosim o medie mobilă pe 7 puncte (7-MA), care:
# - netezește variațiile de scurtă durată (zgomotul),
# - păstrează evoluția de fond a seriei (trendul).
#
# CENTRAREA:
# Pentru o fereastră impară (k = 7), media este centrată natural:
# - 3 observații înainte,
# - observația curentă,
# - 3 observații după.
# Astfel, valoarea MA_t este aliniată temporal cu momentul t.
#
# INTERPRETARE:
# - dacă seria originală oscilează în jurul MA, trendul este stabil;
# - deviațiile mari față de MA pot indica șocuri sau schimbări temporare;
# - o schimbare persistentă a poziției MA sugerează modificarea trendului.
#
# LIMITĂRI:
# - Media mobilă NU este un model de prognoză, ci o metodă descriptivă.
# - Reduce lungimea seriei (nu poate fi calculată la capete).
# - Întârzie detectarea schimbărilor bruște ale trendului.
#
# PARAMETRUL .complete = TRUE:
# Asigură că media este calculată doar atunci când sunt disponibile toate
# cele 7 observații necesare; valorile de la început și sfârșit sunt setate NA.
#
# CONCLUZIE:
# Media mobilă este un instrument explorator simplu, util pentru înțelegerea
# structurii seriei și pentru ghidarea alegerii unui model mai complex
# (Holt, Holt–Winters, ETS, ARIMA).
global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(
    `7-MA` = slider::slide_dbl(
      Exports, mean,
      .before = 3, .after = 3, .complete = TRUE
    )
  )

aus_exports %>%
  autoplot(Exports) +
  geom_line(aes(y = `7-MA`)) +
  labs(y = "% of GDP", title = "Total Australian exports (with 7-MA)")
# INTERPRETARE:   
# Seria originală prezintă fluctuații pe termen scurt, care îngreunează identificarea trendului.
# Media mobilă pe 7 perioade netezește aceste variații și evidențiază clar:
#    un trend crescător de lungă durată, mai accentuat după anii 1990;
#    episoade de încetinire temporară sau corecții, fără a altera tendința generală.
# Diferențele dintre seria brută și 7-MA indică șocuri tranzitorii sau volatilitate ciclică
# Media mobilă confirmă existența unui trend structural crescător al exporturilor Australiei raportate la PIB.

# TEORIE:
# Pe serii lunare, o medie mobilă de 7 e doar exemplu.
# Pentru trend anualizat, uneori se folosește 12-MA (lunar) sau 4-MA (trimestrial).
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade")

us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `7-MA` = slider::slide_dbl(
      Employed, mean,
      .before = 3, .after = 3, .complete = TRUE
    )
  )

us_retail_employment_ma %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `7-MA`)) +
  labs(y = "Persons (thousands)", title = "US retail employment (with 7-MA)")
# INTERPRETARE: 
# Seria lunară prezintă sezonalitate pronunțată și fluctuații de scurtă durată.
# 7-MA elimină mare parte din sezonalitatea de frecvență înaltă, permițând:
#    identificarea unui trend ascendent pe termen lung;
#    evidențierea rupturii structurale din perioada crizei financiare (scădere abruptă);
#    observarea fazei de recuperare post-criză.
# Trendul estimat prin 7-MA este mult mai ușor de interpretat decât seria originală.
# Media mobilă ajută la separarea trendului economic de fluctuațiile sezoniere.

# TEORIE (FOARTE IMPORTANT):
# O serie poate fi scrisă ca:
# - ADITIV:        y_t = Trend_t + Season_t + Error_t
#   -> sezonalitatea are amplitudine aproximativ constantă în timp
#
# - MULTIPLICATIV: y_t = Trend_t * Season_t * Error_t
#   -> sezonalitatea crește/scade proporțional cu nivelul seriei
#
# In limbaj non-academic:
# - dacă “valurile” sezoniere cresc când seria crește -> multiplicativ
# - dacă “valurile” au cam aceeași mărime mereu -> aditiv
autoplot(us_retail_employment)

# Descompunere aditivă
us_retail_employment %>%
  model(classical_decomposition(Employed, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition: US retail employment")
# INTERPRETARE:
# Seria observată (Employed): prezintă trend crescător pe termen lung, cu o scădere abruptă în jurul crizei financiare (ruptură structurală).
# Trend: surprinde clar evoluția de fond a ocupării, inclusiv declinul din 2008–2009 și recuperarea ulterioară.
# Sezonal: amplitudine aproape constantă în timp (aceleași variații lunare, în termeni absoluți).
# Rezidual (random): fluctuații relativ mici, centrate în jurul lui zero; apar șocuri temporare în perioadele de stres economic.

# Descompunere multiplicativă
us_retail_employment %>%
  model(classical_decomposition(Employed, type = "multiplicative")) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical multiplicative decomposition: US retail employment")
# INTERPRETARE:
# Seria observată: aceeași structură generală ca în cazul aditiv.
# Trend: foarte similar cu cel din modelul aditiv (captură robustă a evoluției de fond).
# Sezonal: exprimată proporțional (indice în jurul lui 1); variațiile sezoniere cresc/scad odată cu nivelul seriei.
# Rezidual: fluctuații relative mici (în jurul lui 1), mai omogene pe întreaga perioadă.


###############################################################################
# 5. METODE DE NETEZIRE EXPONENTIALA
###############################################################################
# TEORIE:
# SES presupune: y_t = level_t + error_t
# Actualizare (intuiție): level_t = alpha * y_t + (1-alpha) * level_{t-1}
# - alpha în (0,1): alpha mare => reacționează rapid la schimbări recente
# - alpha mic => serie netezită puternic (memorie lungă)

# Selectăm o subperioadă (window) pentru exemplu (date anuale, de regulă)
oildata <- window(oil, start = 1996)

autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year") +
  ggtitle("Oil production in Saudi Arabia (1996–2013)") +
  theme_bw()

# h = orizontul de prognoză (câte perioade în viitor prognozăm)
# Ex: h=5 => 5 ani înainte dacă seria e anuală
fc_ses_oil <- ses(oildata, h = 5)

# Rezumat: parametri estimați (alpha), erori, IC etc.
summary(fc_ses_oil)
# INTERPRETARE:
# alpha = 0.8339 
# α ∈ (0,1) controlează viteza de adaptare a modelului.
# Formula intuitivă: level_t=αy_t+(1−α)level_t−1
# Valoarea mare alpha acordă pondere foarte mare observațiilor recente;
# reacționează rapid la schimbări
# l = 446.5868
# Reprezintă nivelul inițial estimat al seriei
# Este valoarea de pornire pentru recursivitatea SES
# Nu are interpretare economică directă, dar este necesară tehnic
# sigma = deviația standard a erorilor reziduale
# Măsoară variabilitatea neexplicată de model
# Valoare relativ mare => seria are fluctuații importante care nu pot fi 
# captate de un model foarte simplu.
# Criteriile informationale AIC, AICc, BIC sunt folosite pentru comparația între modele
# Valorile nu se interpretează izolat, ci: mai mic = mai bun, între modele alternative aplicate aceleiași serii.

# fitted = valori estimate in-sample (prognoza pe o perioadă înainte, "one-step")
fc_ses_oil$model$fitted

# residuals = y_t - fitted_t (erorile in-sample)
fc_ses_oil$model$residuals

# Acuratețe in-sample: ME, RMSE, MAE, MAPE etc.
# (pentru comparații serioase, preferăm și evaluare out-of-sample / CV)
round(accuracy(fc_ses_oil), 2)
# Notație:
# y_t     = valoarea observată la momentul t
# y_hat_t = valoarea prognozată (fitted / forecast)
# e_t     = eroarea de prognoză = y_t - y_hat_t
# n       = numărul de observații

# 1) ME – Mean Error (eroare medie)
# Formula:
# ME = (1/n) * sum_{t=1}^n (e_t)
# Interpretare:
# - măsoară bias-ul prognozei
# - ME > 0  → subestimare
# - ME < 0  → supraestimare
# - ME ≈ 0  → prognoză ne-biasată

# 2) RMSE – Root Mean Squared Error
# Formula:
# RMSE = sqrt( (1/n) * sum_{t=1}^n (e_t^2) )
# Interpretare:
# - penalizează puternic erorile mari
# - sensibil la outlieri
# - are aceeași unitate ca seria originală

# 3) MAE – Mean Absolute Error
# Formula:
# MAE = (1/n) * sum_{t=1}^n |e_t|
# Interpretare:
# - mărimea medie a erorii
# - mai robust la outlieri decât RMSE
# - ușor de interpretat

# 4) MPE – Mean Percentage Error
# Formula:
# MPE = (100/n) * sum_{t=1}^n ( e_t / y_t )
# Interpretare:
# - bias relativ exprimat în procente
# - MPE > 0  → subestimare
# - MPE < 0  → supraestimare
# Limitare:
# - instabil dacă y_t este foarte mic sau zero

# 5) MAPE – Mean Absolute Percentage Error
# Formula:
# MAPE = (100/n) * sum_{t=1}^n | e_t / y_t |
# Interpretare:
# - eroare relativă medie (%)
# - MAPE < 10%  → prognoză foarte bună
# - 10–20%     → bună
# - 20–50%     → acceptabilă
# - >50%       → slabă
# Limitare:
# - nu este definit când y_t = 0

# 6) MASE – Mean Absolute Scaled Error
# Formula:
# MASE = 
#   [ (1/n) * sum_{t=1}^n |e_t| ] /
#   [ (1/(n-1)) * sum_{t=2}^n |y_t - y_{t-1}| ]
# Interpretare:
# - compară modelul cu metoda naivă: y_hat_t = y_{t-1}
# - MASE < 1  → model mai bun decât naivul
# - MASE > 1  → model mai slab decât naivul
# Avantaj:
# - comparabil între serii diferite
# - funcționează și când y_t = 0

# 7) ACF1 – Autocorrelation of residuals at lag 1
# Formula:
# ACF1 = Cor(e_t, e_{t-1})
# Interpretare:
# - măsoară dependența temporală a reziduurilor
# - ACF1 ≈ 0  → reziduuri ≈ zgomot alb
# - ACF1 ≠ 0  → structură neexplicată (model incomplet)

# Interpretare in modelul SES
# ME    = 6.4
# RMSE  = 28.12
# MAE   = 22.26
# MPE   = 1.1
# MAPE  = 4.61
# MASE  = 0.93
# ACF1  = -0.03
# Bias mic (ME ≈ 0, MPE ≈ 1%)
# Eroare relativă redusă (MAPE ≈ 4.6%)
# Mai bun decât modelul naiv (MASE < 1)
# Reziduuri necorelate (ACF1 ≈ 0)


# Grafic: prognoze + fitted (ca să vadă studenții diferența)
autoplot(fc_ses_oil) +
  autolayer(fitted(fc_ses_oil), series = "Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year") +
  ggtitle("SES: fitted + forecasts") +
  theme_bw()

# TEORIE:
# Holt adaugă un termen de trend:
# - level_t  (nivel)
# - trend_t  (panta)
# Parametri tipici:
# - alpha (nivel), beta (trend)
# Prognoza va fi o linie (nivel + trend), nu constantă.
air <- window(ausair, start = 1990)

autoplot(air) +
  ylab("Air passengers in Australia (millions)") + xlab("Year") +
  ggtitle("Annual air passengers in Australia (1990–2016)") +
  theme_bw()

fc_holt_trend <- holt(air, h = 5)

summary(fc_holt_trend)
# INTERPRETARE
# alpha = 0.8302  => netezirea NIVELULUI (level)
# beta  = 0.0001  => netezirea TRENDULUI (slope)
# - alpha mare => nivelul se adaptează rapid la observațiile recente.
# - beta foarte mic => trendul este actualizat foarte lent, deci modelul
#   consideră trendul aproape constant (fără accelerări/încetiniri puternice).
# Ecuațiile modelului Holt
# Eroare:        e_t = y_t - (l_{t-1} + b_{t-1})
# Actualizare nivel:
#   l_t = alpha * y_t + (1 - alpha) * (l_{t-1} + b_{t-1})
# Actualizare trend:
#   b_t = beta * (l_t - l_{t-1}) + (1 - beta) * b_{t-1}
# Prognoză h pași înainte:
#   yhat_{t+h|t} = l_t + h * b_t
# Stări inițiale (initial states)
# l = 15.5715  => nivel inițial estimat
# b = 2.1017   => trend inițial (aprox. creșterea per perioadă)
# - l este „punctul de plecare” al seriei.
# - b arată panta inițială (cât crește seria, în medie, pe o perioadă).
# sigma = 2.365
# - deviația standard a reziduurilor (mărimea tipică a erorilor).
# - sigma mai mic → potrivire mai bună (erori mai mici).
# ME    = 0.008  → bias aproape zero (prognoze ne-biasate)
# RMSE  = 2.182  → eroare medie penalizând puternic erorile mari
# MAE   = 1.529  → eroare medie absolută (ușor de interpretat)
# MPE   = -0.324 → ușoară supraestimare (%)
# MAPE  = 3.821  → eroare relativă mică (≈ 3.8%, prognoză foarte bună)
# MASE  = 0.666  → < 1 ⇒ mai bun decât metoda naivă (yhat_t = y_{t-1})
# ACF1  = -0.013 → autocorelare reziduală ~ 0 (reziduuri ~ white noise)

# fitted / residuals
fc_holt_trend$model$fitted
fc_holt_trend$model$residuals

# Acuratețe in-sample
round(accuracy(fc_holt_trend), 2)

# Grafic Holt: prognoză + fitted
autoplot(fc_holt_trend) +
  autolayer(fitted(fc_holt_trend), series = "Fitted") +
  ylab("Air passengers (millions)") + xlab("Year") +
  ggtitle("Holt: fitted + forecasts") +
  theme_bw()

# OUT-OF-SAMPLE PLOT (conceptual):
# - desenăm seria reală și suprapunem prognoza cu intervale de predicție (PI)
autoplot(air) +
  autolayer(fc_holt_trend, series = "Holt's method", PI = TRUE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_bw()


# TEORIE:
# Dacă seria are trend, SES va performa slab (prognoze plate).
# Holt ar trebui să fie mai bun fiindcă include trend.
fc_ses_air <- ses(air, h = 5)

autoplot(air) +
  autolayer(fc_holt_trend, series = "Holt's method", PI = FALSE) +
  autolayer(fc_ses_air, series = "Simple Exponential Smoothing", PI = FALSE) +
  ggtitle("Forecast comparison: Holt vs SES") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_bw()

# TEORIE:
# tsCV face "rolling-origin evaluation":
# - antrenează pe un prefix al seriei
# - prezice 1 pas înainte (h=1)
# - repetă pentru multe puncte din trecut
# Obținem erorile out-of-sample pentru comparații corecte.
#
# Important: tsCV returnează erori e_t = y_t - forecast_t (cu NA la început).
e1 <- tsCV(air, ses,  h = 1)
e2 <- tsCV(air, holt, h = 1)

# MSE și MAE (out-of-sample)
mean(e1^2, na.rm = TRUE)        # MSE SES
mean(e2^2, na.rm = TRUE)        # MSE Holt

mean(abs(e1), na.rm = TRUE)     # MAE SES
mean(abs(e2), na.rm = TRUE)     # MAE Holt

summary(fc_ses_air)
summary(fc_holt_trend)

# Interpretare:
# Modelul cu MSE/MAE mai mic e preferabil pentru această serie.
# (La air, de obicei Holt câștigă pentru că există trend.)

# Inspecție: structura modelului Holt estimat
fc_holt_trend[["model"]]

# Grafic final: prognoza Holt (modelul ales)
autoplot(fc_holt_trend) +
  xlab("Year") + ylab("Air passengers in Australia (millions)") +
  ggtitle("Forecasting air passengers: best non-seasonal method (Holt)") +
  theme_bw()

# TEORIE:
# HW adaugă sezonalitate:
# - Additive: amplitudinea sezonului e aproximativ constantă (ex. +/- 5 mereu)
# - Multiplicative: amplitudinea crește odată cu nivelul seriei
#
# Non-academic:
# - dacă "valurile" cresc când nivelul crește -> multiplicativ
# - dacă "valurile" sunt cam la fel -> aditiv
aust <- window(austourists, start = 2005)

autoplot(aust) +
  ggtitle("International visitor nights in Australia (from 2005)") +
  ylab("Visitor nights (millions)") + xlab("Year") +
  theme_bw()

fit_hw_add <- hw(aust, seasonal = "additive")
fit_hw_mul <- hw(aust, seasonal = "multiplicative")

autoplot(aust) +
  autolayer(fit_hw_add, series = "HW additive forecasts", PI = FALSE) +
  autolayer(fit_hw_mul, series = "HW multiplicative forecasts", PI = FALSE) +
  xlab("Year") + ylab("Visitor nights (millions)") +
  ggtitle("Holt-Winters: additive vs multiplicative") +
  guides(colour = guide_legend(title = "Forecast"))

# Acuratețe (in-sample) – utilă orientativ, dar ideal și CV
round(accuracy(fit_hw_add), 2)
round(accuracy(fit_hw_mul), 2)

summary(fit_hw_add)
# Model: y_t = l_t + b_t + s_t + e_t
# Parametri de netezire:
# alpha = 0.3063  => nivelul se adaptează moderat la observațiile noi
# beta  = 0.0001  => trend aproape constant (fără accelerări)
# gamma = 0.4263  => sezonalitatea se actualizează relativ rapid
# Stări inițiale:
# l = 32.2597     => nivel inițial al seriei
# b = 0.7014      => trend inițial (creștere medie pe perioadă)
# s = (1.311, -1.694, -9.313, 9.696)
#   => efecte sezoniere ADITIVE (se adaugă/scad din nivel)
# sigma = 1.949
#  variabilitate reziduală moderată
# Interpretare:
# Modelul presupune că amplitudinea sezonalității este constantă în timp,
# indiferent de nivelul seriei.
summary(fit_hw_mul)
# Model: y_t = (l_t + b_t) * s_t * e_t
# Parametri de netezire:
# alpha = 0.4406  => nivelul reacționează mai rapid decât în modelul aditiv
# beta  = 0.0134  => trend ușor adaptiv
# gamma = 0.0023  => sezonalitate foarte stabilă în timp
# Stări inițiale:
# l = 32.4875     => nivel inițial
# b = 0.6974      => trend inițial
# s = (1.024, 0.9618, 0.7704, 1.244)
#   => factori sezoniere MULTIPLICATIVE (proporționale cu nivelul)
# sigma = 0.0367
#  reziduuri mult mai mici decât în cazul aditiv
# Interpretare:
# Modelul multiplicativ oferă o potrivire net superioară, indicând că
# sezonalitatea este dependentă de nivelul seriei.

# Holt–Winters multiplicativ este preferabil pentru această serie, deoarece
# surprinde mai bine sezonalitatea proporțională cu nivelul și produce
# reziduuri semnificativ mai mici decât modelul aditiv.


# TEORIE:
# ETS selectează automat forma (E,T,S):
# - E: tipul erorii (A/M)
# - T: tipul trendului (N/A/Ad/M etc.)
# - S: tipul sezonalității (N/A/M)
# În practică, ets() caută un model bun prin criterii (AICc etc.)
fit_ets <- ets(aust)

summary(fit_ets)     # parametri + acuratețe + forma ETS aleasă
# Notatia ETS(E, T, S):
# E = Error component
# T = Trend component
# S = Seasonal component
# ETS(M, A, M) inseamna:
# - E = M => erori multiplicative
# - T = A => trend aditiv
# - S = M => sezonalitate multiplicativa
# Functia ets selecteaza AUTOMAT cea mai buna combinatie de:
# - tip de eroare
# - tip de trend
# - tip de sezonalitate
# pe baza criteriilor informationale (AIC/AICc).
# Pentru seria aust, modelul optim selectat este ETS(M, A, M),
# confirmand ca seria are:
# - trend
# - sezonalitate dependenta de nivel
# - variabilitate proportionala cu nivelul seriei
# alpha = 0.1908  => netezirea NIVELULUI
# - valoare relativ mica
# - nivelul se adapteaza lent la observatiile noi
# - sugereaza o evolutie stabila a nivelului seriei
# beta = 0.0392   => netezirea TRENDULUI
# - trendul este actualizat moderat
# - permite ajustari lente ale pantei in timp
# gamma = 0.0002  => netezirea SEZONALITATII
# - valoare foarte mica
# - structura sezoniera este extrem de stabila
# - sezonalitatea ramane aproape constanta in timp (proportional)
# l = 32.3679  => nivel initial al seriei
# - valoarea de pornire a nivelului estimat
# b = 0.9281   => trend initial
# - cresterea medie pe perioada (mai accentuata decat in HW)
# s = (1.022, 0.9628, 0.7683, 1.247)
# - factori sezoniere MULTIPLICATIVE
# - valori > 1 indica luni cu nivel peste medie
# - valori < 1 indica luni cu nivel sub medie
# sigma = 0.0383
# - deviația standard a erorilor multiplicative
# - valoare foarte mica
# - indica o potrivire excelenta a modelului
# Modelul ETS(M, A, M) confirma existenta unui trend aditiv si a unei
# sezonalitati multiplicative stabile. Parametrii de netezire mici indica
# o dinamica structurala stabila, iar valoarea redusa a lui sigma arata
# ca modelul explica foarte bine variatia seriei.
# ETS(M, A, M) este superior Holt–Winters clasic deoarece:
# - selecteaza automat structura optima
# - include explicit tipul erorii
# - ofera o potrivire statistica mai buna

autoplot(fit_ets)    # componentele modelului (nivel, trend, sezon)

# Prognoză pe 5 pași înainte
forecast(fit_ets, h = 5) %>%
  autoplot() +
  ylab("Visitor nights (millions)") +
  xlab("Year") +
  ggtitle("ETS forecast (h = 5)") +
  theme_bw()


