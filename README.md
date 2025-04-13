# hackathon_bsa
# 📊 TempApp : Modélisation Automatique de Séries Temporelles

Bienvenue sur le dépôt officiel de notre projet développé dans le cadre du Hackathon !  
Notre application facilite la **modélisation automatique de séries temporelles** via une interface intuitive et rapide. 🌟

---

## 🛠️ Structure du projet

```
├── serie.R                # Script principal de l'application (R Shiny / traitement)
├── report_template.Rmd     # Template R Markdown pour la génération automatique des rapports
├── www/                    # Dossier contenant les ressources statiques (images, logos, icônes)
│   ├── image1.png
│   ├── image2.png
│   └── ...
└── README.md               # Documentation du projet
```

---

## 🚀 Fonctionnalités principales

- **Chargement dynamique** de vos propres données (CSV / Excel).
- **Détection automatique** de la tendance et de la saisonnalité.
- **Proposition de modèles adaptés** (ARIMA, SARIMA, Prophet...).
- **Tests de significativité** sur les modèles estimés.
- **Prévisions** sur l'avenir de votre série temporelle.
- **Génération automatique** d'un rapport d'analyse complet en html et word.

---

## 📖 Prérequis

- R (version ≥ 4.0)
- Packages R : `shiny`, `forecast`, `prophet`, `tseries`, `ggplot2`, `rmarkdown`, etc.

Installer les packages manquants via :
```R
install.packages(c("shiny", "forecast", "prophet", "tseries", "ggplot2", "rmarkdown"))
```

---

## 🔄 Lancer l'application

Cliquez sur le lien 
https://hamed-apps.shinyapps.io/serie_temp/ 

L'application s'ouvrira automatiquement dans votre navigateur web.

---

## 🛍️ Notes supplémentaires

- Le format des données doit contenir **au moins deux colonnes** : une colonne de dates et une colonne de valeurs.
- Les prétraitements de base (nettoyage NA, conversion de dates) sont automatisés.
- Le rapport PDF généré à la fin est personnalisable via le fichier `report_template.Rmd`.

---

## 🚀 Contribution

N'hésitez pas à proposer des suggestions ou à ouvrir des issues si vous identifiez des axes d'amélioration !

---


