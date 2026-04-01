#Carreguem els paquets
library(ggplot2)
library(tidyr)
library(dplyr)

#Llegim el conjunt de dades amb els resultats del qüestionari
df <- read.csv('Judicis_humans.csv')


# Calculem la mitja de l'elecció en cada context segons si és neologisme, distractor o una de les altres dues opcions
df_avg <- df %>%
  summarise(across(-terme, mean)) %>%
  pivot_longer(everything(), names_to = "category", values_to = "avg") %>%
  mutate(
    percentage = avg / sum(avg) * 100,
    category = factor(category,
                      levels = c("neologisme", "distractor", "incorrecte_1", "incorrecte_2"),
                      labels = c("Neologisme", "Distractor", "Incorrecte 1", "Incorrecte 2")
    )

#Fem el plot
ggplot(df_avg, aes(x = category, y = percentage, fill = category)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c(
    "Neologisme"   = "#000000",
    "Distractor"   = "#666666",
    "Incorrecte 1" = "#aaaaaa",
    "Incorrecte 2" = "#dddddd"
  )) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Mitjana de judicis humans per categoria (%)",
    x = NULL,
    y = "Percentatge (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11, margin = margin(b = 15)),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

# Carreguem les dades dels models (Si es vol fer amb el segon corpus s'ha de substituïr "Resultats.csv" per "Resultats nous.csv")
df <- read.csv("Resultats.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE)

# Canviem els noms de les columnes perquè no surtin tan llargs els noms
colnames(df)[1] <- "terme"
df <- df %>% rename(
  "Salamandra-7B"     = "BSC-LTsalamandra-7b",
  "CataLlama-8B v0.1" = "catallama-CataLlama-v0.1-Base",
  "CataLlama-v0.2"    = "catallama-CataLlama-v0.2-Base",
  "XGLM-7.5B"         = "facebook-xglm-7.5B",
  "Aitana-6.3B"       = "gplsi/Aitana-6.3B",
  "ALIA-40B"          = "BSC-LT/ALIA-40b"
)

# Pivotem a format llarg i reanomenar categories
df_long <- df %>%
  pivot_longer(-terme, names_to = "model", values_to = "categoria") %>%
  mutate(
    categoria = recode(categoria,
                       "neologisme" = "Neologisme",
                       "distractor" = "Distractor",
                       "erroni_1"   = "Opció errònia 1",
                       "erroni_2"   = "Opció errònia 2"
    ),
    # Col·loquem el neologisme al final perquè aparegui a la dreta
    categoria = factor(categoria, levels = c(
      "Opció errònia 2", "Opció errònia 1", "Distractor", "Neologisme"
    )),
    # rev() perquè els models es llegeixin de dalt a baix
    model = factor(model, levels = rev(c(
      "CataLlama-8B v0.1", "CataLlama-v0.2", "Aitana-6.3B",
      "Salamandra-7B", "XGLM-7.5B", "ALIA-40B"
    )))
  )

# Comptem i calculem percentatges per model
counts <- df_long %>%
  count(model, categoria) %>%
  group_by(model) %>%
  mutate(percentage = n / sum(n) * 100)

totals <- counts %>%
  summarise(total = sum(percentage))

# Fem el plot
ggplot(counts, aes(x = percentage, y = model, fill = categoria)) +
  geom_bar(stat = "identity", position = position_stack()) +
  # Percentatges dins de cada segment
  geom_text(
    aes(label = ifelse(percentage >= 5, sprintf("%.1f", percentage), "")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white", fontface = "bold"
  ) +
  # Total a la dreta de cada barra
  geom_text(
    data = totals,
    aes(x = total + 1, y = model, label = sprintf("%.1f", total)),
    inherit.aes = FALSE, hjust = 0, size = 3
  ) +
  scale_fill_manual(
    values = c(
      "Neologisme"      = "#7B8CDE",
      "Distractor"      = "#C8A2C8",
      "Opció errònia 1" = "#E53935",
      "Opció errònia 2" = "#B71C1C"
    ),
    breaks = c("Neologisme", "Distractor", "Opció errònia 1", "Opció errònia 2")
  ) +
  scale_x_continuous(limits = c(0, 115), breaks = seq(0, 100, 20)) +
  labs(
    title = "Eleccions per model",
    x = "Percentatge (%)",
    y = NULL,
    fill = "Elecció"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 30, b = 10, l = 10),
    legend.position = "bottom"
  )

#Ara fem el mateix però separant els anglicismes dels no-anglicismes

# Carreguem les dades (un altre cop es pot variar entre "Resultats.csv" i "Resultats_nous.csv")
df <- read.csv("Resultats_nous.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE)

# Separem els anglicismes dels no-anglicismes
df$grup <- c(rep("No-anglicismes", 10), rep("Anglicismes", 10))

# Tornem a canviar els noms de les columnes
colnames(df)[1] <- "terme"
df <- df %>% rename(
  "Salamandra-7B"     = "BSC/LTsalamandra-7b",
  "CataLlama-8B v0.1" = "catallama-CataLlama-v0.1-Base",
  "CataLlama-v0.2"    = "catallama-CataLlama-v0.2-Base",
  "XGLM-7.5B"         = "facebook-xglm-7.5B",
  "Aitana-6.3B"       = "gplsi/Aitana-6.3B",
  "ALIA-40B"          = "BSC-LT/ALIA-40b"
)

# De nou canviem a format llarg i tornem a canviar els noms de les categories, per fer-les més llegibles
df_long <- df %>%
  pivot_longer(-c(terme, grup), names_to = "model", values_to = "categoria") %>%
  mutate(
    categoria = recode(categoria,
                       "neologisme" = "Neologisme",
                       "distractor" = "Distractor",
                       "erroni_1"   = "Opció errònia 1",
                       "erroni_2"   = "Opció errònia 2"
    ),
    categoria = factor(categoria, levels = c(
      "Opció errònia 2", "Opció errònia 1", "Distractor", "Neologisme"
    )),
    model = factor(model, levels = rev(c(
      "CataLlama-8B v0.1", "CataLlama-v0.2", "Aitana-6.3B",
      "Salamandra-7B", "XGLM-7.5B", "ALIA-40B"
    ))),
    grup = factor(grup, levels = c("No-anglicismes", "Anglicismes"))
  )

counts <- df_long %>%
  count(model, grup, categoria) %>%
  group_by(model, grup) %>%
  mutate(percentage = n / sum(n) * 100)

# Fem el plot
ggplot(counts, aes(x = percentage, y = model, fill = categoria)) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(
    aes(label = ifelse(percentage >= 5, sprintf("%.1f", percentage), "")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Neologisme"      = "#7B8CDE",
      "Distractor"      = "#C8A2C8",
      "Opció errònia 1" = "#E53935",
      "Opció errònia 2" = "#B71C1C"
    ),
    breaks = c("Neologisme", "Distractor", "Opció errònia 1", "Opció errònia 2")
  ) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  facet_wrap(~ grup, ncol = 1) +
  labs(
    title = "Eleccions per model: anglicismes vs. no-anglicismes",
    x = "Percentatge (%)",
    y = NULL,
    fill = "Elecció"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 10, l = 10),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12)
  )