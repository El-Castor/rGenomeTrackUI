# =============================================================================
# mod_documentation.R — Module de documentation intégrée
# =============================================================================

mod_documentation_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_fluid(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .doc-section { padding: 0.5rem 0 1rem 0; }
        .doc-section h4 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 0.3rem; }
        .workflow-step { display: flex; align-items: flex-start; gap: 1rem;
                         background: #f8f9fa; border-radius: 8px; padding: 1rem;
                         margin-bottom: 0.75rem; border-left: 4px solid #3498db; }
        .step-num { font-size: 1.5rem; font-weight: bold; color: #3498db;
                    min-width: 2rem; text-align: center; }
        .format-table td, .format-table th { vertical-align: middle; font-size: 0.9em; }
        .tip-box { background: #e8f4f8; border-left: 4px solid #17a2b8;
                   padding: 0.75rem 1rem; border-radius: 0 4px 4px 0; margin: 0.5rem 0; }
        .warn-box { background: #fff3cd; border-left: 4px solid #ffc107;
                    padding: 0.75rem 1rem; border-radius: 0 4px 4px 0; margin: 0.5rem 0; }
        .code-inline { font-family: monospace; background: #f0f0f0;
                        padding: 0.1em 0.4em; border-radius: 3px; }
        .faq-q { font-weight: bold; color: #2c3e50; }
        .faq-a { color: #555; margin-bottom: 1rem; }
        .use-case-card { border: 1px solid #dee2e6; border-radius: 8px;
                         padding: 1rem; margin-bottom: 1rem; }
        .use-case-card .case-title { font-weight: bold; color: #495057;
                                      font-size: 1.05rem; margin-bottom: 0.5rem; }
      "))
    ),

    shiny::fluidRow(
      shiny::column(12,
        shiny::h2(shiny::icon("book-open"), " Documentation rGenomeTrackUI",
                  class = "mt-3 mb-1"),
        shiny::p(shiny::HTML(
          "Guide complet pour utiliser <strong>rGenomeTrackUI</strong>, l'interface graphique pour
          <a href='https://pygenometracks.readthedocs.io' target='_blank'>pyGenomeTracks</a>."
        ), class = "lead text-muted mb-4")
      )
    ),

    # Tabs principales
    bslib::navset_tab(
      id = ns("doc_tabs"),

      # -------------------------------------------------------------------------
      # Onglet 1 : Présentation
      # -------------------------------------------------------------------------
      bslib::nav_panel("Présentation",
        shiny::div(class = "doc-section mt-3",
          shiny::h4(shiny::icon("info-circle"), " Qu'est-ce que rGenomeTrackUI ?"),
          shiny::p(shiny::HTML(
            "<strong>rGenomeTrackUI</strong> est une interface Shiny permettant de configurer et
            lancer <strong>pyGenomeTracks</strong> — un outil Python de référence pour la
            visualisation de données génomiques multi-niveaux — sans avoir à écrire de code."
          )),
          shiny::hr(),

          shiny::h4(shiny::icon("layer-group"), " Architecture générale"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header("Ce que fait rGenomeTrackUI"),
              shiny::tags$ul(
                shiny::tags$li("Gestion de projets multi-analyses"),
                shiny::tags$li("Registre d'importation de fichiers génomiques"),
                shiny::tags$li("Configuration visuelle des tracks (pistes)"),
                shiny::tags$li("Génération automatique du fichier ", shiny::code("tracks.ini")),
                shiny::tags$li("Génération de scripts R et Shell reproductibles"),
                shiny::tags$li("Lancement de pyGenomeTracks et affichage des résultats")
              )
            ),
            bslib::card(
              bslib::card_header("Ce que fait pyGenomeTracks"),
              shiny::tags$ul(
                shiny::tags$li("Rendu des figures PNG/PDF/SVG haute résolution"),
                shiny::tags$li("Support : BigWig, BedGraph, GTF, BED, narrowPeak, Links…"),
                shiny::tags$li("Superposition de plusieurs régions"),
                shiny::tags$li("Personnalisation fine des couleurs et styles"),
                shiny::tags$li("Interface en ligne de commande reproductible")
              )
            )
          ),
          shiny::hr(),

          shiny::h4(shiny::icon("check-circle"), " Prérequis"),
          shiny::div(class = "tip-box",
            shiny::tags$ul(
              shiny::tags$li("Environnement conda ", shiny::code("rgenometrackui"), " activé"),
              shiny::tags$li("pyGenomeTracks installé dans l'environnement conda"),
              shiny::tags$li("R ≥ 4.0 avec les packages Shiny, bslib, DT, etc.")
            )
          ),
          shiny::div(class = "warn-box",
            shiny::icon("exclamation-triangle"), " Sur macOS, l'app doit être lancée via ",
            shiny::code("bash scripts/run_app.sh"), " pour que les dépendances conda soient trouvées."
          )
        )
      ),

      # -------------------------------------------------------------------------
      # Onglet 2 : Workflow
      # -------------------------------------------------------------------------
      bslib::nav_panel("Workflow",
        shiny::div(class = "doc-section mt-3",
          shiny::h4(shiny::icon("route"), " Étapes du workflow"),
          shiny::p("Suivez ces étapes dans l'ordre pour produire vos premières figures :"),

          shiny::div(class = "workflow-step",
            shiny::div(class = "step-num", "1"),
            shiny::div(
              shiny::strong("Dashboard — Vérifier les dépendances"),
              shiny::p("Depuis l'onglet Dashboard, vérifiez que tous les statuts sont verts.
                 Si pyGenomeTracks ou BEDTools sont manquants, corrigez l'environnement conda
                 avant de continuer.")
            )
          ),
          shiny::div(class = "workflow-step",
            shiny::div(class = "step-num", "2"),
            shiny::div(
              shiny::strong("Projets — Créer ou ouvrir un projet"),
              shiny::p("Un projet = un dossier sur disque contenant les fichiers de données, la config,
                 et les résultats. Créez un nouveau projet ou ouvrez-en un existant.")
            )
          ),
          shiny::div(class = "workflow-step",
            shiny::div(class = "step-num", "3"),
            shiny::div(
              shiny::strong("Inputs — Importer vos fichiers de données"),
              shiny::p("Uploadez ou référencez vos fichiers (BigWig, BED, GTF…).
                 Consultez l'onglet 'Formats & templates' pour vérifier le format attendu
                 et télécharger un template si besoin.")
            )
          ),
          shiny::div(class = "workflow-step",
            shiny::div(class = "step-num", "4"),
            shiny::div(
              shiny::strong("Track Builder — Configurer les pistes"),
              shiny::p("Ajoutez des tracks, sélectionnez leurs fichiers de données, et ajustez
                 les paramètres visuels (couleur, hauteur, type d'affichage…).")
            )
          ),
          shiny::div(class = "workflow-step",
            shiny::div(class = "step-num", "5"),
            shiny::div(
              shiny::strong("Régions & Figure — Définir la région à visualiser"),
              shiny::p("Saisissez une région au format ", shiny::code("chr:start-end"),
                " ou uploadez un fichier BED multi-régions. Ajustez la taille et le titre de la figure.")
            )
          ),
          shiny::div(class = "workflow-step",
            shiny::div(class = "step-num", "6"),
            shiny::div(
              shiny::strong("Aperçu config → Run — Vérifier et lancer"),
              shiny::p("Consultez l'aperçu de la configuration générée, puis lancez pyGenomeTracks
                 depuis l'onglet Run. Les figures apparaissent dans l'onglet Résultats.")
            )
          )
        )
      ),

      # -------------------------------------------------------------------------
      # Onglet 3 : Formats de fichiers
      # -------------------------------------------------------------------------
      bslib::nav_panel("Formats",
        shiny::div(class = "doc-section mt-3",
          shiny::h4(shiny::icon("file-alt"), " Formats de fichiers supportés"),
          bslib::layout_columns(
            col_widths = c(4, 8),
            shiny::div(
              shiny::selectInput(
                ns("format_selector"),
                "Sélectionner un format :",
                choices = c(
                  "BED (annotations)" = "bed",
                  "BedGraph (signal texte)" = "bedgraph",
                  "BigWig (signal binaire)" = "bigwig",
                  "GTF/GFF (gènes)" = "gtf",
                  "narrowPeak (pics ChIP)" = "narrowpeak",
                  "BEDPE / Links" = "bedpe",
                  "Domains (TADs)" = "domains",
                  "Regions BED" = "regions",
                  "Lignes verticales" = "vlines",
                  "Lignes horizontales" = "hlines"
                )
              ),
              shiny::uiOutput(ns("format_dl_btn"))
            ),
            shiny::div(
              shiny::uiOutput(ns("format_help_panel"))
            )
          ),
          shiny::hr(),
          shiny::h5("Tableau récapitulatif de tous les formats"),
          DT::DTOutput(ns("formats_overview_table"))
        )
      ),

      # -------------------------------------------------------------------------
      # Onglet 4 : Cas d'usage
      # -------------------------------------------------------------------------
      bslib::nav_panel("Cas d'usage",
        shiny::div(class = "doc-section mt-3",
          shiny::h4(shiny::icon("vials"), " Exemples de cas d'usage typiques"),

          shiny::div(class = "use-case-card",
            shiny::div(class = "case-title", shiny::icon("dna"), " Visualisation d'un locus ChIP-seq"),
            shiny::p("Superposer plusieurs tracks d'enrichissement H3K27ac avec annotation de gènes."),
            shiny::tags$ul(
              shiny::tags$li("1 track GTF (gènes)"),
              shiny::tags$li("2-4 tracks BigWig (signal H3K27ac)"),
              shiny::tags$li("1 track narrowPeak (pics appelés)"),
              shiny::tags$li("Région : locus d'intérêt, typiquement 50kb-500kb")
            )
          ),

          shiny::div(class = "use-case-card",
            shiny::div(class = "case-title", shiny::icon("project-diagram"), " Structure chromatinienne Hi-C"),
            shiny::p("Visualiser un domaine TAD avec les interactions intra-domaines."),
            shiny::tags$ul(
              shiny::tags$li("1 track Domains (TAD boundaries)"),
              shiny::tags$li("1 track Links (BEDPE interactions)"),
              shiny::tags$li("1 track BigWig (coverage CTCF ou cohesin)"),
              shiny::tags$li("Région : 1-5 Mb autour du locus")
            )
          ),

          shiny::div(class = "use-case-card",
            shiny::div(class = "case-title", shiny::icon("chart-area"), " Profil d'expression RNA-seq"),
            shiny::p("Afficher la couverture RNA-seq sur un gène ou une région."),
            shiny::tags$ul(
              shiny::tags$li("1-2 tracks BigWig (coverage RNA-seq sens/antisens)"),
              shiny::tags$li("1 track GTF (annotation gènes)"),
              shiny::tags$li("Lignes verticales : marqueurs d'exons d'intérêt"),
              shiny::tags$li("Région : gène ± 5 kb")
            )
          ),

          shiny::div(class = "use-case-card",
            shiny::div(class = "case-title", shiny::icon("map-marked"), " Multi-régions comparatives"),
            shiny::p("Produire automatiquement une figure par région dans un fichier BED."),
            shiny::tags$ul(
              shiny::tags$li("Fichier BED multi-lignes dans 'Régions & Figure'"),
              shiny::tags$li("Même configuration de tracks pour toutes les régions"),
              shiny::tags$li("Utile pour comparer des promoteurs ou des enhancers")
            )
          ),

          shiny::div(class = "use-case-card",
            shiny::div(class = "case-title", shiny::icon("crosshairs"), " Variants/mutations ponctuelles"),
            shiny::p("Mettre en évidence des positions précises (SNPs, indels, breakpoints)"),
            shiny::tags$ul(
              shiny::tags$li("Tracks BigWig ou BedGraph (signal dans la région)"),
              shiny::tags$li("Track GTF (gène affecté)"),
              shiny::tags$li("Lignes verticales : position exacte du ou des variants")
            )
          )
        )
      ),

      # -------------------------------------------------------------------------
      # Onglet 5 : Bonnes pratiques
      # -------------------------------------------------------------------------
      bslib::nav_panel("Bonnes pratiques",
        shiny::div(class = "doc-section mt-3",
          shiny::h4(shiny::icon("star"), " Bonnes pratiques"),

          shiny::h5("Organisation des fichiers"),
          shiny::tags$ul(
            shiny::tags$li("Placez tous vos fichiers d'un projet dans son dossier dédié."),
            shiny::tags$li("Nommez vos fichiers de façon descriptive (", shiny::code("sample_H3K27ac.bw"), ")."),
            shiny::tags$li("Évitez les espaces et caractères spéciaux dans les noms de fichiers.")
          ),

          shiny::h5("Noms de chromosomes"),
          shiny::div(class = "warn-box",
            shiny::icon("exclamation-triangle"),
            " Assurez-vous que les noms de chromosomes sont cohérents entre tous vos fichiers.
            Un fichier en ", shiny::code("chr1"), " et un autre en ", shiny::code("1"),
            " ne pourront pas être combinés."
          ),

          shiny::h5("Performance"),
          shiny::tags$ul(
            shiny::tags$li("Pour les fichiers > 100 Mb, utilisez le format BigWig (binaire) plutôt que BedGraph."),
            shiny::tags$li("Indexer les fichiers volumineux avec tabix si possible."),
            shiny::tags$li("Limitez le nombre de tracks actifs simultanément (< 15) pour des temps de rendu raisonnables.")
          ),

          shiny::h5("Reproductibilité"),
          shiny::tags$ul(
            shiny::tags$li("Utilisez la fonction 'Télécharger le script' (onglet Aperçu) pour sauvegarder votre paramétrage."),
            shiny::tags$li("Le fichier ", shiny::code("tracks.ini"), " généré peut être réutilisé directement en ligne de commande."),
            shiny::tags$li("Documentez vos projets avec des noms de runs explicites.")
          )
        )
      ),

      # -------------------------------------------------------------------------
      # Onglet 6 : Troubleshooting
      # -------------------------------------------------------------------------
      bslib::nav_panel("Troubleshooting",
        shiny::div(class = "doc-section mt-3",
          shiny::h4(shiny::icon("tools"), " Résolution de problèmes"),

          shiny::tags$dl(
            shiny::tags$dt("pyGenomeTracks est marqué 'manquant'"),
            shiny::tags$dd(shiny::HTML(
              "Assurez-vous de lancer l'app via <code>bash scripts/run_app.sh</code> et non
              <code>Rscript app.R</code>. Le script exporte le PATH de l'environnement conda."
            )),

            shiny::tags$dt("BEDTools non trouvé"),
            shiny::tags$dd(shiny::HTML(
              "Vérifiez que l'env conda <code>rgenometrackui</code> est actif et que BEDTools
              est installé : <code>conda install -c bioconda bedtools</code>"
            )),

            shiny::tags$dt("Erreur lors du rendu : 'chromosome not found'"),
            shiny::tags$dd(
              "Les noms de chromosomes ne correspondent pas entre les fichiers. Vérifiez
              qu'ils utilisent tous la même convention (ex: 'chr1' vs '1')."
            ),

            shiny::tags$dt("La figure n'affiche aucun signal"),
            shiny::tags$dd(
              "Vérifiez que la région sélectionnée contient bien des données dans vos fichiers.
              Testez avec une région plus large ou vérifiez le fichier avec un navigateur génomique."
            ),

            shiny::tags$dt("Upload de fichier bloqué"),
            shiny::tags$dd(shiny::HTML(
              "La taille maximale d'upload par défaut Shiny est 30 Mb. Pour les gros fichiers,
              utilisez l'option 'Chemin local' pour référencer les fichiers directement sans upload."
            )),

            shiny::tags$dt("Les packages R ne se chargent pas"),
            shiny::tags$dd(shiny::HTML(
              "Relancez <code>bash scripts/run_app.sh</code> depuis un terminal avec l'env conda actif.
              Vérifiez via <code>bash scripts/check_r_dependencies.R</code>"
            ))
          )
        )
      ),

      # -------------------------------------------------------------------------
      # Onglet 7 : FAQ
      # -------------------------------------------------------------------------
      bslib::nav_panel("FAQ",
        shiny::div(class = "doc-section mt-3",
          shiny::h4(shiny::icon("question-circle"), " Questions fréquentes"),

          shiny::div(class = "faq-q", "Peut-on visualiser des données d'ARN-seq ?"),
          shiny::div(class = "faq-a", shiny::HTML(
            "Oui. Générez un fichier BigWig depuis votre BAM avec <code>bamCoverage</code>
            (deeptools) et importez-le comme track BigWig."
          )),

          shiny::div(class = "faq-q", "Peut-on superposer plusieurs tracks BigWig ?"),
          shiny::div(class = "faq-a", "Oui, ajoutez autant de tracks BigWig que nécessaire dans le Track Builder.
            Chaque track est indépendant et peut avoir ses propres paramètres de couleur et d'échelle."),

          shiny::div(class = "faq-q", "Le format bigBed est-il supporté ?"),
          shiny::div(class = "faq-a", shiny::HTML(
            "Non directement. Convertissez en BED avec <code>bigBedToBed</code> (UCSC tools)."
          )),

          shiny::div(class = "faq-q", "Comment exporter les figures ?"),
          shiny::div(class = "faq-a", "Les figures sont générées dans le dossier du run (PNG par défaut).
            Vous pouvez choisir le format (PNG/PDF/SVG) dans les paramètres de figure."),

          shiny::div(class = "faq-q", "Peut-on utiliser pyGenomeTracks en ligne de commande après avoir configuré via l'interface ?"),
          shiny::div(class = "faq-a", shiny::HTML(
            "Oui. La commande Shell générée (onglet Aperçu > Shell script) peut être copiée
            et exécutée directement dans un terminal avec l'environnement conda actif."
          )),

          shiny::div(class = "faq-q", "Peut-on partager un projet ?"),
          shiny::div(class = "faq-a", "Oui. Le dossier de projet contient tous les fichiers de config,
            les données et les résultats. Compressez-le et partagez-le.")
        )
      )
    )
  )
}

mod_documentation_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    specs <- load_input_specs()

    # ---- Aide contextuelle par format ----
    output$format_help_panel <- shiny::renderUI({
      req(input$format_selector)
      shiny::HTML(render_format_help(input$format_selector, specs))
    })

    # ---- Bouton de téléchargement du template ----
    output$format_dl_btn <- shiny::renderUI({
      req(input$format_selector)
      tpl_path <- get_template_path(input$format_selector, "templates/input_files", specs)
      ex_path  <- get_example_path(input$format_selector, "examples/input_files", specs)
      btns <- list()
      if (!is.null(tpl_path)) {
        btns[[length(btns) + 1]] <- shiny::downloadButton(
          ns(paste0("dl_template_", input$format_selector)),
          "Télécharger template",
          class = "btn btn-outline-secondary btn-sm mt-1 w-100"
        )
      }
      if (!is.null(ex_path)) {
        btns[[length(btns) + 1]] <- shiny::downloadButton(
          ns(paste0("dl_example_", input$format_selector)),
          "Télécharger exemple",
          class = "btn btn-outline-info btn-sm mt-1 w-100"
        )
      }
      if (length(btns) == 0) shiny::p(shiny::em("(Aucun fichier disponible pour ce format)"))
      else shiny::div(btns)
    })

    # ---- Handlers de téléchargement (un par format) ----
    for (fmt_id in names(specs)) {
      local({
        fid <- fmt_id

        output[[paste0("dl_template_", fid)]] <- shiny::downloadHandler(
          filename = function() {
            spec <- specs[[fid]]
            spec$template_file %||% paste0("template_", fid, ".txt")
          },
          content = function(file) {
            tpl <- get_template_path(fid, "templates/input_files", specs)
            if (!is.null(tpl)) file.copy(tpl, file)
          }
        )

        output[[paste0("dl_example_", fid)]] <- shiny::downloadHandler(
          filename = function() {
            spec <- specs[[fid]]
            spec$example_file %||% paste0("example_", fid, ".txt")
          },
          content = function(file) {
            ex <- get_example_path(fid, "examples/input_files", specs)
            if (!is.null(ex)) file.copy(ex, file)
          }
        )
      })
    }

    # ---- Tableau récapitulatif ----
    output$formats_overview_table <- DT::renderDT({
      df <- format_specs_as_dataframe(specs)
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          dom = "t",
          pageLength = 20,
          ordering = FALSE,
          language = list(
            url = "//cdn.datatables.net/plug-ins/1.13.1/i18n/fr-FR.json"
          )
        ),
        class = "table table-sm table-striped format-table"
      ) |>
        DT::formatStyle(
          "Binaire",
          color = DT::styleEqual(c("Oui", "Non"), c("#856404", "#155724")),
          fontWeight = "bold"
        )
    }, server = FALSE)
  })
}
