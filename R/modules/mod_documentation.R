# =============================================================================
# mod_documentation.R — Module de documentation intégrée
# =============================================================================

mod_documentation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    omics_banner(
      "Documentation",
      "Guide complet d'utilisation de rGenomeTrackUI et pyGenomeTracks.",
      small = TRUE
    ),

    # Tabs principales
    bslib::navset_tab(
      id = ns("doc_tabs"),

      # -----------------------------------------------------------------------
      # Onglet 1 : Présentation
      # -----------------------------------------------------------------------
      bslib::nav_panel(
        shiny::tagList(shiny::icon("info-circle"), " Présentation"),
        shiny::div(class = "mt-3",
          shiny::tags$div(
            class = "rt-card",
            shiny::tags$div(class = "doc-section-title",
              shiny::icon("question-circle"), " Qu'est-ce que rGenomeTrackUI ?"),
            shiny::p(shiny::HTML(
              "<strong>rGenomeTrackUI</strong> est une interface Shiny permettant de configurer et
              lancer <strong>pyGenomeTracks</strong> \u2014 un outil Python de r\u00e9f\u00e9rence pour la
              visualisation de donn\u00e9es g\u00e9nomiques multi-niveaux \u2014 sans \u00e9crire de code."
            )),
            shiny::tags$div(class = "doc-section-title",
              shiny::icon("layer-group"), " Architecture g\u00e9n\u00e9rale"),
            shiny::fluidRow(
              shiny::column(6,
                shiny::tags$div(class = "rt-card",
                  shiny::tags$div(class = "rt-card-header",
                    shiny::tags$span(class = "rt-card-icon", shiny::icon("desktop")),
                    shiny::tags$h5("Ce que fait rGenomeTrackUI")),
                  shiny::tags$ul(
                    shiny::tags$li("Gestion de projets multi-analyses"),
                    shiny::tags$li("Registre d'importation de fichiers g\u00e9nomiques"),
                    shiny::tags$li("Configuration visuelle des tracks"),
                    shiny::tags$li("G\u00e9n\u00e9ration automatique de ", shiny::code("tracks.ini")),
                    shiny::tags$li("G\u00e9n\u00e9ration de scripts R et Shell reproductibles"),
                    shiny::tags$li("Lancement de pyGenomeTracks et affichage des r\u00e9sultats")
                  )
                )
              ),
              shiny::column(6,
                shiny::tags$div(class = "rt-card",
                  shiny::tags$div(class = "rt-card-header",
                    shiny::tags$span(class = "rt-card-icon", shiny::icon("python")),
                    shiny::tags$h5("Ce que fait pyGenomeTracks")),
                  shiny::tags$ul(
                    shiny::tags$li("Rendu des figures PNG/PDF/SVG haute r\u00e9solution"),
                    shiny::tags$li("Support : BigWig, BedGraph, GTF, BED, narrowPeak, Links\u2026"),
                    shiny::tags$li("Superposition de plusieurs r\u00e9gions"),
                    shiny::tags$li("Personnalisation fine des couleurs et styles"),
                    shiny::tags$li("Interface en ligne de commande reproductible")
                  )
                )
              )
            ),
            shiny::tags$div(class = "doc-section-title",
              shiny::icon("check-circle"), " Pr\u00e9requis"),
            shiny::div(class = "doc-tip-box",
              shiny::tags$ul(
                shiny::tags$li("Environnement conda ", shiny::code("rgenometrackui"), " activ\u00e9"),
                shiny::tags$li("pyGenomeTracks install\u00e9 dans l'environnement conda"),
                shiny::tags$li("R \u2265 4.0 avec les packages Shiny, bslib, DT, etc.")
              )
            ),
            shiny::div(class = "doc-warn-box",
              shiny::icon("exclamation-triangle"),
              " Sur macOS, l'app doit \u00eatre lanc\u00e9e via ",
              shiny::code("bash scripts/run_app.sh"),
              " pour que les d\u00e9pendances conda soient trouv\u00e9es."
            )
          )
        )
      ),

      # -----------------------------------------------------------------------
      # Onglet 2 : Workflow
      # -----------------------------------------------------------------------
      bslib::nav_panel(
        shiny::tagList(shiny::icon("route"), " Workflow"),
        shiny::div(class = "mt-3",
          shiny::tags$div(
            class = "rt-card",
            shiny::tags$div(class = "doc-section-title",
              shiny::icon("route"), " \u00c9tapes du workflow"),
            shiny::p("Suivez ces \u00e9tapes dans l'ordre pour produire vos premi\u00e8res figures :"),
            shiny::div(class = "doc-workflow-step",
              shiny::div(class = "doc-step-num", "1"),
              shiny::div(
                shiny::tags$strong("Dashboard \u2014 V\u00e9rifier les d\u00e9pendances"),
                shiny::p("Depuis l'onglet Dashboard, v\u00e9rifiez que tous les statuts sont verts.
                   Si pyGenomeTracks ou BEDTools sont manquants, corrigez l'environnement conda
                   avant de continuer.")
              )
            ),
            shiny::div(class = "doc-workflow-step",
              shiny::div(class = "doc-step-num", "2"),
              shiny::div(
                shiny::tags$strong("Projets \u2014 Cr\u00e9er ou ouvrir un projet"),
                shiny::p("Un projet = un dossier sur disque contenant les fichiers de donn\u00e9es, la config,
                   et les r\u00e9sultats. Cr\u00e9ez un nouveau projet ou ouvrez-en un existant.")
              )
            ),
            shiny::div(class = "doc-workflow-step",
              shiny::div(class = "doc-step-num", "3"),
              shiny::div(
                shiny::tags$strong("Inputs \u2014 Importer vos fichiers de donn\u00e9es"),
                shiny::p("Uploadez ou r\u00e9f\u00e9rencez vos fichiers (BigWig, BED, GTF\u2026).
                   Consultez l'onglet 'Formats & templates' pour v\u00e9rifier le format attendu
                   et t\u00e9l\u00e9charger un template si besoin.")
              )
            ),
            shiny::div(class = "doc-workflow-step",
              shiny::div(class = "doc-step-num", "4"),
              shiny::div(
                shiny::tags$strong("Track Builder \u2014 Configurer les pistes"),
                shiny::p("Ajoutez des tracks, s\u00e9lectionnez leurs fichiers de donn\u00e9es, et ajustez
                   les param\u00e8tres visuels (couleur, hauteur, type d'affichage\u2026).")
              )
            ),
            shiny::div(class = "doc-workflow-step",
              shiny::div(class = "doc-step-num", "5"),
              shiny::div(
                shiny::tags$strong("R\u00e9gions & Figure \u2014 D\u00e9finir la r\u00e9gion \u00e0 visualiser"),
                shiny::p("Saisissez une r\u00e9gion au format ", shiny::code("chr:start-end"),
                  " ou uploadez un fichier BED multi-r\u00e9gions. Ajustez la taille et le titre de la figure.")
              )
            ),
            shiny::div(class = "doc-workflow-step",
              shiny::div(class = "doc-step-num", "6"),
              shiny::div(
                shiny::tags$strong("Aper\u00e7u config \u2192 Run \u2014 V\u00e9rifier et lancer"),
                shiny::p("Consultez l'aper\u00e7u de la configuration g\u00e9n\u00e9r\u00e9e, puis lancez pyGenomeTracks
                   depuis l'onglet Run. Les figures apparaissent dans l'onglet R\u00e9sultats.")
              )
            )
          )
        )
      ),

      # -----------------------------------------------------------------------
      # Onglet 3 : Formats de fichiers
      # -----------------------------------------------------------------------
      bslib::nav_panel(
        shiny::tagList(shiny::icon("file-alt"), " Formats"),
        shiny::div(class = "mt-3",
          shiny::tags$div(class = "rt-card",
            shiny::tags$div(class = "doc-section-title",
              shiny::icon("file-alt"), " Formats de fichiers support\u00e9s"),
            shiny::fluidRow(
              shiny::column(4,
                shiny::selectInput(
                  ns("format_selector"),
                  "S\u00e9lectionner un format :",
                  choices = c(
                    "BED (annotations)"       = "bed",
                    "BedGraph (signal texte)"  = "bedgraph",
                    "BigWig (signal binaire)"  = "bigwig",
                    "GTF/GFF (g\u00e8nes)"          = "gtf",
                    "narrowPeak (pics ChIP)"   = "narrowpeak",
                    "BEDPE / Links"            = "bedpe",
                    "Domains (TADs)"           = "domains",
                    "Regions BED"              = "regions",
                    "Lignes verticales"        = "vlines",
                    "Lignes horizontales"      = "hlines"
                  )
                ),
                shiny::uiOutput(ns("format_dl_btn"))
              ),
              shiny::column(8,
                shiny::uiOutput(ns("format_help_panel"))
              )
            ),
            shiny::tags$hr(class = "divider"),
            shiny::tags$h6("Tableau r\u00e9capitulatif de tous les formats"),
            DT::DTOutput(ns("formats_overview_table"))
          )
        )
      ),

      # -----------------------------------------------------------------------
      # Onglet 4 : Cas d'usage
      # -----------------------------------------------------------------------
      bslib::nav_panel(
        shiny::tagList(shiny::icon("vials"), " Cas d'usage"),
        shiny::div(class = "mt-3",
          shiny::tags$div(class = "rt-card",
            shiny::tags$div(class = "doc-section-title",
              shiny::icon("vials"), " Exemples de cas d'usage typiques"),
            shiny::div(class = "doc-use-case",
              shiny::div(class = "doc-use-case-title", shiny::icon("dna"), " Visualisation d'un locus ChIP-seq"),
              shiny::p("Superposer plusieurs tracks d'enrichissement H3K27ac avec annotation de g\u00e8nes."),
              shiny::tags$ul(
                shiny::tags$li("1 track GTF (g\u00e8nes)"),
                shiny::tags$li("2\u20134 tracks BigWig (signal H3K27ac)"),
                shiny::tags$li("1 track narrowPeak (pics appel\u00e9s)"),
                shiny::tags$li("R\u00e9gion : locus d'int\u00e9r\u00eat, typiquement 50 kb\u2013500 kb")
              )
            ),
            shiny::div(class = "doc-use-case",
              shiny::div(class = "doc-use-case-title", shiny::icon("project-diagram"), " Structure chromatinienne Hi-C"),
              shiny::p("Visualiser un domaine TAD avec les interactions intra-domaines."),
              shiny::tags$ul(
                shiny::tags$li("1 track Domains (TAD boundaries)"),
                shiny::tags$li("1 track Links (BEDPE interactions)"),
                shiny::tags$li("1 track BigWig (coverage CTCF ou cohesin)"),
                shiny::tags$li("R\u00e9gion : 1\u20135 Mb autour du locus")
              )
            ),
            shiny::div(class = "doc-use-case",
              shiny::div(class = "doc-use-case-title", shiny::icon("chart-area"), " Profil d'expression RNA-seq"),
              shiny::p("Afficher la couverture RNA-seq sur un g\u00e8ne ou une r\u00e9gion."),
              shiny::tags$ul(
                shiny::tags$li("1\u20132 tracks BigWig (coverage RNA-seq sens/antisens)"),
                shiny::tags$li("1 track GTF (annotation g\u00e8nes)"),
                shiny::tags$li("Lignes verticales : marqueurs d'exons d'int\u00e9r\u00eat"),
                shiny::tags$li("R\u00e9gion : g\u00e8ne \u00b1 5 kb")
              )
            ),
            shiny::div(class = "doc-use-case",
              shiny::div(class = "doc-use-case-title", shiny::icon("map-marked"), " Multi-r\u00e9gions comparatives"),
              shiny::p("Produire automatiquement une figure par r\u00e9gion dans un fichier BED."),
              shiny::tags$ul(
                shiny::tags$li("Fichier BED multi-lignes dans 'R\u00e9gions & Figure'"),
                shiny::tags$li("M\u00eame configuration de tracks pour toutes les r\u00e9gions"),
                shiny::tags$li("Utile pour comparer des promoteurs ou des enhancers")
              )
            ),
            shiny::div(class = "doc-use-case",
              shiny::div(class = "doc-use-case-title", shiny::icon("crosshairs"), " Variants/mutations ponctuelles"),
              shiny::p("Mettre en \u00e9vidence des positions pr\u00e9cises (SNPs, indels, breakpoints)."),
              shiny::tags$ul(
                shiny::tags$li("Tracks BigWig ou BedGraph (signal dans la r\u00e9gion)"),
                shiny::tags$li("Track GTF (g\u00e8ne affect\u00e9)"),
                shiny::tags$li("Lignes verticales : position exacte du ou des variants")
              )
            )
          )
        )
      ),

      # -----------------------------------------------------------------------
      # Onglet 5 : Bonnes pratiques
      # -----------------------------------------------------------------------
      bslib::nav_panel(
        shiny::tagList(shiny::icon("star"), " Bonnes pratiques"),
        shiny::div(class = "mt-3",
          shiny::tags$div(class = "rt-card",
            shiny::tags$div(class = "doc-section-title",
              shiny::icon("star"), " Bonnes pratiques"),
            shiny::tags$h6("Organisation des fichiers"),
            shiny::tags$ul(
              shiny::tags$li("Placez tous vos fichiers d'un projet dans son dossier d\u00e9di\u00e9."),
              shiny::tags$li("Nommez vos fichiers de fa\u00e7on descriptive (", shiny::code("sample_H3K27ac.bw"), ")."),
              shiny::tags$li("\u00c9vitez les espaces et caract\u00e8res sp\u00e9ciaux dans les noms de fichiers.")
            ),
            shiny::tags$h6("Noms de chromosomes"),
            shiny::div(class = "doc-warn-box",
              shiny::icon("exclamation-triangle"),
              " Assurez-vous que les noms de chromosomes sont coh\u00e9rents entre tous vos fichiers.
              Un fichier en ", shiny::code("chr1"), " et un autre en ", shiny::code("1"),
              " ne pourront pas \u00eatre combin\u00e9s."
            ),
            shiny::tags$h6("Performance"),
            shiny::tags$ul(
              shiny::tags$li("Pour les fichiers > 100 Mb, utilisez le format BigWig plut\u00f4t que BedGraph."),
              shiny::tags$li("Indexer les fichiers volumineux avec tabix si possible."),
              shiny::tags$li("Limitez le nombre de tracks actifs simultan\u00e9ment (< 15) pour des temps de rendu raisonnables.")
            ),
            shiny::tags$h6("Reproductibilit\u00e9"),
            shiny::tags$ul(
              shiny::tags$li("Utilisez la fonction 'T\u00e9l\u00e9charger le script' (onglet Aper\u00e7u) pour sauvegarder votre param\u00e9trage."),
              shiny::tags$li("Le fichier ", shiny::code("tracks.ini"), " g\u00e9n\u00e9r\u00e9 peut \u00eatre r\u00e9utilis\u00e9 directement en ligne de commande."),
              shiny::tags$li("Documentez vos projets avec des noms de runs explicites.")
            )
          )
        )
      ),

      # -----------------------------------------------------------------------
      # Onglet 6 : Troubleshooting
      # -----------------------------------------------------------------------
      bslib::nav_panel(
        shiny::tagList(shiny::icon("tools"), " Troubleshooting"),
        shiny::div(class = "mt-3",
          shiny::tags$div(class = "rt-card",
            shiny::tags$div(class = "doc-section-title",
              shiny::icon("tools"), " R\u00e9solution de probl\u00e8mes"),
            shiny::tags$dl(
              shiny::tags$dt("pyGenomeTracks est marqu\u00e9 'manquant'"),
              shiny::tags$dd(shiny::HTML(
                "Assurez-vous de lancer l'app via <code>bash scripts/run_app.sh</code> et non
                <code>Rscript app.R</code>. Le script exporte le PATH de l'environnement conda."
              )),
              shiny::tags$dt("BEDTools non trouv\u00e9"),
              shiny::tags$dd(shiny::HTML(
                "V\u00e9rifiez que l'env conda <code>rgenometrackui</code> est actif et que BEDTools
                est install\u00e9 : <code>conda install -c bioconda bedtools</code>"
              )),
              shiny::tags$dt("Erreur lors du rendu : 'chromosome not found'"),
              shiny::tags$dd(
                "Les noms de chromosomes ne correspondent pas entre les fichiers.
                V\u00e9rifiez qu'ils utilisent tous la m\u00eame convention (ex: 'chr1' vs '1')."
              ),
              shiny::tags$dt("La figure n'affiche aucun signal"),
              shiny::tags$dd(
                "V\u00e9rifiez que la r\u00e9gion s\u00e9lectionn\u00e9e contient bien des donn\u00e9es dans vos fichiers.
                Testez avec une r\u00e9gion plus large ou v\u00e9rifiez le fichier avec un navigateur g\u00e9nomique."
              ),
              shiny::tags$dt("Upload de fichier bloqu\u00e9"),
              shiny::tags$dd(shiny::HTML(
                "La taille maximale d'upload par d\u00e9faut Shiny est 30 Mb. Pour les gros fichiers,
                utilisez l'option 'Chemin local' pour r\u00e9f\u00e9rencer les fichiers directement sans upload."
              )),
              shiny::tags$dt("Les packages R ne se chargent pas"),
              shiny::tags$dd(shiny::HTML(
                "Relancez <code>bash scripts/run_app.sh</code> depuis un terminal avec l'env conda actif."
              ))
            )
          )
        )
      ),

      # -----------------------------------------------------------------------
      # Onglet 7 : FAQ
      # -----------------------------------------------------------------------
      bslib::nav_panel(
        shiny::tagList(shiny::icon("question-circle"), " FAQ"),
        shiny::div(class = "mt-3",
          shiny::tags$div(class = "rt-card",
            shiny::tags$div(class = "doc-section-title",
              shiny::icon("question-circle"), " Questions fr\u00e9quentes"),
            shiny::div(class = "doc-faq-q", "Peut-on visualiser des donn\u00e9es d'ARN-seq ?"),
            shiny::div(class = "doc-faq-a", shiny::HTML(
              "Oui. G\u00e9n\u00e9rez un fichier BigWig depuis votre BAM avec <code>bamCoverage</code>
              (deeptools) et importez-le comme track BigWig."
            )),
            shiny::div(class = "doc-faq-q", "Peut-on superposer plusieurs tracks BigWig ?"),
            shiny::div(class = "doc-faq-a",
              "Oui, ajoutez autant de tracks BigWig que n\u00e9cessaire dans le Track Builder.
              Chaque track est ind\u00e9pendant et peut avoir ses propres param\u00e8tres de couleur et d'\u00e9chelle."
            ),
            shiny::div(class = "doc-faq-q", "Le format bigBed est-il support\u00e9 ?"),
            shiny::div(class = "doc-faq-a", shiny::HTML(
              "Non directement. Convertissez en BED avec <code>bigBedToBed</code> (UCSC tools)."
            )),
            shiny::div(class = "doc-faq-q", "Comment exporter les figures ?"),
            shiny::div(class = "doc-faq-a",
              "Les figures sont g\u00e9n\u00e9r\u00e9es dans le dossier du run (PNG par d\u00e9faut).
              Vous pouvez choisir le format (PNG/PDF/SVG) dans les param\u00e8tres de figure."
            ),
            shiny::div(class = "doc-faq-q",
              "Peut-on utiliser pyGenomeTracks en ligne de commande apr\u00e8s avoir configur\u00e9 via l'interface ?"),
            shiny::div(class = "doc-faq-a", shiny::HTML(
              "Oui. La commande Shell g\u00e9n\u00e9r\u00e9e (onglet Aper\u00e7u > Shell script) peut \u00eatre copi\u00e9e
              et ex\u00e9cut\u00e9e directement dans un terminal avec l'environnement conda actif."
            )),
            shiny::div(class = "doc-faq-q", "Peut-on partager un projet ?"),
            shiny::div(class = "doc-faq-a",
              "Oui. Le dossier de projet contient tous les fichiers de config,
              les donn\u00e9es et les r\u00e9sultats. Compressez-le et partagez-le."
            )
          )
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
