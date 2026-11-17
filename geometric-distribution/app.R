# Geometric Distribution Explorer
# Install once: install.packages(c("shiny", "ggplot2"))
# Open this file in RStudio and click Run App.

library(shiny)
library(ggplot2)

# DISCLAIMER: I used AI to build much of this shiny app dashboard

# 1. Small calculation and display helpers -------------------------------
MIN_P <- 0.0001
MAX_POINTS <- 2001
teal <- "#13776C"
orange <- "#B85C1B"
ink <- "#193B39"

valid_p <- function(p) {
  is.numeric(p) && length(p) == 1 && is.finite(p) && p >= MIN_P && p <= 1
}
whole <- function(x, max = 1e9) {
  is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0 &&
    x <= max && x == floor(x)
}
num <- function(x, digits = 3) {
  if (!is.finite(x)) return("Not finite")
  if (abs(x) >= 1e7) return(formatC(x, format = "e", digits = 2))
  format(round(x, digits), big.mark = ",", trim = TRUE, scientific = FALSE)
}
pct <- function(x) {
  if (x > 0 && x < 0.000001) return(paste0(formatC(100*x, format="e", digits=2), "%"))
  paste0(formatC(100*x, digits = 2, format = "f"), "%")
}
p_label <- function(p) format(signif(p, 5), trim = TRUE, scientific = FALSE)
auto_max <- function(p, cap = 2000) max(8, min(cap, qgeom(0.995, prob = p)))

# Use integer ticks, with enough room for the count labels at this plot width.
auto_tick_step <- function(bounds, width = 800) {
  if (is.null(width) || !is.finite(width) || width <= 0) width <- 800
  label_width <- max(32, 8 * nchar(as.character(bounds[2])) + 12)
  slots <- max(2, min(30, floor((width - 100) / label_width)))
  target <- max(1, ceiling(diff(bounds) / (slots - 1)))
  unit <- 10^floor(log10(target))
  candidates <- c(1, 2, 5, 10) * unit
  min(candidates[candidates >= target])
}

# Interval probabilities use a tail times a CDF, avoiding cancellation.
interval_prob <- function(a, b, p) {
  pgeom(a - 1, prob = p, lower.tail = FALSE) * pgeom(b - a, prob = p)
}
event_prob <- function(kind, k, a, b, p) {
  switch(kind, exact = dgeom(k, p), atmost = pgeom(k, p),
         more = pgeom(k, p, lower.tail = FALSE),
         between = interval_prob(a, b, p))
}
event_mask <- function(x, kind, k, a, b) {
  switch(kind, exact = x == k, atmost = x <= k, more = x > k,
         between = x >= a & x <= b)
}
# Reproducible sampling without changing another session's random seed.
draw_sample <- function(n, p, seed = NULL) {
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE))
        remove(".Random.seed", envir=.GlobalEnv)
    })
    set.seed(seed)
  }
  rgeom(n, prob = p)
}
plot_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(text = element_text(colour = ink),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E9EFED"),
          axis.title = element_text(size = 11, colour = "#526E69"),
          axis.title.x = element_text(margin = margin(t = 12)),
          plot.margin = margin(12, 18, 8, 8),
          strip.text = element_text(face = "bold", size = 12),
          strip.background = element_rect(fill = "#EDF6F3", colour = NA))
}
card <- function(..., class = "") div(class = paste("app-card", class), ...)
section_head <- function(kicker, title, note = NULL) {
  div(class="section-head", div(class="eyebrow", kicker), h2(title),
      if (!is.null(note)) p(class="muted", note))
}
metric <- function(label, value, detail, formula) {
  div(class="metric", div(class="metric-label", label),
      div(class="metric-value", value), div(class="metric-detail", detail),
      div(class="metric-formula", formula))
}
# Long sequences are abbreviated, but their full counts are always reported.
trial_strip <- function(failures, success = TRUE, animate = FALSE, past = FALSE) {
  n_show <- min(failures, 24)
  dots <- lapply(seq_len(n_show), function(i) {
    span(class=paste("trial", if(past) "past" else "failure",
                     if(animate) "trial-animate" else ""),
         style=paste0("animation-delay:", min(i*25,600), "ms"),
         title=paste("Failure", i), "F")
  })
  if (failures > n_show) dots <- c(dots, list(span(class="trial-more",
                                                   paste0("+", num(failures-n_show,0), " failures"))))
  if (success) dots <- c(dots, list(span(class=paste("trial success",
                                                     if(animate) "trial-animate" else ""), title="Success", "S")))
  if (!length(dots)) dots <- list(span(class="muted", "No failures"))
  div(class="trial-strip", role="img",
      `aria-label`=paste(num(failures,0), "failures", if(success) "then success" else ""), dots)
}

# 2. The visual design (self-contained; no external fonts or assets) -------
css <- "
:root{--teal:#13776c;--deep:#193b39;--muted:#59716b;--line:#dfe9e4;--orange:#b85c1b;--paper:#f5f7f3}
*{box-sizing:border-box}body{margin:0;background:var(--paper);color:var(--deep);font-family:Inter,'Segoe UI',Arial,sans-serif;font-size:15px;line-height:1.55}.container-fluid{padding:0}a{color:var(--teal)}a:hover{color:#0a5149}button,input,select{font:inherit}button:focus-visible,a:focus-visible,input:focus-visible,select:focus-visible,summary:focus-visible{outline:3px solid #d89536!important;outline-offset:3px}h1,h2,h3,h4,p{margin-top:0}h1{font-size:33px;letter-spacing:-1.2px;font-weight:700;margin-bottom:8px}h2{font-size:24px;font-weight:700;letter-spacing:-.6px;margin-bottom:6px}h3{font-size:18px;font-weight:700;letter-spacing:-.25px;margin-bottom:8px}h4{font-size:16px;font-weight:700}.muted,.help-block{color:var(--muted)}.eyebrow{font-size:10px;letter-spacing:2.1px;text-transform:uppercase;font-weight:800;color:var(--teal);margin-bottom:10px}.app-header{max-width:1500px;margin:auto;padding:32px 36px 22px;display:flex;align-items:center;justify-content:space-between;gap:20px}.brand-lockup{display:flex;align-items:center;gap:16px}.brand-icon{width:48px;height:48px;border-radius:15px;background:var(--teal);display:flex;align-items:end;gap:4px;justify-content:center;padding:12px;box-shadow:0 7px 16px #13776c20}.brand-icon i{display:block;width:4px;border-radius:2px;background:#fff}.brand-name{font-size:12px;font-weight:800;letter-spacing:2px}.brand-sub{color:var(--muted);font-size:12px}.header-tag{border:1px solid var(--line);border-radius:30px;padding:7px 13px;background:#ffffff80;font-size:12px}.app-layout{max-width:1500px;margin:auto;display:grid;grid-template-columns:260px minmax(0,1fr);gap:28px;padding:0 36px 40px}.sidebar{background:white;border:1px solid var(--line);border-radius:20px;padding:25px 22px;align-self:start;position:sticky;top:20px;box-shadow:0 5px 24px #193b3904}.sidebar h2{font-size:18px}.sidebar .small-note{font-size:11px;line-height:1.5}.sidebar hr{border-color:var(--line);margin:23px 0}.sidebar .form-group{margin-bottom:13px}.form-control{border:1px solid #cbdad3;border-radius:9px;box-shadow:none;min-height:39px;font-size:14px;color:var(--deep);background:#fff}.form-control:focus{border-color:var(--teal);box-shadow:0 0 0 3px #13776c16}.control-label{font-size:12px;font-weight:650;margin-bottom:6px}.form-group{margin-bottom:16px}.shiny-input-container:not(.shiny-input-container-inline){width:100%}.irs--shiny .irs-bar,.irs--shiny .irs-single{background:var(--teal);border-color:var(--teal)}.irs--shiny .irs-handle{border-color:var(--teal)}.irs--shiny .irs-min,.irs--shiny .irs-max{font-size:10px;color:var(--muted)}.btn{border-radius:9px;padding:9px 15px;font-weight:650;font-size:13px;box-shadow:none;transition:background .15s}.btn-primary,.btn-primary:focus,.btn-primary:active{background:var(--teal);border-color:var(--teal)}.btn-primary:hover{background:#0e5f56;border-color:#0e5f56}.btn-default{border:1px solid var(--line);background:#fff;color:var(--deep)}.btn-default:hover{background:#edf6f3;border-color:#b6cdc3}.preset-grid{display:flex;gap:6px;flex-wrap:wrap}.preset-grid .btn{font-size:11px;padding:6px 9px;font-weight:600}.reset-btn{width:100%;margin-top:8px}.convention{background:#edf6f3;border-radius:11px;padding:13px;font-size:12px;margin-top:22px;border-left:3px solid var(--teal)}.convention strong{display:block;margin-bottom:5px}.main{min-width:0}.intro{padding:8px 0 13px}.intro p{color:var(--muted);font-size:15px;margin-bottom:12px}.nav-tabs{border:0;background:#e9eee7;border-radius:12px;display:flex;padding:5px;gap:4px;margin:7px 0 23px;max-width:600px}.nav-tabs:before,.nav-tabs:after{display:none}.nav-tabs>li{flex:1;text-align:center;margin:0;float:none}.nav-tabs>li>a{padding:9px 12px;margin:0;border:0!important;border-radius:8px;font-weight:650;font-size:13px;color:var(--muted)}.nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover{background:#fff;color:var(--teal);box-shadow:0 2px 5px #193b3910}.nav-tabs>li>a:hover{background:#ffffff77;color:var(--deep)}.app-card{border:1px solid var(--line);background:#fff;border-radius:17px;padding:23px;margin-bottom:20px;box-shadow:0 4px 20px #193b3903}.metrics{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:12px;margin-bottom:20px}.metric{padding:18px 17px;background:#fff;border:1px solid var(--line);border-radius:13px;border-top:3px solid #a3cbbc}.metric-label{font-size:11px;color:var(--muted);font-weight:650}.metric-value{font-size:27px;font-weight:700;letter-spacing:-1px;margin:6px 0 2px;font-variant-numeric:tabular-nums;overflow-wrap:anywhere}.metric-detail{font-size:11px;color:var(--muted)}.chart-top{display:flex;justify-content:space-between;gap:16px;align-items:center}.chart-top .form-group{margin:0}.chart-top .radio-inline{font-size:12px;font-weight:600;margin-left:12px}.chart-top .shiny-input-container{width:auto}.chart-top h3{margin-bottom:3px}.chart-sub{font-size:12px;color:var(--muted)}.chart-foot{font-size:11px;color:var(--muted);padding-top:10px;display:flex;justify-content:space-between;gap:14px;flex-wrap:wrap;border-top:1px solid #eff3ef}.legend-dot{display:inline-block;width:8px;height:8px;background:var(--teal);border-radius:50%;margin-right:5px}.legend-dot.orange{background:var(--orange);margin-left:14px}.hover-readout{font-size:12px;color:var(--teal);min-height:25px;font-variant-numeric:tabular-nums}.chart-output{margin-top:12px}.shiny-plot-output{max-width:100%}.two-col{display:grid;grid-template-columns:minmax(0,1.12fr) minmax(0,.88fr);gap:20px}.two-col .app-card{height:100%}.two-col{margin-bottom:20px}.two-col .app-card{margin-bottom:0}.calc-controls{display:grid;grid-template-columns:1fr 1fr;gap:12px}.result-panel{background:#f0f7f3;border:1px solid #d9eade;border-radius:12px;padding:17px;margin-top:12px}.result-panel .result-value{font-size:34px;letter-spacing:-1px;color:var(--teal);font-weight:750;line-height:1.2;margin:7px 0}.result-panel p{font-size:13px;margin-bottom:3px}.formula{font-family:Georgia,'Times New Roman',serif;font-size:19px;line-height:1.8;overflow-wrap:anywhere}.formula.small{font-size:16px}.tiny{font-size:11px}.small-note{font-size:12px;color:var(--muted)}details{margin-top:15px;border-top:1px solid var(--line);padding-top:11px}summary{cursor:pointer;color:var(--teal);font-size:12px;font-weight:700;margin-bottom:8px}details p{font-size:12px}.notice{padding:12px 15px;background:#edf6f3;border:1px solid #dbeae2;border-radius:10px;font-size:12px;margin:12px 0}.notice.warning{background:#fff7ea;border-color:#edd8b9;color:#815219}.notice:empty{display:none}.section-head{margin:0 0 20px}.section-head p{font-size:13px;margin-bottom:0}.control-row{display:flex;flex-wrap:wrap;gap:15px;align-items:end}.control-row>div{flex:1;min-width:100px}.control-row .form-group{margin-bottom:10px}.compare-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:13px}.table{font-size:12px;color:var(--deep);margin-bottom:0}.table>thead>tr>th{border-bottom:1px solid var(--line);color:var(--muted);font-weight:650;white-space:normal}.table>tbody>tr>td{border-top:1px solid #edf2ee;padding:10px 8px}.table-responsive{border:0}input[type=checkbox],input[type=radio]{accent-color:var(--teal)}.checkbox label,.radio label{font-size:12px}.empty-state{padding:35px 20px;text-align:center;border:1px dashed #c8dcd2;border-radius:12px;background:#f9fcf9}.empty-state h3{font-size:16px}.empty-state p{font-size:13px;color:var(--muted);margin:0}.trial-strip{display:flex;flex-wrap:wrap;gap:7px;align-items:center;margin:18px 0}.trial{width:30px;height:30px;display:inline-flex;align-items:center;justify-content:center;font-size:11px;font-weight:750;border-radius:50%;border:1px solid #e6bf97;flex-shrink:0}.trial.failure{color:#94501a;background:#fff4e5}.trial.success{background:var(--teal);color:white;border-color:var(--teal)}.trial.past{color:#6f7c74;background:#eff2ed;border-color:#d6ded4}.trial-more{font-size:11px;color:var(--muted);padding:0 7px}.trial-animate{animation:pop-in .28s both}@keyframes pop-in{from{opacity:0;transform:scale(.6)}to{opacity:1;transform:scale(1)}}.sequence-label{font-size:11px;text-transform:uppercase;letter-spacing:1px;font-weight:700;color:var(--muted)}.equal-cards{display:grid;grid-template-columns:1fr 32px 1fr;gap:8px;align-items:center;margin:16px 0}.equal-sign{text-align:center;font-size:28px;color:var(--teal)}.equal-cards .result-panel{margin:0;min-height:118px}.pill{display:inline-block;padding:4px 10px;background:#edf6f3;color:var(--teal);border-radius:20px;font-size:11px;font-weight:700}.definition-list{padding-left:18px;font-size:13px}.definition-list li{margin-bottom:8px}.footer{font-size:11px;color:var(--muted);display:flex;gap:12px;justify-content:space-between;border-top:1px solid var(--line);padding-top:18px;margin-top:12px}.shiny-output-error-validation{color:#94501a;font-size:13px;white-space:normal}.shiny-output-error{font-size:13px}.shiny-busy .recalculating{opacity:.6}.shiny-disconnected-overlay{background:#f5f7f3;opacity:.75}.sidebar-toggle{display:none}.learn-grid{display:grid;grid-template-columns:1fr 1fr;gap:20px}.learn-grid .app-card{margin-bottom:0}.learn-grid{margin-bottom:20px}.app-card p:last-child{margin-bottom:0}.responsive-table{overflow-x:auto}.plot-caption{font-size:12px;color:var(--muted);margin-top:7px}.sim-stat-label{font-weight:700}.badge-note{font-size:12px;color:var(--muted);margin-bottom:12px}.btn-row{display:flex;gap:10px;flex-wrap:wrap;align-items:center}
.metric-formula{font-family:Georgia,serif;font-size:15px;color:var(--teal);margin-top:12px;overflow-wrap:anywhere}.math-term{display:inline-block;white-space:nowrap}.metric-key{font-size:12px;color:var(--muted);margin:-8px 0 20px}.distribution-formula{background:#f0f7f3;border-radius:10px;padding:12px 15px;margin-top:16px}.distribution-formula .formula{font-size:20px}.distribution-formula p{margin:4px 0 0;font-size:12px;color:var(--muted)}
@media(min-width:1550px){.app-layout{gap:34px}}@media(max-width:1120px){.app-header{padding:24px}.app-layout{padding:0 24px 30px;grid-template-columns:230px minmax(0,1fr);gap:20px}.sidebar{padding:20px 17px}.metrics{gap:8px}.metric{padding:14px 10px}.metric-value{font-size:23px}.two-col{grid-template-columns:1fr}.app-card{padding:20px}.intro h1{font-size:30px}.learn-grid{grid-template-columns:1fr}}
@media(max-width:780px){.app-header{padding:20px 16px}.header-tag{display:none}.app-layout{display:flex;flex-direction:column;padding:0 16px 25px;gap:17px}.sidebar{position:static;padding:17px 18px;width:100%}.sidebar-toggle{display:block;width:100%;text-align:left;border:0;background:transparent;padding:0;font-weight:700;color:var(--teal)}.sidebar h2{display:none}.sidebar-inner{display:none;padding-top:18px}.sidebar.is-open .sidebar-inner{display:block}.sidebar .convention{margin-top:15px}.intro{padding-top:0}.intro h1{font-size:29px}.intro p{font-size:13px}.nav-tabs{max-width:none}.nav-tabs>li>a{padding:9px 4px;font-size:12px}.metrics{grid-template-columns:repeat(2,minmax(0,1fr));gap:9px}.metric-value{font-size:25px}.app-card{padding:18px 14px;border-radius:14px}.chart-top{align-items:start;flex-direction:column;gap:8px}.chart-top .radio-inline{margin-left:0;margin-right:12px}.compare-grid{grid-template-columns:repeat(2,minmax(0,1fr))}.equal-cards{grid-template-columns:1fr}.equal-cards .result-panel{min-height:auto}.equal-sign{line-height:1}.calc-controls{gap:10px}.footer{flex-direction:column;gap:3px}.control-row{gap:10px}.brand-icon{width:40px;height:40px;padding:9px}.brand-name{font-size:11px}}
@media(max-width:780px){#compare_plot{height:880px!important}.control-row>div{flex-basis:100%;min-width:0}}
.btn.btn-primary,.btn.btn-primary:focus,.btn.btn-primary:active{background:var(--teal);border-color:var(--teal);color:#fff}.btn.btn-primary:hover{background:#0e5f56;border-color:#0e5f56;color:#fff}
@media(prefers-reduced-motion:reduce){*,*:before,*:after{animation:none!important;transition:none!important}}
"
js <- "$(document).on('click','#sidebar-toggle',function(){var side=document.getElementById('sidebar');var open=side.classList.toggle('is-open');this.setAttribute('aria-expanded',open?'true':'false');});"

# 3. User interface ------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$title("Geometric Explorer"),
            tags$link(rel="icon", href="data:image/svg+xml,%3Csvg xmlns=%27http://www.w3.org/2000/svg%27 viewBox=%270 0 32 32%27%3E%3Crect width=%2732%27 height=%2732%27 rx=%278%27 fill=%27%2313776c%27/%3E%3Cpath d=%27M8 8v16M14 13v11M20 17v7M26 21v3%27 stroke=%27white%27 stroke-width=%272%27/%3E%3C/svg%3E"),
            tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
            tags$style(HTML(css)), tags$script(HTML(js))),
  div(class="app-header",
      div(class="brand-lockup",
          div(class="brand-icon", `aria-hidden`="true",
              tags$i(style="height:25px"), tags$i(style="height:19px"),
              tags$i(style="height:12px"), tags$i(style="height:7px")),
          div(div(class="brand-name", "DISTRIBUTION LAB"),
              div(class="brand-sub", "STA 632 / Project 1"))),
      div(class="header-tag", "Independent trials. One first success.")),
  div(class="app-layout",
      tags$aside(id="sidebar",class="sidebar", `aria-label`="Distribution controls",
                 tags$button(id="sidebar-toggle",class="sidebar-toggle",type="button",
                             `aria-expanded`="false", `aria-controls`="sidebar-inner",
                             "Adjust probability & range +"),
                 h2("Set your experiment"),
                 div(id="sidebar-inner",class="sidebar-inner",
                     div(class="eyebrow", "Success on each trial"),
                     sliderInput("p_slider", "Success probability (p)", min=MIN_P,
                                 max=1, value=0.2, step=0.000001, ticks=FALSE),
                     numericInput("prob", "Exact probability", value=0.2, min=MIN_P, max=1, step=0.01),
                     p(class="small-note", "Theory: 0 < p \u2264 1. This explorer supports 0.0001 to 1."),
                     div(class="small-note", textOutput("prob_note")),
                     hr(), div(class="eyebrow", "Try a familiar example"),
                     div(class="preset-grid", actionButton("preset_report","20% success"),
                         actionButton("preset_coin","Fair coin"), actionButton("preset_die","Roll a six")),
                     hr(), div(class="eyebrow", "Explore plot range"),
                     checkboxInput("auto_range", "Choose the range automatically", TRUE),
                     conditionalPanel("!input.auto_range",
                                      numericInput("xmin","First failure count",0,min=0,max=1e6,step=1),
                                      numericInput("xmax","Last failure count",20,min=0,max=1e6,step=1),
                                      numericInput("tick_every","Tick every how many discrete values?",1,min=1,max=1e6,step=1),
                                      p(class="small-note","Use a positive whole number. Ticks start at the first failure count.")),
                     p(class="small-note", "Auto aims to show 99.5% of probability. Plots show at most 2,001 counts; any omitted probability is reported."),
                     actionButton("reset","Reset all controls",class="reset-btn")),
                 div(class="convention", strong("We count failures."),
                     "Success on the first trial means X = 0. Total trials = X + 1.")),
      tags$main(class="main",
                div(class="intro", div(class="eyebrow", "The geometric distribution"),
                    h1("How many failures until the first success?"),
                    p("Change the chance of success. See the wait take shape.")),
                tabsetPanel(id="page", type="tabs",
                            tabPanel("Explore", value="explore",
                                     uiOutput("metrics"),
                                     card(div(class="chart-top",
                                              div(h3("See the distribution"),
                                                  div(class="chart-sub", textOutput("chart_subtitle"))),
                                              radioButtons("plot_kind",NULL,
                                                           choices=c("Probability (PMF)"="pmf", "Cumulative (CDF)"="cdf"),
                                                           selected="pmf",inline=TRUE)),
                                          uiOutput("distribution_formula"),
                                          div(class="chart-output", plotOutput("dist_plot",height="340px",
                                                                               hover=hoverOpts("plot_hover",delay=80,delayType="debounce",nullOutside=TRUE))),
                                          div(class="hover-readout",textOutput("hover_text")),
                                          div(class="chart-foot",
                                              div(span(class="legend-dot"),"Distribution",
                                                  span(class="legend-dot orange"),"Selected outcomes (PMF)"),
                                              textOutput("range_note"))),
                                     div(class="two-col",
                                         card(h3("Ask a probability question"),
                                              p(class="small-note", "Select a question; the orange points show the matching outcomes."),
                                              radioButtons("calc_mode",NULL,c("Probability"="probability","Percentile"="percentile"),inline=TRUE),
                                              conditionalPanel("input.calc_mode == 'probability'",
                                                               selectInput("event_kind","What would you like to know?",
                                                                           c("Exactly k failures"="exact","At most k failures"="atmost",
                                                                             "More than k failures"="more","Between a and b failures"="between"),selected="atmost"),
                                                               conditionalPanel("input.event_kind != 'between'",numericInput("k","Failure count (k)",3,min=0,max=1e9,step=1)),
                                                               conditionalPanel("input.event_kind == 'between'",div(class="calc-controls",
                                                                                                                    numericInput("a","From (a), included",2,min=0,max=1e9,step=1),
                                                                                                                    numericInput("b","To (b), included",5,min=0,max=1e9,step=1)))),
                                              conditionalPanel("input.calc_mode == 'percentile'",
                                                               numericInput("percentile","Target cumulative probability (%)",90,min=.01,max=99.99,step=1)),
                                              uiOutput("calculation")),
                                         card(div(class="eyebrow","Make sense of the shape"),h3("A higher chance. A shorter wait."),
                                              uiOutput("shape_story"),
                                              tags$details(tags$summary("The formulas behind the numbers"),
                                                           div(class="formula", "P(X = x) = p(1 \u2212 p)",tags$sup("x")),
                                                           p("x is a whole-number failure count; p is the success probability on each trial."),
                                                           p("Mean failures = (1 \u2212 p) / p. Variance = (1 \u2212 p) / p\u00b2. Standard deviation is the square root of variance."),
                                                           p("For integer x \u2265 0, the CDF is P(X \u2264 x) = 1 \u2212 (1 \u2212 p)^(x + 1).")),
                                              div(class="notice","Try 20% success, then a fair coin. Watch the mass move toward zero.")))),
                            tabPanel("Compare",value="compare",
                                     section_head("Put them side by side","Same axes. Different chances.",
                                                  "Compare two to four success probabilities without changing the scale between panels."),
                                     card(div(class="control-row",
                                              selectInput("compare_n","Number of distributions",c(2,3,4),selected=4),
                                              radioButtons("compare_kind","Plot",c("PMF"="pmf","CDF"="cdf"),inline=TRUE)),
                                          div(class="compare-grid",
                                              numericInput("cp1","Distribution 1: p",.2,min=MIN_P,max=1,step=.05),
                                              numericInput("cp2","Distribution 2: p",.4,min=MIN_P,max=1,step=.05),
                                              conditionalPanel("input.compare_n >= 3",numericInput("cp3","Distribution 3: p",.6,min=MIN_P,max=1,step=.05)),
                                              conditionalPanel("input.compare_n >= 4",numericInput("cp4","Distribution 4: p",.8,min=MIN_P,max=1,step=.05))),
                                          checkboxInput("compare_auto","Choose a common range automatically",TRUE),
                                          conditionalPanel("!input.compare_auto",numericInput("compare_max","Largest failure count shown",25,min=1,max=2000,step=1)),
                                          p(class="small-note","These comparison controls are independent of the sidebar probability.")),
                                     card(plotOutput("compare_plot",height="450px"),
                                          p(class="plot-caption",textOutput("compare_note")),
                                          div(class="responsive-table",tableOutput("compare_table")))),
                            tabPanel("Simulate",value="simulate",
                                     section_head("From chance to observations","Run the experiment.",
                                                  "A sequence ends at its first success. Repeat it to see the distribution emerge."),
                                     div(class="badge-note",textOutput("simulation_probability")),
                                     card(div(class="chart-top",
                                              div(h3("One sequence of trials"),p(class="small-note","F = failure. S = success. Count only the failures.")),
                                              actionButton("run_one","Run one sequence",class="btn-primary")),
                                          checkboxInput("animate","Animate new sequences",TRUE),uiOutput("single_sequence")),
                                     card(h3("Repeat the experiment"),
                                          div(class="control-row",
                                              numericInput("sample_n","Number of sequences",1000,min=10,max=50000,step=100),
                                              div(checkboxInput("fixed_seed","Use a repeatable sample",TRUE),
                                                  conditionalPanel("input.fixed_seed",numericInput("seed","Random seed",632,min=0,max=2147483647,step=1))),
                                              div(actionButton("run_sample","Generate sample",class="btn-primary"))),
                                          uiOutput("sample_status"),
                                          conditionalPanel("output.has_sample",plotOutput("sample_plot",height="340px"),
                                                           p(class="plot-caption","Teal bars: observed proportions. Orange points: theoretical probabilities."),
                                                           div(class="responsive-table",tableOutput("sample_table")),
                                                           p(class="small-note",textOutput("sample_tail"))),
                                          conditionalPanel("!output.has_sample",div(class="empty-state",h3("Your sample will appear here"),
                                                                                    p("Start with 1,000 sequences, then try a larger sample."))))),
                            tabPanel("Understand",value="understand",
                                     section_head("The ideas behind the plot","What does \u201cmemoryless\u201d mean?",
                                                  "Past failures do not change the chance of success on the next independent trial."),
                                     card(div(class="control-row",
                                              numericInput("past","Failures already observed (m)",3,min=0,max=50,step=1),
                                              numericInput("future","Additional failures to consider (n)",4,min=0,max=50,step=1)),
                                          uiOutput("memory_diagram"),uiOutput("memory_results"),
                                          tags$details(tags$summary("See the conditional-probability explanation"),
                                                       p("X counts failures before the first success. m is the number already observed; n is an additional failure threshold. Both are whole numbers. The vertical bar means 'given that'."),
                                                       div(class="formula small","P(X \u2265 m + n | X \u2265 m) = P(X \u2265 n)"),
                                                       p("For 0 < p < 1, the probability of k initial failures is (1 \u2212 p)^k. Divide the chance of m + n failures by the chance of m failures:"),
                                                       div(class="formula small","(1 \u2212 p)",tags$sup("m+n")," / (1 \u2212 p)",tags$sup("m")," = (1 \u2212 p)",tags$sup("n")),
                                                       p("We use \u2265 because X counts failures from zero. If p = 1 and m > 0, the conditioning event is impossible."))),
                                     div(class="learn-grid",
                                         card(div(class="eyebrow","Start with Bernoulli trials"),h3("Two outcomes, the same chance."),
                                              tags$ul(class="definition-list",tags$li("Each trial has two outcomes: success or failure."),
                                                      tags$li("Trials are independent."),tags$li("The success probability p stays the same.")),
                                              p(class="small-note","Rolling a fair die until the first six is one example: p = 1/6. If the chance changes after each trial, this geometric model does not apply.")),
                                         card(div(class="eyebrow","Two ways to count"),h3("Failures versus total trials"),
                                              trial_strip(2),p("F, F, S means 2 failures and 3 total trials."),
                                              div(class="formula","T = X + 1"),
                                              p(class="small-note","X counts failures; T counts all trials including the success. This app uses X, just like R. Mean total trials = 1 / p."))),
                                     card(div(class="eyebrow","A related distribution"),h3("The first success is a special case."),
                                          p("The negative binomial distribution counts failures before a chosen number of successes, r. The geometric distribution stops at just one success, so r = 1."),
                                          div(class="formula small","Geometric(p) = Negative binomial(size = 1, prob = p)"),
                                          p(class="small-note","Here r is a positive integer target. A binomial model instead fixes the number of trials and counts successes."),
                                          tags$details(tags$summary("Check it in R"),tags$pre("dgeom(3, prob = 0.20)\ndnbinom(3, size = 1, prob = 0.20)\n# Both give 0.1024"))))
                ),
                div(class="footer",span("Geometric Explorer \u00b7 Failures before first success"),
                    span(tags$a(href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Geometric.html",target="_blank",rel="noopener","R geometric reference"),
                         " / ",tags$a(href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/NegBinomial.html",target="_blank",rel="noopener","Negative binomial"))))
  )
)

# 4. Server: inputs, calculations, and plots ------------------------------
server <- function(input, output, session) {
  probability <- reactive({
    validate(need(valid_p(input$prob), "Enter a success probability from 0.0001 through 1."))
    input$prob
  })
  output$prob_note <- renderText({
    if (!valid_p(input$prob)) "Please enter a number from 0.0001 through 1."
    else paste0("Each trial has a ", pct(input$prob), " chance of success.")
  })
  observeEvent(input$p_slider, {
    if (valid_p(input$p_slider) && (!valid_p(input$prob) || abs(input$p_slider-input$prob)>0.00000051))
      updateNumericInput(session,"prob",value=input$p_slider)
  },ignoreInit=TRUE)
  observeEvent(input$prob, {
    if (valid_p(input$prob)) updateSliderInput(session,"p_slider",value=input$prob)
  },ignoreInit=TRUE)
  observeEvent(input$preset_report, updateNumericInput(session,"prob",value=.2))
  observeEvent(input$preset_coin, updateNumericInput(session,"prob",value=.5))
  observeEvent(input$preset_die, updateNumericInput(session,"prob",value=1/6))
  
  visible_range <- reactive({
    if (isTRUE(input$auto_range)) return(c(0,auto_max(probability())))
    validate(need(whole(input$xmin,1e6) && whole(input$xmax,1e6),"Use whole-number range limits from 0 to 1,000,000."),
             need(input$xmax>=input$xmin,"The last count must be at least the first count."),
             need(input$xmax-input$xmin<MAX_POINTS,"Show no more than 2,001 counts at a time."))
    c(input$xmin,input$xmax)
  })
  calculation <- reactive({
    prob <- probability()
    if (identical(input$calc_mode,"percentile")) {
      u <- input$percentile
      validate(need(is.numeric(u) && length(u)==1 && is.finite(u) && u>0 && u<100,
                    "Choose a target greater than 0% and less than 100%."))
      q <- qgeom(u/100,prob)
      return(list(kind="percentile",q=q,u=u,value=pgeom(q,prob)))
    }
    kind <- input$event_kind
    req(kind)
    if (kind=="between") {
      validate(need(whole(input$a) && whole(input$b),"Enter whole-number bounds from 0 to 1 billion."),
               need(input$a<=input$b,"The upper bound must be at least the lower bound."))
    } else validate(need(whole(input$k),"Enter a whole failure count from 0 to 1 billion."))
    list(kind=kind,k=input$k,a=input$a,b=input$b,
         value=event_prob(kind,input$k,input$a,input$b,prob))
  })
  output$metrics <- renderUI({
    prob <- probability()
    tagList(div(class="metrics",
                metric("Mean failures",num((1-prob)/prob),"before the first success",
                       tagList("E[X] = ",span(class="math-term","(1 \u2212 p) / p"))),
                metric("Variance",num((1-prob)/prob^2),"spread in failures\u00b2",
                       tagList("Var(X) = ",span(class="math-term","(1 \u2212 p) / p\u00b2"))),
                metric("Standard deviation",num(sqrt(1-prob)/prob),"spread in failures",
                       tagList("SD(X) = ",span(class="math-term","\u221a(1 \u2212 p) / p"))),
                metric("Mean total trials",num(1/prob),"including the success",
                       "E[T] = 1 / p")),
            p(class="metric-key","p is the success probability per trial; X counts failures; T = X + 1 counts total trials. E is the mean, Var is variance, and SD is standard deviation."))
  })
  output$chart_subtitle <- renderText({paste0("p = ",p_label(probability()),"  \u00b7  X counts failures before the first success")})
  output$distribution_formula <- renderUI({
    prob <- probability()
    formula <- if (identical(input$plot_kind,"cdf")) {
      tagList("F(x) = P(X \u2264 x) = 1 \u2212 (1 \u2212 p)",tags$sup("x + 1"))
    } else tagList("P(X = x) = p(1 \u2212 p)",tags$sup("x"))
    div(class="distribution-formula",
        div(class="formula",formula),
        p("X counts failures before the first success; x = 0, 1, 2, \u2026 is a possible count; p is the success probability per trial."),
        if (identical(input$plot_kind,"cdf")) p("F(x) is the chance of at most x failures. The formula above is for whole-number x; the CDF stays flat between counts."),
        if (prob == 1 && identical(input$plot_kind,"pmf"))
          p("At p = 1, P(X = 0) = 1 and all positive failure counts have probability 0."))
  })
  tick_spacing <- reactive({
    bounds <- visible_range()
    if (isTRUE(input$auto_range))
      return(auto_tick_step(bounds,session$clientData$output_dist_plot_width))
    validate(need(whole(input$tick_every,1e6) && input$tick_every >= 1,
                  "Tick spacing must be a whole number from 1 to 1,000,000."))
    input$tick_every
  })
  plot_ticks <- reactive({
    bounds <- visible_range()
    seq(bounds[1],bounds[2],by=tick_spacing())
  })
  plot_data <- reactive({
    bounds <- visible_range(); x <- seq(bounds[1],bounds[2]); c <- calculation()
    selected <- if(c$kind=="percentile") x<=c$q else event_mask(x,c$kind,c$k,c$a,c$b)
    data.frame(x=x,pmf=dgeom(x,probability()),cdf=pgeom(x,probability()),selected=selected)
  })
  output$dist_plot <- renderPlot({
    d <- plot_data(); bounds <- visible_range()
    x_axis <- scale_x_continuous(breaks=plot_ticks(),minor_breaks=NULL,
                                 labels=function(x) format(x,trim=TRUE,scientific=FALSE))
    tick_theme <- theme(axis.ticks.x=element_line(colour=ink),
                        axis.ticks.length=grid::unit(3,"pt"))
    if (input$plot_kind=="cdf") {
      # Include a point on either side to show the first and last steps.
      cx <- seq(bounds[1]-1,bounds[2]+1)
      cd <- data.frame(x=cx,y=pgeom(cx,probability()))
      ggplot(cd,aes(x=x,y=y)) + geom_step(colour=teal,linewidth=.8) +
        geom_point(data=d,aes(x=x,y=cdf),colour=teal,size=if(nrow(d)<100) 2 else .6) +
        coord_cartesian(xlim=c(bounds[1]-.5,bounds[2]+.5),ylim=c(0,1.02),expand=FALSE) +
        labs(x="Failures before first success",y="Cumulative probability  P(X \u2264 x)") +
        plot_theme() + x_axis + tick_theme
    } else {
      ggplot(d,aes(x=x,y=pmf,colour=selected)) +
        geom_segment(aes(xend=x,yend=0),linewidth=.7) +
        geom_point(size=if(nrow(d)<100) 2.6 else .7) +
        scale_colour_manual(values=c("FALSE"=teal,"TRUE"=orange),guide="none") +
        scale_y_continuous(limits=c(0,NA),expand=expansion(mult=c(.01,.1))) +
        coord_cartesian(xlim=c(bounds[1]-.5,bounds[2]+.5),expand=FALSE) +
        labs(x="Failures before first success",y="Probability  P(X = x)") +
        plot_theme() + x_axis + tick_theme
    }
  },res=110,alt="Geometric distribution plot. The summary and probability calculator provide numerical values.")
  output$hover_text <- renderText({
    h <- input$plot_hover
    if (is.null(h$x)) return("Hover over the plot to inspect a value.")
    b <- visible_range()
    x <- if(input$plot_kind=="cdf") floor(h$x+1e-8) else round(h$x)
    if (x<b[1] || x>b[2]) return("Hover over the plot to inspect a value.")
    if (input$plot_kind=="cdf") paste0("At x = ",num(x,0),": P(X \u2264 x) = ",pct(pgeom(x,probability())))
    else paste0("At x = ",num(x,0)," failures: P(X = x) = ",pct(dgeom(x,probability())))
  })
  output$range_note <- renderText({
    b <- visible_range(); mass <- interval_prob(b[1],b[2],probability())
    paste0("Showing ",pct(mass)," of probability \u00b7 ",pct(1-mass)," outside this range")
  })
  output$calculation <- renderUI({
    c <- calculation(); prob <- probability()
    if(c$kind=="percentile") {
      return(div(class="result-panel",div(class="eyebrow",paste0(num(c$u,2),"th percentile")),
                 div(class="result-value",paste(num(c$q,0),"failures")),
                 p(strong(paste0(num(c$q+1,0)," total attempts"))," give a ",pct(c$value)," chance of seeing a success."),
                 p(class="small-note",paste0("With ",num(c$q,0)," attempts, the chance is ",pct(pgeom(c$q-1,prob)),
                                             ". A discrete CDF jumps, so the target may be exceeded."))))
    }
    description <- switch(c$kind,exact=paste("exactly",num(c$k,0),"failures"),
                          atmost=paste("at most",num(c$k,0),"failures"),more=paste("more than",num(c$k,0),"failures"),
                          between=paste("between",num(c$a,0),"and",num(c$b,0),"failures, including both ends"))
    notation <- switch(c$kind,exact=paste0("P(X = ",num(c$k,0),")"),
                       atmost=paste0("P(X \u2264 ",num(c$k,0),")"),more=paste0("P(X > ",num(c$k,0),")"),
                       between=paste0("P(",num(c$a,0)," \u2264 X \u2264 ",num(c$b,0),")"))
    div(class="result-panel",span(class="pill",notation),div(class="result-value",pct(c$value)),
        p("Chance of ",strong(description)," before the first success."),
        p(class="small-note","Calculated over the full distribution, even if some outcomes are off the plot."))
  })
  output$shape_story <- renderUI({
    prob <- probability()
    if (prob==1) return(tagList(p("Success is certain on the first trial. Every outcome is X = 0."),
                                p("The mean, variance, and standard deviation of failures are all zero.")))
    tagList(p("With ",strong(pct(prob))," success on each trial, expect about ",
              strong(paste(num((1-prob)/prob),"failures"))," before success."),
            p(class="small-note","Zero is always the most likely failure count. Smaller success probabilities produce longer waits and more spread; larger probabilities concentrate the mass near zero."),
            p(class="small-note","The PMF gives the probability of one count. The CDF adds the probabilities up through a count."))
  })
  
  # Compare: independent parameters, fixed scales across panels.
  compare_values <- reactive({
    n <- as.integer(input$compare_n); req(n)
    v <- sapply(seq_len(n),function(i) input[[paste0("cp",i)]])
    validate(need(length(v)==n && all(vapply(as.list(v),valid_p,logical(1))),
                  "Each comparison probability must be between 0.0001 and 1."))
    v
  })
  compare_limit <- reactive({
    if(isTRUE(input$compare_auto)) max(vapply(compare_values(),auto_max,numeric(1)))
    else {
      validate(need(whole(input$compare_max,2000) && input$compare_max>=1,"Choose a whole upper limit from 1 to 2,000."))
      input$compare_max
    }
  })
  output$compare_plot <- renderPlot({
    v <- compare_values(); top <- compare_limit()
    d <- do.call(rbind,lapply(seq_along(v),function(i) {
      x <- if(input$compare_kind=="cdf") -1:(top+1) else 0:top
      data.frame(x=x,y=if(input$compare_kind=="cdf") pgeom(x,v[i]) else dgeom(x,v[i]),
                 panel=paste0(i,"  |  p = ",p_label(v[i])))
    }))
    means <- data.frame(
      panel=paste0(seq_along(v),"  |  p = ",vapply(v,p_label,character(1))),
      mean=(1-v)/v)
    # Place each label next to its own mean line, keeping it inside the panel.
    means$label_x <- ifelse(means$mean <= .65*top,
                            means$mean+.02*(top+1),pmin(means$mean-.02*(top+1),top))
    means$label_hjust <- ifelse(means$mean <= .65*top,0,1)
    means$label <- paste0("Mean = ",vapply(means$mean,num,character(1)),
                          ifelse(means$mean > top+.5," (outside range)",""))
    width <- session$clientData$output_compare_plot_width
    cols <- if(!is.null(width) && width < 500) 1 else 2
    g <- ggplot(d,aes(x=x,y=y)) + facet_wrap(~panel,ncol=cols) + plot_theme() +
      labs(x="Failures before first success",y=if(input$compare_kind=="cdf") "Cumulative probability" else "Probability")
    if(input$compare_kind=="cdf") g <- g + geom_step(colour=teal,linewidth=.7) +
      coord_cartesian(xlim=c(-.5,top+.5),ylim=c(0,1.02),expand=FALSE)
    else g <- g + geom_segment(aes(xend=x,yend=0),colour=teal,linewidth=.6) +
      geom_point(colour=teal,size=if(top<100) 1.8 else .5) +
      scale_y_continuous(limits=c(0,NA),expand=expansion(mult=c(.02,.1))) +
      coord_cartesian(xlim=c(-.5,top+.5),ylim=c(0,max(d$y)*1.12),expand=FALSE)
    # Draw the mean last so it remains visible over stems and CDF steps.
    g + geom_vline(data=means,aes(xintercept=mean),colour="#E76F00",
                   linetype="solid",linewidth=1.25) +
      geom_label(data=means,aes(x=label_x,y=Inf,label=label,hjust=label_hjust),
                 inherit.aes=FALSE,vjust=1.2,colour="#AD4800",fill="#FFF3E4",
                 size=3.2,fontface="bold",label.size=0,
                 label.padding=grid::unit(.2,"lines"))
  },res=110,alt="Comparison of geometric distributions with identical axes. Solid orange vertical lines mark the mean failures in each panel; labels give their values.")
  output$compare_note <- renderText({
    paste0("All panels show the same range: 0 to ",num(compare_limit(),0),
           ". Solid orange lines mark mean failures for each distribution. ",
           if(any((1-compare_values())/compare_values() > compare_limit()+.5))
             "A mean outside the displayed range is labeled but its line is off the plot. " else "",
           "The table reports the probability visible in each panel.")
  })
  output$compare_table <- renderTable({
    v <- compare_values()
    data.frame(Distribution=seq_along(v),p=v,Mean=(1-v)/v,Variance=(1-v)/v^2,
               SD=sqrt(1-v)/v,`Visible probability`=vapply(pgeom(compare_limit(),v),pct,character(1)),check.names=FALSE)
  },digits=4,striped=FALSE,bordered=FALSE,spacing="s")
  
  # Simulate: samples only change when the relevant button is clicked.
  single <- reactiveVal(NULL)
  sample <- reactiveVal(NULL)
  output$simulation_probability <- renderText(paste0("New experiments use sidebar p = ",p_label(probability())," (",pct(probability())," success)."))
  observeEvent(input$run_one, {
    prob <- probability()
    single(list(x=draw_sample(1,prob),p=prob,run=input$run_one))
  })
  output$single_sequence <- renderUI({
    s <- single()
    if(is.null(s)) return(div(class="empty-state",h3("How many failures will come first?"),p("Run a sequence to find out.")))
    tagList(div(`data-run`=s$run,trial_strip(s$x,animate=isTRUE(input$animate))),
            p(strong(paste0(num(s$x,0)," failures"))," before success; ",strong(paste0(num(s$x+1,0)," total trials")),"."),
            p(class="small-note",paste0("This sequence used p = ",p_label(s$p),". ",
                                        if(s$p!=probability()) "Run again to use the new sidebar probability." else "Every new run can give a different wait.")))
  })
  observeEvent(input$run_sample, {
    prob <- probability()
    if(!whole(input$sample_n,50000) || input$sample_n<10) {
      showNotification("Choose a whole sample size from 10 to 50,000.",type="error"); return()
    }
    if(isTRUE(input$fixed_seed) && !whole(input$seed,2147483647)) {
      showNotification("Use a whole random seed from 0 to 2,147,483,647.",type="error"); return()
    }
    seed <- if(isTRUE(input$fixed_seed)) input$seed else NULL
    sample(list(x=draw_sample(input$sample_n,prob,seed),p=prob,n=input$sample_n,seed=seed))
  })
  output$has_sample <- reactive(!is.null(sample()))
  outputOptions(output,"has_sample",suspendWhenHidden=FALSE)
  output$sample_status <- renderUI({
    s <- sample(); if(is.null(s)) return(NULL)
    div(class=paste("notice",if(s$p!=probability()) "warning" else ""),
        paste0(num(s$n,0)," sequences generated with p = ",p_label(s$p),
               if(!is.null(s$seed)) paste0("; seed ",num(s$seed,0)) else "; random seed not fixed", ". ",
               if(s$p!=probability()) "This is the previous experiment. Generate a new sample to use the current p." else
                 "Changing controls does not replace this sample; click Generate sample when ready."))
  })
  sample_plot_data <- reactive({
    s <- sample(); req(s)
    top <- auto_max(s$p,cap=300)
    x <- 0:top
    counts <- tabulate(as.integer(s$x[s$x<=top])+1L,nbins=length(x))
    data.frame(x=x,observed=counts/s$n,theory=dgeom(x,s$p))
  })
  output$sample_plot <- renderPlot({
    d <- sample_plot_data()
    ggplot(d,aes(x=x)) + geom_col(aes(y=observed),fill="#83B6A9",width=.8) +
      geom_point(aes(y=theory),colour=orange,size=if(nrow(d)<100) 2 else 1) +
      labs(x="Failures before first success",y="Proportion / probability") + plot_theme()
  },res=110,alt="Simulated proportions as bars with exact geometric probabilities as points.")
  output$sample_table <- renderTable({
    s <- sample(); req(s)
    data.frame(Quantity=c("Mean","Variance","Standard deviation"),
               Theoretical=c((1-s$p)/s$p,(1-s$p)/s$p^2,sqrt(1-s$p)/s$p),
               Observed=c(mean(s$x),var(s$x),sd(s$x)))
  },digits=4,spacing="s")
  output$sample_tail <- renderText({
    s <- sample(); req(s); top <- max(sample_plot_data()$x)
    paste0("Counts shown: 0\u2013",num(top,0),". Observations outside this range: ",num(sum(s$x>top),0),
           "/",num(s$n,0),". Theoretical probability beyond the plot: ",pct(pgeom(top,s$p,lower.tail=FALSE)),
           ". Bars use the full sample size as their denominator.")
  })
  
  # Understand: conditional-probability calculation and counting examples.
  memory <- reactive({
    prob <- probability()
    validate(need(whole(input$past,50) && whole(input$future,50),"Choose whole counts from 0 to 50."))
    m <- input$past; n <- input$future
    list(p=prob,m=m,n=n,possible=!(prob==1 && m>0),chance=pgeom(n-1,prob,lower.tail=FALSE))
  })
  output$memory_diagram <- renderUI({
    z <- memory()
    tagList(div(class="sequence-label",paste0("Past: ",z$m," failures already observed")),
            trial_strip(z$m,success=FALSE,past=TRUE),
            div(class="sequence-label",paste0("Question: will the next ",z$n," trials also fail?")),
            trial_strip(z$n,success=FALSE))
  })
  output$memory_results <- renderUI({
    z <- memory()
    if(!z$possible) return(div(class="notice warning","With p = 1, the first trial always succeeds. Observing past failures is impossible, so this conditional probability is undefined. Set m = 0 or lower p."))
    tagList(div(class="equal-cards",
                div(class="result-panel",p("After the observed failures"),div(class="result-value",pct(z$chance)),
                    p(class="tiny",paste0("P(X \u2265 ",z$m+z$n," | X \u2265 ",z$m,")"))),
                div(class="equal-sign",`aria-hidden`="true","="),
                div(class="result-panel",p("From a fresh start"),div(class="result-value",pct(z$chance)),
                    p(class="tiny",paste0("P(X \u2265 ",z$n,")")))),
            div(class="notice",paste0("The same ",pct(z$chance)," chance of at least ",z$n,
                                      " additional failures. The next trial's success chance is still ",pct(z$p),".")))
  })
  observeEvent(input$reset, {
    updateNumericInput(session,"prob",value=.2)
    updateSliderInput(session,"p_slider",value=.2)
    updateCheckboxInput(session,"auto_range",value=TRUE)
    updateNumericInput(session,"xmin",value=0); updateNumericInput(session,"xmax",value=20)
    updateNumericInput(session,"tick_every",value=1)
    updateRadioButtons(session,"plot_kind",selected="pmf")
    updateRadioButtons(session,"calc_mode",selected="probability")
    updateSelectInput(session,"event_kind",selected="atmost")
    for(id in c("k","past")) updateNumericInput(session,id,value=3)
    updateNumericInput(session,"a",value=2); updateNumericInput(session,"b",value=5)
    updateNumericInput(session,"percentile",value=90); updateNumericInput(session,"future",value=4)
    updateSelectInput(session,"compare_n",selected=4)
    for(i in 1:4) updateNumericInput(session,paste0("cp",i),value=c(.2,.4,.6,.8)[i])
    updateCheckboxInput(session,"compare_auto",value=TRUE)
    updateNumericInput(session,"compare_max",value=25)
    updateRadioButtons(session,"compare_kind",selected="pmf")
    updateNumericInput(session,"sample_n",value=1000); updateNumericInput(session,"seed",value=632)
    updateCheckboxInput(session,"fixed_seed",value=TRUE); updateCheckboxInput(session,"animate",value=TRUE)
    single(NULL); sample(NULL)
    updateTabsetPanel(session,"page",selected="explore")
  })
}

# 5. Start the app --------------------------------------------------------
shinyApp(ui = ui, server = server)
