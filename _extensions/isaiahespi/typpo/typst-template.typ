
// My custom typpo-article typst template
// Everything above is inserted by Quarto

// Appendix function, use with YAML
// functions: [place, appendix]
// And wrap appendix in a {.appendix} div
#let appendix(content) = {
  pagebreak()
  set heading(numbering: none)
  // Reset counters
  // TODO: Programmatically reset all (callout) counters
  // TODO: Reset equation and other counters
  counter(heading).update(0)
  counter(figure.where(kind: "quarto-float-fig")).update(0)
  counter(figure.where(kind: "quarto-float-tbl")).update(0)
  counter(figure.where(kind: "quarto-float-lst")).update(0)
  counter(figure.where(kind: "quarto-callout-Note")).update(0)
  counter(figure.where(kind: "quarto-callout-Warning")).update(0)
  counter(figure.where(kind: "quarto-callout-Important")).update(0)
  counter(figure.where(kind: "quarto-callout-Tip")).update(0)
  counter(figure.where(kind: "quarto-callout-Caution")).update(0)

  // Figure & Table Numbering
  set figure(numbering: it => {
    [A.#it]
  })
  place(auto, scope: "parent", float: true, {
    set align(left)
    content
  })
}


#let typpo-article(
  title: none,
  subtitle: none,
  running-head: none,
  authors: (),
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  col-gutter: 2em,
  
  // layout settings
  margin: (x: 2.2cm, y: 2.6cm),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: ("STIX Two Text", "Spectral", "Crimson Text", "Garamond", "Palatino Linotype", "Wittgenstein", "Libertinus Serif"),
  mathfont: ("New Computer Modern Math"),
  monofont: ("Fira Code", "DejaVu Sans Mono"),
  fontsize: 11pt,
  linkcolor: "#800000",
  title-size: 1.5em,
  subtitle-size: 1.25em,
  
  // Structure settings
  sectionnumbering: "I.A.1",
  pagenumbering: "1",
  linenumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  
  leading: 0.65em,
  spacing: 1em,
  first-line-indent: 1.8em,
  all: true,
  // Bibliography settings (no effect if citeproc used)
  bibliography-title: "References",
  bibliographystyle: "chicago-author-date",
  doc,
) = {


  let runningauth = if authors == none {
    none
  } else if authors.len() == 2 {
    authors.map(author => author.last).join(" and ")
  } else if authors.len() < 5 {
    authors.map(author => author.last).join(", ", last: ", and ")
  } else {
    authors.first().last + " et al."
  }

  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
    number-align: right,
    columns: cols,
    header: context {
      let pg = counter(page).at(here()).first()
        if pg == 1 {
          return
        } else if (calc.odd(pg)) [
            #align(right, running-head)
        ] else [
            #align(left)[
              #text(runningauth)
            ]
        ]
      v(-8pt)
      line(length: 100%)
      }
  )
  set columns(gutter: col-gutter)
  
  // paragraph properties
  set par(
    justify: true,
    leading: leading,
    spacing: spacing,
    first-line-indent: (amount: first-line-indent, all: all)
    )
  set par.line(numbering: linenumbering)
  
  // Configure headings.
  set heading(numbering: sectionnumbering)
  show heading: it => context{
    let levels = counter(heading).get() // Use .get() inside context
    let deepest = if levels != () { levels.last() } else { 1 }
  
  set text(10pt, weight: "regular") // base heading size
  if it.level == 1 [
    // First level headings are centered, smallcaps, and bold.
      #set align(center)
      #set text({ 12pt }) // Smaller level 1 heading font size
      #show: smallcaps
      #set text(weight: "bold")
      #v(1.5em, weak: true) // Space before heading (adjust 1.5em)
      #if it.numbering != none {
        numbering("I.", deepest)
        h(0.2em, weak:true) // space after section number (adjust 0.5em)
      }
      #it.body
      #v(13.75pt, weak: true) // reduced spacing
    ] else if it.level == 2 [
      // Second level headings are run-ins.
      #set par(first-line-indent: 0em)
      #set text(size: 12pt, style: "italic", weight: "bold")
      #v(1.5em, weak: true)
      #it.body
      #v(12pt, weak: true) // space after 2nd level heading
    ] else [
      // Third level headings are also run-ins, but italic and with em-dash
      #if it.level == 3 [
        #set text(size: fontsize * 1.05, weight: "semibold", style: "italic")
        #it.body
      ]
    ]
  }
  
  // text properties of main text(e.g., font)  
  set text(
    lang: lang,
    region: region,
    font: font,
    size: fontsize
  )
  
  
  // math font and code font
  show math.equation: set text(font: mathfont)
  show raw: set text(font: monofont, weight: "medium")
  
  // link and cite colors
  show link: this => {
    if type(this.dest) != label {
      text(this, fill: rgb(linkcolor.replace("\\#", "#")))
    } else {
      text(this, fill: rgb("#0000CC"))
    }
  }
  
  show cite.where(form: "prose"): this => {
    text(this, fill: rgb("#800000"))
  }
  show ref: this => {
    text(this, fill: rgb("#800000"))
  }

  // Lists
  set list(
    marker: ([--], [-]),
    indent: 0.5em
    )
  set enum(numbering: "1.a.", indent: 0.5em)

  // Footnotes  
  set footnote.entry(indent: 0.7em, gap: 0.75em)
  
  // Customize Typst bibliography (no effect if `citeproc: true`)
  set bibliography(title: bibliography-title, style: bibliographystyle)
  show bibliography: set par(spacing: spacing, leading: leading)
  
  // Customize figure display
  show figure: f => { [#v(leading) #f #v(leading) ] }
  show figure.where(kind: "quarto-float-fig"): set block(width: 100%)
  show figure.caption: it => [
    #v(-1em)
    #align(left)[
      #block(inset: 1em)[
        #text(weight: "bold")[
          #it.supplement
          #context it.counter.display(it.numbering)
        ]
        #it.separator
        #it.body
      ]
    ]
  ]
  
  // metadata
  set document(
    title: if title != none { title } else { none },
    author: if authors != none { authors.map(a => str(a.name.text)) } else { () },
    description: abstract,
    date: auto
  )

  // start article content
  place(top, float: true, scope: "parent", clearance: 1.5em, {
    if title != none {
      align(center)[
        #block(width: 100%, above: 0em, below: 0em)[
          #text(weight: "bold", size: title-size)[#title]
        ]
      ]
    }
    if subtitle != none {
      align(center)[
        #block(width: 100%, above: 1em, below: 1em)[
          #text(weight: "bold", size: subtitle-size)[#subtitle]
          ]
      ]}
    
  // display the authors
    if authors != none {
      let count = authors.len()
      let ncols = calc.min(count, 3)
      grid(
        columns: (1fr,) * ncols,
        row-gutter: 1em,
        ..authors.map(author =>
            align(center, {
            text(weight: "bold", author.name)
            if "affiliation" in author [
              \ #emph(author.affiliation)
            ]
            if "email" in author [
              \ #link("mailto:" + "author.email")
            ]
            }))
          )
        }
  

    if date != none {
      align(center)[#block(inset: 1em)[
        #date
      ]]
    }
  
    if abstract != none {
      align(center)[#block(width: 85%)[
        #set text(size: 0.92em, style: "italic")
        #set par(
          justify: true,
          first-line-indent: 0em,
          leading: 0.65em
        )
        #h(1em) _Abstract_---#abstract
      ]]
    }
  
    if toc {
      let title = if toc_title == none {
        auto
      } else {
        toc_title
      }
      block(above: 0em, below: 2em)[
        #outline(
          title: toc_title,
          depth: toc_depth,
          indent: toc_indent
        );
      ]
    }
  }
)
  
  doc
}

// Remove gridlines from tables
#set table(
  inset: 6pt,
  stroke: none
)
