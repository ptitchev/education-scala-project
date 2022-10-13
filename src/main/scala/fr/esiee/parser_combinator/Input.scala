package fr.esiee.parser_combinator

// fr.esiee.parser_combinator.Input of a parser

// * data: Pénurie de carburant, en direct : la CGT appelle à la grève dans tout le secteur de l’énergie,
// TotalEnergies propose 6 % d’augmentation pour ses salariés français en 2023


// * offset:
case class Input(data: String, offset: Int = 0):
  def current(n: Int): String = if (offset > data.length) "" else data.substring(offset, offset+n)

  def next(n: Int): Input = ???

  def remaining: String = ???