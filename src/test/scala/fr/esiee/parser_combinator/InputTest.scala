package fr.esiee.parser_combinator

import munit.FunSuite

class InputTest extends FunSuite {

  test("should get the next current character") {
    // GIVEN
    val input =
      Input(
        "Pénurie de carburant, en direct : la CGT appelle à la grève dans tout le secteur de l’énergie, TotalEnergies propose 6 % d’augmentation pour ses salariés français en 2023"
      )

    // WHEN
    val result: String = input.current(1)

    // THEN
    assertEquals(result, "P")
  }

  test("should get the next 3 characters") {
    // GIVEN
    val input =
      Input(
        "Pénurie de carburant, en direct : la CGT appelle à la grève dans tout le secteur de l’énergie, TotalEnergies propose 6 % d’augmentation pour ses salariés français en 2023"
      )

    // WHEN
    val result: String = input.current(3)

    // THEN
    assertEquals(result, "Pén")
  }

  test("should get the next current character at offset 2") {
    // GIVEN
    val input =
      Input(
        data = "Pénurie de carburant, en direct : la CGT appelle à la grève dans tout le secteur de l’énergie, TotalEnergies propose 6 % d’augmentation pour ses salariés français en 2023",
        offset = 2
      )

    // WHEN
    val result: String = input.current(1)

    // THEN
    assertEquals(result, "n")
  }

  test("should ??? if I try get the current character after the end of the string") {
    val input = Input("toto", 5)

    val result = input.current(1)

    assertEquals(result, ???)
  }

  //test("should ")

}
