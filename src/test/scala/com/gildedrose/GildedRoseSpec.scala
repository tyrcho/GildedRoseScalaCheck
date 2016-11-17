package com.gildedrose

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class GildedRoseSpec extends FlatSpec with Matchers with PropertyChecks {

  "a tavern" should "age simple products" in {
    forAll { (quality: Int, sellIn: Int) =>
      val item = new Item("foo", sellIn, quality)
      val app = new GildedRose(Array(item))
      app.updateQuality()

      whenever(quality > 0 && sellIn > 0) {
        app.items(0).quality shouldBe quality - 1
      }

    }
  }

  it should "age simple products past due date" in {
    forAll { (quality: Int, sellIn: Int) =>
      val item = new Item("foo", sellIn, quality)
      val app = new GildedRose(Array(item))
      app.updateQuality()

      whenever(sellIn <= 0 && quality > -100) {
        app.items(0).quality shouldBe quality - 2
      }
    }
  }
}