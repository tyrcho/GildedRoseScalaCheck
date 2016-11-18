package com.gildedrose

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

@RunWith(classOf[JUnitRunner])
class GildedRoseSpec extends FlatSpec with Matchers with PropertyChecks {

  "a tavern" should "age simple products before due date" in {
    forAll { i: Item =>
      whenever(i.sellIn > 0 && i.quality > 0) {
        verify(i, i.quality - 1, i.sellIn - 1)
      }
    }
  }

  it should "age simple products faster after due date" in {
    forAll { i: Item =>
      whenever(i.sellIn <= 0 && i.quality > 0) {
        verify(i, (i.quality - 2) max 0, i.sellIn - 1)
      }
    }
  }

  it should "not diminish quality below 0" in {
    forAll { i: Item =>
      whenever(i.quality <= 0) {
        verify(i, i.quality, i.sellIn - 1)
      }
    }
  }

  it should "not update Sulfuras" in {
    forAll(genItem(name = "Sulfuras, Hand of Ragnaros")) { r =>
      verify(r, r.quality, r.sellIn)
    }
  }

  it should "improve Aged Brie up to 50 quality" in {
    forAll(agedBrie) { b =>
      whenever(b.sellIn > 0 && b.quality < 50) {
        verify(b, b.quality + 1, b.sellIn - 1)
      }
    }
  }

  it should "improve Aged Brie up to 50 quality, faster after date" in {
    forAll(agedBrie) { b =>
      whenever(b.sellIn <= 0 && b.quality < 50) {
        verify(b, b.quality + 2 min 50, b.sellIn - 1)
      }
    }
  }

  it should "not improve Aged Brie above 50 quality" in {
    forAll(agedBrie) { b =>
      whenever(b.quality >= 50) {
        verify(b, b.quality, b.sellIn - 1)
      }
    }
  }

  it should "improve backstage passes" in {
    forAll(backstagePass) { p =>
      whenever(p.quality <= 50 && p.sellIn > 10) {
        verify(p, p.quality + 1 min 50, p.sellIn - 1)
      }
    }
  }

  it should "not improve backstage passes past 50 quality" in {
    forAll(backstagePass) { p =>
      whenever(p.quality >= 50 && p.sellIn <= 10 && p.sellIn > 0) {
        verify(p, p.quality, p.sellIn - 1)
      }
    }
  }

  it should "destroy backstage passes past due date" in {
    forAll(backstagePass) { p =>
      whenever(p.sellIn <= 0) {
        verify(p, 0, p.sellIn - 1)
      }
    }
  }

  it should "improve backstage passes faster 10 days before" in {
    forAll(backstagePass) { p =>
      whenever(p.quality <= 50 && p.sellIn <= 10 && p.sellIn > 5) {
        verify(p, p.quality + 2 min 50, p.sellIn - 1)
      }
    }
  }

  it should "improve backstage passes even faster 5 days before" in {
    forAll(backstagePass) { p =>
      whenever(p.quality <= 50 && p.sellIn <= 5 && p.sellIn > 0) {
        verify(p, p.quality + 3 min 50, p.sellIn - 1)
      }
    }
  }

  def genItem(name: String): Gen[Item] =
    for {
      quality <- Gen.chooseNum(minT = -60, maxT = 60, 50, 49, 51)
      sellIn <- Gen.oneOf(Gen.chooseNum(-60, 60), Gen.chooseNum(2, 12))
    } yield new Item(name, sellIn, quality)

  def agedBrie = genItem(name = "Aged Brie")
  def backstagePass = genItem(name = "Backstage passes to a TAFKAL80ETC concert")

  implicit val arbitraryItem = Arbitrary(genItem("foo"))

  def update(i: Item): Item = {
    val copy = new Item(i.name, i.sellIn, i.quality)
    val app = new GildedRose(Array(copy))
    app.updateQuality()
    app.items(0)
  }

  def verify(i: Item, quality: Int, sellIn: Int) = {
    val u = update(i)
    u.quality shouldBe quality
    u.sellIn shouldBe sellIn
  }

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 100,
    maxDiscardedFactor = 15)

}