package com.gildedrose

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

@RunWith(classOf[JUnitRunner])
class GildedRoseSpec extends FlatSpec with Matchers with PropertyChecks {

  def genItem(name: String = "foo", quality: Range = 1 to 90, sellIn: Range = 1 to 20): Gen[Item] =
    for {
      q <- Gen.chooseNum(quality.min, quality.max)
      s <- Gen.chooseNum(sellIn.min, sellIn.max)
    } yield new Item(name, s, q)

  def update(i: Item): Item = {
    val copy = new Item(i.name, i.sellIn, i.quality)
    val app = new GildedRose(Array(copy))
    app.updateQuality()
    app.items(0)
  }

  def expect(u: Item, quality: Int, sellIn: Int) = {
    u.quality shouldBe quality
    u.sellIn shouldBe sellIn
  }

  "a tavern" should "age simple products" in {
    forAll(genItem()) { item =>
      val u = update(item)
      expect(u, item.quality - 1, item.sellIn - 1)
    }
  }

  it should "age simple products faster after due date" in {
    forAll(genItem(sellIn = -10 to 0)) { item =>
      val u = update(item)
      expect(u, (item.quality - 2) max 0, item.sellIn - 1)
    }
  }

  it should "update simple products with non positive quality" in {
    forAll(genItem(sellIn = -100 to 100, quality = -100 to 0)) { item =>
      val u = update(item)
      expect(u, item.quality, item.sellIn - 1)
    }
  }

  it should "not update Sulfuras" in {
    forAll(genItem(name = "Sulfuras, Hand of Ragnaros", sellIn = -100 to 100, quality = -100 to 100)) { item =>
      val u = update(item)
      expect(u, item.quality, item.sellIn)
    }
  }

  it should "improve Aged Brie up to 50 quality" in {
    forAll(genItem(name = "Aged Brie", sellIn = 1 to 100, quality = -100 to 49)) { item =>
      val u = update(item)
      expect(u, item.quality + 1, item.sellIn - 1)
    }
  }

  it should "improve Aged Brie up to 50 quality, faster after date" in {
    forAll(genItem(name = "Aged Brie", sellIn = -100 to 0, quality = -100 to 49)) { item =>
      val u = update(item)
      expect(u, item.quality + 2 min 50, item.sellIn - 1)
    }
  }

  it should "leave Aged Brie after 50 quality" in {
    forAll(genItem(name = "Aged Brie", sellIn = -100 to 100, quality = 50 to 100)) { item =>
      val u = update(item)
      expect(u, item.quality, item.sellIn - 1)
    }
  }

  it should "improve backstage passes" in {
    forAll(genItem(name = "Backstage passes to a TAFKAL80ETC concert", sellIn = 11 to 100, quality = -100 to 49)) { item =>
      val u = update(item)
      expect(u, item.quality + 1, item.sellIn - 1)
    }
  }

  it should "not improve backstage passes past 50 quality" in {
    forAll(genItem(name = "Backstage passes to a TAFKAL80ETC concert", sellIn = 1 to 10, quality = 50 to 100)) { item =>
      val u = update(item)
      expect(u, item.quality, item.sellIn - 1)
    }
  }

  it should "destroy backstage passes past due date" in {
    forAll(genItem(name = "Backstage passes to a TAFKAL80ETC concert", sellIn = -10 to 0, quality = -100 to 100)) { item =>
      val u = update(item)
      expect(u, 0, item.sellIn - 1)
    }
  }

  it should "improve backstage passes faster 10 days before" in {
    forAll(genItem(name = "Backstage passes to a TAFKAL80ETC concert", sellIn = 6 to 10, quality = -100 to 48)) { item =>
      val u = update(item)
      expect(u, item.quality + 2, item.sellIn - 1)
    }
  }

  it should "improve backstage passes even faster 4 days before" in {
    forAll(genItem(name = "Backstage passes to a TAFKAL80ETC concert", sellIn = 1 to 5, quality = -100 to 47)) { item =>
      val u = update(item)
      expect(u, item.quality + 3, item.sellIn - 1)
    }
  }

}