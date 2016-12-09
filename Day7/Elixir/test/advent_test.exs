defmodule AdventTest do
  use ExUnit.Case
  doctest Advent

  test "can check if abba" do
    assert (Advent.isABBA?("abba") === true)
    assert (Advent.isABBA?("bddb") === true)
    assert (Advent.isABBA?("ioxxoj") === true)
    assert (Advent.isABBA?("acba") === false)
    assert (Advent.isABBA?("aaaa") === false)
    assert (Advent.isABBA?("zxcvbn") === false)
    assert (Advent.isABBA?("cjkxbnylglnlfleelzu") === true)
  end

  test "can get aba" do
    assert (Advent.getABA("aabaa") === [{"a", "b", "a"}])
    assert (Advent.getABA("zazbz") === [{"z", "b", "z"}, {"z", "a", "z"}])
    assert (Advent.getABA("zaqbz") === [])
  end

  test "can get get outer and inner" do
    assert (Advent.getOuterInner(
      "wysextplwqpvipxdv[srzvtwbfzqtspxnethm]syqbzgtboxxzpwr[kljvjjkjyojzrstfgrw]obdhcczonzvbfby[svotajtpttohxsh]cooktbyumlpxostt"
    ) === {
      ["cooktbyumlpxostt", "obdhcczonzvbfby", "syqbzgtboxxzpwr", "wysextplwqpvipxdv"],
      ["svotajtpttohxsh", "kljvjjkjyojzrstfgrw", "srzvtwbfzqtspxnethm"],
    })
  end

  test "can check if supports ssl" do
    assert (Advent.ssl?("aba[bab]xyz") === true)
    assert (Advent.ssl?("aaa[kek]eke") === true)
    assert (Advent.ssl?("zazbz[bzb]cdb") === true)
    assert (Advent.ssl?("aba[bqb]xyz") === false)
    assert (Advent.ssl?("xyx[xyx]xyx") === false)
  end

  test "can check if supports tls" do
    assert (Advent.tls?("abba[mnop]qrst") === true)
    assert (Advent.tls?("abba[mnop]qrst[abba]") === false)
    assert (Advent.tls?("qrst[mnop]qrst[qrst]") === false)
    assert (Advent.tls?("ioxxojasfdasdfasdf[asdfghasdfasdf]zxcvbnasdfasdf") === true)
    assert (Advent.tls?("abcd[bddb]xyyx") === false)
    assert (Advent.tls?("aaaa[qwer]tyui") === false)
    assert (Advent.tls?("wysextplwqpvipxdv[srzvtwbfzqtspxnethm]syqbzgtboxxzpwr[kljvjjkjyojzrstfgrw]obdhcczonzvbfby[svotajtpttohxsh]cooktbyumlpxostt") === false)
    assert (Advent.tls?("emzopymywhhxulxuctj[dwwvkzhoigmbmnf]nxgbgfwqvrypqxppyq[qozsihnhpztcrpbdc]rnhnakmrdcowatw[rhvchmzmyfxlolwe]uysecbspabtauvmixa") === false)
    assert (Advent.tls?("bqooxxweoytjghrqn[hkwwukixothfyglw]kpasnmikmbzcbfi[vlnyszifsaaicagxtqf]ucdyxasusefuuxl") === false)
    assert (Advent.tls?("rxpusykufgqujfe[rypwoorxdemxffui]cvvcufcqmxoxcphp[witynplrfvquduiot]vcysdcsowcxhphp[gctucefriclxaonpwe]jdprpdvpeumrhokrcjt") === true)
    assert (Advent.tls?("iungssgfnnjlgdferc[xfffplonmzjmxkinhl]dehxdielvncdawomqk[teizynepguvtgofr]fjazkxesmlwryphifh[ppjfvfefqhmuqtdp]luopramrehtriilwlo") === true)
    assert (Advent.tls?("ihekzgbwpjxgbau[eqpvqxncntbtsqn]mbtbcujdkbrhxdu") === false)
    assert (Advent.tls?("hgtbgxarmgbxqrhghxb[hflfvoetxfisqsj]syhkugztqkywetyhad[fmptjuaqittvvyooda]cjkxbnylglnlfleelzu") === true)
    assert (Advent.tls?("yhrowrreplrrsbupeor[nchtznfzbzwnogh]rynudxihckzattbz[dshxeaqusdlhydtm]rvqzuffgqtysfzxp") === false)
    assert (Advent.tls?("unfjgussbjxzlhopoqg[ppdnqkiuooukdmbqlo]flfiieiitmettblfln") === false)
  end
end
