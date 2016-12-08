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
  end
end
