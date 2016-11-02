package org.scalawag.druthers

object HugeTest {
  class Huge(
    val a00:Option[Int] = None,val a01:Option[Int] = None,val a02:Option[Int] = None,val a03:Option[Int] = None,val a04:Option[Int] = None,val a05:Option[Int] = None,val a06:Option[Int] = None,val a07:Option[Int] = None,val a08:Option[Int] = None,val a09:Option[Int] = None,
    val a10:Option[Int] = None,val a11:Option[Int] = None,val a12:Option[Int] = None,val a13:Option[Int] = None,val a14:Option[Int] = None,val a15:Option[Int] = None,val a16:Option[Int] = None,val a17:Option[Int] = None,val a18:Option[Int] = None,val a19:Option[Int] = None,
    val a20:Option[Int] = None,val a21:Option[Int] = None,val a22:Option[Int] = None,val a23:Option[Int] = None,val a24:Option[Int] = None,val a25:Option[Int] = None,val a26:Option[Int] = None,val a27:Option[Int] = None,val a28:Option[Int] = None,val a29:Option[Int] = None,
    val a30:Option[Int] = None,val a31:Option[Int] = None,val a32:Option[Int] = None,val a33:Option[Int] = None,val a34:Option[Int] = None,val a35:Option[Int] = None,val a36:Option[Int] = None,val a37:Option[Int] = None,val a38:Option[Int] = None,val a39:Option[Int] = None,
    val a40:Option[Int] = None,val a41:Option[Int] = None,val a42:Option[Int] = None,val a43:Option[Int] = None,val a44:Option[Int] = None,val a45:Option[Int] = None,val a46:Option[Int] = None,val a47:Option[Int] = None,val a48:Option[Int] = None,val a49:Option[Int] = None,
    val a50:Option[Int] = None,val a51:Option[Int] = None,val a52:Option[Int] = None,val a53:Option[Int] = None,val a54:Option[Int] = None,val a55:Option[Int] = None,val a56:Option[Int] = None,val a57:Option[Int] = None,val a58:Option[Int] = None,val a59:Option[Int] = None,
    val a60:Option[Int] = None,val a61:Option[Int] = None,val a62:Option[Int] = None,val a63:Option[Int] = None,val a64:Option[Int] = None,val a65:Option[Int] = None,val a66:Option[Int] = None,val a67:Option[Int] = None,val a68:Option[Int] = None,val a69:Option[Int] = None,
    val a70:Option[Int] = None,val a71:Option[Int] = None,val a72:Option[Int] = None,val a73:Option[Int] = None,val a74:Option[Int] = None,val a75:Option[Int] = None,val a76:Option[Int] = None,val a77:Option[Int] = None,val a78:Option[Int] = None,val a79:Option[Int] = None,
    val a80:Option[Int] = None,val a81:Option[Int] = None,val a82:Option[Int] = None,val a83:Option[Int] = None,val a84:Option[Int] = None,val a85:Option[Int] = None,val a86:Option[Int] = None,val a87:Option[Int] = None,val a88:Option[Int] = None,val a89:Option[Int] = None,
    val a90:Option[Int] = None,val a91:Option[Int] = None,val a92:Option[Int] = None,val a93:Option[Int] = None,val a94:Option[Int] = None,val a95:Option[Int] = None,val a96:Option[Int] = None,val a97:Option[Int] = None,val a98:Option[Int] = None,val a99:Option[Int] = None
  )
}

import HugeTest._

class HugeTest extends OptionsParserTest {
  test("100 fields") {
    parseOf[Huge]("--a42 7 --a87 42",LONG) match { case (huge,extra) =>
      huge.a00 should be (None)
      huge.a42 should be (Some(7))
      huge.a87 should be (Some(42))
      huge.a99 should be (None)

      extra should be (Nil)
    }
  }
}

/* druthers -- Copyright 2013-2016 Justin Patterson -- All Rights Reserved */
