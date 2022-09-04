package Sort

import spinal.core._
import spinal.lib.Delay

case class Sort4Config(DATA_WIDTH: Int) {
  val sort2Config = Sort2Config(DATA_WIDTH)
  val sort3Config = Sort3Config(DATA_WIDTH)
}

class sort4(sort4Config: Sort4Config) extends Component {
  val io = new Bundle {
    val sData = in Vec(UInt(sort4Config.DATA_WIDTH bits), 4)
    //val mData = out Vec(UInt(sort4Config.DATA_WIDTH bits), 4) //最后输出组大值为0    最小为3
    val max = out (UInt(sort4Config.DATA_WIDTH bits))
    val median_max = out (UInt(sort4Config.DATA_WIDTH bits))
    val median_min = out (UInt(sort4Config.DATA_WIDTH bits))
    val min = out UInt(sort4Config.DATA_WIDTH bits)

  }


  noIoPrefix()

  val delay_four = UInt(sort4Config.DATA_WIDTH bits)
  val oneout = Vec(Reg(UInt(sort4Config.DATA_WIDTH bits)), 3)



  val one = new sort3(sort4Config.sort3Config)
  one.io.value1 <> io.sData(0)
  one.io.value2 <> io.sData(1)
  one.io.value3 <> io.sData(2)

  one.io.max <> oneout(0)
  one.io.median <> oneout(1)
  one.io.min <> oneout(2) //第一次的最小值

  delay_four := Delay(io.sData(3), 3)

  when(delay_four > oneout(0)) {
    delay_four <> io.max
    oneout(0) <> io.median_max
    oneout(1) <> io.median_min
    oneout(2) <> io.min
  } elsewhen (delay_four > oneout(1)) {
    oneout(0) <> io.max
    delay_four <> io.median_max
    oneout(1) <> io.median_min
    oneout(2) <> io.min
  } elsewhen (delay_four > oneout(2) && delay_four <= oneout(1)) {
    oneout(0) <> io.max
    oneout(1) <> io.median_max
    delay_four <> io.median_min
    oneout(2) <> io.min

  } otherwise {
    oneout(0) <> io.max
    oneout(1) <> io.median_max
    oneout(2) <> io.median_min
    delay_four <> io.min

  }


}


object sort4 extends App {
  SpinalVerilog(new sort4(Sort4Config(16)))
}
