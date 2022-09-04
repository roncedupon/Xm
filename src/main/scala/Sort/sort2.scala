package Sort

import spinal.core._
import spinal.lib.{Delay, slave}

case class Sort2Config(DATA_WIDTH: Int)

class sort2(sort2config:Sort2Config) extends Component {
  val io = new Bundle {
    val value1 = in UInt (sort2config.DATA_WIDTH bits)
    val value2 = in UInt (sort2config.DATA_WIDTH bits)
    val max = out UInt (sort2config.DATA_WIDTH bits)
    val min = out UInt (sort2config.DATA_WIDTH bits)
//    val max = out UInt (sort2config.DATA_WIDTH bits)
//    val min = out UInt (sort2config.DATA_WIDTH bits)

  }
  noIoPrefix()
  val max_temp =  UInt (sort2config.DATA_WIDTH bits)
  val min_temp =  UInt (sort2config.DATA_WIDTH bits)

   io.max :=  Delay(max_temp,1)init 0
   io.min := Delay(min_temp,1)init 0
  when(io.value1 > io.value2) {
    max_temp := io.value1
    min_temp := io.value2
  } .otherwise {
    max_temp := io.value2
    min_temp := io.value1
  }
}

object sort2 extends App {
  SpinalVerilog(new sort2(Sort2Config(16)))
}
