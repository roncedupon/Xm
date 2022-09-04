package Sort
import spinal.core._
import spinal.lib.Delay

case class Sort3Config(DATA_WIDTH: Int){
  val sort2Config = Sort2Config(DATA_WIDTH)

}

class sort3(sort3Config:Sort3Config) extends Component {
  val io = new Bundle {
    val value1 = in UInt (sort3Config.DATA_WIDTH bits)
    val value2 = in UInt (sort3Config.DATA_WIDTH bits)
    val value3 = in UInt (sort3Config.DATA_WIDTH bits)
    val max = out(Reg(UInt(sort3Config.DATA_WIDTH bits)) init 0)
    val median= out(Reg(UInt(sort3Config.DATA_WIDTH bits)) init 0)
    val min = out(Reg(UInt(sort3Config.DATA_WIDTH bits)) init 0)
//    val max = out UInt(sort3Config.DATA_WIDTH bits)
//    val median = out UInt(sort3Config.DATA_WIDTH bits)
//    val min = out UInt(sort3Config.DATA_WIDTH bits)
  }
  noIoPrefix()

  val max_temp = UInt (sort3Config.DATA_WIDTH bits)
  val min_temp = UInt(sort3Config.DATA_WIDTH bits)
  val delay_value3= UInt(sort3Config.DATA_WIDTH bits)

  val temp = new sort2(sort3Config.sort2Config)
  temp.io.value1<>io.value1
  temp.io.value2<>io.value2
  temp.io.max <> max_temp
  temp.io.min <> min_temp
  delay_value3 :=Delay(io.value3, 1)


  when(delay_value3>=max_temp){
    io.max := delay_value3
    io.median :=max_temp
    io.min := min_temp


  } otherwise{
     when(delay_value3 > min_temp){
       io.max := max_temp
       io.median := delay_value3
       io.min := min_temp

     }otherwise{
       io.max := max_temp
       io.median := min_temp
       io.min := delay_value3

     }

  }


}

object sort3 extends App {
  SpinalVerilog(new sort3(Sort3Config(16)))
}
