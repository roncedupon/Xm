package Sort
import spinal.core._
import spinal.lib._

case class Sort5Config(DATA_WIDTH: Int){
  val sort2Config = Sort2Config(DATA_WIDTH)
  val sort3Config = Sort3Config(DATA_WIDTH)
}



class sort5(sort5Config:Sort5Config) extends Component{
 val io = new Bundle{
   val sData = in Vec(UInt(sort5Config.DATA_WIDTH bits), 5)
   val mData = out Vec(UInt(sort5Config.DATA_WIDTH bits), 5)




}
  noIoPrefix()
  val one_out = Vec(UInt(sort5Config.DATA_WIDTH bits), 5)//前三个的输出，和后两个的输出
  val delay_one_5 =UInt(sort5Config.DATA_WIDTH bits)
  val delay_one_4 =UInt(sort5Config.DATA_WIDTH bits)
  val delay_one_median =UInt(sort5Config.DATA_WIDTH bits)

  val first_one = new sort3(sort5Config.sort3Config)
  first_one.io.value1<>io.sData(0)
  first_one.io.value2<>io.sData(1)
  first_one.io.value3<>io.sData(2)

  first_one.io.max<>one_out(0)
  first_one.io.median<>one_out(1)
  first_one.io.min<>one_out(2)//第一次的最小值
  delay_one_median := Delay(one_out(1),1)

  val first_two = new sort2(sort5Config.sort2Config)
  first_two.io.value1 <> io.sData(3)
  first_two.io.value2 <> io.sData(4)

  first_two.io.max<> one_out(3)//da
  first_two.io.min<> one_out(4)

  delay_one_5 := Delay(one_out(4),1)
  delay_one_4:= Delay(one_out(3),1)

  //第二次
  val two_out = Vec(UInt(sort5Config.DATA_WIDTH bits), 4)
  val delay_two_min = UInt(sort5Config.DATA_WIDTH bits)
  val delay_two_max = UInt(sort5Config.DATA_WIDTH bits)

  val second_one = new sort2(sort5Config.sort2Config)
  second_one.io.value1<>delay_one_5
  second_one.io.value2<>one_out(2)
  second_one.io.max<>two_out(0)
  second_one.io.min<>two_out(1)  //五个点的最小值

  val second_two = new sort2(sort5Config.sort2Config)
  second_two.io.value1 <> one_out(0)
  second_two.io.value2 <> delay_one_4
  second_two.io.max <> two_out(2)//五个点的最大值
  second_two.io.min <> two_out(3)
  delay_two_max:=Delay(two_out(2),2)
  delay_two_min:=Delay(two_out(1),2)

//第三次
  val three_out = Vec(UInt(sort5Config.DATA_WIDTH bits), 3)//出五个中，中间那三

  val third = new sort3(sort5Config.sort3Config)
  third.io.value1<> two_out(3)
  third.io.value2<> delay_one_median
  third.io.value3<> two_out(0)

  third.io.max<>io.mData(1)
  third.io.median<>io.mData(2)
  third.io.min<>io.mData(3)
  delay_two_max<>io.mData(0)
  delay_two_min<>io.mData(4)

}



object sort5 extends App{
  SpinalVerilog(new sort5(Sort5Config(16)))
}
