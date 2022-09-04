package Sort
import spinal.core._
import spinal.lib._

case class SortTopCongig(DATA_WIDTH: Int){
  val fiveRowColCongig = SortConfig(DATA_WIDTH)
}



class sortTop(sortTopCongig:SortTopCongig) extends Component {

  val io = new Bundle{

   // val sData = Vec(slave Flow UInt(sortTopCongig.DATA_WIDTH bits), 50)
   val sData = in Vec(UInt(sortTopCongig.DATA_WIDTH bits), 50)
    val vaildIn = in Bool()
    val vaildOut = out Bool()
    val mData = out Vec(UInt(sortTopCongig.DATA_WIDTH bits), 2)
    //val median = out UInt (sortTopCongig.DATA_WIDTH bits)

  }
  noIoPrefix()

  val filterOne = new fiveRowCol(sortTopCongig.fiveRowColCongig)
  for(i <- 0 to 24){
    filterOne.io.sData(i)<>io.sData(i)
  }

  filterOne.io.mData(1)<>io.mData(0)


  val filterTwo = new fiveRowCol(sortTopCongig.fiveRowColCongig)
  for (i <- 0 to 24) {
    filterTwo.io.sData(i) <> io.sData(i+25)
  }
  filterTwo.io.mData(1)<>io.mData(1)

  io.vaildOut := Delay(io.vaildIn,17)

}



object sortTop extends App{
  SpinalVerilog(new sortTop(SortTopCongig(16)))


}
