package Sort
import spinal.core._
import spinal.lib.{Delay, master, slave}

case class SortConfig(DATA_WIDTH: Int){
  val sort3Config = Sort3Config(DATA_WIDTH)
  val sort4Config = Sort4Config(DATA_WIDTH)
  val sort5Config = Sort5Config(DATA_WIDTH)


}

//case class GenerateMatrixPort(dataWidth: Int, kernelNum: Int) extends Bundle {
//  val sData = Vec(slave Flow UInt(dataWidth bits), kernelNum)
//  val valid = in Bool()
//}

class fiveRowCol(sortConfig:SortConfig) extends Component {
  val io = new Bundle{
    val sData = in Vec(UInt(sortConfig.DATA_WIDTH bits), 25)
    val mData = out Vec(UInt(sortConfig.DATA_WIDTH bits), 3)

  }
  noIoPrefix()



  val oneRowIn = new sort5(sortConfig.sort5Config)
  val twoRowIn = new sort5(sortConfig.sort5Config)
  val threeRowIn = new sort5(sortConfig.sort5Config)
  val fourRowIn = new sort5(sortConfig.sort5Config)
  val fiveRowIn = new sort5(sortConfig.sort5Config)

  val oneRowOut = Vec(UInt(sortConfig.DATA_WIDTH bits), 5)//第一行
  val twoRowOut = Vec(UInt(sortConfig.DATA_WIDTH bits), 5)//第二行
  val threeRowOut = Vec(UInt(sortConfig.DATA_WIDTH bits), 5)//前第三
  val fourRowOut = Vec(UInt(sortConfig.DATA_WIDTH bits), 5)//
  val fiveRowOut = Vec(UInt(sortConfig.DATA_WIDTH bits), 5)//


  for (i <- 0 to 4) {
    oneRowIn.io.sData(i)<>io.sData(i)
    twoRowIn.io.sData(i)<>io.sData(i+5)
    threeRowIn.io.sData(i)<>io.sData(i+10)
    fourRowIn.io.sData(i)<>io.sData(i+15)
    fiveRowIn.io.sData(i)<>io.sData(i+20)
  }

  oneRowIn.io.mData<>oneRowOut
  twoRowIn.io.mData<>twoRowOut
  threeRowIn.io.mData<>threeRowOut
  fourRowIn.io.mData<>fourRowOut
  fiveRowIn.io.mData<>fiveRowOut

  val oneColIn = new sort5(sortConfig.sort5Config)//最大
  val twoColIn = new sort5(sortConfig.sort5Config)
  val threeColIn = new sort5(sortConfig.sort5Config)
  val fourColIn = new sort5(sortConfig.sort5Config)
  val fiveColIn = new sort5(sortConfig.sort5Config)


  val oneColOut = Vec(UInt(sortConfig.DATA_WIDTH bits), 5) //第一行
  val twoColOut = Vec(UInt(sortConfig.DATA_WIDTH bits), 5) //第二行
  val threeColOut = Vec(UInt(sortConfig.DATA_WIDTH bits), 5) //前第三
  val fourColOut = Vec(UInt(sortConfig.DATA_WIDTH bits), 5) //
  val fiveColOut = Vec(UInt(sortConfig.DATA_WIDTH bits), 5) //


    oneColIn.io.sData(0) <> oneRowOut(0)
    oneColIn.io.sData(1) <>twoRowOut(0)
    oneColIn.io.sData(2) <> threeRowOut(0)
    oneColIn.io.sData(3) <> fourRowOut(0)
    oneColIn.io.sData(4) <> fiveRowOut(0)

    twoColIn.io.sData(0) <> oneRowOut(1)
    twoColIn.io.sData(1) <> twoRowOut(1)
    twoColIn.io.sData(2) <> threeRowOut(1)
    twoColIn.io.sData(3) <> fourRowOut(1)
    twoColIn.io.sData(4) <> fiveRowOut(1)

    threeColIn.io.sData(0) <> oneRowOut(2)
    threeColIn.io.sData(1) <> twoRowOut(2)
    threeColIn.io.sData(2) <> threeRowOut(2)
    threeColIn.io.sData(3) <> fourRowOut(2)
    threeColIn.io.sData(4) <> fiveRowOut(2)

    fourColIn.io.sData(0) <> oneRowOut(3)
    fourColIn.io.sData(1) <> twoRowOut(3)
    fourColIn.io.sData(2) <> threeRowOut(3)
    fourColIn.io.sData(3) <> fourRowOut(3)
    fourColIn.io.sData(4) <> fiveRowOut(3)


    fiveColIn.io.sData(0) <> oneRowOut(4)
    fiveColIn.io.sData(1) <> twoRowOut(4)
    fiveColIn.io.sData(2) <> threeRowOut(4)
    fiveColIn.io.sData(3) <> fourRowOut(4)
    fiveColIn.io.sData(4) <> fiveRowOut(4)



    oneColIn.io.mData <>oneColOut
    twoColIn.io.mData <> twoColOut
    threeColIn.io.mData <> threeColOut
    fourColIn.io.mData <> fourColOut
    fiveColIn.io.mData <> fiveColOut



  val compareFourFirst= new sort4(sortConfig.sort4Config)

  twoColOut(4)<>compareFourFirst.io.sData(1)
  threeColOut(3)<>compareFourFirst.io.sData(2)
  fourColOut(2)<>compareFourFirst.io.sData(3)
  fiveColOut(1)<>compareFourFirst.io.sData(0)

  val compareFive = new sort5(sortConfig.sort5Config)
  oneColOut(4)<>compareFive.io.sData(0)
  twoColOut(3)<>compareFive.io.sData(1)
  threeColOut(2)<>compareFive.io.sData(2)
  fourColOut(1)<>compareFive.io.sData(3)
  fiveColOut(0)<>compareFive.io.sData(4)

  val compareFourSecond= new sort4(sortConfig.sort4Config)
  oneColOut(3)<>compareFourSecond.io.sData(3)
  twoColOut(2)<>compareFourSecond.io.sData(0)
  threeColOut(1)<>compareFourSecond.io.sData(1)
  fourColOut(0)<>compareFourSecond.io.sData(2)


  val delayEndOne = Delay(compareFourFirst.io.max,2)
  val delayEndTwo= Delay(compareFourSecond.io.min,2)

  val compareEnd = new sort3(sortConfig.sort3Config)
  delayEndOne<>compareEnd.io.value1
  compareFive.io.mData(2)<>compareEnd.io.value2
  delayEndTwo<>compareEnd.io.value3

  compareEnd.io.max<>io.mData(0)
  compareEnd.io.median<>io.mData(1)
  compareEnd.io.min<>io.mData(2)



}

object fiveRowCol extends App{
SpinalVerilog(new fiveRowCol(SortConfig(16)))

}
