package MedFilter_V12
import com.sun.org.apache.xml.internal.security.Init
import spinal.core._
import spinal.lib.{Delay, Flow, StreamFifo, master, slave}
import sun.awt.OSInfo.OSType
import wa.{StreamDataWidthConvert, WaCounter, xMul}
import xingmin.memory.math.Mul

case class ConnDomainConfig(DATA_WIDTH: Int, FEATURE_WIDTH: Int, FEATURE_LENGTH: Int, ROW_MEM_DEPTH: Int) { //32 11(2040) 2040 用来缓存数据（2040）
  val STREAM_DATA_WIDTH = DATA_WIDTH * 4
}

object ComputeEnum extends SpinalEnum(defaultEncoding = binaryOneHot) {
  val IDLE, Judge, UPLEFT0, UP0LEFT, UP1LEFT,UP1LEFT1,END= newElement
}

object JudgeLEFTEnum extends SpinalEnum(defaultEncoding = binaryOneHot) {
  val IDLEE, ONE, TWO, THREE, FOUR= newElement
}

case class JudgeLEFTFsm(start: Bool) extends Area {

  val left2 = Bool()
  val left3 = Bool()
  val left4 = Bool()

  val currentState = Reg(JudgeLEFTEnum()) init JudgeLEFTEnum.IDLEE
  val nextState = JudgeLEFTEnum()
  currentState := nextState

  switch(currentState) {
    is(JudgeLEFTEnum.IDLEE) {
      when(start) {
        nextState := JudgeLEFTEnum.ONE
      } otherwise {
        nextState := JudgeLEFTEnum.IDLEE
      }
    }

    is(JudgeLEFTEnum.ONE) {
      when(left2) {
        nextState := JudgeLEFTEnum.TWO
      } otherwise {
        nextState := JudgeLEFTEnum.IDLEE
      }
    }

    is(JudgeLEFTEnum.TWO) {
      when(left3) {
        nextState := JudgeLEFTEnum.THREE
      } otherwise {
        nextState := JudgeLEFTEnum.IDLEE
      }
    }

    is(JudgeLEFTEnum.THREE) {
      when(left4) {
        nextState := JudgeLEFTEnum.FOUR
      } otherwise {
        nextState := JudgeLEFTEnum.IDLEE
      }
    }


    is(JudgeLEFTEnum.FOUR) {
      nextState := JudgeLEFTEnum.IDLEE
    }

  }


}






case class ComputeFsm(start: Bool) extends Area {
  val endEnd = Bool() //是否为最后一个数据
  val finsish = Bool()
  val init = Bool()
  val judge = Bool() //1为  图片小于阈值（upmem标为0）
  val delaySign= Bool()
  val delayUp= Bool()
  val delayLeft = Bool()

  val upleft0 = Bool() //上左都没有
  val up0left = Bool()
  val up1left = Bool()
  val up1lef1 = Bool()
  val delayUpLeft =Bool()

  val currentState = Reg(ComputeEnum()) init ComputeEnum.IDLE
  val nextState = ComputeEnum()
  currentState := nextState

  switch(currentState) {
    is(ComputeEnum.IDLE) {
      when(start) {
        nextState := ComputeEnum.Judge
      } otherwise {
        nextState := ComputeEnum.IDLE
      }
    }

    is(ComputeEnum.Judge) {
      when(init){
        when(upleft0) {
          nextState := ComputeEnum.UPLEFT0
        } elsewhen (up0left) {
          nextState := ComputeEnum.UP0LEFT
        } elsewhen (up1left) {
          nextState := ComputeEnum.UP1LEFT
        }elsewhen (up1lef1){
          nextState := ComputeEnum.UP1LEFT1
        } elsewhen (endEnd) {
          nextState := ComputeEnum.END
        }otherwise {
          nextState := ComputeEnum.Judge
        }


      }otherwise{
        nextState := ComputeEnum.Judge
      }




    }




    is(ComputeEnum.UPLEFT0) {
      when(endEnd) {
        nextState := ComputeEnum.END
      } elsewhen (delaySign){
        nextState := ComputeEnum.Judge
      } otherwise {
        nextState := ComputeEnum.UPLEFT0
      }

    }
    is(ComputeEnum.UP0LEFT) {
      when(endEnd) {
        nextState := ComputeEnum.END
      } elsewhen (delayLeft) {
        nextState := ComputeEnum.Judge
      }otherwise {
        nextState := ComputeEnum.UP0LEFT
      }
    }

    is(ComputeEnum.UP1LEFT) {
      when(endEnd) {
        nextState := ComputeEnum.END
      } elsewhen (delayUp) {
        when(Delay(up1lef1,5)){
          nextState := ComputeEnum.UP1LEFT1
        }otherwise {
          nextState := ComputeEnum.Judge
        }
      } otherwise {
        nextState := ComputeEnum.UP1LEFT
      }
    }


    is(ComputeEnum.UP1LEFT1){
      when(endEnd) {
        nextState := ComputeEnum.END
      } elsewhen (delayUpLeft) {
        nextState := ComputeEnum.Judge
      } otherwise {
        nextState := ComputeEnum.UP1LEFT1
      }
    }


    is(ComputeEnum.END){
      when(finsish){
        nextState := ComputeEnum.IDLE
      }otherwise{
        nextState := ComputeEnum.END
      }



    }



  }
}


class connDomain(connDomainConfig: ConnDomainConfig) extends Component {

  val io = new Bundle {
    val start = in Bool()
    val sData = slave Stream UInt(connDomainConfig.STREAM_DATA_WIDTH bits)
    val threshold = in UInt (connDomainConfig.DATA_WIDTH bits) //阈值s
    val tempBackMean = in UInt (connDomainConfig.DATA_WIDTH bits)
    val mData = master Stream UInt(64 bits)
    val connCount = out UInt (connDomainConfig.DATA_WIDTH bits)
    val last = out Bool()
    val signBit= in Bool()

  }
  noIoPrefix()
  //要进行的数据
  val inputFeature = Flow(UInt(connDomainConfig.DATA_WIDTH bits))




  //fifo 用于存数据
  val dataTemp = StreamFifo(UInt(connDomainConfig.STREAM_DATA_WIDTH bits), connDomainConfig.ROW_MEM_DEPTH)
  dataTemp.io.push.payload := io.sData.payload
  dataTemp.io.push.valid := io.sData.fire

  io.sData.ready := dataTemp.io.push.ready


  //出来要进行处理的数据
  val dataCvt = StreamDataWidthConvert(connDomainConfig.STREAM_DATA_WIDTH, connDomainConfig.DATA_WIDTH, "connDomain64to16", true)
  dataCvt.io.sData <> dataTemp.io.pop
  inputFeature.payload := dataCvt.io.mData.payload
  inputFeature.valid := dataCvt.io.mData.fire


  dataCvt.io.mData.ready.set()



  //状态机
  val fsm = ComputeFsm(io.start)

  val columnCnt = WaCounter(inputFeature.valid, connDomainConfig.FEATURE_WIDTH, connDomainConfig.FEATURE_LENGTH - 1)
  val rowCnt = WaCounter(columnCnt.valid, connDomainConfig.FEATURE_WIDTH, connDomainConfig.FEATURE_LENGTH - 1)
  val totalCnt = WaCounter(inputFeature.valid, connDomainConfig.FEATURE_WIDTH * 2, connDomainConfig.FEATURE_LENGTH * connDomainConfig.FEATURE_LENGTH - 1)
  val count = Reg(UInt(connDomainConfig.DATA_WIDTH bits)) init 0
  val outCount =  WaCounter(fsm.currentState===ComputeEnum.END&&io.mData.ready, connDomainConfig.DATA_WIDTH, count-1)



  fsm.endEnd := totalCnt.valid
  fsm.init := inputFeature.valid






  val rdAddr = Reg(UInt(connDomainConfig.FEATURE_WIDTH bits)) init 0
  val wrAddr = RegNext(rdAddr)
  val tempData = UInt(36 bits)
  when(io.signBit===True){
    tempData:= (U(0,8 bits)@@(inputFeature.payload<<12)) + io.tempBackMean
  }otherwise {
    tempData:= (U(0,8 bits)@@(inputFeature.payload<<12)) - io.tempBackMean
  }



  val uplist: List[UInt] = List.fill(2040)(0)
  val upmem = Mem(UInt(connDomainConfig.DATA_WIDTH bits), initialContent = uplist)
  val upDate = upmem.readSync(rdAddr) //用于mark 的判断
  val upData1= upmem.readSync(rdAddr-3)
  val upData2= upmem.readSync(rdAddr-4)
  val upData3= upmem.readSync(rdAddr-5)


  //存左边的数
  val left = Reg(UInt(connDomainConfig.DATA_WIDTH bits)) init 0
  val left1 = UInt(connDomainConfig.DATA_WIDTH bits)
  val left2 = UInt(connDomainConfig.DATA_WIDTH bits)
  val left3 = UInt(connDomainConfig.DATA_WIDTH bits)
  left1 :=0
  left2 :=0
  left3 :=0

  val value = UInt(connDomainConfig.DATA_WIDTH bits)
  value := 0

  val endAddr = UInt(connDomainConfig.DATA_WIDTH bits)
  when(upDate =/= 0 && left =/= 0) {
    endAddr := upDate
  } elsewhen (upDate > left) {
    endAddr := upDate
  } otherwise {
    endAddr := left
  }

  //最后的mem

  val endmem = Mem(UInt(272 bits), wordCount = 800) // 16  64 64 56 36 36   176
  val endDate = endmem.readSync(Delay(endAddr(9 downto 0),9))
  val endDate1 = endmem.readSync(Delay(endAddr(9 downto 0),4))
  val address = UInt(connDomainConfig.FEATURE_WIDTH bits)
  address :=0


  fsm.judge := (inputFeature.payload <= io.threshold) ? True | False
  fsm.upleft0 := (upDate === 0 && left === 0 && inputFeature.payload > io.threshold) ? True | False
  fsm.up0left := (upDate === 0 && left =/= 0 && inputFeature.payload > io.threshold) ? True | False
  fsm.up1left := (upDate =/= 0 && (left === 0||left===upDate)&&inputFeature.payload > io.threshold) ? True | False
  fsm.up1lef1 := (upDate =/= 0 && upDate =/= left&& left =/= 0 && inputFeature.payload > io.threshold)
  fsm.delaySign:= RegNext(RegNext(RegNext(fsm.upleft0)))
  fsm.delayUp := Delay(fsm.up1left,5)
  fsm.delayLeft:= Delay(fsm.up0left,5)
  fsm.delayUpLeft:= Delay(fsm.up1lef1,10)

  val xinFsm = JudgeLEFTFsm(fsm.up1lef1)
  xinFsm.left2 := (left1 =/= 0 && left1 =/= upDate)
  xinFsm.left3 := (left2 =/= 0 && left2 =/= upDate)
  xinFsm.left4:= (left3 =/= 0 && left3 =/= upDate)



  val Attributes = UInt(272 bits)  // 16  64 64 56 36 36   176  (15 downto 0)  (79 downto 16) (143 downto 80)   Attributes(199 downto 144) (235 downto 200) (271 downto 236)
  Attributes:=0
  val two = UInt(64 bits)
  val three = UInt(64 bits)
  val four = UInt(56 bits)
  val five = Reg(UInt(36 bits)) init 0
  val six =  U(0,2 bits)
  val judgeLeft = Reg(UInt(3 bits)) init 0
  //乘法运算

  val mul = Mul(36, 36, 56, "Unsigned", "Unsigned", 1, "DSP", this.clockDomain, "Mul")
  mul.io.A <> tempData
  mul.io.B <> tempData
  mul.io.P <> four

  val mul1 = Mul(56, 11, 64, "Unsigned", "Unsigned", 2, "DSP", this.clockDomain, "nextMul")
  mul1.io.A <> mul.io.P
  mul1.io.B <> rowCnt.count+5
  mul1.io.P <> two

  val mul2 = Mul(56, 11, 64, "Unsigned", "Unsigned", 2, "DSP", this.clockDomain, "twoMul")
  mul2.io.A <> mul.io.P
  mul2.io.B <> columnCnt.count+4
  mul2.io.P <> three


  when(fsm.currentState === ComputeEnum.UPLEFT0||fsm.currentState === ComputeEnum.UP1LEFT||fsm.currentState === ComputeEnum.UP0LEFT||fsm.currentState ===ComputeEnum.UP1LEFT1){
    dataCvt.io.mData.ready.clear()




  }



  when(fsm.currentState === ComputeEnum.UPLEFT0 && fsm.nextState === ComputeEnum.Judge) {
    Attributes(15 downto 0):=U"0000000000000001"
    endAddr:= count

    // endmem.write(count(9 downto 0), Attributes)
    Attributes(79 downto 16):= two
    Attributes(143 downto 80):=three
    Attributes(199 downto 144):=RegNext(RegNext(four))
    Attributes(235 downto 200) := five
    Attributes(271 downto 236) := five
    left := count


  }

  when ((fsm.currentState === ComputeEnum.UP1LEFT||fsm.currentState ===ComputeEnum.UP0LEFT) && fsm.nextState === ComputeEnum.Judge){
    Attributes(15 downto 0):=U"0000000000000001"+endDate1(15 downto 0)
    Attributes(79 downto 16):=Delay(two,2)+endDate1(79 downto 16)
    Attributes(143 downto 80):=Delay(three,2)+endDate1(143 downto 80)
    Attributes(199 downto 144):=Delay(four,4)+endDate1(199 downto 144)
    Attributes(235 downto 200) := five+endDate1(235 downto 200)
    Attributes(271 downto 236) := (five > endDate1(271 downto 236)) ? five | endDate1(271 downto 236)
    left := Delay(endAddr,5)
    endAddr:=Delay(endAddr,5)



  }


  when((fsm.currentState === ComputeEnum.UP1LEFT1) && fsm.nextState === ComputeEnum.Judge) {
    left := Delay(endAddr,10)
    when(judgeLeft===1){
      Attributes(15 downto 0) := U"0000000000000010" + endDate(15 downto 0)
      Attributes(79 downto 16) := Delay(two(61 downto 0), 7)*2 + endDate(79 downto 16)
      Attributes(143 downto 80) := Delay(three(61 downto 0), 7)*2 + endDate(143 downto 80)-Delay(four, 9)
      Attributes(199 downto 144) := Delay(four, 9)+Delay(four, 9) + endDate(199 downto 144)
      Attributes(235 downto 200) := five+five + endDate(235 downto 200)
      Attributes(271 downto 236) := (five > endDate(271 downto 236)) ? five | endDate(271 downto 236)
      endAddr:=Delay(endAddr,10)

    }

    when(judgeLeft === 2) {
      Attributes(15 downto 0) := U"0000000000000011" + endDate(15 downto 0)
      Attributes(79 downto 16) := Delay(two(60 downto 0), 7) * 3 + endDate(79 downto 16)
      Attributes(143 downto 80) := Delay(three(60 downto 0), 7) * 3 + endDate(143 downto 80) - Delay(four, 9)*2
      Attributes(199 downto 144) := Delay(four(52 downto 0), 9) * 3 + endDate(199 downto 144)
      Attributes(235 downto 200) := five +five+five + endDate(235 downto 200)
      Attributes(271 downto 236) := (five > endDate(271 downto 236)) ? five | endDate(271 downto 236)
      endAddr:=Delay(endAddr,10)

    }

    when(judgeLeft === 3) {
      Attributes(15 downto 0) := U"0000000000000100" + endDate(15 downto 0)
      Attributes(79 downto 16) := Delay(two(60 downto 0), 7) * 3+ Delay(two, 7)+ endDate(79 downto 16)
      Attributes(143 downto 80) := Delay(three(60 downto 0), 7) * 3+ Delay(three, 7) + endDate(143 downto 80) - Delay(four, 9) * 3
      Attributes(199 downto 144) := Delay(four(52 downto 0), 9) * 4 + endDate(199 downto 144)
      Attributes(235 downto 200) := five + five + five+ five + endDate(235 downto 200)
      Attributes(271 downto 236) := (five > endDate(271 downto 236)) ? five | endDate(271 downto 236)
      endAddr:=Delay(endAddr,10)

    }

    when(judgeLeft === 4) {
      Attributes(15 downto 0) := U"0000000000000101" + endDate(15 downto 0)
      Attributes(79 downto 16) := Delay(two(60 downto 0), 7) * 3+ Delay(two, 7)+ Delay(two, 7) + endDate(79 downto 16)
      Attributes(143 downto 80) := Delay(three(60 downto 0), 7) * 3+ Delay(three, 7)+ Delay(three, 7) + endDate(143 downto 80) - Delay(four, 9) * 3- Delay(four, 9)
      Attributes(199 downto 144) := Delay(four(52 downto 0), 9) * 5 + endDate(199 downto 144)
      Attributes(235 downto 200) := five + five + five + five+ five+ endDate(235 downto 200)
      Attributes(271 downto 236) := (five > endDate(271 downto 236)) ? five | endDate(271 downto 236)
      endAddr:=Delay(endAddr,10)

    }


  }



  when(xinFsm.currentState===JudgeLEFTEnum.ONE){//2
    address := wrAddr - 2
    value := RegNext(upDate)
    endAddr(9 downto 0):= left(9 downto 0)
    Attributes:=U(0, 272 bits)
    left1 := upData1
  }elsewhen(xinFsm.currentState===JudgeLEFTEnum.TWO){//4
    address := wrAddr - 3
    value := Delay(value,2)
    left2 := upData2
    judgeLeft := 2
  }elsewhen(xinFsm.currentState===JudgeLEFTEnum.THREE){//8
    address := wrAddr -4
    value := Delay(value,3)
    left3 := upData3
    judgeLeft := 3
  } elsewhen (xinFsm.currentState === JudgeLEFTEnum.FOUR) {//10
    address := wrAddr - 5
    value := Delay(value, 4)
    judgeLeft := 4
  }





  when(xinFsm.currentState===JudgeLEFTEnum.ONE||((fsm.currentState === ComputeEnum.UP1LEFT1 || fsm.currentState === ComputeEnum.UPLEFT0 || fsm.currentState === ComputeEnum.UP1LEFT || fsm.currentState === ComputeEnum.UP0LEFT) && fsm.nextState === ComputeEnum.Judge)) {
    endmem.write(endAddr(9 downto 0), Attributes)
  }




  when(inputFeature.valid){

    when(rdAddr === connDomainConfig.FEATURE_LENGTH - 1) {
      rdAddr := 0
    } elsewhen (fsm.up1left|fsm.up0left|fsm.upleft0|fsm.up1lef1) {
      rdAddr := rdAddr
    } otherwise {
      rdAddr := rdAddr + 1
    }


    when(fsm.upleft0) {
      address := wrAddr
      value:=count + 1
      count := count + 1
      five := tempData

    }

    when(fsm.up1left) {
      address := wrAddr
      value := upDate
      five := tempData
    }

    when(fsm.up1lef1) {
      five := tempData
      value := upDate
      judgeLeft := 1

    }


    when(fsm.up0left) {
      address := wrAddr
      value := left
      five := tempData
    }


    when(fsm.judge) {
      address := wrAddr
      value := U(0, 16 bits)
      left := U(0, 16 bits)
    }



  }otherwise {
    when((fsm.currentState === ComputeEnum.UPLEFT0 || fsm.currentState === ComputeEnum.UP1LEFT|| fsm.currentState ===ComputeEnum.UP0LEFT||fsm.currentState ===ComputeEnum.UP1LEFT1) && fsm.nextState === ComputeEnum.Judge) {
      rdAddr := rdAddr + 1
    }
    fsm.up0left := False
    fsm.up1left := False
    fsm.up1lef1 := False
    fsm.upleft0 := False

  }


  upmem.write(address, value)





  when(fsm.currentState===ComputeEnum.END||fsm.currentState === ComputeEnum.IDLE) {
    dataCvt.io.mData.ready.clear()

  }



  val outValue=endmem.readSync(outCount.count(9 downto 0)+1)
  io.mData.payload <> U(0,64 bits)
  io.mData.valid := fsm.currentState===ComputeEnum.END&&io.mData.ready

  io.connCount := count


  //按列输出

  val endCount = Reg(UInt(20 bits)) init 0
  val endValid = False
  io.last := endValid //
  fsm.finsish :=endValid



  when(RegNext(outCount.valid)&&fsm.currentState===ComputeEnum.END){
    when(endCount===5){
      endCount := 0
      endValid := True
      count :=0



    }otherwise {
      endCount := endCount +1
    }

  }

  switch(endCount){
    is(0){
      io.mData.payload <> U(0,48 bits)@@outValue(15 downto 0)
    }
    is(1) {
      io.mData.payload <> outValue(79 downto 16)
    }
    is(2) {
      io.mData.payload <> outValue(143 downto 80)
    }
    is(3) {
      io.mData.payload <> U(0, 8 bits) @@ outValue(199 downto 144)
    }
    is(4) {
      io.mData.payload <> U(0, 28 bits) @@ outValue(235 downto 200)
    }
    is(5) {
      io.mData.payload <> U(0, 28 bits) @@ outValue(271 downto 236)
    }



  }


}


class LtyLh_Stream extends Component{
  val io=new Bundle{
    def DATA_IN_WIDTH=64
      val m_axis_mm2s_tdata=out UInt(DATA_IN_WIDTH bits)
      val m_axis_mm2s_tkeep=out Bits(DATA_IN_WIDTH/8 bits)
      val m_axis_mm2s_tlast=out Bool()
      val m_axis_mm2s_tready=in Bool()
      val m_axis_mm2s_tvalid=out Bool()
    def DATA_OUT_WIDTH=64
      val s_axis_s2mm_tdata=in UInt(DATA_OUT_WIDTH bits)
      val s_axis_s2mm_tkeep=in Bits(DATA_OUT_WIDTH/8 bits)
      val s_axis_s2mm_tlast=in Bool()
      val s_axis_s2mm_tready=out Bool()
      val s_axis_s2mm_tvalid=in Bool()

      val start=in Bool()
      val threshold = in UInt (16 bits) //阈值s
      val tempBackMean = in UInt (16 bits)
      val connCount=out UInt(16 bits)
      val signBit=in Bool()
    }
    noIoPrefix()
    io.m_axis_mm2s_tkeep.setAll()

    val Lty_Lh=new connDomain(ConnDomainConfig(16, 11, 2040, 2040))
    Lty_Lh.io.start:=io.start
    Lty_Lh.io.sData.payload:=io.s_axis_s2mm_tdata
    Lty_Lh.io.sData.valid:=io.s_axis_s2mm_tvalid
    Lty_Lh.io.sData.ready<>io.s_axis_s2mm_tready

    Lty_Lh.io.mData.payload<>io.m_axis_mm2s_tdata
    Lty_Lh.io.last<>io.m_axis_mm2s_tlast
    Lty_Lh.io.mData.valid<>io.m_axis_mm2s_tvalid
    Lty_Lh.io.mData.ready<>io.m_axis_mm2s_tready

    Lty_Lh.io.tempBackMean<>io.tempBackMean
    Lty_Lh.io.threshold<>io.threshold
    Lty_Lh.io.connCount<>io.connCount
    Lty_Lh.io.signBit<>io.signBit
    
}

object connDomain extends App {
  val verilog_path="./testcode_gen/Lty_Lh" 
  SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new LtyLh_Stream)
}
