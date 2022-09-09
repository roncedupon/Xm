package MedFilter_V12

import spinal.core._
class Axis_Switch() extends Component{
        val io=new Bundle{
            val Switch=in UInt(1 bits)
        }
        noIoPrefix()
        val s0_axis_s2mm=new Bundle{//一个从接口，两个主接口
            val tdata=in UInt(32 bits)
            val tkeep=in UInt(4 bits)
            val tlast=in Bool()
            val tready=out Bool()
            val tvalid=in Bool()
        }
    val m0_axis_mm2s=new Bundle{
        val tdata=out UInt(32 bits)
        val tkeep=out UInt(4 bits)
        val tlast=out Bool()
        val tready=in Bool()
        val tvalid=out Bool()
    }
    val m1_axis_mm2s=new Bundle{
        val tdata=out UInt(32 bits)
        val tkeep=out UInt(4 bits)
        val tlast=out Bool()
        val tready=in Bool()
        val tvalid=out Bool()
    }
    m1_axis_mm2s.tkeep:=s0_axis_s2mm.tkeep
    m0_axis_mm2s.tkeep:=s0_axis_s2mm.tkeep
    when(io.Switch===0){//s口连接0号主接口
        s0_axis_s2mm.tready:=m0_axis_mm2s.tready

        m0_axis_mm2s.tdata:=s0_axis_s2mm.tdata
        m0_axis_mm2s.tlast:=s0_axis_s2mm.tlast
        m0_axis_mm2s.tvalid:=s0_axis_s2mm.tvalid

        m1_axis_mm2s.tdata:=0
        m1_axis_mm2s.tlast:=False
        m1_axis_mm2s.tvalid:=False
    }otherwise{
        m0_axis_mm2s.tdata:=0
        m0_axis_mm2s.tlast:=False
        m0_axis_mm2s.tvalid:=False

        s0_axis_s2mm.tready:=m1_axis_mm2s.tready

        m1_axis_mm2s.tdata:=s0_axis_s2mm.tdata
        m1_axis_mm2s.tlast:=s0_axis_s2mm.tlast
        m1_axis_mm2s.tvalid:=s0_axis_s2mm.tvalid
    }
    
}


case class Stream_Switch(Enum_Num:Int) extends SpinalEnum(defaultEncoding = binaryOneHot) {
    for(i<-0 to log2Up(Enum_Num)){
         val ChoseFlag = newElement
    }
}
class Axis_Switch_sl2ma(Master_Port_Num:Int,Data_Width:Int) extends Component{
    val io=new Bundle{
        val Switch=in UInt(2 bits)
    }
        noIoPrefix()
    val s0_axis_s2mm=new Bundle{//一个从接口，两个主接口
        val tdata=in UInt(Data_Width bits)
        val tkeep=in UInt(Data_Width/8 bits)
        val tlast=in Bool()
        val tready=out Bool()
        val tvalid=in Bool()
    }

    val masterport=Vec(new Bundle{        
        val tdata=out UInt(Data_Width bits)
        val tkeep=out UInt(Data_Width/8 bits)
        val tlast=out Bool()
        val tready=in Bool()
        val tvalid=out Bool()
        tkeep:=s0_axis_s2mm.tkeep
        },Master_Port_Num)

    for(i<-0 to Master_Port_Num-1){
        when(io.Switch===i){
            s0_axis_s2mm.tready:=masterport(i).tready
            masterport(i).tdata:=s0_axis_s2mm.tdata
            masterport(i).tlast:=s0_axis_s2mm.tlast
            masterport(i).tvalid:=s0_axis_s2mm.tvalid
        }otherwise{
            s0_axis_s2mm.tready:=False
            masterport(i).tdata:=0
            masterport(i).tlast:=False
            masterport(i).tvalid:=False
        }
    }
}

class Axis_ma2sla(Slave_Port_Num:Int,Data_Width:Int) extends Component{
    val io=new Bundle{
        val Switch=in Bits(log2Up(Slave_Port_Num) bits)
    }
    noIoPrefix()
    val m0_axis_mm2s=new Bundle{//一个主接口，多个从接口
        val tdata=out UInt(Data_Width bits)
        val tkeep=out UInt(log2Up(Data_Width) bits)
        val tlast=out Bool()
        val tready=in Bool()
        val tvalid=out Bool()
    }
    val s=Vec(new Bundle{        
        val axis_s2mm_tdata=in UInt(Data_Width bits)
        val axis_s2mm_tkeep=in UInt(log2Up(Data_Width) bits)
        val axis_s2mm_tlast=in Bool()
        val axis_s2mm_tready=out Bool()
        val axis_s2mm_tvalid=in Bool()
    },Slave_Port_Num)//生成这么多个slave口，放左边

    for(i<-0 to Slave_Port_Num-1){
        when(io.Switch===i){
            s(i).axis_s2mm_tready:=m0_axis_mm2s.tready//出去的ready
            m0_axis_mm2s.tkeep:=s(i).axis_s2mm_tkeep
            m0_axis_mm2s.tdata:=s(i).axis_s2mm_tdata
            m0_axis_mm2s.tlast:=s(i).axis_s2mm_tlast
            m0_axis_mm2s.tvalid:=s(i).axis_s2mm_tvalid
        }otherwise{
            s(i).axis_s2mm_tready:=False
            m0_axis_mm2s.tdata:=0
            m0_axis_mm2s.tkeep:=0
            m0_axis_mm2s.tlast:=False
            m0_axis_mm2s.tvalid:=False
        }
    }
}
object StreamSwitchGen extends App { 
    val verilog_path="./testcode_gen/MemGen" 
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Axis_ma2sla(2,64))
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Axis_Switch_sl2ma(2,64))
}