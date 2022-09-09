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


object Stream_Switch extends SpinalEnum(defaultEncoding = binaryOneHot) {
    for(i<-0 to 2){
         val ChoseFlag = newElement
    }
}
class Axis_Switch_sl2ma(Master_Port_Num:Int) extends Component{
    val io=new Bundle{
        val Switch=in UInt(2 bits)
    }
        noIoPrefix()
    val s0_axis_s2mm=new Bundle{//一个从接口，两个主接口
        val tdata=in UInt(32 bits)
        val tkeep=in UInt(4 bits)
        val tlast=in Bool()
        val tready=out Bool()
        val tvalid=in Bool()
    }

    val masterport=Vec(new Bundle{        
        val tdata=out UInt(32 bits)
        val tkeep=out UInt(4 bits)
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

class Axis_ma2sla(Slave_Port_Num:Int) extends Component{
    val io=new Bundle{
        val Switch=in Bits(log2Up(Slave_Port_Num) bits)
    }
    noIoPrefix()
    val m0_axis_mm2s=new Bundle{//一个主接口，多个从接口
        val tdata=out UInt(32 bits)
        val tkeep=out UInt(4 bits)
        val tlast=out Bool()
        val tready=in Bool()
        val tvalid=out Bool()
    }
    val SlavePort=Vec(new Bundle{        
        val tdata=in UInt(32 bits)
        val tkeep=in UInt(4 bits)
        val tlast=in Bool()
        val tready=out Bool()
        val tvalid=in Bool()
    },Slave_Port_Num)//生成这么多个slave口，放左边

    for(i<-0 to Slave_Port_Num-1){
        when(io.Switch===i){
            SlavePort(i).tready:=m0_axis_mm2s.tready//出去的ready
            m0_axis_mm2s.tkeep:=SlavePort(i).tkeep
            m0_axis_mm2s.tdata:=SlavePort(i).tdata
            m0_axis_mm2s.tlast:=SlavePort(i).tlast
            m0_axis_mm2s.tvalid:=SlavePort(i).tvalid
        }otherwise{
            SlavePort(i).tready:=False
            m0_axis_mm2s.tdata:=0
            m0_axis_mm2s.tkeep:=0
        }
    }
}