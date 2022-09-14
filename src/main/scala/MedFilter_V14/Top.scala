package MedFilter_V12
import spinal.core._
class Axis_Width_Convert(In_Data_Bytes:Int,Out_Data_Bytes:Int) extends BlackBox{
    val m_axis=new Bundle{
        val tdata=out UInt(Out_Data_Bytes*8 bits)
        val tkeep=out Bits(Out_Data_Bytes bits)
        val tlast=out Bool()
        val tready=in Bool()
        val tvalid=out Bool()
    }
    val s_axis=new Bundle{
        val tdata=in UInt(In_Data_Bytes*8 bits)
        val tkeep=in Bits(In_Data_Bytes bits)
        val tlast=in Bool()
        val tready=out Bool()
        val tvalid=in Bool()
    }
}
class top extends Component{

    val MedSort_Stream=new Med_Sort_Stream
    val MapStream=new Map_Stream
    val Hao_Lty=new LtyLh_Stream

    val Axiswitch1s=new Axis_Switch_1s(3,64)
    val Axiswitch2s=new Axis_Switch_2s(3,64)
    
    val MedSort_Convert_In=new Axis_Width_Convert(8,4)//进32 出32，Dma位宽：64
    val MedSort_Convert_Out=new Axis_Width_Convert(4,8)
    val MapStream_Convert_In=new Axis_Width_Convert(8,12)
    
    MedSort_Stream.io.m_axis_mm2s_tdata<> MedSort_Convert_Out.s_axis.tdata
    MedSort_Stream.io.m_axis_mm2s_tkeep<> MedSort_Convert_Out.s_axis.tkeep
    MedSort_Stream.io.m_axis_mm2s_tlast<> MedSort_Convert_Out.s_axis.tlast
    MedSort_Stream.io.m_axis_mm2s_tready<>MedSort_Convert_Out.s_axis.tready
    MedSort_Stream.io.m_axis_mm2s_tvalid<>MedSort_Convert_Out.s_axis.tvalid

    MedSort_Stream.io.s_axis_s2mm_tdata<>MedSort_Convert_In.s_axis.tdata
    MedSort_Stream.io.s_axis_s2mm_tkeep<>MedSort_Convert_In.s_axis.tkeep
    MedSort_Stream.io.s_axis_s2mm_tlast<>MedSort_Convert_In.s_axis.tlast
    MedSort_Stream.io.s_axis_s2mm_tready<>MedSort_Convert_In.s_axis.tready
    MedSort_Stream.io.s_axis_s2mm_tvalid<>MedSort_Convert_In.s_axis.tvalid

    MapStream.io.s_axis_s2mm_tdata<> MapStream_Convert_In.m_axis.tdata
    MapStream.io.s_axis_s2mm_tkeep<> MapStream_Convert_In.m_axis.tkeep
    MapStream.io.s_axis_s2mm_tlast<> MapStream_Convert_In.m_axis.tlast
    MapStream.io.s_axis_s2mm_tready<>MapStream_Convert_In.m_axis.tready
    MapStream.io.s_axis_s2mm_tvalid<>MapStream_Convert_In.m_axis.tvalid



//axis2s=====================================================================
    MedSort_Convert_Out.m_axis.tdata<>Axiswitch2s.s(0).axis_s2mm_tdata
    MedSort_Convert_Out.m_axis.tkeep<>Axiswitch2s.s(0).axis_s2mm_tkeep
    MedSort_Convert_Out.m_axis.tlast<>Axiswitch2s.s(0).axis_s2mm_tlast
    MedSort_Convert_Out.m_axis.tready<>Axiswitch2s.s(0).axis_s2mm_tready
    MedSort_Convert_Out.m_axis.tvalid<>Axiswitch2s.s(0).axis_s2mm_tvalid

    
    Hao_Lty.io.m_axis_mm2s_tdata<> Axiswitch2s.s(1).axis_s2mm_tdata
    Hao_Lty.io.m_axis_mm2s_tkeep<> Axiswitch2s.s(1).axis_s2mm_tkeep
    Hao_Lty.io.m_axis_mm2s_tlast<> Axiswitch2s.s(1).axis_s2mm_tlast
    Hao_Lty.io.m_axis_mm2s_tready<>Axiswitch2s.s(1).axis_s2mm_tready
    Hao_Lty.io.m_axis_mm2s_tvalid<>Axiswitch2s.s(1).axis_s2mm_tvalid

    MapStream.io.m_axis_mm2s_tdata<> Axiswitch2s.s(2).axis_s2mm_tdata
    MapStream.io.m_axis_mm2s_tkeep<> Axiswitch2s.s(2).axis_s2mm_tkeep
    MapStream.io.m_axis_mm2s_tlast<> Axiswitch2s.s(2).axis_s2mm_tlast
    MapStream.io.m_axis_mm2s_tready<>Axiswitch2s.s(2).axis_s2mm_tready
    MapStream.io.m_axis_mm2s_tvalid<>Axiswitch2s.s(2).axis_s2mm_tvalid

//axis1s======================================================================
    MedSort_Convert_In.s_axis.tdata<> Axiswitch1s.m(0).axis_mm2s_tdata
    MedSort_Convert_In.s_axis.tkeep<> Axiswitch1s.m(0).axis_mm2s_tkeep
    MedSort_Convert_In.s_axis.tlast<> Axiswitch1s.m(0).axis_mm2s_tlast
    MedSort_Convert_In.s_axis.tready<>Axiswitch1s.m(0).axis_mm2s_tready
    MedSort_Convert_In.s_axis.tvalid<>Axiswitch1s.m(0).axis_mm2s_tvalid

    Hao_Lty.io.s_axis_s2mm_tdata<> Axiswitch1s.m(1).axis_mm2s_tdata
    Hao_Lty.io.s_axis_s2mm_tkeep<> Axiswitch1s.m(1).axis_mm2s_tkeep
    Hao_Lty.io.s_axis_s2mm_tlast<> Axiswitch1s.m(1).axis_mm2s_tlast
    Hao_Lty.io.s_axis_s2mm_tready<>Axiswitch1s.m(1).axis_mm2s_tready
    Hao_Lty.io.s_axis_s2mm_tvalid<>Axiswitch1s.m(1).axis_mm2s_tvalid

    MapStream_Convert_In.m_axis.tdata<>Axiswitch1s.m(1).axis_mm2s_tdata
    MapStream_Convert_In.m_axis.tkeep<>Axiswitch1s.m(1).axis_mm2s_tkeep
    MapStream_Convert_In.m_axis.tlast<>Axiswitch1s.m(1).axis_mm2s_tlast
    MapStream_Convert_In.m_axis.tready<>Axiswitch1s.m(1).axis_mm2s_tready
    MapStream_Convert_In.m_axis.tvalid<>Axiswitch1s.m(1).axis_mm2s_tvalid
//============================================================================
    



}


object Top_GEn extends App { 
    val verilog_path="./testcode_gen/Top" 
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new LtyLh_Stream)
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Map_Stream)
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Med_Sort_Stream)
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new RegTable)
}