package MedFilter_V6

import spinal.core._
import Sort.sortTop
import Sort.SortTopCongig
import spinal.lib.slave
import Archive.WaCounter
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
class Med_Sort extends Component{
    val Med_Filter=new MedFilter_V6
    val Sort50=new sortTop(SortTopCongig(16))
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(Config.BRAM_IN_DATA_WIDTH bits))

        val mData=out UInt(32 bits)
        val mValid=out Bool()
        val mReady=in Bool()
        val m_tlast=out Bool()
        val start=in Bool()

    }
    noIoPrefix()

    for(i<-0 to 49){
        Med_Filter.io.mData(i)<>Sort50.io.sData(i)
    }
    Med_Filter.io.mReady<>io.mReady
    Med_Filter.io.sData<>io.sData
    Med_Filter.io.start<>io.start

    Sort50.io.vaildIn<>Med_Filter.io.mValid
    Sort50.io.vaildOut<>io.mValid
    (Sort50.io.mData(0)##Sort50.io.mData(1)).asUInt<>io.mData

    val Data_Out_Cnt=WaCounter(io.mValid&&io.mReady,log2Up(1020*2040),1020*2040-1)//进1024*1024个点了个点了
    io.m_tlast:=Data_Out_Cnt.valid
}



class Med_Sort_Stream extends Component{
    val Med_Sort=new Med_Sort

    val io=new Bundle{
        val m_axis_mm2s_tdata=out UInt(32 bits)
        val m_axis_mm2s_tkeep=out Bits(4 bits)
        val m_axis_mm2s_tlast=out Bool()
        val m_axis_mm2s_tready=in Bool()
        val m_axis_mm2s_tvalid=out Bool()

        val s_axis_s2mm_tdata=in UInt(32 bits)
        val s_axis_s2mm_tkeep=in UInt(4 bits)
        val s_axis_s2mm_tlast=in Bool()
        val s_axis_s2mm_tready=out Bool()
        val s_axis_s2mm_tvalid=in Bool()

        val m_tlast=out Bool()
        val start=in Bool()    
    }
    noIoPrefix()
    Med_Sort.io.mData<>io.m_axis_mm2s_tdata
    Med_Sort.io.mReady<>io.m_axis_mm2s_tready
    Med_Sort.io.mValid<>io.m_axis_mm2s_tvalid
    Med_Sort.io.m_tlast<>io.m_axis_mm2s_tlast
    io.m_axis_mm2s_tkeep:=B"4'b1111"

    Med_Sort.io.sData.payload<>io.s_axis_s2mm_tdata
    Med_Sort.io.sData.ready<>io.s_axis_s2mm_tready
    Med_Sort.io.sData.valid<>io.s_axis_s2mm_tvalid

    Med_Sort.io.m_tlast<>io.m_tlast
    Med_Sort.io.start<>io.start



}
object MemGen extends App { 
    val verilog_path="./testcode_gen/MemGen" 
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Med_Sort)
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new RegTable)
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Med_Sort_Stream)
}