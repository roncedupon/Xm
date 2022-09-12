package MedFilter_V12
import spinal.core._

// class top extends Component{
//     val RegTable=new RegTable
//     val MedSort_Stream=new Med_Sort_Stream
//     val Axiswitch1s=new Axis_Switch_1s(2,64)
//     val Axiswitch2s=new Axis_Switch_2s(2,64)



// }


object Top_GEn extends App { 
    val verilog_path="./testcode_gen/Top" 
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new RegTable)
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Med_Sort_Stream)
}