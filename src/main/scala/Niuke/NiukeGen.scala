package Niuke
import spinal.core._
object NiukeGen extends App { 
    val verilog_path="./NiukeGen_gen" 
    SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new VL1_421)
}