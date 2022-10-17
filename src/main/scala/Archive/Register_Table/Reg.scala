package Sim_719


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._
import spinal.lib.bus.regif.AccessType._
import spinal.core.sim._
object Reg_Config{
	val REG_ADDR_RANGE = 1 MiB
}

class Reg_Table extends Component{

	val regSData = slave(AxiLite4(log2Up(Reg_Config.REG_ADDR_RANGE), 32))

	AxiLite4SpecRenamer(regSData)
	
	val bus = BusInterface(regSData, sizeMap = SizeMapping(0x1230, Reg_Config.REG_ADDR_RANGE))//创建一个总线接口,映射范围是0~1MiB
	//后面的内存映射也可以这样写：BusInterface(IO.regSData,(0x0000, 100 Byte)
	//这个BusInterface是一个object
	//这里的只读和只写对应的是master那边的只读和只写
	val Reg0  = bus.newReg(doc="REG0")
	val Reg1  = bus.newReg(doc="REG1")
	val Read_Data_From_PS_Start=Reg0.field(Bits(4 bit),WO,doc="O:一比特启动信号，让pl从ps的ddr读数据").asOutput()
	//val Read_Ps_Num=Reg0.field(Bits(10 bit),WO,doc="pl 从ps ddr读数据的数量").asInput()--报错
	val Read_Ps_Num=Reg0.field(Bits(10 bit),RO,doc="O:pl 从ps ddr读数据的数量").asInput()
	val Compute_Status=Reg1.field(Bits(4 bits),RO,doc="I:pl返回给上位机的计算状态").asInput()
	//只写部分需要一个驱动
	// Read_Ps_Num:=B"10'b0"
	//总结:只读用asInput(),只写用asOutput()
	//如果你把一个WO变成asInput()会报错的
	//同样地,对于一个只写端口,你想通过axilite从中读取数据,确实能读出来,但是有一个error信号会被拉高,详情看生成的verilog代码

	//如果32bit中全是只读或者只写,会有一个error信号,这个error信号在只读时写或者只写时读就会一直拉高
	//如果32bit中既有只读又有只写,那么error信号就没了
	bus.accept(HtmlGenerator("regif.html", "Daiyao"))
}
//=========================================================================================================================
class Reg_Zedboard extends Component{

	val regSData = slave(AxiLite4(log2Up(1 MiB), 32))//地址位宽-数据位宽

	AxiLite4SpecRenamer(regSData)
	
	val bus = BusInterface(regSData, sizeMap = SizeMapping(0x60000000, 1 MiB))//创建一个总线接口,映射范围是0~1MiB
	//后面的内存映射也可以这样写：BusInterface(IO.regSData,(0x0000, 100 Byte)
	//这个BusInterface是一个object
	//这里的只读和只写对应的是master那边的只读和只写
	val Reg0  = bus.newReg(doc="REG0")
	val Reg1  = bus.newReg(doc="REG1")
	val Reg2  = bus.newReg(doc="REG1")
	val Reg3  = bus.newReg(doc="REG1")
	val Instru1=Reg0.field(Bits(32 bit),WO,doc="O:接外面的灯，测试").asOutput()
	val Instru2=Reg1.field(Bits(32 bit),WO,doc="O:接外面的灯，测试").asOutput()
	val Instru3=Reg2.field(Bits(32 bit),WO,doc="O:接外面的灯，测试").asOutput()
	val Instru4=Reg3.field(Bits(32 bit),WO,doc="O:接外面的灯，测试").asOutput()
	//val Read_Ps_Num=Reg0.field(Bits(10 bit),WO,doc="pl 从ps ddr读数据的数量").asInput()--报错
	
	//只写部分需要一个驱动
	// Read_Ps_Num:=B"10'b0"
	//总结:只读用asInput(),只写用asOutput()
	//如果你把一个WO变成asInput()会报错的
	//同样地,对于一个只写端口,你想通过axilite从中读取数据,确实能读出来,但是有一个error信号会被拉高,详情看生成的verilog代码

	//如果32bit中全是只读或者只写,会有一个error信号,这个error信号在只读时写或者只写时读就会一直拉高
	//如果32bit中既有只读又有只写,那么error信号就没了
	bus.accept(HtmlGenerator("regif.html", "Daiyao"))
}


object GenRtl712 extends App { 
    val verilog_path="./testcode_gen" 
    SpinalConfig(targetDirectory=verilog_path,defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Reg_Zedboard)
}

case class AxiLite4Driver(axi : AxiLite4, clockDomain : ClockDomain) {
  def reset() : Unit = {
    axi.aw.valid #= false
    axi.w.valid #= false
    axi.ar.valid #= false
    axi.r.ready #= true
    axi.b.ready #= true
  }
  def read(address : BigInt) : BigInt = {
    axi.ar.payload.prot.assignBigInt(6)
    
    axi.ar.valid #= true
    axi.ar.payload.addr #= address
    axi.r.ready #= true
    clockDomain.waitSamplingWhere(axi.ar.ready.toBoolean)
    axi.ar.valid #= false
    clockDomain.waitSamplingWhere(axi.r.valid.toBoolean)
    axi.r.ready #= false
    axi.r.payload.data.toBigInt
  }
  def write(address : BigInt, data : BigInt) : Unit = {
    axi.aw.payload.prot.assignBigInt(6)
    axi.w.payload.strb.assignBigInt(15)
    
    axi.aw.valid #= true
    axi.aw.payload.addr #= address
    axi.w.valid #= true
    axi.w.payload.data #= data
    clockDomain.waitSamplingWhere(axi.aw.ready.toBoolean && axi.w.ready.toBoolean)
    axi.aw.valid #= false
    axi.w.valid #= false
  }
}

// object Sim_AxiLite {
//   def main(args: Array[String]) {
//       SimConfig.withWave.workspacePath("./simworkspace").compile(new Reg_Zedboard).doSim{dut =>
//         dut.clockDomain.forkStimulus(100)
// 		val driver=AxiLite4Driver(dut.regSData,dut.clockDomain)
// 		dut.clockDomain.waitSampling()
// 		dut.Const_FEDCB#=1234
// 		dut.clockDomain.waitSampling()
// 		driver.reset()
// 		dut.clockDomain.waitSampling()
//           //print(i+"____")
//           //printf("-----------------%x-------------------\n",dut.pc_module.IO.Pc_out.toBigInt)
// 		driver.write(0,0xf)
// 		dut.clockDomain.waitSampling()
// 		driver.write(0,0xf0)
// 		dut.clockDomain.waitSampling()
// 		driver.read(0x4)
// 		dut.clockDomain.waitSampling()
// 		dut.clockDomain.waitSampling()
// 		dut.clockDomain.waitSampling()
// 		dut.clockDomain.waitSampling()
// //        printf("-读数据%lx-----------------------------------\n",)
//           //printf("%x\n",dut.Reg_file.mem(3).toBigInt)//写法一
//           //printf("%x\n",hh.toBigInt)//写法二
//     }
//   }
// }