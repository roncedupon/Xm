package MedFilter_V6

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._
import spinal.lib.bus.regif.AccessType._
import scala.collection.script.Start
class RegTable extends Component{

	val regSData = slave(AxiLite4(log2Up(1 MiB), 32))//地址位宽-数据位宽

	AxiLite4SpecRenamer(regSData)
	
	val bus = BusInterface(regSData, sizeMap = SizeMapping(0x43C00000, 1 MiB))//创建一个总线接口,映射范围是0~1MiB
	//后面的内存映射也可以这样写：BusInterface(IO.regSData,(0x0000, 100 Byte)
	//这个BusInterface是一个object
	//这里的只读和只写对应的是master那边的只读和只写
	val Reg0  = bus.newReg(doc="Ps Write Start signal to Pl,WO")
	val Reg1  = bus.newReg(doc="Pl ends computing and writes end Signal back to Ps,RO")
    val Reg2  = bus.newReg(doc="Pl Lights up Led ,test")
	val Start_Compute=Reg0.field(Bool(),WO,doc="O:Ps writes start signal to pl").asOutput()
	//val Read_Ps_Num=Reg0.field(Bits(10 bit),WO,doc="pl 从ps ddr读数据的数量").asInput()--报错
	val End_Compute=Reg1.field(Bool(),RO,doc="I:Pl writes back end signal to ps").asInput()
    
    val LD0123=Reg2.field(Bits(4 bit),WO,doc="O:接外面的灯，测试").asOutput()
	//val Read_Ps_Num=Reg0.field(Bits(10 bit),WO,doc="pl 从ps ddr读数据的数量").asInput()--报错
	val LD4567=Reg2.field(Bits(4 bit),WO,doc="O:接外面的灯，测试").asOutput()
	//只写部分需要一个驱动
	// Read_Ps_Num:=B"10'b0"
	//总结:只读用asInput(),只写用asOutput()
	//如果你把一个WO变成asInput()会报错的
	//同样地,对于一个只写端口,你想通过axilite从中读取数据,确实能读出来,但是有一个error信号会被拉高,详情看生成的verilog代码

	//如果32bit中全是只读或者只写,会有一个error信号,这个error信号在只读时写或者只写时读就会一直拉高
	//如果32bit中既有只读又有只写,那么error信号就没了

	bus.accept(HtmlGenerator("regif.html", "RegTable V1"))
}
