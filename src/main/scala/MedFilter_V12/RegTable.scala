package MedFilter_V12

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


	val Sumx1Reg=bus.newReg(doc="Med Sort Threshold 1 32 bits")
	val Sumx2Reg=bus.newReg(doc="Med Sort Threshold 2 32 bits")

	val temp_back_mean_Reg=bus.newReg(doc="temp_back_mean (float) 32 bits")
//temp_back_mean=sumX / sumNum;
	val temp_back_thrd_Reg=bus.newReg(doc="temp_back_thrd (float) 32 bits\ntemp_back_thrd= temp_back_mean + Thrd_Times（5) * temp_back_std( sqrt(sumX2/sumNum - temp_back_mean^2));")
//temp_back_thrd= temp_back_mean + Thrd_Times（5) * temp_back_std( sqrt(sumX2/sumNum - temp_back_mean^2));



	val Start_Compute=Reg0.field(Bool(),WO,doc="O:Ps writes start signal to pl").asOutput()
	val Start_Map=Reg0.field(Bool(),WO,doc="O:Start Map triangle").asOutput()
	val Switch_Stream=Reg0.field(Bits(2 bits),WO,doc="Stream port switch").asOutput()
	//val Read_Ps_Num=Reg0.field(Bits(10 bit),WO,doc="pl 从ps ddr读数据的数量").asInput()--报错
	val End_Compute=Reg1.field(Bool(),RO,doc="I:Pl writes back Med Sort end signal to ps").asInput()
    
	val Sumx1=Sumx1Reg.field(SInt(32 bits),RO,doc="Med Sort Threshold 1 32 bits").asInput()
	val Sumx2=Sumx2Reg.field(UInt(32 bits),RO,doc="Med Sort Threshold 1 32 bits").asInput()

	val temp_back_mean=temp_back_mean_Reg.field(UInt(32 bits),WO,doc="temp_back_mean (float) 32 bits").asOutput()
	val temp_back_thrd=temp_back_thrd_Reg.field(UInt(32 bits),WO,doc="temp_back_thrd (float) 32 bits").asOutput()


    val LD0123=Reg2.field(Bits(4 bit),WO,doc="O:接外面的灯，测试").asOutput()
	//val Read_Ps_Num=Reg0.field(Bits(10 bit),WO,doc="pl 从ps ddr读数据的数量").asInput()--报错
	val LD4567=Reg2.field(Bits(4 bit),WO,doc="O:接外面的灯，测试").asOutput() 

	bus.accept(HtmlGenerator("regif.html", "RegTable V1"))//fuck
	//git pull 测试哈哈哈哈
}
