package FIfo_Dsp_Test
//fifo仿真将复位时间调高一点。。。。
//测试两个乘法器加一个fifo
import spinal.core._
import spinal.lib.slave
import spinal.lib.StreamFifo
import spinal.lib.Delay
class Lty_Pow(A_Wdith:Int,B_Width:Int,P_Width:Int) extends BlackBox{
    val io=new Bundle{//component要求out有驱动，但是black box不要求out的驱动
        val CLK=in Bool()
        val A=in UInt(A_Wdith bits)
        val B=in UInt(B_Width bits)
        val P=out UInt(P_Width bits)
    }
    noIoPrefix()
    mapClockDomain(clock=io.CLK)
}
class Lty_Mul(A_Wdith:Int,B_Width:Int,P_Width:Int) extends BlackBox{
    val io=new Bundle{//component要求out有驱动，但是black box不要求out的驱动
        val CLK=in Bool()
        val A=in UInt(A_Wdith bits)
        val B=in UInt(B_Width bits)
        val P=out UInt(P_Width bits)
    }
    noIoPrefix()
    mapClockDomain(clock=io.CLK)
}
class Fifo_Mul_Test extends Component{
    val io=new Bundle{
        val sData=slave Stream(UInt(16 bits))
    }
    noIoPrefix()
    io.sData.ready:=True
    //创建一个平方乘法器
    val Pow_Mul=new Lty_Pow(26,26,52)
    Pow_Mul.io.A:=io.sData.payload<<10
    Pow_Mul.io.B:=io.sData.payload<<10

    //创建一个i行乘法器

    val I_Mul=new Lty_Mul(52,11,64)
    I_Mul.io.A:=Pow_Mul.io.P
    I_Mul.io.B:=123
    //创建一个fifo
    val Fifo_Test=new StreamFifo(UInt(64 bits),16)
    // Fifo_Test.io.pop.ready:=Reg(Bool())init(False)
    // when(Fifo_Test.io.pop.valid){
    //     Fifo_Test.io.pop.ready:=RegNext(Fifo_Test.io.pop.valid)&&Fifo_Test.io.pop.valid
    // }
    Fifo_Test.io.pop.ready:=True
    Fifo_Test.io.push.payload:=Delay(I_Mul.io.P,2)
    Fifo_Test.io.push.valid:=Delay(io.sData.valid,11)

    val Fifo_Test2=new StreamFifo(UInt(52 bits),16)
    Fifo_Test2.io.pop.ready:=True
    Fifo_Test2.io.push.payload:=Pow_Mul.io.P
    Fifo_Test2.io.push.valid:=Delay(io.sData.valid,4)
}
object Top_GEn extends App { 
    val verilog_path="./CodeGen/FIfo_Dsp_Test" 
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Fifo_Mul_Test)
   
}