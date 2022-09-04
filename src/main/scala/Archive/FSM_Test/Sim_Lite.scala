package Sim_utils.FSM_Test
//描述:实现Axi_Lite Master口仿真,模仿上位机发送指令
//指令格式为64bit,高8位为读写控制,10为读,11为写,低32位是写入lite口的数据,剩下的24位是写lite口地址
//首先testbench从txt中读取指令存入mem中
//然后开始模仿上位机收发指令,由于没有slave lite口,所以slave端的ready信号一直拉高,之后可以接入slave lite口进行进一步测试
//状态机:
	//idle->switch->[idle/read/write]->switch->idle
//注意:
	//有一个输出的读指令使能信号,由于switch用来决定lite是读还是写,所以需要一个控制信号控制指令的正确传入
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import scopt.Read

object Axi_Lite_Ctrl extends SpinalEnum(defaultEncoding = binaryOneHot) {
    val IDLE, WRITE_LITE, READ_LITE, SWITCH_STATE = newElement
}//switch判断是读还是写

//仿真lite口用
case class Axi_Lite_Fsm(Axi_Lite_Port : AxiLite4, clockDomain : ClockDomain) extends Area {
	//状态机
	val Write_Data=Bits(32 bits)
	val Write_Addr=UInt(32 bits)
	val Read_Addr=UInt(32 bits)
	val Inst_Two_High=Bits(8 bits)
	val start=Bool()
	val LAST_INST=Bool()
	val Read_Inst_EN=Bool()//只有在switch状态才读指令
	//val Axi_Lite_Port=master(AxiLite4(log2Up(1 MiB), 32))//AXi lite 主端口,连接到regS_Data上
    val currentState = Reg(Axi_Lite_Ctrl()) init Axi_Lite_Ctrl.IDLE//当前状态用IDLE初始化
    val nextState = Axi_Lite_Ctrl()
    currentState := nextState


	Axi_Lite_Port.aw.valid := False
	Axi_Lite_Port.w.valid := False
	Axi_Lite_Port.ar.valid := False
	Axi_Lite_Port.r.ready := True
	Axi_Lite_Port.b.ready := True
	Axi_Lite_Port.w.payload.data :=0
	Axi_Lite_Port.aw.payload.addr :=0
	Axi_Lite_Port.ar.payload.addr := Read_Addr
	Axi_Lite_Port.ar.payload.prot:=B"3'b000"
	Axi_Lite_Port.aw.payload.prot:=B"3'b000"
	Axi_Lite_Port.w.payload.strb:=B"4'b0000"
	when(nextState===Axi_Lite_Ctrl.SWITCH_STATE){
		Read_Inst_EN:=True//只有在Switch状态才会解析指令是读还是写,其余时刻不解析
	}otherwise{
		Read_Inst_EN:=False
	}
	switch(currentState) {
		is(Axi_Lite_Ctrl.IDLE){
			when(start){
				nextState:=Axi_Lite_Ctrl.SWITCH_STATE
			}otherwise{
				Axi_Lite_Port.aw.valid := False
				Axi_Lite_Port.w.valid := False
				Axi_Lite_Port.ar.valid := False
				Axi_Lite_Port.r.ready := True
				Axi_Lite_Port.b.ready := True
				nextState:=Axi_Lite_Ctrl.IDLE
			}
		}//如果启动信号来了,那么就进入传指令状态机
		is(Axi_Lite_Ctrl.SWITCH_STATE){
			when(Inst_Two_High===B"8'h10"){
				Axi_Lite_Port.aw.valid := True
				Axi_Lite_Port.w.valid := True
				Axi_Lite_Port.w.payload.data := Write_Data
				Axi_Lite_Port.aw.payload.addr := Write_Addr
				nextState:=Axi_Lite_Ctrl.WRITE_LITE
			}elsewhen(Inst_Two_High===B"8'h11") {
				Axi_Lite_Port.ar.payload.prot:=B"3'b110"
				Axi_Lite_Port.ar.valid := True
				Axi_Lite_Port.r.ready := True
				nextState:=Axi_Lite_Ctrl.READ_LITE
			}otherwise{
				nextState:=Axi_Lite_Ctrl.IDLE
			}
		}
		is(Axi_Lite_Ctrl.WRITE_LITE){//如果wvalid和awvalid拉高了说明数据写过去了,接下来就是等那边接受完数据,进入写等待状态
			when(Axi_Lite_Port.aw.ready&&Axi_Lite_Port.w.ready){
				nextState:=Axi_Lite_Ctrl.SWITCH_STATE
				Axi_Lite_Port.aw.valid := False
				Axi_Lite_Port.w.valid := False
				Axi_Lite_Port.aw.payload.prot:=B"3'b110"
				Axi_Lite_Port.w.payload.strb:=B"4'b01111"
			}otherwise{
				nextState:=Axi_Lite_Ctrl.WRITE_LITE

			}
		}
		is(Axi_Lite_Ctrl.READ_LITE){
			// when(Axi_Lite_Port.ar.ready){//
			// 	nextState:=Axi_Lite_Ctrl.READ_LITE
			// 	Axi_Lite_Port.ar.valid := False
			// }else
			when(Axi_Lite_Port.ar.ready&&Axi_Lite_Port.r.valid){
				nextState:=Axi_Lite_Ctrl.SWITCH_STATE
				Axi_Lite_Port.r.ready := False
			}otherwise{
				nextState:=Axi_Lite_Ctrl.READ_LITE
			}
		}
	}
}
case class AxiLite_Control() extends Component{
	val IO=new Bundle{
		val Inst_In=in Bits(64 bits)//64位输入指令
		val Start_Signal=in Bool(1 bits)//开始信号
		val Axi_Lite_Port=master(AxiLite4(32, 32))//lite口
		val Read_Inst_En=out Bool()//读指令使能
	}
	noIoPrefix()
	AxiLite4SpecRenamer(IO.Axi_Lite_Port)
	val Fsm=Axi_Lite_Fsm(IO.Axi_Lite_Port,this.clockDomain)
	Fsm.Inst_Two_High:=IO.Inst_In(63 downto 56)
	Fsm.Read_Addr:=(B"24'b0"##IO.Inst_In(39 downto 32)).asUInt
	Fsm.Write_Addr:=(B"24'b0"##IO.Inst_In(39 downto 32)).asUInt
	Fsm.start:=IO.Start_Signal
	Fsm.Write_Data:=IO.Inst_In(31 downto 0)
	IO.Read_Inst_En:=Fsm.Read_Inst_EN

}
case class Axi_Lite_Driver_Test()extends Component{
	val Axi_Lite_Port=master(AxiLite4(log2Up(1 MiB), 32))
	Axi_Lite_Port.aw.valid := False
	Axi_Lite_Port.w.valid := False
	Axi_Lite_Port.ar.valid := False
	Axi_Lite_Port.r.ready := True
	Axi_Lite_Port.b.ready := True
}

//10天之后已经看不懂自己写的什么鬼了...
object TbConv extends App { 
    val verilog_path="./testcode_gen" 
    SpinalConfig(targetDirectory=verilog_path,defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(AxiLite_Control())
	SpinalConfig(targetDirectory=verilog_path,defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(AxiLite_Control_RWF())
}
