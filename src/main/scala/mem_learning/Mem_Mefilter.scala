package mem_learning
import spinal.core._
import spinal.lib.master
import spinal.lib.slave
import Archive.WaCounter
import spinal.lib.Delay
//实现中值滤波9个ram从上往下,步长为2的卷积
//关于卷积操作:从上往下步长为2,从左往右步长为1,但是一次能出4个点
//目前有三种方法
	//第一种:进128出16,形成流水
	//第二种:进32出32,形成流水(目前)
	//第三种:进64出64,形成流水
//2022/08/4---从上往下卷积
case class GeneratePorts(dataWidth: Int, Num: Int) extends Bundle {
    val mData_flow = Vec(master Flow UInt(dataWidth bits), Num)//Flow 数据,不需要ready,直接流入下面
}
case class MemConfig() {
	val BRAM_IN_DATA_WIDTH=32
	val BRAM_OUT_DATA_WIDTH=32//还没学会怎么进128出16,但是想到了一种进64出64的办法
	val BRAM_DEPTH=(2048)/(BRAM_IN_DATA_WIDTH/16)//BRAM 数据深度
	//一次进64也就是4个点，所以需要进2048/4=512次，可以认为列计数为512
	val COl_CNT_NUM=(2048)/(BRAM_IN_DATA_WIDTH/16)
	val ROW_CNT_NUM=2048//行计数
	val BRAM_NUM=8
}//中值滤波配置
//=====================================FSM=======================================
object MedEnum extends SpinalEnum(defaultEncoding = binaryOneHot) {
    val IDLE, INIT, WAIT_9_ROWS,WAIT_LAST_ROW= newElement//这些状态应该都被用到,不然会有latch,不知道为啥
	//IDLE
	//INIT 初始化,等一下然后开始跑
	//WAIT_9_ROWS 缓存前9行
	//START_OUTPUT 缓存完前9行后,第十行开始缓存,这时开始往外面发数据
	//END 发完最后一行最后一列数据后END
}
object Wait5Enum extends SpinalEnum(defaultEncoding = binaryOneHot) {
	//用来掐头去尾,每拿到一个col valid,等5拍再往外面发送有效数据
    val WAIT_5, DATA_VALID= newElement//这些状态应该都被用到,不然会有latch,不知道为啥
	//IDLE
	//INIT 初始化,等一下然后开始跑
	//WAIT_9_ROWS 缓存前9行
	//START_OUTPUT 缓存完前9行后,第十行开始缓存,这时开始往外面发数据
	//END 发完最后一行最后一列数据后END
}
case class Wait_5_Qtqw(Start_Send:Bool,Stop_Send:Bool) extends Area{
	val currentState = Reg(Wait5Enum()) init Wait5Enum.WAIT_5//初始时一直等
    val nextState = Wait5Enum()
	currentState := nextState
	switch(currentState){
		is(Wait5Enum.WAIT_5){
			when(Start_Send){
				nextState:=Wait5Enum.DATA_VALID
			}otherwise{
				nextState:=Wait5Enum.WAIT_5
			}
		}
		is(Wait5Enum.DATA_VALID){
			when(Stop_Send){
				nextState:=Wait5Enum.WAIT_5
			}otherwise{
				nextState:=Wait5Enum.DATA_VALID
			}
		}
	}
}
case class MedFilterFsm(start: Bool) extends Area {
	val currentState = Reg(MedEnum()) init MedEnum.IDLE//初始化状态机为IDLE状态
    val nextState = MedEnum()
	currentState := nextState

	val INIT_END=Bool()//初始化,打多几拍
	val IS_9_ROWS_END=Bool()
	val IS_LAST_ROW=Bool()
	
	switch(currentState) {
		is(MedEnum.IDLE){
			when(start){
				nextState:=MedEnum.INIT
			}otherwise{
				nextState:=MedEnum.IDLE
			}
		}
		is(MedEnum.INIT){
			when(INIT_END){
				nextState:=MedEnum.WAIT_9_ROWS//等9行缓存
			}otherwise{
				nextState:=MedEnum.INIT
			}
		}		
		// is(MedEnum.FIFO_READY){
		// 	when()
		// }
		is(MedEnum.WAIT_9_ROWS){
			when(IS_9_ROWS_END){
				nextState:=MedEnum.WAIT_LAST_ROW
			}otherwise{
				nextState:=MedEnum.WAIT_9_ROWS
			}
		}

		is(MedEnum.WAIT_LAST_ROW){//列计数数满一行
			when(IS_LAST_ROW){
				nextState:=MedEnum.IDLE
			}otherwise{
				nextState:=MedEnum.WAIT_LAST_ROW
			}
		}		
		
	}
    //ps端将图片发到DDR后给一个启动信号
}
//===================================FSM END=====================================
class Mem_Medfilter extends Component{
	val Config=MemConfig()
	val io=new Bundle{
		val sData = slave Stream UInt(Config.BRAM_IN_DATA_WIDTH bits)
		val start =	in Bool()//Fsm启动信号
		val mData=GeneratePorts(16,50)
		//val input=Vec(out UInt(10 bits),10)
		//val mData = Vec(master Stream UInt(64 bits), 25)//25个16 bit输出,流水一个周期给到下层
	}
	noIoPrefix()
	//====================FSM=========================================
	val Fsm=MedFilterFsm(io.start)
		//只有在进入有效数据时才会计数
	val Col_Cnt=WaCounter(io.sData.valid&&io.sData.ready, log2Up(Config.COl_CNT_NUM), Config.COl_CNT_NUM-1)//创建列计数器
	val Row_Cnt_9=WaCounter(Col_Cnt.valid,log2Up(Config.ROW_CNT_NUM),8)//在进最后一行的时候，前面的那几行已经存满了，所以这里设置的应该是9而不是8
	val Row_Cnt_All=WaCounter(Col_Cnt.valid,log2Up(Config.ROW_CNT_NUM),Config.ROW_CNT_NUM-1)//这里到底是减1还是不减？----bug

	//初始化
	val INIT_CNT=WaCounter(Fsm.currentState === MedEnum.INIT, 3, 5)
	Fsm.INIT_END:=INIT_CNT.valid
	//等9行
	Fsm.IS_9_ROWS_END:=Row_Cnt_9.valid
	//等全部行
	Fsm.IS_LAST_ROW:=Row_Cnt_All.valid
	
	

	//====================FSM  END====================================
	//sData 控制====================================================
	// when(Fsm.currentState===MedEnum.INIT&&Fsm.nextState===MedEnum.WAIT_9_ROWS||Fsm.currentState===MedEnum.WAIT_9_ROWS&&Fsm.nextState===MedEnum.WAIT_LAST_ROW){
	// 	io.sData.ready:=True
	// }otherwise{
	// 	io.sData.ready:=False
	// }
	when(Fsm.currentState===MedEnum.WAIT_9_ROWS||Fsm.currentState===MedEnum.WAIT_LAST_ROW){//
		io.sData.ready:=True
	}otherwise{
		io.sData.ready:=False
	}

	//control signal
	val Bram_Chose=Reg(Bool())init(False)//Bram选择信号,一开始选第8个ram
	
	//写数据和读数据连线
	val WrData = Vec(UInt(Config.BRAM_IN_DATA_WIDTH bits), 10)//bram的写数据
    val RdData = Vec(UInt(Config.BRAM_OUT_DATA_WIDTH bits), 10)//bram的读数据

	val WrAddr_Cnt=Reg(UInt(log2Up(Config.BRAM_DEPTH) bits))//写地址计数器
	val RdAddr_Cnt=Reg(UInt(log2Up(Config.BRAM_DEPTH) bits))//读地址计数器
	WrAddr_Cnt:=RdAddr_Cnt//读地址比写地址慢一拍

	when(io.sData.fire){
		when(RdAddr_Cnt===Config.COl_CNT_NUM-1){
			RdAddr_Cnt:=0
		}otherwise{
			RdAddr_Cnt:=RdAddr_Cnt+1
		}
	}otherwise{
		RdAddr_Cnt:=0
	}
	//也可以用val WrAddr_Cnt:=RegNext(RdAddr_Cnt)
	val Wr_En=Reg(Bool())init(False)//写使能
	Wr_En:=io.sData.fire

//方案一========================================
	// WrData(0):=RdData(2)
	// WrData(2):=RdData(4)
	// WrData(4):=RdData(6)
	// WrData(6):=RdData(8)
	// WrData(8):=RegNextWhen(io.sData.payload,Bram_Chose) 
	
	// WrData(1):=RdData(3)
	// WrData(3):=RdData(5)
	// WrData(5):=RdData(7)
	// WrData(7):=RdData(9)//头尾相连
	// WrData(9):=RegNextWhen(io.sData.payload,!Bram_Chose)//从下往上,依次为0 2 4 6 8---1 3 5 7 9
	// when(Col_Cnt.valid){
	// 	Bram_Chose:=(!Bram_Chose)
	// }otherwise{
	// 	Bram_Chose:=Bram_Chose
	// }
//方案二===========================================
	//不用方案一的原因：不好整理数据格式(4个点的方案二)
	//方案二每发完一行都得停一行的时间，因为有重复的
	// WrData(0):=RdData(1)
	// WrData(1):=RdData(2)
	// WrData(2):=RdData(3)
	// WrData(3):=RdData(4)
	// WrData(4):=RdData(5)
	// WrData(5):=RdData(6)
	// WrData(6):=RdData(7)
	// WrData(7):=RdData(8)
	// WrData(8):=RdData(9)//头尾相连
	// WrData(9):=io.sData.payload//RegNextWhen(,!Bram_Chose)//从下往上,依次为0 2 4 6 8---1 3 5 7 9
//=================================================
//方案三============================================
	WrData(0):=RdData(1)
	WrData(1):=RdData(2)
	WrData(2):=RdData(3)
	WrData(3):=RdData(4)
	WrData(4):=RdData(5)
	WrData(5):=RdData(6)
	WrData(6):=RdData(7)
	WrData(7):=io.sData.payload//RegNext(io.sData.payload)
	when(Col_Cnt.valid){
		Bram_Chose:=(!Bram_Chose)
	}otherwise{
		Bram_Chose:=Bram_Chose
	}
	// val Bram_En=Vec(Bool(),2)
	// Bram_En(0):=Bram_Chose
	// Bram_En(1):=Bram_Chose
	val Shift_Register_Even=Vec(UInt(16 bits),5)
	val Shift_Register_UnEven=Vec(UInt(16 bits),5)

	val Ram_Out_Even_Vec=Vec(Vec(UInt(16 bits),5),Config.BRAM_NUM)
	val Ram_Out_UnEven_Vec=Vec(Vec(UInt(16 bits),5),Config.BRAM_NUM)
	val mem=Array.tabulate(Config.BRAM_NUM)(i=>{
		def gen():Mem[UInt]={//在循环中定义一个函数,这个函数在每次循环都会返回一个UInt的mem
			val mem=Mem(UInt(Config.BRAM_IN_DATA_WIDTH bits),wordCount=Config.BRAM_DEPTH)//
			mem.write(WrAddr_Cnt,RegNext(WrData(i)),Wr_En)
			RdData(i):=mem.readSync(RdAddr_Cnt)//没有寄存器,简单连线

			Ram_Out_Even_Vec(i)(4):=RegNext(Ram_Out_Even_Vec(i)(3))
			Ram_Out_Even_Vec(i)(3):=RegNext(Ram_Out_Even_Vec(i)(2))
			Ram_Out_Even_Vec(i)(2):=RegNext(Ram_Out_Even_Vec(i)(1))
			Ram_Out_Even_Vec(i)(1):=RegNext(Ram_Out_Even_Vec(i)(0))
			Ram_Out_Even_Vec(i)(0):=RegNext(RdData(i)(15 downto 0))
			//Ram_Out_Even_Vec(i)(0):=RdData(i)(15 downto 0)
			//为啥这里的第0个Ram_Out_Even_Vec需要RegNext而不是直接赋值?
			//因为看仿真调出来的,,,,,现在也知不道为啥
			//但是这样的话最开始的两个点会被丢掉...我敲...
			Ram_Out_UnEven_Vec(i)(4):=RegNext(Ram_Out_UnEven_Vec(i)(3))
			Ram_Out_UnEven_Vec(i)(3):=RegNext(Ram_Out_UnEven_Vec(i)(2))
			Ram_Out_UnEven_Vec(i)(2):=RegNext(Ram_Out_UnEven_Vec(i)(1))
			Ram_Out_UnEven_Vec(i)(1):=RegNext(Ram_Out_UnEven_Vec(i)(0))//慢了读地址两拍,读出来,一个寄存器,RegNext又是一个寄存器
			Ram_Out_UnEven_Vec(i)(0):=RegNext(RdData(i)(31 downto 16))
			//Ram_Out_UnEven_Vec(i)(0):=RdData(i)(31 downto 16)



			mem
		}
		gen()
	})
	//最下面的直接连到输出
	Shift_Register_Even(0):=RegNext(Shift_Register_Even(1))
	Shift_Register_Even(1):=RegNext(Shift_Register_Even(2))
	Shift_Register_Even(2):=RegNext(Shift_Register_Even(3))
	Shift_Register_Even(3):=RegNext(Shift_Register_Even(4))
	Shift_Register_Even(4):=RegNext(io.sData.payload(15 downto 0))
	
	Shift_Register_UnEven(0):=RegNext(Shift_Register_UnEven(1))
	Shift_Register_UnEven(1):=RegNext(Shift_Register_UnEven(2))
	Shift_Register_UnEven(2):=RegNext(Shift_Register_UnEven(3))
	Shift_Register_UnEven(3):=RegNext(Shift_Register_UnEven(4))
	Shift_Register_UnEven(4):=RegNext(io.sData.payload(31 downto 16))

	//开始掐头去尾======================================================
	val Start_Send=Reg(Bool())init(False)
	val Stop_Send=Reg(Bool())init(False)
	val Fsm_Qtqw=Wait_5_Qtqw(Start_Send,Stop_Send)//掐头去尾状态机
	val Wait_5_Cnt=WaCounter(Fsm_Qtqw.currentState===Wait5Enum.WAIT_5&&Fsm.currentState===MedEnum.WAIT_LAST_ROW,3,1)//创建列计数器,这里是4还是5是看仿真看出来的....
																													//更新:好像得是3...
	Start_Send:=Wait_5_Cnt.valid//开始发送:数完五拍后开始发送
	Stop_Send:=Col_Cnt.valid//结束发送:发送完一行就结束发送,酱紫,可能有bug
	// when(Fsm.currentState===MedEnum.WAIT_LAST_ROW){
	// 	when()
	// }otherwise{
	// 	Start_Send:=False
	// 	Stop_Send:=True
	// }

	//掐头去尾结束=======================================================
	
	val Out_25_Data=Bool()
	when(Fsm.currentState===MedEnum.WAIT_9_ROWS&&Fsm.nextState===MedEnum.WAIT_LAST_ROW){
		for(i<-0 to 49)
		io.mData.mData_flow(i).valid:=True
	}otherwise{
		for(i<-0 to 49)
		io.mData.mData_flow(i).valid:=False
	}
	// io.mData.mData_flow(24).payload:=Shift_Register_Even(4)//Delay(Shift_Register_Even(4),5)
	// io.mData.mData_flow(23).payload:=Shift_Register_Even(3)//Delay(Shift_Register_Even(3),4)
	// io.mData.mData_flow(22).payload:=Shift_Register_Even(2)//Delay(Shift_Register_Even(2),3)
	// io.mData.mData_flow(21).payload:=Shift_Register_Even(1)//Delay(Shift_Register_Even(1),2)
	// io.mData.mData_flow(20).payload:=Shift_Register_Even(0)//Delay(Shift_Register_Even(0),1)
	when(Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID){
		io.mData.mData_flow(24).payload:=Shift_Register_Even(4)//Delay(Shift_Register_Even(4),1)
		io.mData.mData_flow(23).payload:=Shift_Register_Even(3)//Delay(Shift_Register_Even(3),2)
		io.mData.mData_flow(22).payload:=Shift_Register_Even(2)//Delay(Shift_Register_Even(2),3)
		io.mData.mData_flow(21).payload:=Shift_Register_Even(1)//Delay(Shift_Register_Even(1),4)
		io.mData.mData_flow(20).payload:=Shift_Register_Even(0)//Delay(Shift_Register_Even(0),5)
		
		
		io.mData.mData_flow(19).payload:=Ram_Out_Even_Vec(0)(4)//Delay(Shift_Register_Even(4),1)
		io.mData.mData_flow(18).payload:=Ram_Out_Even_Vec(0)(3)//Delay(Shift_Register_Even(3),2)
		io.mData.mData_flow(17).payload:=Ram_Out_Even_Vec(0)(2)//Delay(Shift_Register_Even(2),3)
		io.mData.mData_flow(16).payload:=Ram_Out_Even_Vec(0)(1)//Delay(Shift_Register_Even(1),4)
		io.mData.mData_flow(15).payload:=Ram_Out_Even_Vec(0)(0)//Delay(Shift_Register_Even(0),5)
		
		io.mData.mData_flow(14).payload:=Ram_Out_Even_Vec(2)(4)//Delay(Shift_Register_Even(4),1)
		io.mData.mData_flow(13).payload:=Ram_Out_Even_Vec(2)(3)//Delay(Shift_Register_Even(3),2)
		io.mData.mData_flow(12).payload:=Ram_Out_Even_Vec(2)(2)//Delay(Shift_Register_Even(2),3)
		io.mData.mData_flow(11).payload:=Ram_Out_Even_Vec(2)(1)//Delay(Shift_Register_Even(1),4)
		io.mData.mData_flow(10).payload:=Ram_Out_Even_Vec(2)(0)//Delay(Shift_Register_Even(0),5)
		
		io.mData.mData_flow( 9).payload:=Ram_Out_Even_Vec(4)(4)//Delay(Shift_Register_Even(4),1)
		io.mData.mData_flow( 8).payload:=Ram_Out_Even_Vec(4)(3)//Delay(Shift_Register_Even(3),2)
		io.mData.mData_flow( 7).payload:=Ram_Out_Even_Vec(4)(2)//Delay(Shift_Register_Even(2),3)
		io.mData.mData_flow( 6).payload:=Ram_Out_Even_Vec(4)(1)//Delay(Shift_Register_Even(1),4)
		io.mData.mData_flow( 5).payload:=Ram_Out_Even_Vec(4)(0)//Delay(Shift_Register_Even(0),5)
		 
		io.mData.mData_flow( 4).payload:=Ram_Out_Even_Vec(6)(4)//Delay(Shift_Register_Even(4),1)
		io.mData.mData_flow( 3).payload:=Ram_Out_Even_Vec(6)(3)//Delay(Shift_Register_Even(3),2)
		io.mData.mData_flow( 2).payload:=Ram_Out_Even_Vec(6)(2)//Delay(Shift_Register_Even(2),3)
		io.mData.mData_flow( 1).payload:=Ram_Out_Even_Vec(6)(1)//Delay(Shift_Register_Even(1),4)
		io.mData.mData_flow( 0).payload:=Ram_Out_Even_Vec(6)(0)//Delay(Shift_Register_Even(0),5)

	}otherwise{
		io.mData.mData_flow(24).payload:=0
		io.mData.mData_flow(23).payload:=0
		io.mData.mData_flow(22).payload:=0
		io.mData.mData_flow(21).payload:=0
		io.mData.mData_flow(20).payload:=0

		io.mData.mData_flow(19).payload:=0
		io.mData.mData_flow(18).payload:=0
		io.mData.mData_flow(17).payload:=0
		io.mData.mData_flow(16).payload:=0
		io.mData.mData_flow(15).payload:=0

		io.mData.mData_flow(14).payload:=0
		io.mData.mData_flow(13).payload:=0
		io.mData.mData_flow(12).payload:=0
		io.mData.mData_flow(11).payload:=0
		io.mData.mData_flow(10).payload:=0

		io.mData.mData_flow( 9).payload:=0
		io.mData.mData_flow( 8).payload:=0
		io.mData.mData_flow( 7).payload:=0
		io.mData.mData_flow( 6).payload:=0
		io.mData.mData_flow( 5).payload:=0

		io.mData.mData_flow( 4).payload:=0
		io.mData.mData_flow( 3).payload:=0
		io.mData.mData_flow( 2).payload:=0
		io.mData.mData_flow( 1).payload:=0
		io.mData.mData_flow( 0).payload:=0
	}	
	// for(i<-0 to 19)
	// 	io.mData.mData_flow(i).payload:=0
	for(i<-25 to 49)
		io.mData.mData_flow(i).payload:=0
}
