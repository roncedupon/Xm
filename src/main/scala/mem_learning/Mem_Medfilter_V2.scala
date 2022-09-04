package mem_learning
import spinal.core._
import spinal.lib.master
import spinal.lib.slave
import Archive.WaCounter
import spinal.lib.Delay
//第一版的重新修改,将输出那边的两级寄存器改为一级,希望能解决第一个点丢失的问题

class Mem_Medfilter_V2 extends Component{
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
	val Wr_En=RegNext(io.sData.fire)//RegNext()//RegNext()//RegNext(io.sData.fire)init(False)//Reg(Bool())init(False)//写使能
	//Wr_En:=io.sData.fire

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

	//最下面的直接连到输出
	// Shift_Register_Even(0):=RegNext(Shift_Register_Even(1))
	// Shift_Register_Even(1):=RegNext(Shift_Register_Even(2))
	// Shift_Register_Even(2):=RegNext(Shift_Register_Even(3))
	// Shift_Register_Even(3):=RegNext(Shift_Register_Even(4))
	// Shift_Register_Even(4):=RegNext(io.sData.payload(15 downto 0))
	
	Shift_Register_UnEven(0):=RegNext(Shift_Register_UnEven(1))
	Shift_Register_UnEven(1):=RegNext(Shift_Register_UnEven(2))
	Shift_Register_UnEven(2):=RegNext(Shift_Register_UnEven(3))
	Shift_Register_UnEven(3):=RegNext(Shift_Register_UnEven(4))
	Shift_Register_UnEven(4):=RegNext(io.sData.payload(31 downto 16))

	//开始掐头去尾======================================================
	val Start_Send=Reg(Bool())init(False)
	val Stop_Send=Reg(Bool())init(False)
	val Fsm_Qtqw=Wait_5_Qtqw(Start_Send,Stop_Send)//掐头去尾状态机
	val Wait_5_Cnt=WaCounter(Fsm_Qtqw.currentState===Wait5Enum.WAIT_5&&Fsm.currentState===MedEnum.WAIT_LAST_ROW,3,3)//创建列计数器,这里是4还是5是看仿真看出来的....
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

	val mem=Array.tabulate(8)(i=>{
		def gen():Mem[UInt]={//在循环中定义一个函数,这个函数在每次循环都会返回一个UInt的mem
			val mem=Mem(UInt(Config.BRAM_IN_DATA_WIDTH bits),wordCount=Config.BRAM_DEPTH)//
			mem.write(WrAddr_Cnt,WrData(i),Wr_En)//就是这里写数据延迟了一拍,然后就解决了第一个点被丢掉的问题
			RdData(i):=mem.readSync(RdAddr_Cnt)//,Bram的数据读出来
			//这里有个读写冲突,如果一开始读地址和写地址初始化都为0,那么当fire来了,就是同时从这个地址读写,所以读出来的是个x,也就是前两个点被丢了
			//开始连线:实现从左往右的卷积---只要4个Bram
			if(i<4){//处理0,2,4,6的ram---
				for(j<-1 to 4){//j+5*i:
				//0,1,2,3,4
				//5,6,7,8,9
					// when(Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID){
					// 	io.mData.mData_flow(j+5*i).payload:=RegNext(io.mData.mData_flow(j+5*i-1).payload)//,Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID)
					// }otherwise{
					// 	io.mData.mData_flow(j+5*i).payload:=0//,Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID)
					// }
					io.mData.mData_flow(j+5*i).payload:=RegNext(io.mData.mData_flow(j+5*i-1).payload)//RegNext()//,Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID)
				}//
				//io.mData.mData_flow(1+5*i).payload:=io.mData.mData_flow(5*i).payload//RegNext()//RegNext()//,Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID)
				//从上往下,20个点
				//每个ram负责输出5*2=10个点
				// when(Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID){
				// 	io.mData.mData_flow(5*i).payload:=RegNext(RdData(2*i)(15 downto 0))//低16位为双数
				// }otherwise{
				// 	io.mData.mData_flow(5*i).payload:=0
				// }
				io.mData.mData_flow(5*i).payload:=RegNext(RdData(2*i)(15 downto 0))//RegNext()//低16位为双数
				// Ram_Out_Even_Vec(i)(4):=RegNext(Ram_Out_Even_Vec(i)(3))
				// Ram_Out_Even_Vec(i)(3):=RegNext(Ram_Out_Even_Vec(i)(2))
				// Ram_Out_Even_Vec(i)(2):=RegNext(Ram_Out_Even_Vec(i)(1))
				// Ram_Out_Even_Vec(i)(1):=RegNext(Ram_Out_Even_Vec(i)(0))
				// Ram_Out_Even_Vec(i)(0):=RegNext(RdData(i)(15 downto 0))
				// //Ram_Out_Even_Vec(i)(0):=RdData(i)(15 downto 0)
				// //为啥这里的第0个Ram_Out_Even_Vec需要RegNext而不是直接赋值?
				// //因为看仿真调出来的,,,,,现在也知不道为啥
				// //但是这样的话最开始的两个点会被丢掉...我敲...
				Ram_Out_UnEven_Vec(2*i)(4):=RegNext(Ram_Out_UnEven_Vec(2*i)(3))
				Ram_Out_UnEven_Vec(2*i)(3):=RegNext(Ram_Out_UnEven_Vec(2*i)(2))
				Ram_Out_UnEven_Vec(2*i)(2):=RegNext(Ram_Out_UnEven_Vec(2*i)(1))
				Ram_Out_UnEven_Vec(2*i)(1):=RegNext(Ram_Out_UnEven_Vec(2*i)(0))//慢了读地址两拍,读出来,一个寄存器,RegNext又是一个寄存器
				Ram_Out_UnEven_Vec(2*i)(0):=RegNext(RdData(2*i)(31 downto 16))
				//Ram_Out_UnEven_Vec(i)(0):=RdData(i)(31 downto 16)
			}
			mem
		}
		gen()
	})

	// when(Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID){
	// 	io.mData.mData_flow(24).payload:=RegNext(io.mData.mData_flow(23).payload)
	// 	io.mData.mData_flow(23).payload:=RegNext(io.mData.mData_flow(22).payload)
	// 	io.mData.mData_flow(22).payload:=RegNext(io.mData.mData_flow(21).payload)
	// 	io.mData.mData_flow(21).payload:=RegNext(io.mData.mData_flow(20).payload)
	// 	io.mData.mData_flow(20).payload:=io.sData.payload(15 downto 0)//RegNext()
	// }otherwise{
	// 	io.mData.mData_flow(24).payload:=0//RegNext(io.mData.mData_flow(23).payload)
	// 	io.mData.mData_flow(23).payload:=0//RegNext(io.mData.mData_flow(22).payload)
	// 	io.mData.mData_flow(22).payload:=0//RegNext(io.mData.mData_flow(21).payload)
	// 	io.mData.mData_flow(21).payload:=0//RegNext(io.mData.mData_flow(20).payload)
	// 	io.mData.mData_flow(20).payload:=0//io.sData.payload(15 downto 0)//RegNext()
	// }
	io.mData.mData_flow(24).payload:=RegNext(io.mData.mData_flow(23).payload)
	io.mData.mData_flow(23).payload:=RegNext(io.mData.mData_flow(22).payload)
	io.mData.mData_flow(22).payload:=RegNext(io.mData.mData_flow(21).payload)
	io.mData.mData_flow(21).payload:=RegNext(io.mData.mData_flow(20).payload)
	io.mData.mData_flow(20).payload:=RegNext(io.sData.payload(15 downto 0))//RegNext()
	// for(i<-0 to 24){
	// 	when(Fsm_Qtqw.currentState!==Wait5Enum.DATA_VALID){
	// 		io.mData.mData_flow(i).payload:=0
	// 	}
	// }

	for(i<-25 to 49)
		io.mData.mData_flow(i).payload:=0
}
