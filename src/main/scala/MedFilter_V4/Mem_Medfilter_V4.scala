package MedFilter_V4
import spinal.core._
import spinal.lib.master
import spinal.lib.slave
import Archive.WaCounter
import spinal.lib.Delay
//第一版的重新修改,将输出那边的两级寄存器改为一级,希望能解决第一个点丢失的问题

class Mem_Medfilter_V4 extends Component{
	val Config=MemConfig()
	val io=new Bundle{
		val sData = slave Stream UInt(Config.BRAM_IN_DATA_WIDTH bits)
		val start =	in Bool()//Fsm启动信号
		val mData=GeneratePorts(16,50)
		val mValid=out Bool()
		//val input=Vec(out UInt(10 bits),10)
		//val mData = Vec(master Stream UInt(64 bits), 25)//25个16 bit输出,流水一个周期给到下层
	}
	noIoPrefix()
	val mData_flow=Vec(UInt(16 bits),50)
	//====================FSM=========================================
	val Fsm=MedFilterFsm(io.start)
		//只有在进入有效数据时才会计数
	val Col_Cnt=WaCounter(io.sData.valid&&io.sData.ready, log2Up(Config.COl_CNT_NUM)+3, Config.COl_CNT_NUM-1)//创建列计数器
	val Row_Cnt_9=WaCounter(Col_Cnt.valid,log2Up(Config.ROW_CNT_NUM),8)//在进最后一行的时候，前面的那几行已经存满了，所以这里设置的应该是9而不是8
	val Row_Cnt_All=WaCounter(Col_Cnt.valid,log2Up(Config.ROW_CNT_NUM),Config.ROW_CNT_NUM-1)//这里到底是减1还是不减？----bug

	//初始化
	val INIT_CNT=WaCounter(Fsm.currentState === MedEnum.INIT, 3, 5)//初始化计数器,数五拍
	Fsm.INIT_END:=INIT_CNT.valid
	//等9行
	Fsm.IS_9_ROWS_END:=Row_Cnt_9.valid
	//等全部行
	Fsm.IS_LAST_ROW:=Row_Cnt_All.valid
	
	

	//====================FSM  END====================================
	//sData 控制====================================================

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

	//val WrAddr_Cnt=Reg(UInt(log2Up(Config.BRAM_DEPTH) bits))//写地址计数器
	val RdAddr_Cnt=Reg(UInt(log2Up(Config.BRAM_DEPTH) bits))init(0)//读地址计数器
	val WrAddr_Cnt=RegNext(RdAddr_Cnt)//读地址比写地址慢一拍

	when(io.sData.fire){
		when(RdAddr_Cnt===Config.COl_CNT_NUM-1){
			RdAddr_Cnt:=0
		}otherwise{
			RdAddr_Cnt:=RdAddr_Cnt+1
		}
	}otherwise{
		RdAddr_Cnt:=RdAddr_Cnt
	}
	//也可以用val WrAddr_Cnt:=RegNext(RdAddr_Cnt)
	val Wr_En=RegNext(io.sData.fire)//RegNext()//RegNext()//RegNext()//RegNext(io.sData.fire)init(False)//Reg(Bool())init(False)//写使能
	//Wr_En:=io.sData.fire
	//写地址慢读地址一拍,写使能也应该慢一拍吧??????
	//2022/08/09更新---终于他喵的对上了
	//写地址应该比读地址慢一拍,那么写使能也应该慢一拍到,同样地,写数据也要慢一拍

	WrData(0):=RdData(1)
	WrData(1):=RdData(2)
	WrData(2):=RdData(3)
	WrData(3):=RdData(4)
	WrData(4):=RdData(5)
	WrData(5):=RdData(6)
	WrData(6):=RdData(7)
	WrData(7):=RegNext(io.sData.payload)//RegNext()//RegNext()//RegNext(io.sData.payload)
	//↑此处非常重要,多看多看22/08/09,要知道只有第7个需要regnext的原因
	when(Col_Cnt.valid){
		Bram_Chose:=(!Bram_Chose)
	}otherwise{
		Bram_Chose:=Bram_Chose
	}


	//开始掐头去尾======================================================
	val Start_Send=Bool()//Reg(Bool())init(False)//开始发送
	val Stop_Send=Bool()//Reg(Bool())init(False)//停止发送,数完一行就停下来
	val Wait_9_Rows_Start=Bool()
	val Fsm_Qtqw=Wait_5_Qtqw(Wait_9_Rows_Start,Start_Send,Stop_Send)//掐头去尾状态机
	//val Wait_5_Cnt=WaCounter(Fsm_Qtqw.currentState===Wait5Enum.WAIT_5&&Fsm.currentState===MedEnum.WAIT_LAST_ROW,3,3)//创建列计数器,这里是4还是5是看仿真看出来的....
	Fsm_Qtqw.Wait_9_Rows_End:=(Fsm.currentState===MedEnum.WAIT_9_ROWS&&Fsm.nextState===MedEnum.WAIT_LAST_ROW)		
	Wait_9_Rows_Start:=(Fsm.currentState===MedEnum.INIT&&Fsm.nextState===MedEnum.WAIT_9_ROWS)		
	Fsm_Qtqw.Send_All_Rows:=(Fsm.currentState===MedEnum.WAIT_LAST_ROW&&Fsm.nextState===MedEnum.IDLE)																												//更新:好像得是3...

	//Start_Send:=Mux(io.sData.fire,Delay(Stop_Send,4),False)//这样居然会碰到一个完美的bug....
	//如果mvalid只拉高512下,但是数据由1024个,stop_send在第1024下拉高,向上面那样写的话Start_Send永远不会拉高了
	Start_Send:=Delay(Stop_Send,4)//RegNext()//开始发送:数完x拍后开始发送
	Stop_Send:=RegNext(Col_Cnt.valid)//||(!io.sData.fire)//RegNext()//结束发送:发送完一行就结束发送,酱紫,可能有bug
	//这里也有点东西..
	// when(Fsm.currentState===MedEnum.WAIT_LAST_ROW){
	// 	when()
	// }otherwise{
	// 	Start_Send:=False
	// 	Stop_Send:=True
	// }

	//掐头去尾结束=======================================================
	//Tmp数据状态机
	val Fsm_Data_Valid=Data_Valid(Fsm.currentState===MedEnum.WAIT_LAST_ROW&&Start_Send)
	Fsm_Data_Valid.Mvalid_Clear:=(!io.sData.valid)
	Fsm_Data_Valid.Mvalid_Set:=(io.sData.valid)
	Fsm_Data_Valid.Tmp_Data_End:=Stop_Send
	Fsm_Data_Valid.Wait_5_Flag:=Delay(Fsm_Data_Valid.currentState===Valid_Enum.DATA_UNVALID_TMP&&(Fsm_Data_Valid.nextState===Valid_Enum.WAIT_5),3)
	when(Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID&&io.sData.fire){
		// for(i<-0 to 49){
		// 	io.mData.mData_flow(i).valid:=True
		// }
		io.mValid:=True
	}otherwise{
		// for(i<-0 to 49){
		// 	io.mData.mData_flow(i).valid:=False
		// }
		
		io.mValid:=False
	}
	val mem=Array.tabulate(8)(i=>{
		def gen():Mem[UInt]={//在循环中定义一个函数,这个函数在每次循环都会返回一个UInt的mem
			val mem=Mem(UInt(Config.BRAM_IN_DATA_WIDTH bits),wordCount=Config.BRAM_DEPTH)//
			mem.write(WrAddr_Cnt,RegNext(WrData(i)),Wr_En)//就是这里写数据延迟了一拍,然后就解决了第一个点被丢掉的问题
			RdData(i):=mem.readSync(RdAddr_Cnt,io.sData.valid)//,Bram的数据读出来
			//这里有个读写冲突,如果一开始读地址和写地址初始化都为0,那么当fire来了,就是同时从这个地址读写,所以读出来的是个x,也就是前两个点被丢了
			//开始连线:实现从左往右的卷积---只要4个Bram
			if(i<4){//处理0,2,4,6的ram---
				for(j<-1 to 4){//j+5*i:
					mData_flow(j+5*i):=RegNext(mData_flow(j+5*i-1))//RegNext()//RegNext()//,Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID)
					mData_flow(j+5*i+25):=RegNext(mData_flow(j+5*i-1+25))//RegNext()//RegNext()//,Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID)
				}//
				mData_flow(5*i):=RegNext(RdData(2*i)(15 downto 0))//RegNext()//RegNext()//低16位为双数
				mData_flow(5*i+25):=RegNext(RdData(2*i)(31 downto 16))//RegNext()//RegNext()//高16位为双数
			}
			mem
		}
		gen()
	})

	mData_flow(24):=RegNext(mData_flow(23))
	mData_flow(23):=RegNext(mData_flow(22))
	mData_flow(22):=RegNext(mData_flow(21))
	mData_flow(21):=RegNext(mData_flow(20))
	mData_flow(20):=RegNext(io.sData.payload(15 downto 0))//RegNext()//RegNext()


	mData_flow(49):=RegNext(mData_flow(48))
	mData_flow(48):=RegNext(mData_flow(47))
	mData_flow(47):=RegNext(mData_flow(46))
	mData_flow(46):=RegNext(mData_flow(45))
	mData_flow(45):=RegNext(io.sData.payload(31 downto 16))//RegNext()//RegNext()

	val valid = Vec(Reg(Bool()) init False, 50)
	val WAIT_LAST_ROW_Flag=Bool()
	WAIT_LAST_ROW_Flag:=(Fsm.currentState===MedEnum.WAIT_LAST_ROW)//&&(Fsm_Data_Valid.currentState===Valid_Enum.DATA_VALID_TMP)
	io.mData.mData_flow(0).valid:=Delay(valid(0),4)
	io.mData.mData_flow(1).valid:=Delay(valid(1),3)
	io.mData.mData_flow(2).valid:=Delay(valid(2),2)
	io.mData.mData_flow(3).valid:=Delay(valid(3),1)
	io.mData.mData_flow(4).valid:=valid(4)//Delay(,)
	when(WAIT_LAST_ROW_Flag){
		when(RegNext(Col_Cnt.count)<2047-4){
			valid(0):=True
		}otherwise{
			valid(0):=False
		}
		when(RegNext(Col_Cnt.count)>0&&RegNext(Col_Cnt.count)<2047-3){
			valid(1):=True
		}otherwise{
			valid(1):=False
		}
		when(RegNext(Col_Cnt.count)>1&&RegNext(Col_Cnt.count)<2047-2){
			valid(2):=True
		}otherwise{
			valid(2):=False
		}
		when(RegNext(Col_Cnt.count)>2&&RegNext(Col_Cnt.count)<2047-1){
			valid(3):=True
		}otherwise{
			valid(3):=False
		}
		when(RegNext(Col_Cnt.count)>3&&RegNext(Col_Cnt.count)<2047){
			valid(4):=True
		}otherwise{
			valid(4):=False
		}
	}otherwise{
		valid.map(_ := False)
	}

	for(i<-0 to 49){
		io.mData.mData_flow(i).payload:=mData_flow(i)
		// when((Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID)){//&&RegNext(io.sData.fire)
		// 	io.mData(i):=mData_flow(i)//加后面那个或判断
		// }otherwise{
		// 	io.mData(i):=0
		// }
	}
	for(i<-5 to 49){
		io.mData.mData_flow(i).valid:=False
	}
	// when(Fsm.currentState===MedEnum.WAIT_LAST_ROW){
	// 	for(i<-0 to 9){
	// 		for(j<-0 to 4){
	// 			io.mData.mData_flow(i*5+j).valid:=Delay(io.sData.valid,j)
	// 		}
	// 	}
	// }otherwise{
	// 	for(i<-0 to 9){
	// 		for(j<-0 to 4){
	// 			io.mData.mData_flow(i*5+j).valid:=False
	// 		}
	// 	}
	// }

	val Valid_FLag=Bool()
	Valid_FLag:=Fsm.currentState===MedEnum.WAIT_LAST_ROW&&Delay(io.sData.valid,5)
	// when(Delay(io.sData.valid,5)){
	// 	Valid_FLag:=True
	// }otherwise{
	// 	Valid_FLag:=False
	// }
}
