package MedFilter_V6
import spinal.core._
import spinal.lib.slave
import spinal.lib.master
import Archive.WaCounter
import spinal.lib.Delay
import spinal.lib.StreamFifo
class Fifo_Test extends Component{
    val Fifo = new StreamFifo(
        dataType = UInt(16 bits),
        depth = 16//fifo的深度至少是5吧?
      )
    val io=new Bundle{
        val Data_In=slave Stream(UInt(16 bits))
        val Data_out=out UInt(16 bits)
        val fifo_ready=in Bool()//这个fifo_ready说的是下一层准备好接收fifo来的数据了，而下面状态机里面的fifo_ready说的是这个fifo准备好接收滤波完的数据
        val Pop_Valid=out Bool()
        //两个fifo_ready的方向一个进一个出，不要弄混了
    }
    
    io.Pop_Valid:=Fifo.io.pop.valid
    Fifo.io.push<>io.Data_In
    Fifo.io.pop.ready:=io.fifo_ready
    io.Data_out:=Fifo.io.pop.payload
}
case class MemConfig() {
    val BRAM_IN_DATA_WIDTH=32
    val BRAM_OUT_DATA_WIDTH=32//还没学会怎么进128出16,但是想到了一种进64出64的办法
    val BRAM_DEPTH=(2048)/(BRAM_IN_DATA_WIDTH/16)//BRAM 数据深度
    //一次进64也就是4个点，所以需要进2048/4=512次，可以认为列计数为512
    val COl_CNT_NUM=(2048)/(BRAM_IN_DATA_WIDTH/16)//假如2048个点,一次进32bit,一次读32bit,那么需要1024个地址
    val ROW_CNT_NUM=2048//行计数
    val BRAM_NUM=8
}//中值滤波配置
object MedEnum extends SpinalEnum(defaultEncoding = binaryOneHot) {
    val IDLE, INIT, WAIT_9_ROWS,WAIT_LAST_ROW,COl_END,WAIT_FIFO_READY= newElement//这些状态应该都被用到,不然会有latch,不知道为啥
}
case class MedFilterFsm(start: Bool) extends Area {
    val currentState = Reg(MedEnum()) init MedEnum.IDLE//初始化状态机为IDLE状态
    val nextState = MedEnum()
    currentState := nextState
    val INIT_END=Bool()//初始化,打多几拍
    val IS_9_ROWS_END=Bool()
    val IS_LAST_ROW=Bool()
    val Col_End=Bool()
    val Col_End_End=Bool()
	val Fsm_Fifo_Ready=Bool()
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
                nextState:=MedEnum.WAIT_FIFO_READY//等9行缓存
            }otherwise{
                nextState:=MedEnum.INIT
            }
        }
		is(MedEnum.WAIT_FIFO_READY){
			when(Fsm_Fifo_Ready){
				when(!IS_9_ROWS_END){
					nextState:=MedEnum.WAIT_9_ROWS
				}otherwise{
					nextState:=MedEnum.WAIT_LAST_ROW
				}
			}otherwise{
				nextState:=MedEnum.WAIT_FIFO_READY
			}
		}
        is(MedEnum.WAIT_9_ROWS){
            when(Col_End){
                nextState:=MedEnum.COl_END//进入col_end判断
            }otherwise{
                nextState:=MedEnum.WAIT_9_ROWS
            }
        }
        is(MedEnum.WAIT_LAST_ROW){//列计数数满一行
            when(Col_End){
                nextState:=MedEnum.COl_END
            }otherwise{
                nextState:=MedEnum.WAIT_LAST_ROW
            }
        }
        is(MedEnum.COl_END){
            when(IS_LAST_ROW){
                nextState:=MedEnum.IDLE
            }elsewhen(Col_End_End){//因为一行完了,可能会等一段时间,而这一段时间col_valid会一直拉高
                nextState:=MedEnum.WAIT_FIFO_READY
            }otherwise{
                nextState:=MedEnum.COl_END
            }
        }       
        
    }
    //ps端将图片发到DDR后给一个启动信号
}
class MedFilter_V6 extends Component{
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(32 bits))
        val mData=out Vec(UInt(16 bits), 50)
		val mValid=out Bool()//输出数据是否有效信号
        val mReady=in Bool()//下层给上层的ready信号
        val start=in Bool()

    }
    val start_not=RegNext(io.start)
    noIoPrefix()
    val WrData = Vec(UInt(Config.BRAM_IN_DATA_WIDTH bits), 10)//bram的写数据
    val RdData = Vec(UInt(Config.BRAM_OUT_DATA_WIDTH bits), 10)//bram的读数据
    WrData(0):=RdData(1)//RegNext(RdData(1))
    WrData(1):=RdData(2)//RegNext(RdData(2))
    WrData(2):=RdData(3)//RegNext(RdData(3))
    WrData(3):=RdData(4)//RegNext(RdData(4))
    WrData(4):=RdData(5)//RegNext(RdData(5))
    WrData(5):=RdData(6)//RegNext(RdData(6))
    WrData(6):=RdData(7)//RegNext(RdData(7))
    WrData(7):=RegNext(io.sData.payload)//RegNext()//RegNext()//RegNext()//RegNext()//RegNext(io.sData.payload)
    RdData(8):=RegNext(io.sData.payload)
    val Wr_En=RegNext(io.sData.fire)
    val Fsm=MedFilterFsm(io.start&&(!start_not))//自清零，只拉高一次
    
    //0到1023的地址,实际上是1024个点
    val Col_Cnt=WaCounter(io.sData.valid&&io.sData.ready, log2Up(Config.COl_CNT_NUM)+3, Config.COl_CNT_NUM-1)//创建列计数器

    val Row_Cnt_9=WaCounter(Fsm.currentState===MedEnum.WAIT_FIFO_READY&&Fsm.nextState===MedEnum.WAIT_9_ROWS,log2Up(9),8)//在进最后一行的时候，前面的那几行已经存满了，所以这里设置的应该是9而不是8
    val Row_Cnt_All=WaCounter(Fsm.currentState===MedEnum.WAIT_FIFO_READY&&Fsm.nextState===MedEnum.WAIT_LAST_ROW,log2Up(Config.ROW_CNT_NUM),Config.ROW_CNT_NUM-8)//这里到底是减1还是不减？----bug
    when(Fsm.currentState===MedEnum.IDLE){
        Row_Cnt_9.clear
        Row_Cnt_All.clear
    }
    val mData_flow=Vec(UInt(16 bits),50)
    val RdAddr_Cnt=Reg(UInt(log2Up(Config.BRAM_DEPTH) bits))init(0)//读地址计数器
    val WrAddr_Cnt=RegNext(RdAddr_Cnt)//读地址比写地址慢一拍
    //初始化
    val INIT_CNT=WaCounter(Fsm.currentState === MedEnum.INIT, 3, 5)//初始化计数器,数五拍
    Fsm.INIT_END:=INIT_CNT.valid
    //等9行
    Fsm.IS_9_ROWS_END:=(Row_Cnt_9.valid)//要8行,所以设置为7
    //等一行
    Fsm.Col_End:=Col_Cnt.valid//最后一列数完了
    Fsm.Col_End_End:=Fsm.currentState===MedEnum.COl_END&&io.sData.valid//下一行新的数据来了，就切换状态
    //等全部行
    Fsm.IS_LAST_ROW:=Row_Cnt_All.valid
	//等fifo好-
	// Fsm.Fsm_Fifo_Ready:=Fifo_0.io.Data_In.ready
    when(io.sData.fire){
        when(RdAddr_Cnt===Config.COl_CNT_NUM-1){
            RdAddr_Cnt:=0
        }otherwise{
            RdAddr_Cnt:=RdAddr_Cnt+1
        }
    }otherwise{
        RdAddr_Cnt:=RdAddr_Cnt
    }
    
    val mem=Array.tabulate(8)(i=>{
        def gen():Mem[UInt]={
            val mem=Mem(UInt(Config.BRAM_IN_DATA_WIDTH bits),wordCount=Config.BRAM_DEPTH)//
            mem.write(WrAddr_Cnt,WrData(i),Wr_En)//写地址,写数据,写使能都延迟了一拍
            RdData(i):=mem.readSync(RdAddr_Cnt)
            if(i<4){//处理0,2,4,6的ram---
                for(j<-1 to 4){//j+5*i:
                    mData_flow(j+5*i):=RegNext(mData_flow(j+5*i-1))//RegNext()//RegNext()//,Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID)
                    mData_flow(j+5*i+25):=RegNext(mData_flow(j+5*i-1+25))//RegNext()//RegNext()//,Fsm_Qtqw.currentState===Wait5Enum.DATA_VALID)
                }//
                mData_flow(5*i):=RegNext(RdData(2*i)(15 downto 0))//低16位为双数
                mData_flow(5*i+25):=RegNext(RdData(2*i)(31 downto 16))//高16位为双数
            }
            mem
        }
        gen()
    })
    //开始掐头去尾
    val Out_Data_Valid_Bram=RegNext(io.sData.fire&&Fsm.currentState===MedEnum.WAIT_LAST_ROW)//第六版改的这里，需要Bram valid对齐
    val Out_Data_Valid=Delay(Out_Data_Valid_Bram,1)//仿真通过,修改的最后一行代码,完美---22/08/12/0:48 am
    val Out_Data_Col=Delay(Col_Cnt.count,2)//
    val valid = Vec(Bool(), 5)
    val Delay_valid=Vec(Bool(), 5)
    Delay_valid(4):=Delay(valid(4),4)
    Delay_valid(3):=Delay(valid(3),3)
    Delay_valid(2):=Delay(valid(2),2)
    Delay_valid(1):=Delay(valid(1),1)//需要注意的是,
    Delay_valid(0):=valid(0)//Delay(,1)//Delay(,1)//////Delay(,1)//这个Valid对应的是rData的有效数据,对于mDataFlow由于有一个寄存器,所以会慢一拍
    
    mData_flow(24):=RegNext(mData_flow(23))
    mData_flow(23):=RegNext(mData_flow(22))
    mData_flow(22):=RegNext(mData_flow(21))
    mData_flow(21):=RegNext(mData_flow(20))
    mData_flow(20):=RegNext(RdData(8)(15 downto 0))

    mData_flow(49):=RegNext(mData_flow(48))
    mData_flow(48):=RegNext(mData_flow(47))
    mData_flow(47):=RegNext(mData_flow(46))
    mData_flow(46):=RegNext(mData_flow(45))
    mData_flow(45):=RegNext(RdData(8)(31 downto 16))
    
    val Fsm_Watched=Delay(Fsm.currentState,2)
    when(Fsm_Watched===MedEnum.WAIT_LAST_ROW){
        when(Out_Data_Col>3&&Out_Data_Col<1024){
            valid(0):=Out_Data_Valid
        }otherwise{
            valid(0):=False
        }
        when(Out_Data_Col>2&&Out_Data_Col<1024-1){
            valid(1):=Out_Data_Valid
        }otherwise{
            valid(1):=False
        }
        when(Out_Data_Col>1&&Out_Data_Col<1024-2){
            valid(2):=Out_Data_Valid
        }otherwise{
            valid(2):=False
        }
        when(Out_Data_Col>0&&Out_Data_Col<1024-3){
            valid(3):=Out_Data_Valid
        }otherwise{
            valid(3):=False
        }
        when(Out_Data_Col<1024-4){
            valid(4):=Out_Data_Valid
        }otherwise{
            valid(4):=False
        }
        }otherwise{
        valid.map(_ := False)
    }

	val Fifo=Array.tabulate(50)(i=>{
        def gen():Fifo_Test={
            val Fifo=new Fifo_Test//
            //Delay_valid(0),ready--输入的ready信号,告诉fifo要pop数据了
			Fifo.io.Data_In.payload:=mData_flow(i)//进fifo的数据,push_payload
			Fifo.io.Data_In.valid:=Delay_valid(i%5)//进fifo使能,push_valid
            Fifo
        }
        gen()
    })
    for(i<-0 to 49){
        io.mData(i):=Fifo(i).io.Data_out
        Fifo(i).io.fifo_ready:=Fifo(0).io.Pop_Valid&&io.mReady//不能走flow流了，下一层可能存在没准备好的情况
    }
    io.mValid:=Fifo(0).io.Pop_Valid//Delay(Delay_valid(0),2)
	Fsm.Fsm_Fifo_Ready:=Fifo(0).io.Data_In.ready//只要有一个fifo准备好接收数据才能开始发送数据，那么其他fifo应该都准备好了
	io.sData.ready:=(Fsm.currentState===MedEnum.WAIT_9_ROWS||Fsm.currentState===MedEnum.WAIT_LAST_ROW)&&Fifo(0).io.Data_In.ready&&io.mReady//Fifo_0.io.Data_In.ready//
    //第六版改的这里，需要Bram valid对齐
}

