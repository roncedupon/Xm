package MedFilter_V12.Lty_Gen_V2
import spinal.core._
import spinal.lib.slave
import MedFilter_V12.MemConfig
import Archive.WaCounter
import spinal.lib.master
import spinal.lib.Delay
class Lty_Bram extends BlackBox{//黑盒，入32bit，出16 bit
    val Config=MemConfig()//浮点乘法器
    
    val io=new Bundle{//component要求out有驱动，但是black box不要求out的驱动
        val clka=in Bool()
        val addra=in UInt(log2Up(Config.LTY_DATA_BRAM_A_DEPTH) bits)
        val dina=in UInt(Config.LTY_DATA_BRAM_A_WIDTH bits)
        val ena=in Bool()
        val wea=in Bool()

        //B口读使能一直有效----后来发现还是给个读使能比较方便
        val addrb=in UInt(log2Up(Config.LTY_DATA_BRAM_B_DEPTH) bits)
        val clkb=in Bool()
        val doutb=out UInt((Config.LTY_DATA_BRAM_B_WIDTH) bits)
        val enb=in Bool()
        
    }

    noIoPrefix()
    // Clock A is map on a specific clock Domain
    mapClockDomain(this.clockDomain, io.clka)
    // Clock B is map on the current clock domain
    mapCurrentClockDomain(io.clkb)
}
object LTY_ENUM extends SpinalEnum(defaultEncoding = binaryOneHot) {
  val IDLE, INIT, LOAD_2_ROWS,EXTRACT_LTY,JUDGE_LAST_ROW = newElement
  //WAIT_NEXT_READY：等待下一个模块ready（可以认为LTY参数累加模块ready了）
  //Judge Last Row:每一行结束了都需要启动最后一行的判断
}
object LTY_ENUM_UP extends SpinalEnum(defaultEncoding = binaryOneHot) {
  val WAIT_EXTRACT_LTY,EXTRACT_LTY,WAIT_LINE_DOWN= newElement
  //等待提取连通域
  //开始提取连通域
  //等待下一层计算完成
}
case class LTY_UP_FSM(start:Bool)extends Area{
    val currentState = Reg(LTY_ENUM_UP()) init LTY_ENUM_UP.WAIT_EXTRACT_LTY
    val nextState = LTY_ENUM_UP()
    currentState := nextState

    val Extract_Lty_End=Bool()
    val Line_Down_End=Bool()
    switch(currentState){
        is(LTY_ENUM_UP.WAIT_EXTRACT_LTY){
            when(start){
                nextState:=LTY_ENUM_UP.EXTRACT_LTY
            }otherwise{
                nextState:=LTY_ENUM_UP.WAIT_EXTRACT_LTY
            }
        }
        is(LTY_ENUM_UP.EXTRACT_LTY){
            when(Extract_Lty_End){
                nextState:=LTY_ENUM_UP.WAIT_LINE_DOWN
            }otherwise{
                nextState:=LTY_ENUM_UP.EXTRACT_LTY
            }
        }
        is(LTY_ENUM_UP.WAIT_LINE_DOWN){
            when(Line_Down_End){
                nextState:=LTY_ENUM_UP.WAIT_EXTRACT_LTY
            }otherwise{
                nextState:=LTY_ENUM_UP.WAIT_LINE_DOWN
            }
        }
    }
}
case class LTY_FSM(start: Bool) extends Area {
  

  val currentState = Reg(LTY_ENUM()) init LTY_ENUM.IDLE
  val nextState = LTY_ENUM()
  currentState := nextState

  val Init_End=Bool()
  val last_Row = Bool()  //是否为最后一行

  val Col_End=Bool()//输出完一行,这里要注意，这是输出完一行的意思
  val Load_2_Row_End=Bool()//加载完两行

  val Judge2Row_Col_End=Bool()//判断加载前两行的一行是否结束
  switch(currentState){
    is(LTY_ENUM.IDLE){
        when(start){
            nextState:=LTY_ENUM.INIT
        }otherwise{
            nextState:=LTY_ENUM.IDLE
        }
    }
    is(LTY_ENUM.INIT){
        when(Init_End){
            nextState:=LTY_ENUM.LOAD_2_ROWS//初始化完成就加载前两行
        }otherwise{
            nextState:=LTY_ENUM.INIT
        }
    }
    is(LTY_ENUM.LOAD_2_ROWS){//加载前两行
        when(Judge2Row_Col_End){//Col_End的标准是进一行
            nextState:=LTY_ENUM.JUDGE_LAST_ROW
        }otherwise{
            nextState:=LTY_ENUM.LOAD_2_ROWS
        }
    }
    is(LTY_ENUM.JUDGE_LAST_ROW){
        when(last_Row){
            nextState:=LTY_ENUM.IDLE//输出完最后一行
        }elsewhen(Load_2_Row_End){
            nextState:=LTY_ENUM.EXTRACT_LTY//等待下面的计算模块准备好接受计算数据
        }otherwise{
            nextState:=LTY_ENUM.LOAD_2_ROWS//前两行没加载完继续加载前两行
        }
    }
    // is(LTY_ENUM.WAIT_NEXT_READY){
    //     when(Next_Ready){
    //         nextState:=LTY_ENUM.EXTRACT_LTY
    //     }otherwise{
    //         nextState:=LTY_ENUM.WAIT_NEXT_READY//下一个模块没准备好，就一直等着
    //     }
    // }
    //由于进数据和出数据是独立的，无需考虑数据的反压，所以无需这个等下一层好的状态
    //只要在Extract Lty状态，那么数据就能valid，只要下一层ready，那么数据就能流下去
    is(LTY_ENUM.EXTRACT_LTY){
        when(Col_End){//只要进入这个状态，我们就可以保证数据要处理的那两行数据已经缓存完了，
            //接下来要做的就是正确地将数据输出去更新连通域标记矩阵
            nextState:=LTY_ENUM.JUDGE_LAST_ROW
        }otherwise{
            nextState:=LTY_ENUM.EXTRACT_LTY
        }
    }
    
  }
}



//Recoder：考虑将数据缓存和连通域提取分开
class Lty_Feature_Cache extends Component{//连通域标记
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(Config.LTY_DATA_BRAM_A_WIDTH bits))//进来的数据
        val mData1=master Stream(UInt(Config.LTY_DATA_BRAM_B_WIDTH bits))//第一行出去的数据
        val mData1_End_Receive=in Bool()
        val mData2=master Stream(UInt(Config.LTY_DATA_BRAM_B_WIDTH bits))//第二行出去的数据

        val Mark1Up_Out=out UInt(Config.LTY_MARK_BRAM_WIDTH bits)//出去的第一行的点上面对应的标记点
        val Mark2Up_Out=out UInt(Config.LTY_MARK_BRAM_WIDTH bits)//出去的第一行的点上面对应的标记点

        val Mark1_In=in UInt(Config.LTY_MARK_BRAM_WIDTH bits)//进来的的第一行的点对应的标记点，用于更新连通域标记矩阵
        val Mark1_In_Addr=in UInt(log2Up(Config.LTY_MARK_BRAM_DEPTH) bits)//第一行需要更新的点的标记
        val Mark1_In_Valid=in Bool()//写数据和写地址有效，更新连通域标记矩阵


        val Mark2_In=in UInt(Config.LTY_MARK_BRAM_WIDTH bits)//出去的第2行的点对应的标记点
        val Mark2_In_Addr=in UInt(log2Up(Config.LTY_MARK_BRAM_DEPTH) bits)//第2行需要更新的点的标记
        val Mark2_In_Valid=in Bool()
        val start=in Bool()//lty计算启动信号

        val strat_Sub_Module1=out Bool()//启动下层的连通域标记子模块
        val strat_Sub_Module2=out Bool()//启动下层的连通域标记子模块

        

    }
    noIoPrefix()
    val Fsm=LTY_FSM(io.start)
//状态机相关====================================================================
    //val Data_Out_Flag=Fsm.currentState===LTY_ENUM.EXTRACT_LTY||Fsm.currentState===LTY_ENUM.WAIT_NEXT_READY
    //不用这个↑作为mValid的判断是因为mValid标识出来的输出数据开头第一个会重复一下，结尾会少一个
    val Col_Cnt=WaCounter(io.sData.valid&&io.sData.ready, log2Up(Config.LTY_DATA_BRAM_A_DEPTH), Config.LTY_DATA_BRAM_A_DEPTH-1)//创建输入数据的列计数器
    //Bram_Out_Cnt决定一行的结束，由于Bram2一定慢Bram1，所以选Bram2的出数据作为一行结束的标志
    //一开始只有一个计数器，但是由于有两个数据Bram，所以必须实现两个数据Bram计数器
    
    val FeatureMem_13_Addr=WaCounter(io.mData2.ready&&Fsm.currentState===LTY_ENUM.EXTRACT_LTY,log2Up(Config.LTY_DATA_BRAM_B_DEPTH), Config.LTY_DATA_BRAM_B_DEPTH-1)
    val Bram_Out_Cnt=WaCounter(io.mData2.ready&&Fsm.currentState===LTY_ENUM.EXTRACT_LTY, log2Up(Config.LTY_DATA_BRAM_B_DEPTH), Config.LTY_DATA_BRAM_B_DEPTH-1)//创建输出数据的列计数器
    //valid会在慢ready一拍，RegNext的原因：由于计数的是输出数据计数器，下层收到数据会慢一拍，所以要加一个RegNext
    //还与上一个mData.ready的原因：下层模块处于getData 状态时才会接受数据，如果第一行完了，那么他会等第二行连通域提取结束
    //第二行没结束，那么当前模块会处于Extract Lty状态，导致边界不稳定
    

    //解决：22、9、21/19:19---由于下层是通过ready信号向上层请求数据的，所以下层的列计数器也应该是
    val Bram_Out_Row_Cnt=WaCounter(Bram_Out_Cnt.valid, log2Up(Config.LTY_ROW_NUM/2),Config.LTY_ROW_NUM/2-1)//创建输出行数计数器,记得除2，因为并行度是2
    val INIT_CNT=WaCounter(Fsm.currentState === LTY_ENUM.INIT, 3, 5)//初始化计数器,数五拍
    val Row_Cnt_All=WaCounter(Col_Cnt.valid,log2Up(Config.LTY_ROW_NUM),Config.LTY_ROW_NUM-1)//输入行计数器
    val Row_Cnt_2=WaCounter(Col_Cnt.valid,2,2)//缓存两行计数器，它的值一直是0，1，2，不可能是3，因为当Row_Cnt_2===2时，sReady拉低，不会再进数据
    //然后等输出完两行，进入最后一行判断，Row_Cnt_2被reset
    val Bram_Write_Choose=WaCounter(Col_Cnt.valid,log2Up(4),3)//0，1，2，3循环写
    Fsm.Init_End:=INIT_CNT.valid
    Fsm.last_Row:=Bram_Out_Row_Cnt.valid//缓存数完所有行，但是存在一种情况：
        //所有行都缓存完了，但是连通域还没被提取完，计算也没计算完，所以last_Row不能作为计算结束然后进入idle的标志
        //于是在状态机里又加了一些其他的判断
    Fsm.Judge2Row_Col_End:=Col_Cnt.valid//缓存完开头两行中的一行
    
 
    Fsm.Col_End:=Bram_Out_Cnt.valid//发完了一行数据，也就是发送数据计数器数完了2040，表面上发送完一行，实际上处理完了两行
    //regNext的原因：由于当ready拉高，地址开始自增，但是出去的数据会慢一拍，为了不丢掉最后一个数，所以regnext
//===================================================================================================================
    val Fsm_LineUp=LTY_UP_FSM(Row_Cnt_All.count>1)//缓存完开头两行，就启动上一行的连通域提取
    val FeatureMem_02_Addr=WaCounter(io.mData1.ready&&Fsm_LineUp.currentState===LTY_ENUM_UP.EXTRACT_LTY,log2Up(Config.LTY_DATA_BRAM_B_DEPTH), Config.LTY_DATA_BRAM_B_DEPTH-1)
    Fsm.Load_2_Row_End:=Row_Cnt_All.count>1&&(FeatureMem_02_Addr.count>6)//开头两行被缓存完了，行计数器为2的时候那么就加载完两行了,不能大于等于一，因为加载完第0行后，Row_Cnt就已经是1了
    Fsm_LineUp.Extract_Lty_End:=FeatureMem_02_Addr.valid//数据发完后就进入idle状态
    Fsm_LineUp.Line_Down_End:=Bram_Out_Cnt.valid
//一个Bram写选择器，一个Bram读选择器
    val Bram_Read_Chose=Reg(Bool())init(False)//要准确控制才行，是读0，1还是2，3
    //4个Bram的作用是在计算前两行的时候加载后两行数据
    when(RegNext(Bram_Out_Cnt.valid)){
        Bram_Read_Chose:=(!Bram_Read_Chose)
    }
   //违背逻辑的原因是避免在导入前两行的时候输出
    //也就是说，在加载第3，4行的时候，输出1，2行，加载1，2行时同理
//Line Up fsm==============================================================================


//建立四个数据缓存Bram=======================================================================
    val Wr_En=Vec(Bool(),4)//循环写
    for(i<-0 to 3){
        Wr_En(i):=Bram_Write_Choose.count===i
    }
    val FeatureMem=Array.tabulate(4)(i=>{
        def gen():Lty_Bram={
            val mem=new Lty_Bram//
            mem
        }
        gen()
    })
    //写数据，需要处理写使能，写地址，写数据
    for(i<-0 to 3){
        FeatureMem(i).io.ena:=Wr_En(i)//4个Bram的写使能
        FeatureMem(i).io.addra:=Col_Cnt.count//4个Bram的写地址
        FeatureMem(i).io.dina:=io.sData.payload//4个Bram的写数据
        FeatureMem(i).io.wea:=True
    }//读数据=================================
    //之前这里用的是计数触发信号是io.mData2.ready，会在ready拉低后丢一个点---连通域记录一
    for(i<-0 to 3){
        if(i%2==0){//0,2,第一行
            FeatureMem(i).io.addrb:=FeatureMem_02_Addr.count//02代表的是0，2Bram
        }else{//1,3，第二行
            FeatureMem(i).io.addrb:=FeatureMem_13_Addr.count//
        }
        FeatureMem(i).io.enb:=True//以后有需要在处理
    }
    io.mData1.payload:=Bram_Read_Chose?FeatureMem(2).io.doutb|FeatureMem(0).io.doutb//输出数据选择器
    io.mData2.payload:=Bram_Read_Chose?FeatureMem(3).io.doutb|FeatureMem(1).io.doutb//输出数据选择器
//mux必须是圆括号
//控制数据输入输出


    io.sData.ready:=((Fsm.currentState=/=LTY_ENUM.IDLE)&&(Fsm.currentState=/=LTY_ENUM.INIT))&&Row_Cnt_2.count<2//待处理
    //按道理说，只要另外两个Bram没满，任何时候都能接受数据
    when(Fsm.currentState===LTY_ENUM.JUDGE_LAST_ROW&&(!io.sData.ready)){//为毛线要取个反？我敲，我忘记为啥了
        Row_Cnt_2.clear//每缓存完一行数据都要进行是否是最后一行判断
    }
    // when(){//这里加RegNext的原因：对波形时开头多了一个点结尾少了一个点，待分析
    // }otherwise{
    //     io.mData1.valid:=False
    //     io.mData2.valid:=False
    // }
    io.mData1.valid:=RegNext(io.mData1.ready)//添加后面的Fsm.currentState===LTY_ENUM.WAIT_NEXT_READY条件是因为没必要让mValid在连通域提取状态拉高拉低
    io.mData2.valid:=RegNext(io.mData2.ready)//这里情况比较特殊，mValid应该由mReady驱动，也就是说，mReady不来的话，mValid不会拉高
//加RegNext的原因：下层ready进来的下一周期出去的数才是有效的
//与上LTY_ENUM.EXTRACT_LTY的原因：只有在这个状态内数据才是有效的
    // io.mData1.valid:=RegNext(Data_Out_Flag)
    // io.mData2.valid:=RegNext(Data_Out_Flag)
//最后还是决定将连通域标记矩阵放在这里面，因为出去的pixel应该和上标记一一对应
/*
现在需要决定连通域标记矩阵的调度
    比如第一行数据Bram出去的时候，跟着出去的还有Up_Mark_Mem1对应的标记，也就是说，认为Up_Mark_Mem1是第一行上面的那个标记矩阵
    但是，第一行数据被标记后，需要更新标记矩阵，那么，第一行的标记应该被写回到Up_Mark_Mem1
    这也是为什么Up_Mark_Mem1的读写地址不同的原因
    总结：
        FeatureMem0是FeatureMem1的理论上一行
        Feature0的标记写入Up_Mark_Mem2
        Feature1的标记写入Up_Mark_Mem1
⭐：关于读写冲突：
    首先第一行读出标记，然后写回标记，假如第一行处于23，那么需要向Up_Mark_Mem2的23地址写入，
    又由于第二行始终慢于第一行，所以读写冲突不会产生？22、9、16/23:22
    似乎没啥问题。。。。。
*/
    val Up_Mark_Mem1=Mem(UInt(Config.LTY_MARK_BRAM_WIDTH bits),wordCount=Config.LTY_MARK_BRAM_DEPTH)//第一行的上标记矩阵
    Up_Mark_Mem1.write(io.Mark2_In_Addr,io.Mark2_In,io.Mark2_In_Valid)//写地址,写数据,写使能都延迟了一拍
    val Up_Mark_Mem2=Mem(UInt(Config.LTY_MARK_BRAM_WIDTH bits),wordCount=Config.LTY_MARK_BRAM_DEPTH)//第二行的上标记矩阵
    Up_Mark_Mem2.write(io.Mark1_In_Addr,io.Mark1_In,io.Mark1_In_Valid)//写地址,写数据,写使能都延迟了一拍    

    io.Mark1Up_Out:=Up_Mark_Mem1.readSync(FeatureMem_02_Addr.count)//mark地址和Bram地址同步
    io.Mark2Up_Out:=Up_Mark_Mem2.readSync(FeatureMem_13_Addr.count)
    when(Bram_Out_Row_Cnt.count<=0){
        io.Mark1Up_Out:=0//第一行出去的是0//第一行最后会出来一个xx，很奇怪
    }

    io.strat_Sub_Module1:=Fsm_LineUp.currentState===LTY_ENUM_UP.EXTRACT_LTY&&RegNext(!(Fsm_LineUp.currentState===LTY_ENUM_UP.EXTRACT_LTY))//在这个状态下就启动连通域提取
    io.strat_Sub_Module2:=Fsm.currentState===LTY_ENUM.EXTRACT_LTY&&RegNext(!(Fsm.currentState===LTY_ENUM.EXTRACT_LTY))//在这个状态下就启动连通域提取
}
object MARK_ENUM extends SpinalEnum(defaultEncoding = binaryOneHot) {
    //启动信号是非常有必要的，比如第一行启动后，需要处理至少5个点后才能启动第二行的处理
    //也就是说，多并行度下，第二行的处理必须慢第一行5个标记点处理的时间
  val IDLE, INIT, GET_DATA,GEN_NEW_LTY,UP1_COND,UP0_LEFT1,UPDATA_LEFT1,UPDATA_LEFT2,UPDATA_LEFT3,UPDATA_LEFT4= newElement
    /*三种条件，COND_CHOSE,
        GEN_NEW_LTY  上，左都为0---新建一个连通域,生成新的连通域
        UP1_COND:  上不为0---处理左边四个点，Up is 1 condition
        UP0_LEFT1：上为0，左不为0---单独处理当前点  
    */ 
    //后来又添加了四个状态用于处理左边四个点   
}
class Mark_Fsm(start:Bool) extends Area{
    val currentState = Reg(MARK_ENUM()) init MARK_ENUM.IDLE
    val nextState = MARK_ENUM()
    currentState := nextState

    val Init_End=Bool()
    val Get_Data_End=Bool()
    val Row_End=Bool()//一行数据被标记完了

    val Gen_New_Lty=Bool()
    val Gen_New_Lty_End=Bool()//这个意思是构建完了新的连通域，也就是说Lty_Num加1，并且当前Lty_Num+1对应的连通域的参数也计算完了，等待写回了

   
    val Up1_Cond=Bool()
    val Up1_Cond_End=Bool()

    
    val Up0_Left1=Bool()
    val Up0_Left1_End=Bool()

    val UpData_Left1_End=Bool()
    val UpData_Left2_End=Bool()
    val UpData_Left3_End=Bool()
    val UpData_Left4_End=Bool()

    switch(currentState){
        is(MARK_ENUM.IDLE){
            when(start){
                nextState:=MARK_ENUM.INIT
            }otherwise{
                nextState:=MARK_ENUM.IDLE
            }
        }
        is(MARK_ENUM.INIT){
            when(Init_End){
                nextState:=MARK_ENUM.GET_DATA
            }otherwise{
                nextState:=MARK_ENUM.INIT
            }
        }
        is(MARK_ENUM.GET_DATA){
            when(Get_Data_End){//也就是sData.fire拉高了
                when(Up1_Cond){
                    nextState:=MARK_ENUM.UP1_COND
                }elsewhen(Up0_Left1){
                    nextState:=MARK_ENUM.UP0_LEFT1
                }elsewhen(Gen_New_Lty){//Gen_New_Lty
                    nextState:=MARK_ENUM.GEN_NEW_LTY
                }otherwise{//这一条件就是pixel小于阈值，直接不更新当前点，跳过
                    nextState:=MARK_ENUM.GET_DATA
                }
            }elsewhen(Row_End){
                nextState:=MARK_ENUM.IDLE
            }otherwise{
                nextState:=MARK_ENUM.GET_DATA
            }
        }
        // is(MARK_ENUM.COND_CHOSE){//多一个状态就多一个状态吧。。。
        //     when(Up1_Cond){
        //         nextState:=MARK_ENUM.UP1_COND
        //     }elsewhen(Up0_Left1){
        //         nextState:=MARK_ENUM.UP0_LEFT1
        //     }elsewhen(Gen_New_Lty){//Gen_New_Lty
        //         nextState:=MARK_ENUM.GEN_NEW_LTY
        //     }elsewhen(Row_End){
        //         nextState:=MARK_ENUM.IDLE
        //     }otherwise{//这一条件就是pixel小于阈值，直接不更新当前点，跳过
        //         nextState:=MARK_ENUM.GET_DATA
        //     }
        // }
        is(MARK_ENUM.GEN_NEW_LTY){
            when(Gen_New_Lty_End){
                nextState:=MARK_ENUM.GET_DATA
            }otherwise{
                nextState:=MARK_ENUM.GEN_NEW_LTY
            }
        }
        is(MARK_ENUM.UP0_LEFT1){
            when(Up0_Left1_End){
                nextState:=MARK_ENUM.GET_DATA
            }otherwise{
                nextState:=MARK_ENUM.UP0_LEFT1
            }
        }
        is(MARK_ENUM.UP1_COND){
            when(Up1_Cond_End){
                nextState:=MARK_ENUM.UPDATA_LEFT1
            }otherwise{
                nextState:=MARK_ENUM.UP1_COND
            }
        }
        //左边四个===================================
        is(MARK_ENUM.UPDATA_LEFT1){
            when(UpData_Left1_End){
                nextState:=MARK_ENUM.UPDATA_LEFT2
            }otherwise{
                nextState:=MARK_ENUM.UPDATA_LEFT1
            }
        }
        is(MARK_ENUM.UPDATA_LEFT2){
            when(UpData_Left2_End){
                nextState:=MARK_ENUM.UPDATA_LEFT3
            }otherwise{
                nextState:=MARK_ENUM.UPDATA_LEFT2
            }
        }
        is(MARK_ENUM.UPDATA_LEFT3){
            when(UpData_Left3_End){
                nextState:=MARK_ENUM.UPDATA_LEFT4
            }otherwise{
                nextState:=MARK_ENUM.UPDATA_LEFT3
            }
        }
        is(MARK_ENUM.UPDATA_LEFT4){
            when(UpData_Left4_End){
                nextState:=MARK_ENUM.GET_DATA
            }otherwise{
                nextState:=MARK_ENUM.UPDATA_LEFT4
            }
        }
        //============左边四个点end===========================        
    }
}
class Lty_Mark_Sub_Module extends Component{//标记子模块
    val Config=MemConfig()
    val io=new Bundle{
        val start=in Bool()
        val sData_Receive_End=out Bool()//接收完一行
        val sData=slave Stream(UInt(Config.LTY_DATA_BRAM_B_WIDTH bits))//进来的滤波后图片数据点
        val Up_mark=in UInt(Config.LTY_MARK_BRAM_WIDTH bits)//上标记，对应的当前像素点的上标记
        val Lty_Total_NUm=in UInt(log2Up(Config.LTY_PARAM_MEM_DEPTH) bits)//处理当前点时的连通域总数量，用于更新标记矩阵
        val Mark_Out=out UInt(Config.LTY_MARK_BRAM_WIDTH bits)//输出的当前点的标记
        val Mark_Out_Addr=out UInt(log2Up(Config.LTY_MARK_BRAM_DEPTH) bits)//其实就是当前点所处的列
        val Mark_Out_Valid=out Bool()//写数据和写地址有效，更新连通域标记矩阵

        val New_Lty_Gen=out Bool()
        // val J_Out=out UInt(log2Up(Config.LTY_DATA_BRAM_B_DEPTH)bits)//出去的计算值j
        //不需要单独的J_Out是因为J_Out其实和Mark_Out_Addr一样，并且专门针对左四个点进行了处理⭐计算的时候记得加一
        //阈值相关：
        val Temp_Back_Mean=in UInt(16 bits)//左移32Bit需要32 bit位宽---不过目前按左移12、13bit来处理的
        val Sign_Flag=in Bool()//有无符号位
        val Temp_Back_Thrd=in UInt(16 bits)//由于进来的图片像素点都是整形，而Temp_Back_Thrd的实际值带小数，所以可以将Temp_Back_Thrd向上取整
        //为了防止pixel=70，Temp_Back_Thrd=69.9（向上取整后为70）取伪的情况，第一个判断应该使用大于等于
        val Lty_Para_mReady=in Bool()//其实接受方那边是一直准备好了的，但是，为了处理两行同时产生新的连通域的情况，需要有一个先后处理顺序
        //如果一二行的Valid同时拉高
        //不过此时MEM一次只能处理一个点的数据
            //第二行的mReady应该 是！lineValid&&line2Valid
            //第一行的mReady就一直拉高就行了
        val Lty_Para_mValid=out Bool()
         //写回计算结果相关，6个参数
        //我们只需要算出来需要累加的结果，然后再发过去，累加操作让接受模块做，所以在此不考虑读冲突，只考虑写冲突
        // val Lty_Para1_mData=out UInt(Config.LTY_PARAM1_MEM_WIDTH bits)
        // val Lty_Para2_mData=out UInt(Config.LTY_PARAM2_MEM_WIDTH bits)
        // val Lty_Para3_mData=out UInt(Config.LTY_PARAM3_MEM_WIDTH bits)
        // val Lty_Para4_mData=out UInt(Config.LTY_PARAM4_MEM_WIDTH bits)
        // val Lty_Para5_mData=out UInt(Config.LTY_PARAM5_MEM_WIDTH bits)//其实6和5一样，不知道还要不要再多开一个。。。
        // val Lty_Para6_mData=out UInt(Config.LTY_PARAM6_MEM_WIDTH bits)---注掉的原因：Dsp可以计算A*B+C,还是单独拿一个模块来处理累加计算好了
    }
    io.Mark_Out:=0
    noIoPrefix()
//状态机相关=============================================================================================
    val Fsm=new Mark_Fsm(io.start)//&&(!RegNext(io.start))
    val Init_Cnt=WaCounter(Fsm.currentState === MARK_ENUM.INIT, 3, 5)//初始化计数器,数五拍
    Fsm.Init_End:=Init_Cnt.valid
    //进数据控制相关---这里的计数器没有减一，要注意⭐
    val Pixel_In_Cnt=WaCounter(RegNext(io.sData.ready)&&Fsm.currentState===MARK_ENUM.GET_DATA,log2Up(Config.LTY_DATA_BRAM_B_DEPTH),Config.LTY_DATA_BRAM_B_DEPTH-1)//当前处理的点的坐标计数器
    //不等valid的原因：只有ready拉高valid才会拉高

    io.Mark_Out_Addr:=Pixel_In_Cnt.count//本来是第0个点，但是拿到一个有效点后，count变成了1、但是需要关心的是0的地址，所以要减去一
        /*这里的计数器原理：首先状态位于拿数据状态，Pixel_Pos从0开始数
        当sData.fire拉高，进来一个滤波后图片数据和当前点的上标记，状态跳转到之后的状态，然后Pixel_Pos拉高
        假如只收一个数据，sData.fire拉高后下一个周期Pixel_Pos Valid也拉高，下一次拿数据状态，Pixel_Pos=1，那么这时应该退出去了
        
        反正这里不能用减一的原因是这里是先拿点再判断，而前面减一是先判断再拿点⭐
        */
    Fsm.Row_End:=Pixel_In_Cnt.valid//必须加一个fire,存在一种情况：当最后一个点进来后，但是
    Fsm.Get_Data_End:=RegNext(io.sData.ready)&&io.sData.payload>=io.Temp_Back_Thrd&&Fsm.currentState===MARK_ENUM.GET_DATA//拿到一个数据结束信号拉高并同时启动三个子条件的判断，如果三个子条件都不满足，那么继续拿数据
    io.sData_Receive_End:=Pixel_In_Cnt.valid||Fsm.currentState===MARK_ENUM.INIT||(Fsm.currentState===MARK_ENUM.IDLE)
//连通域条件判断相关=======================================================================================
    val Left_Mark=Vec(Reg(UInt(Config.LTY_MARK_BRAM_WIDTH bits))init(0),4)//创建四个移位寄存器，代表左边的四个标记点
    val Shift_Mark_In=UInt(Config.LTY_MARK_BRAM_WIDTH bits)
    val Shift_Start=Bool()//启动移位寄存器
    Shift_Start:=False
    Shift_Mark_In:=0
//     for(i<-0 to 2){
//         when(Shift_Start){
//             Left_Mark(i+1):=Left_Mark(i)
//         }
//     }
    when(Shift_Start){
        Left_Mark(0):=Shift_Mark_In
    }otherwise{
        Left_Mark(0):=Left_Mark(0)
    }
    when(Shift_Start){
        Left_Mark(1):=Left_Mark(0)
    }otherwise{
        Left_Mark(1):=Left_Mark(1)
    }
    when(Shift_Start){
        Left_Mark(2):=Left_Mark(1)
    }otherwise{
        Left_Mark(2):=Left_Mark(2)
    }
    when(Shift_Start){
        Left_Mark(3):=Left_Mark(2)
    }otherwise{
        Left_Mark(3):=Left_Mark(3)
    }
    /*关于连通域数量加一的问题
        有一种可能：第一行连通域和第二行连通域同时满足创建新连通域的条件，那么连通域要加2
        现在的问题是如何处理这种情况？
        还有一种情况，如果第一行在最后一个点创建了一个新连通域，那么第二行之前处理的所有新连通域标记都要作废了。。。(问题不大，已解决)

        又有一种情况：⭐⭐⭐⭐⭐⭐
        如果上下两行同时创建了两个新连通域，怎么更新标记矩阵以及怎么更新Lty_Data?
            也就是上下两行要同时更新连通域，怎么处理
                解决方案：读Lty_Data_Mem时给一个Valid信号，和switch选择开关
            第二天补充：由于上下两行同时创建新连通域，但是他俩创建的连通域标记都是一样的，这是不可能发生的，还需要对这周情况进行处理

    */
    // when(Fsm.currentState===MARK_ENUM.COND_CHOSE){//首先应该进入拿数据状态
    //然后拿像素点和阈值比较
        
    // }
    Fsm.Gen_New_Lty:=False//进入生成新连通域状态
    Fsm.Up0_Left1:=False//进入上为0左不为0状态
    Fsm.Up1_Cond:=False//进入上不为0状态，处理左边四个点

    // Fsm.Gen_New_Lty_End:=False//进入生成新连通域状态
    // Fsm.Up0_Left1_End:=False//进入上为0左不为0状态
    // Fsm.Up1_Cond_End:=False//进入上不为0状态，处理左边四个点
    io.New_Lty_Gen:=False
    when(io.Up_mark === 0 && Left_Mark(0)===0) {//上面和左边都没被标记
        io.Mark_Out:=io.Lty_Total_NUm+1//那么这是一个新的连通域、
        io.New_Lty_Gen:=True
        Shift_Mark_In:=io.Lty_Total_NUm+1
        Fsm.Gen_New_Lty:=True//进入生成新连通域状态
        Shift_Start:=True//更新移位寄存器的值供下一个点
        // Fsm.Up0_Left1:=False//进入上为0左不为0状态
        // Fsm.Up1_Cond:=False//进入上不为0状态，处理左边四个点
    }.elsewhen(io.Up_mark === 0 && Left_Mark(0)=/=0) {//上面没被标记，左边被标记了,那么当前点的标记就应该和左边点标记一样
        io.Mark_Out:=Left_Mark(0)//将当前点的标记点更新为左标记点
        Shift_Mark_In:=Left_Mark(0)
        // Fsm.Gen_New_Lty:=False//进入生成新连通域状态
        Fsm.Up0_Left1:=True//进入上为0左不为0状态
        // Fsm.Up1_Cond:=False//进入上不为0状态，处理左边四个点
    }.elsewhen(io.Up_mark =/= 0 && Left_Mark(0) === 0) {//上面点被标记，左边点没被标记，将当前点标记为上面的点
        io.Mark_Out:=io.Up_mark
        Shift_Mark_In:=io.Up_mark//Shift_Mark_In是一个wire类型
        Fsm.Up1_Cond:=True//进入上不为0状态，处理左边四个点                
    }        
    
//生成新连通域(上，左都为0)==============================================================================
    io.Mark_Out_Valid:=True//从False改为一直True的原因：如果不满足三个条件，它也应该有效，只不过它的值是0
    io.Lty_Para_mValid:=True
    Fsm.Gen_New_Lty_End:=io.Lty_Para_mReady&&io.Lty_Para_mValid//修改原因：需要数据发过去才能退出这个状态Delay(Fsm.currentState===MARK_ENUM.GEN_NEW_LTY,Config.DSP_PIPELINE_STAGE)//控制状态结束
    when(Fsm.currentState===MARK_ENUM.GEN_NEW_LTY){//这部分代码主要处理状态结束
        //首先得进入生成新连通域状态
        //在这一状态下，标记相关操作
        //向当前Pixel_Cnt-1对应的mark Mem写入Lty_Num+1  只操作独立的mem，无写冲突
        io.Mark_Out_Valid:=True
        //更新移位寄存器的值供下一轮使用
    }

    //io.Lty_Para_mValid:=Delay(Fsm.currentState===MARK_ENUM.GEN_NEW_LTY,Config.DSP_PIPELINE_STAGE)&&Fsm.currentState===MARK_ENUM.GEN_NEW_LTY
//上为0，左不为0
    Fsm.Up0_Left1_End:=io.Lty_Para_mReady&&io.Lty_Para_mValid
    when(Fsm.currentState===MARK_ENUM.UP0_LEFT1){
        io.Mark_Out_Valid:=True
    }
    //io.Lty_Para_mValid:=Delay(Fsm.currentState===MARK_ENUM.UP0_LEFT1,Config.DSP_PIPELINE_STAGE)&&Fsm.currentState===MARK_ENUM.UP0_LEFT1
//上不为0，还要处理左四个点
    Fsm.Up1_Cond_End:=io.Lty_Para_mReady&&io.Lty_Para_mValid
        //同样地，只有当数据算出来的数据发过去后才能退出这个状态去干其他的事情
    when(Fsm.currentState===MARK_ENUM.UP1_COND){
        //位于上不为0状态，先处理当前点
        io.Mark_Out_Valid:=True//需要向当前点对应的mem地址写入mark标记0
    }
    //io.Lty_Para_mValid:=Delay(Fsm.currentState===MARK_ENUM.UP1_COND,Config.DSP_PIPELINE_STAGE)&&Fsm.currentState===MARK_ENUM.UP1_COND
//===========================左边四个点处理=====================================
    Fsm.UpData_Left1_End:=io.Lty_Para_mReady&&io.Lty_Para_mValid//数据发完就结束
    when(Fsm.currentState===MARK_ENUM.UPDATA_LEFT1){//更新左边四个点
        when(Left_Mark(0)=/=0&&Left_Mark(0)=/=io.Up_mark){
            Left_Mark(0):=io.Up_mark
            io.Mark_Out:=io.Up_mark
            io.Mark_Out_Valid:=True//将当前点标记更新为上标记
            io.Mark_Out_Addr:=Pixel_In_Cnt.count-2
        }
    }
    Fsm.UpData_Left2_End:=io.Lty_Para_mReady&&io.Lty_Para_mValid//数据发完就结束
    when(Fsm.currentState===MARK_ENUM.UPDATA_LEFT2){//更新左边四个点
        when(Left_Mark(1)=/=0&&Left_Mark(1)=/=io.Up_mark){
            Left_Mark(1):=io.Up_mark
            io.Mark_Out:=io.Up_mark
            io.Mark_Out_Valid:=True//将当前点标记更新为上标记
            io.Mark_Out_Addr:=Pixel_In_Cnt.count-3
        }

    }
    Fsm.UpData_Left3_End:=io.Lty_Para_mReady&&io.Lty_Para_mValid//数据发完就结束
    when(Fsm.currentState===MARK_ENUM.UPDATA_LEFT3){//更新左边四个点
        when(Left_Mark(2)=/=0&&Left_Mark(2)=/=io.Up_mark){
            Left_Mark(2):=io.Up_mark
            io.Mark_Out:=io.Up_mark
            io.Mark_Out_Valid:=True//将当前点标记更新为上标记
            io.Mark_Out_Addr:=Pixel_In_Cnt.count-4
        }
    }
    Fsm.UpData_Left4_End:=io.Lty_Para_mReady&&io.Lty_Para_mValid//数据发完就结束
    when(Fsm.currentState===MARK_ENUM.UPDATA_LEFT4){//更新左边四个点
        when(Left_Mark(3)=/=0&&Left_Mark(3)=/=io.Up_mark){
            Left_Mark(3):=io.Up_mark
            io.Mark_Out:=io.Up_mark
            io.Mark_Out_Valid:=True//将当前点标记更新为上标记
            io.Mark_Out_Addr:=Pixel_In_Cnt.count-5
        }
    }
//输出的要更新的标记点握手信号处理============================================================
    when(Fsm.currentState===MARK_ENUM.GEN_NEW_LTY||Fsm.currentState===MARK_ENUM.UP0_LEFT1||Fsm.currentState===MARK_ENUM.UP1_COND){
        io.Mark_Out_Valid:=True
    }
    
    // io.J_Out:=Pixel_In_Cnt.count-1//列标
//sData握手信号控制
    io.sData.ready:=Fsm.currentState===MARK_ENUM.GET_DATA&&(!Fsm.Get_Data_End)//只要在拿数据状态下，sReady一之拉高，直到拿到一个数据
}

class Feature_Mark extends Component{//整合图片缓存模块和标记模块
    val Lty_Cache_Module=new Lty_Feature_Cache
    val Lty_Mark_Up=new Lty_Mark_Sub_Module
    val Lty_Mark_Down=new Lty_Mark_Sub_Module
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(Config.LTY_DATA_BRAM_A_WIDTH bits))//进来的数据
        val start=in Bool()//lty计算启动信号

        val Temp_Back_Mean=in UInt(16 bits)//左移32Bit需要32 bit位宽---不过目前按左移12、13bit来处理的(仿真用左移10位)
        val Sign_Flag=in Bool()//有无符号位
        val Temp_Back_Thrd=in UInt(16 bits)//由于进来的图片像素点都是整形，而Temp_Back_Thrd的实际值带小数，所以可以将Temp_Back_Thrd向上取整
    }
    noIoPrefix()
    Lty_Cache_Module.io.sData<>io.sData
    Lty_Cache_Module.io.start<>io.start
    val Total_Num_Reg=Reg(UInt(log2Up(Config.LTY_PARAM_MEM_DEPTH) bits))init(0)//创建连通域计数器
    when(io.start){
        Total_Num_Reg:=0
    }otherwise{
        Total_Num_Reg:=Total_Num_Reg
    }
    //Line up=======================================================
    Lty_Mark_Up.io.start:=Lty_Cache_Module.io.strat_Sub_Module1
    Lty_Mark_Up.io.sData<>Lty_Cache_Module.io.mData1
    Lty_Mark_Up.io.Lty_Total_NUm:=Total_Num_Reg

    Lty_Mark_Up.io.Up_mark<>Lty_Cache_Module.io.Mark1Up_Out
    Lty_Mark_Up.io.Mark_Out_Addr<>Lty_Cache_Module.io.Mark1_In_Addr
    Lty_Mark_Up.io.Mark_Out_Valid<>Lty_Cache_Module.io.Mark1_In_Valid
    Lty_Mark_Up.io.Mark_Out<>Lty_Cache_Module.io.Mark1_In

    Lty_Mark_Up.io.Sign_Flag<>io.Sign_Flag
    Lty_Mark_Up.io.Temp_Back_Mean<>io.Temp_Back_Mean
    Lty_Mark_Up.io.Temp_Back_Thrd<>io.Temp_Back_Thrd

    Lty_Mark_Up.io.sData_Receive_End<>Lty_Cache_Module.io.mData1_End_Receive
    Lty_Mark_Up.io.Lty_Para_mReady:=True//待修改
    //Line Down=====================================================
    Lty_Mark_Down.io.start:=Lty_Cache_Module.io.strat_Sub_Module2
    Lty_Mark_Down.io.sData<>Lty_Cache_Module.io.mData2
    Lty_Mark_Down.io.Lty_Total_NUm:=Total_Num_Reg

    Lty_Mark_Down.io.Up_mark<>Lty_Cache_Module.io.Mark2Up_Out
    Lty_Mark_Down.io.Mark_Out_Addr<>Lty_Cache_Module.io.Mark2_In_Addr
    Lty_Mark_Down.io.Mark_Out_Valid<>Lty_Cache_Module.io.Mark2_In_Valid
    Lty_Mark_Down.io.Mark_Out<>Lty_Cache_Module.io.Mark2_In

    Lty_Mark_Down.io.Sign_Flag<>io.Sign_Flag
    Lty_Mark_Down.io.Temp_Back_Mean<>io.Temp_Back_Mean
    Lty_Mark_Down.io.Temp_Back_Thrd<>io.Temp_Back_Thrd    
    Lty_Mark_Down.io.Lty_Para_mReady:=True//待修改



    when(Lty_Mark_Up.io.New_Lty_Gen&&Lty_Mark_Down.io.New_Lty_Gen){
        Total_Num_Reg:=Total_Num_Reg+2//同时满足，加2个连通域
    }elsewhen(Lty_Mark_Up.io.New_Lty_Gen||Lty_Mark_Down.io.New_Lty_Gen){//只有一个连通域生成
        Total_Num_Reg:=Total_Num_Reg+1
    }
}
object LtyGen extends App { 
    val verilog_path="./testcode_gen/Lty_Gen" 
//    SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Lty_Mark_Gen)
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Feature_Mark)
}