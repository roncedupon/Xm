package MedFilter_V4
import spinal.core._
import spinal.lib.master
import spinal.lib.Delay
case class GeneratePorts(dataWidth: Int, Num: Int) extends Bundle {
    val mData_flow = Vec(master Stream UInt(dataWidth bits), Num)//Flow 数据,不需要ready,直接流入下面
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
    val IDLE,WAIT_9_ROWS,WAIT_5, DATA_VALID= newElement//这些状态应该都被用到,不然会有latch,不知道为啥
	//IDLE:就是啥也不干,IDLE的跳转
	//DATA_VALID_TMP,在DATA_VALID状态里还要再加一个DATA_VALID_TMP状态,用来满足mvalid信号
}

object Valid_Enum extends SpinalEnum(defaultEncoding = binaryOneHot) {
	//用来掐头去尾,每拿到一个col valid,等5拍再往外面发送有效数据
    val IDLE,DATA_VALID_TMP,DATA_UNVALID_TMP,WAIT_5= newElement//这些状态应该都被用到,不然会有latch,不知道为啥
	//IDLE:就是啥也不干,IDLE的跳转
	//DATA_VALID_TMP,在DATA_VALID状态里还要再加一个DATA_VALID_TMP状态,用来满足mvalid信号
}
case class Wait_5_Qtqw(Wait_9_Rows_Start:Bool,Start_Send:Bool,Stop_Send:Bool) extends Area{
	val currentState = Reg(Wait5Enum()) init Wait5Enum.IDLE//初始时一直等
    val nextState = Wait5Enum()
	val Wait_9_Rows_End=Bool()
	val Send_All_Rows=Bool()
	currentState := nextState
	switch(currentState){
		is(Wait5Enum.IDLE){
			when(Wait_9_Rows_Start){
				nextState:=Wait5Enum.WAIT_9_ROWS
			}otherwise{
				nextState:=Wait5Enum.IDLE
			}
		}
		is(Wait5Enum.WAIT_9_ROWS){
			when(Wait_9_Rows_End){
				nextState:=Wait5Enum.WAIT_5
			}otherwise{
				nextState:=Wait5Enum.WAIT_9_ROWS
			}
		}
		is(Wait5Enum.WAIT_5){//掐头
			when(Start_Send){
				nextState:=Wait5Enum.DATA_VALID
			}otherwise{
				nextState:=Wait5Enum.WAIT_5
			}
		}
		is(Wait5Enum.DATA_VALID){//
			when(Stop_Send&&Send_All_Rows){
				nextState:=Wait5Enum.IDLE
			}elsewhen(Stop_Send){//列计数为最后一列时才会结束发送
				nextState:=Wait5Enum.WAIT_5
			}otherwise{
				nextState:=Wait5Enum.DATA_VALID
			}
		}
		// is(Wait5Enum.PAUSE_SEND){
		// 	when(Mvalid_Set){
		// 		nextState:=Wait5Enum.WAIT_5
		// 	}otherwise{
		// 		nextState:=Wait5Enum.PAUSE_SEND
		// 	}
		// }
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
object Send_Data_Enum extends SpinalEnum(defaultEncoding = binaryOneHot) {
	//用来掐头去尾,每拿到一个col valid,等5拍再往外面发送有效数据
    val IDLE,WAIT,WAIT_PAUSE,SENDING_NOW,WAIT_TAIL= newElement//这些状态应该都被用到,不然会有latch,不知道为啥
	//IDLE:就是啥也不干,IDLE的跳转
	//DATA_VALID_TMP,在DATA_VALID状态里还要再加一个DATA_VALID_TMP状态,用来满足mvalid信号
}
case class Fsm_Send_Data_Ctrl(start: Bool) extends Area {
}
case class Data_Valid(start: Bool) extends Area {
	val currentState = Reg(Valid_Enum()) init Valid_Enum.IDLE//初始化状态机为IDLE状态
    val nextState = Valid_Enum()
	val Mvalid_Set=Bool()
	val Mvalid_Clear=Bool()
	val Tmp_Data_End=Bool()//一个DATA_VALID完成
	val Wait_5_Flag=Bool()
	currentState := nextState
	switch(currentState) {
		is(Valid_Enum.IDLE){
			when(start){
				nextState:=Valid_Enum.DATA_VALID_TMP
			}otherwise{
				nextState:=Valid_Enum.IDLE
			}
		}
		is(Valid_Enum.DATA_VALID_TMP){
			when(Tmp_Data_End){
				nextState:=Valid_Enum.IDLE
			}
			when(Mvalid_Clear){
				nextState:=Valid_Enum.DATA_UNVALID_TMP
			}otherwise{
				nextState:=Valid_Enum.DATA_VALID_TMP
			}
		}
		is(Valid_Enum.DATA_UNVALID_TMP){
			when(Tmp_Data_End){
				nextState:=Valid_Enum.IDLE
			}elsewhen(Mvalid_Set){
				nextState:=Valid_Enum.WAIT_5
			}otherwise{
				nextState:=Valid_Enum.DATA_UNVALID_TMP
			}
		}
		is(Valid_Enum.WAIT_5){
			when(Wait_5_Flag){
				nextState:=Valid_Enum.DATA_VALID_TMP
			}otherwise{
				nextState:=Valid_Enum.WAIT_5
			}
		}
	}
}
