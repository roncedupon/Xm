package Config

import spinal.core._
case class TotalConfig(){
	val REG_TABLE_ADDR_DEPTH=1 MiB//寄存器表地址深度
	val REG_TABLE_DATA_WIDTH=32//寄存器表数据位宽
	val REG_TABLE_BASE_ADDR=0x0//寄存器表映射基地址
	val DATA_WIDTH=16//数据位宽度
}