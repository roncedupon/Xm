package Niuke
import spinal.core._
class VL1_421 extends Component{

	val io=new Bundle{
		val d0=in Bits(2 bits)
		val d1=in Bits(2 bits)
		val d2=in Bits(2 bits)
		val d3=in Bits(2 bits)
		val mux_out=out Bits(2 bits)
		val sel=in Bits(2 bits)
	}
noIoPrefix()

// d0    11--3
// d1    10--2
// d2    01--1
// d3    00--0

	//switch用于一选多
	switch(io.sel) {
		is(B"2'b00") {
			io.mux_out:=io.d3
		}
		is(B"2'b01") {
			io.mux_out:=io.d2
		}
		is(B"2'10"){
			io.mux_out:=io.d1
		}
		is(B"2'11"){
			io.mux_out:=io.d0
		}
	}
} 
