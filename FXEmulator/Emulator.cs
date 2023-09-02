using System;
using System.Linq;
using System.Collections.Generic;
using System.Numerics;
using SFML.Graphics;

namespace FXEmulator;

//public delegate void WriteEvent(ushort address, byte value);

//public delegate byte ReadEvent(ushort address);

public delegate byte instruct();

//Status Flags
public enum sf
{
	C = (byte)(1 << 0), //1   Carry bit
	Z = (byte)(1 << 1), //2   Zero
	I = (byte)(1 << 2), //4   Disable Interupts
	D = (byte)(1 << 3), //8   Decimal Mode (un-used in this implementation)
	B = (byte)(1 << 4), //16  Break
	U = (byte)(1 << 5), //32  unusesd
	V = (byte)(1 << 6), //64  Overflow
	N = (byte)(1 << 7), //128 Negative
}
//still gets allocated on the heap -_-
public struct INSTRUCTION
{
	public INSTRUCTION() { }
	public INSTRUCTION(string n, instruct o, instruct a, byte c)
	{
		name = n;
		operate = o;
		addrmode = a;
		cycles = c;
	}
	public string name;
	public instruct operate;
	public instruct addrmode;
	public byte cycles;
}

public class Emulator
{
	public Emulator()
	{
		lookup = new List<INSTRUCTION>()
		{
			/*00*/new("BRK",BRK,IMM,7),/*01*/new("ORA",ORA,IZX,6),/*02*/new("???",XXX,IMP,2),/*03*/new("???",XXX,IMP,8),/*04*/new("???",NOP,IMP,3),/*05*/new("ORA",ORA,ZP0,3),/*06*/new("ASL",ASL,ZP0,5),/*07*/new("???",XXX,IMP,5),/*08*/new("PHP",PHP,IMP,3),/*09*/new("ORA",ORA,IMM,2),/*0a*/new("ASL",ASL,IMP,2),/*0b*/new("???",XXX,IMP,2),/*0c*/new("???",NOP,IMP,4),/*0d*/new("ORA",ORA,ABS,4),/*0e*/new("ASL",ASL,ABS,6),/*0f*/new("???",XXX,IMP,6),
			/*10*/new("BPL",BPL,REL,2),/*11*/new("ORA",ORA,IZY,5),/*12*/new("???",XXX,IMP,2),/*13*/new("???",XXX,IMP,8),/*14*/new("???",NOP,IMP,4),/*15*/new("ORA",ORA,ZPX,4),/*16*/new("ASL",ASL,ZPX,6),/*17*/new("???",XXX,IMP,6),/*18*/new("CLC",CLC,IMP,2),/*19*/new("ORA",ORA,ABY,4),/*1a*/new("???",NOP,IMP,2),/*1b*/new("???",XXX,IMP,7),/*1c*/new("???",NOP,IMP,4),/*1d*/new("ORA",ORA,ABX,4),/*1e*/new("ASL",ASL,ABX,7),/*1f*/new("???",XXX,IMP,7),
			/*20*/new("JSR",JSR,ABS,6),/*21*/new("AND",AND,IZX,6),/*22*/new("???",XXX,IMP,2),/*23*/new("???",XXX,IMP,8),/*24*/new("BIT",BIT,ZP0,3),/*25*/new("AND",AND,ZP0,3),/*26*/new("ROL",ROL,ZP0,5),/*27*/new("???",XXX,IMP,5),/*28*/new("PLP",PLP,IMP,4),/*29*/new("AND",AND,IMM,2),/*2a*/new("ROL",ROL,IMP,2),/*2b*/new("???",XXX,IMP,2),/*2c*/new("BIT",BIT,ABS,4),/*2d*/new("AND",AND,ABS,4),/*2e*/new("ROL",ROL,ABS,6),/*2f*/new("???",XXX,IMP,6),
			/*30*/new("BMI",BMI,REL,2),/*31*/new("AND",AND,IZY,5),/*32*/new("???",XXX,IMP,2),/*33*/new("???",XXX,IMP,8),/*34*/new("???",NOP,IMP,4),/*35*/new("AND",AND,ZPX,4),/*36*/new("ROL",ROL,ZPX,6),/*37*/new("???",XXX,IMP,6),/*38*/new("SEC",SEC,IMP,2),/*39*/new("AND",AND,ABY,4),/*3a*/new("???",NOP,IMP,2),/*3b*/new("???",XXX,IMP,7),/*3c*/new("???",NOP,IMP,4),/*3d*/new("AND",AND,ABX,4),/*3e*/new("ROL",ROL,ABX,7),/*3f*/new("???",XXX,IMP,7),
			/*40*/new("RTI",RTI,IMP,6),/*41*/new("EOR",EOR,IZX,6),/*42*/new("???",XXX,IMP,2),/*43*/new("???",XXX,IMP,8),/*44*/new("???",NOP,IMP,3),/*45*/new("EOR",EOR,ZP0,3),/*46*/new("LSR",LSR,ZP0,5),/*47*/new("???",XXX,IMP,5),/*48*/new("PHA",PHA,IMP,3),/*49*/new("EOR",EOR,IMM,2),/*4a*/new("LSR",LSR,IMP,2),/*4b*/new("???",XXX,IMP,2),/*4c*/new("JMP",JMP,ABS,3),/*4d*/new("EOR",EOR,ABS,4),/*4e*/new("LSR",LSR,ABS,6),/*4f*/new("???",XXX,IMP,6),
			/*50*/new("BVC",BVC,REL,2),/*51*/new("EOR",EOR,IZY,5),/*52*/new("???",XXX,IMP,2),/*53*/new("???",XXX,IMP,8),/*54*/new("???",NOP,IMP,4),/*55*/new("EOR",EOR,ZPX,4),/*56*/new("LSR",LSR,ZPX,6),/*57*/new("???",XXX,IMP,6),/*58*/new("CLI",CLI,IMP,2),/*59*/new("EOR",EOR,ABY,4),/*5a*/new("???",NOP,IMP,2),/*5b*/new("???",XXX,IMP,7),/*5c*/new("???",NOP,IMP,4),/*5d*/new("EOR",EOR,ABX,4),/*5e*/new("LSR",LSR,ABX,7),/*5f*/new("???",XXX,IMP,7),
			/*60*/new("RTS",RTS,IMP,6),/*61*/new("ADC",ADC,IZX,6),/*62*/new("???",XXX,IMP,2),/*63*/new("???",XXX,IMP,8),/*64*/new("???",NOP,IMP,3),/*65*/new("ADC",ADC,ZP0,3),/*66*/new("ROR",ROR,ZP0,5),/*67*/new("???",XXX,IMP,5),/*68*/new("PLA",PLA,IMP,4),/*69*/new("ADC",ADC,IMM,2),/*6a*/new("ROR",ROR,IMP,2),/*6b*/new("???",XXX,IMP,2),/*6c*/new("JMP",JMP,IND,5),/*6d*/new("ADC",ADC,ABS,4),/*6e*/new("ROR",ROR,ABS,6),/*6f*/new("???",XXX,IMP,6),
			/*70*/new("BVS",BVS,REL,2),/*71*/new("ADC",ADC,IZY,5),/*72*/new("???",XXX,IMP,2),/*73*/new("???",XXX,IMP,8),/*74*/new("???",NOP,IMP,4),/*75*/new("ADC",ADC,ZPX,4),/*76*/new("ROR",ROR,ZPX,6),/*77*/new("???",XXX,IMP,6),/*78*/new("SEI",SEI,IMP,2),/*79*/new("ADC",ADC,ABY,4),/*7a*/new("???",NOP,IMP,2),/*7b*/new("???",XXX,IMP,7),/*7c*/new("???",NOP,IMP,4),/*7d*/new("ADC",ADC,ABX,4),/*7e*/new("ROR",ROR,ABX,7),/*7f*/new("???",XXX,IMP,7),
			/*80*/new("???",NOP,IMP,2),/*81*/new("STA",STA,IZX,6),/*82*/new("???",NOP,IMP,2),/*83*/new("???",XXX,IMP,6),/*84*/new("STY",STY,ZP0,3),/*85*/new("STA",STA,ZP0,3),/*86*/new("STX",STX,ZP0,3),/*87*/new("???",XXX,IMP,3),/*88*/new("DEY",DEY,IMP,2),/*89*/new("???",NOP,IMP,2),/*8a*/new("TXA",TXA,IMP,2),/*8b*/new("???",XXX,IMP,2),/*8c*/new("STY",STY,ABS,4),/*8d*/new("STA",STA,ABS,4),/*8e*/new("STX",STX,ABS,4),/*8f*/new("???",XXX,IMP,4),
			/*90*/new("BCC",BCC,REL,2),/*91*/new("STA",STA,IZY,6),/*92*/new("???",XXX,IMP,2),/*93*/new("???",XXX,IMP,6),/*94*/new("STY",STY,ZPX,4),/*95*/new("STA",STA,ZPX,4),/*96*/new("STX",STX,ZPY,4),/*97*/new("???",XXX,IMP,4),/*98*/new("TYA",TYA,IMP,2),/*99*/new("STA",STA,ABY,5),/*9a*/new("TXS",TXS,IMP,2),/*9b*/new("???",XXX,IMP,5),/*9c*/new("???",NOP,IMP,5),/*9d*/new("STA",STA,ABX,5),/*9e*/new("???",XXX,IMP,5),/*9f*/new("???",XXX,IMP,5),
			/*a0*/new("LDY",LDY,IMM,2),/*a1*/new("LDA",LDA,IZX,6),/*a2*/new("LDX",LDX,IMM,2),/*a3*/new("???",XXX,IMP,6),/*a4*/new("LDY",LDY,ZP0,3),/*a5*/new("LDA",LDA,ZP0,3),/*a6*/new("LDX",LDX,ZP0,3),/*a7*/new("???",XXX,IMP,3),/*a8*/new("TAY",TAY,IMP,2),/*a9*/new("LDA",LDA,IMM,2),/*aa*/new("TAX",TAX,IMP,2),/*ab*/new("???",XXX,IMP,2),/*ac*/new("LDY",LDY,ABS,4),/*ad*/new("LDA",LDA,ABS,4),/*ae*/new("LDX",LDX,ABS,4),/*af*/new("???",XXX,IMP,4),
			/*b0*/new("BCS",BCS,REL,2),/*b1*/new("LDA",LDA,IZY,5),/*b2*/new("???",XXX,IMP,2),/*b3*/new("???",XXX,IMP,5),/*b4*/new("LDY",LDY,ZPX,4),/*b5*/new("LDA",LDA,ZPX,4),/*b6*/new("LDX",LDX,ZPY,4),/*b7*/new("???",XXX,IMP,4),/*b8*/new("CLV",CLV,IMP,2),/*b9*/new("LDA",LDA,ABY,4),/*ba*/new("TSX",TSX,IMP,2),/*bb*/new("???",XXX,IMP,4),/*bc*/new("LDY",LDY,ABX,4),/*bd*/new("LDA",LDA,ABX,4),/*be*/new("LDX",LDX,ABY,4),/*bf*/new("???",XXX,IMP,4),
			/*c0*/new("CPY",CPY,IMM,2),/*c1*/new("CMP",CMP,IZX,6),/*c2*/new("???",NOP,IMP,2),/*c3*/new("???",XXX,IMP,8),/*c4*/new("CPY",CPY,ZP0,3),/*c5*/new("CMP",CMP,ZP0,3),/*c6*/new("DEC",DEC,ZP0,5),/*c7*/new("???",XXX,IMP,5),/*c8*/new("INY",INY,IMP,2),/*c9*/new("CMP",CMP,IMM,2),/*ca*/new("DEX",DEX,IMP,2),/*cb*/new("???",XXX,IMP,2),/*cc*/new("CPY",CPY,ABS,4),/*cd*/new("CMP",CMP,ABS,4),/*ce*/new("DEC",DEC,ABS,6),/*cf*/new("???",XXX,IMP,6),
			/*d0*/new("BNE",BNE,REL,2),/*d1*/new("CMP",CMP,IZY,5),/*d2*/new("???",XXX,IMP,2),/*d3*/new("???",XXX,IMP,8),/*d4*/new("???",NOP,IMP,4),/*d5*/new("CMP",CMP,ZPX,4),/*d6*/new("DEC",DEC,ZPX,6),/*d7*/new("???",XXX,IMP,6),/*d8*/new("CLD",CLD,IMP,2),/*d9*/new("CMP",CMP,ABY,4),/*da*/new("NOP",NOP,IMP,2),/*db*/new("???",XXX,IMP,7),/*dc*/new("???",NOP,IMP,4),/*dd*/new("CMP",CMP,ABX,4),/*de*/new("DEC",DEC,ABX,7),/*df*/new("???",XXX,IMP,7),
			/*e0*/new("CPX",CPX,IMM,2),/*e1*/new("SBC",SBC,IZX,6),/*e2*/new("???",NOP,IMP,2),/*e3*/new("???",XXX,IMP,8),/*e4*/new("CPX",CPX,ZP0,3),/*e5*/new("SBC",SBC,ZP0,3),/*e6*/new("INC",INC,ZP0,5),/*e7*/new("???",XXX,IMP,5),/*e8*/new("INX",INX,IMP,2),/*e9*/new("SBC",SBC,IMM,2),/*ea*/new("NOP",NOP,IMP,2),/*eb*/new("???",SBC,IMP,2),/*ec*/new("CPX",CPX,ABS,4),/*ed*/new("SBC",SBC,ABS,4),/*ee*/new("INC",INC,ABS,6),/*ef*/new("???",XXX,IMP,6),
			/*f0*/new("BEQ",BEQ,REL,2),/*f1*/new("SBC",SBC,IZY,5),/*f2*/new("???",XXX,IMP,2),/*f3*/new("???",XXX,IMP,8),/*f4*/new("???",NOP,IMP,4),/*f5*/new("SBC",SBC,ZPX,4),/*f6*/new("INC",INC,ZPX,6),/*f7*/new("???",XXX,IMP,6),/*f8*/new("SED",SED,IMP,2),/*f9*/new("SBC",SBC,ABY,4),/*fa*/new("NOP",NOP,IMP,2),/*fb*/new("???",XXX,IMP,7),/*fc*/new("???",NOP,IMP,4),/*fd*/new("SBC",SBC,ABX,4),/*fe*/new("INC",INC,ABX,7),/*ff*/new("???",XXX,IMP,7),
        };
	}

    public byte[] data = new byte[0x10000];


    //Accumalator / A register
    private byte a = 0x00;
	//x register
	private byte x = 0x00;
	//y register
	private byte y = 0x00;
	//whatever we last fetched
	private byte fetched = 0x00;
	//status register
	private byte status = 0x00;
	
	private ushort temp = 0x00;

	//address relative and absolute
	private ushort addr_rel = 0x00;
	private ushort addr_abs = 0x0000;

	//current opcode
	private byte opcode = 0x00;

	//the amount of cycles to take
	private byte cycles = 0;

	//keep track of clock
	private ulong clock_count = 0;

	//Program Counter
	private ushort pc = 0xfffe;

	//Stack Pointer
	private byte sp = 0xff;

	private List<INSTRUCTION> lookup = new();

	//public WriteEvent OnWrite;
	//public ReadEvent OnRead;

	public bool Debug = false;

	private byte read(ushort addr)
	{
		if (Debug)
		{ 
			addr.print("reading:",n:4);
			byte val = Read(addr);
			val.print("got value:");
			return val;
		}
		else
			return Read(addr);
	}
	private void write(ushort addr,byte val)
	{
		if (Debug) 
		{
			addr.print("writing to:",n:4);
			val.print("with value:");
		}
		Write(addr,val);
	}

    public byte Read(ushort addr)
    {
        return data[addr];
    }
    public void Write(ushort addr, byte val)
    {
        if (addr.isBetween<ushort>(0x0000, 0xFFFF))
        {
            //if (addr.isBetween<ushort>(0x0200, 0x05FF))
            //{
            //    int index = (int)(addr - 0x0200u);
            //    (int x, int y) = IndexToXY(index);
            //     //Program.Image?.SetPixel((uint)x, (uint)y,Color.Green);
            //}
            //else
            //{
            //}
			data[addr] = val;
        }
    }
    public (int x, int y) IndexToXY(int i)
    {
        int x = i % 512;
        int y = i / 512;
        return (x, y);
    }
    private byte fetch()
	{
        if (!(lookup[opcode].addrmode == IMP))//everything except implied!
            fetched = read(addr_abs);
        return fetched;
    }

	public void clock()
	{
		if (cycles == 0)
		{
			opcode = read(pc);
			if (Debug)
			{
				opcode.print("opcode: ",wl:false,c:ConsoleColor.Cyan);
				lookup[opcode].name.print(" - ",hex:false,c: ConsoleColor.Cyan);
			}
			SetFlag(sf.U, true); //always true
			pc++;

			//get cycles from lookup
			cycles = lookup[opcode].cycles;
			//Aditional Cycles1
			byte ac1 = lookup[opcode].addrmode();
			//Aditional Cycles2
			byte ac2 = lookup[opcode].operate();

			cycles += (byte)(ac1 & ac2);

			SetFlag(sf.U, true); //set true again
		}
		clock_count++;
		cycles--;
	}
	public void reset()
	{
		addr_abs = 0xfffc;

		ushort lo = read((ushort)(addr_abs + 0));
		ushort hi = read((ushort)(addr_abs + 1));

		pc = (ushort)((hi << 8) | lo);

		//print some debug info
		if (Debug)
		{ 
			hi.print("hi ");
			lo.print("lo ");
			pc.print("pc ", n: 4);
		}


		//reset the registers
		a = 0;
		x = 0;
		y = 0;

		sp = 0xFD;
		//reset status
		//status unused bit is always 1
		status = 0x00 | (byte)sf.U;

		//clear helper vars
		addr_rel = 0x0000;
		addr_abs = 0x0000;
		fetched = 0x00;

		//reset sequence takes 7 clock cycles, but 8 because logic i guess
		cycles = 8;
	}
	public bool complete() 
	{
		return (cycles == 0);
	}
	public void Emulate()
	{
		reset();
		step();
		//pc.print("instruction at ", n: 4);
		//read(pc).print();
	}
	public void step(int amount = 1) 
	{
        do
        {
            clock();
        }
        while (complete());
		amount--;
		if (amount > 0)
			step(amount);
    }
	private void SetFlag(sf f, bool v) //voltage? weatger on or off
	{
		if (v) //set
			status |= (byte)f;
		else //clear/reset
			status &= (byte)~f;
	}
	private byte GetFlag(sf f)
	{
		return (byte)(((status & (byte)f) > (byte)0) ? 1 : 0);
	}
    public bool CheckFlag(sf f)
    {
        return ((status & (byte)f) > (byte)0);
    }
    #region addressing modes
    private byte IMP()
	{
		fetched = a;
		return 0;
	}
	private byte IMM()
	{
		addr_abs = pc++;
		return 0;
	}
	private byte ZP0()
	{
		addr_abs = read(pc);
		pc++;
		addr_abs &= 0x00FF;
		return 0;
	}
	private byte ZPX()
	{
		addr_abs = (ushort)(read(pc) + x);
		pc++;
		addr_abs &= 0x00FF;
		return 0;
	}
	private byte ZPY()
	{
		addr_abs = (ushort)(read(pc) + y);
		pc++;
		addr_abs &= 0x00FF;
		return 0;
	}
	private byte ABS()
	{
		ushort lo = read(pc);
		pc++;
		ushort hi = read(pc);
		pc++;

		addr_abs = (ushort)((hi << 8) | lo);
		return 0;
	}
	private byte ABX()
	{
		ushort lo = read(pc);
		pc++;
		ushort hi = read(pc);
		pc++;

		addr_abs = (ushort)((hi << 8) | lo);
		addr_abs += x;

		//however!
		//this might be a diff page!, so we might need an extra clock scyle so we add a 1 if so! 

		if ((addr_abs & 0xFF00) != (hi << 8))
			return 1;
		else
			return 0;
	}
	private byte ABY()
	{
		ushort lo = read(pc);
		pc++;
		ushort hi = read(pc);
		pc++;

		addr_abs = (ushort)((hi << 8) | lo);
		addr_abs += y;

		//however!
		//this might be a diff page!, so we might need an extra clock scyle so we add a 1 if so! 

		if ((addr_abs & 0xFF00) != (hi << 8))
			return 1;
		else
			return 0;
	}
	private byte IND() //(pointers)
	{
		ushort ptr_lo = read(pc);
		pc++;
		ushort ptr_hi = read(pc);
		pc++;

		ushort ptr = (ushort)((ptr_hi << 8) | ptr_lo);

		//we add bug :3
		if (ptr_lo == 0x00FF) // Simulate page boundary hardware bug
		{
			addr_abs = (ushort)((ushort)(read((ushort)(ptr & 0xFF00)) << 8) | read((ushort)(ptr + 0)));
		}
		else // Behave normally
		{
			//addr_abs = (ushort)(read((ushort)(ptr + 1)) << 8) | (ushort)read((ushort)(ptr + 0));
            addr_abs = (ushort)((read((ushort)(ptr + 1)) << 8) | read((ushort)(ptr + 0)));

        }


        return 0;
	}
	private byte IZX()
	{
		ushort t = read(pc);
		pc++;

		ushort lo = read((ushort)((ushort)(t + x) & 0x00FF));
		ushort hi = read((ushort)((ushort)(t + x + 1) & 0x00FF));

		addr_abs = (ushort)((hi << 8) | lo);

		return 0;
	}
	private byte IZY()
	{
		ushort t = read(pc);
		pc++;

		ushort lo = read((ushort)(t & 0x00FF));
		ushort hi = read((ushort)((t + 1) & 0x00FF));

		addr_abs = (ushort)((hi << 8) | lo);
		addr_abs += y;

		if ((addr_abs & 0xFF00) != (hi << 8))
			return 1;
		else
			return 0;
	}
	private byte REL()
	{
		addr_rel = read(pc);
		pc++;

		//signed bulls#%t
		//fix
		//org:if (addr_rel & 0x80)
		if ((addr_rel & 0x80) != 0)
			addr_rel |= 0xFF00;

		return 0;
	}
	#endregion
	#region instructions
	private byte AND()
	{
		fetch();
		a = (byte)(a & fetched);
		SetFlag(sf.Z, a == 0x00); //if its Zero set Zero to true
								  //fix
								  //org:SetFlag(sf.N, a & 0x80);
		SetFlag(sf.N, (a & 0x80) != 0); //if its Negative set Negative to true
		return 1;
	}
	//branches :3
	private byte BCS()
	{
		if (GetFlag(sf.C) == 1)
		{
			cycles++;
			addr_abs = (ushort)(pc + addr_rel);

			if ((addr_abs & 0xFF00) != (pc & 0xFF00))
				cycles++;

			pc = addr_abs;
		}
		return 0;
	}
	private byte BCC()
	{
		if (GetFlag(sf.C) == 0)
		{
			cycles++;
			addr_abs = (ushort)(pc + addr_rel);

			if ((addr_abs & 0xFF00) != (pc & 0xFF00))
				cycles++;

			pc = addr_abs;
		}
		return 0;
	}
	private byte BEQ()
	{
		if (GetFlag(sf.Z) == 1)
		{
			cycles++;
			addr_abs = (ushort)(pc + addr_rel);

			if ((addr_abs & 0xFF00) != (pc & 0xFF00))
				cycles++;

			pc = addr_abs;
		}
		return 0;
	}
	private byte BMI()
	{
		if (GetFlag(sf.N) == 1)
		{
			cycles++;
			addr_abs = (ushort)(pc + addr_rel);

			if ((addr_abs & 0xFF00) != (pc & 0xFF00))
				cycles++;

			pc = addr_abs;
		}
		return 0;
	}
	private byte BNE()
	{
		if (GetFlag(sf.Z) == 0)
		{
			cycles++;
			addr_abs = (ushort)(pc + addr_rel);

			if ((addr_abs & 0xFF00) != (pc & 0xFF00))
				cycles++;

			pc = addr_abs;
		}
		return 0;
	}
	private byte BPL()
	{
		if (GetFlag(sf.N) == 0)
		{
			cycles++;
			addr_abs = (ushort)(pc + addr_rel);

			if ((addr_abs & 0xFF00) != (pc & 0xFF00))
				cycles++;

			pc = addr_abs;
		}
		return 0;
	}
	private byte BVC()
	{
		if (GetFlag(sf.V) == 0)
		{
			cycles++;
			addr_abs = (ushort)(pc + addr_rel);

			if ((addr_abs & 0xFF00) != (pc & 0xFF00))
				cycles++;

			pc = addr_abs;
		}
		return 0;
	}
	private byte BVS()
	{
		if (GetFlag(sf.V) == 1)
		{
			cycles++;
			addr_abs = (ushort)(pc + addr_rel);

			if ((addr_abs & 0xFF00) != (pc & 0xFF00))
				cycles++;

			pc = addr_abs;
		}
		return 0;
	}

	//set flags :3
	private byte CLC()
	{
		SetFlag(sf.C, false);
		return 0;
	}
	private byte CLD()
	{
		SetFlag(sf.D, false);
		return 0;
	}
	private byte CLI()
	{
		SetFlag(sf.I, false);
		return 0;
	}
	private byte CLV()
	{
		SetFlag(sf.V, false);
		return 0;
	}


	//other bs :D
	// Instruction: Compare Accumulator
	private byte CMP()
	{
		fetch();
		temp = (ushort)((ushort)a - (ushort)fetched);
		SetFlag(sf.C, a >= fetched);
		SetFlag(sf.Z, (temp & 0x00FF) == 0x0000);
		SetFlag(sf.N, (temp & 0x0080) != 0);
		return 1;
	}
	// Instruction: Compare X Register
	private byte CPX()
	{
		fetch();
		temp = (ushort)((ushort)x - (ushort)fetched);
		SetFlag(sf.C, x >= fetched);
		SetFlag(sf.Z, (temp & 0x00FF) == 0x0000);
		SetFlag(sf.N, (temp & 0x0080) != 0);
		return 0;
	}
	// Instruction: Compare Y Register
	private byte CPY()
	{
		fetch();
		temp = (ushort)((ushort)y - (ushort)fetched);
		SetFlag(sf.C, y >= fetched);
		SetFlag(sf.Z, (temp & 0x00FF) == 0x0000);
		SetFlag(sf.N, (temp & 0x0080) != 0);
		return 0;
	}
	// Instruction: Decrement Value at Memory Location
	private byte DEC()
	{
		fetch();
		temp = (ushort)(fetched - 1);
		write(addr_abs, (byte)(temp & 0x00FF));
		SetFlag(sf.Z, (temp & 0x00FF) == 0x0000);
		SetFlag(sf.N, (temp & 0x0080) != 0);
		return 0;
	}
	// Instruction: Decrement X Register
	private byte DEX()
	{
		x--;
		SetFlag(sf.Z, x == 0x00);
		SetFlag(sf.N, (x & 0x80) != 0);
		return 0;
	}
	// Instruction: Decrement Y Register
	private byte DEY()
	{
		y--;
		SetFlag(sf.Z, y == 0x00);
		SetFlag(sf.N, (y & 0x80) != 0);
		return 0;
	}
	// Instruction: Bitwise Logic XOR
	private byte EOR()
	{
		fetch();
		a = (byte)(a ^ fetched);
		SetFlag(sf.Z, a == 0x00);
		SetFlag(sf.N, (a & 0x80) != 0);
		return 1;
	}
	// Instruction: Increment Value at Memory Location
	private byte INC()
	{
		fetch();
		temp = (ushort)(fetched + 1);
		write(addr_abs, (byte)(temp & 0x00FF));
		SetFlag(sf.Z, (temp & 0x00FF) == 0x0000);
		SetFlag(sf.N, (temp & 0x0080) != 0);
		return 0;
	}
	// Instruction: Increment X Register
	private byte INX()
	{
		x++;
		SetFlag(sf.Z, x == 0x00);
		SetFlag(sf.N, (x & 0x80) != 0);
		return 0;
	}
	// Instruction: Increment Y Register
	private byte INY()
	{
		y++;
		SetFlag(sf.Z, y == 0x00);
		SetFlag(sf.N, (y & 0x80) != 0);
		return 0;
	}
    // Instruction: Jump To Location
    private byte JMP()
    {
        pc = addr_abs;
        return 0;
    }
    // Instruction: Jump To Sub-Routine
    private byte JSR()
    {
        pc--;

        write((ushort)(0x0100 + sp), (byte)((pc >> 8) & 0x00FF));
        sp--;
        write((ushort)(0x0100 + sp), (byte)(pc & 0x00FF));
        sp--;

        pc = addr_abs;
        return 0;
    }
    // Instruction: Load The Accumulator
    private byte LDA()
    {
        fetch();
        a = fetched;
        SetFlag(sf.Z, a == 0x00);
        SetFlag(sf.N, (a & 0x80) != 0);
        return 1;
    }
    // Instruction: Load The X Register
    private byte LDX()
    {
        fetch();
        x = fetched;
        SetFlag(sf.Z, x == 0x00);
        SetFlag(sf.N, (x & 0x80) != 0);
        return 1;
    }
    // Instruction: Load The Y Register
    private byte LDY()
    {
        fetch();
        y = fetched;
        SetFlag(sf.Z, y == 0x00);
        SetFlag(sf.N, (y & 0x80) != 0);
        return 1;
    }



    private byte LSR()
    {
        fetch();
        SetFlag(sf.C, (fetched & 0x0001) != 0);
        temp = (ushort)(fetched >> 1);
        SetFlag(sf.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(sf.N, (temp & 0x0080) != 0);
        if (lookup[opcode].addrmode == IMP)
            a = (byte)(temp & 0x00FF);
        else
            write(addr_abs, (byte)(temp & 0x00FF));
        return 0;
    }
    private byte NOP()
    {
        // Sadly not all NOPs are equal, Ive added a few here
        // based on https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
        // and will add more based on game compatibility, and ultimately
        // I'd like to cover all illegal opcodes too
        switch (opcode)
        {
            case 0x1C:
            case 0x3C:
            case 0x5C:
            case 0x7C:
            case 0xDC:
            case 0xFC:
                return 1;
                break;
        }
        return 0;
    }
    // Instruction: Bitwise Logic OR
    private byte ORA()
    {
        fetch();
        a = (byte)(a | fetched);
        SetFlag(sf.Z, a == 0x00);
        SetFlag(sf.N, (a & 0x80) != 0);
        return 1;
    }
    // Instruction: Push Accumulator to Stack
    private byte PHA()
    {
        write((ushort)(0x0100 + sp), a);
        sp--;
        return 0;
    }
    // Instruction: Push Status Register to Stack
    private byte PHP()
    {
        write((ushort)(0x0100 + sp), (byte)(status | (byte)sf.B | (byte)sf.U));
        SetFlag(sf.B, false);
        SetFlag(sf.U, false);
        sp--;
        return 0;
    }
    // Instruction: Pop Accumulator off Stack
    private byte PLA()
    {
        sp++;
        a = read((ushort)(0x0100 + sp));
        SetFlag(sf.Z, a == 0x00);
        SetFlag(sf.N, (a & 0x80) != 0);
        return 0;
    }
    // Instruction: Pop Status Register off Stack
    private byte PLP()
    {
        sp++;
        status = read((ushort)(0x0100 + sp));
        SetFlag(sf.U, true);
        return 0;
    }

    private byte ROL()
    {
        fetch();
        temp = (ushort)((ushort)(fetched << 1) | GetFlag(sf.C));
        SetFlag(sf.C, (temp & 0xFF00) != 0);
        SetFlag(sf.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(sf.N, (temp & 0x0080) != 0);
        if (lookup[opcode].addrmode == IMP)
            a = (byte)(temp & 0x00FF);
        else
            write(addr_abs, (byte)(temp & 0x00FF));
        return 0;
    }
    private byte ROR()
    {
        fetch();
        temp = (ushort)((ushort)(GetFlag(sf.C) << 7) | (ushort)(fetched >> 1));
        SetFlag(sf.C, (fetched & 0x01) != 0);
        SetFlag(sf.Z, (temp & 0x00FF) == 0x00);
        SetFlag(sf.N, (temp & 0x0080) != 0);
        if (lookup[opcode].addrmode == IMP)
            a = (byte)(temp & 0x00FF);
        else
            write(addr_abs, (byte)(temp & 0x00FF));
        return 0;
    }
    private byte RTS()
    {
        sp++;
        pc = (ushort)read((ushort)(0x0100 + sp));
        sp++;
        //(void)(pc |= (ushort)read((ushort)((ushort)(0x0100 + sp)) << 8));
        pc |= (ushort)(read((ushort)(0x0100 + sp)) << 8);


        pc++;
        return 0;
    }
    // Instruction: Set Carry Flag
    private byte SEC()
    {
        SetFlag(sf.C, true);
        return 0;
    }

    // Instruction: Set Decimal Flag
    private byte SED()
    {
        SetFlag(sf.D, true);
        return 0;
    }
    // Instruction: Set Interrupt Flag / Enable Interrupts
    private byte SEI()
    {
        SetFlag(sf.I, true);
        return 0;
    }
    // Instruction: Store Accumulator at Address
    private byte STA()
    {
        write(addr_abs, a);
        return 0;
    }
    // Instruction: Store X Register at Address
    private byte STX()
    {
        write(addr_abs, x);
        return 0;
    }
    // Instruction: Store Y Register at Address
    private byte STY()
    {
        write(addr_abs, y);
        return 0;
    }
    // Instruction: Transfer Accumulator to X Register
    private byte TAX()
    {
        x = a;
        SetFlag(sf.Z, x == 0x00);
        SetFlag(sf.N, (x & 0x80) != 0);
        return 0;
    }
    // Instruction: Transfer Accumulator to Y Register
    private byte TAY()
    {
        y = a;
        SetFlag(sf.Z, y == 0x00);
        SetFlag(sf.N, (y & 0x80) != 0);
        return 0;
    }
    // Instruction: Transfer Stack Pointer to X Register
    private byte TSX()
    {
        x = sp;
        SetFlag(sf.Z, x == 0x00);
        SetFlag(sf.N, (x & 0x80) != 0);
        return 0;
    }
    // Instruction: Transfer X Register to Accumulator
    private byte TXA()
    {
        a = x;
        SetFlag(sf.Z, a == 0x00);
        SetFlag(sf.N, (a & 0x80) != 0);
        return 0;
    }
    // Instruction: Transfer X Register to Stack Pointer
    private byte TXS()
    {
        sp = x;
        return 0;
    }
    // Instruction: Transfer Y Register to Accumulator
    private byte TYA()
    {
        a = y;
        SetFlag(sf.Z, a == 0x00);
        SetFlag(sf.N, (a & 0x80) != 0);
        return 0;
    }
    private byte ADC()
    {
        // Grab the data that we are adding to the accumulator
        fetch();

        // Add is performed in 16-bit domain for emulation to capture any
        // carry bit, which will exist in bit 8 of the 16-bit word
        temp = (ushort)((ushort)a + (ushort)fetched + (ushort)GetFlag(sf.C));

        // The carry flag out exists in the high byte bit 0
        SetFlag(sf.C, temp > 255);

        // The Zero flag is set if the result is 0
        SetFlag(sf.Z, (temp & 0x00FF) == 0);

        // The signed Overflow flag is set based on all that up there! :D
        SetFlag(sf.V, ((~((ushort)a ^ (ushort)fetched) & ((ushort)a ^ (ushort)temp)) & 0x0080) != 0);

        // The negative flag is set to the most significant bit of the result
        SetFlag(sf.N, (temp & 0x80) != 0);

        // Load the result into the accumulator (it's 8-bit dont forget!)
        a = (byte)(temp & 0x00FF);

        // This instruction has the potential to require an additional clock cycle
        return 1;
    }
    // Instruction: Arithmetic Shift Left
    private byte ASL()
    {
        fetch();
        temp = (ushort)((ushort)fetched << 1);
        SetFlag(sf.C, (temp & 0xFF00) > 0);
        SetFlag(sf.Z, (temp & 0x00FF) == 0x00);
        SetFlag(sf.N, (temp & 0x80) != 0);
        if (lookup[opcode].addrmode == IMP)
            a = (byte)(temp & 0x00FF);
        else
            write(addr_abs, (byte)(temp & 0x00FF));
        return 0;
    }
    private byte BIT()
    {
        fetch();
        temp = (ushort)(a & fetched);
        SetFlag(sf.Z, (temp & 0x00FF) == 0x00);
        SetFlag(sf.N, (fetched & (1 << 7)) != 0);
        SetFlag(sf.V, (fetched & (1 << 6)) != 0);
        return 0;
    }
    // Instruction: Break
    private byte BRK()
    {
        pc++;

        SetFlag(sf.I, true);
		//write current address to stack pointer
        write((ushort)(0x0100 + sp), (byte)((pc >> 8) & 0x00FF));
        sp--;
        write((ushort)(0x0100 + sp), (byte)(pc & 0x00FF));
        sp--;

		//break is true
        SetFlag(sf.B, true);
		//apearantly write our status code to stack?
        write((ushort)(0x0100 + sp), status);
        sp--;
        SetFlag(sf.B, false);

        pc = (ushort)((ushort)read(0xFFFE) | ((ushort)read(0xFFFF) << 8));
        return 0;
    }
    private byte SBC()
    {
        fetch();

        // Operating in 16-bit domain to capture carry out

        // We can invert the bottom 8 bits with bitwise xor
        ushort value = (ushort)(((ushort)fetched) ^ 0x00FF);

        // Notice this is exactly the same as addition from here!
        temp = (ushort)((ushort)a + value + (ushort)GetFlag(sf.C));
        SetFlag(sf.C, (temp & 0xFF00) != 0);
        SetFlag(sf.Z, ((temp & 0x00FF) == 0));
        SetFlag(sf.V, ((temp ^ (ushort)a) & (temp ^ value) & 0x0080) != 0);
        SetFlag(sf.N, (temp & 0x0080) != 0);
        a = (byte)(temp & 0x00FF);
        return 1;
    }
    //illegal op codes
    private byte XXX()
	{
		return 0;
	}
    #endregion
    public void irq()
    {
        if (GetFlag(sf.I) == 0)
        {
            write((ushort)(0x0100 + sp), (byte)((pc >> 8) & 0x00FF));
            sp--;
            write((ushort)(0x0100 + sp), (byte)(pc & 0x00FF));
            sp--;

            SetFlag(sf.B, false);
            SetFlag(sf.U, true);
            SetFlag(sf.I, true);
            write((ushort)(0x0100 + sp), status);
            sp--;

            addr_abs = 0xFFFE;
            ushort lo = read((ushort)(addr_abs + 0));
            ushort hi = read((ushort)(addr_abs + 1));
            pc = (ushort)((hi << 8) | lo);

            cycles = 7;
        }
    }
    public void nmi()
    {
        write((ushort)(0x0100 + sp), (byte)((pc >> 8) & 0x00FF));
        sp--;
        write((ushort)(0x0100 + sp), (byte)(pc & 0x00FF));
        sp--;

        SetFlag(sf.B, false);
        SetFlag(sf.U, true);
        SetFlag(sf.I, true);
        write((ushort)(0x0100 + sp), status);
        sp--;

        addr_abs = 0xFFFE;
        ushort lo = read((ushort)(addr_abs + 0));
        ushort hi = read((ushort)(addr_abs + 1));
        pc = (ushort)((hi << 8) | lo);

        cycles = 7;
    }

    private byte RTI()
    {
        sp++;
        status = read((ushort)(0x0100 + sp));
		unchecked 
		{
			status &= (byte)~sf.B;
			status &= (byte)~sf.U;
		}

        sp++;
        pc = (ushort)read((ushort)(0x0100 + sp));
        sp++;
        //(void)(pc |= (ushort)read((ushort)(0x0100 + sp)) << 8);
        pc |= (ushort)(read((ushort)(0x0100 + sp)) << 8);

        return 0;
    }

}
public static class printExt
{
	public static void print(this object obj, string p = "", bool hex = true, int n = 2,bool wl = true,ConsoleColor c = ConsoleColor.White)
	{
		if (c != ConsoleColor.White)
			Console.ForegroundColor = c;
		if (hex)
		{
			Console.Write(p + "0x" + Int32.Parse(obj.ToString()).ToString("x" + n));
		}
		else
		{
			Console.Write(p + obj.ToString());
		}
		if (wl)
		{
			Console.Write("\n");
		}
		if (c != ConsoleColor.White)
			Console.ResetColor();
	}
    public static bool isBetween<T>(this T value, T min, T max, bool inclusive = true)
        where T : IComparable<T>
    {
        int comparisonMin = value.CompareTo(min);
        int comparisonMax = value.CompareTo(max);

        if (inclusive)
        {
            return comparisonMin >= 0 && comparisonMax <= 0;
        }
        else
        {
            return comparisonMin > 0 && comparisonMax < 0;
        }
    }
    public static void AddCodeAt(this byte[] data, ushort address, params byte[] opcodes)
    {
        if (data == null)
            throw new ArgumentNullException(nameof(data));

        if (address >= data.Length)
            throw new ArgumentOutOfRangeException(nameof(address));

        int end = address + opcodes.Length;
        if (end > data.Length)
            throw new ArgumentException("Opcode array doesn't fit at the specified address.", nameof(opcodes));

        for (int i = 0; i < opcodes.Length; i++)
        {
            data[address + i] = opcodes[i];
        }
    }
}
