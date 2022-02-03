package main

import (
	"fmt"
	"reflect"
	"testing"
)

func Test_convertMode(t *testing.T) {
	type args struct {
		argument  string
		symbolMap map[string]int64
	}
	tests := []struct {
		name    string
		args    args
		want    int
		want1   Operand
		wantErr bool
	}{
		{"immediate mode easy case", args{"#$10", nil}, Immediate, Operand{16, "", false}, false},
		{"immediate mode base 10 case", args{"#10", nil}, Immediate, Operand{10, "", false}, false},
		{"immediate convert fails", args{"#$1Z", nil}, Immediate, Operand{}, true},
		{"symbol map lookup okay", args{"#bob", map[string]int64{"bob": 80}}, Immediate, Operand{80, "bob", true}, false},
		{"symbol map lookup fails", args{"#bob", map[string]int64{"fred": 808}}, Immediate, Operand{0, "bob", false}, false},
		{"accumulator test", args{"A", nil}, Accumulator, Operand{}, false},
		{"single argument test easy case", args{"$F0", nil}, SingleArgument, Operand{0xF0, "", false}, false},
		{"single argument test with symbol fail", args{"bob", map[string]int64{"fred": 808}}, SingleArgument, Operand{0, "bob", false}, false},
		{"single argument test with symbol success", args{"bob", map[string]int64{"bob": 808}}, SingleArgument, Operand{808, "bob", true}, false},
		{"single argument X test easy case", args{"$F0,X", nil}, SingleArgumentX, Operand{0xF0, "", false}, false},
		{"single argument X test with symbol fail", args{"bob,X", map[string]int64{"fred": 808}}, SingleArgumentX, Operand{0, "bob", false}, false},
		{"single argument X test with symbol success", args{"bob,X", map[string]int64{"bob": 808}}, SingleArgumentX, Operand{808, "bob", true}, false},
		{"single argument Y test easy case", args{"$F0,Y", nil}, SingleArgumentY, Operand{0xF0, "", false}, false},
		{"single argument Y test with symbol fail", args{"bob,Y", map[string]int64{"fred": 808}}, SingleArgumentY, Operand{0, "bob", false}, false},
		{"single argument Y test with symbol success", args{"bob,Y", map[string]int64{"bob": 808}}, SingleArgumentY, Operand{808, "bob", true}, false},
		{"indirect X test easy case", args{"($F0,X)", nil}, IndirectX, Operand{0xF0, "", false}, false},
		{"indirect X test with symbol fail", args{"(bob,X)", map[string]int64{"fred": 808}}, IndirectX, Operand{0, "bob", false}, false},
		{"indirect X test with symbol success", args{"(bob,X)", map[string]int64{"bob": 808}}, IndirectX, Operand{808, "bob", true}, false},
		{"indirect Y test easy case", args{"($F0),Y", nil}, IndirectY, Operand{0xF0, "", false}, false},
		{"indirect Y test with symbol fail", args{"(bob),Y", map[string]int64{"fred": 808}}, IndirectY, Operand{0, "bob", false}, false},
		{"indirect Y test with symbol success", args{"(bob),Y", map[string]int64{"bob": 808}}, IndirectY, Operand{808, "bob", true}, false},
		{"indirect test easy case", args{"($F0)", nil}, Indirect, Operand{0xF0, "", false}, false},
		{"indirect test with symbol fail", args{"(bob)", map[string]int64{"fred": 808}}, Indirect, Operand{symbol: "bob"}, false},
		{"indirect test with symbol success", args{"(bob)", map[string]int64{"bob": 808}}, Indirect, Operand{808, "bob", true}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, got1, err := convertMode(tt.args.argument, tt.args.symbolMap)
			if (err != nil) != tt.wantErr {
				t.Errorf("convertMode() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got != tt.want {
				t.Errorf("convertMode() got = %v, want %v", got, tt.want)
			}
			if got1 != tt.want1 {
				t.Errorf("convertMode() got1 = %v, want %v", got1, tt.want1)
			}
		})
	}
}

func Test_convertStringToAssemblerLine(t *testing.T) {
	type args struct {
		line string
	}
	tests := []struct {
		name    string
		args    args
		want    AssemblerLine
		wantErr bool
	}{
		{"simple comment test", args{";abc"}, AssemblerLine{}, true},
		{"simple empty line test", args{"           "}, AssemblerLine{}, true},
		{"simple TAX test", args{"    TAX ; blobbb"}, AssemblerLine{operator: "TAX"}, false},
		{"simple LDX error", args{"    LDX ; blobbb"}, AssemblerLine{}, true},
		{"simple LDX correct", args{"    LDX  #AB  ; blobbb"}, AssemblerLine{operator: "LDX", operand: "#AB"}, false},
		{"simple TAX error", args{"    TAX #23 ; blobbb"}, AssemblerLine{}, true},
		{"simple labelled LDX correct", args{" labelxxx   LDX  #AB  ; blobbb"}, AssemblerLine{0, "labelxxx", "LDX", "#AB"}, false},
		{"simple labelled TAX correct", args{" labelxxx   TAX  ; blobbb"}, AssemblerLine{0, "labelxxx", "TAX", ""}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := convertStringToAssemblerLine(tt.args.line)
			if (err != nil) != tt.wantErr {
				t.Errorf("convertStringToAssemblerLine() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("convertStringToAssemblerLine() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_getInstructionBytes(t *testing.T) {
	type args struct {
		assembler AssemblerLine
		symbolMap SymbolMap
		pc        uint16
	}
	tests := []struct {
		name    string
		args    args
		want    []uint8
		want1   Operand
		wantErr bool
	}{
		{"Simple tax test", args{AssemblerLine{0, "", "TAX", ""}, SymbolMap{}, 0}, []uint8{0xAA}, Operand{}, false},
		{"Simple asl a test", args{AssemblerLine{0, "", "ASL", "A"}, SymbolMap{}, 0}, []uint8{0x0A}, Operand{}, false},
		{"Simple cpx immediate test", args{AssemblerLine{0, "", "CPX", "#$AB"}, SymbolMap{}, 0}, []uint8{0xE0, 0xAB}, Operand{value: 0xAB}, false},
		{"Symbol map cpx immediate test", args{AssemblerLine{0, "", "CPX", "#AB"}, SymbolMap{"AB": 0xFF}, 0}, []uint8{0xE0, 0xFF}, Operand{0xFF, "AB", true}, false},
		{"ldx zero page test", args{AssemblerLine{0, "", "LDX", "$AB"}, SymbolMap{}, 0}, []uint8{0xA6, 0xAB}, Operand{value: 0xAB}, false},
		{"ldx absolute test", args{AssemblerLine{0, "", "LDX", "$ABCD"}, SymbolMap{}, 0}, []uint8{0xAE, 0xCD, 0xAB}, Operand{value: 0xABCD}, false},
		{"ldx zero page y test", args{AssemblerLine{0, "", "LDX", "$AB,Y"}, SymbolMap{}, 0}, []uint8{0xB6, 0xAB}, Operand{value: 0xAB}, false},
		{"ldx absolute y test", args{AssemblerLine{0, "", "LDX", "$ABCD,Y"}, SymbolMap{}, 0}, []uint8{0xBE, 0xCD, 0xAB}, Operand{value: 0xABCD}, false},
		{"ldy zero page x test", args{AssemblerLine{0, "", "LDY", "$AB,X"}, SymbolMap{}, 0}, []uint8{0xB4, 0xAB}, Operand{value: 0xAB}, false},
		{"ldy absolute x test", args{AssemblerLine{0, "", "LDY", "$ABCD,X"}, SymbolMap{}, 0}, []uint8{0xBC, 0xCD, 0xAB}, Operand{value: 0xABCD}, false},
		{"jump absolute test", args{AssemblerLine{0, "", "JMP", "$ABCD"}, SymbolMap{}, 0}, []uint8{0x4C, 0xCD, 0xAB}, Operand{value: 0xABCD}, false},
		{"jump indirect test", args{AssemblerLine{0, "", "JMP", "($ABCD)"}, SymbolMap{}, 0}, []uint8{0x6C, 0xCD, 0xAB}, Operand{value: 0xABCD}, false},
		{"bne test", args{AssemblerLine{0, "", "BNE", "$FE"}, SymbolMap{}, 0}, []uint8{0xD0, 0xFE}, Operand{value: 0xFE}, false},
		{"bne test with symbol", args{AssemblerLine{0, "", "BNE", "bob"}, SymbolMap{"bob": 10}, 10}, []uint8{0xD0, 0xFE}, Operand{value: 10, symbol: "bob", symbolFound: true}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, got1, err := getInstructionBytes(tt.args.assembler, tt.args.symbolMap, tt.args.pc)
			if (err != nil) != tt.wantErr {
				t.Errorf("getInstructionBytes() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("getInstructionBytes() got = %v, want %v", got, tt.want)
			}
			if !reflect.DeepEqual(got1, tt.want1) {
				t.Errorf("getInstructionBytes() got1 = %v, want1 %v", got1, tt.want1)
			}
		})
	}
}

func Test_assemble(t *testing.T) {
	type args struct {
		sourceCode SourceCode
		offset     uint16
	}
	tests := []struct {
		name string
		args args
		want AssembledCode
	}{
		{name: "Simple no symbol test", args: args{SourceCode{[]string{"abc LDX #$23", "def LDX $ABCD"}}, 0},
			want: AssembledCode{
				symbolMap:  SymbolMap{"abc": 0, "def": 2},
				binaryDump: HexDump{bytes: []uint8{0xA2, 0x23, 0xAE, 0xCD, 0xAB}},
				assemblerLines: []AssemblerLine{
					{0, "abc", "LDX", "#$23"},
					{0, "def", "LDX", "$ABCD"}}}},
		{name: "Simple symbol test zero page", args: args{SourceCode{[]string{"abc LDX #$23", "def LDX abc"}}, 0},
			want: AssembledCode{
				symbolMap:  SymbolMap{"abc": 0, "def": 2},
				binaryDump: HexDump{bytes: []uint8{0xA2, 0x23, 0xA6, 0x0}},
				assemblerLines: []AssemblerLine{
					{0, "abc", "LDX", "#$23"},
					{0, "def", "LDX", "abc"}}}},
		{name: "Simple symbol test not zero page", args: args{SourceCode{[]string{"abc LDX #$23", "def LDX abc"}}, 1024},
			want: AssembledCode{
				symbolMap:  SymbolMap{"abc": 1024, "def": 1026},
				binaryDump: HexDump{bytes: []uint8{0xA2, 0x23, 0xAE, 0x00, 0x04}},
				assemblerLines: []AssemblerLine{
					{0, "abc", "LDX", "#$23"},
					{0, "def", "LDX", "abc"}}}},
		{name: "Simple symbol test branch", args: args{SourceCode{[]string{"abc LDX #$23", "def BNE abc"}}, 1024},
			want: AssembledCode{
				symbolMap:  SymbolMap{"abc": 1024, "def": 1026},
				binaryDump: HexDump{bytes: []uint8{0xA2, 0x23, 0xD0, 0xFC}},
				assemblerLines: []AssemblerLine{
					{0, "abc", "LDX", "#$23"},
					{0, "def", "BNE", "abc"}}}},
		{name: "Look ahead symbol test branch", args: args{SourceCode{[]string{"LDX #$23", "def BNE abc", "NOP", "abc NOP"}}, 1024},
			want: AssembledCode{
				symbolMap:  SymbolMap{"abc": 1029, "def": 1026},
				binaryDump: HexDump{bytes: []uint8{0xA2, 0x23, 0xD0, 0x01, 0xEA, 0xEA}},
				assemblerLines: []AssemblerLine{
					{0, "", "LDX", "#$23"},
					{0, "def", "BNE", "abc"},
					{0, "", "NOP", ""},
					{0, "abc", "NOP", ""}}}},
		{name: "Look ahead symbol test branch", args:
			args{SourceCode{[]string{"JSR init", "JSR loop", "JSR end", "init LDX #$00", "RTS", "loop INX",
				"CPX #$05", "BNE loop", "RTS", "end BRK",}}, 0x600},
			want: AssembledCode{
				symbolMap:  SymbolMap{"end":1554, "init":1545, "loop":1548},
				binaryDump: HexDump{bytes: []uint8{0x20, 0x9, 0x6, 0x20, 0xc, 0x6, 0x20, 0x12, 0x6, 0xa2, 0x0, 0x60, 0xe8, 0xe0, 0x5, 0xd0, 0xfb, 0x60, 0x0 }},
				assemblerLines: []AssemblerLine{
					{0, "", "JSR", "init"},
					{0, "", "JSR", "loop"},
					{0, "", "JSR", "end"},
					{0, "init", "LDX", "#$00"},
					{0,"","RTS",""},
					{0, "loop", "INX", ""},
					{0, "", "CPX", "#$05"},
					{0, "", "BNE", "loop"},
					{0,"","RTS",""},
					{0, "end", "BRK", ""},
				}}},

	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := assemble(tt.args.sourceCode, tt.args.offset)
			if !reflect.DeepEqual(got, tt.want) {
				for _,byteValue := range got.binaryDump.bytes{
					print(fmt.Sprintf("%x ",byteValue))
				}
				println()
				t.Errorf("assemble() = \ngot  %+v \nwant %+v", got, tt.want)
			}
		})
	}
}

func Test_disassemble(t *testing.T) {
	type args struct {
		hexdump         []uint8
		startingAddress uint16
	}
	tests := []struct {
		name string
		args args
		want []string
	}{
		{"simple ldx test", args{[]uint8{0xA2, 0x44}, 1024}, []string{"LDX #$44"}},
		{"simple ldx test", args{[]uint8{0xAE, 0x00, 0x44}, 1024}, []string{"LDX $4400"}},
		{"lsr a test", args{[]uint8{0x4A}, 1024}, []string{"LSR A"}},
		{"longer test", args{[]uint8{0xa9, 0x01, 0x8d, 0x00, 0x02, 0xa9, 0x05, 0x8d, 0x01, 0x02, 0xa9, 0x08, 0x8d, 0x02, 0x02}, 1024},
			[]string{"LDA #$01", "STA $0200", "LDA #$05", "STA $0201", "LDA #$08", "STA $0202"}},
		{"longer test 2", args{[]uint8{0xa0, 0x01, 0xa9, 0x03, 0x85, 0x01, 0xa9, 0x07, 0x85, 0x02, 0xa2, 0x0a, 0x8e, 0x04, 0x07, 0xb1, 0x01}, 1024},
			[]string{"LDY #$01", "LDA #$03", "STA $01", "LDA #$07", "STA $02", "LDX #$0a", "STX $0704", "LDA ($01),Y"}},
		{"sta test", args{[]uint8{0x85, 0x01, 0x85, 0x02}, 1024},
			[]string{"STA $01", "STA $02"}},
		{"jmp test", args{[]uint8{0x4C, 0x00, 0x06}, 0x600},
			[]string{"label1 JMP label1"}},
		{"jmp test 2", args{[]uint8{0x4C, 0x03, 0x06, 0x00}, 0x600},
			[]string{"JMP label1", "label1 BRK"}},
		{"bne forward test", args{[]uint8{0xd0, 0x00, 0x00}, 0x600},
			[]string{"BNE label1", "label1 BRK"}},
		{"bne backwards test", args{[]uint8{0x00, 0xd0, 0xFD, 0x00}, 0x600},
			[]string{"label1 BRK", "BNE label1", "BRK"}},
		{"jsr test 1 jsr to 1 label", args{[]uint8{0x20, 0x03, 0x06, 0x00}, 0x600},
			[]string{"JSR label1", "label1 BRK"}},
		{"jsr test 2 jsrs to same label", args{[]uint8{0x20, 0x06, 0x06, 0x20, 0x06, 0x06, 0x00}, 0x600},
			[]string{"JSR label1", "JSR label1", "label1 BRK"}},
		{"jsr test 2 labels", args{[]uint8{0x20, 0x06, 0x06, 0x20, 0x07, 0x06, 0x00, 0x00}, 0x600},
			[]string{"JSR label1", "JSR label2", "label1 BRK", "label2 BRK"}},
		{"jsr test label outside of program", args{[]uint8{0x20, 0x06, 0x06, 0x20, 0x10, 0x06, 0x00, 0x00}, 0x600},
			[]string{"JSR label1", "JSR $0610", "label1 BRK", "BRK"}},
		{"jsr test label outside of program other direction", args{[]uint8{0x20, 0x06, 0x06, 0x20, 0xff, 0x05, 0x00, 0x00}, 0x600},
			[]string{"JSR label1", "JSR $05ff", "label1 BRK", "BRK"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := disassemble(tt.args.hexdump, tt.args.startingAddress); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("disassemble() = %#v, want %#v", got, tt.want)
			}
		})
	}
}
