package main

import (
	"errors"
	"fmt"
	"regexp"
)
import "strings"
import "strconv"

type OperandTypeMap map[int]uint8

type SymbolMap map[string]int64

type OperandDefinition struct {
	operator          string
	typeToMachineCode OperandTypeMap
	numberOperands    int
}

type Command interface {
	do(machine CPU6502)
	undo(machine CPU6502)
}

type AssemblerLine struct {
	index    int
	label    string
	operator string
	operand  string
}

func buildModes(origin int) OperandTypeMap {
	var base = origin * 16
	var base1 = (origin + 1) * 16
	return OperandTypeMap{
		Immediate: uint8(base + 0x9),
		ZeroPage:  uint8(base + 0x5),
		ZeroPageX: uint8(base1 + 0x5),
		Absolute:  uint8(base + 0xd),
		AbsoluteX: uint8(base1 + 0xd),
		AbsoluteY: uint8(base1 + 0x9),
		IndirectX: uint8(base + 0x1),
		IndirectY: uint8(base1 + 0x1)}
}

func buildTransferOperandDefinition(name string, i uint8) OperandDefinition {
	return OperandDefinition{name, OperandTypeMap{Implied: i}, 0}
}

const (
	Immediate int = iota
	ZeroPage
	ZeroPageX
	ZeroPageY
	Absolute
	AbsoluteX
	AbsoluteY
	IndirectX
	IndirectY
	Relative
	Implied
	Accumulator
	Indirect
	SingleArgument
	SingleArgumentX
	SingleArgumentY
)

type CPU6502 struct {
	A      uint8
	X      uint8
	Y      uint8
	Status uint8
	SP     uint8
	PC     uint16
}

var CPXModes = OperandTypeMap{Immediate: 0xE0, ZeroPage: 0xE4, Absolute: 0xEC}
var CPYModes = OperandTypeMap{Immediate: 0xC0, ZeroPage: 0xC4, Absolute: 0xCC}
var DECModes = OperandTypeMap{ZeroPage: 0xC6, ZeroPageX: 0xD6, Absolute: 0xCE, AbsoluteX: 0xDE}
var INCModes = OperandTypeMap{ZeroPage: 0xE6, ZeroPageX: 0xF6, Absolute: 0xEE, AbsoluteX: 0xFE}
var JMPModes = OperandTypeMap{Absolute: 0x4C, Indirect: 0x6C}
var ASLModes = OperandTypeMap{Accumulator: 0x0A, ZeroPage: 0x06, ZeroPageX: 0x16, Absolute: 0x0E, AbsoluteX: 0x1E}
var BITModes = OperandTypeMap{ZeroPage: 0x24, Absolute: 0x2C}
var LDXModes = OperandTypeMap{Immediate: 0xA2, ZeroPage: 0xA6, ZeroPageY: 0xB6, Absolute: 0xAE, AbsoluteY: 0xBE}
var LDYModes = OperandTypeMap{Immediate: 0xA0, ZeroPage: 0xA4, ZeroPageX: 0xB4, Absolute: 0xAC, AbsoluteX: 0xBC}
var LSRModes = OperandTypeMap{Accumulator: 0x4A, ZeroPage: 0x46, ZeroPageX: 0x56, Absolute: 0x4E, AbsoluteX: 0x5E}
var ROLModes = OperandTypeMap{Accumulator: 0x2A, ZeroPage: 0x26, ZeroPageX: 0x36, Absolute: 0x2E, AbsoluteX: 0x3E}
var RORModes = OperandTypeMap{Accumulator: 0x6A, ZeroPage: 0x66, ZeroPageX: 0x76, Absolute: 0x6E, AbsoluteX: 0x7E}

var operandDefinitions = []OperandDefinition{
	{"BPL", OperandTypeMap{Relative: 0x10}, 1},
	{"BMI", OperandTypeMap{Relative: 0x30}, 1},
	{"BVC", OperandTypeMap{Relative: 0x50}, 1},
	{"BVS", OperandTypeMap{Relative: 0x70}, 1},
	{"BCC", OperandTypeMap{Relative: 0x90}, 1},
	{"BCS", OperandTypeMap{Relative: 0xB0}, 1},
	{"BNE", OperandTypeMap{Relative: 0xD0}, 1},
	{"BEQ", OperandTypeMap{Relative: 0xF0}, 1},

	{"CLC", OperandTypeMap{Implied: 0x18}, 0},
	{"SEC", OperandTypeMap{Implied: 0x38}, 0},
	{"CLI", OperandTypeMap{Implied: 0x58}, 0},
	{"SEI", OperandTypeMap{Implied: 0x78}, 0},
	{"CLV", OperandTypeMap{Implied: 0xB8}, 0},
	{"CLD", OperandTypeMap{Implied: 0xD8}, 0},
	{"SED", OperandTypeMap{Implied: 0xF8}, 0},

	{"ADC", buildModes(6), 1},
	{"AND", buildModes(2), 1},
	{"ASL", ASLModes, 1},
	{"BIT", BITModes, 1},
	{"BRK", OperandTypeMap{Implied: 0x00}, 0},
	{"CMP", buildModes(0xC), 1},
	{"CPX", CPXModes, 1},
	{"CPY", CPYModes, 1},
	{"DEC", DECModes, 1},
	//
	{"EOR", buildModes(4), 1},
	{"INC", INCModes, 1},
	{"JMP", JMPModes, 1},
	{"JSR", OperandTypeMap{Absolute: 0x20}, 1},
	{"LDA", buildModes(0xA), 1},
	{"LDX", LDXModes, 1},
	{"LDY", LDYModes, 1},
	{"LSR", LSRModes, 1},
	{"NOP", OperandTypeMap{Implied: 0xEA}, 0},
	{"ORA", buildModes(0), 1},
	{"PHA", OperandTypeMap{Implied: 0x48}, 0},
	{"PLA", OperandTypeMap{Implied: 0x68}, 0},
	{"PHP", OperandTypeMap{Implied: 0x08}, 0},
	{"PLP", OperandTypeMap{Implied: 0x28}, 0},
	{"ROL", ROLModes, 1},
	{"ROR", RORModes, 1},
	{"STA", buildModes(8), 1},

	{"RTI", OperandTypeMap{Implied: 0x40}, 0},
	{"RTS", OperandTypeMap{Implied: 0x60}, 0},
	{"SBC", buildModes(0xE), 0},
	{"STX", OperandTypeMap{ZeroPage: 0x86, ZeroPageY: 0x96, Absolute: 0x8E}, 0},
	{"STY", OperandTypeMap{ZeroPage: 0x84, ZeroPageX: 0x94, Absolute: 0x8C}, 0},
	buildTransferOperandDefinition("TAX", 0xAA),
	buildTransferOperandDefinition("TXA", 0x8A),
	buildTransferOperandDefinition("TSX", 0xBA),
	buildTransferOperandDefinition("TXS", 0x9A),
	buildTransferOperandDefinition("DEX", 0xCA),
	buildTransferOperandDefinition("INX", 0xE8),
	buildTransferOperandDefinition("TAY", 0xA8),
	buildTransferOperandDefinition("TYA", 0x98),
	buildTransferOperandDefinition("DEY", 0x88),
	buildTransferOperandDefinition("INY", 0xC8),
}
var operandMap = buildOperandMap(operandDefinitions)
var operatorMap = buildOperatorMap(operandDefinitions)

type OperandOutput struct {
	numBytes    int16
	format      string
	formatLabel string
}

//
var operandOutputs = map[int]OperandOutput{
	Immediate:   {numBytes: 1, format: "#$%02x"},
	ZeroPage:    {numBytes: 1, format: "$%02x"},
	ZeroPageX:   {numBytes: 1, format: "$%02x,X"},
	ZeroPageY:   {numBytes: 1, format: "$%02x,Y"},
	Absolute:    {numBytes: 2, format: "$%04x", formatLabel: "%s"},
	AbsoluteX:   {numBytes: 2, format: "$%04x,X", formatLabel: "%s,X"},
	AbsoluteY:   {numBytes: 2, format: "$%04x,Y", formatLabel: "%s,Y"},
	IndirectX:   {numBytes: 1, format: "($%02x,X)"},
	IndirectY:   {numBytes: 1, format: "($%02x),Y"},
	Relative:    {numBytes: 1, format: "$%02x,X"},
	Implied:     {},
	Accumulator: {},
	Indirect:    {numBytes: 2, format: "($%04x)", formatLabel: "($%s)"},
}

type DisassembleLine struct {
	operator string
	operand  string
	address  uint16
	label    string
}

func disassemble(hexdump []uint8, startingAddress uint16) []string {

	i := 0
	labelIndex := 1
	labelMap := map[uint16]string{}
	reverseMap := map[string]uint16{}
	var disassembleLines []DisassembleLine
	for i < len(hexdump) {
		operator := hexdump[i]
		operandDefinition, found := operatorMap[operator]
		jumpValue := uint16(0)
		isJump := false
		if found {
			for k, v := range operandDefinition.typeToMachineCode {
				if v == operator {
					operandOutput := operandOutputs[k]
					operandString := ""
					if operandOutput.numBytes == 1 {
						operandString = fmt.Sprintf(operandOutput.format, hexdump[i+1])
						if k == Relative {
							var sValue = int8(hexdump[i+1])
							jumpValue = startingAddress + uint16(i) + 2 + uint16(sValue)
							isJump = true
						}
					} else if operandOutput.numBytes == 2 {
						lsb := uint16(hexdump[i+1])
						msb := uint16(hexdump[i+2])
						value := msb*256 + lsb
						operandString = fmt.Sprintf(operandOutput.format, value)
						if k == Absolute || k == Indirect || k == AbsoluteX || k == AbsoluteY {
							jumpValue = value
							isJump = true
						}
					} else if k == Accumulator {
						operandString = "A"
					}
					if isJump && jumpValue >= startingAddress && jumpValue < startingAddress+uint16(len(hexdump)) {
						labelName, exists := labelMap[jumpValue]
						if !exists {
							labelName = fmt.Sprintf("label%d", labelIndex)
							labelIndex++
							labelMap[jumpValue] = labelName
							reverseMap[labelName] = jumpValue
						}
						operandString = labelName
					}

					disassembleLines = append(disassembleLines, DisassembleLine{operator: operandDefinition.operator, operand: operandString, address: startingAddress + uint16(i)})
					i = i + 1 + int(operandOutput.numBytes)
				}
			}
		} else {
			panic(fmt.Sprintf("Not found %0x", operator))
		}
	}
	for i := range disassembleLines {
		address := disassembleLines[i].address
		label, exists := labelMap[address]
		if exists {
			disassembleLines[i].label = label
		}
	}
	var result []string
	for i := range disassembleLines {
		line := disassembleLines[i].operator
		if disassembleLines[i].operand != "" {
			line = line + " " + disassembleLines[i].operand
		}
		if disassembleLines[i].label != "" {
			line = disassembleLines[i].label + " " + line
		}
		result = append(result, line)
	}
	return result
}

func buildOperandMap(operandDefinitions []OperandDefinition) map[string]OperandDefinition {
	operandMap := make(map[string]OperandDefinition)
	for i := range operandDefinitions {
		operandMap[operandDefinitions[i].operator] = operandDefinitions[i]
	}
	return operandMap
}
func buildOperatorMap(operandDefinitions []OperandDefinition) map[uint8]OperandDefinition {
	operatorMap := make(map[uint8]OperandDefinition)
	for i := range operandDefinitions {
		for _, v := range operandDefinitions[i].typeToMachineCode {
			operatorMap[v] = operandDefinitions[i]
		}
	}
	return operatorMap
}

type Operand struct {
	value       int64
	symbol      string
	symbolFound bool
}

type ParseArgument struct {
	regexPattern *regexp.Regexp
	mode         int
}

var parseArguments = []ParseArgument{
	{regexp.MustCompile(`^#(.*)$`), Immediate},
	{regexp.MustCompile(`^\((.*),X\)`), IndirectX},
	{regexp.MustCompile(`^\((.*)\),Y`), IndirectY},
	{regexp.MustCompile(`^(.*),X`), SingleArgumentX},
	{regexp.MustCompile(`^(.*),Y`), SingleArgumentY},
	{regexp.MustCompile(`^\((.*)\)$`), Indirect},
}

func convertMode(operandString string, symbolMap SymbolMap) (int, Operand, error) {
	if operandString == "A" {
		return Accumulator, Operand{}, nil
	}
	for _, arg := range parseArguments {
		match := arg.regexPattern.FindStringSubmatch(operandString)
		if len(match) > 0 {
			operand, err := extractValueOrSymbol(match[1], symbolMap)
			return arg.mode, operand, err
		}
	}
	operand, err := extractValueOrSymbol(operandString, symbolMap)
	return SingleArgument, operand, err
}

var symbolPattern = regexp.MustCompile(`^[A-Za-z][A-Za-z0-9]*$`)

func validSymbol(symbol string) bool {
	return symbolPattern.MatchString(symbol)
}

func extractValueOrSymbol(rest string, symbolMap SymbolMap) (Operand, error) {
	value, err := convertStringToInt(rest)
	if err != nil {
		if !validSymbol(rest) {
			return Operand{}, fmt.Errorf("invalid symbol found %v", rest)
		}
		val, exists := symbolMap[rest]
		return Operand{val, rest, exists}, nil
	}
	return Operand{value: value}, nil
}

func convertStringToInt(str string) (int64, error) {
	if strings.HasPrefix(str, "$") {
		return strconv.ParseInt(strings.TrimPrefix(str, "$"), 16, 64)
	}
	return strconv.ParseInt(str, 10, 64)

}
func convertStringToAssemblerLine(line string) (AssemblerLine, error) {
	line = strings.Trim(line, " ")
	index := strings.Index(line, ";")
	if len(line) == 0 || index == 0 {
		return AssemblerLine{}, errors.New("empty line")
	}
	if index != -1 {
		line = line[:index]
	}
	line = strings.Trim(line, " ")
	words := deleteEmpty(strings.Split(line, " "))

	if len(words) == 1 {
		operandDefinition, exists := operandMap[words[0]]
		if exists && operandDefinition.numberOperands == 0 {
			return AssemblerLine{operator: operandDefinition.operator}, nil
		}
	}
	if len(words) == 2 {
		operandDefinition, exists := operandMap[words[0]]
		if exists && operandDefinition.numberOperands == 1 {
			return AssemblerLine{0, "", operandDefinition.operator, words[1]}, nil
		}
		if !exists {
			operandDefinition, exists := operandMap[words[1]]
			if exists && operandDefinition.numberOperands == 0 {
				return AssemblerLine{0, words[0], operandDefinition.operator, ""}, nil
			}
		}

	}
	if len(words) == 3 {
		operandDefinition, exists := operandMap[words[1]]
		if exists && operandDefinition.numberOperands == 1 {
			return AssemblerLine{0, words[0], operandDefinition.operator, words[2]}, nil
		}
	}
	return AssemblerLine{}, fmt.Errorf("some problem or another %v", line)
}

func getSingleValue(mymap OperandTypeMap) uint8 {
	for _, v := range mymap {
		return v
	}
	return 255 // should never happen
}

type OperandTypeCheck struct {
	internal int
	zeroPage int
	absolute int
}

var operandTypeChecks = []OperandTypeCheck{
	{SingleArgument, ZeroPage, Absolute},
	{SingleArgumentX, ZeroPageX, AbsoluteX},
	{SingleArgumentY, ZeroPageY, AbsoluteY},
}

func getInstructionBytes(assembler AssemblerLine, symbolMap SymbolMap, pc uint16) ([]uint8, Operand, error) {
	operandDefinition, exists := operandMap[assembler.operator]
	if exists && operandDefinition.numberOperands == 0 && len(operandDefinition.typeToMachineCode) == 1 {
		return []uint8{getSingleValue(operandDefinition.typeToMachineCode)}, Operand{}, nil
	}
	if !exists {
		return []byte{}, Operand{}, fmt.Errorf("unrecognized operator %v", assembler.operator)
	}

	operandType, operand, err := convertMode(assembler.operand, symbolMap)
	if err != nil {
		return []uint8{}, Operand{}, fmt.Errorf("unable to find operator argument type for operand %v", assembler.operand)
	}
	if operandType == Accumulator {
		byteCode, found := operandDefinition.typeToMachineCode[Accumulator]
		if found {
			return []uint8{byteCode}, Operand{}, nil
		}
	}
	if operandType == SingleArgument {
		byteCode, found := operandDefinition.typeToMachineCode[Relative]
		if found {
			if operand.symbol == "" {
				return []uint8{byteCode, uint8(operand.value)}, operand, nil
			} else {
				if operand.symbolFound {
					var nextPc = pc + 2
					var jumpPc = uint16(operand.value)
					var offset = uint8((jumpPc - nextPc) & 0xff)
					return []uint8{byteCode, offset}, operand, nil
				} else {
					return []uint8{byteCode, 0}, operand, nil
				}
			}
		}
	}
	if operandType == Immediate || operandType == IndirectX || operandType == IndirectY {
		byteCode, found := operandDefinition.typeToMachineCode[operandType]
		if found {
			return []uint8{byteCode, uint8(operand.value)}, operand, nil
		}
	}
	for _, check := range operandTypeChecks {
		if operandType == check.internal {
			bytes, err := getSingleOperandBytes(operand.value, operandDefinition, check.zeroPage, check.absolute)
			if err == nil {
				return bytes, operand, nil
			}
		}
	}
	if operandType == Indirect {
		byteCode, found := operandDefinition.typeToMachineCode[operandType]
		if found {
			lsb, msb := getLsbAndMsb(operand.value)
			return []uint8{byteCode, lsb, msb}, operand, nil
		}
	}
	return []uint8{}, Operand{}, fmt.Errorf("unrecognized operator %d", operandType)
}

func getLsbAndMsb(value int64) (uint8, uint8) {
	lsb := uint8(value & 0xff)
	msb := uint8(value & 0xff00 >> 8)
	return lsb, msb
}

type SourceCode struct{ code []string }
type HexDump struct{ bytes []uint8 }

type AssembledCode struct {
	symbolMap      SymbolMap
	assemblerLines []AssemblerLine
	binaryDump     HexDump
}

func assemble(sourceCode SourceCode, offset uint16) AssembledCode {
	if len(sourceCode.code) == 0 {
		return AssembledCode{}
	}

	type AssembledLine struct {
		assemblerLine AssemblerLine
		bytes         []uint8
		operand       Operand
		pc            uint16
	}
	var assembledLines []AssembledLine
	symbolMap := SymbolMap{}
	for _, codeLine := range sourceCode.code {
		assemblerLine, err := convertStringToAssemblerLine(codeLine)
		if err == nil {
			if assemblerLine.label != "" {
				symbolMap[assemblerLine.label] = int64(offset)
			}
			bytes, operand, err := getInstructionBytes(assemblerLine, symbolMap, offset)
			if err == nil {
				assembledLines = append(assembledLines, AssembledLine{assemblerLine, bytes, operand, offset})
				offset += uint16(len(bytes))
			}
		}
	}
	for i := range assembledLines {
		if assembledLines[i].operand.symbol != "" && !assembledLines[i].operand.symbolFound {
			bytes, operand, err := getInstructionBytes(assembledLines[i].assemblerLine, symbolMap, assembledLines[i].pc)
			if err == nil {
				assembledLines[i].bytes = bytes
				assembledLines[i].operand = operand
			}
		}
	}
	var binaryDump []uint8
	var assemblerLines []AssemblerLine
	for i := range assembledLines {
		binaryDump = append(binaryDump, assembledLines[i].bytes...)
		assemblerLines = append(assemblerLines, assembledLines[i].assemblerLine)
	}
	return AssembledCode{assemblerLines: assemblerLines, binaryDump: HexDump{binaryDump}, symbolMap: symbolMap}
}

func getSingleOperandBytes(value int64, operandDefinition OperandDefinition, zeroPage int, absolute int) ([]uint8, error) {
	if value <= 255 {
		byteCode, found := operandDefinition.typeToMachineCode[zeroPage]
		if found {
			lsb, _ := getLsbAndMsb(value)
			return []uint8{byteCode, lsb}, nil
		}
	}
	byteCode, found := operandDefinition.typeToMachineCode[absolute]
	if found {
		lsb, msb := getLsbAndMsb(value)
		return []uint8{byteCode, lsb, msb}, nil
	}
	return nil, errors.New("not recognized type")
}

func deleteEmpty(s []string) []string {
	var r []string
	for _, str := range s {
		if str != "" {
			r = append(r, str)
		}
	}
	return r
}

func main() {
	fmt.Println("Hello, World!")
	fmt.Println(operandMap)
	ops := []string{"ADC", "AND", "ASL", "BCC", "BCS", "BEQ", "BIT", "BMI", "BNE", "BPL", "BRK", "BVC", "BVS", "CLC", "CLD", "CLI", "CLV", "CMP", "CPX", "CPY", "DEC", "DEX", "DEY", "EOR", "INC", "INX", "INY", "JMP",
		"JSR", "LDA", "LDX", "LDY", "LSR", "NOP", "ORA", "PHA", "PHP", "PLA", "PLP", "ROL", "ROR", "RTI", "RTS", "SBC", "SEC", "SED", "SEI", "STA", "STX", "STY", "TAX", "TAY", "TSX", "TXA", "TXS", "TYA"}
	for _, str := range ops {
		_, exists := operandMap[str]
		if !exists {
			println("Bad " + str)
		}
	}
	var x uint8 = 0
	x = x - 3
	fmt.Println(fmt.Sprintf("%x", x))
}
