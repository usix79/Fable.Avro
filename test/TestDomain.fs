module Foo.Bar

type TestState =
    | Red = 2
    | Yellow = 1
    | Green = 0

type SimpleRecord = {
    Id : int
    Name : string
    Version : int64
}

type ParentRecord = {
    Chield1 : SimpleRecord
    Chield2 : SimpleRecord
}

type HierarchyRecord = {
    Title: string
    Details: SimpleRecord
    IsProcessed: bool
}

type Price = Price of decimal

type ItemRecord = {
    Id : int
    Name : string
    Price : Price
}

type RecordWithId = {
    Id : int
}

type RecordWithNewId = {
    Id : int
    //[<DefaultValue("null")>]
    NewId: int option
}

type RecordWithNewField = {
    Id : int
    NewField: string
}

type RecordWithOption = {
    Id : int
    Id2 : int option
}

type GenericRecord<'T> = {
    Value : 'T
}

type BinaryTree =
    | Leaf of value:string
    | Node of left: BinaryTree * right: BinaryTree

type PriceRecord = {
    Id : int
    Caption: string
    Price : decimal
}


type OldRecord = {
    Id : int
    Title: string
}


type NewRecord = {
    Id : int
    Caption: string
    Description : string
}

type NewTestState =
    | Red = 3
    | Yellow = 2
    | Blue = 10

// Snippet link: http://fssnip.net/kW
// Authors: Tomas Petricek & Phil Trelford

type [<Measure>] GBP
type [<Measure>] Q

type UnitPrice = decimal<GBP/Q>
type Amount = decimal<GBP>
type Name = string
type Code = string
type Quantity = decimal<Q>

type Product = Code * Name * UnitPrice

type Tender =
  | Cash
  | Card of string
  | Voucher of Code

type LineItem =
  | SaleItem of (*Product*)(Code * Name * UnitPrice) * Quantity
  | TenderItem of Tender * Amount
  | CancelItem of int

type Basket = list<LineItem>

// Union evolution

type UnionV1 =
    | UnknownCase
    | Case1

type RecordV1 = {
    Union : UnionV1
}

type UnionV2 =
    | UnknownCase
    | Case1
    | Case2
    | Case3

type RecordV2 = {
    Union : UnionV2
}