# This file is a part of Julia. License is MIT: http://julialang.org/license

# "is a null with type T", curried on 2nd argument
isnull_typed(x::Nullable, T::Type) = typeof(x).parameters[1] == T && isnull(x)
isnull_typed(t::Type) = x -> isnull_typed(x, t)

# "is a nullable with value egal to x", curried on 2nd argument
isnullableof(y::Nullable, x) = !isnull(y) && y.value === x
isnullableof(x) = y -> isnullableof(y, x)

types = [
    Bool,
    Float16,
    Float32,
    Float64,
    Int128,
    Int16,
    Int32,
    Int64,
    Int8,
    UInt16,
    UInt32,
    UInt64,
    UInt8,
]

# Nullable{T}() = new(true)
for T in types
    x = Nullable{T}()
    @test x.isnull === true
    @test isa(x.value, T)
    @test eltype(Nullable{T}) === T
    @test eltype(x) === T
end

# Nullable{T}(value::T) = new(false, value)
for T in types
    x = Nullable{T}(zero(T))
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === zero(T)
    @test eltype(x) === T

    x = Nullable{T}(one(T))
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === one(T)
    @test eltype(x) === T
end

# Nullable{T}(value::T, isnull::Bool) = new(isnull, value)
for T in types
    x = Nullable{T}(zero(T),false)
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === zero(T)
    @test eltype(x) === T

    x = Nullable{T}(zero(T),true)
    @test x.isnull === true
    @test isa(x.value, T)
    @test eltype(Nullable{T}) === T
    @test eltype(x) === T
end


# immutable NullException <: Exception
@test isa(NullException(), NullException)
@test_throws NullException throw(NullException())

# Nullable{T}(value::T) = Nullable{T}(value)
for T in types
    v = zero(T)
    x = Nullable(v)
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === v

    v = one(T)
    x = Nullable(v)
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === v
end

# show{T}(io::IO, x::Nullable{T})
io1 = IOBuffer()
io2 = IOBuffer()
for (i, T) in enumerate(types)
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))
    show(io1, x1)
    @test takebuf_string(io1) == @sprintf("Nullable{%s}()", T)
    show(io1, x2)
    showcompact(io2, get(x2))
    @test takebuf_string(io1) == @sprintf("Nullable{%s}(%s)", T, takebuf_string(io2))
    show(io1, x3)
    showcompact(io2, get(x3))
    @test takebuf_string(io1) == @sprintf("Nullable{%s}(%s)", T, takebuf_string(io2))

    a1 = [x2]
    show(IOContext(io1, compact=false), a1)
    show(IOContext(io2, compact=false), x2)
    @test takebuf_string(io1) ==
        @sprintf("Nullable{%s}[%s]", string(T), takebuf_string(io2))

    show(io1, a1)
    show(IOContext(io2, compact=true), x2)
    @test takebuf_string(io1) ==
        @sprintf("Nullable{%s}[%s]", string(T), takebuf_string(io2))
end

module NullableTestEnum
    io = IOBuffer()
    @enum TestEnum a b
    show(io, Nullable(a))
    Base.Test.@test takebuf_string(io) == "Nullable{NullableTestEnum.TestEnum}(a)"
end

# showcompact(io::IO, x::Nullable)
io1 = IOBuffer()
io2 = IOBuffer()
for (i, T) in enumerate(types)
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))
    showcompact(io1, x1)
    @test takebuf_string(io1) == "#NULL"
    showcompact(io1, x2)
    showcompact(io2, get(x2))
    @test takebuf_string(io1) == takebuf_string(io2)
    showcompact(io1, x3)
    showcompact(io2, get(x3))
    @test takebuf_string(io1) == takebuf_string(io2)

    a1 = [x2]
    showcompact(io1, a1)
    showcompact(io2, x2)
    @test takebuf_string(io1) ==
        @sprintf("Nullable{%s}[%s]", string(T), takebuf_string(io2))
end

# get(x::Nullable)
for T in types
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))

    @test_throws NullException get(x1)
    @test get(x2) === zero(T)
    @test get(x3) === one(T)
end

# get{S, T}(x::Nullable{S}, y::T)
for T in types
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))

    @test get(x1, zero(T)) === zero(T)
    @test get(x1, one(T)) === one(T)
    @test get(x2, one(T)) === zero(T)
    @test get(x3, zero(T)) === one(T)
end

# isnull(x::Nullable)
for T in types
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))

    @test isnull(x1) === true
    @test isnull(x2) === false
    @test isnull(x3) === false
end

# function isequal{S, T}(x::Nullable{S}, y::Nullable{T})
for T in types
    x1 = Nullable{T}()
    x2 = Nullable{T}()
    x3 = Nullable(zero(T))
    x4 = Nullable(one(T))

    @test isequal(x1, x1) === true
    @test isequal(x1, x2) === true
    @test isequal(x1, x3) === false
    @test isequal(x1, x4) === false

    @test isequal(x2, x1) === true
    @test isequal(x2, x2) === true
    @test isequal(x2, x3) === false
    @test isequal(x2, x4) === false

    @test isequal(x3, x1) === false
    @test isequal(x3, x2) === false
    @test isequal(x3, x3) === true
    @test isequal(x3, x4) === false

    @test isequal(x4, x1) === false
    @test isequal(x4, x2) === false
    @test isequal(x4, x3) === false
    @test isequal(x4, x4) === true
end

# function =={S, T}(x::Nullable{S}, y::Nullable{T})
for T in types
    x1 = Nullable{T}()
    x2 = Nullable{T}()
    x3 = Nullable(zero(T))
    x4 = Nullable(one(T))

    @test_throws NullException (x1 == x1)
    @test_throws NullException (x1 == x2)
    @test_throws NullException (x1 == x3)
    @test_throws NullException (x1 == x4)

    @test_throws NullException (x2 == x1)
    @test_throws NullException (x2 == x2)
    @test_throws NullException (x2 == x3)
    @test_throws NullException (x2 == x4)

    @test_throws NullException (x3 == x1)
    @test_throws NullException (x3 == x2)
    @test_throws NullException (x3 == x3)
    @test_throws NullException (x3 == x4)

    @test_throws NullException (x4 == x1)
    @test_throws NullException (x4 == x2)
    @test_throws NullException (x4 == x3)
    @test_throws NullException (x4 == x4)
end

# function hash(x::Nullable, h::UInt)
for T in types
    x1 = Nullable{T}()
    x2 = Nullable{T}()
    x3 = Nullable(zero(T))
    x4 = Nullable(one(T))

    @test isa(hash(x1), UInt)
    @test isa(hash(x2), UInt)
    @test isa(hash(x3), UInt)
    @test isa(hash(x4), UInt)

    @test hash(x1) == hash(x2)
    @test hash(x1) != hash(x3)
    @test hash(x1) != hash(x4)
    @test hash(x2) != hash(x3)
    @test hash(x2) != hash(x4)
    @test hash(x3) != hash(x4)
end

type TestNType{T}
    v::Nullable{T}
end

for T in types
    x1 = TestNType{T}(Nullable{T}())
    @test isnull(x1.v)
    x1.v = one(T)
    @test !isnull(x1.v)
    @test get(x1.v, one(T)) === one(T)
end

# issue #9462
for T in types
    @test isa(convert(Nullable{Number}, Nullable(one(T))), Nullable{Number})
    @test isa(convert(Nullable{Number}, one(T)), Nullable{Number})
    @test isa(convert(Nullable{T}, one(T)), Nullable{T})
    @test isa(convert(Nullable{Any}, Nullable(one(T))), Nullable{Any})
    @test isa(convert(Nullable{Any}, one(T)), Nullable{Any})

    # one(T) is convertible to every type in types
    # let's test that with Nullables
    for S in types
        @test isa(convert(Nullable{T}, one(S)), Nullable{T})
    end
end

@test isnull(convert(Nullable, nothing))
@test isnull(convert(Nullable{Int}, nothing))
@test isa(convert(Nullable{Int}, nothing), Nullable{Int})

@test convert(Nullable, 1) === Nullable(1)
@test convert(Nullable, Nullable(1)) === Nullable(1)
@test isequal(convert(Nullable, "a"), Nullable("a"))
@test isequal(convert(Nullable, Nullable("a")), Nullable("a"))

@test promote_type(Nullable{Int}, Int) === Nullable{Int}
@test promote_type(Nullable{Union{}}, Int) === Nullable{Int}
@test promote_type(Nullable{Float64}, Nullable{Int}) === Nullable{Float64}
@test promote_type(Nullable{Union{}}, Nullable{Int}) === Nullable{Int}
@test promote_type(Nullable{Date}, Nullable{DateTime}) === Nullable{DateTime}

@test Base.promote_op(+, Nullable{Int}, Nullable{Int}) == Nullable{Int}
@test Base.promote_op(-, Nullable{Int}, Nullable{Int}) == Nullable{Int}
@test Base.promote_op(+, Nullable{Float64}, Nullable{Int}) == Nullable{Float64}
@test Base.promote_op(-, Nullable{Float64}, Nullable{Int}) == Nullable{Float64}
@test Base.promote_op(-, Nullable{DateTime}, Nullable{DateTime}) == Nullable{Base.Dates.Millisecond}

# getindex
@test_throws NullException Nullable()[]
@test_throws NullException Nullable{Int}()[]
@test Nullable(0)[] === 0

@test_throws BoundsError Nullable()[-1]
@test_throws BoundsError Nullable()[0]
@test_throws BoundsError Nullable()[1]
@test_throws BoundsError Nullable()[2]
@test_throws BoundsError Nullable()[big(2)^64 + 1]
@test_throws BoundsError Nullable(0)[-1]
@test_throws BoundsError Nullable(0)[0]
@test_throws BoundsError Nullable(0)[2]
@test Nullable(0)[1] === 0

# collect
@test isempty(collect(Nullable())) && eltype(collect(Nullable())) == Union{}
@test isempty(collect(Nullable{Int}())) && eltype(collect(Nullable{Int}())) == Int
@test collect(Nullable(85)) == [85] && eltype(collect(Nullable(85))) == Int
@test collect(Nullable(1.0)) == [1.0] && eltype(collect(Nullable(1.0))) == Float64

# length & endof
@test length(Nullable()) === 0
@test length(Nullable{Int}()) === 0
@test length(Nullable(1.0)) === 1
@test endof(Nullable()) === 0
@test endof(Nullable(85)) === 1

# filter
for p in (_ -> true, _ -> false)
    @test filter(p, Nullable())      |> isnull_typed(Union{})
    @test filter(p, Nullable{Int}()) |> isnull_typed(Int)
end
@test filter(_ -> true, Nullable(85))  |> isnullableof(85)
@test filter(_ -> false, Nullable(85)) |> isnull_typed(Int)
@test filter(x -> x > 0, Nullable(85)) |> isnullableof(85)
@test filter(x -> x < 0, Nullable(85)) |> isnull_typed(Int)

# map
sqr(x) = x^2
@test map(sqr, Nullable())      |> isnull_typed(Union{})
@test map(sqr, Nullable{Int}()) |> isnull_typed(Union{})
@test map(sqr, Nullable(2))     |> isnullableof(4)

@test map(+, Nullable(1), Nullable(2), Nullable(3)) |> isnullableof(6)
@test map(+, Nullable(), Nullable(), Nullable())    |> isnull_typed(Union{})
@test map(+, Nullable{Int}(), Nullable{Int}())      |> isnull_typed(Union{})

# foldl/foldr
@test foldl((_, x) -> x^2, -1, Nullable())   == -1
@test foldl((_, x) -> x^2, -1, Nullable(10)) == 100
@test foldr((x, _) -> x^2, -1, Nullable())   == -1
@test foldr((x, _) -> x^2, -1, Nullable(10)) == 100

# broadcast and elementwise
@test sin.(Nullable(0.0))             |> isnullableof(0.0)
@test sin.(Nullable{Float64}())       |> isnull_typed(Float64)

@test Nullable(8) .+ Nullable(10)     |> isnullableof(18)
@test Nullable(8) .- Nullable(10)     |> isnullableof(-2)
@test Nullable(8) .+ Nullable{Int}()  |> isnull_typed(Int)
@test Nullable{Int}() .- Nullable(10) |> isnull_typed(Int)

@test log.(10, Nullable(1.0))                 |> isnullableof(0.0)
@test log.(10, Nullable{Float64}())           |> isnull_typed(Float64)
@test log.(Nullable(10), Nullable(1.0))       |> isnullableof(0.0)
@test log.(Nullable(10), Nullable{Float64}()) |> isnull_typed(Float64)

@test Nullable(2) .^ Nullable(4)      |> isnullableof(16)
@test Nullable(2) .^ Nullable{Int}()  |> isnull_typed(Int)

# multi-arg broadcast
@test Nullable(1) .+ Nullable(1) .+ Nullable(1) .+ Nullable(1) .+ Nullable(1) .+
    Nullable(1) |> isnullableof(6)
@test Nullable(1) .+ Nullable(1) .+ Nullable(1) .+ Nullable{Int}() .+
    Nullable(1) .+ Nullable(1) |> isnull_typed(Int)

us = map(Nullable, 1:20)
@test broadcast(max, us...)                  |> isnullableof(20)
@test broadcast(max, us..., Nullable{Int}()) |> isnull_typed(Int)

# test all elementwise operations
for (eop, op) in ((.+, +), (.-, -), (.*, *), (./, /), (.\, \), (.//, //),
                  (.==, ==), (.<, <), (.!=, !=), (.<=, <=), (.÷, ÷), (.%, %),
                  (.<<, <<), (.>>, >>), (.^, ^))
    # op(1, 1) chosen because it works for all operations
    res = op(1, 1)
    @test eop(Nullable(1), Nullable(1))         |> isnullableof(res)
    @test eop(Nullable{Int}(), Nullable(1))     |> isnull_typed(typeof(res))
    @test eop(Nullable(1), Nullable{Int}())     |> isnull_typed(typeof(res))
    @test eop(Nullable{Int}(), Nullable{Int}()) |> isnull_typed(typeof(res))
end

# imperative style
s = 0
for x in Nullable(10)
    s += x
end
@test s === 10

s = 0
for x in Nullable{Int}()
    s += x
end
@test s === 0

# issue #11675
@test repr(Nullable()) == "Nullable{Union{}}()"
