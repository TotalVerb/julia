# This file is a part of Julia. License is MIT: http://julialang.org/license

immutable NullException <: Exception
end

Nullable{T}(value::T, isnull::Bool=false) = Nullable{T}(value, isnull)
Nullable() = Nullable{Union{}}()

eltype{T}(::Type{Nullable{T}}) = T

convert{T}(::Type{Nullable{T}}, x::Nullable{T}) = x
convert(   ::Type{Nullable   }, x::Nullable   ) = x

convert{T}(t::Type{Nullable{T}}, x::Any) = convert(t, convert(T, x))

function convert{T}(::Type{Nullable{T}}, x::Nullable)
    return isnull(x) ? Nullable{T}() : Nullable{T}(convert(T, get(x)))
end

convert{T}(::Type{Nullable{T}}, x::T) = Nullable{T}(x)
convert{T}(::Type{Nullable   }, x::T) = Nullable{T}(x)

convert{T}(::Type{Nullable{T}}, ::Void) = Nullable{T}()
convert(   ::Type{Nullable   }, ::Void) = Nullable{Union{}}()

promote_rule{S,T}(::Type{Nullable{S}}, ::Type{T}) = Nullable{promote_type(S, T)}
promote_rule{S,T}(::Type{Nullable{S}}, ::Type{Nullable{T}}) = Nullable{promote_type(S, T)}
promote_op{S,T}(op::Any, ::Type{Nullable{S}}, ::Type{Nullable{T}}) = Nullable{promote_op(op, S, T)}

function show{T}(io::IO, x::Nullable{T})
    if get(io, :compact, false)
        if isnull(x)
            print(io, "#NULL")
        else
            show(io, x.value)
        end
    else
        print(io, "Nullable{")
        showcompact(io, eltype(x))
        print(io, "}(")
        if !isnull(x)
            showcompact(io, x.value)
        end
        print(io, ')')
    end
end

get(x::Nullable) = x.isnull ? throw(NullException()) : x.value

@inline function get{T}(x::Nullable{T}, y)
    if isbits(T)
        ifelse(x.isnull, convert(T, y), x.value)
    else
        x.isnull ? convert(T, y) : x.value
    end
end

isnull(x::Nullable) = x.isnull

function isequal(x::Nullable, y::Nullable)
    if x.isnull && y.isnull
        return true
    elseif x.isnull || y.isnull
        return false
    else
        return isequal(x.value, y.value)
    end
end

==(x::Nullable, y::Nullable) = throw(NullException())

const nullablehash_seed = UInt === UInt64 ? 0x932e0143e51d0171 : 0xe51d0171

function hash(x::Nullable, h::UInt)
    if x.isnull
        return h + nullablehash_seed
    else
        return hash(x.value, h + nullablehash_seed)
    end
end

# as collection
length(x::Nullable) = x.isnull ? 0 : 1
endof(x::Nullable)  = length(x)

# indexing is either without index, or with 1 as index
# generalized linear indexing is not supported
linearindexing{T}(::Nullable{T}) = LinearFast()
function getindex(x::Nullable)
    @boundscheck x.isnull && throw(NullException())
    x.value
end
function getindex(x::Nullable, i::Integer)
    @boundscheck (x.isnull | (i ≠ 1)) && throw(BoundsError(i, x))
    x.value
end

# convenience method for getindex without bounds checking
unsafe_getindex(x::Nullable) = x.value
unsafe_getindex(x::Number) = x

# convenience method for detecting null value
isnullvalue(x::Nullable) = x.isnull
isnullvalue(x::Number) = false

# iteration protocol
start(x::Nullable) = 1
next(x::Nullable, i::Integer) = x.value, 0
done(x::Nullable, i::Integer) = x.isnull | (i == 0)

# higher-order functions
function filter{T}(p, x::Nullable{T})
    if x.isnull
        x
    elseif p(x.value)
        x
    else
        Nullable{T}()
    end
end

function map{T}(f, x::Nullable{T})
    if x.isnull
        Nullable{Union{}}()  # Union{} for consistency with arrays
    else
        Nullable(f(x.value))
    end
end

function map(f, xs::Nullable...)
    if all(isnull, xs)
        Nullable()
    elseif !any(isnull, xs)
        Nullable(map(f, map(unsafe_getindex, xs)...))
    else
        throw(DimensionMismatch("expected all null or all nonnull"))
    end
end
