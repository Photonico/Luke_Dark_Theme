import Base: getindex, setindex!, values
export getindex, setindex!, values

println("\nTuple:")
T = (1, 2.34, 5 + 6im, 7 + 8.9im, "Fool")
println(typeof((1, 2.34, 5 + 6im, 7 + 8.9im, "Fool")))      # Tuple{Int64,Float64,Complex{Int64},Complex{Float64},String}
println(typeof(T))                                          # Tuple{Int64,Float64,Complex{Int64},Complex{Float64},String}

println(Tuple{Int64} <: Tuple{Real})                        # true
println(Tuple{Int64} <: Tuple{Float64})                     # false
println(Tuple{Real}  <: Tuple{Int64})                       # false
println(Tuple{Real}  >: Tuple{Int64})                       # true
println(Tuple{Real}  <: Tuple{Real})                        # true

println(Tuple{Integer, Real} <: Tuple{Any})                 # false
println(Tuple{Integer, Real} <: Tuple{Any, Any})            # true
println(Tuple{Integer, Real} <: Tuple{Real, Real})          # true
println(Tuple{Integer, Any}  <: Tuple{Real, Real})          # false

struct JointProbabilityDistribution <: AbstractProbabilityDistribution
    variables::AbstractVector
    probabilities::Dict
    values::Dict{Any, AbstractVector}
    function JointProbabilityDistribution(variables::AbstractVector)
        return new(variables, Dict(), Dict{Any, AbstractVector}());
    end
end

function fitness(ent)
    # we want the expression a+2b+3c+4d+5e-42
    # to be as close to 0 as possible
    score = ent.abcde[1] +
            2 * ent.abcde[2] +
            3 * ent.abcde[3] +
            4 * ent.abcde[4] +
            5 * ent.abcde[5]
    abs(score - 42)
end

function setindex!(jpd::JointProbabilityDistribution, value, key_values)
    local key::Tuple = event_values(key_values, jpd.variables);
    jpd.probabilities[key] = value;
    for (k, v) in zip(jpd.variables, key)
        if (!haskey(jpd.values, k))
            jpd.values[k] = [v];
        elseif (!(v in jpd.values[k]))
            push!(jpd.values[k], v);
        end
    end
    nothing;
end
