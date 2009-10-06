#!/usr/bin/ruby

#
# This script is an extremely pessimistic filter designed to yield
# in binary format a sorted set of all unique, likely to be viable,
# whole genome programs in the substrate.
#
# It is designed to be used in tandem with further compression by a
# program like bzip2 to generate an estimate of the number of unique
# nonredundant bits of viable genome information in the substrate.
#

# Discard genomes for which there are less than this many copies
# These are likely non-viable offspring.
$COUNT_THRESHOLD = 3

# Discard genomes smaller than this, since genomes smaller than this
# simply could not have a functional copy loop.
$SIZE_THRESHOLD = 5

# This hash will contain genomes and how many times they were seen.
specimens = Hash.new

$stdin.each { |csv|
  csv.strip!
  # CSV file format: ID,parentID,lineage,generation,genome
  specimen = csv.split(',')
  if specimen.size == 5
    # Column four is the actual genome program in hex
    genome = specimen[4]
    genome.strip!

    # Strip the first character since that's the "logo" and is not an
    # instruction.
    genome.gsub!(/^./,'')

    # Strip the argument of the XCHG instruction since that is not
    # technically an instruction.
    genome.gsub!(/c./,'c')

    # Strip anything after the first STOP instruction to include only
    # instructions that are *definitely* functional.
    genome.gsub!(/f.*/,'')
    
    # Count how many instances of the given genome pattern there are
    # using this hash.
    genome.freeze
    specimens[genome] = specimens[genome].to_i + 1
  end
}

# Delete genome strings for which there are less than the
# count threshold copies or which are too small.
specimens.delete_if { |genome,cnt| ((cnt < $COUNT_THRESHOLD) or (genome.length < $SIZE_THRESHOLD)) }

# Get a sorted array of all genome strings
genomes = specimens.keys
genomes.sort!

# Remove genome strings that are substrings of other ones (probably
# abortive reproduction attempts). This leaves only unique genomes
# that appear to be "whole specimens."
wholeGenomes = Array.new
genomes.each { |g|
  whole = true
  genomes.each { |g2|
    # It's not a "whole genome" if it's a substring of another and
    # is shorter than that one.
    if ((not g2.index(g).nil?) and (g.length < g2.length))
      whole = false
      break
    end
  }
  wholeGenomes << g if whole
}
genomes = wholeGenomes

# Sanity check
exit unless genomes.size > 0

# Concatenate it all together into one big hexadecimal string
allGenomes = String.new
genomes.each { |genome|
  allGenomes << genome
}

# Output each pair of four-bit words as a single eight-bit byte
tmp = String.new
allGenomes.split(//).each { |b|
  tmp << b
  if tmp.length == 2
    $stdout.putc(tmp.to_i(16))
    tmp.chop!
    tmp.chop!
  end
}

# Output the last four bits if there is an odd number of four-bit words.
if tmp.length == 1
  $stdout.putc(tmp.to_i(16))
end
