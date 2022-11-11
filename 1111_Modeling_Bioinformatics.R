mito_fasta = read.fasta('NC_012920.fasta')
print(mito_fasta)
mito_seq = mito_fasta$NC_012920.1

#Exercise 4
nucleo_content = function(seq_vec) {
  total = length(seq_vec)
  #set the counter for each nucleotide
  a = 0
  t = 0
  g = 0
  c = 0
  for (i in 1:total) {
    if (tolower(seq_vec[i]) == 'a') {
      a = a+1
    }
    else if (tolower(seq_vec[i]) == 't') {
      t = t+1
    }
    else if (tolower(seq_vec[i]) == 'g') {
      g = g+1
    }
    else if (tolower(seq_vec[i]) == 'c') {
      c = c+1
    }
  }
  cat("G:", 100*g/total, "%\n")
  cat("C:", 100*c/total, "%\n")
  cat("A:", 100*a/total, "%\n")
  cat("T:", 100*t/total, "%")
}

#Exercise 5
create_complement = function(seq_vec) {
  seq_copy = seq_vec
  tolower(seq_copy)
  #use capital letters to avoid overlap
  seq_copy[seq_copy == 'a'] = 'T'
  seq_copy[seq_copy == 't'] = 'A'
  seq_copy[seq_copy == 'g'] = 'C'
  seq_copy[seq_copy == 'c'] = 'G'
  new_seq = tolower(seq_copy)[length(seq_copy):1]
  return(new_seq)
}

#Exercise 6
rm(list=ls())
mito_fasta = read.fasta('NC_012920.fasta')
mito_seq = mito_fasta$NC_012920.1
length(mito_seq) #16569
nucleo_content(mito_seq)
##G: 13.09071 %
##C: 31.26924 %
##A: 30.92522 %
##T: 24.70879 %
comp_mito_seq = create_complement(mito_seq)
nucleo_content(comp_mito_seq)
##G: 31.26924 %
##C: 13.09071 %
##A: 24.70879 %
##T: 30.92522 %

#Exercise 7 ?? the range should not contain the calculation
frameslide = function(seq_vec, nucleo, framesize) {
  framesize = as.numeric(framesize)
  reading_length = length(seq_vec)-framesize+1
  nucleo_prop = NULL
  for (i in 1:reading_length) {
    endsite = i+framesize-1
    frame_read = tolower(seq_vec[i:endsite])
    count_num = length(frame_read[frame_read==nucleo])
    count_prop = 100*count_num/framesize
    nucleo_prop[i] = count_prop
  }
  plot(1:length(nucleo_prop),nucleo_prop, type='l',
       las=1,
       xlab = 'sequence range',
       ylab = 'nucleo proportion (%)',
       col = 'green')
}
frameslide(mito_seq, 'a', 200)
frameslide(mito_seq, 't', 200)
frameslide(mito_seq, 'g', 200)
frameslide(mito_seq, 'c', 200)

#Exercise 8
find_codon = function(seq_vec, codon, reading_frame) {
  codon_position = NULL
  codon = tolower(codon)
  i = reading_frame
  while((i+2) <= length(seq_vec)) {
    seq_str = paste(seq_vec[i:(i+2)], collapse = "")
    if (seq_str == substr(codon,1,3)) {
      codon_position = c(codon_position, i)
      }
    i = i + 3
    } 
  return(codon_position)
}
find_codon(mito_seq,'atg',1)
##[1]    73   415   772  1090  1399  1567  1798  2629
##[9]  2749  3313  3355  3367  4444  5665  5869  6085
##[17]  7039  7237  7390  7495  7504  7894  8071  8170
##[25]  8389  8527  8695  8836  9037  9223  9385  9943
##[33]  9964 10369 10429 10450 10723 10804 11164 11344
##[41] 11401 11557 11833 12004 12343 12499 13357 14833
##[49] 14836 15232 15559 15880 15931 16087 16567

#Exercise 9
find_start_codon = function(seq_vec, reading_frame) {
  codon_vec = c('ATG','ATA','ATT','ATC','GTG')
  start_pos = NULL
  for (n in 1:length(codon_vec)) {
    codon = codon_vec[n]
    codon_position = find_codon(seq_vec, codon, reading_frame)
    start_pos = c(start_pos, codon_position)
  }
  start_pos = sort(start_pos)
  return(start_pos)
}
find_start_codon(mito_seq[1:500],1)
##[1]  13  49  73  82 148 160 178 205 211 232 235 238
##[13] 244 397 415 448 451 484

#Exercise 10
find_stop_codon = function(seq_vec, reading_frame) {
  codon_vec = c('TAA','TAG','AGA','AGG')
  stop_pos = NULL
  for (n in 1:length(codon_vec)) {
    codon = codon_vec[n]
    codon_position = find_codon(seq_vec, codon, reading_frame)
    stop_pos = c(stop_pos, codon_position)
  }
  stop_pos = sort(stop_pos)
  return(stop_pos)
}
find_stop_codon(mito_seq[1:500],1)
##[1]   7 199 217 226 274 334 388 424 442

#Exercise 11
first_ORF = paste(mito_seq[13:201],collapse = "")

#Exercise 12
find_all_open_frame = function(seq_vec, reading_frame) {
  start_pos = find_start_codon(seq_vec,reading_frame)
  stop_pos = find_stop_codon(seq_vec,reading_frame)
  ORF_pos = c(0) #set the first default value 0
  for (i in start_pos) {
    if (i > ORF_pos[length(ORF_pos)]) { #larger than the last stop position
      for (j in stop_pos) {
        if (j > i) {
          ORF_pos = c(ORF_pos,i)
          ORF_pos = c(ORF_pos,(j+2))
          break #only find the first match j
        }
      }
    }
  }
  ORF_pos = ORF_pos[-1] #remove the first value 0
  ORF_matrix = matrix(ORF_pos, ncol=2, byrow=TRUE)
  return(ORF_matrix)
}
find_all_open_frame(mito_seq[1:500],1)
##     [,1] [,2]
##[1,]   13  201
##[2,]  205  219
##[3,]  232  276
##[4,]  397  426

#Exercise 13
ORF_matrix = find_all_open_frame(mito_seq[1:500],1)
ORF_longgest = 0
start_position = 0
stop_positon = 0
for (n in 1:nrow(ORF_matrix)) {
  start_site = ORF_matrix[n,1]
  stop_site = ORF_matrix[n,2]
  ORF_length = stop_site - start_site + 1
  if (ORF_length > ORF_longgest) {
    ORF_longgest = ORF_length
    start_position = start_site
    stop_positon = stop_site
  }
}
cat('start:',start_position,'stop:',stop_positon,'longest length:',ORF_longgest)
##start: 13 stop: 201 longest length: 189

#Exercise 14
Base1 = "TTTTTTTTTTTTTTTTCCCCCCCCCCCCCCCCAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGG"
Base2 = "TTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGG"
Base3 = "TCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAG"
Am.acid = "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG"

translate_aa = function(seq_vec) {
  seq_vec = toupper(seq_vec)
  aa_seq = NULL
  for (i in seq(1,length(seq_vec),3)) {
    for (j in 1:nchar(Base1)) {
      if (seq_vec[i]==substr(Base1,j,j) && seq_vec[i+1]==substr(Base2,j,j)
          && seq_vec[i+2]==substr(Base3,j,j)) {
        aa_seq = c(aa_seq, substr(Am.acid,j,j))
      }
    }
  }
  return(aa_seq)
}
translate_aa(mito_seq[13:201])

#Exercise 15
aa_seq = translate_aa(mito_seq[13:201])
aa_name = c('*','A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y')
aa_freq = data.frame(
  amino_acid = aa_name,
  count_num = rep(0,21),
  freq = rep(0,21)
)
for (i in 1:length(aa_seq)) {
  for (j in 1:length(aa_name)) {
    if (aa_seq[i]==aa_name[j]) {
      aa_freq[j,2] = aa_freq[j,2] + 1 #finish counting
    }
  }
}
aa_freq$freq = 100*aa_freq$count_num/length(aa_seq)
barplot(aa_freq$freq, names.arg = aa_freq$amino_acid, space = 0.5)
