library(testthat)
expect_true(is_tmhmm_installed())
fasta_gz_filename <- tempfile(fileext = "_UP000005640_9606.fasta.gz")
download.file(
  url = "ftp://ftp.ebi.ac.uk/pub/databases/reference_proteomes/QfO/Eukaryota/UP000005640_9606.fasta.gz",
  destfile = fasta_gz_filename
)
fasta_filename <- tempfile(fileext = "_UP000005640_9606.fasta")
R.utils::gunzip(
  filename = fasta_gz_filename,
  destname = fasta_filename,
  remove = FALSE
)
expect_true(file.exists(fasta_filename))
# We know this reference proteome has 20600 proteins
expect_equal(
  20600,
  nrow(pureseqtmr::load_fasta_file_as_tibble_cpp(fasta_filename))
)
tmhmm_filename <- tempfile(fileext = "_UP000005640_9606_no_u.tmhmm")

# The original file does not work, takes minutes for this to find out
expect_error(
  run_tmhmm_to_file(
    fasta_filename = fasta_filename,
    tmhmm_filename = tmhmm_filename
  ),
  "Character 'U' not allowed in alphabet 'ACDEFGHIKLMNPQRSTVWYBXZ'."
)

# Remove all proteins with a selenocysteine
fasta_no_u_filename <- "UP000005640_9606_no_u.fasta"
t <- pureseqtmr::load_fasta_file_as_tibble_cpp(fasta_filename)
# Remove the Us
t_no_u <- t[ -stringr::str_which(string = t$sequence, pattern = "U"), ]
nrow(t_no_u)
pureseqtmr::save_tibble_as_fasta_file(t = t_no_u, fasta_filename = fasta_no_u_filename)


run_tmhmm_to_file(
  fasta_filename = fasta_no_u_filename,
  tmhmm_filename = tmhmm_filename
)

expect_true(file.exists(tmhmm_filename))
expect_equal(
  nrow(pureseqtmr::load_fasta_file_as_tibble_cpp(fasta_no_u_filename))
  nrow(pureseqtmr::load_fasta_file_as_tibble_cpp(tmhmm_filename))
)

