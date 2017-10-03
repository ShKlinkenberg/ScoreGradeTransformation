ScoreGradeTransformationSplitLinear <- function(score, guess.score, min.grade, cutoff.precentage, max.score) {
  # Computes the grade corrected for guessing.
  #
  # Args:
  #   score             : Value or vector with raw exam scores
  #   guess.score       : Total amount of raw guessing score
  #   min.grade         : Minimal grate to be given
  #   cutoff.precentage : Raw cut-off score for a pass mark
  #   max.score         : Maximum raw score to be given
  #
  # Returns:
  #   Grade expressed as a number between minimal grade and 10
  
  # Setup vector to store grades
  grade = vector()
  
  # Determine cut-off score based on percentage and guessing score
  cutoff.score = guess.score + ( cutoff.precentage * (non.guess.score) )
  
  # Grade when guessing
  grade[score <= guess.score] = min.grade
  # Grade between guessing and cutoff
  
  ## Slope of transformation function
  a.tot.cutoff = (cutoff.precentage - min.grade) / (cutoff.score - guess.score); a.tot.cutoff
  
  grade[score > guess.score & score < cutoff.score] = (a.tot.cutoff * (score[score > guess.score & score < cutoff.score]-guess.score)) + min.grade
  
  # Grade between cutoff and max score
  
  ## Slope of transformation function
  a.na.cutoff = (1 - cutoff.precentage) / (max.score - cutoff.score); a.na.cutoff   
  
  grade[score >= cutoff.score] = (a.na.cutoff * (score[score >= cutoff.score]-cutoff.score)) + cutoff.precentage
  
  return(grade * 10)   
}   