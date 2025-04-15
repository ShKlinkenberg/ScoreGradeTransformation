ScoreGradeTransformationCorrectForGuessingOnMaxScore <- function(score, max.score, guessing.score) {
  # Transform raw scores to guessing corrected for total amount of points on exam
  #
  # Args:
  #   score          : Total amount of raw points on an exam
  #   max.score      : The maximum amount of points possible on the exam
  #   guessing.score : The number of guessing points on the total exam
  #
  # Returns:
  #   grade : The grade corrected for guessing. Between 0 and 10. 
  
  grade = ( (score - guessing.score) / (max.score - guessing.score) ) * 10
  
  # Cap grade to minimum and maximum
  grade[grade <  0] = 0
  grade[grade > 10] = 10
  
  return(grade)
}

ScoreGradeTransformationCorrectForGuessingOnClosed <- function(closed.sum, open.sum, total.guess, max.closed, max.points) {
  # Transform raw scores to guessing corrected for closed amount of points on exam
  #
  # Args:
  #   closed.sum  : sum score integer for closed questions
  #   open.sum    : sum score integer for open questions
  #   total.guess : total amount of quessing points float
  #   max.closed  : max amount of points possible for closed questions
  #   max.points  : max amount of points possibla for entire exam
  #  
  # Returns:
  #   grade : calculated grade as float Between 0 and 10
  
  grade = ( ( closed.sum - total.guess ) / ( max.closed - total.guess ) *  max.closed + open.sum ) / max.points * 10
  
  # Cap grade to minimum and maximum
  grade[grade <  0] = 0
  grade[grade > 10] = 10
    
  return(grade)
}

SGT_CorrectOnlyForQuessQuestionsLimitToZero <- function(sum.guess.questions, sum.non.guess.questions, max.guess.score, max.non.guess.questions, max.points) {
  # Transform raw scores to guessing corrected only for guess questions amount of points on exam
  #
  # Args:
  #   sum.guess.questions     : sum score integer for guess questions
  #   sum.non.guess.questions : sum score integer for non quess quations like fill in the blanks or essay questions
  #   max.guess.score         : float total amount of quessing points 
  #   max.non.guess.questions : max amount of points possible for non quess questions
  #   max.points              : max amount of points possibla for entire exam
  #  
  # Returns:
  #   grade : calculated grade as float Between 0 and 10

  # Calculate proportion guess corrected score
  
  guess.corrected.score = sum.guess.questions - total.guess.score

  # Limit guess corrected score to 0. In case the guess score is less than the total amount of quess points. 
  # This ensures the guess part does not become negative and that students need to compensate the negative guessing score
  # with open question points.

  guess.corrected.score = max(guess.corrected.score, 0)

  # Calculate guess corrected max quess score

  guess.corrected.max.quess.score = max.non.guess.questions - max.guess.score
  
  grade = ( guess.corrected.score / guess.corrected.max.quess.score * max.non.guess.questions + sum.non.guess.questions ) / max.points * 10
  
  # Cap grade to minimum and maximum
  grade[grade <  0] = 0
  grade[grade > 10] = 10
    
  return(grade)
}
