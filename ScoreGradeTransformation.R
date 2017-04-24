ScoreGradeTransformation <- function(score, max.score, guessing.score) {
  # Transform raw scores to guessing corrected grades
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
