## makeCacheMatrix函数创建一个特殊的矩阵对象，该对象可以缓存其逆矩阵。
makeCacheMatrix <- function(x = matrix()) {
  # 初始化逆矩阵为一个空值
  inverse_matrix <- NULL
  
  # 定义设置新矩阵的函数
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  # 获取矩阵
  get <- function() x
  
  # 设置逆矩阵
  setInverse <- function(inverse) inverse_matrix <<- inverse
  
  # 获取逆矩阵
  getInverse <- function() inverse_matrix
  
  # 返回一个包含上述四个函数的列表
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve函数计算并返回一个矩阵的逆矩阵。如果逆矩阵已经被计算过，则从缓存中获取。
cacheSolve <- function(x, ...) {
  # 尝试从缓存中获取逆矩阵
  inverse_matrix <- x$getInverse()
  
  # 如果逆矩阵已经在缓存中，则返回它
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  
  # 如果没有缓存的逆矩阵，则计算它
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  
  # 将逆矩阵存储到缓存中
  x$setInverse(inverse_matrix)
  
  # 返回逆矩阵
  return(inverse_matrix)
}

