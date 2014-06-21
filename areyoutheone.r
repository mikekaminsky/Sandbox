#Areyoutheone.r

#boys <- c(1:10)
#girls <- c(1:10)
#couples <- cbind("boys" = c(1,2,3,4,5,6,7,8,9,10), "girls" = c(7,4,1,10,2,5,3,8,6,9))

n <- 10 

########################################################


num_correct <- function(test, correct){
    num <- sum(test[1:n] == correct)
    return(num)
}


swap <- function(df,a,b){
    x <- df[a]
    df[a] <- df[b]
    df[b] <- x
    return(df)
}

new_draw <- function(h){
    temp_pos <- sample(1:n,n,FALSE)    
    if (tail(duplicated(rbind(h,temp_pos)),1)==FALSE){
        return(temp_pos)
    } 
    else {new_draw(h)}
}

#Most naive play -- randomly reshuffle
#rules_engine <- function(h){
    #rules_pos <- new_draw(h)
    #return(rules_pos)
#}

rules_engine <- function(vec,h){
    repeat{
        r <- sample(1:n,2,FALSE) 
        r1 <- r[1]
        r2 <- r[2]
        if (length(h) == n+1) {
            best <- h[1:n]
        }else{
           best <- h[max(h[,n+1])==h[,n+1],]
           if (length(best)>n+1){
                best <- head(best,1)
           }
           best <- best[1:n]
        }
        rules_pos <- swap(best,r1,r2)
        if (length(h) != n+1) {
            if (tail(duplicated(rbind(h[,1:n],rules_pos)),1)==FALSE){
                break
            }
        }else{break}
    }
    return(rules_pos)
}

play_game <- function(){
    cor <- sample(1:n,n,FALSE)
    new_pos <- c(1:n)    
    history <- c(new_pos,num_correct(new_pos,cor))
    i=1
    while (num_correct(new_pos,cor) != n){
        new_pos  <- rules_engine(new_pos,history)
        new_pos <- c(new_pos,num_correct(new_pos,cor))
        history <- rbind(history,new_pos)
        i = i+1
   } 
   return(history)
}

repeat_game <- function(games){
    x <- c(1:games) 
    for(j in 1:games){
        num_tries <- nrow(play_game())
        x[j] <- num_tries
    }
    return(x)
}

#game_output <- repeat_game(100)
#h1 <- play_game()
print(nrow(play_game()))
