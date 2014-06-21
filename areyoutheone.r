#Areyoutheone.r

#boys <- c(1:10)
#girls <- c(1:10)
#couples <- cbind("boys" = c(1,2,3,4,5,6,7,8,9,10), "girls" = c(7,4,1,10,2,5,3,8,6,9))

n <- 10 

########################################################


num_correct <- function(test, correct){
    num <- sum(test == correct)
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

rules_engine <- function(vec,h,c){
    repeat{
        r <- sample(1:n,2,FALSE) 
        r1 <- r[1]
        r2 <- r[2]
        prior <- num_correct(vec,c)
        rules_pos <- swap(vec,r1,r2)
        post <- num_correct(rules_pos,c)
        if (tail(duplicated(rbind(h,rules_pos)),1)==FALSE){
            h <- rbind(h,rules_pos)
            if(prior >= post){
                rules_pos <- vec
            }
            break
        }

    }
    return(list(rules_pos,h))
}

get_num_correct <- function(his_frame,c){
    output <- apply(his_frame,1,function(x) num_correct(x,c))
    output <- cbind(his_frame,output)
    return(output)
}

play_game <- function(){
    cor <- sample(1:n,n,FALSE)
    new_pos <- c(1:n)    
    history <- new_pos
    i=1
    while (num_correct(new_pos,cor) != n){
        #new_pos <- new_draw(history)
        both <- rules_engine(new_pos,history,cor)
        new_pos <- both[[1]]
        history <- both[[2]]
        i = i+1
   } 
   #print(get_num_correct(history,cor))
   return(i)
}

repeat_game <- function(games){
    
    x <- c(1:games) 
    for(j in 1:games){
        x[j] <- play_game()
    }
    return(x)
}

game_output <- repeat_game(100)

