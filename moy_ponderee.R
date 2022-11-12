
note =as.data.frame(cbind(notes=c(5,20,12,11),
                          coefs=c(6,2,6,2),
                          nom=c("marie","marie","paul","paul")))

note$notes <- as.numeric(note$notes)
note$coefs <- as.numeric(note$coefs)

tapply(seq_along(note$notes),note$nom,
                    function(xx){return(weighted.mean(x=note$notes[xx],w=note$coefs[xx]))})

# verification :
(5*6+20*2)/(6+2)
(12*6+11*2)/(6+2)

