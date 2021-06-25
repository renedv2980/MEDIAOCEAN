*          DATA SET SPRES61    AT LEVEL 038 AS OF 11/17/00                      
*PHASE T20F61A,+0                                                               
         TITLE 'DATA FOR RANKER'                                                
T20F61   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F61**,RA,RR=R2                                              
*                                                                               
         BR    RE                  ONLY DATA HERE                               
         EJECT                                                                  
PHSTABLE DC    A(T20F61)                                                        
DATTABLE DC    A(FRIENDS)                                                       
         DC    A(CFRENDS)                                                       
         DC    A(BBMDPTS)                                                       
         DC    A(RSTANDPT)                                                      
         DC    A(STANDPTS)                                                      
         DC    30A(0)              SPARE                                        
         EJECT                                                                  
*                                  STANDARD DAYPART TABLE                       
         EJECT                                                                  
       ++INCLUDE RADSDPLA                                                       
       ++INCLUDE RADSDPL                                                        
*                                  CANADIAN DAYPART TABLE                       
       ++INCLUDE RADSDPLC                                                       
         EJECT                                                                  
         EJECT                                                                  
*                                  FRIENDLY DAYPART EXPRESSIONS                 
FRIENDS  DC    C'AMDRIVE ',X'7C',AL1(06,10),12X'00'                             
         DC    C'PMDRIVE ',X'7C',AL1(15,19),12X'00'                             
         DC    C'DRIVE   ',X'7C',AL1(06,10),X'7C',AL1(15,19),9X'00'             
         DC    X'FF'                                                            
*                                  FRIENDLY CANADIAN EXPRESSIONS                
CFRENDS  DC    C'BREAKFAST      ',X'7C',AL1(01,01),12X'00'                      
         DC    C'DAY            ',X'7C',AL1(02,02),12X'00'                      
         DC    C'DRIVE          ',X'7C',AL1(03,03),12X'00'                      
         DC    C'EVENING        ',X'7C',AL1(04,04),12X'00'                      
         DC    C'BR+DA          ',X'7C',AL1(09,09),12X'00'                      
         DC    C'BR+DR          ',X'7C',AL1(10,10),12X'00'                      
         DC    C'BR+EV          ',X'7C',AL1(11,11),12X'00'                      
         DC    C'DA+DR          ',X'7C',AL1(12,12),12X'00'                      
         DC    C'DA+EV          ',X'7C',AL1(13,13),12X'00'                      
         DC    C'DR+EV          ',X'7C',AL1(14,14),12X'00'                      
         DC    C'BR+DA+DR       ',X'7C',AL1(15,15),12X'00'                      
         DC    C'BR+DA+EV       ',X'7C',AL1(16,16),12X'00'                      
         DC    C'BR+DR+EV       ',X'7C',AL1(17,17),12X'00'                      
         DC    C'DA+DR+EV       ',X'7C',AL1(18,18),12X'00'                      
         DC    C'BR+DA+DR+EV    ',X'7C',AL1(19,19),12X'00'                      
         DC    C'SATURDAY       ',X'02',AL1(05,05),12X'00'                      
         DC    C'SUNDAY         ',X'01',AL1(06,06),12X'00'                      
         DC    C'SA+SU          ',X'03',AL1(20,20),12X'00'                      
         DC    C'BR+SA          ',X'7E',AL1(21,21),12X'00'                      
         DC    C'DA+SA          ',X'7E',AL1(22,22),12X'00'                      
         DC    C'DR+SA          ',X'7E',AL1(23,23),12X'00'                      
         DC    C'BR+DR+SA       ',X'7E',AL1(24,24),12X'00'                      
         DC    C'BR+SA+SU       ',X'7F',AL1(25,25),12X'00'                      
         DC    C'DA+SA+SU       ',X'7F',AL1(26,26),12X'00'                      
         DC    C'DR+SA+SU       ',X'7F',AL1(27,27),12X'00'                      
         DC    C'EV+SA+SU       ',X'7F',AL1(28,28),12X'00'                      
         DC    C'BR+DA+DR+SA    ',X'7E',AL1(29,29),12X'00'                      
         DC    C'BR+DA+SA+SU    ',X'7F',AL1(30,30),12X'00'                      
         DC    C'BR+DA+DR+SA+SU ',X'7F',AL1(31,31),12X'00'                      
         DC    C'BR+DA+DR+EV+SA ',X'7E',AL1(32,32),12X'00'                      
         DC    C'ALL            ',X'7F',AL1(33,33),12X'00'                      
         DC    C'REACH          ',X'7F',AL1(07,07),12X'00'                      
         DC    C'M-S 5A-1A      ',X'7F',AL1(07,07),12X'00'                      
         DC    X'FF'                                                            
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038SPRES61   11/17/00'                                      
         END                                                                    
