*          DATA SET CPFILFIX   AT LEVEL 012 AS OF 09/01/00                      
*PHASE CPFILFIA CPFILFIX                                                        
         TITLE 'CPFILFIX - LOADABLE FILE FIX PROGRAM'                           
CPFILFIX CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FLFX**                                                       
         L     R2,0(R1)            A(RECORD)                                    
         USING CPKEYD,R2                                                        
         CLI   CPKTYPE,X'02'                                                    
         BE    FIX2                                                             
         CLI   CPKTYPE,X'04'                                                    
         BE    FIX2                                                             
         B     XIT                                                              
         SPACE 2                                                                
FIX2     CLC   CPKMKT,=H'880'      BAD MARKET(SANTA CLARA)                      
         BNE   *+10                                                             
         MVC   CPKMKT,=H'807'      EQUATED SAN FRANCISCO                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE SUBSTITUTE VALUES                                        
         SPACE 3                                                                
CONV     CLI   0(R4),X'FF'                                                      
         BE    CONV2                                                            
         CLC   0(1,R3),0(R4)                                                    
         BE    CONV2                                                            
         LA    R4,2(R4)                                                         
         B     CONV                                                             
         SPACE 2                                                                
CONV2    MVC   0(1,R3),1(R4)                                                    
         BR    RE                                                               
         SPACE 2                                                                
DPLIST   DS    0F                                                               
         DC    X'11',C'C'                                                       
         DC    X'12',C'E'                                                       
         DC    X'21',C'G'                                                       
         DC    X'22',C'J'                                                       
         DC    X'23',C'L'                                                       
         DC    X'32',C'N'                                                       
         DC    X'41',C'P'                                                       
         DC    C'AA'                                                            
         DC    C'CC'                                                            
         DC    C'EE'                                                            
         DC    C'GG'                                                            
         DC    C'JJ'                                                            
         DC    C'KK'                                                            
         DC    C'LL'                                                            
         DC    C'NN'                                                            
         DC    C'PP'                                                            
         DC    C'RR'                                                            
         DC    X'FF',C'R'                                                       
         SPACE 2                                                                
GREY     DS    0H                                                               
         DC    C'RO'                                                            
         DC    C'TO'                                                            
         DC    C'CN'                                                            
         DC    C'UK'                                                            
         DC    C'VN'                                                            
         DC    C'WO'                                                            
         DC    C'SO'                                                            
         DC    C'FS'                                                            
         DC    X'FF'                                                            
         DC    C'O'                                                             
         SPACE 2                                                                
PRLIST   DS    0H                                                               
         DC    C'SS'                                                            
         DC    C'MM'                                                            
         DC    C'NN'                                                            
         DC    C'KK'                                                            
         DC    C'FF'                                                            
         DC    X'FF'                                                            
         DC    C'O'                                                             
         EJECT                                                                  
       ++INCLUDE CPGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012CPFILFIX  09/01/00'                                      
         END                                                                    
