*          DATA SET SDIRCMP    AT LEVEL 002 AS OF 07/06/77                      
*PHASE SDIRCMP,*,NOAUTO                                                         
*INCLUDE REGSAVE                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFFZZWZ                                                               
         TITLE 'SPTDIR COMPARE'                                                 
         PRINT NOGEN                                                            
         NBASE 0,SDIRCMP,=V(REGSAVE)                                            
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(11),=C'SPTDIR CMPR'                                        
         MVC   SUB1(8),=C'NOT ON 1'                                             
         MVC   SUB1+40(8),=C'NOT ON 2'                                          
         OPEN  TP1,TP2                                                          
*                                                                               
GET12    GET   TP1,REC1                                                         
         GET   TP2,REC2                                                         
CMPR     CLC   REC1(14),REC2                                                    
         BE    GET12                                                            
         BH    NOTON1                                                           
* NOT ON 2                                                                      
         GOTO1 =V(HEXOUT),P1,REC1,P,14,=C'TOG'                                  
         GOTO1 =V(PRINTER)                                                      
         GET   TP1,REC1                                                         
         B     CMPR                                                             
* NOT ON 1                                                                      
NOTON1   DS    0H                                                               
         GOTO1 =V(HEXOUT),P1,REC2,P+40,14,=C'TOG'                               
         GOTO1 =V(PRINTER)                                                      
         GET   TP2,REC2                                                         
         B     CMPR                                                             
END1     CLOSE TP1                                                              
END2     CLOSE TP2                                                              
         LTORG                                                                  
REC1     DS    18C                                                              
REC2     DS    18C                                                              
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
         EJECT                                                                  
TP1      DTFMT DEVADDR=SYS010,IOAREA1=AREA1,FILABL=STD,RECFORM=FIXBLK, X        
               BLKSIZE=3600,RECSIZE=18,WORKA=YES,EOFADDR=END1                   
TP2      DTFMT DEVADDR=SYS011,IOAREA1=AREA2,FILABL=STD,RECFORM=FIXBLK, X        
               BLKSIZE=3600,RECSIZE=18,WORKA=YES,EOFADDR=END2                   
AREA1    DS    3600C                                                            
AREA2    DS    3600C                                                            
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
 END                                                                            
