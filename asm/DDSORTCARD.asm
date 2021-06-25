*          DATA SET DDSORTCARD AT LEVEL 038 AS OF 09/08/88                      
*PHASE SORTCARD,*                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
SORTCARD TITLE 'SORT 80 COLUMN DATA'                                            
SORTCARD CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SORTCARD,VREGSAVE                                              
*                                                                               
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         B     INIT                                                             
*                                                                               
STXTAB   DS    0H                                                               
         DC    A(SORTCARD)                                                      
         DC    V(PDUMPER)                                                       
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT     OPEN  (CARDIN,(INPUT))                                                 
         GOTO1 =V(SORTER),DMCB,SORTCRD,RECCRD                                   
*                                                                               
SORT2    GET   CARDIN,IOAREA                                                    
         GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
         B     SORT2                                                            
*                                                                               
SORT10   CLOSE (CARDIN,)                                                        
         OPEN  (CARDOUT,(OUTPUT))                                               
*                                                                               
SORT11   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BE    SORT20                                                           
         L     R3,DMCB+4                                                        
         MVC   P(80),0(R3)                                                      
         PUT   CARDOUT,(R3)                                                     
         GOTO1 =V(PRINTER)                                                      
         B     SORT11                                                           
*                                                                               
SORT20   DS    0H                                                               
         CLOSE CARDIN                                                           
         XBASE                                                                  
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(1,80,A),FORMAT=BI,WORK=1'                      
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=80'                                    
         EJECT                                                                  
CARDIN   DCB   DDNAME=CARDIN,DSORG=PS,RECFM=FB,MACRF=(GM),             X        
               EODAD=SORT10                                                     
CARDOUT  DCB   DDNAME=CARDOUT,DSORG=PS,RECFM=FB,MACRF=(PM)                      
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
         DS    F                                                                
IOAREA   DS    CL80                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038DDSORTCARD09/08/88'                                      
         END                                                                    
