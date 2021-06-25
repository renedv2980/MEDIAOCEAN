*          DATA SET DEISSMPL5  AT LEVEL 007 AS OF 08/17/00                      
*PHASE DEISSMPA                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
         TITLE 'SAMPLE PROGRAM'                                                 
SAMPLE   CSECT                                                                  
*********PRINT NOGEN                                                            
         NBASE 0,*SAMPLE*,=V(REGSAVE)                                           
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         EDIT  (R3),(10,WORK1)                                                  
         SPACE 3                                                                
         EDIT  (R3),(10,PRNTAREA+5)                                             
         SPACE 3                                                                
         EDIT  (R3),(L'A,A)                                                     
         SPACE 3                                                                
         EDIT  (TIME,NOW),(10,WORK1)                                            
         SPACE 3                                                                
         EDIT  TODAY,(10,WORK1)                                                 
         SPACE 3                                                                
*                                                                               
         AF                                                                     
         SPACE 3                                                                
         AF    FULL1,FULL2                                                      
         SPACE 3                                                                
         AF    10(R5),FULL2                                                     
         SPACE 3                                                                
         AF    FULL1,10(R5)                                                     
         SPACE 3                                                                
         AF    0(R5),FULL2                                                      
         SPACE 3                                                                
         AF    FULL1,0(R5)                                                      
         SPACE 3                                                                
         AF    (R5),FULL2                                                       
         SPACE 3                                                                
         AF    FULL1,(R5)                                                       
         SPACE 3                                                                
         AF    R5,FULL2                                                         
         SPACE 3                                                                
         AF    FULL1,R5                                                         
         SPACE 3                                                                
         AF    FULL1,10                                                         
         SPACE 3                                                                
         AF    10,FULL2                                                         
         SPACE 3                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
FULLWORD DS    F                                                                
PRNTAREA DS    CL16                                                             
WORK1    DS    CL16                                                             
A        DS    CL16                                                             
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(SAMPLE,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         MVC   TITLE(14),=C'SAMPLE PROGRAM'                                     
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
*                                                                               
         MVC   P(31),=C'THE STUDENTS IN THIS CLASS ARE:'                        
         BASR  RE,RF                                                            
*                                                                               
         LA    R3,STUDTAB          POINT TO THE STUDENT TABLE                   
LOOP     MVC   P(10),0(R3)         PUT STUDENT NAME INTO PRINT LINE             
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF               PRINT A LINE                                 
         LA    R3,L'STUDTAB(R3)    BUMP TO NEXT ENTRY                           
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   LOOP                NO                                           
*                                                                               
         MVC   P(18),=C'THAT''S ALL, FOLKS!'                                    
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
DUB      DS    D                                                                
*                                                                               
STUDTAB  DS    0CL10                                                            
         DC    C'JOHN      '                                                    
         DC    C'ROBERT    '                                                    
         DC    CL10'WILL'                                                       
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DEISSMPL5 08/17/00'                                      
         END                                                                    
