*          DATA SET DEISSMPL6  AT LEVEL 002 AS OF 08/17/00                      
*PHASE DEISSMPA                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
         TITLE 'SAMPLE PROGRAM'                                                 
SAMPLE   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*SAMPLE*,=V(REGSAVE)                                           
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
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
         PRINT GEN                                                              
MYSUB    NTR1                                                                   
*                                                                               
*        DO NOTHING                                                             
*                                                                               
         XIT1                                                                   
         PRINT NOGEN                                                            
         EJECT                                                                  
DMCB     DS    6F                                                               
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
**PAN#1  DC    CL21'002DEISSMPL6 08/17/00'                                      
         END                                                                    
