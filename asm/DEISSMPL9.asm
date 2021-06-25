*          DATA SET DEISSMPL9  AT LEVEL 023 AS OF 09/21/00                      
*PHASE DEISSMPA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'SAMPLE PROGRAM FOR IDF DEMO'                                    
SAMPLE   CSECT                                                                  
         PRINT GEN                                                              
         NBASE 0,*SAMPLE*,=V(REGSAVE)                                           
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         MVC   TITLE(14),=C'SAMPLE PROGRAM'                                     
         MVC   P(31),=C'THE STUDENTS IN THIS CLASS ARE:'                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R3,STUDTAB          POINT TO THE STUDENT TABLE                   
         USING STUDENTD,R3                                                      
LOOP     MVC   PRTNAME,NAME        PUT STUDENT NAME INTO PRINT LINE             
         MVC   PRTZIP,ZIPCODE      PUT ZIPCODE INTO PRINT LINE                  
         GOTO1 =V(PRINTER)         PRINT A LINE                                 
         LA    R3,STUDENTQ(,R3)    BUMP TO NEXT ENTRY                           
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   LOOP                NO                                           
         DROP  R3                                                               
*                                                                               
         MVC   P(18),=C'THAT''S ALL, FOLKS!'                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         BAS   RE,TROUBLE          CALL VERY, VERY BAD SUBROUTINE               
*                                                                               
* EXIT THE PROGRAM                                                              
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
TROUBLE  NTR1                                                                   
*                                                                               
* THESE ARE RANDOMLY WRITTEN INSTRUCTIONS. THEY MAY OR NOT DO WHAT THE          
* PROGRAMMER HAD ACTUALLY INTENDED.                                             
*                                                                               
* NOTE THAT EVERY ONE OF THESE STATEMENTS ASSEMBLES CLEANLY.                    
*                                                                               
         LA    R3,FULL1                                                         
         L     R3,FULL1                                                         
         MVC   HEX3,HEX4                                                        
         ST    R5,FULL1                                                         
         STH   R5,HALF1                                                         
*                                                                               
         STH   R5,FULL1                                                         
         ST    R5,HALF1                                                         
         MVC   8(R2),9(R3)                                                      
         CVB   R0,DUB                                                           
         A     R7,FULL1                                                         
         AH    R7,FULL2                                                         
         CLC   9(23,R9),6                                                       
         SR    R3,R2                                                            
         MVC   DUB+4,HALF2                                                      
         LA    R2,HEX4(7)                                                       
         A     R2,8                                                             
         XC    STRING1,(R7)                                                     
         MVC   HALF2,FULL1                                                      
         ICM   R5,R8,15                                                         
         MVC   5(R2),C'A'                                                       
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
DMCB     DS    6F                                                               
*                                                                               
DUB      DS    D                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
HEX3     DS    XL3                                                              
HEX4     DS    XL4                                                              
STRING1  DS    CL10                                                             
STRING2  DS    CL15                                                             
*                                                                               
STUDTAB  DS    0C                                                               
         DC    C'10024',CL10'JOHN'                                              
         DC    C'10027',CL10'SEAN'                                              
         DC    C'10101',CL10'GALINA'                                            
         DC    C'10304',CL10'JAIME'                                             
         DC    C'12483',CL10'CARLTON'                                           
         DC    C'14953',CL10'EVA'                                               
         DC    C'10026',CL10'SHERIDAN'                                          
         DC    C'11105',CL10'YI'                                                
         DC    C'14930',CL10'MICHELLE'                                          
         DC    X'FF'               END-OF-TABLE MARKER                          
         SPACE 3                                                                
DUMPLIST DS    0F                                                               
         DC    A(SAMPLE),V(DUMMY)                                               
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
STUDENTD DSECT                                                                  
ZIPCODE  DS    CL5                                                              
NAME     DS    CL10                                                             
STUDENTQ EQU   *-STUDENTD                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         ORG   P                                                                
PRTNAME  DS    CL10                                                             
         DS    CL3                                                              
PRTZIP   DS    CL5                                                              
         ORG                                                                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023DEISSMPL9 09/21/00'                                      
         END                                                                    
