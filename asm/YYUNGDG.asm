*          DATA SET YYUNGDG    AT LEVEL 037 AS OF 01/18/02                      
*PHASE YYUNGDGA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE XSORT                                                                  
*                                                                               
READGDG  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*READGDG*,=V(REGSAVE)                                          
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         OPEN  (GDG,INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,=A(BUFFER)                                                    
         L     R3,=A(KEYTAB)                                                    
         SR    R4,R4               RESET TOTAL REC COUNTER (NO KEY REC)         
         SR    R6,R6               RESET TOTAL KEY REC COUNTER                  
*                                                                               
READ1ST  GET   GDG,REC                                                          
         CLC   =C'**',REC          BEGINING OF JCL FILE                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE '**' AS 1ST REC                    
*                                                                               
STKEY    AHI   R6,1                                                             
         CHI   R6,MAXRECQ                                                       
         BNH   *+6                                                              
         DC    H'0'                EXCESS MAX KEY REC                           
*                                                                               
         MVC   0(L'REC,R3),REC     MOVE INTO THE KEYTAB                         
         STH   R4,L'REC(R3)        BEGINING INDEX TO BUFFER                     
         SR    R5,R5               RESET REC COUNTER FOR THIS JCL FILE          
*                                                                               
NEXT     GET   GDG,REC                                                          
         CLC   =C'**',REC          BEGINING OF JCL FILE                         
         BE    NEXTJCL                                                          
*                                                                               
         C     R2,=A(EOBUFFER)                                                  
         BNH   *+6                                                              
         DC    H'0'                EXCESS THE STORAGE OF BUFFER                 
*                                                                               
         MVC   0(L'REC,R2),REC                                                  
         LA    R2,L'REC(R2)                                                     
         AHI   R4,1                                                             
         AHI   R5,1                                                             
         B     NEXT                                                             
*                                                                               
NEXTJCL  STH   R5,L'REC+2(R3)      # REC IN THE JCL FILE                        
         LA    R3,L'KEYTAB(R3)                                                  
         B     STKEY               STORE THE KEY REC                            
*                                                                               
EOGDG    STH   R5,L'REC+2(R3)      # REC IN THE JCL FILE                        
*                                                                               
         OR    R4,R4               ANY REC?                                     
         BNZ   SORT                YES - SORT THEM                              
*                                                                               
         MVC   P(38),=C'NO SOON JOBS EXCESS THE TCB TIME LIMIT'                 
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
*                                                                               
SORT     DS    0H                                                               
         GOTO1 =V(XSORT),DMCB,(0,=A(KEYTAB)),(R6),L'KEYTAB,L'KEYTAB,1           
*                                                                               
         L     R3,=A(KEYTAB)                                                    
*                                  PRINT KEY RECORD                             
PRTNEXT  OC    0(L'KEYTAB,R3),0(R3)      ANY MORE REC?                          
         BZ    EXIT                                                             
*                                                                               
         ZICM  R4,L'REC(R3),2      BEGINING INDEX IN BUFFER                     
         ZICM  R5,L'REC+2(R3),2    # OF REC FOR THIS JCL FILE                   
*                                                                               
         BCTR  R5,0                                                             
         AR    R4,R5               LAST REC # INDEX                             
         MHI   R4,L'BUFFER                                                      
         A     R4,=A(BUFFER)       POINT TO LAST REC (TCB=, ET=)                
*                                                                               
         MVC   P(L'BUFFER),0(R4)   TCB=, ET=  (P LINE COL# 45-69)               
         MVC   P(38),2(R3)         KEY (P LINE COL# 1-38)                       
         OC    P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   PRTCNTL,C'Y'        WANT TO PRINT CONTROL+REQUEST CARDS?         
         BNE   PRT60               NO                                           
*                                                                               
         ZICM  R4,L'REC(R3),2      BEGINING INDEX IN BUFFER                     
         ZICM  R5,L'REC+2(R3),2    # OF REC FOR THIS JCL FILE                   
         MHI   R4,L'BUFFER                                                      
         A     R4,=A(BUFFER)       POINT TO THAT JCL FILE IN BUFFER             
*                                  PRINT JCL FILE                               
PRT10    MVC   P(L'BUFFER),0(R4)                                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R4,L'BUFFER                                                      
         BCT   R5,PRT10                                                         
*                                                                               
PRT60    LA    R3,L'KEYTAB(R3)                                                  
         B     PRTNEXT                                                          
*                                                                               
EXIT     CLOSE GDG                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
GDG      DCB   DDNAME=GDG,MACRF=GM,DSORG=PS,RECFM=FB,LRECL=80,         +        
               EODAD=EOGDG                                                      
DUB      DS    D           TEMP STORAGE FOR PRINT PROCESS                       
DMCB     DS    8F                                                               
PRTCNTL  DC    C'N'        NO NEED TO PRINT CONTROL+REQUEST CARDS               
REC      DS    CL80                                                             
*                                                                               
         DC    C'*KEYTAB*'                                                      
KEYTAB   DS    (MAXRECQ)CL(L'REC+4)                                             
* TRAILING 4 BYTES ARE: START REC INDEX # IN BUFFER AND # REC FOR THIS          
*                       PARTICULAR JCL FILE                                     
EOKEYTAB EQU   *                                                                
*                                                                               
         DC    C'*BUFFER*'                                                      
BUFFER   DS    (MAXRECQ*AVELINQ)CL(L'REC)                                       
EOBUFFER EQU   *                                                                
MAXRECQ  EQU   2000                MAXIUM OF PQ JCL FILE                        
AVELINQ  EQU   25                  AVERAGE # LINES IN A PQ JCL FILE             
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037YYUNGDG   01/18/02'                                      
         END                                                                    
