*          DATA SET DMWORKTST  AT LEVEL 005 AS OF 03/13/91                      
*PHASE WORKTST,*,NOAUTO                                                         
*INCLUDE DMDMGR                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE RANDOM                                                                 
       ++INCLUDE DMGREQUS                                                       
         TITLE 'WKTEST - PROGRAM TO TEST NEW WKFILE'                            
         PRINT NOGEN                                                            
WKTEST   CSECT                                                                  
         NBASE 0,WKTEST,WORK=A(WKWORK)                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(16),=C'WORKER FILE TEST'                                   
*                                                                               
         L     R3,=A(WKREC)        R3=A(WKFILE RECORD)                          
         L     R4,=A(WKBUFF)       R4=A(WKFILE BUFFER)                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'BUFF',WKFILE,WKINDEX,(R3),(R4)               
         L     R7,8(R4)            R7=A(WKFILE BUFFER SAVE AREA)                
*                                                                               
         GOTO1 =V(DMENQDEQ),PARM,(C'X',0)                                       
         L     RE,4(R1)                                                         
         MVC   RETADDRS,0(RE)      SAVE ADCONS RETURNED BY ENQDEQ               
         L     RE,RMYKEY                                                        
         MVC   CPU(8),0(RE)        SAVE MY CPUID AND ADRID                      
*                                                                               
WKTEST4  GOTO1 RANDOM,100                                                       
         STH   R1,NFILES           SAVE N'FILES TO GENERATE                     
         XC    FILESN,FILESN                                                    
         L     R2,=A(WKEYS)                                                     
         B     ADD                                                              
         EJECT                                                                  
ADD      LH    R1,FILESN                                                        
         LA    R1,1(R1)                                                         
         STH   R1,FILESN                                                        
         CLC   FILESN,NFILES                                                    
         BH    NDX                                                              
         XC    0(L'WKEYS,R2),0(R2)                                              
         XC    KEY,KEY                                                          
         LH    R1,FILESN                                                        
         LA    R1,2000(R1)                                                      
         STCM  R1,3,KUSR                                                        
*&&DO                                                                           
         UNPK  DUB,CPU+2(3)                                                     
         MVI   KSYS,C'D'           S IS DOS SYSTEM ID                           
         MVC   KPRG(3),DUB+4       PPX IS CPUID=C'NNN'                          
         MVI   KDAY,X'01'                                                       
         MVC   KCLS,ADR+1          CLASS IS PRTN CHR=C'0',C'1',..,C'B'          
         CLI   KCLS,C'G'                                                        
         BNE   *+8                                                              
         MVI   KCLS,C'0'                                                        
*&&                                                                             
*&&OS                                                                           
         MVI   KSYS,C'M'           M IS MVS SYSTEM ID                           
         MVC   KPRG(3),CPU         PPX IS CPUID=C'SYN'                          
         SR    R0,R0                                                            
         ICM   R0,3,ADR+2                                                       
         CVD   R0,DUB                                                           
         MVC   KDAY,DUB+6          DAY/CLASS IS ASID (TRUE)                     
         PACK  KCLS,DUB+7(1)                                                    
         OI    KCLS,C'0'                                                        
*&&                                                                             
*                                                                               
         THMS                                                                   
         STCM  R1,15,0(R2)         SAVE TIME                                    
         GOTO1 RANDOM,1000                                                      
         STH   R1,NRECS                                                         
         MVC   4(2,R2),NRECS       SAVE N'RECORDS                               
         L     RF,RCOUNTS                                                       
         LA    RF,120(RF)          POINT TO WORKER COUNTS                       
         ZAP   8(4,R2),0(4,RF)     SAVE WORKER ENQUES                           
*                                                                               
         XC    WKINDEX,WKINDEX     BUILD KEY                                    
         MVC   WKINDEX(8),KEY                                                   
         MVC   16(8,R2),KEY        SAVE KEY                                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'OPEN'                                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R5,NRECS                                                         
ADD2     L     R6,=A(WKREC)        SET IN START OF RECORD                       
         MVC   0(2,R6),=H'278'                                                  
         MVC   2(8,R6),KEY                                                      
         THMS                                                                   
         STCM  R1,15,10(R6)                                                     
         L     RF,RCOUNTS                                                       
         LA    RF,120(RF)                                                       
         MVC   14(120,R6),0(RF)    WORKER COUNTS                                
         MVC   134(144,R6),120(RF) PHYSICAL COUNTS                              
         GOTO1 =V(DATAMGR),DMCB,=C'ADD'                                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BCT   R5,ADD2                                                          
*                                                                               
ADD4     GOTO1 =V(DATAMGR),DMCB,=C'CLOSE'                                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,RCOUNTS                                                       
         LA    RF,120(RF)                                                       
         ZAP   12(4,R2),0(4,RF)                                                 
         MVI   P,C'A'                                                           
         GOTO1 =V(HEXOUT),PARM,WKINDEX,P+2,16,=C'TOG'                           
         GOTO1 =V(HEXOUT),PARM,(R4),P+40,32,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARM,(R2),P+40,32,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         CLC   16(8,R2),0(R4)                                                   
         BE    *+6                                                              
         DC    H'0'                OH FUCK                                      
         LA    R2,L'WKEYS(R2)                                                   
         B     ADD                                                              
         EJECT                                                                  
NDX      L     R2,=A(WKEYS)                                                     
         XC    FILESN,FILESN                                                    
*                                                                               
NDX2     LH    R1,FILESN                                                        
         LA    R1,1(R1)                                                         
         STH   R1,FILESN                                                        
         CLC   FILESN,NFILES                                                    
         BH    STA                                                              
         XC    WKINDEX,WKINDEX                                                  
         MVC   WKINDEX(8),16(R2)                                                
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'80',=C'INDEX')                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    RECSN,RECSN                                                      
*                                                                               
RDF      GOTO1 =V(DATAMGR),DMCB,=C'READ'                                        
         CLI   8(R1),0                                                          
         BE    RDF2                                                             
         TM    8(R1),X'80'                                                      
         BO    RDF6                                                             
         DC    H'0'                                                             
*                                                                               
RDF2     OC    RECSN,RECSN                                                      
         BNZ   RDF4                                                             
         MVI   P,C'I'                                                           
         GOTO1 =V(HEXOUT),PARM,WKINDEX,P+2,16,=C'TOG'                           
         GOTO1 =V(HEXOUT),PARM,(R3),P+40,32,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
RDF4     LH    R1,RECSN                                                         
         LA    R1,1(R1)                                                         
         STH   R1,RECSN                                                         
         L     R6,=A(WKREC)                                                     
         CLC   2(8,R6),16(R2)                                                   
         BE    *+6                                                              
         DC    H'0'                OH FUCK                                      
         B     RDF                                                              
*                                                                               
RDF6     CLC   RECSN,4(R2)                                                      
         BE    *+6                                                              
         DC    H'0'                OH FUCK                                      
         LA    R2,L'WKEYS(R2)                                                   
         B     NDX2                                                             
         EJECT                                                                  
STA      XC    FILESN,FILESN                                                    
         L     R2,=A(WKEYS)                                                     
STA2     LH    R1,FILESN                                                        
         LA    R1,1(R1)                                                         
         STH   R1,FILESN                                                        
         CLC   FILESN,NFILES                                                    
         BNH   STA4                                                             
         MVI   P,C'-'                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 =V(PRINTER)                                                      
         B     WKTEST4                                                          
*                                                                               
STA4     XC    WKINDEX,WKINDEX                                                  
         MVC   WKINDEX(8),16(R2)                                                
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'80',=C'INDEX')                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'PURGE'                                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   P,C'P'                                                           
         GOTO1 =V(HEXOUT),PARM,WKINDEX,P+2,16,=C'TOG'                           
         GOTO1 =V(HEXOUT),PARM,(R2),P+40,32,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         LA    R2,L'WKEYS(R2)                                                   
         B     STA2                                                             
*                                                                               
WKEOJ    XBASE                                                                  
         SPACE 1                                                                
RANDOM   LR    R0,RE               WALLACE IS STILL HERE TO HAUNT US            
         LR    RF,R1                                                            
         BCTR  RF,0                                                             
         GOTO1 =V(RANDOM),PARM,(RF)                                             
         L     R1,4(R1)                                                         
         LA    R1,1(R1)                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
CPU      DS    XL4                                                              
ADR      DS    XL4                                                              
NFILES   DS    H                                                                
FILESN   DS    H                                                                
NRECS    DS    H                                                                
RECSN    DS    H                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
*                                                                               
KEY      DS    0XL8                WORKER KEY                                   
KUSR     DS    XL2                                                              
KSYS     DS    CL1                                                              
KPRG     DS    CL2                                                              
KSPG     DS    CL1                                                              
KDAY     DS    XL1                                                              
KCLS     DS    CL1                                                              
*                                                                               
         DS    0F                                                               
RETADDRS DS    0CL20               RETURN ADDRS FROM DMENQDEQ                   
RDDSQDQ  DS    A                   A(DDSQDQ DTF)                                
RMYKEY   DS    A                   A(KEY WHEN I OWN RECORD)                     
RIOKEY   DS    A                   A(KEY WHEN HE OWNS RECORD)                   
RIORKEY  DS    A                   A(KEY AND DATA ACTUAL)                       
RCOUNTS  DS    A                   A(ENQUEUE COUNTERS)                          
         EJECT                                                                  
         DS    0D                                                               
WKFILE   DC    CL8'WKFILE'                                                      
         DC    C'**WKNDX*'                                                      
WKINDEX  DC    XL32'00'                                                         
         DS    0D                                                               
         DC    C'**WKREC*'                                                      
WKREC    DC    1024X'00'                                                        
         DS    0D                                                               
         DC    C'**WKBUF*'                                                      
WKBUFF   DC    4000X'00'                                                        
         DS    0D                                                               
         DC    C'**WKWRK*'                                                      
WKWORK   DC    5000D'0'                                                         
         DS    0D                                                               
         DC    C'**WKEYS*'                                                      
WKEYS    DS    100XL32                                                          
*                                                                               
UTL      CSECT                                                                  
         DC    F'0',X'01000000'                                                 
         SPACE 1                                                                
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DMWORKTST 03/13/91'                                      
         END                                                                    
