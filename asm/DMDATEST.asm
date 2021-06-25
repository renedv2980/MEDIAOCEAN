*          DATA SET DMDATEST   AT LEVEL 063 AS OF 05/01/02                      
*PHASE DMDATEST                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDADDS                                                                
*INCLUDE DMDYNDD                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE CARDS                                                                  
         TITLE 'DMDATEST - TEST NEW DADDS'                                      
         GBLC  &OFFLINE                                                         
&OFFLINE SETC  'N'                                                              
DMDATEST CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DMDATEST,=V(REGSAVE)                                           
         USING WORKD,RC                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
REOPEN   LA    R6,DAFILE                                                        
         USING DTFPHD,R6                                                        
         GOTO1 =V(DADDS),P1,14,,,DAFILE  OPEN THE FILE                          
**       MVC   P(10),=C'AFTER OPEN'                                             
**       GOTO1 =V(PRINTER)                                                      
**       BAS   RE,PRTBUFF                                                       
*&&DO                                                                           
DMIS2    DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   =C'/*',C                                                         
         BE    EXIT                                                             
*&&                                                                             
         LA    R4,DALIST                                                        
         B     DMDA4                                                            
*                                                                               
DMDA2    LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
*                                                                               
DMDA4    MVC   P6(4),0(R4)                                                      
*                                                                               
         GOTO1 =V(DADDS),P1,01,AIOA,0,,P6                                       
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DCHO                                                                   
         BAS   RE,PRTBUFF                                                       
         B     DMDA2                                                            
*                                                                               
DALIST   DC    X'09610100'                                                      
         DC    X'09710100'                                                      
         DC    X'09810100'                                                      
         DC    X'0A610100'                                                      
         DC    X'0B610100'                                                      
         DC    X'FF'                                                            
*                                                                               
SHIT     ICM   RE,3,P6                                                          
         AHI   RE,1                                                             
         STCM  RE,3,P6                                                          
         MVI   P6+2,1                                                           
*                                                                               
         CLC   P6(2),=X'0010'                                                   
         BL    AGAIN1                                                           
         B     EXIT                                                             
*                                                                               
         MVC   P6(4),=X'00010201'                                               
         GOTO1 =V(DADDS),P1,1,AIOA,,,P6                                         
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,PRTBUFF                                                       
         MVC   P6(4),=X'000A0201'                                               
         GOTO1 =V(DADDS),P1,1,AIOA,,,P6                                         
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,PRTBUFF                                                       
         BCT   R4,AGAIN                                                         
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'COUNT',IOCOUNT,C'DUMP',4,=C'1D'               
         GOTO1 =V(DADDS),P1,15,,,DAFILE  CLOSE THE FILE                         
         MVC   P(11),=C'AFTER CLOSE'                                            
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PRTBUFF                                                       
*                                                                               
         BC    0,EXIT                                                           
         MVI   *-3,X'F0'                                                        
         B     REOPEN                                                           
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
PRTBUFF  NTR1                                                                   
*        LA    R6,DAFILE                                                        
*        USING DTFPH,R6                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'DTF',(R6),C'DUMP',172,=C'1D'                  
         LH    R0,P3+2                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'D/A',P6,C'DUMP',4,=C'1D'                      
         GOTO1 =V(PRNTBL),DMCB,=C'IOA',AIOA,C'DUMP',(R0),=C'1D'                 
         XIT1                                                                   
         EJECT                                                                  
         ENTRY ADWAIT                                                           
         USING *,RF                                                             
ADWAIT   DS    0H                                                               
         LTR   RE,RE               RE IS NEG IF 31 BIT CALLER                   
         BP    ADWAITX                                                          
         AP    IOCOUNT,PCKD1                                                    
ADWAITX  BR    RE                                                               
         DROP  RF                                                               
IOCOUNT  DC    PL4'0'                                                           
PCKD1    DC    PL1'1'                                                           
         DS    0D                                                               
         EJECT                                                                  
DUB      DC    D'0'                                                             
COUNT    DC    PL4'0'                                                           
MAXCOUNT DS    PL4                                                              
DMCB     DS    6F                                                               
P1       DC    F'0'                                                             
P2       DC    F'0'                                                             
P3       DC    F'0'                                                             
P4       DC    F'0'                                                             
P5       DC    F'0'                                                             
P6       DC    F'0'                                                             
DSKADR   DC    F'0'                                                             
AIOA     DC    A(IOA)                                                           
WORK     DC    20F'0'                                                           
C        DS    CL80                                                             
SVC      DS    CL80                                                             
MVSDUMP  DC    C'N'                                                             
KEYST    DC    X'00'                                                            
         DS    0D                                                               
         DC    CL8'**KEY**'                                                     
KEY      DC    XL32'00'                                                         
KEYSAVE  DC    XL32'00'                                                         
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
         PRINT GEN                                                              
DAFILE   DMDA  BLKSIZE=4628                                                     
D2FILE   DMDA  BLKSIZE=4628                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'**IOA**'                                                     
IOA      DS    16000C                                                           
WORKD    DSECT                                                                  
         SPACE 2                                                                
SSB      CSECT                                                                  
         DC    X'0000',X'FF',X'88' WRITE TO FACWRK/COPIES REQUIRED              
         DC    X'00000000'                                                      
         DC    CL8' '                                                           
         DC    A(0),A(0)                                                        
         DC    XL32'00'                                                         
         SPACE 2                                                                
UTL      CSECT                                                                  
         DC    10F'0'                                                           
         SPACE 2                                                                
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063DMDATEST  05/01/02'                                      
         END                                                                    
