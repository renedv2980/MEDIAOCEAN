*          DATA SET SPREPFXB1  AT LEVEL 015 AS OF 04/18/94                      
*PHASE SPFX02C,+0                                                               
*INCLUDE SPB1WI                                                                 
         TITLE 'SPFX02C - TEST WESTERN BILLING INTERFACE'                       
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         MVI   FCRDBUYS,C'N'                                                    
         CLI   MODE,ESTFRST                                                     
         BE    GOB1                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
B1D      USING SPWID,B1BLK                                                      
*                                                                               
GOB1     DS    0H                                                               
         XC    B1BLK,B1BLK                                                      
         LA    R0,B1HOOK                                                        
         ST    R0,B1D.SPWHOOK                                                   
         MVC   B1D.SPWAIO,ADBUY                                                 
*                                                                               
         MVC   B1D.SPWQPRD,BPRD                                                 
         MVC   B1D.SPWQEST,BEST                                                 
         GOTO1 DATCON,DMCB,QSTART,(3,DUB)                                       
         MVC   B1D.SPWQSYM,DUB                                                  
         GOTO1 DATCON,DMCB,QEND,(3,DUB)                                         
         MVC   B1D.SPWQEYM,DUB                                                  
         MVI   B1D.SPWQBTYP,C'E'                                                
*                                                                               
         CLI   QOPT1,C'Y'          TEST TO TRACE ALL                            
         BNE   *+8                                                              
         MVI   B1D.SPWINDS,X'C0'                                                
         CLI   QOPT1,C'H'          TEST TO TRACE HOOK ONLY                      
         BNE   *+8                                                              
         MVI   B1D.SPWINDS,X'40'                                                
         CLI   QOPT1,C'T'          TEST TO TRACE TSAR ONLY                      
         BNE   *+8                                                              
         MVI   B1D.SPWINDS,X'80'                                                
*                                                                               
         GOTO1 =V(SPB1WI),DMCB,(RA),B1BLK                                       
         B     EXIT                                                             
         EJECT                                                                  
B1HOOK   NTR1                                                                   
         L     R4,B1D.SPWAIO                                                    
         GOTO1 PRNTBL,DMCB,=C'B1HOOK',(R4),C'DUMP',WIRECL,=C'1D00'              
         XIT1                                                                   
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
* CONSTANTS                                                                     
*                                                                               
         LTORG                                                                  
ELCODE   DS    CL1                                                              
         DS    0D                                                               
         DC    C'**BIBLK'                                                       
B1BLK    DS    XL32                                                             
                                                                                
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPB1WIBLK                                                      
         SPACE 2                                                                
       ++INCLUDE SPB1WIREC                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPFXB1 04/18/94'                                      
         END                                                                    
