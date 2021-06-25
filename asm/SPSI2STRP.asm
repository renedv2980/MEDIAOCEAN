*          DATA SET SPSI2STRP  AT LEVEL 002 AS OF 04/18/19                      
*PHASE SI2STRPA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
         TITLE 'SPSI2STRP - DELETE LOCAL CANADIAN NETWORK BUYS'                 
SI2STRP  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,SI2STRP,VREGSAVE                                               
*                                                                               
         L     RE,=V(PRINT)                                                     
         ST    RE,VPRINT                                                        
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT2    DS    0H                                                               
         LA    RE,SI2STRP          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         LA    R0,=CL30'DSPACE= CARD EXPECTED'                                  
         BRAS  RE,GETCARD                                                       
         CLC   =C'DSPACE=',CARD                                                 
         JNE   INITERR                                                          
*                                                                               
         LARL  RE,SSB                                                           
         USING SSBOFFD,RE                                                       
         MVC   SSODSPAC,CARD+7                                                  
         DROP  RE                                                               
*                                                                               
         BRAS  RE,GETCARD                                                       
         JNE   *+2                                                              
*                                                                               
         LA    R0,=CL30'DDSIO= CARD EXPECTEED'                                  
         CLC   =C'DDSIO=',CARD                                                  
         JNE   INITERR                                                          
         L     RE,=V(DDSIO)                                                     
         MVC   0(8,RE),CARD+6                                                   
*                                                                               
         LA    R0,=CL30'SPOTXAG CARD EXPECTED'                                  
         BRAS  RE,GETCARD                                                       
         JNE   INITERR                                                          
*                                                                               
         CLC   =C'SPOT',CARD                                                    
         JNE   INITERR                                                          
         MVC   QSYS,CARD                                                        
                                                                                
*=====================================================================          
* LOCATE SPOT SYSTEM AND GET SENUM. OPTIONAL AGENCY ALPHA ALSO                  
* CONVERT TO USE TWO CHR SYSTEM NAME SPOTXYAA WHERE XY=SYS AND AA=AGY           
*                                   +01234567                                   
*=====================================================================          
                                                                                
         CLI   CARD+7,C' '         SPOTXAA > SPOTX AA                           
         BNE   INIT10                                                           
         CLI   CARD+6,C' '                                                      
         BE    INIT10                                                           
         ICM   R0,3,CARD+5         MOVE AGY CODE ONE BYTE TO RIGHT              
         MVI   CARD+5,C' '                                                      
         STCM  R0,3,CARD+6                                                      
*                                                                               
INIT10   MVC   SESNAM+7(2),CARD+4  EXTRACT ONE/TWO CHR SYSTEM ID                
         MVC   QAGY,CARD+6         AGENCY ALWAYS AT CARD+6                      
*                                                                               
GETSEX   GOTO1 =V(DMDDNAME),DMCB,(X'24',=C'DDNAME'),SESNAM,0                    
         CLI   8(R1),0                                                          
         BNE   INITERR                                                          
         L     RF,8(R1)            GET A(FILE INFO LIST)                        
         MVC   SESNUM,1(RF)        EXTRACT SENUM                                
         L     RE,=A(UTL)          MOVE TO UTL                                  
         MVC   4(1,RE),SESNUM                                                   
         B     INIT12                                                           
*                                                                               
SESNAM   DC    C'SYS=SPT##'                                                     
SESNUM   DS    C                                                                
                                                                                
INIT12   LA    R0,SPTFLIST                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'SPOT',(R0)                        
                                                                                
* READ ID RECORD                                                                
                                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),QAGY       2 CHAR AGENCY CODE                           
         MVC   KEYSAVE,KEY                                                      
         LARL  R6,RCVREC                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY               
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         GOTO1 =V(DATAMGR),DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R6),   X         
               DMWORK                                                           
*                                                                               
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'02'                                                     
         MVI   ELCDHI,X'02'                                                     
         BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
         LLC   R0,3(R6)            GET AGY-MD BYTE                              
         N     R0,=X'000000F0'     DROP MEDIA                                   
         STC   R0,BAGY                                                          
         J     INIT20                                                           
                                                                                
SPTFLIST DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSPTFIL'                                                     
         DC    C'X'                                                             
*                                                                               
INIT20   OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (RECVOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TITLE(30),=CL30'SI2 STRIP'                                       
         B     PROC2                                                            
*                                                                               
INITERR  LR    RE,R0                                                            
         MVC   P(30),0(RE)                                                      
         GOTO1 =V(LOGIO),DMCB,1,(30,P)                                          
         GOTO1 =V(PRINTER)                                                      
CANCEL   DC    0H'0'                                                            
         ABEND 999                                                              
         EJECT                                                                  
*                                                                               
*** PROCESS INPUT FILE ***                                                      
*                                                                               
PROC2    GET   RECVIN,RCVREC                                                    
         AP    RECSIN,=P'1'                                                     
*                                                                               
         CLI   DM$RFILTY,X'21'        TEST SPTFILE                              
         JNE   PROC2                                                            
*                                                                               
         CLI   DM$RPRG,X'00'       CHANGED OFFLINE                              
         JE    PROC2               YES - IGNORE                                 
         CLI   DM$RPRG,X'28'       CHANGED BY MATCHMAKER                        
         JE    PROC2               YES - IGNORE                                 
         CLI   DM$RPRG,X'13'       TEST CHANGED BY PAY PROGRAM                  
         JE    PROC2               YES - IGNORE                                 
         CLI   DM$RPRG,X'01'       CHANGED BY PFM                               
         JE    PROC2               YES - IGNORE                                 
*                                                                               
         LLC   R0,RKEY                                                          
         N     R0,=X'000000F0'     DROP MEDIA                                   
         CLM   R0,1,BAGY                                                        
         JNE   PROC2                                                            
*                                                                               
         TM    RKEY,X'03'          TEST NETWORK                                 
         JNO   PROC10                                                           
         OC    RKEY+4(2),RKEY+4    TEST MARKET 0                                
         JNZ   PROC2                                                            
*                                                                               
PROC10   LA    R0,RCVREC                                                        
         PUT   RECVOUT,(0)                                                      
         AP    RECSOUT,=P'1'                                                    
         J     PROC2                                                            
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE RECVIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLOSE RECVOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    RECSIN+3,X'0F'                                                   
         UNPK  P(7),RECSIN                                                      
         MVC   P+8(10),=C'RECORDS IN'                                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OI    RECSOUT+3,X'0F'                                                  
         UNPK  P(7),RECSOUT                                                     
         MVC   P+8(10),=C'RECORDS OUT'                                          
         GOTO1 =V(PRINTER)                                                      
EOJ      DS    0H                                                               
         XBASE                                                                  
*                                                                               
GETCARD  NTR1                                                                   
*                                                                               
GETCARD2 GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         CLI   CARD,C'*'           TEST NOP                                     
         JE    GETCARD2                                                         
         CLC   =C'/*',CARD                                                      
         JE    NEQXIT                                                           
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
                                                                                
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JZ    *+2                                                              
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
RECSIN   DC    PL4'0'                                                           
RECSOUT  DC    PL4'0'                                                           
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,                       X        
               MACRF=PM,LRECL=8100,BLKSIZE=23476                                
         EJECT                                                                  
         DS    0D                                                               
DMCB     DS    6F                                                               
DMWORK   DS    12F                                                              
CARD     DS    CL80                                                             
VPRINT   DS    A                                                                
BAGY     DS    C                                                                
QAGY     DS    CL2                                                              
QSYS     DS    CL5                                                              
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
         DS    CL6                 SPARE                                        
DUB      DS    D                                                                
KEY      DS    CL24                                                             
KEYSAVE  DS    CL24                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'**UTL **'                                                    
UTL      DC    F'0',X'00'                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'02' NO RECOVERY                                  
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RCVREC   DC    F'0'                                                             
*PREFIX=DM$                                                                     
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
RKEY     DS    0CL13                                                            
         DS    8000C                                                            
       ++INCLUDE DDDPRINT                                                       
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPSI2STRP 04/18/19'                                      
         END                                                                    
