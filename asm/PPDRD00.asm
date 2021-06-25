*          DATA SET PPDRD00    AT LEVEL 020 AS OF 05/01/02                      
         TITLE 'CHANGE LOG'                                                     
*                                                                               
* SMYE 11/27/00 "PROGRAM REPLACED BY SFM" MESSAGE DISPLAYED                     
*                ALL ACTIVITY DISABLED                                          
*                                                                               
* ROSA  2/9/90  ADD CLIENT SECURITY                                L01          
*                                                                               
*------------\                                                                  
*PHASE T40900A                                                                  
*------------/                                                                  
         TITLE 'PPDRD00  PRINTPAK LOGICAL FILE MAINT.  DIV/REG/DST'             
T40900   CSECT                                                                  
         NMOD1 500,T40900,RR=R9                                                 
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         BAS   RE,INITL                                                         
         USING T409FFD,RA                                                       
         LA    R9,IOAREA                                                        
*                                                                               
         B     EXIT        NO ACTIVITY - DEACTIVATED WEEK OF 11/27/00           
*                                                                               
*    SAVE THE FIRST THREE BYTES OF OVERLAY SCREEN AND SET HDRLAST               
*    TO CLEAR AFTER SO LOWER SCREEN WILL BE CLEARED IF AN ERROR                 
*    OCCURS IN THE HEADER FIELDS                                                
         MVC   SAVESCRN(3),HDRLAST                                              
         MVC   HDRLAST(3),=X'000001'                                            
         XC    HDRMSG(60),HDRMSG                                                
         TM    HDRRECH+4,X'20'                                                  
         BO    *+8                                                              
         B     NOTVAL                                                           
         TM    HDRACTH+4,X'20'                                                  
         BO    CKVALC                                                           
         CLC   HDRACT(3),=C'CHA'      IF ACT=CHA AND LAST ACT=DISP              
         BNE   NOTVAL             THEN GOT TO EDIT IF KEY THE SAME              
         CLI   BACT,X'03'                                                       
         BNE   NOTVAL                                                           
         TM    HDRMEDH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         TM    HDRCLTH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         TM    HDRDIVH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         CLI   BREC,X'03'         DIVISION                                      
         BE    CHAVAL                                                           
         TM    HDRREGH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         CLI   BREC,X'04'          REGION                                       
         BE    CHAVAL                                                           
         TM    HDRDSTH+4,X'20'                                                  
         BNO   NOTVAL                                                           
CHAVAL   OI    HDRACTH+4,X'20'     VALIDATE                                     
         MVI   BACT,X'02'                                                       
         B     CKVALC                                                           
*                                                                               
NOTVAL   NI    HDRRECH+4,X'DF'                                                  
         NI    HDRACTH+4,X'DF'                                                  
         NI    HDRMEDH+4,X'DF'                                                  
         NI    HDRCLTH+4,X'DF'                                                  
         NI    HDRDIVH+4,X'DF'                                                  
         NI    HDRREGH+4,X'DF'                                                  
         NI    HDRDSTH+4,X'DF'                                                  
*                                                                               
*  VALIDATE RECORD                                                              
*                                                                               
         LA    R2,HDRRECH                                                       
         LA    R3,RECERR                                                        
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         LA    R5,LFRECS                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,RELO                                                          
         LA    R5,6(R5)                                                         
         IC    R4,5(R2)                                                         
         BCTR  R4,R0                                                            
CKREC    EX    R4,COMP                                                          
         BE    CKRECA                                                           
         BXLE  R5,R6,CKREC                                                      
         B     ERROR                                                            
CKRECA   MVC   BREC(1),8(R5)                                                    
         OI    HDRRECH+4,X'20'                                                  
         FOUT  HDRRECH                                                          
*                                                                               
*  VALIDATE ACTION                                                              
         LA    R2,HDRACTH                                                       
         LA    R3,ACTERR                                                        
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         LA    R5,LFACTS                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,RELO                                                          
         LA    R5,6(R5)                                                         
         IC    R4,5(R2)                                                         
         BCTR  R4,R0                                                            
CKACT    EX    R4,COMP                                                          
         BE    CKACTA                                                           
         BXLE  R5,R6,CKACT                                                      
         B     ERROR                                                            
CKACTA   MVC   BACT(1),8(R5)                                                    
         OI    HDRACTH+4,X'20'                                                  
         FOUT  HDRACTH                                                          
*                                                                               
COMP     CLC   8(0,R2),0(R5)                                                    
*                                                                               
*                                                                               
*  VALIDATE  RECORD/ACTION COMBINATION                                          
CKVALC   LA    R5,COMBOS                                                        
         LA    R6,3                                                             
         LA    R7,COMBOSX-1                                                     
CKCOMB   CLC   BREC(2),0(R5)                                                    
         BE    CKMED                                                            
         BXLE  R5,R6,CKCOMB                                                     
         NI    HDRRECH+4,X'DF'                                                  
         NI    HDRACTH+4,X'DF'                                                  
         LA    R3,COMBERR                                                       
         B     ERROR                                                            
*                                                                               
*    REC/ACT OK  - SO EDIT KEY                                                  
*                                                                               
*  VALIDATE MEDIA                                                               
CKMED    MVC   OLNUM(1),2(R5)                                                   
         LA    R2,HDRMEDH                                                       
         LA    R3,MEDERR                                                        
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         XC    KEY,KEY                                                          
         MVC   KAGY,AGYALPHA            READ AGYHDR                             
         MVC   KMED,8(R2)                                                       
         MVI   KRCD,X'01'                                                       
         TM    4(R2),X'20'                                                      
         BO    CKCLT                                                            
         FOUT  HDRMEDNH,SPACES,10                                               
         FOUT  HDRCLTNH,SPACES,20                                               
         FOUT  HDRDIVNH,SPACES,20                                               
         FOUT  HDRREGNH,SPACES,20                                               
         FOUT  HDRDSTNH,SPACES,20                                               
         NI    HDRCLTH+4,X'DF'                                                  
         NI    HDRDIVH+4,X'DF'                                                  
         NI    HDRREGH+4,X'DF'                                                  
         NI    HDRDSTH+4,X'DF'                                                  
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         FOUT  HDRMEDNH,PAGYMED,10                                              
         OI    4(R2),X'20'              VALIDATE                                
         MVI   BYTE2,1                  SET ACTION = FORMAT                     
         B     CKCLT                                                            
         EJECT                                                                  
*                                                                               
*  VALIDATE CLIENT                                                              
*                                                                               
CKCLT    LA    R2,HDRCLTH                                                       
         LA    R3,CLTERR                                                        
         OI    HDRCLT+2,X'40'                                                   
         MVC   KCLT,HDRCLT                                                      
         MVI   KRCD,X'02'                                                       
         TM    4(R2),X'20'                                                      
         BO    CKDIV                                                            
         FOUT  HDRCLTNH,SPACES,20                                               
         FOUT  HDRDIVNH,SPACES,20                                               
         FOUT  HDRREGNH,SPACES,20                                               
         FOUT  HDRDSTNH,SPACES,20                                               
         NI    HDRDIVH+4,X'DF'                                                  
         NI    HDRREGH+4,X'DF'                                                  
         NI    HDRDSTH+4,X'DF'                                                  
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         CLI   T409FFD+1,C'*'      DDS TERMINAL                                 
         BE    CKCLT5                                                           
*                                                                               
         PRINT GEN                                                L02           
         LA    RF,SECURITY                                                      
         LA    R3,VDATAMGR                                                      
         GOTO1 (RF),DMCB,T409FFD,PCLTREC,(R3)                                   
         PRINT NOGEN                                                            
         BE    CLT4                                                             
         LA    R3,207                                           L01             
         B     ERROR                                            L01             
*                                                                               
**************                                                  L01             
CLT4     DS    0H                                                               
CKCLT5   MVC   CPROFLE(20),PCLTPROF                                             
         FOUT  HDRCLTNH,PCLTNAME,20                                             
         OI    4(R2),X'20'              VALIDATE                                
         MVI   BYTE2,1                  SET ACTION = FORMAT                     
         B     CKDIV                                                            
         EJECT                                                                  
CKDIV    LA    R2,HDRDIVH                                                       
         LA    R3,DIVERR                                                        
         BAS   RE,ANY                                                           
         BAS   RE,PACK                                                          
         CP    DUB,=P'999'         MAX IS 999                                   
         BH    ERROR                                                            
         OI    DUB+7,X'0F'                                                      
         UNPK  KDIV(3),DUB+6(2)                                                 
         MVI   KRCD,X'03'                                                       
         TM    4(R2),X'20'                                                      
         BO    CKREG                                                            
         FOUT  HDRDIVNH,SPACES,20                                               
         FOUT  HDRREGNH,SPACES,20                                               
         FOUT  HDRDSTNH,SPACES,20                                               
         NI    HDRREGH+4,X'DF'                                                  
         NI    HDRDSTH+4,X'DF'                                                  
         CLC   BREC(2),=X'0301'                                                 
         BE    CKADD                                                            
         BAS   RE,READ                                                          
         MVC   DIVADDR,KEY+27                                                   
         BAS   RE,GETREC                                                        
         FOUT  HDRDIVNH,PDIVNAME,20                                             
         OI    4(R2),X'20'              VALIDATE                                
         MVI   BYTE2,1                  SET ACTION = FORMAT                     
         B     CKREG                                                            
         EJECT                                                                  
CKREG    CLI   BREC,X'03'                                                       
         BE    GETOVLY                                                          
         LA    R2,HDRREGH                                                       
         LA    R3,REGERR                                                        
         BAS   RE,ANY                                                           
         BAS   RE,PACK                                                          
         CP    DUB,=P'999'         MAX IS 999                                   
         BH    ERROR                                                            
         OI    DUB+7,X'0F'                                                      
         UNPK  KREG(3),DUB+6(2)                                                 
         MVI   KRCD,X'04'                                                       
         TM    4(R2),X'20'                                                      
         BO    CKDST                                                            
         FOUT  HDRREGNH,SPACES,20                                               
         FOUT  HDRDSTNH,SPACES,20                                               
         NI    HDRDSTH+4,X'DF'                                                  
         CLC   BREC(2),=X'0401'                                                 
         BE    CKADD                                                            
         BAS   RE,READ                                                          
         MVC   REGADDR,KEY+27                                                   
         BAS   RE,GETREC                                                        
         FOUT  HDRREGNH,PREGNAME,20                                             
         OI    4(R2),X'20'              VALIDATE                                
         MVI   BYTE2,1                  SET ACTION = FORMAT                     
         B     CKDST                                                            
         EJECT                                                                  
CKDST    CLI   BREC,X'04'                                                       
         BE    GETOVLY                                                          
         LA    R2,HDRDSTH                                                       
         LA    R3,DSTERR                                                        
         BAS   RE,ANY                                                           
         BAS   RE,PACK                                                          
         CP    DUB,=P'999'         MAX IS 999                                   
         BH    ERROR                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         OI    DUB+7,X'0F'                                                      
         UNPK  KDST(3),DUB+6(2)                                                 
         MVI   KRCD,X'05'                                                       
         TM    4(R2),X'20'                                                      
         BO    GETOVLY                                                          
         FOUT  HDRDSTNH,SPACES,20                                               
         CLC   BREC(2),=X'0501'                                                 
         BE    CKADD                                                            
         BAS   RE,READ                                                          
         MVC   DSTADDR,KEY+27                                                   
         BAS   RE,GETREC                                                        
         FOUT  HDRDSTNH,PDSTNAME,20                                             
         OI    4(R2),X'20'              VALIDATE                                
         MVI   BYTE2,1                  SET ACTION = FORMAT                     
         B     GETOVLY                                                          
         EJECT                                                                  
CKADD    LA    R4,=C'DMREAD'                                                    
         LA    R5,=C'PRTDIR'                                                    
         LA    R6,KEY                                                           
         LA    R7,KEYSAVE                                                       
         LA    R1,DMCB                                                          
         STM   R4,R7,0(R1)                                                      
         MVI   0(R1),X'08'         SET TO PASS DELETED RECORDS                  
         MVC   16(1,R1),TERMNAL                                                 
         L     RF,VDATAMGR                                                      
         BASR  RE,RF                                                            
         LA    R3,51               DISK ERROR                                   
         TM    8(R1),X'40'                                                      
         BNZ   ERROR                                                            
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ADDOK                                                            
         LA    R3,56               DELETED                                      
         TM    KEYSAVE+25,X'80'                                                 
         BNZ   ERROR                                                            
         LA    R3,52               DUP KEY ON ADD                               
         B     ERROR                                                            
ADDOK    MVI   BYTE2,1             SET ACTION = FORMAT                          
         OI    4(R2),X'20'         VALIDATE                                     
         B     GETOVLY                                                          
         EJECT                                                                  
         CNOP  2,4                                                              
LFRECS   DC    H'9'                                                             
         DC    A(LFRECSX-1)                                                     
         DC    CL8'DIVISION'                                                    
         DC    X'03'                                                            
         DC    CL8'DIVREC'                                                      
         DC    X'03'                                                            
         DC    CL8'REGION'                                                      
         DC    X'04'                                                            
         DC    CL8'REGREC'                                                      
         DC    X'04'                                                            
         DC    CL8'DISTRICT'                                                    
         DC    X'05'                                                            
         DC    CL8'DSTREC'                                                      
         DC    X'05'                                                            
LFRECSX  EQU   *                                                                
         EJECT                                                                  
         CNOP  2,4                                                              
LFACTS   DC    H'9'                                                             
         DC    A(LFACTSX-1)                                                     
         DC    CL8'ADD'                                                         
         DC    X'01'                                                            
         DC    CL8'CHANGE'                                                      
         DC    X'02'                                                            
         DC    CL8'DISPLAY'                                                     
         DC    X'03'                                                            
LFACTSX  EQU   *                                                                
*                                                                               
*                                                                               
COMBOS   DC    X'030101'                                                        
         DC    X'030201'                                                        
         DC    X'030301'                                                        
         DC    X'040102'                                                        
         DC    X'040202'                                                        
         DC    X'040302'                                                        
         DC    X'050103'                                                        
         DC    X'050203'                                                        
         DC    X'050303'                                                        
COMBOSX  EQU   *                                                                
*                                                                               
SAVESCRN DS    CL3                                                              
SPACES   DC    CL40' '                                                          
ZEROS    DC    40C'0'                                                           
RECERR   EQU   11                                                               
ACTERR   EQU   12                                                               
MEDERR   EQU   13                                                               
CLTERR   EQU   14                                                               
DIVERR   EQU   22                                                               
REGERR   EQU   23                                                               
DSTERR   EQU   24                                                               
COMBERR  EQU   10                                                               
CACCERR  EQU   207                 LIMIT ACCESS ERROR                           
         EJECT                                                                  
*    RESTORE FIRST THREE BYTES OF OVERLAY SCREEN - NO ERROR IN                  
*    HEADER LINES                                                               
*                                                                               
GETOVLY  MVC   HDRLAST(3),SAVESCRN                                              
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),OLNUM                                                    
         ST    RA,DMCB+4                                                        
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
         BASR  RE,RF                                                            
         TM    4(R1),X'FF'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RC,DMCB                                                          
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         CLI   BYTE3,1                                                          
         BNE   EXXMOD                                                           
         MVC   HDRMSG,=CL60'*** ACTION COMPLETED ***'                           
         CLI   BACT,1                                                           
         BE    REQ                                                              
         CLI   BACT,2                                                           
         BE    REQ                                                              
GTOV2    DS    0H                                                               
         LA    R2,HDRRECH                                                       
         CLI   BACT,1                                                           
         BNE   EXIT                                                             
         NI    HDRMEDH+4,X'DF'     UNVALIDATE MEDIA ON COMPLETED ADD            
         B     EXIT                                                             
         SPACE 2                                                                
REQ      DS    0H                                                               
         XC    QCTL,QCTL                                                        
         MVI   QAREA,C' '                                                       
         MVC   QAREA+1(79),QAREA                                                
         MVC   QAREA(2),=C'42'                                                  
         MVC   QAREA+2(2),AGYALPHA                                              
         MVC   QAREA+4(1),HDRMED                                                
         MVC   QAREA+5(3),HDRCLT                                                
         MVC   QAREA+8(3),KDIV                                                  
         CLI   KRCD,X'05'                                                       
         BNE   REQ2                                                             
         MVC   QAREA+17(3),KDST                                                 
         B     REQ3                                                             
REQ2     DS    0H                                                               
         CLI   KRCD,X'04'                                                       
         BNE   REQ4                                                             
REQ3     DS    0H                                                               
         MVC   QAREA+14(3),KREG                                                 
REQ4     DS    0H                                                               
*                                                                               
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,42                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                   
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    GTOV2                                                            
         SR    R3,R3                                                            
         B     ERROR                                                            
*                                                                               
*          DATA SET PPACCTEST  AT LEVEL 006 AS OF 02/07/90                      
         SPACE 2                                                                
*       *************************                                               
******  TEST OFFICE LIST SECURITY  ******                                       
*       *************************                                               
         SPACE 2                                                                
*                  / **************************\                                
*                  \ **************************/                                
         SPACE 2                                                                
       ++INCLUDE PPSECURITY                                                     
*******************************************************                         
*******************************************************                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGENEROL                                                       
*                                                                               
       ++INCLUDE GENOLD                                                         
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
PUBIO    DS    CL1         LABEL TO SATISFY DATAMGR CALL IN GENOLD              
*                                                                               
         ORG   IOAREA                                                           
*                                                                               
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PDIVREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PREGREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PDSTREC                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND                                                         
*                                                                               
*                                                                               
         ORG   KEY                                                              
KAGY     DS    CL2                                                              
KMED     DS    CL1                                                              
KRCD     DS    CL1                                                              
KCLT     DS    CL3                                                              
KDIV     DS    CL3                                                              
KREG     DS    CL3                                                              
KDST     DS    CL3                                                              
         DS    CL6                                                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPDRDFFD                                                       
         ORG   T409FFD                                                          
         DS    CL16                                                             
BREC     DS    CL1                                                              
BACT     DS    CL1                                                              
OLNUM    DS    CL1                                                              
DIVADDR  DS    F                                                                
REGADDR  DS    F                                                                
DSTADDR  DS    F                                                                
CPROFLE  DS    CL20                                                             
       ++INCLUDE DDOFFICED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020PPDRD00   05/01/02'                                      
         END                                                                    
