*          DATA SET SPINF01    AT LEVEL 024 AS OF 05/08/06                      
*PHASE T21A01A                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE DPTRD                                                                  
         PRINT NOGEN                                                            
         TITLE 'T21A01-SPOTPAK INFO-KEY VALIDATION ROUTINE'                     
T21A01   CSECT                                                                  
         NMOD1 0,T21A01,RR=R8                                                   
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         B     ST                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
ST       XC    FLEN,FLEN                                                        
         LA    R2,SINIKEYH                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    SVKEYDTA,SVKEYDTA                                                
         LA    R7,KEYTAB                                                        
         LH    R8,0(R7)                                                         
         L     R9,2(R7)                                                         
         A     R9,RELO                                                          
         LA    R7,6(R7)                                                         
*                                                                               
         CLC   SVOVLY,0(R7)                                                     
         BE    *+10                                                             
         BXLE  R7,R8,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         L     RE,0(R7)                                                         
         A     RE,RELO                                                          
         BR    RE                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
CLT      MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SINKMSG(10),WORK+1                                               
         FOUT  SINKMSGH                                                         
         MVC   SVKEY+1(1),SVAGYMD                                               
*                                                                               
         MVI   ERRCD,2                                                          
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+2(2),SVCLT                                                 
*                                                                               
         BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   SINKMSG+11(20),CNAME                                             
         DROP  R8                                                               
*                                                                               
         MVI   ERRCD,3                                                          
         BAS   R9,GETPRD                                                        
         MVC   SVKEY+4(3),SVEBCPRD                                              
         CLC   SVEBCPRD,=C'ALL'                                                 
         BE    EXIT                                                             
*                                                                               
         BAS   R9,RDPRD                                                         
         USING PRDHDRD,R8                                                       
         MVC   SVPRD,PCODE+1                                                    
         DROP  R8                                                               
*                                                                               
CLT2     CLI   SVREC,13                                                         
         BL    EXIT                                                             
         BAS   RE,CHKEND           TEST IF START ESTIMATE ENTERED               
*                                                                               
         MVI   ERRCD,4                                                          
         BAS   R9,GETNUM                                                        
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BADKEY                                                           
         CH    R0,=H'255'                                                       
         BH    BADKEY                                                           
         STC   R0,SVEST                                                         
         STC   R0,SVKEY+7                                                       
         B     EXIT                                                             
         EJECT                                                                  
* PROCESS KEYS FOR COMMENT RECORDS *                                            
         SPACE 1                                                                
COM      XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=X'0D0C'                                                
*                                                                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SINKMSG(10),WORK+1                                               
         FOUT  SINKMSGH                                                         
         MVC   SVKEY+2(1),SVAGYMD                                               
*                                                                               
         MVC   SVKEY+3(1),SINIREC  FIRST CHAR OF REC = COMMENT TYPE             
         CLC   =C'A3',SINIREC                                                   
         BNE   *+8                                                              
         MVI   SVKEY+3,C'3'        EXCEPT FOR A2 COMMENTS                       
*                                                                               
         BAS   RE,CHKEND                                                        
         MVI   ERRCD,2                                                          
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+4(2),SVCLT                                                 
*                                                                               
         BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   SINKMSG+11(20),CNAME                                             
         DROP  R8                                                               
*                                                                               
         BAS   RE,CHKEND                                                        
         MVI   ERRCD,3                                                          
         BAS   R9,GETPRD                                                        
*                                                                               
         CLI   SVREC,X'1F'         TEST SDR                                     
         BE    COM2                                                             
         MVC   SVKEY+6(3),SVEBCPRD                                              
         CLC   SVEBCPRD,=C'ALL'                                                 
         BE    CLT2                                                             
*                                                                               
COM2     BAS   R9,RDPRD                                                         
         USING PRDHDRD,R8                                                       
         MVC   SVPRD,PCODE+1                                                    
         CLI   SVREC,X'1F'         TEST SDR                                     
         BNE   COM4                                                             
         XC    SVKEY+6(3),SVKEY+6  SDR USES BINARY PRODUCT                      
         MVC   SVKEY+8(1),SVPRD                                                 
         DROP  R8                                                               
*                                                                               
COM4     BAS   RE,CHKEND                                                        
         MVI   ERRCD,4                                                          
         BAS   R9,GETNUM                                                        
         CP    DUB,=P'1'                                                        
         BL    BADKEY                                                           
         CP    DUB,=P'255'                                                      
         BH    BADKEY                                                           
         STC   R0,SVKEY+9                                                       
*                                                                               
         CLI   SVREC,X'1F'         TEST STA DESC REC                            
         BNE   COMX                                                             
*                                                                               
         BAS   RE,CHKEND                                                        
         MVI   ERRCD,5                                                          
         BAS   R9,GETSTA                                                        
         GOTO1 VMSPACK,DMCB,=C'0000',SVSTA,DUB                                  
         MVC   SVKEY+10(3),DUB+2   MOVE STATION ONLY                            
*                                                                               
COMX     B     EXIT                                                             
         EJECT                                                                  
PGRP     MVC   SVKEY(2),=X'0D01'                                                
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SINKMSG(10),WORK+1                                               
         FOUT  SINKMSGH                                                         
         MVC   SVKEY+4(1),SVAGYMD                                               
*                                                                               
         XC    SVCLT,SVCLT                                                      
         CLI   SVREC,X'15'                                                      
         BNE   *+8                                                              
         BAS   R9,CHKEND                                                        
         MVI   ERRCD,2                                                          
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+5(2),SVCLT                                                 
         BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   SINKMSG+11(20),CNAME                                             
         DROP  R8                                                               
*                                                                               
         MVI   ERRCD,3                                                          
         XC    SVEST(3),SVEST                                                   
         BAS   RE,CHKEND                                                        
         BAS   R9,GETGRP                                                        
         CLI   SVREC,X'15'         TEST PGRDEF                                  
         BNE   PGRPX                                                            
* PGRDEF MUST SPECIFY ID/OMIT NUMBER                                            
         CLI   FULL,0                                                           
         BE    BADKEY                                                           
         OC    FULL+1(2),FULL+1                                                 
         BNZ   BADKEY                                                           
         B     PGRPX                                                            
*                                                                               
PGRPX    MVC   SVKEY+7(3),FULL                                                  
         MVC   SVEST(3),FULL                                                    
         B     EXIT                                                             
         EJECT                                                                  
MGRP     MVC   SVKEY(2),=X'0D02'                                                
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+2(1),SVAGYMD                                               
         MVC   SINKMSG(10),WORK+1                                               
         FOUT  SINKMSGH                                                         
* EDIT CLIENT OR 'ALL'                                                          
         MVI   ERRCD,2                                                          
         BAS   RE,FLDVAL                                                        
         CLC   =C'ALL',0(R4)                                                    
         BNE   MGRP10                                                           
* PRDGRP MUST BE 'ALL' IF CLIENT 'ALL'                                          
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,3                                                         
         BNE   BADKEY                                                           
         CLC   =C'ALL',0(R4)                                                    
         BNE   BADKEY                                                           
         B     MGRP12                                                           
         EJECT                                                                  
SDEF     MVC   SVKEY(2),=X'0D13'                                                
         MVC   SVKEY+2(2),AGYALPHA                                              
         MVI   ERRCD,1                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,0                                                         
         BE    BADKEY                                                           
         CLI   0(R4),C'B'                                                       
         BNE   SDEF1                                                            
         MVI   SVKEY+4,C'0'                                                     
         B     SDEF2                                                            
SDEF1    CLI   0(R4),C'N'                                                       
         BNE   BADKEY                                                           
         MVI   SVKEY+4,C'1'                                                     
*                                                                               
SDEF2    MVI   ERRCD,2                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,0                                                         
         BE    BADKEY                                                           
         CLC   =C'ALL',0(R4)                                                    
         BE    SDEF3                                                            
         MVC   SVKEY+5(5),=C'     '                                             
         MVC   SVKEY+5(3),0(R4)                                                 
         CLI   FLEN+1,3                                                         
         BL    BADKEY                                                           
         BE    *+10                                                             
         MVC   SVKEY+5(4),0(R4)                                                 
*                                                                               
SDEF3    MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,0                                                         
         BE    EXIT                                                             
         CLI   FLEN+1,3                                                         
         BNE   SDEF5                                                            
         CLC   =C'ALL',0(R4)                                                    
         BNE   BADKEY                                                           
         MVC   SVKEY+10(3),0(R4)                                                
         B     EXIT                                                             
SDEF5    CLI   FLEN+1,2                                                         
         BNE   BADKEY                                                           
         MVC   SVKEY+10(2),0(R4)                                                
         B     EXIT                                                             
         EJECT                                                                  
PROG     XC    SVKEY,SVKEY                                                      
         MVI   SVEBCMED,C'N'                                                    
         GOTO1 =V(MEDGET),DMCB,(SVEBCMED,AGYALPHA),VDATAMGR,WORK,      X        
               RR=RELO                                                          
         MVC   SVAGYMD,WORK                                                     
         MVC   SVKEY(2),=X'0D12'                                                
         MVC   SVKEY+2(2),AGYALPHA                                              
         MVI   ERRCD,1                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,0                                                         
         BE    BADKEY                                                           
         MVC   SVKEY+4(4),=C'    '                                              
         ZIC   RE,FLEN+1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVKEY+4(0),0(R4)                                                 
*                                                                               
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,0                                                         
         BE    EXIT                                                             
         ZIC   RE,FLEN+1                                                        
         CH    RE,=H'4'                                                         
         BNH   *+8                                                              
         LH    RE,=H'4'                                                         
         BCTR  RE,0                                                             
         MVC   SVKEY+8(4),=C'    '                                              
         EX    RE,*+8                                                           
         B     EXIT                                                             
         MVC   SVKEY+8(0),0(R4)                                                 
         EJECT                                                                  
MENU     XC    SVKEY,SVKEY         SET UP KEY FOR MENU RECORDS                  
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY(2),=X'0D26'   RECORD CODE                                  
         MVC   SVKEY+2(1),SVAGYMD  AGENCY/MEDIA                                 
         MVI   ERRCD,2                                                          
         BAS   RE,FLDVAL           SEE IF START AT MENU GIVEN                   
         CLI   FLEN+1,0                                                         
         BE    EXIT                                                             
         MVC   SVKEY+3(4),=C'    '                                              
         ZIC   RE,FLEN+1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8              SET FIRST MENU                               
         B     *+10                                                             
         MVC   SVKEY+3(0),0(R4)                                                 
         B     EXIT                                                             
         EJECT                                                                  
NDEF     XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=X'0D11'                                                
         MVC   SVKEY+2(2),AGYALPHA                                              
         MVC   SVKEY+4(4),=C'    '                                              
         MVI   ERRCD,1                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,0                                                         
         BE    BADKEY                                                           
         CLI   FLEN+1,4                                                         
         BH    BADKEY                                                           
         ZIC   RE,FLEN+1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVKEY+4(0),0(R4)                                                 
*                                                                               
         MVI   ERRCD,2                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,0                                                         
         BE    NDEF2                                                            
         CLI   FLEN+1,3                                                         
         BNE   NDEF1                                                            
         CLC   =C'ALL',0(R4)                                                    
         BNE   BADKEY                                                           
         MVC   SVKEY+8(3),0(R4)                                                 
         B     NDEF2                                                            
*                                                                               
NDEF1    CLI   FLEN+1,2                                                         
         BNE   BADKEY                                                           
         MVC   SVKEY+8(2),0(R4)                                                 
*                                                                               
NDEF2    BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,1                                                         
         BH    BADKEY                                                           
         CLI   FLEN+1,0                                                         
         BE    EXIT                                                             
         MVC   SVKEY+11(1),0(R4)                                                
         B     EXIT                                                             
         EJECT                                                                  
MGRP10   XC    FLEN,FLEN           SET TO RE-EDIT                               
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+3(2),SVCLT                                                 
         BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   SINKMSG+11(20),CNAME                                             
         DROP  R8                                                               
* EDIT PRDGRP                                                                   
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,0                                                         
         BE    MGRP12                                                           
         CLI   FLEN+1,3                                                         
         BNE   *+14                                                             
         CLC   =C'ALL',0(R4)                                                    
         BE    MGRP12                                                           
*                                                                               
         BAS   R9,GETGRP                                                        
         MVC   SVKEY+5(3),FULL                                                  
         OC    FULL+1(2),FULL+1                                                 
         BZ    BADKEY                                                           
* GET PRDGRP NAME                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+4(6),SVKEY+2    MOVE A/M/CLT/PGRP                            
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADKEY                                                           
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         USING PRGRECD,R8                                                       
         LA    R6,PRGEL                                                         
         USING PRGEL10,R6                                                       
         MVC   SINKMSG+31(13),PRGNAM1                                           
         DROP  R6,R8                                                            
         EJECT                                                                  
* EDIT MKTGRP                                                                   
MGRP12   MVI   ERRCD,4                                                          
         BAS   R9,GETGRP                                                        
         CLI   SVREC,X'17'         TEST MGRDEF                                  
         BNE   MGRP14                                                           
* MGRDEF MUST SPECIFY ID/OMIT NUMBER                                            
         CLI   FULL,0                                                           
         BE    BADKEY                                                           
         OC    FULL+1(2),FULL+1                                                 
         BNZ   BADKEY                                                           
         B     MGRPX                                                            
MGRP14   DS    0H                                                               
*                                                                               
MGRPX    MVC   SVKEY+8(3),FULL                                                  
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
AGY      MVI   SVKEY,6                                                          
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
DPT      MVI   SVKEY,8                                                          
*                                                                               
         MVI   ERRCD,2                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+3(1),SVEBCMED                                              
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
EQU      MVI   SVKEY,9                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+3(1),SVEBCMED                                              
*                                                                               
         XC    SVCLT,SVCLT                                                      
         AR    R4,R5                                                            
         CLI   0(R4),C' '                                                       
         BNH   EQUX                                                             
*                                                                               
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+4(2),SVCLT                                                 
*                                                                               
EQUX     B     EXIT                                                             
         SPACE 2                                                                
ERR      MVI   SVKEY,X'0F'                                                      
         MVI   SVKEY+1,2           SYSTEM NUMBER                                
         BAS   RE,CHKEND                                                        
         BAS   R9,GETNUM                                                        
         CP    DUB,=P'0'                                                        
         BNH   BADKEY                                                           
         CP    DUB,=P'255'                                                      
         BH    BADKEY                                                           
         STC   R0,SVKEY+2                                                       
         B     EXIT                                                             
         EJECT                                                                  
MAS      MVC   SVKEY(17),ZEROS                                                  
         MVI   SVKEY,C'S'                                                       
         BAS   R9,GETMED                                                        
         MVI   ERRCD,1                                                          
         BAS   R9,GETSTA                                                        
         MVC   SVKEY+1(1),SVEBCMED                                              
         MVC   SVKEY+2(5),SVSTA                                                 
         MVC   SVKEY+7(2),AGYALPHA                                              
* CHECK FOR OPTIONAL CLIENT CODE                                                
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    MASX                                                             
         XC    FLEN,FLEN           SET TO EDIT AGAIN                            
         BAS   R9,GETCLT                                                        
         BAS   R9,RDCLT                                                         
         MVC   SVKEY+9(3),SVEBCCLT                                              
MASX     B     CHKSTA                                                           
         EJECT                                                                  
ADDR     MVC   SVKEY,ZEROS                                                      
         MVI   SVKEY,C'A'                                                       
* FIRST CHECK FOR 'BOX' - REMITTANCE ADDRESS REC                                
         MVI   ERRCD,1                                                          
         BAS   R9,FLDVAL                                                        
         CLI   FLEN+1,4                                                         
         BNE   ADDR2                                                            
         CLC   =C'BOX',0(R4)                                                    
         BNE   ADDR2                                                            
         MVI   SVKEY+1,C'T'        FORCE MEDIA TO T                             
         MVC   SVKEY+2(4),0(R4)                                                 
         MVI   SVKEY+6,C'T'                                                     
         B     CHKSTA                                                           
         SPACE 2                                                                
ADDR2    XC    FLEN,FLEN           SET TO RE-EDIT                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETSTA                                                        
*                                                                               
         MVC   SVKEY+1(1),SVEBCMED                                              
         MVC   SVKEY+2(5),SVSTA                                                 
         MVC   SVKEY+7(2),AGYALPHA                                              
*                                                                               
         MVI   ERRCD,3                                                          
         LA    RE,0(R4,R5)         POINT TO STOP CHAR                           
         CLI   0(RE),C' '          IT SHOULD BE X'00' OR C' '                   
         BH    BADKEY                                                           
         B     CHKSTA                                                           
         EJECT                                                                  
REP      XC    SVKEY,SVKEY                                                      
         MVI   SVKEY,C'R'                                                       
*                                                                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+1(1),SVEBCMED                                              
*                                                                               
         MVI   ERRCD,2             EDIT START REP                               
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    CHKSTA                                                           
         CH    R5,=H'3'                                                         
         BH    BADKEY                                                           
         BCTR  R5,0                                                             
         EX    R5,MVREP                                                         
         MVC   SVKEY+5(2),AGYALPHA                                              
         B     CHKSTA                                                           
MVREP    MVC   SVKEY+2(0),0(R4)                                                 
         SPACE 2                                                                
MKT      MVC   SVKEY,ZEROS                                                      
         MVI   SVKEY,C'M'                                                       
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+1(1),SVEBCMED                                              
         MVC   SVKEY+6(2),AGYALPHA                                              
         MVI   ERRCD,2                                                          
         BAS   R9,GETNUM                                                        
         UNPK  SVKEY+2(4),DUB                                                   
         B     CHKSTA                                                           
         EJECT                                                                  
CHKSTA   DS    0H                                                               
* ALL KEY DATA SHOULD BE PROCESSED                                              
         MVI   ERRCD,KEYLNERR                                                   
         L     R4,FADDR                                                         
         AH    R4,FLEN                                                          
         CLI   0(R4),C' '                                                       
         BH    KEYERR                                                           
         OI    SINIKEYH+4,X'20'     SET KEY VALID                               
         B     EXIT                                                             
         EJECT                                                                  
GETMED   BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,1                                                         
         BNE   BADKEY                                                           
         MVC   SVEBCMED,0(R4)                                                   
         CLI   SVEBCMED,C'T'                                                    
         BE    GETMEDA                                                          
         CLI   SVEBCMED,C'R'                                                    
         BE    GETMEDA                                                          
         CLI   SVEBCMED,C'N'                                                    
         BE    GETMEDA                                                          
         CLI   SVEBCMED,C'C'                                                    
         BE    GETMEDA                                                          
         CLI   SVEBCMED,C'X'                                                    
         BE    GETMEDA                                                          
         B     BADKEY                                                           
GETMEDA  GOTO1 =V(MEDGET),DMCB,(SVEBCMED,AGYALPHA),VDATAMGR,WORK,      X        
               RR=RELO                                                          
         CLI   8(R1),X'FF'                                                      
         BE    BADKEY                                                           
         MVC   SVAGYMD,WORK                                                     
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),AGYALPHA                                                
         GOTO1 READ                                                             
         LA    RF,REC                                                           
         ST    RF,AREC                                                          
         GOTO1 GETREC                                                           
         MVC   SVCNTRY,AGYPROF+7-AGYHDR+REC                                     
         BR    R9                                                               
         SPACE 2                                                                
GETCLT   CLI   SVREC,X'11'                                                      
         BE    CLTOPT                                                           
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,2                                                         
         BL    BADKEY                                                           
         CLI   FLEN+1,3                                                         
         BH    BADKEY                                                           
         MVC   SVEBCCLT,SPACES                                                  
         BCTR  R5,0                                                             
         EX    R5,MVCLT                                                         
*                                                                               
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),SVEBCCLT,SVCLT                                         
         BR    R9                                                               
*                                                                               
CLTOPT   MVC   SVEBCCLT,SPACES                                                  
         BAS   RE,FLDVAL           NO - GET START AT CLIENT                     
         LTR   R5,R5                                                            
         BNZ   CLTOPT2                                                          
         OC    T21AFFD+6(2),T21AFFD+6  TEST ANY LIMIT ACCESS                    
         BZ    EXIT                NO                                           
         MVI   ERRCD,SCRTYERR      ELSE NEED A VALID CLIENT                     
         GOTO1 ERROR                                                            
*                                                                               
CLTOPT2  BCTR  R5,0                                                             
         EX    R5,MVCLT                                                         
         OC    SVEBCCLT,=C'   '                                                 
* FIX THE CLPACK PROBLEM THAT C' ' IS HIGHER THAN C'A'                          
         MVC   DUB(3),SVEBCCLT                                                  
         CLI   DUB+1,C' '                                                       
         BH    *+8                                                              
         MVI   DUB+1,C'A'                                                       
         CLI   DUB+2,C' '                                                       
         BH    *+8                                                              
         MVI   DUB+2,C'A'                                                       
*                                                                               
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),DUB,SVCLT                                              
*                                                                               
         BAS   RE,CALLOFCR         MAKE SURE HAVE ACCESS                        
         B     EXIT                                                             
*                                                                               
MVCLT    MVC   SVEBCCLT(0),0(R4)                                                
         SPACE 2                                                                
GETPRD   CLI   SVREC,X'12'                                                      
         BE    PRDOPT                                                           
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,2                                                         
         BL    BADKEY                                                           
         CLI   FLEN+1,3                                                         
         BH    BADKEY                                                           
         MVC   SVEBCPRD,SPACES                                                  
         BCTR  R5,0                                                             
         EX    R5,MVPRD                                                         
         BR    R9                                                               
MVPRD    MVC   SVEBCPRD(0),0(R4)                                                
*                                                                               
PRDOPT   MVC   SVEBCPRD(0),SPACES                                               
         BAS   RE,CHKEND           END                                          
         BAS   RE,FLDVAL            NO - GET START AT PRODUCT                   
         LTR   R5,R5                                                            
         BZ    EXIT                                                             
         CH    R5,=H'4'                                                         
         BL    *+8                                                              
         LA    R5,3                                                             
         BCTR  R5,0                                                             
         EX    R5,MVPRD                                                         
         B     EXIT                                                             
         EJECT                                                                  
GETNUM   BAS   RE,FLDVAL                                                        
         MVC   DUB,=PL8'0'                                                      
         LTR   R5,R5               TEST DATA                                    
         BZR   R9                                                               
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    BADKEY                                                           
         CLI   FLEN+1,4            MAX LEN 4                                    
         BH    BADKEY                                                           
         BCTR  R5,0                                                             
         EX    R5,NUMPACK                                                       
         CVB   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         BR    R9                                                               
NUMPACK  PACK  DUB,0(0,R4)                                                      
         SPACE 2                                                                
* CHECK FOR END OF INPUT KEY                                                    
*                                                                               
CHKEND   L     R4,FADDR                                                         
         LH    R5,FLEN                                                          
         LTR   R5,R5                                                            
         BZ    *+8                                                              
         LA    R5,1(R5)                                                         
         AR    R4,R5                                                            
         CLI   0(R4),0                                                          
         BE    EXIT                                                             
         BR    RE                                                               
         EJECT                                                                  
GETSTA   BAS   RE,FLDVAL                                                        
         MVC   SVSTA,=C'     '                                                  
         CLI   FLEN+1,0                                                         
         BER   R9                                                               
         CLI   FLEN+1,4                                                         
         BH    BADKEY                                                           
         BCTR  R5,0                                                             
         EX    R5,MVSTA                                                         
         BR    R9                                                               
*                                                                               
MVSTA    MVC   SVSTA(0),0(R4)                                                   
STAAM    CLC   0(0,R4),=C'AM'                                                   
STAFM    CLC   0(0,R4),=C'FM'                                                   
STATV    CLC   0(0,R4),=C'TV'                                                   
         SPACE 2                                                                
GETAGY   BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,2                                                         
         BNE   BADKEY                                                           
         MVC   DUB(2),0(R4)                                                     
         CLC   DUB(2),ZEROS                                                     
         BE    GETAGYX                                                          
         CLI   DUB,C'A'                                                         
         BL    BADKEY                                                           
         CLI   DUB,C'Z'                                                         
         BH    BADKEY                                                           
GETAGYX  BR    R9                                                               
         EJECT                                                                  
* EDIT GROUP CODE - FORMAT IS A999                                              
*                                                                               
GETGRP   XC    FULL,FULL                                                        
         BAS   RE,FLDVAL                                                        
         CLC   0(3,R4),=C'ALL'                                                  
         BER   R9                                                               
         LTR   R5,R5                                                            
         BZ    GETGRPX                                                          
         CLI   0(R4),C'A'                                                       
         BL    BADKEY                                                           
         CLI   0(R4),C'Z'                                                       
         BH    BADKEY                                                           
         MVC   FULL(1),0(R4)       MOVE GROUP ID                                
*                                                                               
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BZ    GETGRPX                                                          
*                                                                               
         CLI   FLEN+1,5                                                         
         BH    BADKEY                                                           
         STM   R4,R5,WORK                                                       
         XC    DMCB(4),DMCB                                                     
         LA    RE,DMCB                                                          
GETGRP2  CLI   0(R4),C'0'                                                       
         BL    BADKEY                                                           
         CLI   0(R4),C'9'                                                       
         BH    BADKEY                                                           
         MVC   0(1,RE),0(R4)                                                    
         LA    RE,1(RE)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,GETGRP2                                                       
         LM    R4,R5,WORK                                                       
         EX    R5,NUMPACK          PACK 1 EXTRA BYTE                            
         PACK  DUB,DMCB(5)                                                      
         MVC   FULL+1(2),DUB+5                                                  
         NI    FULL+3,X'F0'                                                     
*                                                                               
GETGRPX  BR    R9                                                               
         EJECT                                                                  
RDCLT    XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERRCD,NOCLTERR                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   KEYERR                                                           
         LA    R8,REC                                                           
         USING CLTHDRD,R8                                                       
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
RDCLT10  MVC   SVCLPROF,CPROF                                                   
         MVC   SVCLEX,CEXTRA                                                    
         MVC   SVCOFF,COFFICE                                                   
         XC    DUB,DUB                                                          
         MVC   DUB(3),CACCESS                                                   
*                                                                               
         CLI   T21AFFD+6,C'+'      TEST MARKET LIMIT                            
         BE    *+8                                                              
         BAS   RE,CALLOFCR                                                      
*                                                                               
         MVC   SVCLTDA,KEY+14                                                   
         BR    R9                                                               
         DROP  R8                                                               
         EJECT                                                                  
*=============================================================                  
* CALL NEW OFFICER                                                              
*=============================================================                  
                                                                                
CALLOFCR NTR1                                                                   
         LA    R5,REC2                                                          
         A     R5,=F'2000'                                                      
         PRINT GEN                                                              
         GOTO1 INITSEC,DMCB,((R5))                                              
         PRINT NOGEN                                                            
* READ CLIENT RECORD                                                            
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING CLTHDR,R5                                                        
         MVC   CKEYAM,SVAGYMD                                                   
         MVC   CKEYCLT,SVCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CALLOFEX                                                         
         LA    R5,REC                                                           
         ST    R5,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R5,REC2                                                          
         A     R5,=F'2000'                                                      
         GOTO1 CHKSEC,DMCB,((R5)),REC,SVEBCCLT                                  
CALLOFEX B     EXIT                                                             
         DROP  R5                                                               
**********************************************                                  
***      LA    R1,WORK                                                          
***      XC    WORK,WORK                                                        
***      USING OFFICED,R1                                                       
*                                                                               
***      MVI   OFCSYS,C'S'                                                      
***      MVC   OFCAUTH,T21AFFD+6   NOTE MOVES 2 BYTES                           
***      MVC   OFCAGY,AGYALPHA                                                  
***      MVC   OFCOFC,SVCOFF                                                    
***      MVC   OFCCLT,SVEBCCLT     UNPACKED CLIENT                              
***      MVC   OFCSAGMD,SVAGYMD                                                 
***      MVC   OFCLMT,T21AFFD+6    NOTE MOVES 4 BYTES                           
***      MVC   OFCACCSC(3),DUB                                                  
***      DROP  R1                                                               
*                                                                               
***      MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
***      GOTO1 VCALLOV,DMCB,0                                                   
*                                                                               
***      L     RF,DMCB                                                          
***      GOTO1 (RF),DMCB,(C'N',WORK),VCOMFACS                                   
***      CLI   0(R1),0                                                          
***      BE    EXIT                EXIT WITH CC NEQ                             
*                                                                               
***      MVI   ERRCD,SCRTYERR                                                   
***      GOTO1 ERROR                                                            
***      DROP  R8                                                               
         EJECT                                                                  
RDPRD    XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),SVEBCPRD                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERRCD,NOPRDERR                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   KEYERR                                                           
*                                                                               
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVPRDDA,KEY+14                                                   
         BR    R9                                                               
         EJECT                                                                  
* VALIDATE FIELD FOR ALPHA/NUMERIC.                                             
* FIELDS ARE DELIMITED BY BLANK, COMMA, OR DASH.                                
*  FADDR  HAS PREVIOUS FIELD ADDRESS.                                           
*  FLEN   HAS PREVIOUS FIELD LENGTH.                                            
*  FVAL   HAS VALIDITY BITS (X'04'=VALID ALPHA,X'08'=VALID NUMERIC)             
*                                                                               
* ON EXIT, R4 HAS FIELD ADDRESS, R5 HAS FIELD LENGTH                            
FLDVAL   DS    0H                                                               
         MVI   FVAL,X'0C'          SET ALL VALID                                
         L     R4,FADDR                                                         
         LH    R5,FLEN                                                          
         LTR   R5,R5                                                            
         BZ    *+8                                                              
         LA    R5,1(R5)                                                         
         AR    R4,R5                                                            
         ST    R4,FADDR                                                         
         SR    R0,R0               TEST STILL IN FIELD                          
         IC    R0,0(R2)                                                         
         AR    R0,R2               R0 HAS FLDHDR END+1                          
         CR    R4,R0                                                            
         BL    FLDVAL2                                                          
         LA    R5,0                                                             
         STH   R5,FLEN                                                          
         BR    RE                                                               
FLDVAL2  CLI   0(R4),C' '                                                       
         BE    FLDVALX                                                          
         CLI   0(R4),0                                                          
         BE    FLDVALX                                                          
         CLI   0(R4),C','                                                       
         BE    FLDVALX                                                          
         CLI   0(R4),C'-'                                                       
         BE    FLDVALX                                                          
         CLI   0(R4),C'A'                                                       
         BL    FLDVAL4                                                          
         CLI   0(R4),C'Z'                                                       
         BNH   FLDVAL6                                                          
FLDVAL4  NI    FVAL,X'FB'          FIELD NOT ALPHA                              
         CLI   0(R4),C'0'                                                       
         BL    FLDVAL6                                                          
         CLI   0(R4),C'9'                                                       
         BNH   FLDVAL8                                                          
FLDVAL6  NI    FVAL,X'F7'          FIELD NOT NUMERIC                            
*                                                                               
FLDVAL8  LA    R4,1(R4)                                                         
         CR    R4,R0               TEST STILL IN FIELD                          
         BNL   FLDVALX                                                          
         B     FLDVAL2                                                          
*                                                                               
FLDVALX  LR    R5,R4                                                            
         S     R5,FADDR            END-START GIVES LENGTH                       
         STH   R5,FLEN                                                          
         L     R4,FADDR                                                         
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
BADKEY   DS    0H                                                               
         MVC   SINMSG(35),=C'** ERROR * KEY FIELD 9 NOT VALID **'               
         OI    ERRCD,X'F0'                                                      
         MVC   SINMSG+21(1),ERRCD                                               
*                                                                               
         OI    6(R2),X'C0'         POSITION CURSOR                              
         B     EXIT                                                             
*                                                                               
KEYERR   GOTO1 ERROR                                                            
*                                                                               
ZEROS    DC    20C'0'                                                           
         LTORG                                                                  
         EJECT                                                                  
         CNOP  2,4                                                              
KEYTAB   DC    H'4'                                                             
         DC    A(KEYTABX-1)                                                     
*                                                                               
         DC    X'11',AL3(CLT)      CLT                                          
         DC    X'12',AL3(CLT)      PRD                                          
         DC    X'13',AL3(CLT)      EST                                          
         DC    X'18',AL3(CLT)      BILL                                         
*                                                                               
         DC    X'1D',AL3(COM)      COMMENTS                                     
         DC    X'1F',AL3(COM)      STAT DESC                                    
*                                                                               
         DC    X'21',AL3(MAS)      MAS                                          
         DC    X'22',AL3(ADDR)     ADDR                                         
         DC    X'23',AL3(REP)      REP                                          
         DC    X'24',AL3(MKT)      MKT                                          
*                                                                               
         DC    X'32',AL3(AGY)      AGY                                          
         DC    X'34',AL3(DPT)      DPT                                          
         DC    X'35',AL3(EQU)      EQU                                          
*                                                                               
         DC    X'14',AL3(PGRP)     PGRDEF                                       
         DC    X'15',AL3(PGRP)     PGROUP                                       
         DC    X'16',AL3(MGRP)     MGRDEF                                       
         DC    X'17',AL3(MGRP)     MGROUP                                       
*                                                                               
         DC    X'44',AL3(MENU)     DEMO MENU                                    
         DC    X'45',AL3(MKT)      MKT                                          
         DC    X'46',AL3(MAS)      MAS                                          
         DC    X'47',AL3(REP)      REP                                          
KEYTABX  EQU   *                                                                
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
* SPGENAGY                                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
* SPGENCLT                                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
* SPGENPRD                                                                      
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
* SPGENPRG                                                                      
       ++INCLUDE SPGENPRG                                                       
*                                                                               
       ++INCLUDE DDOFFICED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPINF01   05/08/06'                                      
         END                                                                    
