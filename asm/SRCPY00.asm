*          DATA SET SRCPY00    AT LEVEL 010 AS OF 05/28/08                      
*PHASE T14300A                                                                  
*INCLUDE TWANG                                                                  
         TITLE '$COPY - COPY TWA0 TO PRTQUE'                                    
         PRINT NOGEN                                                            
COPY     CSECT                                                                  
         NMODL WRKX-WRKD,**$COPY*                                               
         USING WRKD,RC             RC=A(W/S)                                    
         ST    R1,APARM                                                         
         USING SRPARMD,R1                                                       
         L     RA,SRPARM6          RA=A(TWA)                                    
         L     R3,SRPARM2          R3=A(TIA)                                    
         USING SRCPYFFD,RA                                                      
         L     R9,SRPARM4          R9=A(COMFACS)                                
         ST    R9,ACOM                                                          
         USING COMFACSD,R9                                                      
         L     R8,SRPARM1          R8=A(SYSFACS)                                
         USING SYSFACD,R8                                                       
         L     RE,VSSB                                                          
         MVC   RECLEN,SSBTWAL-SSBD(RE)                                          
         L     R4,SRPARM3          R4=A(UTL)                                    
         USING UTLD,R4                                                          
         MVC   TERMINAL,TNUM       SAVE TNUM FOR CALLER                         
         ST    R4,AUTL                                                          
         MVI   DDS,0               SET DDS INDIC                                
         TM    TSTAT,X'60'                                                      
         BZ    *+8                                                              
         MVI   DDS,1                                                            
         XC    FILLS,FILLS                                                      
         GOTO1 CDICTATE,DMCB,C'LU  ',DDDCLST,DDDSLST                            
         EJECT                                                                  
*                                  VALIDATE P1 (TERMINAL)                       
VALP1    LA    R2,SRVP1H                                                        
         CLI   DDS,1               NOT VALID FOR NON-DDS TERMS                  
         BNE   READTWA                                                          
         CLI   5(R2),0                                                          
         BE    VALP2                                                            
*                                                                               
         GOTO1 CTERMVAL,DMCB,(R2)                                               
         MVI   ERRNUM,1                                                         
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    ERROR                                                            
         MVC   AUTL,DMCB+4         SET A(UTL)                                   
*                                  VALIDATE P2 (FILL CHRS)                      
VALP2    CLC   SRVP2(5),=C'FILL='                                               
         BNE   READTWA                                                          
         MVC   FILLS,SRVP2+5                                                    
*                                                                               
READTWA  L     R4,AUTL             R4=A(SOURCE TERM UTL ENTRY)                  
         MVI   ERRNUM,2                                                         
         OC    TUSER,TUSER         MUST BE CONNECTED                            
         BZ    ERROR                                                            
         MVI   ERRNUM,6                                                         
         OC    TPRNT,TPRNT         MUST BE A TERMINAL                           
         BNZ   ERROR                                                            
*                                                                               
         XC    DMCB+8(4),DMCB+8    READ TWA 0                                   
         MVC   DMCB+10(2),TNUM     SET TERM NUMBER                              
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,TWA0                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT READ                             
         LR    R0,R3               R0=TIA                                       
         LA    RE,TWA0             RE=TWA FROM TEMPSTR0                         
         LH    R1,RECLEN                                                        
         LR    RF,R1               MOVE TWA0 TO TIA                             
         MVCL  R0,RE                                                            
         LR    R0,R3               SAVE A(GLOBALS)                              
         AH    R0,=Y(CHKPTGLD)                                                  
         ST    R0,AGLOBALS                                                      
*                                                                               
CHKTWA   MVI   ERRNUM,6            CHECK NON OLAY SCREENS                       
         TM    TWA0+65,X'20'                                                    
         BZ    ERROR               FIRST FIELD MUST BE PROTECTED                
         CLI   TWA0+64,X'44'                                                    
         BNE   *+12                                                             
         LA    RE,TWA0+132         AND 60 BYTES LONG                            
         B     CHKTWA1                                                          
         CLI   TWA0+64,X'4C'                                                    
         BNE   ERROR                                                            
         LA    RE,TWA0+140                                                      
CHKTWA1  CLI   0(RE),X'19'         SECOND FIELD MUST BE 17 BYTES LONG           
         BE    *+12                                                             
         CLI   0(RE),X'21'                                                      
         BNE   ERROR                                                            
*                                                                               
         MVC   DMCB+8(2),=C'PU'                                                 
         MVC   DMCB+10(2),FILLS                                                 
         CLI   DMCB+10,C' '                                                     
         BNE   *+8                                                              
         MVI   DMCB+10,0                                                        
         CLI   DMCB+11,C' '                                                     
         BNE   *+8                                                              
         MVI   DMCB+11,0                                                        
*                                  TRANSLATE TWA                                
         GOTO1 =V(TWANG),DMCB,TWA0,TBLOCK,RR=RB                                 
         EJECT                                                                  
SETATT   MVC   SUBID,SR3TRM        SET DEFAULT SUBID                            
         MVI   CLASS,C'S'          SET DEFAULT CLASS                            
         MVC   LIVEHRS,=H'8'       SET DEFAULT LIVE RETAIN                      
         MVC   DEADHRS,=H'1'       SET DEFAULT DEAD RETAIN                      
         MVC   DESC,SPACES         SET DEFAULT DESCRIPTION                      
         MVC   SPPFF,=C'DSR  '     SET DEFAULT APPFF (MAKER)                    
         MVC   DESC(4),SR4TRM                                                   
         L     R4,AUTL                                                          
         LH    R0,TNUM                                                          
         CVD   R0,DUB                                                           
         UNPK  DESC+4(4),DUB                                                    
         OI    DESC+7,X'F0'                                                     
*                                                                               
         LA    RE,SRVSREQ+1        POINT TO S/R FIELD                           
         LA    RF,L'SRVSREQ-5      I AM PREPARED TO SCAN THIS FAR               
*                                                                               
SETA1    CLI   0(RE),C','          FIND S/R DELIMITER                           
         BE    SETA1A                                                           
         CLI   0(RE),C'='                                                       
         BE    SETA1A                                                           
         CLI   0(RE),C' '                                                       
         BE    SETA1X                                                           
         CLI   0(RE),X'00'                                                      
         BE    SETA1X                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,SETA1                                                         
         B     SETA1X                                                           
SETA1A   LA    RE,1(RE)            FIND 1ST NONBLANK CHR AFTER COMMA            
         CLI   0(RE),C' '                                                       
         BNE   *+12                                                             
         BCT   RF,SETA1A                                                        
         B     SETA1X                                                           
         LR    RF,RE               RE=A(START OF STRING)                        
         LA    R0,4                                                             
SETA1B   CLI   0(RF),C' '          FIND 1ST BLANK AFTER STRING                  
         BE    SETA1C                                                           
         CLI   0(RF),X'00'                                                      
         BE    SETA1C                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,SETA1B                                                        
         B     SETA1X                                                           
SETA1C   SR    RF,RE               RF=L'STRING                                  
         CH    RF,=H'2'            ALLOW TWO OR THREE CHR INITIALS              
         BL    SETA1X                                                           
         BCTR  RF,0                                                             
         MVC   DUB(3),SPACES                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),0(RE)                                                     
         CLC   DUB(3),=C'ALL'                                                   
         BE    SETA1X                                                           
         MVC   SUBID,DUB           SET SUBID TO INITIALS                        
SETA1X   EQU   *                                                                
*                                                                               
SETA2    SR    RE,RE               GET SYSTEM/PROGRAM NAMES                     
         ICM   RE,7,TASYS                                                       
         BZ    SETA2X                                                           
         MVC   DUB(3),SENAME-SELISTD(RE)                                        
         ICM   RE,7,TAPRG                                                       
         BZ    SETA2X                                                           
*                                                                               
         L     R9,VSSB                                                          
         TM    SSBSTAT4-SSBD(R9),SSBPGDSP                                       
         BZ    SETA2A                                                           
         XR    R9,R9                                                            
         ICM   R9,7,TARSYS                                                      
         ICM   R9,15,SEPGMS-SELISTD(R9)                                         
         AR    RE,R9                                                            
*                                                                               
SETA2A   L     R9,ACOM             RESTORE COMFACS                              
         MVC   DUB+4(3),PGMNAME-PGMLSTD(RE)                                     
         MVC   DESC,SPACES                                                      
         MVC   DESC(3),SR3SCRN                                                  
         MVI   DESC+3,C'='                                                      
         MVC   DESC+4(3),DUB                                                    
         MVI   DESC+7,C'/'                                                      
         MVC   DESC+8(3),DUB+4                                                  
*                                                                               
SETA2X   EQU   *                                                                
         EJECT                                                                  
         MVI   PQCTL,0             INITIALIZE                                   
         BAS   RE,PRINT                                                         
         BNE   ERROR                                                            
         MVI   PQCTL,1             START                                        
         LA    R6,=80CL1'*'                                                     
         BAS   RE,PRINT                                                         
         BNE   ERROR                                                            
         LA    R6,TBLOCK           SCREEN                                       
         LA    R7,24                                                            
         BAS   RE,PRINT                                                         
         BNE   ERROR                                                            
         LA    R6,80(R6)                                                        
         BCT   R7,*-12                                                          
         LA    R6,=80CL1'*'        END                                          
         BAS   RE,PRINT                                                         
         BNE   ERROR                                                            
         MVI   PQCTL,X'FF'         TERMINATION                                  
         BAS   RE,PRINT                                                         
         BNE   ERROR                                                            
         MVI   ERRNUM,0                                                         
         LA    R2,SRVSREQH                                                      
         B     ERROR                                                            
         EJECT                                                                  
*OUTPUT MESSAGE & EXIT                                                          
*                                                                               
ERROR    SR    RE,RE               ERRNUM IS ZERO FOR OK ELSE ERROR             
         ICM   RE,1,ERRNUM                                                      
         SLL   RE,2                                                             
         EX    0,ERROR2(RE)                                                     
         B     ERRORX                                                           
*                                                                               
ERROR2   B     INFO1               ERR 0 IS NOT AN ERROR                        
         LA    RE,182              INVALID TERMINAL                             
         LA    RE,183              TERMINAL NOT CONNECTED                       
         LA    RE,184              END OF FILE ON PRINT QUEUE                   
         LA    RE,185              FORMAT ERROR ON PRINT QUEUE                  
         LA    RE,186              DISK ERROR ON PRINT QUEUE                    
         LA    RE,187              CANT COPY UNFORMATTED SCREENS                
*                                                                               
ERRORX   GOTO1 CGETTXT,DMCB,(RE),0,(C'E',0),0,0,X'010000'                       
         B     ERROR3                                                           
INFO1    L     R4,AUTL                                                          
         CLC   TERMINAL,TNUM       IS THIS MY SCREEN                            
         BNE   INFO2                                                            
         LR    R0,RA               R0=TWA                                       
         LR    RE,R3               RE=TWA FROM TIA                              
         LH    R1,RECLEN                                                        
         LR    RF,R1               MOVE TIA TO TWA                              
         MVCL  R0,RE                                                            
*                                                                               
INFO2    XC    MSG,MSG                                                          
         MVC   MSG+1(3),SUBID      OUTPUT REPORT SUB ID                         
         MVI   MSG+4,C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,REPNO                                                       
         EDIT  (R0),(5,MSG+5),ALIGN=LEFT                                        
         LA    RF,5                                                             
         AR    RF,R0                                                            
         STC   RF,MSG                                                           
         BCTR  RF,0                                                             
*                                                                               
         GOTO1 CGLOBBER,DMCB,(X'80',PUTD),MSG+1,(RF),1,(X'80',AGLOBALS)         
*                                                                               
         XC    DMCB+8(4),DMCB+8    WRITE TWA 0                                  
         MVC   DMCB+10(2),TNUM     SET TERM NUMBER                              
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R3)                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT READ                             
*                                                                               
         MVCDD MSG1,SR#QUEUE                                                    
         MVI   FULL,4                                                           
*                                                                               
         CLI   DDS,0                                                            
         BE    MSGOUT                                                           
         MVC   MSG1(6),PRTQID                                                   
         MVI   FULL,6                                                           
MSGOUT   GOTO1 CGETTXT,DMCB,176,0,(C'I',0),(FULL,MSG1),MSG,X'010000'            
ERROR3   NI    SRVSREQH+6,X'BF'    UNSET & INSERT CURSOR                        
         OI    6(R2),X'40'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
* PQCTL SET TO X'00' TO INITIALIZE A REPORT                                     
*              X'01' TO PRINT A LINE (R6=A(DATA))                               
*              X'FF' TO TERMINATE A REPORT.                                     
* ON EXIT CC=NEQ ON ERROR WITH ERRNUM SET TO ERROR.                             
*                                                                               
PRINT    NTR1                                                                   
         MVI   ERRNUM,0                                                         
         XC    PQLINE,PQLINE       CLEAR PRINT LINE                             
         CLI   PQCTL,0                                                          
         BNE   PRT2                                                             
*                                                                               
PRT1     LA    R5,PQLINE           SET REPORT ATTRIBUTES                        
         USING PQPLD,R5                                                         
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLSUBID,SUBID                                                    
         MVC   QLCLASS,CLASS                                                    
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,LIVEHRS                                                  
         MVC   QLRETND,DEADHRS                                                  
         MVC   QLDESC,DESC                                                      
         MVC   QLMAKER,SPPFF                                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,PQLINE,TWA0               
         CLI   DMCB+8,0                                                         
         BNE   PCHECK                                                           
         MVC   REPNO,QLREPRNO      EXTRACT REPORT NUMBER                        
         MVC   SUBID,QLSUBID       EXTRACT REPORT SUBID                         
         MVC   PRTQID,=C'PRTQ  '                                                
         MVC   PRTQID+4(1),QLREPCHR   PRTQ FILEID NEW LOCATION                  
         CLI   PRTQID+4,C'A'                                                    
         BNL   *+10                                                             
         MVC   PRTQID+4(1),QLREPRCI-1 PRTQ FILEID OLD LOCATION                  
         CLI   PRTQID+4,C'A'                                                    
         BNL   *+8                                                              
         MVI   PRTQID+4,C' '       UNKNOWN PRTQ FILE                            
*                                                                               
         MVC   PQLINE,SPACES                                                    
         MVI   PQLINE,X'8B'                                                     
         B     PRT4                                                             
*                                                                               
PRT2     CLI   PQCTL,1                                                          
         BNE   PRT6                                                             
         MVC   PQLINE,SPACES                                                    
         MVI   PQLINE,X'09'                                                     
         MVC   PQLINE+1(80),0(R6)                                               
         B     PRT4                                                             
*                                                                               
PRT4     L     RF,VDATAMGR                                                      
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         B     PCHECK                                                           
*                                                                               
PRT6     MVI   PQLINE,X'FF'                                                     
         B     PRT4                                                             
*                                  TEST FOR ERRORS & SET ERRNUM                 
PCHECK   CLI   DMCB+8,0                                                         
         BE    PXIT                                                             
         MVI   ERRNUM,3                                                         
         TM    DMCB+8,X'80'        E-O-F ERROR                                  
         BO    PXIT                                                             
         MVI   ERRNUM,4                                                         
         TM    DMCB+8,X'01'        FORMAT ERROR                                 
         BO    PXIT                                                             
         MVI   ERRNUM,5                                                         
         TM    DMCB+8,X'40'        DISK ERROR                                   
         BO    PXIT                                                             
         DC    H'0'                                                             
*                                                                               
PXIT     CLI   ERRNUM,0            SET CONDITION CODE                           
         XIT1                                                                   
         EJECT                                                                  
DDDCLST  DS    0C                                                               
         DCDDL SR#TRM,3,L,LABEL=SR3TRM                                          
         DCDDL SR#SCRN,3,L,LABEL=SR3SCRN                                        
         DCDDL SR#TRM,4,L,LABEL=SR4TRM                                          
         LTORG                                                                  
         SPACE 1                                                                
PUTD     DC    CL8'PUTD'                                                        
SPACES   DC    CL134' '                                                         
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
APARM    DS    F                                                                
ACOM     DS    F                                                                
AUTL     DS    F                                                                
AGLOBALS DS    A                                                                
FILLS    DS    CL2                                                              
TERMINAL DS    H                                                                
RECLEN   DS    H                                                                
DDS      DS    C                                                                
ERRNUM   DS    C                                                                
*                                                                               
LIVEHRS  DS    XL2                                                              
DEADHRS  DS    XL2                                                              
REPNO    DS    XL2                                                              
SUBID    DS    CL3                                                              
CLASS    DS    CL1                                                              
DESC     DS    CL11                                                             
SPPFF    DS    CL5                                                              
PRTQID   DS    CL6                                                              
WORK     DS    CL20                                                             
MSG      DS    CL40                                                             
MSG1     DS    CL40                                                             
*                                                                               
PQCTL    DS    C                                                                
PQLINE   DS    CL133                                                            
DDDSLST  DS    0C                                                               
         DSDDL PRINT=YES                                                        
TBLOCK   DS    1920C                                                            
TWA0     DS    18432C                                                           
WRKX     DS    0C                                                               
         EJECT                                                                  
*DMPRTQL                                                                        
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
SRCPYFFD DSECT                                                                  
         DS    CL64                                                             
*SRCPYFFD                                                                       
       ++INCLUDE SRCPYFFD                                                       
         SPACE 1                                                                
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
*FADCHKPT                                                                       
       ++INCLUDE FACHKPT                                                        
         SPACE 1                                                                
*SRDDEQUS                                                                       
       ++INCLUDE SRDDEQUS                                                       
         SPACE 1                                                                
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SRCPY00   05/28/08'                                      
         END                                                                    
