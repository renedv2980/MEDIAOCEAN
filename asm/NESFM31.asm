*          DATA SET NESFM31    AT LEVEL 055 AS OF 07/17/12                      
*PHASE T31C31A,+0,NOAUTO                                                        
***********************************************************************         
*                                                                               
*  TITLE: T31C31 - MAINTENANCE/LIST OF CLIENT GROUP DEF (CGRDEF)                
*                                                                               
*  COMMENTS: MAINTAINS CLIENT GROUP DEFINITIONS                                 
*                                                                               
*  CALLED FROM: NET SFM CONTROLLER (T31C00), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS NESFMDA (T31CDA) -- MAINTENANCE                              
*                  NESFMDB (T31CDB) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW BUYERS                                               
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - WORK                                                            
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - WORK                                                            
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM                                                          
*          RF - SYSTEM                                                          
*                                                                               
***********************************************************************         
         TITLE 'NESFM31 - CLIENT GROUP DEFINITIONS'                             
T31C31   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NETCGD                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         BAS   RE,INIT                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE RECORDS                               
         BE    DELERR                                                           
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                  VALIDATE KEY                                 
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=X'0D06'                                                
         LA    R3,SVKEY                                                         
         USING CLGRECD,R3                                                       
*                                                                               
         MVI   NOPTFLG,0                                                        
*                                                                               
VK10     LA    R2,SFMMEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   CLGKAGMD,BAGYMD                                                  
*                                                                               
         CLI   ACTNUM,ACTLIST      IF LIST                                      
         BNE   VK20                                                             
         MVI   NOPTFLG,1           SET OPTIONAL FLAG                            
*                                                                               
VK20     LA    R2,SFMIDH                                                        
         GOTO1 VALIFLD                                                          
         BZ    VKX                                                              
*                                                                               
         NI    MYFLAG,X'FF'-ID2                                                 
         CLI   9(R2),C' '          CHECK IF 1 CHARACTER ID                      
         BE    VK25                                                             
         CLI   9(R2),C'A'                                                       
         BL    VK25                                                             
         CLI   9(R2),C'Z'                                                       
         BH    VK25                                                             
         OI    MYFLAG,ID2                                                       
*                                                                               
VK25     MVC   DUB(2),8(R2)                                                     
         OI    DUB+1,C' '                                                       
         BAS   RE,TRANS21          GET 1 BYTE EQUATE                            
         MVC   CLGKID,BYTE                                                      
         MVC   FILTID,BYTE                                                      
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         MVC   DELKEY,CLGRECD                                                   
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
TRANS21  DS    0H                                                               
         MVI   BYTE,0                                                           
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
TRANS21A CLC   DUB(2),0(R1)                                                     
         BNE   *+12                                                             
         MVC   BYTE,2(R1)                                                       
         BR    RE                                                               
*                                                                               
         LA    R1,3(R1)                                                         
         BCT   R0,TRANS21A                                                      
         B     INVID                                                            
*                                                                               
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                  DISPLAY KEY                                  
         MVC   PREVKEY,KEY                                                      
         MVI   PREVFLAG,1                                                       
         L     R3,AIO                                                           
         USING CLGRECD,R3                                                       
         MVI   SFMMED,C'N'                                                      
         OI    SFMMEDH+6,X'80'     XMIT                                         
*                                                                               
         LA    R2,SFMIDH                                                        
         MVC   BYTE,CLGKID                                                      
         BAS   RE,TRANS12                                                       
         MVC   SFMID(2),DUB                                                     
         OI    SFMIDH+6,X'80'      XMIT                                         
*                                                                               
         NI    MYFLAG,X'FF'-ID2                                                 
         CLI   9(R2),C' '          CHECK IF 1 CHARACTER ID                      
         BE    DK25                                                             
         CLI   9(R2),C'A'                                                       
         BL    DK25                                                             
         CLI   9(R2),C'Z'                                                       
         BH    DK25                                                             
         OI    MYFLAG,ID2                                                       
*                                                                               
DK25     MVC   DELKEY,CLGRECD                                                   
DKX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
TRANS12  DS    0H                                                               
         XC    DUB,DUB                                                          
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
TRANS12A CLC   BYTE,2(R1)                                                       
         BNE   *+12                                                             
         MVC   DUB(2),0(R1)                                                     
         BR    RE                                                               
*                                                                               
         LA    R1,3(R1)                                                         
         BCT   R0,TRANS12A                                                      
         B     INVID                                                            
***********************************************************************         
* DELETE-NOT USED                                                               
***********************************************************************         
DELREC   DS    0H                                                               
         CLI   T31CFFD+1,C'*'                                                   
         BE    DELR05                                                           
         LA    R2,CONACTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(45),=C'ACTION DELETE - INVALID FOR NON-DDS TERMIX        
               NALS'                                                            
         OI    CONHEADH+6,X'80'    XMIT                                         
         GOTO1 ERREX2                                                           
*                                                                               
DELR05   DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    WORK2,WORK2                                                      
         MVC   KEY(CLGKGRP-CLGKTYP),DELKEY                                      
*                                                                               
         MVI   BYTE,0              DON'T PASS DELETES                           
         GOTO1 MYHIGH                                                           
         MVC   WORK2(20),KEY                                                    
         GOTO1 MYSEQ               SKIP DEFINITION RECORD                       
         CLC   KEY(CLGKGRP-CLGKTYP),DELKEY                                      
         BE    MYERR5              DEFINITION IN USE                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),WORK2                                                    
         OI    KEY+13,X'80'        DELETE DEFINITION RECORD                     
         GOTO1 MYDIRWRT                                                         
*                                                                               
         L     R5,AIO                                                           
         USING CLGRECD,R5                                                       
         MVI   BYTE,X'80'          READ FOR UPDATE                              
         GOTO1 MYGETREC                                                         
         OI    CLGCNTL,X'80'                                                    
         GOTO1 MYPUTREC            DELETE DA                                    
         DROP  R5                                                               
*                                                                               
         LA    R2,CONACTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(17),=C'SELECTION DELETED'                                
         OI    CONHEADH+6,X'80'    XMIT                                         
         GOTO1 ERREX2                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                  DISPLAY RECORD                               
         L     R7,AIO                                                           
         USING CLGRECD,R7                                                       
         L     R6,AIO                                                           
         USING CLGDESD,R6                                                       
         MVI   ELCODE,CLGDESQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SFMBK1,CLGBK1                                                    
         EDIT  (B1,CLGBK1LN),(1,SFMLN1)                                         
         MVC   SFMBK2,CLGBK2                                                    
         EDIT  (B1,CLGBK2LN),(1,SFMLN2)                                         
         OI    SFMBK1H+6,X'80'     XMIT                                         
         OI    SFMLN1H+6,X'80'     XMIT                                         
         OI    SFMBK2H+6,X'80'     XMIT                                         
         OI    SFMLN2H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SVBKLNS+0(1),CLGBK1LN                                            
         MVC   SVBKLNS+1(1),CLGBK2LN                                            
         MVC   SVBKLNS+2(1),CLGBK3LN                                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,CLGBK1LN                                                      
         SR    RE,RE                                                            
         IC    RE,CLGBK2LN                                                      
         AR    R0,RE                                                            
         IC    RE,CLGBK3LN                                                      
         AR    R0,RE                                                            
         STC   R0,BYTE2            SAVE TOTAL DIGITS                            
         B     DR20                                                             
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
DR20     DS    0H                  CLEAR DISPLAY LINES                          
         LA    R2,SFMLISTH                                                      
DR30     OC    8(72,R2),8(R2)                                                   
         BZ    DR40                                                             
         XC    8(72,R2),8(R2)                                                   
         OI    6(R2),X'80'         XMIT                                         
DR40     SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    DR30                                                             
*                                                                               
*--DISPLAY PIDS                                                                 
         LA    R2,SFMSCM1H                                                      
         LA    R1,SFMSCM6H                                                      
*                                                                               
DR50     CR    R2,R1               DONE WITH THE FIELDS?                        
         BH    DR60                 - YUP, WE'RE DONE                           
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    DR55                                                             
         XC    8(8,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         RETRANSMIT                                   
*                                                                               
         XR    R0,R0                                                            
DR55     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR50                                                             
*                                                                               
*****      NEW SECURITY MANAGER ID ELEMENT      MHC  03/01/06                   
DR60     BAS   RE,SECMGREL                                                      
                                                                                
*                                                                               
* DISPLAY PRODUCT GROUP NAMES                                                   
*                                                                               
         LA    R2,SFMLISTH                                                      
* READ HI FOR GRP DEF'N REC                                                     
         MVC   MYKEYSV,KEY         SAVE CURRENT KEY                             
         MVC   AIOSV,AIO           SAVE CURRENT AIO AREA                        
         MVC   KEY(6),0(R7)        IO AREA HAS CGRDEF REC                       
         MVI   BYTE,0              DON'T PASS DELETES                           
         GOTO1 MYHIGH                                                           
*                                                                               
DR80     GOTO1 MYSEQ                                                            
*                                                                               
         CLC   KEY(CLGKGRP-CLGKEY),KEYSAVE  TEST SAME THRU GRP ID               
         BNE   DRX                                                              
*                                                                               
         MVC   AIO,AIO2            READ PROGRAM GROUP REC INTO AIO2             
         MVI   BYTE,0                                                           
         GOTO1 MYGETREC                                                         
         L     R7,AIO                                                           
*                                                                               
         L     R6,AIO                                                           
         USING CLGMEMD,R6                                                       
         MVI   ELCODE,CLGMEMQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,8(R2)         POINT TO DISPLAY POSITION                       
*                                                                               
         MVC   BYTE,CLGKID                                                      
         BAS   RE,TRANS12                                                       
         MVC   0(2,R4),DUB                                                      
*                                                                               
         UNPK  DUB,CLGKGRP(3)                                                   
         IC    RE,BYTE2            GET TOTAL DIGITS                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),DUB+3                                                    
*                                                                               
         TM    MYFLAG,ID2                                                       
         BZ    *+14                                                             
         MVC   2(3,R4),WORK                                                     
         B     *+10                                                             
         MVC   1(3,R4),WORK                                                     
*                                                                               
         MVC   6(24,R4),CLGNAM1                                                 
         CLI   SVBKLNS+1,0                                                      
         BE    DR100                                                            
         MVC   32(24,R4),CLGNAM2                                                
*                                                                               
DR100    DS    0H                                                               
         OI    6(R2),X'80'         XMIT                                         
         SPACE 2                                                                
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNH   DRX                 END OF SCREEN                                
         B     DR80                                                             
*                                                                               
DRX      MVC   KEY,MYKEYSV         RESTORE KEY                                  
         MVC   AIO,AIOSV                                                        
         B     EXIT                                                             
         EJECT                                                                  
****  CODE COPIED FROM SPSFM15 ADDRESS RECORD MAINTENANCE  MHC 03/01/06         
***********************************************************************         
*        SECMGREL SUBROUTINE                                          *         
***********************************************************************         
*****      NEW SECURITY MANAGER ID ELEMENT      MHC  03/01/06                   
SECMGREL NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,CLGSCMCQ     SECURITY MANAGER ID ELEMENT                  
         BRAS  RE,GETEL                                                         
         BNE   SMELX                                                            
*                                                                               
         USING CLGSCMD,R6                                                       
         LA    R2,SFMSCM1H                                                      
         LA    R3,SFMSCM6H                                                      
         LA    R4,CLGSCM1                                                       
*                                                                               
SECMGR10 OC    0(2,R4),0(R4)       ANYTHING IN THE CLGSCM?                      
         BZ    SMELX                - NOPE, WE'RE DONE                          
*                                                                               
         CR    R2,R3                                                            
         BH    SMELX               WE'RE DONE                                   
*                                                                               
         LR    RE,R2                                                            
         TM    1(RE),X'02'         DO WE HAVE AN EXTENDED FIELD HEADER?         
         BZ    SECMGRNX                                                         
         XR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         SHI   RE,8                POINT TO START OF EXTENDED HEADER            
         CLI   0(RE),111           IS IT ONE OF THE PID FIELDS?                 
         BNE   SECMGRNX             - NOPE                                      
*                                                                               
         GOTO1 SECMGRID,DMCB,(R2),(R4)                                          
*                                                                               
         LA    R4,L'CLGSCM(R4)     BUMP TO NEXT SECURITY MANAGER ID             
*                                                                               
SECMGRNX XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     SECMGR10                                                         
*                                                                               
SMELX    DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SECMGRID SUBROUTINE                                          *         
*        P1 = SCREEN HEADER                                           *         
*        P2 = PID                                                     *         
***********************************************************************         
SECMGRID NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
* PERSONAL ID                                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AUTHD,R3                                                         
         MVC   SECRAGY,SECALPHA                                                 
         MVC   PASSWD,0(R4)                                                     
         MVC   AIO,AIO2                                                         
         GOTO1 VALIAUTH,DMCB,WORK   GET PERSONAL ID                             
         MVC   AIO,AIO1                                                         
         MVC   8(8,R2),PRSNLID                                                  
         MVI   5(R2),8             INPUT LENGTH OF 8                            
         OI    6(R2),X'80'         RETRANSMIT                                   
         DROP  R3                                                               
*                                                                               
SMIDX    DS    0H                                                               
         J     EXIT                                                             
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         L     R7,AIO                                                           
         CLI   ACTNUM,ACTADD       TEST ADD                                     
         BNE   VR4                                                              
* COUNT NUMBER OF CLTGRPS ON FILE                                               
         ZAP   HALF,=P'0'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(5),SVKEY        X'0D06'/A-M/CLT                              
         MVI   BYTE,0              DON'T COUNT DELETES                          
         GOTO1 MYHIGH                                                           
         B     VR1B                                                             
VR1A     MVC   KEY+6(2),=X'FFFF'                                                
         MVI   BYTE,0                                                           
         GOTO1 MYHIGH                                                           
VR1B     CLC   KEY(5),SVKEY                                                     
         BNE   VR1X                                                             
         AP    HALF,=P'1'                                                       
         CP    HALF,=P'25'         LIMIT OF 26 SCHEMES                          
         BNH   VR1A                                                             
         B     MYERR1              TOO MANY SCHEMES                             
VR1X     MVC   CLGKEY,SVKEY                                                     
         MVC   CLGLEN,=H'65'                                                    
         MVC   CLGAGYA,USERNAME                                                 
         MVI   CLGDESCD,CLGDESQ                                                 
         MVI   CLGDESLN,CLGDESLQ                                                
         B     VR4                                                              
*                                                                               
VR4      L     R6,AIO                                                           
         USING CLGDESD,R6                                                       
         MVI   ELCODE,CLGDESQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,SFMBK1H                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         MVC   CLGBK1,8(R2)                                                     
         LA    R2,SFMLN1H                                                       
         MVC   BYTE,8(R2)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,3                                                           
         BH    INVLFLD                                                          
         CLI   BYTE,1                                                           
         BL    INVLFLD                                                          
         CLC   CLGBK1LN,BYTE                                                    
         BE    VR6                                                              
         CLI   ACTNUM,ACTADD                                                    
         BNE   MYERR3                                                           
         MVC   CLGBK1LN,BYTE                                                    
*                                                                               
VR6      LA    R2,SFMBK2H                                                       
         CLI   5(R2),0             IF NO NAME INPUT                             
         BNE   VR7                                                              
         LA    R2,SFMLN2H                                                       
         CLI   5(R2),0             LENGTH INPUT INVALID                         
         BE    VR10                                                             
         B     INVLFLD                                                          
VR7      MVC   CLGBK2,8(R2)                                                     
         LA    R2,SFMLN2H                                                       
         MVC   BYTE,8(R2)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,3                                                           
         BH    INVLFLD                                                          
         CLI   BYTE,1                                                           
         BL    INVLFLD                                                          
         CLC   BYTE,CLGBK2LN                                                    
         BE    VR10                                                             
         CLI   ACTNUM,ACTADD                                                    
         BNE   MYERR3                                                           
         MVC   CLGBK2LN,BYTE                                                    
         SR    R4,R4               CHECK IF SUM OF LENGTHS EXCEEDS 3            
         IC    R4,CLGBK1LN                                                      
         SR    R5,R5                                                            
         IC    R5,CLGBK2LN                                                      
         AR    R4,R5                                                            
         C     R4,=F'3'                                                         
         BH    MYERR4                                                           
*                                                                               
VR10     DS    0H                                                               
         LA    R2,SFMBK1H                                                       
         ZIC   R1,BYTE                                                          
         CH    R1,=H'3'                                                         
         BH    MYERR2              TOO MANY DIGITS                              
*                                                                               
* VALIDATE SECURITY ID'S                                                        
         MVI   ELCODE,CLGSCMCQ                                                  
         GOTO1 DELEL                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CLGSCMD,R6                                                       
         MVI   CLGSCMCD,CLGSCMCQ   X'12' SECURITY MANAGER ID ELEMENT            
         MVI   CLGSCMLN,CLGSCKLQ                                                
         LA    R5,CLGSCM                                                        
****  WE WILL FILL THE CLGSCM'S ONE BY ONE A BIT LATER                          
*                                                                               
         LA    R2,SFMSCM1H                                                      
         LA    R3,SFMSCM6H                                                      
*                                                                               
VR30     DS    0H                                                               
         CR    R2,R3                                                            
         BH    VR60                WE'RE DONE                                   
*                                                                               
         LR    RE,R2                                                            
         TM    1(RE),X'02'         DO WE HAVE AN EXTENDED FIELD HEADER?         
         BZ    VR40                                                             
         XR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         SHI   RE,8                POINT TO START OF EXTENDED HEADER            
         CLI   0(RE),111           IS IT ONE OF THE PID FIELDS?                 
         BNE   VR40                                                             
         CLI   5(R2),0             ANYTHING HERE?                               
         BE    VR40                 - NOPE, MOVE TO THE NEXT SCMFIELD           
         MVC   PIDNAME,8(R2)                                                    
         OC    PIDNAME,=C'        '                                             
*                                                                               
         BRAS  RE,CHECKPID         WE NEED TO CHECK ALL THE PIDS                
         BNE   BADPID                                                           
         MVC   0(L'CLGSCM,R5),PIDNUM   SAVE OFF THE PID NUMBER                  
         LA    R5,L'CLGSCM(R5)                                                  
*                                                                               
VR40     XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR30                                                             
*                                                                               
VR60     GOTO1 PUTEL                                                            
         DROP  R6                                                               
*                                                                               
VRX      B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECKPID SUBROUTINE                                          *         
***********************************************************************         
CHECKPID NTR1                                                                   
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         USING SAPEREC,R6                                                       
         XC    SAPEKEY,SAPEKEY     BUILD PERSON KEY                             
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECALPHA                                                 
         MVC   SAPEPID,PIDNAME                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'CTFILE ',KEY2,AIO2               
         L     R6,AIO2                                                          
         CLC   KEY2(SAPEDEF-SAPEKEY),0(R6)                                      
         BNE   CHKPIDNO                                                         
*                                                                               
         USING SAPWDD,R6                                                        
         MVI   ELCODE,SAPWDELQ     X'C4' - PERSON PASSWORD ELEM                 
         BRAS  RE,GETEL2                                                        
         BNE   CHKPIDNO                                                         
         MVC   PIDNUM,SAPWDNUM                                                  
*                                                                               
CHKPIDYS CR    RE,RE                                                            
         B     *+6                                                              
*                                                                               
CHKPIDNO LTR   RE,RE                                                            
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS                                                                  
***********************************************************************         
LR       DS    0H                                                               
         LA    R2,LISTAR                                                        
         USING PLINED,R2                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
LR02     MVI   NLISTS,15           SET NUMBER OF LIST LINES                     
         CLI   PREVFLAG,0                                                       
         BE    LR03                                                             
*                                                                               
         MVC   KEY,PREVKEY                                                      
         XC    PREVFLAG,PREVFLAG                                                
*                                                                               
LR03     LA    R3,KEY                                                           
         USING CLGRECD,R3                                                       
*                                                                               
         OC    KEY(13),KEY                                                      
         BNZ   LR30                                                             
*                                                                               
         LA    R0,SORTDATA                                                      
         LHI   R1,SORTDATX-SORTDATA                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   SORTLAST,0                                                       
         LA    R5,SORTDATA                                                      
*                                                                               
         MVC   CLGKTYP,=X'0D06'                                                 
         MVC   CLGKAGMD,BAGYMD                                                  
*                                                                               
         MVC   KEY,SVKEY                                                        
*                                                                               
LR12     GOTO1 HIGH                                                             
         B     LR13                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR13     DS    0H                                                               
         LA    R3,KEY                                                           
         CLC   KEY(3),SVKEY        SAME TYPE/AGYMD                              
         BNE   LR20                                                             
         OC    KEY+4(9),KEY+4      GROUP DESCRIPTION RECORD?                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,SPCGRTAB                                                      
         LHI   RF,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
LR14     CLC   CLGKID,2(RE)                                                     
         BE    LR16                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,LR14                                                          
         DC    H'0'                                                             
*                                                                               
LR16     CLC   0(2,RE),SFMID       COMPARE TO START VALUE                       
         BL    LR18                                                             
         MVC   0(3,R5),0(RE)       SAVE BOTH CODES                              
         AHI   R5,3                                                             
*                                                                               
LR18     DS    0H                                                               
         MVC   KEY+4(9),=9X'FF'    FORCE NEXT GROUP                             
         B     LR12                                                             
*                                                                               
LR20     LR    R1,R5                                                            
         LA    R0,SORTDATA                                                      
         SR    R1,R0               GIVES LENGTH USED                            
         BZ    LRX                                                              
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LR    R0,R1                                                            
*                                                                               
         GOTO1 XSORT,DMCB,SORTDATA,(R0),3,2,0                                   
         EJECT                                                                  
*=============================================================                  
* NOW DISPLAY DATA FROM SORTED LIST                                             
*=============================================================                  
LR30     DS    0H                                                               
         LA    R5,SORTDATA         FIND LAST ENTRY DISPLAYED                    
         LHI   R0,(SORTDATX-SORTDATA)/3  LOOP PREVENTION                        
         CLI   SORTLAST,0                                                       
         BE    LR34                                                             
*                                                                               
LR32     DS    0H                                                               
         CLC   SORTLAST,2(R5)                                                   
         BE    LR34                                                             
         AHI   R5,3                                                             
         BCT   R0,LR32                                                          
         DC    H'0'                                                             
*                                                                               
LR34     DS    0H                                                               
         MVC   SORTLAST,2(R5)      SET START POINT                              
*                                                                               
         OC    0(3,R5),0(R5)       TEST MORE DATA                               
         BZ    LRX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         MVC   CLGKID,2(R5)        MOVE 1 CHAR CODE                             
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING CLGDESD,R6                                                       
         MVI   ELCODE,CLGDESQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        GOTO1 CLUNPK,DMCB,CLGKCLT,PLCLI                                        
*                                                                               
* TRANSLATE 1 CHAR CODE TO DISPLAYABLE 2 CHARS                                  
*                                                                               
         LA    RE,SPCGRTAB                                                      
         LHI   RF,SPCGRTBX-SPCGRTAB                                             
*                                                                               
LR35     CLC   CLGKID,2(RE)                                                     
         BE    LR36                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,LR35                                                          
         DC    H'00'                                                            
*                                                                               
LR36     MVC   PLGRPID,0(RE)                                                    
*                                                                               
         MVC   PLLEV1,CLGBK1                                                    
         MVC   PLLEV2,CLGBK2                                                    
         CLI   MODE,PRINTREP                                                    
         BE    LR40                                                             
         GOTO1 LISTMON                                                          
         B     LR45                                                             
*                                                                               
LR40     MVC   P+12(PLEND-PLGRPID),PLGRPID                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR45     DS    0H                                                               
         AHI   R5,3                                                             
         OC    0(3,R5),0(R5)                                                    
         BNZ   LR34                                                             
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
INIT     NTR1                                                                   
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R3                                                               
INITX    J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
MYERR1   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'*** ERROR-LIMIT IS 26 SCHEMES'                    
         B     MYERR                                                            
MYERR2   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(25),=C'*** ERROR-TOO MANY DIGITS'                        
         B     MYERR                                                            
MYERR3   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'*** MAY NOT CHANGE BREAK LEN'                     
         B     MYERR                                                            
MYERR4   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'*** ERROR-SUM OF LENGTHS EXCEEDS 3'               
         B     MYERR                                                            
MYERR5   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'*** CANNOT DELETE-DEFINITION IN USE'              
         B     MYERR                                                            
INVID    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'*** INVALID GROUP ID               '              
         B     MYERR                                                            
BADPID   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'*** USER ID DOES NOT EXIST         '              
         B     MYERR                                                            
*                                                                               
MYERR    GOTO1 ERREX2                                                           
         B     EXIT                                                             
DELERR   MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
         GETEL2 R6,CTDISP,ELCODE                                                
         EJECT                                                                  
* SUB-ROUTINES FOR ELEMENT MAINTENANCE                                          
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R7)),0                   
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
PUTEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE'),0(R7),ELEM                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER CALLS                                                            
***********************************************************************         
MYHIGH   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(BYTE,=C'DMRDHI'),=CL8'SPTDIR',KEY,KEY,0            
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
MYSEQ    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(BYTE,=C'DMRSEQ'),=CL8'SPTDIR',KEY,KEY,0            
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
MYGETREC NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(BYTE,=C'GETREC'),=C'SPTFILE ',            X        
               KEY+14,AIO,MYDMWRK                                               
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=CL8'SPTDIR',KEY,KEY                      
         B     EXIT                                                             
MYPUTREC NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=CL8'SPTFILE',KEY+14,AIO,MYDMWRK         
         B     EXIT                                                             
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'92'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
*                                                                               
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
***********************************************************************         
* REPORT HEADING AND HEAD HOOK                                                  
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK CLIENT DEFINITION RECORDS'                       
         SSPEC H2,46,C'----------------------------------'                      
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,93,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PLGRPID(2),=C'ID'                                                
         MVC   PLGRPID+132(2),=20C'-'                                           
         MVC   PLLEV1(7),=C'LEVEL 1'                                            
         MVC   PLLEV1+132(12),=20C'-'                                           
         MVC   PLLEV2(7),=C'LEVEL 2'                                            
         MVC   PLLEV2+132(12),=20C'-'                                           
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE SPCGRTAB                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMDAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMDBD                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
         PRINT ON                                                               
*                                                                               
*                                                                               
*                                                                               
*                           *******  T31C31 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
BYTE2    DS    CL1                                                              
SVBKLNS  DS    CL6                                                              
PREVFLAG DS    CL1                                                              
PREVKEY  DS    CL48                                                             
MYKEYSV  DS    CL48                                                             
DELKEY   DS    CL48                                                             
WORK2    DS    CL48                                                             
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
AIOSV    DS    F                                                                
MYFLAG   DS    XL1                                                              
ID2      EQU   X'02'        2 CHARACTER CLIENT GROUP                            
FILTID   DS    XL1                                                              
*                                                                               
PIDNAME  DS    CL8                                                              
PIDNUM   DS    CL2                                                              
SECALPHA DS    CL2                  SECURITY AGENCY ALPHA                       
KEY2     DS    CL50                                                             
*                                                                               
SORTLAST DS    XL1                 LAST 1-BYTE CODE DISPLAYED                   
SORTDATA DS    XL768               MAX 256 3 BYTE CODES                         
SORTDATX EQU   *                                                                
*                                                                               
WORKEND  EQU   *                                                                
*                                                                               
PLINED   DSECT                                                                  
         DS    CL2                                                              
PLGRPID  DS    CL2                                                              
         DS    CL2                                                              
PLLEV1   DS    CL12                                                             
         DS    CL2                                                              
PLLEV2   DS    CL12                                                             
         DS    CL2                                                              
PLEND    EQU   *                                                                
         EJECT                                                                  
*SPGENCLG                                                                       
       ++INCLUDE SPGENCLG                                                       
       ++INCLUDE SEACSFILE                                                      
 END                                                                            
