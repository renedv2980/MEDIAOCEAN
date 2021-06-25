*          DATA SET SPTRA17    AT LEVEL 035 AS OF 01/26/11                      
*PHASE T21617B                                                                  
         TITLE 'T21617 STATIONS BY MARKET DISPLAY, CHANGE, ADD LIST'            
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30) SHIPPING RECORD TO BE MAINTAINED         
*             AIO2 -                                                            
*             AIO3 -                                                            
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - WORK                                                              
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
* LEV 9 JUN6/86 EDIT STATIONS TO ALLOW VALID RADIO FOR CHANGE OPTION            
* LEV 10             ALLOW MAINT AND LIST TO ALL CLT ALL PRD MKTLISTS           
* LEV 11-12 JAN15/87 ADD CLIENT AND PRODUCT TO REPORT                           
* LEV 13    JAN28/87 BLK CLIENT                                                 
* LEV 14-16 FEB03/87 ADD PASSIVE PTR AND USE IT FOR LIST MODE                   
* LEV 17    JUN22/87 MAKE PROD POL INVALID AND CK FOR TRAFFIC STATION           
* LEV 18    JUL15/87 DELETE PASSIVE POINTER WITH DELETE REC                     
* LEV 19    SEP05/89 ALLOW FOR LARGER SCREENS                                   
* LEV 20    DEC06/89 FORCE CLEAR OF LARGER SCREENS                              
* LEV 21    JUN17/92 ADD AGENCY CODE TO RECORD                        *         
* LEV 22    DEC02/92 CHANGE FOR STATION FILE                          *         
* LEV 23    MAR22/92 CHANGE-CABLE NETWORK STATION                     *         
* LEV 24    MAY07/93 CHANGE NEW TRAFFIC SYSTEM                        *         
* LEV 25    JUL27/93 ALLOW NUMERIC STATIONS                           *         
* LEV 26    JUL27/95 FIX DELETE PASSIVE KEY FROM SPTDIR TO TRFDIR     *         
* LEV 27    APR27/95 FIX DELETE PASSIVE KEY                           *         
* LEV 28 BG OCT10/01 ADD CLIENT SECURITY FOR LISTS                    *         
* LEV 29 BG MAR12/02 ALLOW H9 (STAR) TO HAVE NO STA ADDR & ADD        *         
*                    MORE STATIONS TO SCREEN                          *         
* LEV 30 SMUR JUN04/02 FIX CLIENT SECURITY WHEN LIST                  *         
* LEV 31 SMUR JUN26/02 CLIENT STRING SECURITY                                   
* LEV 32 BGRI SEP04/03 REMOVE UNUSED INVSTAT                          *         
* LEV 33 SMUR JUL29/04 SOX                                            *         
* LEV 34 SMUR JAN26/11 BYPASS EMPTY STATION LIST RECS (NO X'10' ELEM) *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21617   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21617**                                                       
         LA    R7,2048(,RB)                                                     
         LA    R7,2048(,R7)                                                     
         USING T21617,RB,R7                                                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 3                                                                
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
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,XRECADD        AFTER ADD RECORD (ADD PASSIVE PTR)           
         BE    AAR                                                              
         CLI   MODE,XRECDEL        AFTER DEL RECORD (DEL PASSIVE PTR)           
         BE    ADR                                                              
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE                                                                  
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
         SPACE                                                                  
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     DS    0H                                                               
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         XC    QCLT,QCLT                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK10                                                             
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK20                 NO                                          
         SPACE                                                                  
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
         SPACE                                                                  
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
         SPACE                                                                  
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
         SPACE                                                                  
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
         SPACE                                                                  
         MVI   ERROR,0                                                          
         B     VK20                                                             
         SPACE                                                                  
VK10     GOTO1 VALICLT                                                          
         SPACE                                                                  
VK20     LA    R2,TRAPRDH          PRODUCT                                      
         MVI   BPRD,0                                                           
         XC    QPRD,QPRD                                                        
         CLI   5(R2),0                                                          
         BE    VK30                YES                                          
         OC    BCLT,BCLT           CLIENT MUST BE ENTERED FOR PRODUCT           
         BZ    MISSCLT                                                          
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    PRDERR                                                           
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         SPACE                                                                  
VK30     LA    R2,TRAMKTH          MARKET                                       
         XC    BMKT,BMKT                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK34                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK40                                                             
VK34     GOTO1 VALIMKT                                                          
         SPACE                                                                  
VK40     LA    R2,TRAFLTRH         FILTER                                       
         CLI   5(R2),0                                                          
         BNE   FLTRERR                                                          
         SPACE                                                                  
* NOW BUILD KEY                                                                 
         SPACE                                                                  
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING STLKEY,R4                                                        
         MVC   STLKID,=XL2'0A31'                                                
         MVC   STLKAM,BAGYMD                                                    
         MVC   STLKCLT,BCLT        MOVE IN CLIENT                               
         MVC   STLKPRD,QPRD        MOVE PRODUCT                                 
         MVC   STLKMKT,BMKT        MOVE MARKET                                  
         MVC   SVKMKT,BMKT                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE 3                                                                
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VRS01    DS    0H                                                               
         LA    R0,8                                                             
         LA    R1,250                                                           
         L     RE,AIO2                                                          
         L     RF,AIO1                                                          
         MVC   20(2,RF),AGENCY                                                  
VR04     MVC   0(250,RE),0(RF)                                                  
         AR    RE,R1                                                            
         AR    RF,R1                                                            
         BCT   R0,VR04                                                          
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVC   SVKEY,KEY                                                        
         USING STLKEY,R4                                                        
         MVC   SVBMKT,STLKMKT                                                   
         DROP  R4                                                               
         LA    R2,TRASTA1H         FIRST FLD                                    
         MVI   ELCODE,X'10'        DATA ELEMENT                                 
         L     R6,AIO2                                                          
         GOTO1 REMELEM             REMOVE ALL STATIONS                          
         MVI   STASW,0                                                          
         SPACE                                                                  
VR10     LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING STLDTAEL,R6                                                      
         MVI   ELCODE,X'10'                                                     
         USING STLDTAEL,R6                                                      
         MVI   STLDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   STLDTALN,STLDTAX-STLDTAEL ELEMENT LENGTH                         
         CLI   5(R2),0             ANY DATA ENTERED                             
         BE    VR30                NO                                           
         SPACE                                                                  
         GOTO1 VALISTA                                                          
         SPACE                                                                  
* READ STATION ADDRESS RECORD AND SAVE IT *                                     
         SPACE                                                                  
         CLC   SVBMKT,BMKT          ALL STATIONS MUST BE IN KEY MARKET          
         BNE   MKTERR                                                           
         SPACE                                                                  
         CLC   AGENCY,=C'H9'       THIS STARCOM                                 
         BE    VR14                  YES, ALLOW NO TRAFFIC STA ADDR             
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(5),QSTA                                                    
         SPACE                                                                  
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   KEY+7,C'T'                                                       
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADTRSTA                                                         
         SPACE                                                                  
VR14     DS    0H                                                               
         MVC   STLSTA,BSTA                                                      
         L     R6,AIO2                                                          
         BAS   RE,GETEL                                                         
         BNE   VR24                                                             
VR20     CLC   STLSTA,BSTA                                                      
         BE    EQSTAER                                                          
         BAS   RE,NEXTEL                                                        
         BE    VR20                                                             
VR24     MVC   AIO,AIO2                                                         
         GOTO1 ADDELEM                                                          
         MVI   STASW,255                                                        
         SPACE                                                                  
VR30     ZIC   R0,0(R2)            GET LEN OF THIS FLD                          
         AR    R2,R0               POINT TO PTR CMML                            
         LA    R1,TRATAGH          SEE IF AT END OF SCREEN                      
         CR    R2,R1                                                            
         BL    VR10                                                             
         CLI   STASW,255           ANY STATIONS ENTERED                         
         BNE   NOSTAER                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
         MVC   AIO,AIO1            DO NOT DESTROY UPDATED                       
         CLI   ACTNUM,ACTADD                                                    
         BE    VR38                                                             
         GOTO1 GETREC                                                           
VR38     LA    R0,8                                                             
         LA    R1,250                                                           
         L     RE,AIO1                                                          
         ST    RE,AIO                                                           
         L     RF,AIO2                                                          
VR40     MVC   0(250,RE),0(RF)                                                  
         AR    RE,R1                                                            
         AR    RF,R1                                                            
         BCT   R0,VR40                                                          
         B     DR                  GO DISPLAY UPDATED RECORD                    
         SPACE                                                                  
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE                                                                  
DR       LA    R2,TRASTA1H                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING STLDTAEL,R6                                                      
DR10     MVC   BSTA,STLSTA                                                      
         XC    WORK,WORK                                                        
         GOTO1 MSUNPK,DMCB,(X'80',BMKTSTA),WORK,WORK+4                          
         SPACE                                                                  
* EDIT STATION  HERE                                                            
         SPACE                                                                  
         MVC   BLOCK(L'TRASTA1),SPACES                                          
         MVC   BLOCK(4),WORK+4                                                  
         SPACE                                                                  
         CLC   WORK+9(3),SPACES    THIS CABLE NET                               
         BE    DR12                                                             
         MVC   BLOCK(8),WORK+4                                                  
         MVI   BLOCK+4,C'/'                                                     
         B     DR16                                                             
DR12     LA    R1,BLOCK+4                                                       
         CLI   BLOCK+3,C' '                                                     
         BH    DR14                                                             
         BCTR  R1,0                                                             
DR14     CLI   WORK+8,C' '        IF TV, LEAVE ALONE                            
         BE    DR16                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),WORK+8                                                   
         CLI   WORK+8,C'X'                                                      
         BE    DR16                                                             
         MVI   2(R1),C'M'                                                       
DR16     CLC   8(L'TRASTA1,R2),BLOCK                                            
         BE    *+14                                                             
         MVC   8(L'TRASTA1,R2),BLOCK                                            
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BAS   RE,NEXTEL                                                        
         BE    DR10                                                             
         BAS   RE,CLR                                                           
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE 3                                                                
DK       LA    R2,TRAMEDH                                                       
         L     R4,AIO                                                           
         USING STLKEY,R4                                                        
         XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMED,WORK         MOVE IN MEDIA                                
         OI    TRAMEDH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         XC    WORK(L'TRACLT),WORK                                              
         GOTO1 CLUNPK,DMCB,STLKCLT,WORK                                         
         CLC   TRACLT,WORK                                                      
         BE    *+14                                                             
         MVC   TRACLT,WORK         MOVE IN CLIENT                               
         OI    TRACLTH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         XC    WORK(L'TRAPRD),WORK                                              
         MVC   WORK(L'STLKPRD),STLKPRD                                          
         CLC   TRAPRD,WORK                                                      
         BE    *+14                                                             
         MVC   TRAPRD,WORK         MOVE IN PROD                                 
         OI    TRAPRDH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         LH    R0,STLKMKT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         XC    WORK(L'TRAMKT),WORK                                              
         UNPK  WORK(4),DUB                                                      
         CLC   TRAMKT,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMKT,WORK         MOVE IN MARKET                               
         OI    TRAMKTH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ADD PASSIVE POINTER FOR NEW STATION LIST RECORD *                             
         SPACE                                                                  
AAR      MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STLKEY,R4                                                        
         MVC   STLKID,=XL2'0A31'                                                
         MVC   STLKAM,BAGYMD                                                    
         MVC   STLKCLT,BCLT        MOVE IN CLIENT                               
         MVC   STLKPRD,QPRD        MOVE PRODUCT                                 
         MVC   STLKMKT,BMKT        MOVE MARKET                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JUST ADDED RECORD, MUST BE THERE             
         OI    STLKID+1,X'80'       CHANGE 31 TO B1                             
         MVC   STLPMKT,BMKT        MOVE IN MARKET                               
         MVC   STLPCLT,BCLT        AND CLIENT                                   
         MVC   STLPPRD,QPRD        AND PRODUCT                                  
         GOTO1 DATAMGR,DMCB,=C'DMADD',SYSDIR,KEY,KEY                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 3                                                                
* DEL PASSIVE POINTER FOR DELETED STATION LIST RECORD *                         
         SPACE                                                                  
ADR      MVC   SVKEY,KEY                                                        
         L     R1,AIO1                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STLKEY,R4                                                        
         MVC   KEY(3),0(R1)                                                     
         OI    STLKID+1,X'80'       CHANGE 31 TO B1                             
         MVC   STLPMKT,STLKMKT-STLKEY(R1)                                       
         MVC   STLPCLT,STLKCLT-STLKEY(R1)                                       
         MVC   STLPPRD,STLKPRD-STLKEY(R1)                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'               JUST DELETED RECORD, MUST BE THERE            
         SPACE                                                                  
         OI    KEY+13,X'80'        DELETE PASSIVE KEY                           
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TRFDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE                                                                  
LR       LA    R4,KEY                                                           
         MVI   NLISTS,16                                                        
         SPACE                                                                  
         MVC   SVBCLT,BCLT         SAVE FOR 1 CLIENT ONLY                       
         SPACE                                                                  
         USING STLKEY,R4                                                        
         OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR10                NO, GOTO HIGH                                
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             HEADING ROUTINE                              
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         SPACE                                                                  
* BUILD KEY, AND DO READHI                                                      
         MVC   STLPID,=XL2'0AB1'                                                
         MVC   STLPAM,BAGYMD                                                    
         MVC   STLPMKT,BMKT        MOVE MARKET                                  
         MVC   STLPCLT,BCLT        MOVE IN CLIENT                               
         MVC   STLPPRD,QPRD        MOVE PRODUCT                                 
         GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR24                                                             
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   EXIT                                                             
         MVC   P(33),=CL33'NO MARKET STATION LIST RECS FOUND'                   
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
         SPACE                                                                  
* USING PASSIVE KEY SEQUENCE, MUST BUILD PASSIVE KEY FROM LAST REC *            
         SPACE                                                                  
LR10     L     R4,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AB1'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),STLKMKT                                                 
         MVC   KEY+5(5),STLKCLT                                                 
         GOTO1 HIGH                                                             
         SPACE                                                                  
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
         SPACE                                                                  
LR22     CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   EXIT                YES                                          
LR24     MVC   SVKEY(2),=XL2'0AB1'                                              
         MVC   SVKEY+2(1),BAGYMD                                                
         CLC   SVKEY(3),KEY                                                     
         BH    LR20                                                             
         BL    EXIT                                                             
         OC    SVBCLT,SVBCLT       WAS CLIENT ENTERED                           
         BZ    LR30                                                             
         CLC   SVBCLT,KEY+5                                                     
         BNE   LR20                                                             
LR30     CLI   BPRD,0                                                           
         BE    LR34                                                             
         CLC   QPRD,KEY+7                                                       
         BNE   LR20                                                             
LR34     OC    SVKMKT,SVKMKT                                                    
         BZ    LR36                                                             
         CLC   SVKMKT,KEY+3                                                     
         BNE   LR20                                                             
LR36     DS    0H                                                               
*NOP     CLC   BCLT,KEY+5                                                       
*NOP     BE    LR40                                                             
         SPACE                                                                  
         BAS   RE,FCLT                                                          
         BNE   LR20                                                             
         SPACE                                                                  
LR40     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         TM    15(R4),X'80'        DELETED RECORD                               
         BO    LR20                BYPASS                                       
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR20                BYPASS EMPTY RECORD                          
                                                                                
         USING STLDTAEL,R6                                                      
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
         SPACE                                                                  
LRL      MVC   LISTAR,SPACES                                                    
         SPACE                                                                  
         MVC   LCLT,QCLT                                                        
         MVC   LPRD,STLKPRD                                                     
         SR    R0,R0                                                            
         ICM   R0,3,STLKMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         MVC   LMKT,QMKT                                                        
         LA    R2,L'LSTALST/9                                                   
         LA    R3,LSTALST                                                       
         SPACE                                                                  
LRL10    MVC   BSTA,STLSTA                                                      
         BAS   RE,FMTSTA                                                        
         MVC   0(7,R3),STAPRNT                                                  
         SPACE                                                                  
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   0(8,R3),STANET                                                   
         SPACE                                                                  
         BAS   RE,NEXTEL                                                        
         BNE   LRL20                                                            
         LA    R3,9(,R3)                                                        
         BCT   R2,LRL10                                                         
         SPACE                                                                  
         SH    R3,=H'9'              BACK TO LAST STATION PRINTED               
         MVC   0(7,R3),=CL7'MORE...' INDICATE MORE NOT PRINTED                  
         SPACE                                                                  
LRL20    MVI   NLISTS,16           LONGER LIST (STD=15)                         
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE                                                    
         SPACE                                                                  
LRR      MVC   P,SPACES                                                         
         SPACE                                                                  
         MVC   PCLT,=C'ALL'                                                     
         OC    STLKCLT,STLKCLT                                                  
         BZ    LRR02                                                            
         GOTO1 CLUNPK,DMCB,STLKCLT,PCLT                                         
         SPACE                                                                  
LRR02    MVC   PPROD,=C'ALL'                                                    
         OC    STLKPRD,STLKPRD                                                  
         BZ    LRR04                                                            
         MVC   PPROD,STLKPRD                                                    
         SPACE                                                                  
* READ MARKET RECORD FOR NAME *                                                 
         SPACE                                                                  
LRR04    CLI   HDHKSW,1                                                         
         BE    *+14                                                             
         CLC   SVBMKT,STLKMKT                                                   
         BE    LRR10                                                            
         MVI   HDHKSW,0                                                         
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         SR    R0,R0                                                            
         ICM   R0,3,STLKMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         MVC   SVBMKT,STLKMKT                                                   
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R5,AIO2                                                          
         USING MKTRECD,R5                                                       
         SPACE                                                                  
         CLC   KEY(8),0(R5)                                                     
         BE    LRR06                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO2                     
         SPACE                                                                  
LRR06    LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(8),0(R5)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         SPACE                                                                  
         MVC   PMKTNM,0(R1)                                                     
         MVC   MKTNM,0(R1)                                                      
         MVC   PMKT,QMKT                                                        
LRR10    LA    R2,L'PSTALST/8                                                   
         LA    R3,PSTALST                                                       
LRR14    MVC   BSTA,STLSTA                                                      
         BAS   RE,FMTSTA                                                        
         MVC   0(7,R3),STAPRNT                                                  
         SPACE                                                                  
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   0(8,R3),STANET                                                   
         SPACE                                                                  
         LA    R3,9(,R3)                                                        
         BAS   RE,NEXTEL                                                        
         BNE   LRR20                                                            
         BCT   R2,LRR14                                                         
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         B     LRR10                                                            
LRR20    GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         B     LR20                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
* FORMAT MARKET/STATION FOR PRINTING                                            
         SPACE                                                                  
FMTSTA   NTR1                                                                   
         GOTO1 MSUNPK,DMCB,(X'80',BMKTSTA),DUB,WORK                             
         SPACE                                                                  
         XC    STANET,STANET                                                    
         CLC   WORK+5(3),SPACES    THIS A CABLE NET                             
         BE    FMTSTA10             NO                                          
         MVC   STANET,WORK                                                      
         MVI   STANET+4,C'/'                                                    
         SPACE                                                                  
FMTSTA10 CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),WORK                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C' '                                                       
FMTSTAX  B     EXIT                                                             
         EJECT                                                                  
* FIND CLIENT HEADER AND CHECK SECURITY                                         
         SPACE                                                                  
FCLT     NTR1                                                                   
         SPACE                                                                  
         MVC   BCLT,KEY+5                                                       
         SPACE                                                                  
         OC    BCLT,BCLT                                                        
         BZ    FCLT50                                                           
         SPACE                                                                  
* SAVE CURRENT RECORD & KEY                                                     
         SPACE                                                                  
FCLT10   DS   0H                                                                
         MVC   SVKEY,KEY                                                        
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         SPACE                                                                  
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         MVI   ERROR,0                                                          
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
FCLT20   DS    0H                                                               
         L     R0,AIO1             MOVE CML RECORD BACK                         
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         SPACE                                                                  
         MVC   AIO,AIO2                                                         
         CLC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
         CLI   ERROR,0             SET CC FOR RETURN                            
         SPACE                                                                  
         B     EXIT                                                             
         SPACE                                                                  
FCLT50   SR    R2,R2                                                            
         SR    R3,R3                                                            
         L     RE,ASVCLIST                                                      
         LA    RF,880                                                           
         MVCL  RE,R2                                                            
         MVC   CLTNM,=CL20'NON-CLIENT SPECIFIC'                                 
         MVC   QCLT,SPACES                                                      
         SPACE                                                                  
         CR    RB,RB               SET EQ TO ACCEPT RECS                        
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 3                                                                
* CLEAR REST OF SCREEN AND SET OFF PROTECT BITS                                 
         SPACE                                                                  
CLR      NTR1                                                                   
         LA    RF,TRATAGH          BLANK REST OF SCREEN                         
         XC    WORK,WORK                                                        
CLR10    CR    R2,RF               AT END OF SCREEN                             
         BNL   EXIT                YES                                          
         ZIC   R0,0(R2)                                                         
         LR    R1,R0                                                            
         SH    R1,=H'9'            GET FLD LEN-1                                
         SPACE                                                                  
         EX    R1,CLROC             SEE IF FLD BINARY ZEROS                     
         BE    CLR14                                                            
         EX    R1,CLRCLC            SEE IF FLD SPACES                           
         BE    CLR14                                                            
         EX    R1,CLRMVC            MAKE FLD BLANK                              
         OI    6(R2),X'80'                                                      
CLR14    AR    R2,R0                                                            
         B     CLR10                                                            
CLROC    OC    8(0,R2),8(R2)                                                    
CLRCLC   CLC   8(0,R2),SPACES                                                   
CLRMVC   MVC   8(0,R2),WORK                                                     
         EJECT                                                                  
         SPACE 3                                                                
* HEADING ROUTINE FOR REPORT *                                                  
         SPACE                                                                  
HDHK     NTR1                                                                   
         MVC   H5+11(1),QMED                                                    
         CLC   P,SPACES                                                         
         BE    HDHK10                                                           
         OC    P,P                                                              
         BZ    HDHK10                                                           
         MVC   PMKT,QMKT                                                        
         MVC   PMKTNM,MKTNM                                                     
         B     EXIT                                                             
         SPACE                                                                  
HDHK10   MVI   HDHKSW,1                                                         
         B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         SPACE 2                                                                
EQSTAER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'EQSTAMSG),EQSTAMSG                                     
         B     ERREXIT                                                          
MKTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MKTERMSG),MKTERMSG                                     
         SR    R0,R0                                                            
         ICM   R0,3,BMKT                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+33(4),DUB                                                
         ICM   R0,3,SVBMKT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+43(4),DUB                                                
         B     ERREXIT                                                          
FLTRERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FLTRMSG),FLTRMSG                                       
         B     ERREXIT                                                          
NOSTAER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOSTAMSG),NOSTAMSG                                     
         LA    R2,TRASTA1H                                                      
ERREXIT  GOTO1 ERREX2                                                           
         DC    H'0'                                                             
BADTRSTA MVI   ERROR,NOSTAADR                                                   
         B     TRAPERR                                                          
*NVSTA   MVI   ERROR,INVSTAT                                                    
*        B     TRAPERR                                                          
PRDERR   MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
MISSCLT  LA    R2,TRACLTH          POINT TO CLT                                 
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
FLTRMSG  DC    C'* ERROR * NO FILTERS *'                                        
NOSTAMSG DC    C'* ERROR * NO STATIONS ENTERED, MUST ENTER 1 OR MORE *'         
EQSTAMSG DC    C'* ERROR * THIS STATION ALREADY ENTERED IN LIST *'              
MKTERMSG DC    C'* ERROR * THIS STATION IN MARKET XXXX, NOT YYYY *'             
         SPACE 3                                                                
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'--------------'                                           
         SSPEC H3,3,PAGE                                                        
         SSPEC H5,3,C'MEDIA'                                                    
         SSPEC H1,30,C'M A R K E T  S T A T I O N  L I S T'                     
         SSPEC H2,30,C'-----------------------------------'                     
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H8,3,C'CLIENT'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,11,C'PROD'                                                    
         SSPEC H9,11,C'----'                                                    
         SSPEC H8,17,C'MARKET'                                                  
         SSPEC H9,17,C'------'                                                  
         SSPEC H8,27,C'MARKET NAME'                                             
         SSPEC H9,27,C'--------------------'                                    
         SSPEC H8,53,C'STATION LIST'                                            
         SSPEC H9,53,C'---------------------------------------------'           
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
       ++INCLUDE SPTRSTAL                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAB7D                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
STASW    DS    XL1                                                              
SVBMKT   DS    XL2                                                              
SVKMKT   DS    XL2                                                              
SVBCLT   DS    XL2                                                              
HDHKSW   DS    XL1                                                              
FLDH     DS    XL8                                                              
FLD      DS    CL40                                                             
         SPACE                                                                  
* OFFLINE REPORT                                                                
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL4                                                              
PCLT     DS    CL3                                                              
         DS    CL4                                                              
PPROD    DS    CL3                                                              
         DS    CL3                                                              
PMKT     DS    CL4                                                              
         DS    CL5                                                              
PMKTNM   DS    CL24                                                             
         DS    CL2                                                              
PSTALST  DS    CL70                                                             
         SPACE 3                                                                
* ONLINE LIST                                                                   
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LMKT     DS    CL4                                                              
         DS    CL3                                                              
LSTALST  DS    CL54                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035SPTRA17   01/26/11'                                      
         END                                                                    
