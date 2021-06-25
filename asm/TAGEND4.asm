*          DATA SET TAGEND4    AT LEVEL 114 AS OF 07/20/12                      
*PHASE T702D4C,*                                                                
         TITLE 'T702D4 - HOLD MAINTENANCE'                                      
T702D4   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702D4,R8,R5                                                   
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         LA    R7,TWAHOLE          R7=PROGRAM SAVED STORAGE                     
         USING HOLDD,R7                                                         
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'  GET ADDRESS OF TRPACK                    
         GOTO1 CALLOV,DMCB                                                      
         MVC   ATRPACK,0(R1)                                                    
*                                                                               
         CLI   PFAID,13            IF PF13 PRESSED TO RETURN                    
         BNE   *+8                                                              
         BAS   RE,CLRELINF         CLEAR LOWER LINE INFO                        
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       ADD NOT ALLOWED EVER                         
         BE    ERRADD                                                           
         SPACE 1                                                                
         ZIC   R1,SNXTAGH          R1=L'1ST FIELD ON LOWER HALF                 
         LA    R1,SNXTAGH-8+4(R1)  R1=A(SCREEN CODE IN 1ST FIELD)               
         MVC   LWRSCR,0(R1)        SAVE LOWER HALF SCREEN CODE                  
         SPACE 1                                                                
         CLC   =C'Feeds',SNXTAG                                                 
         BNE   *+8                                                              
         MVI   LWRSCR,SCR9E                                                     
         SPACE 1                                                                
         CLC   =C'Cycle1',SNXTAG                                                
         BNE   *+8                                                              
         MVI   LWRSCR,SCR9D                                                     
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     HLDX                                                             
         SPACE 1                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     HLDX                                                             
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   HLD10                                                            
         MVI   FRSTDISP,C'Y'       FIRST TIME TO DISPLAY                        
         BAS   RE,DISPLAY          DISPLAY RECORD                               
         MVI   FRSTDISP,C'N'       NOT FIRST TIME TO DISPLAY                    
         BAS   RE,CLRELINF         CLEAR LOWER LINE INFO                        
*                                                                               
         CLI   ACTNUM,ACTUMTCH     IF UNMATCHING RECORD                         
         BNE   HLD8                                                             
         BAS   RE,UNMATCH          GO UNMATCH IT                                
         BAS   RE,DISPLAY          RE-DISPLAY RECORD                            
         B     HLDUMTCH                                                         
*                                                                               
HLD8     CLI   ACTNUM,ACTMTCH      IF MATCHING                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MATCH            GO MATCH IT                                  
         BAS   RE,DISPLAY          RE-DISPLAY RECORD                            
         B     HLDMTCH                                                          
*                                                                               
HLD10    CLI   MODE,RECDEL         OR DELETE RECORD                             
         BE    *+12                                                             
         CLI   MODE,RECREST        OR RESTORE RECORD                            
         BNE   HLD20                                                            
         BAS   RE,RDELRES          MARK RECORD APPROPRIATELY                    
         MVI   IOOPT,C'Y'          TELL CONTROLLER NOT TO DO I/O                
         B     XIT                                                              
         SPACE 1                                                                
HLD20    CLI   MODE,XRECDEL        IF RECORD 'DELETED'                          
         BE    *+12                                                             
         CLI   MODE,XRECREST       OR 'RESTORED'                                
         BNE   HLD30                                                            
         BAS   RE,FIXHLD           FIX REMAINING HOLD RECORDS                   
         MVI   IOOPT,C'N'          RESET I/O SWITCH                             
         BAS   RE,DISPLAY          RE-DISPLAY RECORD                            
         B     HLDX                                                             
*                                                                               
HLD30    CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BNE   HLD50                                                            
         CLI   THISLSEL,C'D'       AND SELECTED FOR DELETION                    
         BE    HLDX                DON'T DISPLAY YET                            
         BAS   RE,DISPLAY          ELSE DISPLAY THE RECORD                      
         CLI   ACTNUM,ACTDIS       IF ACTION IS DISPLAY                         
         BE    PGDSPMSG            GIVE MY OWN MESSAGE                          
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BE    PGDSPMSG            GIVE MY OWN MESSAGE                          
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   HLDX                                                             
         CLC   THISPG,NPAGES       AND NOT LAST PAGE                            
         BNE   PGDSPMSG            GIVE MY OWN MESSAGE                          
         B     HLDX                                                             
         SPACE 1                                                                
HLD50    CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   HLDX                                                             
         L     R4,AIO              R4=A(HOLD RECORD)                            
         USING TLNXD,R4                                                         
         XC    KEY,KEY             SET HOLD KEY (GENCON CLEARS)                 
         MVC   KEY(L'TLNXKEY),TLNXKEY                                           
         DROP  R4                                                               
         BAS   RE,CLRELINF         CLEAR LOWER LINE INFO                        
         BAS   RE,PREP             PRINT THE REPORT                             
         TM    WHEN,X'40'          IF SPOOLING NOW                              
         BZ    HLDX                                                             
         XC    CONSERV,CONSERV                                                  
         MVC   CONSERV(4),=C'$DQU'                                              
         BAS   RE,CLRLWR           CLEAR LOWER SCREEN LINES                     
*                                                                               
HLDX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
VKEY     NTR1                                                                   
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BE    *+8                                                              
         NI    SNXAGYH+4,X'DF'     SET AGENCY FIELD CHANGED                     
         SPACE 1                                                                
         TM    SNXAGYH+4,X'20'     IF CHANGED                                   
         BO    VK10                                                             
         NI    SNXNIDH+4,X'DF'     SET TO VALIDATE NEXT FIELD                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SNXAGYH),SNXAGYNH  AGENCY             
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   VK10                                                             
         MVC   AGYSTAT,TAAYSTAT                                                 
         DROP  R4                                                               
         SPACE 1                                                                
VK10     LA    R2,SNXNIDH          R2=A(NETWORK/SPOT COMML ID FIELD)            
         TM    4(R2),X'20'         IF CHANGED                                   
         BO    VK20                                                             
         NI    SNXNCLIH+4,X'DF'    SET TO VALIDATE NEXT FIELD                   
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK15                                                             
         OC    TGADID,TGADID       USE AD-ID IF WE HAVE                         
         BZ    FLDMISS                                                          
         MVC   SNXNID,TGADID                                                    
         OI    6(R2),X'80'                                                      
         B     VK20                                                             
VK15     GOTO1 ANY                                                              
         MVC   TGADID,WORK         SET GLOBAL AD-ID                             
         OC    TGADID,=12C' '                                                   
*                                                                               
*                                                                               
VK20     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXNCLIH         R2=A(NETWORK/SPOT CLIENT FIELD)              
         TM    4(R2),X'20'         IF CHANGED                                   
         BO    VK30                                                             
         NI    SNXNPRDH+4,X'DF'    SET TO VALIDATE NEXT FIELD                   
         XC    TGNCLI,TGNCLI                                                    
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 ANY                                                              
         MVC   TGNCLI,WORK         SET GLOBAS CLIENT                            
*                                                                               
VK30     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXNPRDH         R2=A(NETWORK/SPOT PRODUCT FIELD)             
         TM    4(R2),X'20'         IF CHANGED                                   
         BO    VK40                                                             
         NI    SNXMEDH+4,X'DF'     SET TO VALIDATE NEXT FIELD                   
         XC    TGNPRD,TGNPRD                                                    
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 ANY                                                              
         MVC   TGNPRD,WORK         SET GLOBAS PRODUCT                           
*                                                                               
VK40     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXMEDH          R2=A(NETWORK/SPOT MEDIA FIELD)               
         TM    4(R2),X'20'         IF CHANGED                                   
         BO    VK42                                                             
         NI    SNXUSEH+4,X'DF'     SET TO VALIDATE NEXT FIELD                   
         XC    TGMENAME,TGMENAME                                                
         CLI   5(R2),0                                                          
         BE    VK42                                                             
         GOTO1 MEDVAL,DMCB,8(R2)   SET GLOBAS MEDIA                             
         BNE   FLDINV                                                           
*                                                                               
VK42     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXUSEH          R2=A(NETWORK/SPOT USE FIELD)                 
         TM    4(R2),X'20'         IF CHANGED                                   
         BO    VK44                                                             
         NI    SNXADTEH+4,X'DF'    SET TO VALIDATE NEXT FIELD                   
         XC    TGUSCDE,TGUSCDE                                                  
         CLI   5(R2),0                                                          
         BE    VK44                                                             
         GOTO1 USEVAL,DMCB,(X'40',8(R2))                                        
         BNE   FLDINV                                                           
*                                                                               
VK44     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXADTEH         R2=A(HOLD DATE FIELD)                        
         TM    4(R2),X'20'         IF CHANGED                                   
         BO    VK50                                                             
         NI    SNXUIDH+4,X'DF'     SET TO NEXT FIELD                            
         XC    TGDATE,TGDATE                                                    
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         GOTO1 DTVAL,DMCB,TGDATE                                                
*                                                                               
VK50     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXUIDH          R2=A(USER ID FIELD)                          
         TM    4(R2),X'20'         IF CHANGED                                   
         BO    VK58                                                             
         NI    SNXCHGH+4,X'DF'     SET TO VALIDATE NEXT FIELD                   
         XC    TGUSER,TGUSER                                                    
         CLI   5(R2),0                                                          
         BE    VK58                                                             
******** GOTO1 USERVAL,DMCB,(X'10',(R2))                                        
*                                                                               
VK58     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXCHGH          R2=A(CHANGE FIELD)                           
         TM    4(R2),X'20'         IF CHANGED                                   
         BO    VK60                                                             
         NI    SNXCHGH+1,X'F7'     RETURN FIELD TO NORMAL INTENSITY             
         MVI   TGTYPE,0                                                         
         MVI   CHGERR,0            PRE CLEAR ERROR PENDING STATUS               
         CLI   5(R2),0             IF NO INPUT                                  
         BE    VK59                                                             
         BAS   RE,VKCHG            VALIDATE CHANGE REASONS                      
*                                                                               
VK59     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         BAS   RE,CLRELINF         CLEAR LOWER LINE INFO                        
*                                                                               
VK60     MVC   SVDATE,TGDATE       SAVE UNCOMPLEMENTED DATE                     
         OC    TGDATE,TGDATE       IF GLOBAS DATE SET                           
         BZ    *+10                                                             
         XC    TGDATE,XFFS         COMPLEMENT FOR READ                          
         GOTO1 RECVAL,DMCB,TLNXCDQ,(X'40',0)                                    
         MVC   TGDATE,SVDATE       RESET UNCOMPLEMENTED DATE                    
         MVI   DISPDOK,C'N'        DISPLAY OF DELETE NOT OKAY                   
         BAS   RE,CHKKEY           CHECK OKAY TO DEL/REST/MTCH/UNMTCH           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CHANGE REASONS                               
         SPACE                                                                  
VKCHG    NTR1                                                                   
         MVI   TGTYPE,0            PRE CLEAR CHANGE CODE                        
         CLC   TGUSCDE,=C'WSP'     NOT IMPLEMENTED FOR WSP YET                  
         BE    FLDINV                                                           
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(4,BLOCK)                                      
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         XR    R3,R3               R3=(DISPLACEMENT INTO FIELD)                 
         ZIC   R0,4(R1)            R0=(N'SCANNER ENTRIES)                       
         LA    R4,BLOCK            R4=A(SCANNER BLOCK)                          
         USING SCAND,R4                                                         
*                                                                               
VKCHG2   LA    RE,CHGTAB           RE=A(CHANGE DESCRIPTION TABLE)               
         USING CHGTABD,RE                                                       
*                                                                               
VKCHG5   CLI   CHGSTAT,X'FF'       TEST END OF TABLE                            
         BE    FLDINV2                                                          
         CLC   SCDATA1,CHGDESC     TEST MATCH ON DESCRIPTION                    
         BE    *+12                                                             
         LA    RE,L'CHGTAB(RE)     BUMP TO NEXT CHANGE IN TABLE                 
         B     VKCHG5              LOOP                                         
*                                                                               
         OC    TGTYPE,CHGSTAT      TURN ON STATUS BYTE                          
         ZIC   RF,SCLEN1                                                        
         LA    R3,1(R3,RF)         INCREMENT DISPLACEMENT INTO FIELD            
         LA    R4,SCANNEXT         BUMP TO NEXT SCANNER ENTRY                   
         BCT   R0,VKCHG2           LOOP                                         
         B     XIT                                                              
         DROP  R4,RE                                                            
         EJECT                                                                  
*              CHECK KEY OKAY FOR DELETE/RESTORE/MATCH/UNMATCH                  
         SPACE                                                                  
CHKKEY   NTR1                                                                   
         BAS   RE,MYRDHLD                                                       
         BNE   ERRNTFND            HOLD RECORD NOT FOUND                        
         L     R4,AIO              R4=A(HOLD RECORD)                            
         USING TLNXD,R4                                                         
*                                                                               
         CLI   ACTNUM,ACTDEL       IF DELETING                                  
         BE    CHKKEY5                                                          
         CLI   ACTNUM,ACTSEL       OR SELETED FOR DELETE                        
         BNE   CHKKEY10                                                         
         CLI   THISLSEL,C'D'                                                    
         BNE   CHKKEY10                                                         
CHKKEY5  TM    TLNXSTAT,TLNXSDEL   MUST NOT BE 'DELETED'                        
         BO    ERDELETE                                                         
         TM    TLNXSTAT,TLNXSMAT   MUST BE UNMATCHED                            
         BO    NODELETE                                                         
         TM    TLNXSTAT,TLNXSUSD   AND NOT USED                                 
         BO    NODELETE                                                         
         B     CHKKEYX                                                          
*                                                                               
CHKKEY10 CLI   ACTNUM,ACTREST      IF RESTORING                                 
         BE    CHKKEY20                                                         
         CLI   ACTNUM,ACTSEL       OR DELETED FOR RESTORE                       
         BNE   CHKKEY30                                                         
         CLI   THISLSEL,C'R'                                                    
         BNE   CHKKEY30                                                         
CHKKEY20 TM    TLNXSTAT,TLNXSDEL   MUST BE 'DELETED'                            
         BZ    ERREST                                                           
         B     CHKKEYX                                                          
*                                                                               
CHKKEY30 TM    TLNXSTAT,TLNXSDEL   ELSE ALL OTHER ACTIONS                       
         BZ    CHKKEY35            DON'T SHOW DELETED                           
         CLI   DISPDOK,C'Y'        UNLESS SPECIFICALLY ASK FOR IT               
         BE    CHKKEYX                                                          
         B     ERRNTFND                                                         
*                                                                               
CHKKEY35 CLI   ACTNUM,ACTMTCH      IF ACTION IS MATCH                           
         BNE   CHKKEY40                                                         
         TM    TLNXSTAT,TLNXSUSD   HOLD RECORD MUST BE UNUSED                   
         BO    ERRACT                                                           
         TM    TLNXSTAT,TLNXSMAT   AND UNMATCHED                                
         BO    ERRMTCH                                                          
         B     CHKKEYX                                                          
*                                                                               
CHKKEY40 CLI   ACTNUM,ACTUMTCH     IF ACTION IS UNMATCH                         
         BNE   CHKKEYX                                                          
         TM    TLNXSTAT,TLNXSUSD   HOLD RECORD MUST BE UNUSED                   
         BO    ERRACT                                                           
         TM    TLNXSTAT,TLNXSMAT   AND MUST BE MATCHED                          
         BZ    ERRUMTCH                                                         
*                                                                               
CHKKEYX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              READ FOR HOLD RECORD                                             
*                                                                               
MYRDHLD  NTR1                                                                   
         GOTO1 HIGH                READ HIGH                                    
         B     MYRDHLD5                                                         
*                                                                               
MYRDHLD2 GOTO1 SEQ                 READ SEQUENCIAL                              
*                                                                               
MYRDHLD5 CLC   KEY(TLNXNCLI-TLNXD),KEYSAVE                                      
         BNE   ERRNTFND            RECORD NOT FOUND                             
         LA    R4,KEY                                                           
         USING TLNXD,R4                                                         
*                                                                               
         OC    TGNCLI,TGNCLI       IF NETWORK/SPOT CLIENT                       
         BZ    *+14                                                             
         CLC   TLNXNCLI,TGNCLI     MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         OC    TGNPRD,TGNPRD       IF NETWORK/SPOT PRODUCT                      
         BZ    *+14                                                             
         CLC   TLNXNPRD,TGNPRD     MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         OC    TGMENAME,TGMENAME   IF MEDIA                                     
         BZ    *+14                                                             
         CLC   TLNXMED,TGMENAME    MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         OC    TGUSCDE,TGUSCDE     IF USE CODE                                  
         BZ    *+14                                                             
         CLC   TLNXUSE,TGUSCDE     MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         OC    TGDATE,TGDATE       IF DATE ADDED                                
         BZ    MYRDHLD7                                                         
         MVC   SVDATE,TGDATE       COMPLEMENT FOR MATCH                         
         XC    SVDATE,XFFS                                                      
         CLC   TLNXDATE,SVDATE     MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
MYRDHLD7 OC    TGUSER,TGUSER       IF USER ID                                   
         BZ    *+14                                                             
         CLC   TLNXUID,TGUSER      MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         CLI   TGTYPE,0            IF CHANGE CODE                               
         BE    *+14                                                             
         CLC   TLNXCCDE,TGTYPE     MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         GOTO1 GETREC              GET RECORD                                   
         BAS   RE,SETGVALS         SET KEY VALUES TO GLOBAS                     
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET GLOBASS AND SCREEN FROM KEY VALUES                
         SPACE 1                                                                
SETGVALS NTR1                                                                   
         LA    R4,KEY              R4=A(HOLD KEY)                               
         USING TLNXD,R4                                                         
         MVC   TGNCLI,TLNXNCLI     SET GLOBAS NETWORK/SPOT CLIENT               
         MVC   SNXNCLI,TLNXNCLI    DISPLAY CLIENT CODE TO SCREEN                
         OI    SNXNCLIH+6,X'80'                                                 
*                                                                               
         MVC   TGNPRD,TLNXNPRD     SET GLOBAS NETWORK/SPOT PRODUCT              
         MVC   SNXNPRD,TLNXNPRD    DISPLAY PRODUCT CODE TO SCREEN               
         OI    SNXNPRDH+6,X'80'                                                 
*                                                                               
         GOTO1 MEDVAL,DMCB,TLNXMED SET GLOBAS NETWORK/SPOT MEDIA                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SNXMED,TGMENAME     DISPLAY TO SCREEN                            
         OI    SNXMEDH+6,X'80'                                                  
*                                                                               
         GOTO1 USEVAL,DMCB,(X'40',TLNXUSE)                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SNXUSE,TGUSCDE                                                   
         OI    SNXUSEH+6,X'80'                                                  
*                                                                               
         MVC   TGDATE,TLNXDATE     SET GLOBAS DATE ADDED                        
         XC    TGDATE,XFFS         MAKE SURE UNCOMPLEMENTED                     
         GOTO1 DATCON,DMCB,(1,TGDATE),(5,SNXADTE)                               
         OI    SNXADTEH+6,X'80'    DISPLAY DATE TO SCREEN                       
*                                                                               
         MVC   TGUSER,TLNXUID      SET GLOBAS USER ID NUMBER                    
*                                                                               
         MVC   TGTYPE,TLNXCCDE     SET GLOBAS CHANGE CODE                       
         BAS   RE,DISCHG           DISPLAY TO SCREEN                            
*                                                                               
         XC    SNXMDTE,SNXMDTE                                                  
         XC    SNXUDTE,SNXUDTE                                                  
         XC    SNXVIA,SNXVIA                                                    
         XC    SNXMESS,SNXMESS                                                  
         L     R3,AIO                                                           
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R3                                                         
         MVC   TGUSERID,TANXUID    SET GLOBAS USER ID                           
         MVC   SNXUID,TANXUID      DISPLAY TO SCREEN                            
         OI    SNXUIDH+6,X'80'                                                  
         GOTO1 DATCON,DMCB,(1,TANXMDTE),(5,SNXMDTE)                             
         GOTO1 DATCON,DMCB,(1,TANXUDTE),(5,SNXUDTE)                             
         CLI   TANXTYPE,0                                                       
         BE    SETGV10                                                          
         CLI   TANXTYPE,TANXTCYC                                                
         BE    SETGV20                                                          
         MVC   BYTE,TANXTYPE                                                    
         BAS   RE,SETMVIA          RETURNS MATCHED VIA... IN WORK               
         MVC   SNXVIA,WORK                                                      
         B     SETGVX                                                           
*                                                                               
SETGV10  MVC   SNXMESS,NOTONSYS                                                 
         B     SETGVX                                                           
*                                                                               
SETGV20  MVC   SNXMESS,MISSCYC                                                  
         B     SETGVX                                                           
*                                                                               
SETGVX   OI    SNXMDTEH+6,X'80'                                                 
         OI    SNXUDTEH+6,X'80'                                                 
         OI    SNXVIAH+6,X'80'                                                  
         OI    SNXMESSH+6,X'80'                                                 
         MVI   IOOPT,C'Y'          TELL CONTROLLER I'M DOING I/O                
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE TO UNMATCH A HOLD RECORD                                 
         SPACE                                                                  
UNMATCH  NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVI   ALIASDEL,C'Y'       ALIAS DELETED                                
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
*                                                                               
         L     R6,AIO              R6=A(HOLD RECORD)                            
         USING TLNXD,R6                                                         
*                                                                               
         NI    TLNXSTAT,X'FF'-TLNXSMAT SET UNMATCHED IN RECORD                  
*                                                                               
         LR    R3,R6                                                            
         MVI   ELCODE,TANXELQ      GET NETWORK/SPOT TRANSFER DETAILS            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R3                                                         
         XC    TANXMDTE,TANXMDTE   CLEAR MATCHED TODAY                          
         XC    TANXCOM,TANXCOM     CLEAR INTERNAL COMMERCIAL NUMBER             
         MVC   OLDCTYPE,TANXTYPE   SAVE MATCH TYPE                              
         MVI   TANXTYPE,0          CLEAR COMMERCIAL TYPE                        
*                                                                               
         USING TANPD,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TANPELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
UM10     BAS   RE,NEXTEL                                                        
         BNE   UM20                                                             
         CLI   TANPLEN,TANPLNQS                                                 
         BL    UM10                                                             
         XC    TANPCYCS(11),TANPCYCS                                            
         B     UM10                                                             
         DROP  R3                                                               
*                                                                               
UM20     GOTO1 ACTVIN,DMCB,0       ADD LAST CHANGED                             
*                                                                               
         BAS   RE,MYPUTREC         PUTREC/FORCE CHANGE ALL/ACTIVEPTR            
         GOTO1 ADDPTRS,DMCB,(X'A0',PTRBLK)                                      
*                                                                               
         CLI   OLDCTYPE,TANXTAKA   IF PREVIOUSLY MATCHED VIA ALIAS              
         BE    *+12                                                             
         CLI   OLDCTYPE,TANXTALF   OR ALIAS(LIFT)                               
         BNE   *+8                                                              
         BAS   RE,DELALIAS         DELETE THE ALIAS RECORD                      
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         BAS   RE,SETGVALS                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO DELETE ALIAS RECORD IF NO OTHER                       
*              HOLD RECORDS ARE USING ALIAS CONNECTION                          
         SPACE                                                                  
DELALIAS NTR1                                                                   
         MVC   AIO,AIO2            READ HIGH FOR HOLD RECORDS                   
         XC    KEY,KEY             UP TO AND INCLUDING MEDIA                    
         LA    R4,KEY                                                           
         USING TLNXD,R4                                                         
         MVI   TLNXCD,TLNXCDQ                                                   
         MVC   TLNXAGY,TGAGY                                                    
         MVC   TLNXNID,TGADID                                                   
         MVC   TLNXNCLI,TGNCLI                                                  
         MVC   TLNXNPRD,TGNPRD                                                  
         MVC   TLNXMED,TGMENAME                                                 
         GOTO1 HIGH                                                             
         B     DELAL20                                                          
*                                                                               
DELAL10  GOTO1 SEQ                                                              
*                                                                               
DELAL20  CLC   KEY(TLNXUSE-TLNXKEY),KEYSAVE                                     
         BNE   DELAL30             DELETE ALIAS - NO HOLD RECORDS FND           
         CLC   TLNXKEY,SVKEY       SKIP MYSELF                                  
         BE    DELAL10                                                          
         CLI   TLNXSEQ,0           SKIP NON-BASE RECORDS                        
         BNE   DELAL10                                                          
         CLI   KEY+TLDRSTAT-TLDRD,0 SKIP UNMATCHED RECORDS                      
         BE    DELAL10                                                          
         TM    KEY+TLDRSTAT-TLDRD,TLNXSDEL+TLNXSUSD SKIP DEL/USED               
         BNZ   DELAL10                                                          
         MVI   ALIASDEL,C'N'       DON'T DELETE ALIAS                           
         B     DELALX              HOLD RECORDS FOUND                           
*                                                                               
DELAL30  XC    DUB,DUB             PACK ADID AND TRY AGAIN                      
         GOTO1 ATRPACK,DMCB,(C'P',TGADID),DUB                                   
         XC    KEY,KEY             READ HIGH FOR PACKED HOLD RECORDS            
         LA    R4,KEY              UP TO AND INCLUDING MEDIA                    
         USING TLNXD,R4                                                         
         MVI   TLNXCD,TLNXCDQ                                                   
         MVC   TLNXAGY,TGAGY                                                    
         MVC   TLNXNID,DUB         PACKED ADID                                  
         MVC   TLNXNCLI,TGNCLI                                                  
         MVC   TLNXNPRD,TGNPRD                                                  
         MVC   TLNXMED,TGMENAME                                                 
         GOTO1 HIGH                                                             
         B     DELAL50                                                          
*                                                                               
DELAL40  GOTO1 SEQ                                                              
*                                                                               
DELAL50  CLC   KEY(TLNXUSE-TLNXKEY),KEYSAVE                                     
         BNE   DELAL60             DELETE ALIAS - NO HOLD RECORDS FND           
         CLC   TLNXKEY,SVKEY       SKIP MYSELF                                  
         BE    DELAL40                                                          
         CLI   TLNXSEQ,0           SKIP NON-BASE RECORDS                        
         BNE   DELAL40                                                          
         CLI   KEY+TLDRSTAT-TLDRD,0 SKIP UNMATCHED RECORDS                      
         BE    DELAL40                                                          
         TM    KEY+TLDRSTAT-TLDRD,TLNXSDEL+TLNXSUSD SKIP DEL/USED               
         BNZ   DELAL40                                                          
         MVI   ALIASDEL,C'N'       DON'T DELETE ALIAS                           
         B     DELALX              HOLD RECORDS FOUND                           
*                                                                               
DELAL60  GOTO1 RECVAL,DMCB,TLAKCDQ,(X'B0',0)  READ ALIAS FOR UPDATE             
         BNE   DELALX                                                           
*                                                                               
         L     R4,AIO                                                           
         USING TLAKD,R4                                                         
         OI    TLAKSTAT,X'80'      MARK ALIAS RECORD DELETED                    
         GOTO1 PUTREC                                                           
         OI    KEY+TLDRSTAT-TLDRD,X'80' MARK KEY DELETED                        
         GOTO1 WRITE                                                            
*                                                                               
DELALX   MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO MATCH A HOLD RECORD                                   
MATCH    NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         XC    NEWCOM,NEWCOM       PRE-CLEAR NEW INTERNAL COMM #                
         MVI   NEWCTYPE,0          PRE-CLEAR NEW COMMERCIAL TYPE                
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         L     R6,AIO              R6=A(HOLD RECORD)                            
         USING TLNXD,R6                                                         
*                                                                               
         CLI   TLNXCCDE,0          IF RECORD IS A CHANGE                        
         BE    MTCH10                                                           
         TM    TLNXCCDE,TANXCAIR   IF NOT AIRED                                 
         BO    ERRMTCH5                                                         
         TM    TLNXCCDE,TANXCUNM   OR IF UNMARKED                               
         BO    ERRMTCH4            NEVER ALLOWED TO BE MATCHED                  
*                                                                               
         TM    CHGERR,CHGEOVR      AND NOT PREVIOUSLY OVERRIDDEN                
         BO    MTCH10                                                           
         TM    CHGERR,CHGEPEND     TEST IF ERROR PENDING                        
         BZ    ERRMTCH3                                                         
         CLI   PFAID,20            TEST PF20 PRESSED                            
         BNE   ERRMTCH3            IF NOT, REGIVE MESSAGE                       
         NI    CHGERR,X'FF'-CHGEPEND                                            
         OI    CHGERR,CHGEOVR      SET OVERRODE ERROR PENDING                   
MTCH10   NI    SNXCHGH+1,X'F7'     RETURN FIELD TO NORMAL INTENSITY             
         OI    SNXCHGH+6,X'80'     TRANSMIT                                     
*                                                                               
         BAS   RE,GETMTCH          CHECK NWK ID A MATCH                         
         BNE   MTCH20                                                           
         BAS   RE,CHKINP           AUTO MATCH POSSIBLE - CHECK INPUT            
         BRAS  RE,CBLDATES                                                      
         B     MTCH30              NO/SAME INPUT - GO MATCH                     
*                                                                               
MTCH20   LA    R2,SNXTCIDH         R2=A(TALENT COMMERCIAL ID)                   
         XC    TGCID,TGCID                                                      
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SNXTCNMH                       
         MVC   AIO,AIO1                                                         
         L     RE,AIO2             RE=A(TALENT COMMERCIAL RECORD)               
         USING TLCOD,RE                                                         
         MVC   NEWCOM,TLCOCOM                                                   
         MVI   NEWCTYPE,TANXTAKA   MATCHING VIA ALIAS                           
         DROP  RE                                                               
*                                                                               
         LA    R2,SNXPLFTH         R2=A(PAY LIFT FIELD)                         
         GOTO1 ANY                 Y/N INPUT REQUIRED                           
         CLI   5(R2),1                                                          
         BNE   FLDINV                                                           
         CLI   WORK,C'N'                                                        
         BE    *+16                                                             
         CLI   WORK,C'Y'                                                        
         BNE   FLDINV                                                           
         MVI   NEWCTYPE,TANXTALF   MATCHING VIA ALIAS(LIFT)                     
         BRAS  RE,CBLDATES                                                      
         BAS   RE,NEWALIAS         ADD NEW ALIAS RECORD                         
*                                                                               
MTCH30   OI    TLNXSTAT,TLNXSMAT   SET MATCHED STATUS AND ADD                   
         BAS   RE,SETTANXD         SET TANXD INFO                               
         GOTO1 ACTVIN,DMCB,0       ADD LAST CHANGED                             
         BAS   RE,MYPUTREC         PUTREC/FORCE CHANGE ALL/ACTIVE PTR           
         GOTO1 ADDPTRS,DMCB,(X'A0',PTRBLK)                                      
         MVC   KEY,SVKEY           RESTORE KEY                                  
         BAS   RE,SETGVALS                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK WHETHER NET/SPOT COMML ID IS ITSELF A           
*              TALENT COMML, LIFT ID , ALIAS, OR ALIAS LIFT                     
*                                  XIT - CC NEQ - NO MATCH                      
*                                        CC EQ  - NEWCOM,NEWCTYPE SET           
         SPACE                                                                  
GETMTCH  NTR1                                                                   
         MVC   AIO,AIO2            SET IOAREA FOR TAL COMML RECORD              
*                                                                               
         L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
*                                                                               
         BAS   RE,CHKNID           FIND MATCH                                   
         BNE   GETMTCHX                                                         
*                                                                               
         L     R3,AIO              R3=A(TALENT COMMERCIAL RECORD)               
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R3                                                         
         MVC   TGCID,TACOCID       SET TGCID                                    
         CR    RB,RB                                                            
*                                                                               
GETMTCHX MVC   AIO,AIO1            RESET IOAREA                                 
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
       ++INCLUDE TACHKNID                                                       
         DROP  R6                                                               
         EJECT                                                                  
*        ROUTINE TO CHECK TAL CID AND PAY LIFT INPUT                            
*        NID IS A MATCH SO MUST BE NO/SAME INPUT                                
         SPACE                                                                  
CHKINP   NTR1                                                                   
         LA    R2,SNXTCIDH         R2=A(TALENT CID FIELD)                       
         CLI   5(R2),0             IF INPUT - MUST BE CORRECT                   
         BE    CHKINP10                                                         
         GOTO1 ANY                                                              
         CLC   TGCID,WORK                                                       
         BNE   ERRMTCH2            INCORRECT GIVE ERROR                         
*                                                                               
CHKINP10 LA    R2,SNXPLFTH         R2=A(PAY LIFT FIELD)                         
         CLI   5(R2),0             IF INPUT - MUST BE CORRECT                   
         BE    CHKINPX                                                          
         GOTO1 ANY                                                              
         CLI   NEWCTYPE,TANXTLFT   IF MATCH TO LIFT ID                          
         BE    *+12                                                             
         CLI   NEWCTYPE,TANXTALF   OR ALIAS(LIFT)                               
         BNE   CHKINP20                                                         
         CLI   5(R2),1                                                          
         BNE   FLDINV                                                           
         CLI   WORK,C'Y'           THEN INPUT MUST BE A 'Y'                     
         BE    CHKINPX                                                          
         B     ERRMTCH2                                                         
*                                                                               
CHKINP20 CLI   WORK,C'Y'           ELSE INPUT MUST BE A 'N'                     
         BE    ERRMTCH2                                                         
*                                                                               
CHKINPX  B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET MATCHED DATE, INTERNAL COMMERCIAL NUMBER          
*              AND COMMERCIAL TYPE IN TANXD                                     
         SPACE                                                                  
SETTANXD NTR1                                                                   
         L     R3,AIO                                                           
         MVI   ELCODE,TANXELQ      GET NETWORK/SPOT TRANSFER DETAILS            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R3                                                         
         MVC   TANXMDTE,TGTODAY1   SET MATCHED TODAY                            
         MVC   TANXCOM,NEWCOM      SET INTERNAL COMMERCIAL NUMBER               
         MVC   TANXTYPE,NEWCTYPE   SET COMMERCIAL TYPE                          
         MVC   TANXVERS,NEWVER     SET VERSION LETTER                           
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO ADD NEW ALIAS RECORD (COPY TAFNDS FROM HLD)           
         SPACE                                                                  
NEWALIAS NTR1                                                                   
         MVC   AIO,AIO2            SET IOAREA                                   
         GOTO1 RECVAL,DMCB,TLAKCDQ,(X'40',0)                                    
         LA    R6,KEY                                                           
         USING TLAKD,R6                                                         
         MVC   TLAKCOM,NEWCOM                                                   
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         CLC   KEY(L'TLAKKEY),KEYSAVE   IF KEY EXISTS                           
         BNE   NEWAL20                                                          
         TM    KEY+TLDRSTAT-TLDRD,X'80' AND IS NOT DELETED                      
         BZ    NEWALX                   SHOULD NEVER HAPPEN                     
*                                                                               
         NI    KEY+TLDRSTAT-TLDRD,X'7F' UNDELETE POINTER                        
         CLI   NEWCTYPE,TANXTALF                                                
         BE    *+12                                                             
         NI    KEY+TLDRSTAT-TLDRD,X'FF'-TLAKSLFT                                
         B     *+8                                                              
         OI    KEY+TLDRSTAT-TLDRD,TLAKSLFT                                      
         GOTO1 WRITE                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO              SET NEW STATUS IN RECORD                     
         MVC   TLAKSTAT,KEY+TLDRSTAT-TLDRD                                      
         GOTO1 PUTREC                                                           
         B     NEWALX                                                           
*                                                                               
NEWAL20  MVC   KEY,KEYSAVE                                                      
         L     R6,AIO                                                           
         MVC   TLAKKEY,KEY                                                      
         MVC   TLAKLEN,DATADISP                                                 
         XC    TLAKSTAT(10),TLAKSTAT                                            
         CLI   NEWCTYPE,TANXTALF   IF ALIAS(LIFT)                               
         BNE   *+8                                                              
         OI    TLAKSTAT,TLAKSLFT   SET STATUS                                   
*                                                                               
         MVC   AIO,AIO1            IOAREA FOR HOLD RECORD                       
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTCLI))                                     
         BNE   *+8                                                              
         BAS   RE,CPYELE           COPY ELEMENT TO ALIAS RECORD                 
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTPRD))                                     
         BNE   *+8                                                              
         BAS   RE,CPYELE           COPY ELEMENT TO ALIAS RECORD                 
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTTTL))                                     
         BNE   *+8                                                              
         BAS   RE,CPYELE           COPY ELEMENT TO ALIAS RECORD                 
         MVC   AIO,AIO2            RESET IOAREA                                 
*                                                                               
         GOTO1 ACTVIN,DMCB,0                                                    
         GOTO1 ADDREC              ADD THE RECORD                               
*                                                                               
NEWALX   MVC   AIO,AIO1            RESET IOAREA                                 
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
*              ROUTINE TO COPY ELEMENT FROM HOLD RECORD TO                      
*              ALIAS RECORD                                                     
         SPACE                                                                  
CPYELE   NTR1                                                                   
         L     R4,TGELEM           R4=A(ELEMENT TO ADD TO ALIAS REC)            
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT,0(R4)                                                    
         MVC   AIO,AIO2            IOAREA FOR ALIAS RECORD                      
         GOTO1 ADDELEM                                                          
         MVC   AIO,AIO1            RESET IOAREA FOR HOLD RECORD                 
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO WRITE BACK RECORD                                     
         SPACE                                                                  
MYPUTREC NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         MVC   TLDRDA,SVKEY+TLDRDA-TLDRKEY                                      
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              PROTECT AGAINST PUTREC DRAMA                 
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 PUTREC                                                           
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY THE KEY                                       
         SPACE 1                                                                
DKEY     NTR1                                                                   
         CLC   SVKEY,KEY           IF KEY CHANGED                               
         BE    *+8                                                              
         BAS   RE,CLRELINF         CLEAR LOWER LINE INFO                        
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVI   DISPDOK,C'N'        DISPLAY OF DELETE NOT OKAY                   
         L     R6,AIO              R6=A(HOLD RECORD)                            
         USING TLNXD,R6                                                         
         LR    R3,R6                                                            
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R3                                                         
*                                                                               
         MVC   SNXAGY,TLNXAGY      AGENCY CODE                                  
         OI    SNXAGYH+6,X'80'                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SNXAGYH),SNXAGYNH   AGY NAME          
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    TGADID,TGADID                                                    
         MVC   SNXNID,=CL12' '                                                  
         TM    TLNXSTAT,TLNXADID+TLNXPACK   AD-ID COMML OR PACKED?              
         BNZ   DK10                                                             
         MVC   SNXNID(8),TLNXNID   NETWORK/SPOT COMML ID                        
         MVC   TGADID(L'TLNXNID),TLNXNID                                        
         B     DK20                                                             
DK10     MVC   SNXNID,TANXADID                                                  
         MVC   TGADID,TANXADID                                                  
*                                                                               
DK20     OI    SNXNIDH+6,X'80'                                                  
         OC    TGADID,=12C' '                                                   
         MVC   SNXNCLI,TLNXNCLI    NETWORK/SPOT CLIENT CODE                     
         OI    SNXNCLIH+6,X'80'                                                 
         MVC   TGNCLI,TLNXNCLI                                                  
         MVC   SNXNPRD,TLNXNPRD    NETWORK/SPOT PRODUCT CODE                    
         OI    SNXNPRDH+6,X'80'                                                 
         MVC   TGNPRD,TLNXNPRD                                                  
         GOTO1 MEDVAL,DMCB,TLNXMED NETWORK/SPOT MEDIA CODE                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SNXMED,TGMENAME                                                  
         OI    SNXMEDH+6,X'80'                                                  
         GOTO1 USEVAL,DMCB,(X'40',TLNXUSE)                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SNXUSE,TGUSCDE      NETWORK/SPOT USE CODE                        
         OI    SNXUSEH+6,X'80'                                                  
*                                                                               
         MVC   TGDATE,TLNXDATE                                                  
         XC    TGDATE,XFFS         UNCOMPLEMENT DATE                            
         GOTO1 DATCON,DMCB,(1,TGDATE),(5,SNXADTE)                               
         OI    SNXADTEH+6,X'80'                                                 
         MVC   TGUSER,TLNXUID                                                   
         MVC   TGUSERID,TANXUID                                                 
         MVC   SNXUID,TANXUID      USER ID FROM WORKER FILE                     
         OI    SNXUIDH+6,X'80'     TRAMSIT                                      
*                                                                               
         MVC   TGTYPE,TLNXCCDE                                                  
         BAS   RE,DISCHG           DISPLAY CHANGE CODE                          
*                                                                               
         TM    TLNXSTAT,TLNXSDEL   IF RECORD DELETED                            
         BZ    *+8                                                              
         MVI   DISPDOK,C'Y'        DISPLAY OF DELETE OKAY                       
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         BAS   RE,CHKKEY           CHECK OKAY TO DEL/REST/MTCH/UNMTCH           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY CHANGE FIELD                                  
         SPACE                                                                  
DISCHG   NTR1                                                                   
         XC    SNXCHG,SNXCHG       PRE CLEAR CHANGE FIELD                       
         OI    SNXCHGH+6,X'80'                                                  
*                                                                               
         XR    RF,RF               FIRST TIME INDICATOR                         
         LA    R2,SNXCHG           R2=A(CHANGE FIELD)                           
         LA    RE,CHGTAB           RE=A(CHANGE DESCRIPTION TABLE)               
         USING CHGTABD,RE                                                       
*                                                                               
DISCHG5  CLI   CHGSTAT,X'FF'       TEST END OF TABLE                            
         BE    DISCHGX                                                          
         MVC   BYTE,TGTYPE                                                      
         NC    BYTE,CHGSTAT        TEST MATCH ON CHANGE                         
         BZ    DISCHG10                                                         
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         MVI   0(R2),C','          SEPERATE CHANGES                             
         LA    R2,1(R2)            BUMP TO NEXT SCREEN POSITION                 
         LA    RF,1(RF)            SET NOT FIRST TIME INDICATOR                 
         MVC   0(L'CHGDESC,R2),CHGDESC MOVE CHANGE TO SCREEN                    
         LA    R2,L'CHGDESC-1(R2)  BUMP TO NEXT SCREEN POSITION                 
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
*                                                                               
DISCHG10 LA    RE,L'CHGTAB(RE)     BUMP TO NEXT CHANGE IN TABLE                 
         B     DISCHG5             LOOP                                         
*                                                                               
DISCHGX  B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         BAS   RE,SETOVNUM         SET OVERLAY TO CORRECT LOWER SCREEN          
         BAS   RE,GETSCRN          INSURE CORRECT LOWER SCRN IS LOADED          
*                                                                               
         BAS   RE,CLRLWR           CLEAR THE LOWER SCREEN                       
         NI    SNXCHGH+1,X'F7'     RETURN FIELD TO NORMAL INTENSITY             
         OI    SNXCHGH+6,X'80'     AND TRANSMIT                                 
*                                                                               
         BAS   RE,DISHDR           DISPLAY HEADER INFORMATION                   
*                                                                               
         CLI   LWRSCR,SCR9E        IF CLA SCREEN LOADED                         
         BNE   DISP10                                                           
         BAS   RE,DISCLA           DISP USE LINES(AIO HAS LAST REC)             
         B     DISP30                                                           
*                                                                               
DISP10   CLI   LWRSCR,SCR9D        IF CBL SCREEN LOADED                         
         BNE   DISP20                                                           
         BAS   RE,DISCBL           DISP CBL LINES (AIO HAS LAST REC)            
         B     DISP30                                                           
*                                                                               
DISP20   DC    H'00'                                                            
*                                                                               
DISP30   OI    SNXTCIDH+4,X'20'    SET TALENT CID VALIDATED                     
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLDRKEY),SVKEY                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC              RESTORE IOAREA                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET OVERLAY OF LOWER SCREEN TO LOAD                   
         SPACE 1                                                                
SETOVNUM NTR1                                                                   
         MVI   OVERLAY,SCR9E       USE TASCR9E                                  
         CLC   TGUSCDE,=C'CLA'     FOR CLASS A                                  
         BE    XIT                                                              
         CLC   TGUSCDE,=C'PAX'     PAX                                          
         BE    XIT                                                              
         CLC   TGUSCDE,=C'ITN'     ITN                                          
         BE    XIT                                                              
         CLC   TGUSCDE(2),=C'LN'   AND LATE NIGHT USES                          
         BE    XIT                                                              
         SPACE 1                                                                
         MVI   OVERLAY,SCR9D       USE TASCR9D                                  
         CLC   TGUSCDE,=C'CBL'     FOR CABLE USES                               
         BE    XIT                                                              
         CLC   TGUSCDE,=C'WSP'                                                  
         BE    XIT                                                              
         CLC   TGUSCDE,=C'LCB'                                                  
         BE    XIT                                                              
         SPACE 1                                                                
         MVI   OVERLAY,SCR9F       ELSE, USE WILDSPOT SCREEN                    
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE INSURES CORRECT LOWER SCREEN IS LOADED                   
*                                  OVERLAY=CORRECT SCREEN CODE                  
         SPACE 1                                                                
GETSCRN  NTR1                                                                   
         CLC   OVERLAY,LWRSCR      TEST CORRECT SCREEN IS LOADED                
         BE    GSCRX                                                            
         MVC   LWRSCR,OVERLAY      SAVE SCREEN CODE TO BE LOADED                
         MVI   TGBYTE,C'N'                                                      
         CLC   SNXLAST+1(2),=X'0101'                                            
         BNE   *+8                                                              
         MVI   TGBYTE,C'Y'                                                      
         LA    R3,SNXTAGH          SET TO LOAD APPROPRIATE SCREEN               
         GOTO1 LOADSOPH,DMCB,1                                                  
         XR    R1,R1                                                            
GSCR8    OI    6(R3),X'80'         TRANSMIT                                     
         OI    7(R3),X'80'         AND RE-BUILD                                 
         ICM   R1,1,0(R3)                                                       
         BZ    *+10                                                             
         AR    R3,R1                                                            
         B     GSCR8                                                            
         SPACE 1                                                                
         CLI   TGBYTE,C'Y'                                                      
         BE    *+14                                                             
         CLC   TWASCR,MYSCR        IF THIS IS FIRST TIME FOR SCREEN             
         BE    *+16                                                             
         MVC   1(2,R3),=X'0101'    INSURE WE TRANSMIT ALL                       
         MVC   MYSCR,TWASCR                                                     
*                                                                               
GSCRX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY HEADER INFORMATION                            
DISHDR   NTR1                                                                   
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTTTL))                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,TGELEM                                                        
         USING TAFND,R3                                                         
         ZIC   R2,TAFNLEN                                                       
         SH    R2,=Y(TAFNLNQ)                                                   
         GOTO1 CHOPPER,DMCB,((R2),TAFNNAME),(38,BLOCK),2                        
         MVC   SNXNIDN,BLOCK                                                    
         OI    SNXNIDNH+6,X'80'                                                 
         MVC   SNXNDN2,BLOCK+38                                                 
         OI    SNXNDN2H+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,SNXNCLNH,TAFNTCLI                           
         GOTO1 CHAROUT,DMCB,TAFNELQ,SNXNPRNH,TAFNTPRD                           
*                                                                               
         XC    SNXMDTE,SNXMDTE     CLEAR MATCHED & USED DATES                   
         OI    SNXMDTEH+6,X'80'                                                 
         XC    SNXUDTE,SNXUDTE                                                  
         OI    SNXUDTEH+6,X'80'                                                 
         XC    SNXVIA,SNXVIA       CLEAR MATCHED VIA                            
         OI    SNXVIAH+6,X'80'                                                  
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,TANXELQ      NETWORK/SPOT DETAILS ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   DISHDR30                                                         
         USING TANXD,R3                                                         
         MVC   SNXUID,TANXUID                                                   
         EDIT  TANXSEC,(3,SNXLEN),ALIGN=LEFT  LENGTH IN SECONDS                 
         OI    SNXLENH+6,X'80'                                                  
         OC    TANXMDTE,TANXMDTE   IF MATCHED                                   
         BZ    DISHDR10                                                         
         GOTO1 DATCON,DMCB,(1,TANXMDTE),(5,SNXMDTE)                             
DISHDR10 OC    TANXUDTE,TANXUDTE   IF USED                                      
         BZ    DISHDR20                                                         
         GOTO1 DATCON,DMCB,(1,TANXUDTE),(5,SNXUDTE)                             
*                                                                               
DISHDR20 CLI   ACTNUM,ACTMTCH      IF MATCHING                                  
         BNE   *+12                                                             
         CLI   FRSTDISP,C'Y'       AND DISPLAYING FOR FIRST TIME                
         BE    DISHDR30            SKIP TCID/PAY LIFT/CHANGE                    
         BAS   RE,DISTCID                                                       
         BAS   RE,DISPLFT                                                       
*                                                                               
DISHDR30 CLI   LWRSCR,SCR9E                                                     
         BNE   DISHDR40                                                         
         GOTO1 FLDVAL,DMCB,(1,SCLFEEDH),1                                       
*                                                                               
DISHDR40 CLI   LWRSCR,SCR9D                                                     
         BNE   DISHDRX                                                          
         XC    SCBUSE1,SCBUSE1                                                  
         OI    SCBUSE1H+6,X'80'                                                 
         XC    SCBUSE2,SCBUSE2                                                  
         OI    SCBUSE2H+6,X'80'                                                 
         CLC   SNXUSE,=C'CBL'                                                   
         BNE   DISHDR50                                                         
         MVC   SCBUSE1,NUSES                                                    
         MVC   SCBUSE2,NUSES                                                    
DISHDR50 CLI   ACTNUM,ACTMTCH                                                   
         BE    DISHDRX                                                          
         GOTO1 FLDVAL,DMCB,(1,SCBCYC1H),(X'80',SCBLEN3H)                        
DISHDRX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY TALENT CID                                    
         USING TANXD,R3            R3=A(TANXD ELEMENT)                          
DISTCID  NTR1                                                                   
         XC    SNXTCID,SNXTCID     CLEAR TALENT CID                             
         OI    SNXTCIDH+6,X'80'                                                 
         XC    SNXTCNM,SNXTCNM     TALENT TITLE                                 
         OI    SNXTCNMH+6,X'80'                                                 
         XC    TGCID,TGCID         CLEAR GLOBAS TALENT CID                      
*                                                                               
         OC    TANXCOM,TANXCOM     IF TALENT COMMERCIAL                         
         BZ    DISTCDX                                                          
*                                                                               
         MVC   BYTE,TANXTYPE                                                    
         BAS   RE,SETMVIA          RETURNS MATCHED VIA... IN WORK               
         MVC   SNXVIA,WORK                                                      
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TANXCOM)                             
         BNE   DISTCD80                                                         
         BAS   RE,SETTGCID                                                      
*                                                                               
         CLI   TANXTYPE,TANXTTAL   IF MATCHED VIA LIFT/ALIAS                    
         BE    DISTCD80                                                         
         MVC   SNXTCID,TGCID       DISPLAY TALENT CID AND NAME                  
         GOTO1 CHAROUT,DMCB,TANAELQ,SNXTCNMH                                    
DISTCD80 MVC   AIO,AIO1            RESET IOAREA                                 
         MVC   KEY,SVKEY           RESET KEY TOO                                
DISTCDX  B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
*              ROUTINE TO SET MATCHED VIA ....                                  
*                                  NTRY - BYTE = MATCHED TYPE                   
*                                  XIT -  WORK = MATCHED WORD                   
SETMVIA  NTR1                                                                   
         XC    WORK,WORK           CLEAR WORK FOR RETURN                        
         LA    RE,MTCHTAB          RE=A(MATCH TABLE)                            
*                                                                               
SETMVIA5 CLI   0(RE),X'FF'         TEST END OF TABLE                            
         BE    SETMVIAX                                                         
         CLC   BYTE,0(RE)          TEST MATCHED TYPE                            
         BE    *+12                                                             
         LA    RE,L'MTCHTAB(RE)                                                 
         B     SETMVIA5                                                         
         MVC   WORK(8),1(RE)       SET MATCHED VIA ....                         
SETMVIAX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET TGCID                                             
         SPACE                                                                  
SETTGCID NTR1                                                                   
         L     R3,AIO              R3=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R3                                                         
         MVC   TGCID,TACOCID                                                    
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
*              ROUTINE TO DISPLAY PAY LIFT FIELD                                
         USING TANXD,R3            R3=A(TANXD ELEMENT)                          
         SPACE                                                                  
DISPLFT  NTR1                                                                   
         XC    SNXPLFT,SNXPLFT     CLEAR PAY LIFT?                              
         OI    SNXPLFTH+6,X'80'                                                 
*                                                                               
         CLI   TANXTYPE,0          IF MATCH COMMERCIAL TYPE                     
         BE    DISPLFTX                                                         
*                                                                               
         MVI   SNXPLFT,C'N'                                                     
         CLI   TANXTYPE,TANXTLFT   IF LIFT ID OR                                
         BE    *+12                                                             
         CLI   TANXTYPE,TANXTALF   ALIAS(LIFT)                                  
         BNE   *+8                                                              
         MVI   SNXPLFT,C'Y'        INDICATE SO                                  
         OI    SNXPLFTH+6,X'80'                                                 
*                                                                               
         CLI   TANXTYPE,TANXTVER   IF VERSION                                   
         BE    DLFT10                                                           
         CLI   TANXTYPE,TANXTALV   IF VERSION ALIAS                             
         BNE   DISPLFTX                                                         
DLFT10   MVC   SNXPLFT(1),TANXVERS INDICATE VERSION                             
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    DISPLFTX                                                         
         EDIT  TANXVERS,SNXPLFT,ALIGN=LEFT                                      
DISPLFTX B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE DISPLAYS CLA USE LINES                                   
         SPACE 1                                                                
DISCLA   NTR1                                                                   
         GOTO1 ACTVOUT,DMCB,SCLACTVH  LAST CHANGED INFO                         
         SPACE 1                                                                
         L     R6,AIO              R6 = A(RECORD)                               
         USING TLNXD,R6                                                         
         SPACE 1                                                                
         BAS   RE,SETSTART         SET STARTING POINT                           
         L     R3,FULL             RETURNS A(EL) IN FULL                        
         USING TANPD,R3            R3=A(CLA USE ELEMENT)                        
         SPACE 1                                                                
         LA    R2,SCLL1H           R2=A(1ST USE LINE)                           
         USING LINED,R2                                                         
         LA    R4,MXLINES          R4=N'LINES                                   
         MVC   CLAFRST,TANPEL      SAVE KEY OF FIRST EL. DISPLAYED              
         MVC   CLAFRST+TANPLNQ(1),TLNXSEQ  AND SEQ OF CORRES. RECORD            
*                                                                               
DCLA60   MVC   CLALST,TANPEL       SAVE KEY OF LAST EL. DISPLAYED               
         MVC   CLALST+TANPLNQ(1),TLNXSEQ  AND SEQ OF CORRES. RECORD             
         BAS   RE,DSPCLALN         DISPLAY ONE USE LINE                         
*                                                                               
         CLI   TGCTSTTY,TASTTYPP   IF PROGRAMMER, DISPLAY SEQ.                  
         BNE   DCLA70                                                           
         EDIT  (1,TLNXSEQ),(1,LINSEQ),ZERO=NOBLANK                              
*                                                                               
DCLA70   BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    *+12                                                             
         BAS   RE,GETNXT           GET NEXT RECORD SETS (R3)                    
         BNE   DCLAX                                                            
         LA    R2,LINNEXT          BUMP TO NEXT LINE                            
         BCT   R4,DCLA60                                                        
*                                                                               
DCLAX    BAS   RE,PGIT             CALC THIS PAGE BASED ON LAST ELE             
         MVC   THISPG,BYTE         DISPLAYED                                    
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
*              ROUTINE TO DISPLAY ONE CLA USE LINE                              
         SPACE 1                                                                
         USING TANPD,R3                                                         
DSPCLALN NTR1                                                                   
         GOTO1 DATCON,DMCB,(1,TANPDATE),(5,LINUDTE)                             
         MVC   LINPGM,TANPPNME     PROGRAM NAME                                 
         MVC   LINNTWRK,TANPNWK    NETWORK CODE                                 
         TM    TANPSTAT,TANPMR     IF MULTI-RUN                                 
         BZ    *+10                                                             
         MVC   LINCHG,=CL3'MR'     INDICATE SO                                  
*                                                                               
         GOTOR DSPFEED,DMCB,LINFEED DISPLAY FEED                                
*                                                                               
         CLI   TANPAIR,0           ANY REASON FOR NOT AIRED                     
         BE    XIT                                                              
         MVC   LINCHG,=CL3'*PE'    PRE-EMPTED                                   
         CLI   TANPAIR,TANPEMPT                                                 
         BE    XIT                                                              
         MVC   LINCHG,=CL3'*MI'    MISSED                                       
         CLI   TANPAIR,TANPMISD                                                 
         BE    XIT                                                              
         MVC   LINCHG,=CL3'*DE'    DELETED                                      
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY FEED CODES                                    
         USING TANPD,R3                                                         
DSPFEED  NTR1                                                                   
         L     R2,0(R1)                                                         
         SPACE 1                                                                
         L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
         SPACE 1                                                                
         CLI   TANPLEN,TANPLNQ3    IF NEW ELEMENT LENGTH                        
         BL    XIT                                                              
         OC    TANPFEED,SPACES     AND FEED CODE IS PRESENT                     
         CLC   TANPFEED,SPACES                                                  
         BE    XIT                                                              
         SPACE 1                                                                
         LHI   R4,1                INITIALIZE COUNTER                           
         SPACE 1                                                                
         LA    RE,SCLFEED          RE=A(FEED CODE SCREEN LINE)                  
         ZIC   RF,SCLFEEDH+5       RF=CURRENT LENGTH OF FEED CODE INFO          
         AR    RF,RE               RF=A(NEXT OPEN FEED ENTRY)                   
         SPACE 1                                                                
         USING SCLFEEDD,RE                                                      
         OC    SCLFEED,SPACES                                                   
         CLC   SFFEED,SPACES                                                    
         BE    DF15                                                             
         SPACE 1                                                                
DF10     AHI   R4,1                                                             
         CLC   SFFEED,TANPFEED     IF FEED CODE IS NOT ALREADY                  
         BE    DF20                LISTED IN SCREEN LINE                        
         LA    RE,SFLNQ(RE)                                                     
         CR    RE,RF                                                            
         BL    DF10                                                             
         SPACE 1                                                                
         MVI   SFCOM,C','               MOVE COMMA                              
DF15     EDIT  (R4),SFNUM,ZERO=NOBLANK  FEED CODE NUMBER                        
         MVI   SFEQU,C'='               EQUATE SIGN                             
         MVC   SFFEED,TANPFEED          AND FEED CODE                           
         MHI   R4,SFLNQ                                                         
         STC   R4,SCLFEEDH+5            AND NEW LENGTH TO SCREEN                
         SPACE 1                                                                
DF20     MVC   0(L'LINFEED,R2),SFNUM                                            
         B     XIT                                                              
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
*              ROUTINE DISPLAYS CBL LINES                                       
         SPACE 1                                                                
DISCBL   NTR1                                                                   
         XC    CYCLE1(CYCLELNQ),CYCLE1                                          
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,SCBACTVH  LAST CHANGED INFO                         
         SPACE 1                                                                
         L     R6,AIO              R6 = A(RECORD)                               
         USING TLNXD,R6                                                         
         SPACE 1                                                                
         BAS   RE,SETSTART         SET STARTING POINT                           
         L     R3,FULL             RETURNS A(EL) IN FULL                        
         USING TANPD,R3            R3=A(CBL USE ELEMENT)                        
         SPACE 1                                                                
         MVC   SCBHD1,HEADNTI             SET FIELD HEADERS                     
         OI    SCBHD1H+6,X'80'                                                  
         MVC   SCBHD2,HEADNTI                                                   
         OI    SCBHD2H+6,X'80'                                                  
         CLI   TANPTYP,TANPNET                                                  
         BE    DCBL10                                                           
         MVC   SCBHD1,HEADMKT                                                   
         MVC   SCBHD2,HEADMKT                                                   
         CLI   TANPTYP,TANPSPT                                                  
         BE    DCBL10                                                           
         MVC   SCBHD1,HEADCSYS                                                  
         MVC   SCBHD2,HEADCSYS                                                  
         CLI   TANPTYP,TANPCSYS                                                 
         BE    DCBL10                                                           
         DC    H'00'                                                            
         SPACE 1                                                                
DCBL10   LA    R2,SCBL1H           R2=A(1ST USE LINE)                           
         USING LINED,R2                                                         
         LA    R4,MXLINES          R4=N'LINES                                   
         MVC   CLAFRST,TANPEL      SAVE KEY OF FIRST EL. DISPLAYED              
         MVC   CLAFRST+TANPLNQ(1),TLNXSEQ  AND SEQ OF CORRES. RECORD            
         SPACE 1                                                                
DCBL20   MVC   CLALST,TANPEL       SAVE KEY OF LAST EL. DISPLAYED               
         MVC   CLALST+TANPLNQ(1),TLNXSEQ  AND SEQ OF CORRES. RECORD             
         BAS   RE,DSPCBLLN         DISPLAY ONE USE LINE                         
         SPACE 1                                                                
DCBL30   BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    *+12                                                             
         BAS   RE,GETNXT           GET NEXT RECORD SETS (R3)                    
         BNE   DCBL40                                                           
         LA    R2,LINCNEXT         BUMP TO NEXT LINE                            
         BCT   R4,DCBL20                                                        
         SPACE 1                                                                
DCBL40   BAS   RE,PGIT             CALC THIS PAGE BASED ON LAST ELE             
         MVC   THISPG,BYTE         DISPLAYED                                    
         SPACE 1                                                                
         OC    CYCLE1,CYCLE1                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,CYC1STRT),(8,SCBCYC1)                             
         MVC   SCBLEN1,CYC1LEN                                                  
         OC    CYCLE2,CYCLE2                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,CYC2STRT),(8,SCBCYC2)                             
         MVC   SCBLEN2,CYC2LEN                                                  
         OC    CYCLE3,CYCLE3                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,CYC3STRT),(8,SCBCYC3)                             
         MVC   SCBLEN3,CYC3LEN                                                  
         B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 2                                                                
HEADNTI  DC    CL(L'SCBHD1)'CNet'                                               
HEADMKT  DC    CL(L'SCBHD1)'Mkt'                                                
HEADCSYS DC    CL(L'SCBHD1)'CSys'                                               
NUSES    DC    CL(L'SCBUSE1)'#Uses'                                             
         EJECT                                                                  
*              ROUTINE TO DISPLAY ONE CBL/WSP/LCB USE LINE                      
         SPACE 1                                                                
         USING TANPD,R3                                                         
DSPCBLLN NTR1                                                                   
         GOTO1 DATCON,DMCB,(1,TANPDATE),(5,LINUDTE)                             
         MVC   LINCNTI,TANPNTI     CNET/MKT/CSYS CODE                           
         CLC   SNXUSE,=C'CBL'      IF USE IS CABLE                              
         BNE   DCBLLN05                                                         
         EDIT  TANPCNTR,LINCUSES   NUMBER OF USES                               
         SPACE 1                                                                
DCBLLN05 OC    TANPCYCS,TANPCYCS   FOR SPOT USES                                
         BZ    XIT                 IF CYCLE HAS BEEN DEFINED                    
         CLC   CYCLE1,TANPCYCS                                                  
         BE    XIT                                                              
         CLC   CYCLE2,TANPCYCS                                                  
         BE    XIT                                                              
         CLC   CYCLE3,TANPCYCS                                                  
         BE    XIT                                                              
         LA    RE,CYCLE1                                                        
         OC    CYCLE1,CYCLE1                                                    
         BZ    DCBLLN10                                                         
         LA    RE,CYCLE2                                                        
         OC    CYCLE2,CYCLE2                                                    
         BZ    DCBLLN10                                                         
         LA    RE,CYCLE3                                                        
         OC    CYCLE3,CYCLE3                                                    
         BNZ   XIT                                                              
DCBLLN10 MVC   0(L'CYCLE1,RE),TANPCYCS    SAVE IT IN THE CYCLE TABLE            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO SET STARTING POINT FOR DISPLAY                        
         SPACE 1                                                                
SETSTART NTR1                                                                   
*                                                                               
SST10    MVI   TGBYTE,0            SET WE WANT FIRST RECORD                     
         BAS   RE,SETREC           ENSURES CORRECT REC, R3=>1ST ELE             
         BE    *+6                                                              
         DC    H'0'                VANISHED!!                                   
*                                                                               
         LA    RE,CLAUSE                                                        
         LA    RF,L'CLAUSE-1                                                    
         CLI   LWRSCR,SCR9E                                                     
         BE    SST20                                                            
         LA    RF,L'CLAUSE-1                                                    
         CLI   LWRSCR,SCR9D                                                     
         BE    SST20                                                            
         DC    H'00'                                                            
SST20    EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)       IF WE HAVEN'T DISPLAYED ANYTHING YET         
         BNZ   *+12                                                             
         BAS   RE,CNTPGS           SET NPAGES                                   
         B     SSTX                THEN START WITH FIRST                        
*                                                                               
         XR    R0,R0               INITIALIZE START SWITCH                      
         MVC   TGBYTE,CLALST+TANPLNQ SET SEQ. NUMBER OF CORRES. RECORD          
         CLI   LWRSCR,SCR9E                                                     
         BE    SST25                                                            
         MVC   TGBYTE,CLALST+TANPLNQ SET SEQ. NUMBER OF CORRES. RECORD          
         CLI   LWRSCR,SCR9D                                                     
         BE    SST25                                                            
         DC    H'00'                                                            
*                                                                               
SST25    CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BNE   SST30                                                            
         TM    SNXTCIDH+4,X'20'    AND TALENT CID CHANGED                       
         BO    SST30                                                            
         LA    R0,1                SET START SWITCH TO RE-DISPLAY               
         MVC   TGBYTE,CLAFRST+TANPLNQ SET SEQ. NUMBER OF CORRES. RECORD         
         CLI   LWRSCR,SCR9E                                                     
         BE    SST30                                                            
         MVC   TGBYTE,CLAFRST+TANPLNQ SET SEQ. NUMBER OF CORRES. RECORD         
         CLI   LWRSCR,SCR9D                                                     
         BE    SST30                                                            
         DC    H'00'                                                            
*                                                                               
SST30    BAS   RE,SETREC           ENSURE CORRECT REC, R3=>1ST NP/MT            
         BNE   SST50               IT'S GONE, SO START OVER                     
*                                                                               
         LTR   R0,R0               IF NEED TO DISPLAY CURRENT PAGE              
         BZ    SST40                                                            
         CLI   LWRSCR,SCR9E                                                     
         BNE   SST36                                                            
SST35    CLC   0(TANPLNQ,R3),CLAFRST SCAN FOR FIRST EL DISPLAYED                
         BNL   SSTX                                                             
         BAS   RE,NEXTEL                                                        
         BE    SST35                                                            
         B     SST50                                                            
SST36    CLI   LWRSCR,SCR9D                                                     
         BNE   SST38                                                            
SST37    CLC   0(TANPLNQ,R3),CLAFRST SCAN FOR FIRST EL DISPLAYED                
         BNL   SSTX                                                             
         BAS   RE,NEXTEL                                                        
         BE    SST37                                                            
         B     SST50                                                            
SST38    DC    H'00'                                                            
*                                                                               
SST40    CLI   LWRSCR,SCR9E                                                     
         BNE   SST44                                                            
SST42    CLC   0(TANPLNQ,R3),CLALST  SCAN FOR 1ST EL AFTER LAST DISP            
         BH    SSTX                                                             
         BAS   RE,NEXTEL                                                        
         BE    SST42                                                            
         B     SST50                                                            
SST44    CLI   LWRSCR,SCR9D                                                     
         BNE   SST46                                                            
SST45    CLC   0(TANPLNQ,R3),CLALST  SCAN FOR 1ST EL AFTER LAST DISP            
         BH    SSTX                                                             
         BAS   RE,NEXTEL                                                        
         BE    SST45                                                            
         B     SST50                                                            
SST46    DC    H'00'                                                            
*                                                                               
SST50    DS    0H                  NONE LEFT - START FROM BEGINNING             
         BAS   RE,CLRELINF         CLEAR LOWER LINE INFO                        
         B     SST10               GO POINT TO FIRST EL.                        
*                                                                               
SSTX     ST    R3,FULL             RETURN A(ELEMENT) IN FULL                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ENSURES CORRECT RECORD IS IN AIO                         
*                       NTRY   -   TGBYTE = CORRECT SEQUENCE NO.                
*                       XIT    -   CC EQ, R3=A(1ST TANPD/TAMTD ELE)             
         SPACE 1                                                                
SETREC   NTR1                                                                   
         LA    R6,KEY              R6 = A(KEY)                                  
         USING TLNXD,R6                                                         
         CLC   TLNXSEQ,TGBYTE      IF WE HAVE CORRECT REC. ALREADY              
         BE    SRECX               THEN DONE                                    
         MVC   TLNXSEQ,TGBYTE      ELSE SET TO GET CORRECT REC.                 
         GOTO1 HIGH                                                             
         CLC   TLNXKEY,KEYSAVE     IF GONE, THEN RETURN CC NE                   
         BNE   NO                                                               
         GOTO1 GETREC              ELSE GET THE RECORD                          
*                                                                               
SRECX    L     R3,AIO                                                           
         MVI   ELCODE,TANPELQ      LOOK FOR APPROPRIATE ELEMENTS                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE AT LEAST ONE ELEMENT                 
         XIT1  REGS=(R3)           RETURN CC EQ, R3=A(FIRST TANPD)              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO GET NEXT RECORD                                       
*                                  XIT - SETS ELECNT                            
         SPACE                                                                  
GETNXT   NTR1                                                                   
         BAS   RE,CNTUSE           COUNT CURRENT # OF ELEMENTS (HALF)           
         L     R6,AIO                                                           
         USING TLNXD,R6                                                         
*                                                                               
         MVC   KEY,TLNXKEY         RE-READ KEY OF CURRENT RECORD                
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(TLNXSEQ-TLNXKEY),KEYSAVE                                     
         BNE   NO                                                               
*                                                                               
         LH    R1,ELECNT           ADD PREVIOUS RECORD TO                       
         AH    R1,HALF             ELEMENT COUNT SO FAR                         
         STH   R1,ELECNT                                                        
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,TANPELQ                                                   
         CLI   LWRSCR,SCR9F                                                     
         BNE   *+8                                                              
         MVI   ELCODE,TAMTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE AT LEAST ONE ELEMENT                 
         XIT1  REGS=(R3)                                                        
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO CALCULATE TOTAL NUMBER OF PAGES                       
         SPACE                                                                  
CNTPGS   NTR1                                                                   
         BAS   RE,CNTUSE           COUNT TOTAL NUMBER IN RECORD                 
         LH    R4,HALF                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         LA    R3,KEY                                                           
         USING TLNXD,R3                                                         
         MVC   TLNXKEY,SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   TLNXKEY,SVKEY                                                    
         BE    *+6                                                              
         DC    H'0'                MUST HAVE RECORD                             
CNTPGS10 GOTO1 SEQ                                                              
         CLC   0(TLNXSEQ-TLNXKEY,R3),SVKEY                                      
         BNE   CNTPGS20                                                         
*                                                                               
         GOTO1 GETREC                                                           
         BAS   RE,CNTUSE           COUNT TOTAL NUMBER IN RECORD                 
         AH    R4,HALF             ADD TO TOTAL ELEMENT COUNT                   
         B     CNTPGS10                                                         
*                                                                               
CNTPGS20 BAS   RE,CALCPGS          CALCULATE N'PAGES FROM ELE COUNT             
         MVC   NPAGES,BYTE                                                      
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO CALCULATE TOTAL NUMBER TANPD/TAMTD ELES               
*                                  XIT HALF EQUALS NUMBER OF ELEMENTS           
         SPACE                                                                  
CNTUSE   NTR1                                                                   
         XR    R2,R2               R2=COUNT                                     
         L     R3,AIO                                                           
         MVI   ELCODE,TANPELQ      LOOK FOR CLA ELEMENTS                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CNTUSE5  BAS   RE,NEXTEL                                                        
         BNE   *+12                                                             
         AH    R2,=H'1'            ADD 1 TO COUNT                               
         B     CNTUSE5                                                          
*                                                                               
         STH   R2,HALF                                                          
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CALCULATE NUMBER OF PAGES                             
*                                  NTRY (R4)= NUMBER OF ELEMENTS                
*                                  XIT BYTE = NUMBER OF PAGES                   
         SPACE                                                                  
CALCPGS  NTR1                                                                   
         LA    R1,MXLINES-1                                                     
         AR    R1,R4               ADD N'LINES-1 FOR DIVIDE                     
         XR    R0,R0                                                            
         LA    R2,MXLINES                                                       
         DR    R0,R2               DIVIDE BY N'LINES/SCREEN                     
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                MUST HAVE AT LEAST 1 PAGE                    
         STC   R1,BYTE             RETURN BYTE=PAGE NUMBER                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES CURRENT PAGE NUMBER                           
         SPACE 1                                                                
PGIT     NTR1                                                                   
         XR    R4,R4               R4=COUNT                                     
         L     R3,AIO                                                           
         MVI   ELCODE,TANPELQ      LOOK FOR CLA ELEMENTS                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PG10     BAS   RE,NEXTEL                                                        
         BNE   PG20                                                             
         AH    R4,=H'1'            ADD 1 TO COUNT                               
         CLI   LWRSCR,SCR9F        IF CLA                                       
         BE    PG15                                                             
         CLC   CLALST(TANPLNQ),0(R3)   TEST IF WE'VE REACHED IT                 
         BNE   PG10                                                             
         B     PG20                                                             
         SPACE 1                                                                
PG15     DC    H'00'                                                            
         SPACE 1                                                                
PG20     AH    R4,ELECNT           ADD TO ELEMENT COUNT FROM PREV RECS          
         BAS   RE,CALCPGS          CALCULATE NUMBER OF PAGES                    
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CLEAR FLAGS FOR LOWER SCREEN                          
         SPACE 1                                                                
CLRELINF NTR1                                                                   
         XC    CLAUSE,CLAUSE       CLEAR LAST EL. KEYS                          
         XC    ELECNT,ELECNT       CLEAR ELEMENT COUNT                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS THE REPORT                                        
PREP     NTR1                                                                   
         L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         LA    R0,MYSPECS          SET A(SPECS)                                 
         ST    R0,SPECS                                                         
         LA    R0,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R0,HEADHOOK                                                      
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         BAS   RE,SETOVNUM         SET OVERLAY TO CORRECT LOWER SCREEN          
         BAS   RE,GETSCRN          INSURE CORRECT LOWER SCRN IS LOADED          
         BAS   RE,CLRLWR           CLEAR THE LOWER SCREEN                       
         NI    SNXCHGH+1,X'F7'     RETURN FIELD TO NORMAL INTENSITY             
         OI    SNXCHGH+6,X'80'     AND TRANSMIT                                 
*                                                                               
         BAS   RE,DISHDR           DISPLAY HEADER INFORMATION                   
*                                                                               
PREP10   LA    R4,MXLINES/2        PRINT ONE LINE AT A TIME                     
         CLI   LWRSCR,SCR9E                                                     
         BNE   PREP15                                                           
         BAS   RE,DISCLA           DISPLAY USE LINES                            
         LA    R2,SCLL1H           R2=A(FIRST USE LINE)                         
         LA    R0,2*LINLNQ         R0=LENGTH OF WHOLE SCREEN LINE               
         B     PREP20                                                           
*                                                                               
PREP15   CLI   LWRSCR,SCR9D                                                     
         BNE   PREP16                                                           
         BAS   RE,DISCBL                                                        
         LA    R2,SCBL1H                                                        
         LA    R0,2*LINCLNQ                                                     
         B     PREP20                                                           
*                                                                               
PREP16   DC    H'00'                                                            
*                                                                               
PREP20   DS    0H                                                               
******** LA    R3,0(R0,R2)         R3=A(NEXT LINE)                              
         LR    R3,R2                                                            
         AR    R3,R0               R3=A(NEXT LINE)                              
         MVI   BYTE,C'P'                                                        
         GOTO1 PRTSCRN,DMCB,(R2),(R3),P                                         
         LR    R2,R3               R2=A(BEGINNING OF NEXT LINE)                 
         BCT   R4,PREP20                                                        
*                                                                               
         CLC   THISPG,NPAGES       TEST MORE TO DISPLAY                         
         BE    PREPX                                                            
         BAS   RE,CLRLWR           CLEAR LOWER SCREEN LINES                     
         B     PREP10                                                           
*                                                                               
PREPX    MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO CLEAR LOWER SCREEN LINES                              
CLRLWR   NTR1                                                                   
         CLI   LWRSCR,SCR9E        IF LOWER SCREEN IS CLA                       
         BNE   CL10                                                             
         BAS   RE,CLRUSE           CLEAR CLA LINES                              
CL10     CLI   LWRSCR,SCR9D        IF LOWER SCREEN IS CBL                       
         BNE   CL20                                                             
         BAS   RE,CLRCBL           CLEAR CBL LINES                              
CL20     CLI   LWRSCR,SCR9F                                                     
         BNE   XIT                                                              
         DC    H'00'                                                            
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CLEAR CLA LOWER SCREEN                                
         SPACE 1                                                                
CLRUSE   NTR1                                                                   
         LA    R2,SCLL1H           R2=A(1ST USE LINE)                           
         USING LINED,R2                                                         
         LA    R4,MXLINES          R4=N'LINES                                   
CLRUSE5  XC    LINDATA,LINDATA                                                  
         OI    LINEH+6,X'80'                                                    
         LA    R2,LINNEXT                                                       
         BCT   R4,CLRUSE5                                                       
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO CLEAR CBL LOWER SCREEN                                
         SPACE 1                                                                
CLRCBL   NTR1                                                                   
         LA    R2,SCBL1H           R2=A(1ST USE LINE)                           
         USING LINED,R2                                                         
         LA    R4,MXLINES          R4=N'LINES                                   
CLRCBL5  XC    LINDATA(L'SCBLST),LINDATA                                        
         OI    LINEH+6,X'80'                                                    
         LA    R2,LINCNEXT                                                      
         BCT   R4,CLRCBL5                                                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO 'DELETE/RESTORE' HOLD RECORD                          
         SPACE                                                                  
RDELRES  NTR1                                                                   
         ZIC   R2,MODE             SAVE MODE                                    
         MVI   MODE,VALREC         FUDGE FOR SAVPTRS                            
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         STC   R2,MODE             RESET MODE                                   
*                                                                               
         L     R6,AIO                                                           
         USING TLNXD,R6                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLNXKEY),TLNXKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLNXKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   MODE,RECDEL                                                      
         BNE   *+12                                                             
         OI    KEY+TLDRSTAT-TLDRD,TLNXSDEL                                      
         B     *+8                                                              
         NI    KEY+TLDRSTAT-TLDRD,X'FF'-TLNXSDEL                                
         GOTO1 WRITE                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'       SET STATUS IN RECORD                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   TLNXSTAT(1),KEY+TLDRSTAT-TLDRD                                   
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         ZIC   R2,MODE             SAVE MODE                                    
         MVI   MODE,XRECPUT        FUDGE FOR ADDPTRS                            
         GOTO1 ADDPTRS,DMCB,(X'20',PTRBLK)                                      
         STC   R2,MODE             RESET MODE                                   
         MVC   KEY,SVKEY           RESET KEY                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO SEE IF ANY OTHER HOLD RECORDS NEED TO                 
*              BE 'DELETED' OR 'RESTORED'                                       
         SPACE                                                                  
FIXHLD   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2            SET IOAREA                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLNXSEQ-TLNXKEY),SVKEY                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FIXHLD5  GOTO1 SEQ                                                              
         CLC   KEY(TLNXSEQ-TLNXKEY),SVKEY                                       
         BNE   FIXHLDX                                                          
         CLI   MODE,XRECDEL                                                     
         BNE   *+12                                                             
         OI    KEY+TLDRSTAT-TLDRD,TLNXSDEL                                      
         B     *+8                                                              
         NI    KEY+TLDRSTAT-TLDRD,X'FF'-TLNXSDEL                                
         GOTO1 WRITE                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING TLNXD,R6            SET STATUS IN RECORD                         
         MVC   TLNXSTAT(1),KEY+TLDRSTAT-TLDRD                                   
         GOTO1 PUTREC                                                           
         B     FIXHLD5                                                          
*                                                                               
FIXHLDX  MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            RESET IOAREA                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              HEADLINE HOOK FOR PRINTING                                       
         SPACE                                                                  
HDHOOK   NTR1                                                                   
         L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
*                                                                               
         MVI   BYTE,C'H'           MOVE UPPER PORTION OF SCRN TO HEADS          
         LA    R2,SCLL1H                                                        
         CLI   LWRSCR,SCR9E                                                     
         BE    HH10                                                             
         LA    R2,SWSL1H                                                        
         CLI   LWRSCR,SCR9F                                                     
         BE    HH10                                                             
         LA    R2,SCBL1H                                                        
HH10     GOTO1 PRTSCRN,DMCB,CONTAGH,(R2),H4                                     
         MVI   BYTE,C'P'           RESET FOR LINE PRINTING                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDINV2  MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         STC   R3,ERRDISP          WITH DISPLACEMENT INTO FIELD                 
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
ERRADD   LA    R2,CONACTH          INVALID RECORD/ACTION COMBINATION            
         MVI   ERROR,INVRCACT                                                   
         B     THEEND                                                           
         SPACE 1                                                                
ERRNTFND LA    R2,SNXAGYH          RECORD NOT FOUND                             
         MVI   ERROR,NOTFOUND                                                   
         B     THEEND                                                           
         SPACE 1                                                                
NODELETE LA    R2,CONACTH                                                       
         MVC   8(8,R2),=CL8'DELETE'                                             
         OI    6(R2),X'80'                                                      
         MVI   ERROR,ERNXDEL       CANNOT DELETE - MUST BE UNMATCHED            
         B     THEEND              AND UNUSED                                   
         SPACE 1                                                                
ERDELETE LA    R2,CONACTH                                                       
         MVC   8(8,R2),=CL8'DELETE'                                             
         OI    6(R2),X'80'                                                      
         MVI   ERROR,RECISDEL      RECORD ALREADY DELETED                       
         B     THEEND                                                           
         SPACE 1                                                                
ERREST   LA    R2,CONACTH                                                       
         MVC   8(8,R2),=CL8'RESTORE'                                            
         OI    6(R2),X'80'                                                      
         MVI   ERROR,RECNTDEL      RECORD NOT DELETED                           
         B     THEEND                                                           
         SPACE 1                                                                
ERRACT   LA    R2,CONACTH          NO LONGER AVAIL. FOR MATCH/UNMATCH           
         MVI   ERROR,ERNXACT                                                    
         B     THEEND                                                           
         SPACE 1                                                                
ERRMTCH  LA    R2,CONACTH                                                       
         MVI   ERROR,ERNXMTCH      HOLD HAS ALREADY BEEN MATCHED                
         B     THEEND                                                           
         SPACE 1                                                                
ERRMTCH2 MVI   MYMSGNO1,ERNXMTH2   NID MATCHES VIA &1 - ERASE TO                
         MVI   MYMTYP,GTMERR                            AUTO MATCH              
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,9                                                          
         MVC   BYTE,NEWCTYPE                                                    
         BAS   RE,SETMVIA          RETURNS MATCHED VIA IN WORK                  
         MVC   BLOCK+1(8),WORK                                                  
         MVI   BLOCK+9,0                                                        
         B     THEEND                                                           
         SPACE 1                                                                
ERRMTCH3 L     R6,AIO                                                           
         USING TLNXD,R6                                                         
         MVI   ERROR,ERNXMTH3      RECORD IS A CHANGE - PRESS PF20              
         TM    TLNXCCDE,TANXCVFY                                                
         BZ    *+8                                                              
         MVI   ERROR,ERNXMTH5      RECORD NEEDS VERIFICATION-PRESS PF20         
         OI    CHGERR,CHGEPEND     SET ERROR PENDING                            
         LA    R2,SNXCHGH          R2=A(CHANGE CODE FIELD)                      
         OI    1(R2),X'08'         SET FIELD TO HIGH INTENSITY                  
         OI    6(R2),X'80'         TRANSMIT                                     
         B     THEEND                                                           
         SPACE 1                                                                
ERRMTCH4 MVI   BLOCK,9                                                          
         MVC   BLOCK+1(8),=C'UNMARKED'                                          
         MVI   BLOCK+9,0                                                        
         B     *+18                                                             
ERRMTCH5 MVI   BLOCK,10                                                         
         MVC   BLOCK+1(9),=C'NOT AIRED'                                         
         MVI   BLOCK+10,0                                                       
         MVI   MYMSGNO1,ERNXMTH4   RECORD IS &1-MATCH NOT ALLOWED               
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,CONACTH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
ERRUMTCH LA    R2,CONACTH                                                       
         MVI   ERROR,ERNXUMTH      HOLD HAS ALREADY BEEN UNMATCHED              
         B     THEEND                                                           
         SPACE 1                                                                
PGDSPMS2 CLI   PFAID,14            TEST DISPLAYED NEW PAGE                      
         BE    PLSENTER                                                         
PGDSPMSG MVI   MYMSGNO1,54         PAGE X OF Y DISPLAYED                        
         L     R2,AFRSTKEY                                                      
         CLI   ACTNUM,ACTDIS                                                    
         BE    PGDSP2                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+12                                                             
         CLI   THISLSEL,C'S'                                                    
         BE    PGDSP2                                                           
         MVI   MYMSGNO1,55          ... - ENTER CHANGES AS DESIRED              
         L     R2,AFRSTREC                                                      
PGDSP2   MVI   BLOCK,2                                                          
         EDIT  THISPG,(1,BLOCK+1)                                               
         MVI   BLOCK+2,2                                                        
         EDIT  NPAGES,(1,BLOCK+3)                                               
         MVI   BLOCK+4,0                                                        
         B     INFEND                                                           
         SPACE 1                                                                
PLSENTER MVI   MYMSGNO1,2          PLEASE ENTER REQUIRED FIELDS                 
         MVI   MYMSYS,X'FF'                                                     
         LA    R2,SNXTAGH          CURSOR TO FIRST USE LINE                     
         B     INFEND                                                           
         SPACE 1                                                                
HLDMTCH  MVI   MYMSGNO1,87         HOLD RECORD HAS BEEN MATCHED                 
         B     RECEND                                                           
         SPACE 1                                                                
HLDUMTCH MVI   MYMSGNO1,88         HOLD RECORD HAS BEEN UNMATCHED               
         CLI   ALIASDEL,C'Y'                                                    
         BE    *+8                                                              
         MVI   MYMSGNO1,89         HOLD UNMATCHED (ALIAS NOT DELETED)           
         B     RECEND                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
RECEND   LA    R2,CONRECH          R2=A(RECORD FIELD)                           
INFEND   OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
XFFS     DC    6X'FF'                                                           
NOTONSYS DC    CL(L'SNXMESS)'Traffic ID not on Talent System'                   
MISSCYC  DC    CL(L'SNXMESS)'All Uses not Covered By Existing Cycles'           
         SPACE 1                                                                
MXLINES  EQU   20                        MAX N'USE LINES/SCREEN                 
         SPACE 1                                                                
*              TABLE OF MATCHED VALUES                                          
MTCHTAB  DS    0CL9                                                             
         DC    AL1(TANXTTAL),CL8'TAL CID'                                       
         DC    AL1(TANXTLFT),CL8'LIFT ID'                                       
         DC    AL1(TANXTAKA),CL8'ALIAS'                                         
         DC    AL1(TANXTALF),CL8'ALIAS(L)'                                      
         DC    AL1(TANXTVER),CL8'TAL VER'                                       
         DC    AL1(TANXTALV),CL8'ALIAS(V)'                                      
         DC    X'FF'                                                            
         SPACE 1                                                                
*              TABLE OF CHANGE DESCRIPTIONS                                     
CHGTAB   DS    0CL11                                                            
         DC    AL1(TANXCCID),CL10'COMMERCIAL'                                   
         DC    AL1(TANXCDTE),CL10'DATE'                                         
         DC    AL1(TANXCPRD),CL10'PRODUCT'                                      
         DC    AL1(TANXCLEN),CL10'LENGTH'                                       
         DC    AL1(TANXCAIR),CL10'NOT AIRED'                                    
         DC    AL1(TANXCUNM),CL10'UNMARKED'                                     
**NO-OP* DC    AL1(TANXCMGD),CL10'MAKEGOOD'                                     
         DC    AL1(TANXCVFY),CL10'VERIFY'                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'HOLD    ',CL8'LIST    '                               
PF13X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'HOLD    ',CL8'MATCH   '                               
PF14X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF15X-*,15,0,0,0)                                            
         DC    CL3' ',CL8'HOLD    ',CL8'UNMATCH '                               
PF15X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF20X-*,20,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF20X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF24X-*,24,0,0,PFTSETPN)                                     
         DC    CL3' ',CL8'        ',CL8'REPORT  '                               
PF24X    EQU   *                                                                
         SPACE 1                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              SPECS FOR REPORT                                                 
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,32,C'HOLD RECORD'                                             
         SSPEC H2,32,C'-----------'                                             
         DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINE TO ASSIGN DATES TO THE HOLD RECORD'S USES                
         SPACE 1                                                                
CBLDATES NTR1  BASE=*,LABEL=*                                                   
         CLI   LWRSCR,SCR9D        IF NOT ON CABLE OR WILDSPOT OR               
         BNE   CDX                 SCREEN, EXIT                                 
         SPACE 1                                                                
         USING TANPD,R3                                                         
         L     R3,AIO1             IF NOT A WILDSPOT HOLD RECORD,               
         MVI   ELCODE,TANPELQ      EXIT                                         
         BRAS  RE,GETEL                                                         
         BNE   CDX                                                              
         CLI   TANPTYP,TANPSPT                                                  
         BNE   CDX                                                              
         SPACE 1                                                                
         XC    TANPCYCS(8),TANPCYCS   DELETE PREVIOUSLY VALIDATED               
         BRAS  RE,NEXTEL              CYCLES FROM TANPD ELEMENTS                
         BE    *-10                                                             
         DROP  R3                                                               
         SPACE 1                                                                
         USING TACOD,R3                                                         
         L     R3,AIO2             IF NOT BEING MATCHED TO A                    
         MVI   ELCODE,TACOELQ      RADIO OR ADDENDUM COMMERCIAL, EXIT           
         BRAS  RE,GETEL                                                         
         BNE   CDX                                                              
         MVC   SVTYPE,TACOMED      SAVE TYPE                                    
         CLI   TACOMED,C'R'                                                     
         BE    CD10                                                             
         MVC   SVTYPE,TACOTYPE     OR MEDIA OF COMMERCIAL IN TGBYTE             
         CLI   TACOTYPE,C'A'                                                    
         BNE   CDX                                                              
         DROP  R3                                                               
         SPACE 1                                                                
CD10     LA    R2,SCBCYC1H         R2=A(FIRST CYCLE DEFINTION)                  
         LA    R3,SCBCYC3H         R3=A(LAST CYCLE DEFINTION)                   
         LA    R4,CYCLE1           R4=A(SAVED FIRST CYCLE)                      
         SPACE 1                                                                
         XC    CYCLE1(CYCLELNQ),CYCLE1                                          
         SPACE 1                                                                
CD20     CLI   5(R2),0             IF DATE NOT INPUTTED                         
         BNE   CD30                                                             
         CLI   5(R3),0             LENGTH CANNOT BE INPUTTED                    
         BNE   ERRDMIS                                                          
         B     CD80                                                             
         SPACE 1                                                                
CD30     GOTO1 DATVAL,DMCB,8(R2),DUB         VALIDATE DATE                      
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRDINV                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(1,(R4))  AND SAVE IN TABLE                  
         GOTO1 DATCON,DMCB,(0,DUB),(8,8(R2)) AND ON SCREEN                      
         SPACE 1                                                                
         LA    RF,SCBLEN1H-SCBCYC1H(R2)      IF DATE ENTERED                    
         CLI   5(RF),0                                                          
         BE    ERRLMIS                       CYCLE LENGTH MUST BE TOO           
         SPACE 1                                                                
         MVC   LENKEYWS+5(4),=C'3M) '        DEFAULT 13 WEEK SETTING            
         MVI   LENKEYWS+9,X'D'               TO 3 MONTH                         
         TM    AGYSTAT,TAAYS13W              UNLESS AGENCY IS SET               
         BZ    CD35                          FOR 13 WEEK CYCLES                 
         MVC   LENKEYWS+5(4),=C'13W)'                                           
         MVI   LENKEYWS+9,X'E'                                                  
         SPACE 1                                                                
CD35     LA    RE,LENKEYWS                                                      
         OC    8(L'SCBLEN1,RF),CDSPACES      CHECK THAT CYCLE LENGTH            
CD40     CLI   0(RE),X'FF'                   FIELD CONTAINS A VALID             
         BE    ERRLINV                       KEYWORD                            
         CLC   8(L'SCBLEN1,RF),0(RE)                                            
         BE    CD50                                                             
         LA    RE,L'LENKEYWS(RE)                                                
         B     CD40                                                             
         SPACE 1                                                                
CD50     CLI   SVTYPE,C'A'                   IF ADDENDUM COMMERCIAL             
         BNE   CD60                                                             
         TM    10(RE),LENADD                 ENSURE LENGTH IS VALID             
         BZ    ERRLINV                                                          
         SPACE 1                                                                
CD60     CLI   SVTYPE,C'R'                   IF RADIO COMMERCIAL                
         BNE   CD70                                                             
         TM    10(RE),LENRAD                 ENSURE LENGTH IS VALID             
         BZ    ERRLINV                                                          
         SPACE 1                                                                
CD70     MVC   6(L'CYC1LEN,R4),0(RE)         SAVE LENGTH IN TABLE               
         SPACE 1                                                                
         MVC   WORK,CDSPACES                 BUILD CYCLE START AND              
         MVC   WORK(8),8(R2)                 LENGTH IN WORK                     
         MVC   WORK+8(2),=C'-('                                                 
         MVC   WORK+10(4),5(RE)                                                 
         ZIC   RF,9(RE)                                                         
         SPACE 1                                                                
         USING PERVALD,RF                                                       
         GOTO1 PDVAL,DMCB,(X'40',BLOCK),((RF),WORK) CALC CYCLE END DATE         
         LA    RF,BLOCK                                                         
         MVC   3(L'CYC1END,R4),PVALPEND             AND SAVE IN TABLE           
         DROP  RF                                                               
         SPACE 1                                                                
CD80     LA    R2,SCBCYC2H-SCBCYC1H(R2)       BUMP TO NEXT CYCLE                
         LA    R4,L'CYCLE1(R4)                ON SCREEN AND TABLE               
         CR    R2,R3                                                            
         BNH   CD20                                                             
         SPACE 1                                                                
         OC    CYCLE1(CYCLELNQ),CYCLE1    IF NOTHING ENTERED,                   
         BZ    ERRCYCNC                   ERROR                                 
         SPACE 1                                                                
         CLI   SCBCYC1H+5,0               IF 1ST AND 2ND CYCLE                  
         BE    CD120                      ARE INPUTTED                          
         CLI   SCBCYC2H+5,0                                                     
         BE    CD100                                                            
         SPACE 1                                                                
         LA    R2,SCBCYC2H                                                      
         SPACE 1                                                                
         CLC   CYCLE2(L'CYC1STRT),CYCLE1  1ST AND 2ND CYCLE CANNOT              
         BE    ERRCYCOV                   BEGIN ON THE SAME DATE                
         SPACE 1                                                                
         CLC   CYCLE2(L'CYC1STRT),CYCLE1  IF 2ND CYCLE STARTS LATER             
         BNH   CD90                       THAN 1ST CYCLE                        
         CLC   CYCLE2(L'CYC1END),CYCLE1+3 2ND CYCLE MUST START LATER            
         BNH   ERRCYCOV                   THAN 1ST CYCLE ENDS                   
         B     CD100                                                            
         SPACE 1                                                                
CD90     CLC   CYCLE2+3(L'CYC1END),CYCLE1 ELSE 2ND CYCLE MUST END               
         BNL   ERRCYCOV                   LATER THAN THE 1ST BEGINS             
         SPACE 1                                                                
CD100    CLI   SCBCYC3H+5,0               IF 1ST AND 3RD CYCLE ARE              
         BE    CD140                      INPUTTED                              
         SPACE 1                                                                
         LA    R2,SCBCYC3H                                                      
         SPACE 1                                                                
         CLC   CYCLE3(L'CYC1STRT),CYCLE1  1ST AND 3RD CYCLE CANNOT              
         BE    ERRCYCOV                   BEGIN ON THE SAME DATE                
         SPACE 1                                                                
         CLC   CYCLE3(L'CYC1STRT),CYCLE1  IF 3RD CYCLE STARTS LATER             
         BNH   CD110                      THAN 1ST CYCLE                        
         CLC   CYCLE3(L'CYC1END),CYCLE1+3 3RD CYCLE MUST START LATER            
         BNH   ERRCYCOV                   THAN 1ST CYCLE ENDS                   
         B     CD120                                                            
         SPACE 1                                                                
CD110    CLC   CYCLE3+3(L'CYC1END),CYCLE1 ELSE 3RD CYCLE MUST END               
         BNL   ERRCYCOV                   LATER THAN THE 1ST BEGINS             
         SPACE 1                                                                
CD120    CLI   SCBCYC2H+5,0               IF 2ND AND 3RD CYCLES                 
         BE    CD140                      ARE INPUTTED                          
         SPACE 1                                                                
         CLC   CYCLE3(L'CYC1STRT),CYCLE2  2ND AND 3RD CYCLE CANNOT              
         BE    ERRCYCOV                   BEGIN ON THE SAME DATE                
         SPACE 1                                                                
         CLC   CYCLE3(L'CYC1STRT),CYCLE2  IF 3RD CYCLE STARTS LATER             
         BNH   CD130                      THAN 2ND CYCLE                        
         CLC   CYCLE3(L'CYC1END),CYCLE2+3 3RD CYCLE MUST START LATER            
         BNH   ERRCYCOV                   THAN 2ND CYCLE ENDS                   
         B     CD140                                                            
         SPACE 1                                                                
CD130    CLC   CYCLE3+3(L'CYC1END),CYCLE2 ELSE 3RD CYCLE MUST END               
         BNL   ERRCYCOV                   LATER THAN THE 2ND BEGINS             
         SPACE 1                                                                
         USING TANPD,R3                                                         
CD140    L     R3,AIO1                    READ THROUGH ALL THE TANPD            
         MVI   ELCODE,TANPELQ             ELEMENTS                              
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
CD170    BRAS  RE,NEXTEL                                                        
         BNE   CD210                                                            
         LA    RE,CYC1STRT                IF USE DATE FITS WITHIN               
         CLC   TANPDATE,CYC1STRT          ONE OF THE CYCLES                     
         BL    CD180                                                            
         CLC   TANPDATE,CYC1END                                                 
         BNH   CD200                                                            
CD180    LA    RE,CYC2STRT                                                      
         CLC   TANPDATE,CYC2STRT                                                
         BL    CD190                                                            
         CLC   TANPDATE,CYC2END                                                 
         BNH   CD200                                                            
CD190    LA    RE,CYC3STRT                                                      
         CLC   TANPDATE,CYC3STRT                                                
         BL    CD170                                                            
         CLC   TANPDATE,CYC3END                                                 
         BH    CD170                                                            
CD200    MVC   TANPCYCS(L'CYCLE1),0(RE)   ASSIGN IT TO THAT CYCLE               
         B     CD170                                                            
         SPACE 1                                                                
CD210    L     R3,AIO1                    IN ORDER TO MATCH,                    
         MVI   ELCODE,TANPELQ             ALL USES MUST BE ASSIGNED             
         BRAS  RE,GETEL                   TO A CYCLE                            
         B     *+8                                                              
CD220    BRAS  RE,NEXTEL                                                        
         BNE   CDX                                                              
         OC    TANPCYCS,TANPCYCS                                                
         BNZ   CD220                                                            
         B     ERRCYCNC                                                         
         DROP  R3                                                               
CDX      XIT1                                                                   
         SPACE 2                                                                
ERRDMIS  MVI   ERROR,MISSING                                                    
         B     CDERREND                                                         
ERRDINV  MVI   ERROR,INVALID                                                    
         B     CDERREND                                                         
ERRLMIS  MVI   ERROR,MISSING                                                    
         B     LEND                                                             
ERRLINV  MVI   ERROR,INVALID                                                    
         B     LEND                                                             
LEND     LA    R2,SCBLEN1H-SCBCYC1H(R2)                                         
         B     CDERREND                                                         
ERRCYCOV MVC   MYMSGNO,=Y(ERCYCOVL)                                             
         B     CDEXTEND                                                         
ERRCYCNC LA    R2,SCBCYC1H                                                      
         MVC   MYMSGNO,=Y(ERCYCNCV)                                             
         B     CDEXTEND                                                         
CDEXTEND OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     CDERREND                                                         
CDERREND GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
CDSPACES DC    20C' '                                                           
         SPACE 2                                                                
LENKEYWS DC    0CL11                                                            
         DC    C'13WK ',C'0000',X'00',AL1(LENRAD+LENADD)                        
         DC    C'8WK  ',C'8W) ',X'0D',AL1(LENRAD)                               
         DC    C'31DAY',C'31D)',X'0E',AL1(LENADD)                               
         DC    C'4WK  ',C'4W) ',X'0D',AL1(LENADD)                               
         DC    C'1WK  ',C'1W) ',X'0D',AL1(LENADD)                               
         DC    C'3DAY ',C'3D) ',X'0D',AL1(LENADD)                               
         DC    X'FF'                                                            
         SPACE 2                                                                
LENRAD   EQU   X'80'                                                            
LENADD   EQU   X'40'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              LOCAL SAVED STORAGE                                              
HOLDD    DSECT                                                                  
*                                                                               
ATRPACK  DS    A                                                                
*                                                                               
CLAUSE   DS    0CL(2*TANPLNQ+2)                                                 
CLAFRST  DS    CL(TANPLNQ+1)       KEY OF FIRST TANPD DSPLYD ON SCRN            
CLALST   DS    CL(TANPLNQ+1)       KEY OF LAST TANPD DSPLYD ON SCRN             
*                                                                               
ELECNT   DS    H                   RUNNING COUNT OF ELEMENTS                    
THISPG   DS    XL1                 CURRENT PAGE NUMBER                          
NPAGES   DS    XL1                 TOTAL N'PAGES                                
ALIASDEL DS    CL1                 N= ALIAS NOT DELETED                         
FRSTDISP DS    CL1                 Y=FIRST DISPLAY FOR MATCH                    
DISPDOK  DS    CL1                 Y=OKAY TO DISPLAY 'DELETED' HOLD             
LWRSCR   DS    CL1                 LOWER SCREEN NUMBER                          
MYSCR    DS    CL1                                                              
*                                                                               
CHGERR   DS    XL1                                                              
CHGEPEND EQU   X'80'               ERROR MESSAGE PENDING                        
CHGEOVR  EQU   X'40'               ERROR MESSAGE OVERRIDDEN                     
*                                                                               
NEWCOM   DS    XL4                 NEW TALENT INTERNAL COMMERCIAL #             
NEWVER   DS    CL1                 NEW COMMERCIAL VERSION LETTER                
NEWCTYPE DS    CL1                 NEW COMMERCIAL MATCH TYPE                    
OLDCTYPE DS    CL1                 OLD COMMERCIAL MATCH TYPE                    
SVDATE   DS    XL3                 WORKING DATE FOR COMP/UNCOMP                 
*                                                                               
TEMPUSER DS    XL10                                                             
*                                                                               
CYCLE1   DS    0XL11                                                            
CYC1STRT DS    XL3                                                              
CYC1END  DS    XL3                                                              
CYC1LEN  DS    XL5                                                              
CYCLE2   DS    0XL11                                                            
CYC2STRT DS    XL3                                                              
CYC2END  DS    XL3                                                              
CYC2LEN  DS    XL5                                                              
CYCLE3   DS    0XL11                                                            
CYC3STRT DS    XL3                                                              
CYC3END  DS    XL3                                                              
CYC3LEN  DS    XL5                                                              
CYCLELNQ EQU   *-CYCLE1                                                         
*                                                                               
AGYSTAT  DS    X                                                                
*                                                                               
SVTYPE   DS    X                                                                
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
PTRBLK   DS    CL(3*L'TLDRREC+1)   PASSIVE PTR + ACTIVE                         
         DS    0X                                                               
         SPACE 1                                                                
*              DSECT TO COVER LOWER SCREEN USE LINES                            
LINED    DSECT                                                                  
         DS    CL8                                                              
         DS    CL1                                                              
LINEH    DS    CL8                                                              
LINDATA  DS    CL(L'SCLLST)                                                     
         ORG   LINDATA                                                          
LINUDTE  DS    CL8                 USE DATE                                     
         DS    CL1                                                              
LINPGM   DS    CL13                PROGRAM NAME                                 
         DS    CL1                                                              
         DS    CL1                                                              
LINNTWRK DS    CL1                 NETWORK CODE                                 
LINSEQ   DS    CL1                                                              
         DS    CL1                 REAL CHANGE IF GENERIC 'NOT AIRED'           
LINCHG   DS    CL3                                                              
         DS    CL1                                                              
LINFEED  DS    CL1                 FEED                                         
LINLNQ   EQU   *-LINED                                                          
LINNEXT  EQU   *                                                                
         SPACE 1                                                                
         ORG   LINPGM                                                           
LINCNTI  DS    CL4                 NTI                                          
         ORG   LINCNTI                                                          
LINCMKT  DS    CL4                                                              
         DS    CL4                                                              
LINCUSES DS    CL3                 # USES                                       
LINCLNQ  EQU   *-LINED                                                          
LINCNEXT EQU   *                                                                
         ORG   LINDATA                                                          
LINWNEXT EQU   *                                                                
                                                                                
         SPACE 2                                                                
*              DSECT TO COVER CHGTAB                                            
CHGTABD  DSECT                                                                  
CHGSTAT  DS    XL1                 CHANGE STATUS                                
CHGDESC  DS    CL10                CHANGE DESCRIPTION                           
         EJECT                                                                  
*              DSECT TO COVER FEED CODE ENTRY                                   
SCLFEEDD DSECT                                                                  
SFCOM    DS    CL1                 ,                                            
SFNUM    DS    CL1                 FEED CODE NUMBER                             
SFEQU    DS    CL1                 =                                            
SFFEED   DS    CL4                 FEED CODE                                    
SFLNQ    EQU   *-SCLFEEDD                                                       
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRD4D                                                       
         EJECT                                                                  
         ORG   SNXTAGH                                                          
       ++INCLUDE TASCR9ED        CLASS A                                        
         EJECT                                                                  
         ORG   SNXTAGH                                                          
       ++INCLUDE TASCR9FD        WILDSPOT                                       
         EJECT                                                                  
         ORG   SNXTAGH                                                          
       ++INCLUDE TASCR9DD        CABLE                                          
         EJECT                                                                  
* TASYSIOD   (MUST FOLLOW LAST SCREEN)                                          
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
         ORG   CONTAGH+2300                                                     
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114TAGEND4   07/20/12'                                      
         END                                                                    
