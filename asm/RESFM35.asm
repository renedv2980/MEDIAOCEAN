*          DATA SET RESFM35    AT LEVEL 027 AS OF 03/05/08                      
*PHASE T81835A,*                                                                
         TITLE 'T81835 - RESFM35 - AGENCY/SP SWITCH ONLINE '                    
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM35 (T81835) --- AGENCY/SP SWITCH ONLINE             *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* SEP12/02 (BU ) --- ORIGINAL ENTRY                               *             
*                                                                 *             
* DEC02/02 (BU ) --- ADD WRAPPER REGENVER                         *             
*                                                                 *             
* OCT20/04 (HQ ) --- SALESPERSON SWITCH MAKEGOOD BUG              *             
*                                                                 *             
* MAR05/08 (SKU) --- MAKE ADV FIELD OPTIONAL FOR PV, BL           *             
*                                                                 *             
*                                                                 *             
*                    ***  END TOMBSTONE  ***                      *             
*******************************************************************             
*                                                                               
T81835   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1835**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HHOOK                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
*                                                                               
*   THESE AREN'T NEEDED IN 'CODESWIT', AND ARE VESTIGIAL FROM                   
*        'TAKEOVER'.  THEY ARE LEFT IN CASE THIS MODULE MORPHS                  
*        INTO SOMETHING MORE SOPHISTICATED.                                     
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VSWITCH,CSWITCH     SET A(SWITC)                                 
         MVC   VBINSRCH,CBINSRCH   SET A(BINSRCH)                               
         DROP  RE                                                               
*                                  ESTABLISH A(REPFACS)                         
         GOTO1 CALLOV,DMCB,0,X'D9000AAC',0                                      
         MVC   VREPFACS,0(R1)                                                   
*                                  EXTERNAL ROUTINE, MISC. SUBROUTINES          
*                                                                               
         LA    RF,SPSLAST          SET A(EQUIVALENCY TABLE)                     
         A     RF,=F'8000'         DISPLACE 8000 DOWN                           
*                                                                               
*   NOT USED, BUT LEFT IN                                                       
*                                                                               
         ST    RF,AEQUITBL         SAVE A(EQUIVALENCY TABLE)                    
         ST    RF,AEQUNTRY         SAVE A(EQUIVALENCY TABLE IN USE)             
         A     RF,=F'3000'         DISPLACE ANOTHER 3000                        
*                                                                               
         ST    RF,AMISFLGS         SAVE A(MISCELLANEOUS FLAGS SPACE)            
*                                                                               
*  'MISFLGS' ARE SET UP 11K AFTER SPSLAST                                       
*                                                                               
         A     RF,=F'120'          DISPLACE ANOTHER 120                         
         ST    RF,ACONTROL         SAVE A(CONTROL RESTORATION SPACE)            
         A     RF,=F'400'          DISPLACE ANOTHER 400                         
         ST    RF,ACMBOTBL         SAVE A(COMBO PARTIC TABLE)                   
         ST    RF,ACMBNTRY         SAVE A(COMBO PARTIC TABLE IN USE)            
*                                                                               
*   LEAVE 400 CHARS FOR CMBOTBL AREA:  57 CHARS * 6 ENTRIES = 342               
*                                                                               
         A     RF,=F'600'          DISPLACE ANOTHER 600                         
         ST    RF,ACMBOSCR         SAVE A(COMBOS ON SCREEN TABLE)               
         ST    RF,ACMBSCIT         SAVE A(COMBOS ON SCREEN IN USE)              
*                                                                               
*   LEAVE 32 CHARS FOR CMBOSCR AREA:  4 PER CON# * 6 ENTRIES                    
*                                                                               
         MVI   ACTELOPT,C'N'       SET 'NO ACTIVITY ELEMENT'                    
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
*                                                                               
         L     R3,AMISFLGS         MISCELLANEOUS FLAGS STORAGE AREA             
*                                                                               
         USING MISFLGS,R3                                                       
*                                                                               
         TM    MISFLAGS,X'80'      RETURN FROM GLOBBER CALL?                    
*                                                                               
***>>>   GOTO1 =A(SUBROUT),DMCB,(RC),('QCKGLOB',0),0,RR=YES                     
         BNO   MAIN0040                                                         
*                                                                               
         NI    MISFLAGS,X'FF'-X'80'                                             
*                                  TURN OFF RETURN FLAG                         
         DROP  R3                                                               
*                                                                               
         B     LIST1040            CONTINUE TO CHECK DISPLAY ARRAY              
*                                                                               
MAIN0040 EQU   *                                                                
         MVC   SRCEREP,TWAAGY      INITIALIZE REP CODE FOR RUN                  
*                                                                               
***   SHOULD THIS TRANSFER TO CHECK-TABLE ROUTINE??                             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,LISTRECS       LIST POSSIBLE CODESWIT ORDERS                
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    REPORT                                                           
                                                                                
         B     EXIT                                                             
                                                                                
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
* VALIDATE KEY                                                                  
*******************************************************************             
VKEY     DS    0H                                                               
*                                                                               
         XC    IOCOUNT,IOCOUNT     CLEAR IO COUNT                               
         MVI   MAXIOFLG,0          CLEAR MAX IO FLAG                            
         GOTO1 =A(VKEYNMOD),DMCB,(RC),RR=Y                                      
         BNZ   VKEY0800            ERROR RETURN:  EXIT                          
*                                                                               
         CLI   SKIPCLER,1          SKIP CLEAR SCREEN?                           
         BE    VKEY0010            YES                                          
         GOTO1 FOUTBLK,DMCB,SPSSELH,SPSLAST,0,0                                 
VKEY0010 EQU   *                                                                
         GOTO1 CHKCMPSP                                                         
*                                                                               
         XC    AGYFILT,AGYFILT     CLEAR ANY AGENCY FILTER                      
         LA    R2,SPSAGIH          AGENCY MANDATORY                             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 CHEKAGI,DMCB,(0,(R2)),AGYFILT,OAGY2DAR                           
         BNZ   INVAGY              ERROR - AGENCY CODE NOT FOUND                
                                                                                
VKEY0020 DS    0H                                                               
*                                                                               
         XC    ADVFILT,ADVFILT     CLEAR ANY ADVERT FILTER                      
         LA    R2,SPSADVH                                                       
         CLI   5(R2),0                                                          
         BNE   VKEY0025                                                         
         CLC   =C'PV',SRCEREP                                                   
         BE    VKEY0028                                                         
         CLC   =C'BL',SRCEREP                                                   
         BE    VKEY0028                                                         
         B     MISSFLD                                                          
VKEY0025 DS    0H                                                               
         GOTO1 =A(CHEKADV),DMCB,(RC),RR=Y                                       
         BNZ   INVADV              ERROR - ADVERT CODE NOT FOUND                
                                                                                
VKEY0028 DS    0H                                                               
         MVI   KEYTYPE,X'BF'       SET BF KEY: NON-STATION FILTERING            
*                                                                               
         XC    SRCESTAT,SRCESTAT   CLEAR STATION SAVE AREA                      
         LA    R2,SPSSTAH                                                       
         CLI   5(R2),0                                                          
         BE    VKEY0030                                                         
         GOTO1 VALISTA             VALIDATE THE STATION                         
         MVC   SRCESTAT,WORK       RETURNED FROM VALISTA                        
         MVC   SRCEGRP,WORK+41                                                  
         MVI   KEYTYPE,X'8E'       SET RIS KEY WITH    STATION                  
*                                                                               
VKEY0030 EQU   *                                                                
         XC    OFFFILT,OFFFILT     CLEAR ANY OFFICE FILTER                      
         LA    R2,SPSOFFH          OFFICE OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VKEY0040                                                         
         GOTO1 =A(CHEKOFF),DMCB,(RC),RR=Y                                       
         BNZ   INVOFF              OFF NOT FOUND ON SOURCE REP FILE             
VKEY0040 DS    0H                                                               
*                                                                               
                                                                                
* IF STATION SIGN-ON, CHECK IF IT'S A VALID SIGN ON ID                          
                                                                                
         CLI   TWAACCS,C'$'                                                     
         BNE   VKEY0060                                                         
*                                                                               
         B     SLOCKOUT            STATION SHOULDN'T BE DOING THIS!!            
*                                                                               
                                                                                
VKEY0060 DS    0H                                                               
         LA    R2,SPSSALH          S/P FILTER OPTIONAL                          
         CLI   5(R2),0                                                          
         BE    VKEY0080                                                         
         GOTO1 CHEKSAL,DMCB,(0,(R2)),SPFILT                                     
*                                  CHECK S/P FILTER                             
         BZ    VKEY0080            SUCCESSFUL RETURN                            
         CLI   BYTE,1              S/P LEAVE DATE FOUND?                        
         BE    INVSPLEV            YES - DISPLAY MESSAGE                        
         B     INVSALPR            NO  - SALESPERSON NOT ON FILE                
                                                                                
VKEY0080 DS    0H                                                               
         LA    R2,SPSDARH          CHECK DARE FLAG VALUE                        
         MVI   DAREFLAG,C'B'       SET DARE FLAG DEFAULT                        
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKEY0100            NO                                           
         MVC   DAREFLAG,SPSDAR     YES - SET DARE FLAG TO INPUT                 
         CLI   SPSDAR,C'B'         DARE+NON-DARE?                               
         BE    VKEY0100            YES                                          
         CLI   SPSDAR,C'Y'         DARE ONLY?                                   
         BE    VKEY0100            YES                                          
         CLI   SPSDAR,C'N'         NON-DARE ONLY?                               
         BE    VKEY0100            YES                                          
         B     INVFLD              NO  - NOT RECOGNIZED                         
                                                                                
VKEY0100 DS    0H                                                               
         XC    AGYNEW,AGYNEW       CLEAR ANY AGENCY REPLACEMENT                 
         LA    R2,SPSNAGH          AGENCY OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VKEY0120                                                         
         GOTO1 CHEKAGI,DMCB,(1,(R2)),AGYNEW,NAGY2DAR                            
         BZ    VKEY0110                                                         
         CLI   BYTE,X'FF'          ERROR: NEED OFFICE?                          
         BE    NEEDOFFC            YES                                          
         B     INVAGY              AGY NOT FOUND ON SOURCE REP FILE             
                                                                                
VKEY0110 DS    0H                                                               
*                                                                               
*   AGENCY SWITCH IS BEING DONE:  DARE AGENCY/OFFICE EQUIVALENTS MUST           
*        BE IN AGREEMENT.                                                       
*                                                                               
         CLC   OAGY2DAR(20),NAGY2DAR FULL AGREEMENT?                            
         BNE   NGDAREQV            NO                                           
VKEY0120 DS    0H                                                               
         LA    R2,SPSNSPH          NEW S/P OPTIONAL                             
         CLI   5(R2),0                                                          
         BE    VKEY0140                                                         
         GOTO1 CHEKSAL,DMCB,(1,(R2)),SPNEW                                      
*                                  CHECK NEW S/P                                
         BZ    VKEY0140            SUCCESSFUL RETURN                            
         CLI   BYTE,1              S/P LEAVE DATE FOUND?                        
         BE    INVSPLEV            YES - DISPLAY MESSAGE                        
         B     INVSALPR            NO  - SALESPERSON NOT ON FILE                
                                                                                
VKEY0140 DS    0H                                                               
*                                                                               
         XC    EFDTCOMP,EFDTCOMP   CLEAR ANY DATE FILTERS                       
         XC    BEFFDATE,BEFFDATE                                                
         LA    R2,SPSDATEH         EFFECTIVE DATE REQUIRED                      
         CLI   5(R2),0             MUST BE ENTERED                              
         BE    INVFLD                                                           
         GOTO1 VALIPERI                                                         
*                                                                               
VKEY0160 DS    0H                                                               
         LA    R2,SPSCSPH          CHECK COMP S/P FLAG VALUE                    
         MVI   COMPSP,C' '         SET FLAG DEFAULT                             
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKEY0180            NO                                           
         L     RF,AMISFLGS         COMP S/P REP?                                
         USING MISFLGS,RF                                                       
         TM    MISFLAGS,X'10'      REP USING COMP S/P?                          
         BNO   INVCMPSP            NO                                           
         DROP  RF                                                               
         MVC   COMPSP,SPSCSP       YES - SET FLAG TO INPUT                      
         CLI   SPSCSP,C'A'         CHANGE TO NEW S/P?                           
         BE    VKEY0180            YES                                          
         CLI   SPSCSP,C'B'         CHANGE TO HOUSE ACCOUNT?                     
         BE    VKEY0180            YES                                          
         B     INVFLD              NO  - NOT RECOGNIZED                         
VKEY0180 EQU   *                                                                
         MVC   SPSLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     VKEY0800                                                         
VKEY0800 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE AS AT DATE                                                           
* INPUT  - R2 POINTS TO AS AT FIELD  HEADER                                     
*                                                                               
*                                                                               
***********************************************************************         
VALIASOF NTR1                                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                               
* INPUT  - R2 POINTS TO PERIOD FIELD HEADER                                     
* OUTPUT - EFFDATE HAS EFFECTIVE DATE                                           
*                                                                               
***********************************************************************         
VALIPERI NTR1                                                                   
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),BLOCK,C',=,-'                                  
         CLI   DMCB+4,0                                                         
         BE    INVLDAT2            ERROR ENCOUNTERED                            
*                                                                               
* VALIDATE START DATE                                                           
*                                                                               
         LA    R5,BLOCK                                                         
         GOTO1 DATVAL,DMCB,(0,12(R5)),EFFDATE                                   
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLDAT2                                                         
         GOTO1 DATCON,DMCB,(0,EFFDATE),(2,EFDTCOMP)                             
*                                  CONVERT EFFECTIVE DATE TO COMP               
         GOTO1 DATCON,DMCB,(0,EFFDATE),(3,BEFFDATE)                             
*                                  CONVERT EFFECTIVE DATE TO BINARY             
*                                                                               
VALPERX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
REPORT   EQU   *                                                                
         GOTO1 =A(DOREPORT),DMCB,(RC),RR=Y                                      
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* LIST RECORDS                                                    *             
*      'LIST' MODE IS DONE AFTER 'SETFILE' MODE IS RECEIVED, SO   *             
*             LIST IS NOW POINTING AT THE SOURCE REPFILE.         *             
*******************************************************************             
LIST     DS    0H                                                               
*                                                                               
         MVI   TAKEALL,C'N'        SET 'TAKEALL = NO'                           
         LA    RF,SPSSELH          SET A(FIRST SELECT FIELD)                    
         LA    RE,SPSLAST          SEND A(END OF SCREEN)                        
         LR    R2,RF                                                            
*                                                                               
*  TEST                                                                         
         LA    R3,1                                                             
*  TEST                                                                         
*                                                                               
         CLI   5(RF),2             INPUT CAN'T BE MORE THAN 2                   
         BH    INVLFLD                                                          
         CLC   8(2,RF),=C'S+'      ALL LINES SELECTED?                          
         BNE   LIST0080            NO                                           
         MVI   TAKEALL,C'Y'        SET 'TAKEALL = YES'                          
         B     LIST0040                                                         
LIST0020 EQU   *                                                                
         CR    RE,RF               END OF SCREEN REACHED?                       
         BNH   LIST0060            YES - FINISHED                               
         CLI   5(RF),0             IF FIRST POS. = 'S+'                         
         BE    LIST0040            OTHERS MUST BE BLANK OR '*S'                 
         LR    R2,RF                                                            
*                                                                               
*  TEST                                                                         
         LA    R3,2                                                             
*  TEST                                                                         
*                                                                               
         CLI   5(RF),2                                                          
         BNE   INVLFLD                                                          
*                                                                               
*  TEST                                                                         
         LA    R3,3                                                             
*  TEST                                                                         
*                                                                               
         CLC   8(2,RF),=C'*S'      ELSE ERROR                                   
         BNE   INVLFLD                                                          
LIST0040 EQU   *                                                                
         LA    RF,SPSSEL1H-SPSSELH(RF)                                          
         B     LIST0020            GO BACK FOR NEXT                             
LIST0060 EQU   *                                                                
         CLI   TAKEALL,C'Y'        IF SELECT ALL, PROCESS SELECT LINES          
         BE    LIST0160                                                         
         B     LIST0180                                                         
*                                                                               
*   LOOK FOR PROCESS REQUEST (S ON REQUEST LINE)                                
*                                                                               
LIST0080 EQU   *                                                                
         CR    RE,RF               END OF SCREEN REACHED?                       
         BNH   LIST0180            YES - FINISHED                               
         LR    R2,RF                                                            
         CLI   5(RF),2                                                          
         BNE   LIST0100                                                         
         CLC   8(2,RF),=C'*S'                                                   
         BE    LIST0140                                                         
LIST0100 CLI   5(RF),1                                                          
         BNE   LIST0120                                                         
         CLI   8(RF),C'S'          LINE SELECTED?                               
         BE    LIST0160            YES - PROCESS SELECTED LINE(S)               
LIST0120 EQU   *                                                                
*                                                                               
*  TEST                                                                         
         LA    R3,4                                                             
*  TEST                                                                         
*                                                                               
         CLI   5(RF),0             IF NOT 'S' OR '*S' OR BLANK - ERROR          
         BNE   INVLFLD                                                          
LIST0140 EQU   *                                                                
         LA    RF,SPSSEL1H-SPSSELH(RF)                                          
*                                  NO  - BUMP TO NEXT SELECT FIELD              
         B     LIST0080            GO BACK FOR NEXT                             
LIST0160 EQU   *                                                                
         BAS   RE,ADDCONS          PROCESS SCREEN DATA                          
         BZ    LIST0980            NO ERRORS - UPDATED: REDISPLAY               
         L     R2,DUB              LOAD A(ERROR FIELD)                          
         B     ERREND              EXIT WITH ERROR                              
LIST0180 EQU   *                                                                
         L     RF,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
         XCEFL 0(RF),1000          CLEAR TABLE                                  
         L     RF,ACMBOTBL         CLEAR COMBO CONTROL TABLE                    
         XC    0(200,RF),0(RF)     CLEAR 1ST PART OF COMBO TABLE AREA           
         XC    200(100,RF),200(RF) CLEAR REMAINDER OF COMBO TABLE AREA          
*                                                                               
         L     RF,ACMBOSCR         CLEAR COMBOS ON SCREEN TABLE                 
         XC    0(32,RF),0(RF)                                                   
         B     LIST0200                                                         
*                                                                               
LIST0200 EQU   *                                                                
         LA    RF,SPSSELH          SET A(FIRST SELECT FIELD)                    
         LA    RE,SPSLAST          SEND A(END OF SCREEN)                        
LIST0220 EQU   *                                                                
         CR    RE,RF               END OF SCREEN REACHED?                       
         BNH   LIST0240            YES - FINISHED                               
         OI    4(RF),X'20'         SET PREVIOUSLY VALID BIT                     
         LA    RF,SPSSEL1H-SPSSELH(RF)                                          
*                                  BUMP TO NEXT SELECT FIELD                    
         B     LIST0220            GO BACK FOR NEXT LINE                        
LIST0240 EQU   *                                                                
*                                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
*                                                                               
         GOTO1 FOUTBLK,DMCB,SPSSELH,SPSLAST,0,0                                 
*                                                                               
LIST0260 EQU   *                                                                
         LA    R2,SPSCONH          FIRST FIELD ON SCREEN                        
                                                                                
         CLI   NEXTSCRN,C'Y'       USER JUST PRESSED ENTER?                     
         BNE   LIST0320            W/O CHANGING THE KEY FIELDS                  
*                                  YES - DISPLAY NEXT PAGE OF CONTRACTS         
         XC    SCRNKEY,SCRNKEY     CLEAR FIRST SCROLLING KEY                    
         CLI   LASTSCRN,C'Y'       LAST SCREEN DISPLAYED?                       
         BNE   LIST0280            NO                                           
         MVI   LASTSCRN,C'*'       YES - SET 'NO SEQ FOR BF KEY'                
         CLI   KEYTYPE,X'BF'       X'BF' KEY SEQUENCE?                          
         BE    LIST0270            YES                                          
         MVI   LASTSCRN,C'N'       YES - SET LAST SCREEN DISPLAYED OFF          
LIST0270 EQU   *                                                                
         MVC   KEY,FIRSTKEY        RESET FIRST KEY TO RESTART                   
         B     LIST0300                                                         
LIST0280 EQU   *                                                                
         MVC   KEY,SAVEKEY         INSERT LAST CONTRACT DISPLAYED               
         CLI   KEYTYPE,X'BF'       AGENCY KEY SEQUENCE?                         
         BE    LIST0300            YES - DON'T MODIFY KEY                       
*                                  NO  - CLEAR X'8E' KEYS FOR RESTART           
         XC    KEY+16(11),KEY      CLEAR LOW-ORDER KEY                          
         MVI   KEY+16,10           SET TO SKIP-READ THIS KEY                    
                                                                                
LIST0300 EQU   *                                                                
         GOTO1 HIGH                                                             
         CLI   KEYTYPE,X'8E'       STATION KEY SEQUENCE?                        
         BE    LIST0400            YES - NO SEQUENTIAL READ NEEDED,             
*                                     PER NOTE BELOW                            
*                                                                               
*   X'8E' AND X'BF' KEYS ARE RESTARTED ON NEW SCREEN WITH THIS 'HIGH'           
*        CALL.  X'8E' RETURNED WILL BE PROCESSED AS THE NEXT RECORD.            
*        THIS IS BECAUSE THE KEY HAS BEEN MODIFIED TO RETRIEVE THE              
*        NEXT ORDER IN SEQUENCE FROM THAT SAVED.  THE X'BF' KEY ISN'T           
*        CHANGED. HOWEVER, THIS IS THE LAST KEY PROCESSED ON THE                
*        PREVIOUS SCREEN, AND IT HAS NOW BEEN MARKED AS DELETED,                
*        SO A READ FOR IT WILL RETURN THE NEXT KEY IN SEQUENCE.                 
*        THAT IS THE ONE THAT IS TO BE PROCESSED!                               
*        WHEN ALL DATA FOR THE X'BF' HAS BEEN DISPLAYED, AND THE                
*        SCREEN HAS TO BEGIN THE DISPLAY WITH THE FIRST SCREEN,                 
*        THE SEQUENTIAL READ CAN'T BE DONE, OR THE FIRST RECORD                 
*        THAT IS TO APPEAR WILL BE DROPPED.                                     
*                                                                               
         CLI   LASTSCRN,C'*'       ALL DATA DISPLAYED?                          
         BNE   LIST0400            NO  - KEY DELIVERED IS ONE TO                
*                                     BE PROCESSED                              
         MVI   LASTSCRN,C'N'       YES - NO SEQ READ NEEDED                     
         B     LIST0400                                                         
                                                                                
LIST0320 DS    0H                                                               
         XC    KEY,KEY                                                          
                                                                                
*                                                                               
*   ESTABLISH KEY BASED ON FILTER OPTIONS                                       
*                                                                               
         MVC   RCONSTYP,KEYTYPE    SET TYPE INTO KEY                            
*                                                                               
         CLI   KEYTYPE,X'BF'       AGENCY KEY SEQUENCE?                         
         BE    LIST0340            YES                                          
*                                  NO  - STATION SEQUENCE X'8E'                 
*                                                                               
         MVC   RCON8ERP,SRCEREP    INSERT SOURCE REP                            
         MVC   RCON8EST,SRCESTAT   INSERT TAKEOVER STATION                      
         B     LIST0360                                                         
LIST0340 EQU   *                   AGENCY KEY SEQUENCE X'BF'                    
         MVC   RCONCSRP,SRCEREP    INSERT SOURCE REP                            
         MVC   RCONCSAG(6),AGYFILT INSERT AGENCY FILTER                         
LIST0360 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     LIST0400                                                         
LIST0380 EQU   *                                                                
         GOTO1 SEQ                                                              
LIST0400 EQU   *                                                                
         L     RF,IOCOUNT          INCREASE IO COUNT                            
         LA    RF,1(RF)                                                         
         ST    RF,IOCOUNT                                                       
         CLC   IOCOUNT,=F'08000'   MAX IO COUNT REACHED?                        
***      CLC   IOCOUNT,=F'50000'   MAX IO COUNT REACHED?                        
         BL    LIST0420                                                         
         MVC   SAVEKEY,MAXIOKEY    SAVE KEY FOR RESTART                         
         MVI   MAXIOFLG,1          SET MAX IO ENCOUNTERED                       
         L     RF,MAXIOCTR         INCREASE MAX IO COUNT                        
         LA    RF,1(RF)                                                         
         ST    RF,MAXIOCTR                                                      
         B     LIST0980            END THIS CYCLE                               
LIST0420 EQU   *                                                                
         MVC   MAXIOKEY,KEY        SAVE KEY FOR MAXIO SITUATION                 
         CLC   KEY(8),KEYSAVE      SAME KEY TYPE/REP/STATION?                   
         BNE   LIST0920            NO  - FINISHED SWEEP                         
*                                                                               
         CLI   KEYTYPE,X'8E'       STATION SEQUENCE RETURN?                     
         BNE   LIST0580                                                         
         CLC   RCON8EFE,EFDTCOMP   KEY END DATE VS EFFECTIVE DATE               
*                                     ENDED BEFORE EFF DATE?:  SKIP             
         BNL   LIST0460            NO  - DATES INCLUDE EFFECTIVE DATE           
LIST0440 EQU   *                                                                
         B     LIST0560            RESTART ON NEXT KEY                          
LIST0460 EQU   *                                                                
DIE2     EQU   *                                                                
*                                                                               
         CLI   SPSAGIH+5,0         AGENCY FILTER?                               
         BE    LIST0500            NO  -                                        
         CLI   RCON8EID,1          YES - TYPE 1 KEY (AGENCY/ADVERT)?            
         BNE   LIST0500            NO  - FILTER TEST ALREADY DONE -             
*                                     NEXT FILTER(S) TO BE DONE                 
         CLC   AGYFILT+4(2),SPACES AGENCY-OFFICE IN FILTER?                     
         BE    LIST0480            NO                                           
         CLC   RCON8AGY,AGYFILT    YES - ORDER FOR AGENCY/OFF FILTER?           
*                                     COMPARE SIX CHARS                         
         BE    LIST0500            YES - CHECK NEXT FILTER                      
         B     LIST0560            NO  - SET TO SKIP                            
LIST0480 EQU   *                                                                
         CLC   RCON8AGY(4),AGYFILT YES - ORDER FOR AGENCY FILTER?               
*                                     COMPARE FOUR CHARS                        
         BNE   LIST0560            NO  - SET TO SKIP THIS CON                   
LIST0500 EQU   *                                                                
         CLI   SPSADVH+5,0         ADVERT FILTER?                               
         BNE   LIST0510            YES -                                        
*                                                                               
         CLC   =C'PV',SRCEREP                                                   
         BE    LIST0520                                                         
         CLC   =C'BL',SRCEREP                                                   
         BE    LIST0520                                                         
         DC    H'0'                MUST FILTER ON ADVERT                        
*                                                                               
LIST0510 EQU   *                                                                
         CLI   RCON8EID,1          YES - TYPE 1 KEY (AGENCY/ADVERT)?            
         BNE   LIST0520            NO  - FILTER TEST ALREADY DONE -             
         CLC   RCON8ADV,ADVFILT    YES - ORDER FOR ADVERT FILTER?               
         BNE   LIST0560            NO  - SET TO SKIP TO NEXT CON                
LIST0520 EQU   *                                                                
         CLI   RCON8EID,2          TYPE 2 KEY (SP/CONTYP/ETC)?                  
         BL    LIST0380            LOW  - GO BACK FOR TYP2                      
         BH    LIST0540            HIGH - CHECK FILTERS ON 3RD KEY              
         CLI   SPSSALH+5,0         S/P FILTER?                                  
         BE    LIST0540            NO                                           
         CLC   RCON8ESP,SPFILT     YES - ORDER FOR S/P?                         
         BNE   LIST0560            NO  - SKIP ORDER                             
LIST0540 EQU   *                                                                
         CLI   SPSOFFH+5,0         OFFICE FILTER?                               
         BE    LIST0700            NO  -                                        
         CLI   RCON8EID,3          YES - TYPE 3 KEY (OFF/DEMO/CREAT)?           
         BNE   LIST0380            NO  - GO BACK FOR NEXT KEY                   
         CLC   RCON8EOF,SPSOFF     ORDER FOR FILTER OFFICE?                     
         BE    LIST0700            YES - PROCESS ORDER                          
LIST0560 EQU   *                                                                
         MVI   RCON8EID,10         NO  - SET TYPE TO SKIP TO NEXT CON           
         XC    RCON8EAG(10),RCON8EAG                                            
*                                  CLEAR LOW KEY                                
         B     LIST0360            RESTART ON NEXT KEY                          
*                                                                               
*   SEQUENCE FOR X'BF' KEY PROCESSING: AGENCY HIGH                              
*                                                                               
LIST0580 DS    0H                                                               
         CLC   RCONCSFE,EFDTCOMP   KEY END DATE VS EFFECTIVE DATE               
*                                     ENDED BEFORE EFF DATE?:  SKIP             
         BNL   LIST0620            NO  - DATES INCLUDE EFFECTIVE DATE           
LIST0600 EQU   *                                                                
         B     LIST0380            READ SEQUENTIAL KEY NEXT                     
LIST0620 EQU   *                                                                
*                                                                               
         CLI   SPSAGIH+5,0         AGENCY FILTER?                               
         BE    LIST0660            NO  -                                        
         CLC   AGYFILT+4(2),SPACES AGENCY-OFFICE IN FILTER?                     
         BNH   LIST0640            NO                                           
         CLC   RCONCSAG(6),AGYFILT YES - ORDER FOR AGENCY/OFF FILTER?           
*                                     COMPARE SIX CHARS                         
         BE    LIST0660            YES - CHECK NEXT FILTER                      
         B     LIST0680            NO  - SET TO SKIP                            
LIST0640 EQU   *                                                                
         CLC   RCONCSAG(4),AGYFILT YES - ORDER FOR AGENCY FILTER?               
*                                     COMPARE FOUR CHARS                        
         BNE   LIST0680            NO  - SET TO SKIP THIS CON                   
LIST0660 EQU   *                                                                
         CLI   SPSADVH+5,0         ADVERT FILTER?                               
         BNE   LIST0665            YES -                                        
*                                                                               
         CLC   =C'PV',SRCEREP                                                   
         BE    LIST0670                                                         
         CLC   =C'BL',SRCEREP                                                   
         BE    LIST0670                                                         
*                                                                               
         DC    H'0'                MUST FILTER ON ADVERT                        
LIST0665 EQU   *                                                                
         CLC   RCONCSAD,ADVFILT    YES - ORDER FOR ADVERT FILTER?               
         BNE   LIST0680            NO  - SET TO SKIP TO NEXT CON                
*                                                                               
LIST0670 EQU   *                                                                
         CLI   SPSOFFH+5,0         OFFICE FILTER?                               
         BE    LIST0700            NO  -                                        
         CLC   RCONCSOF,SPSOFF     ORDER FOR FILTER OFFICE?                     
         BE    LIST0700            YES - PROCESS ORDER                          
LIST0680 EQU   *                                                                
         B     LIST0380            READ NEXT SEQUENTIAL KEY                     
*                                                                               
         DROP  R6                                                               
*                                                                               
LIST0700 DS    0H                                                               
         OI    DMINBTS,X'08'       READ DELETED                                 
         MVC   HIGHKEY,KEY                                                      
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08' RESET                                        
                                                                                
         L     R5,AIO              IN CASE RECORD IS DELETED BUT THE            
         USING RCONREC,R5             PASSIVE POINTERS AREN'T                   
         TM    RCONCNTL,X'80'      RECORD MARKED AS DELETE?                     
         BNO   LIST0720            NO                                           
         CLI   KEYTYPE,X'BF'       BF KEY SEQUENCE?                             
         BNE   LIST0560            NO  - 8E:  SKIP TO NEXT CONTRACT             
         B     LIST0380            YES - READ NEXT SEQUENTIAL KEY               
*                                                                               
LIST0720 EQU   *                                                                
*                                                                               
*   FOR KEY SEQUENCE BF, THE SALESPERSON CODE IS IN THE RECORD, NOT             
*        THE KEY.  THE FILTER MUST WAIT UNTIL THE RECORD HAS BEEN               
*        READ BEFORE THE TEST CAN BE ACCOMPLISHED.  BY THIS TIME,               
*        SUFFICIENT FILTERING HAS BEEN DONE SO THAT THIS SHOULDN'T              
*        BE TOO GREAT AN OVERHEAD.                                              
*                                                                               
         CLI   KEYTYPE,X'BF'       BF KEY SEQUENCE?                             
         BNE   LIST0740            NO  - SP FILTERING ALREADY DONE              
         CLI   SPSSALH+5,0         S/P FILTER?                                  
         BE    LIST0740            NO  - NO CHECK NEEDED                        
         CLC   RCONSAL,SPSSAL      YES - ORDER FOR S/P?                         
         BNE   LIST0380            NO  - SKIP ORDER                             
LIST0740 EQU   *                                                                
*                                                                               
*   CHECK IF ORDER IS ON THE STATION SIDE:  SKIP IF SO                          
*                                                                               
         LA    RF,RCONELEM                                                      
LIST0745 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    LIST0753            YES - SKIP IT                                
         CLI   0(RF),X'20'         SEND INFO ELEMENT?                           
         BE    LIST0747            YES                                          
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     LIST0745                                                         
LIST0747 EQU   *                                                                
         USING RCONSEND,RF                                                      
         TM    RCONSENF,X'10'                                                   
         BNO   LIST0753            STATION NOT ADVANCED: DON'T LIST             
         DROP  RF                                                               
*                                                                               
*                                                                               
*   CHECK IF COMBO ORDER, AND ALREADY INCLUDED ON SCREEN                        
*                                                                               
         BAS   RE,SCRNCMBO         COMBO - CHECK IF ON SCREEN                   
         BNZ   LIST0753            ALREADY ON SCREEN: SKIP                      
*                                                                               
         CLI   DAREFLAG,C'B'       DARE FLAG FILTER?                            
         BE    LIST0758            NO  - ACCEPT BOTH                            
         MVI   ELCODE,X'1D'        LOOK FOR DARE ELEMENT                        
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   LIST0755            NO ELEMENT - NOT DARE                        
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      FOUND:  LINKED?                              
         BNO   LIST0755            NO  - NOT DARE                               
         DROP  R6                                                               
         LA    R6,KEY              RESET R6 FOR SEQ PROCESSING                  
*                                                                               
*   1D FOUND, AND ORDER IS LINKED:  DARE ORDER                                  
*                                                                               
         CLI   DAREFLAG,C'Y'       ACCEPT ONLY DARE?                            
         BE    LIST0758            YES - ACCEPT THIS ONE                        
LIST0753 EQU   *                                                                
         CLI   KEYTYPE,X'BF'       BF KEY SEQUENCE?                             
         BNE   LIST0560            NO  - 8E:  SKIP TO NEXT CONTRACT             
         B     LIST0380            YES - SKIP THIS ONE                          
LIST0755 EQU   *                                                                
         LA    R6,KEY              RESET R6 FOR SEQ PROCESSING                  
*                                                                               
*   1D NOT FOUND, OR ORDER IS UNLINKED:  NOT A DARE ORDER                       
*                                                                               
         CLI   DAREFLAG,C'Y'       ACCEPT ONLY DARE?                            
         BNE   LIST0758            NO  - ACCEPT THIS ONE                        
         CLI   KEYTYPE,X'BF'       BF KEY SEQUENCE?                             
         BNE   LIST0560            NO  - 8E:  SKIP TO NEXT CONTRACT             
         B     LIST0380            YES - SKIP THIS ONE                          
*                                                                               
LIST0758 EQU   *                                                                
*                                                                               
*   TEST DIE                                                                    
***      MVC   DIE(2),=X'0000'                                                  
*   TEST DIE END                                                                
*                                                                               
LIST0780 DS    0H                                                               
         OC    FIRSTKEY,FIRSTKEY                                                
         BNZ   LIST0800                                                         
         MVC   FIRSTKEY,KEY        SAVE KEY TO BE DISPLAYED                     
LIST0800 DS    0H                                                               
         OC    SCRNKEY,SCRNKEY                                                  
         BNZ   LIST0820                                                         
         MVC   SCRNKEY,KEY         SAVE KEY TO BE DISPLAYED                     
LIST0820 DS    0H                                                               
*                                                                               
         L     RF,CONCOUNT         COUNT UP THE CONTRACTS                       
         LA    RF,1(RF)                                                         
         ST    RF,CONCOUNT                                                      
         CLC   CONCOUNT,=F'6'      SCREEN FULL?                                 
         BH    LIST0840            YES                                          
         GOTO1 DISCON,DMCB,(R2)    DISPLAY THE CONTRACT                         
         LA    R2,SPSCON1H-SPSCONH(R2)                                          
*                                  BUMP TO NEXT SCREEN FIELD                    
                                                                                
LIST0840 EQU   *                                                                
         CLI   CONCTFLG,C'Y'       CONTRACT COUNTER FLAG SET 'YES'?             
         BE    LIST0860            YES - DON'T STOP AT END OF SCREEN            
         LA    RF,SPSTAGH          A(END OF SCREEN)                             
         CR    R2,RF               END OF SCREEN REACHED?                       
         BNL   LIST0980            YES                                          
LIST0860 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY DISPLAYED                          
         MVC   KEY(16),HIGHKEY     RESTORE KEY THRU CONTRACT #                  
         CLI   KEYTYPE,X'BF'       BF KEY SEQUENCE?                             
         BE    LIST0380            YES - GO BACK FOR NXT SEQUENTIAL KEY         
         MVI   KEY+16,10           SET TO SKIP THIS SET OF KEYS                 
         B     LIST0360            NO  - GO BACK FOR ANOTHER CONTRACT           
                                                                                
LIST0920 DS    0H                  WE'VE HIT THE LAST CONTRACT                  
         L     RF,ACONTROL         SET A(CONTROL SAVE AREA)                     
         MVC   0(LCTLAREA,RF),CTRLAREA                                          
*                                  SAVE CONTROL AREA FOR RESTORE                
*                                     IF NECESSARY                              
         XC    SPSCTR(06),SPSCTR   CLEAR COUNTER                                
         CLI   CONCTFLG,C'Y'       COUNT CONTRACT OPTION?                       
         BNE   LIST0940            NO                                           
***      MVC   SPSCTR(6),=C'COUNT:'                                             
***      EDIT  CONCOUNT,(6,SPSCTR+7)                                            
         EDIT  CONCOUNT,(6,SPSCTR)                                              
         MVC   SAVEKEY,CON#KEY     INSERT LAST CONTRACT NUMBER                  
LIST0940 EQU   *                                                                
         FOUT  SPSCTRH             SET FOR TRANSMIT                             
         CLI   CONCTFLG,C'Y'       COUNT CONTRACT OPTION?                       
         BNE   LIST0960            NO  - DON'T TEST CONTRACT COUNT              
         CLC   CONCOUNT,=F'6'      YES - MORE THAN SIX  CONTRACTS?              
         BH    ENDLIST             YES - DON'T SET 'LAST CON FOUND'             
LIST0960 EQU   *                                                                
         MVI   LASTSCRN,C'Y'                                                    
         MVC   SAVEKEY,FIRSTKEY    NEXT SCREEN GOES TO THE BEGINNING            
         MVC   SPSLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
*                                                                               
         B     ENDLIST                                                          
LIST0980 DS    0H                                                               
*                                                                               
         MVI   LASTSCRN,C'N'                                                    
         CLI   MAXIOFLG,0          MAX IO INDICATOR SET?                        
         BNE   LIST1000            YES                                          
         MVC   SAVEKEY,CON#KEY     NO  - INSERT LAST CONTRACT NUMBER            
LIST1000 EQU   *                                                                
         MVC   SPSLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         L     RF,ACONTROL         SET A(CONTROL SAVE AREA)                     
         MVC   0(LCTLAREA,RF),CTRLAREA                                          
*                                  SAVE CONTROL AREA FOR RESTORE                
*                                     IF NECESSARY                              
         CLI   MAXIOFLG,0          MAX IO INDICATOR SET?                        
         BE    LIST1020            NO                                           
         EDIT  MAXIOCTR,(3,SPSOPT+9)                                            
         MVI   SPSOPT+8,C' '       SET SPACE                                    
         FOUT  SPSOPTH                                                          
         OC    CONCOUNT,CONCOUNT   ANY ORDERS FOUND?                            
         BZ    MAXSERCH            NO  - SET 'SEARCHING' MESSAGE                
         B     MAXDATA             YES - SET 'PROCESS' MESSAGE                  
LIST1020 EQU   *                                                                
         CLI   UPDTDONE,C'Y'       UPDATE DONE?                                 
         BNE   NEXTLIST            NO  - NO PAPERWORK NEEDED                    
LIST1040 DS    0H                                                               
*                                                                               
         B     NEXTUPDT            PUT OUT 'UPDATE MESSAGE'                     
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
         TITLE 'ADD CONTRACT RECORD'                                            
***********************************************************************         
* SCRNCMBO - A COMBO ORDER ALREADY ON THE SCREEN MAY CONTAIN THIS               
*        CONTRACT.  THIS WOULD RESULT IN DOUBLE PROCESSING, AND                 
*        MUST BE AVOIDED.                                                       
***********************************************************************         
SCRNCMBO NTR1                                                                   
         MVI   ELCODE,X'17'        FIND COMBO CONTROL ELEMENT                   
         L     R6,AIO              AIO -> RCONREC AT THIS TIME                  
         BAS   RE,GETEL                                                         
         BNE   SCMB0800            NO ELEMENT - NOT COMBO                       
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         SR    RE,RE                                                            
         SH    RF,=H'2'            SUBTRACT CONTROL FROM LENGTH                 
         LA    R1,9                                                             
         DR    RE,R1               DIVIDE ELT LEN BY DATA LEN                   
         LR    R0,RF               SET RESULT FOR LOOP                          
         LR    RE,RF               SAVE RESULT FOR DOUBLE LOOP                  
         LTR   R0,R0               ANYTHING IN REG?                             
         BZ    SCMB0800            NO  - NO CON#S IN CONTROL                    
         LA    R6,7(R6)            BUMP TO 1ST CONTRACT # IN CTL ELT            
         LR    R5,R6               SAVE A(FIRST CON# IN CONTROL ELT             
         L     RF,ACMBOSCR         SET A(COMBOS ON SCREEN TABLE)                
SCMB0020 EQU   *                                                                
         CLI   0(RF),0             SCREEN SLOT EMPTY?                           
         BE    SCMB0800            YES - CONTROL ENTRY NOT ON SCRN              
         CLC   0(4,R6),0(RF)       NO  - CONTRACT IN LIST?                      
         BE    SCMB0700            YES - EXIT W/CC NOT ZERO                     
         LA    R6,9(R6)            BUMP TO NEXT CON# IN CTL ELT                 
         BCT   R0,SCMB0020         GO BACK FOR NEXT                             
         LR    R0,RE               SET CTL ELT LOOP AGAIN                       
         LA    RF,4(RF)            BUMP TO NEXT SCREEN SLOT                     
         LR    R6,R5               RESET A(1ST CON # IN CONTROL ELT)            
         B     SCMB0020            GO BACK FOR NEXT                             
SCMB0700 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     SCMB0900                                                         
SCMB0800 EQU   *                                                                
         SR    R0,R0               EXIT CC ZERO                                 
SCMB0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ADDCONS  - PROCESS RECORD(S) SELECTED ON SCREEN                               
***********************************************************************         
ADDCONS  NTR1                                                                   
*                                                                               
         XC    ANEWCMBO,ANEWCMBO   CLEAR ADDR POINTER                           
         MVC   ACMBNTRY,ACMBOTBL   SET A(1ST ENTRY IN COMBO TABLE)              
         MVI   UPDTDONE,C'N'       SET 'UPDATE DONE' = NO                       
         GOTO1 =A(VALSCRN),DMCB,(RC),RR=Y                                       
*                                  VALIDATE SCREEN FIELDS                       
         BNZ   ACON0420            ERROR FOUND - EXIT                           
*                                                                               
ACON0010 EQU   *                                                                
         XC    WORK,WORK                                                        
         L     R5,AEQUITBL         SET A(EQUIVALENCY TABLE 1ST ENTRY)           
         ST    R5,AEQUNTRY         SAVE A(ENTRY IN PROGRESS)                    
         USING EQUITABL,R5                                                      
ACON0020 EQU   *                                                                
         MVI   CTLCOMBO,C'N'       SET 'NOT COMBO'                              
         TM    ETFLAG,X'80'        ENTRY 'SELECTED'?                            
         BO    ACON0060            YES - PROCESS IT                             
ACON0040 EQU   *                                                                
         LA    R5,LEQUITBL(R5)     BUMP TO NEXT ENTRY                           
         L     RF,ACMBNTRY         BUMP TO NEXT ENTRY IN COMBO TABLE            
         LA    RF,LCMBBUCK(RF)     BUMP TO CORRESP CMBO TABLE                   
         ST    RF,ACMBNTRY         PUT IT BACK                                  
         OC    0(6,R5),0(R5)       ANYTHING IN ENTRY?                           
         BZ    ACON0340            NO  - FINISHED WITH JOB                      
         B     ACON0020                                                         
ACON0060 EQU   *                                                                
         L     RF,ACMBNTRY         CHECK FOR COMBO ENTRY                        
         OC    0(LCMBNTRY,RF),0(RF)  ANY ENTRY?                                 
         BZ    ACON0080            NO                                           
         MVI   CTLCOMBO,C'Y'       SET COMBO ORDER 'YES'                        
ACON0080 EQU   *                                                                
         OI    ETFLAG,X'40'        MARK ENTRY 'PROCESSED'                       
         MVI   UPDTDONE,C'Y'       SET 'UPDATE DONE' = YES                      
ACON0100 EQU   *                                                                
         L     R4,AIO                                                           
         USING RCONREC,R4                                                       
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),ETDSKADR  SET DISK ADDR OF CONTRACT                    
         MVC   SRCECON,ETOCONUM    INSERT CONTRACT NUMBER                       
*                                                                               
         MVI   RDUPDATE,C'Y'       RETRIEVE FOR UPDATE                          
*                                                                               
         GOTO1 GETREC,DMCB,RCONREC                                              
*                                  RETRIEVE THE CONTRACT RECORD                 
*                                                                               
         MVC   KEY(27),RCONKEY     SET KEY FOR ADDREC                           
****>>>> GOTO1 =A(UPDTCON),DMCB,(RC),(R4),RR=Y                                  
*                                                                               
****     GOTO1 =A(TAKEDARE),DMCB,(RC),(R4),(R5),RR=Y                            
*                                  R5 -> EQUIV TABLE ENTRY IN PROCESS           
         GOTO1 =A(CONCREAT),DMCB,(RC),RR=Y                                      
*                                  ADD CONTRACTS AND POINTERS NOW               
ACON0240 DS    0H                                                               
*                                                                               
         CLI   CTLCOMBO,C'N'       NO COMBO WITH THIS ORDER?                    
         BE    ACON0320            NO COMBO - MOVE TO NEXT SCRN ENTRY           
         CLI   CTLCOMBO,C'Y'       YES - FIRST COMBO ORDER DONE?                
         BE    ACON0260            NO  - SET UP FOR 1ST                         
         ZIC   RF,CTLCOMBO         YES - BUMP TO NEXT ORDER                     
         B     ACON0280                                                         
ACON0260 EQU   *                                                                
         MVC   SAVCON#S(8),ETOCONUM                                             
         MVC   SAVGRPS(4),SRCEGRP  SAVE SOURCE/TARGET GROUPS                    
         MVC   SAVSTYPS(2),SRCETYPE                                             
*                                  SAVE SOURCE/TARGET TYPES                     
*                                  SAVE OLD/NEW CONTRACT NUMBERS                
         SR    RF,RF               SET COUNT TO ZERO                            
ACON0280 EQU   *                                                                
         LA    RF,1(RF)            BUMP TO 1ST/NEXT SLOT                        
         STC   RF,CTLCOMBO         STORE IT BACK                                
         CLI   CTLCOMBO,3          THREE PARTICIPATING CON#S DONE?              
         BH    ACON0300            YES - DON'T DO ANY MORE FOR                  
*                                     THIS BASE CONTRACT                        
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SR    RE,RE                                                            
         M     RE,=F'19'           CALC DISPL INTO CMBO TABL                    
         L     RE,ACMBNTRY         DOES A COMBO NEED DOING?                     
         AR    RE,RF               DISPLACE TO ENTRY                            
         OC    0(LCMBNTRY,RE),0(RE)  ANY ENTRY HERE?                            
         BZ    ACON0300            NO  - FINISHED                               
         MVC   ETOCONUM(8),ACMBOCON(RE)                                         
*                                  INSERT COMBO OLD/NEW ORDER #S                
         MVC   SRCEGRP(4),ACMBOGRP(RE)                                          
*                                  INSERT COMBO ORDER GROUP/SUBGRPS             
         MVC   SRCETYPE(2),ACMBOSTY(RE)                                         
*                                  INSERT COMBO ORDER STATION TYPES             
*                                                                               
*   RETRIEVE KEY FOR COMBO ORDER TO GET D/A INTO TABLE.                         
*                                                                               
         XC    KEY,KEY                                                          
         ZAP   DUB(5),=P'99999999'                                              
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),ETOCONUM    INSERT ORIGINAL ORDER NUMBER                 
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         MVI   KEY,X'8C'           SET UP KEY                                   
         MVC   KEY+21(2),SRCEREP   INSERT SOURCE REP CODE                       
         MVC   KEY+23(4),WORK      INSERT KEY                                   
*                                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NOT FOUND?                                   
         MVC   ETDSKADR,KEY+28     INSERT DISK ADDR INTO TABLE                  
*                                                                               
*   NOTE:  NO ATTEMPT IS MADE TO SAVE THE ORIGINAL D/A, AS IT                   
*        WILL NOT BE USED AGAIN.                                                
         B     ACON0100            GO BACK AND PROCESS IT                       
ACON0300 EQU   *                                                                
         MVC   ETOCONUM(8),SAVCON#S                                             
*                                  RESET ORIGINAL CON #S                        
         MVC   SRCEGRP(4),SAVGRPS  RESET ORIGINAL GROUP/SUBGROUPS               
         MVC   SRCETYPE(2),SAVSTYPS                                             
*                                  RESET ORIGINAL STATION TYPES                 
ACON0320 EQU   *                                                                
         B     ACON0040            GO BACK FOR NEXT SELECT                      
*                                                                               
ACON0340 EQU   *                                                                
         MVC   CONACT,SPACES                                                    
         MVC   CONACT(4),=C'LIST'  TURN IT TO LIST                              
         LA    RF,CONACTH          SET FLAGS ON FIELD                           
         OI    6(RF),X'80'         SET TO 'TRANSMIT'                            
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         L     R4,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
         USING EQUITABL,R4                                                      
*                                                                               
         LA    R3,SPSSELH          SET A(1ST SCREEN SELECT FIELD)               
ACON0360 EQU   *                                                                
         CLI   TAKEALL,C'Y'        ALL CONTRACTS SELECTED?                      
         BE    ACON0380            YES                                          
         CLI   5(R3),1             USER ONLY INPUT 'S' - NO 'S+'                
         BNE   ACON0400            ONLY INPUT LEN OK                            
         CLI   8(R3),C'S'          CONTRACT SELECTED?                           
         BNE   ACON0400            NO  - SKIP IT                                
ACON0380 EQU   *                                                                
**                                                                              
*   THIS SERVES AS AN INDICATOR, BUT WON'T BE SET DURING THE RUN OF             
*        THIS PROGRAM.  HOW CAN THIS BE BYPASSED?                               
*                                                                               
***      OC    ETNCONUM,ETNCONUM   ANYTHING IN FIELD?                           
***      BZ    ACON0400            NO  - DON'T FLAG AS 'SELECTED'               
         MVC   8(2,R3),=C'*S'      YES - INDICATE 'PROCESSED'                   
         FOUT  (R3)                                                             
ACON0400 EQU   *                                                                
         LA    R4,LEQUITBL(R4)     BUMP TO NEXT EQUIV ENTRY                     
         LA    R3,SPSSEL1H-SPSSELH(R3)                                          
*                                  BUMP TO NEXT SELECT FIELD                    
         LA    RF,SPSTAGH          CHECK FOR END OF SCREEN                      
         CR    R3,RF               END OF SCREEN REACHED?                       
         BL    ACON0360            NO  - GO BACK AND CHECK NEXT                 
*                                                                               
         SR    R0,R0               YES - SET CC ZERO FOR RETURN                 
*                                                                               
         DROP  R4                                                               
ACON0420 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
****>>>                                                                         
*              ROUTINE TO FOUT BLANKS                                           
         SPACE 1                                                                
*              PARAMETER  1        A(FIRST FIELD)                               
*              PARAMETER  2        A(END-ADDR)  EX=BUYLAST                      
*              PARAMETER  3        0=DO NOT FOUT IF SPACES                      
*                                  1=FOUT IF NOT SPACES                         
*              PARAMETER  4        0=CLEAR PROTECTED FIELDS ALSO                
*                                  1=DON'T CLEAR PROTECTED FIELDS               
         SPACE 1                                                                
FOUTBLK  NTR1                                                                   
         LM    R2,R5,0(R1)                                                      
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(79),WORK2                                                
         SPACE 1                                                                
FOUT1    ZIC   RE,0(R2)                                                         
         LTR   R5,R5               CLEAR PROTECTED FIELDS ALSO?                 
         BZ    FOUT1A              YES - SKIP PROTECTED TEST                    
         TM    1(R2),X'20'         NO  - PROTECTED?                             
         BO    FOUT9               YES - DON'T CLEAR                            
FOUT1A   EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*        SPECIAL TEST TO LEAVE SPECIFIC PROTECTED FIELDS ALONE                  
*                                                                               
         CLC   =C'NEW->',8(R2)     NEW LINE INDICATOR?                          
         BE    FOUT9               YES - DON'T CLEAR IT                         
         LR    RF,RE                                                            
         SH    RF,=H'9'                                                         
         TM    1(R2),X'02'         EXTENDED FLD HDR?                            
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         SPACE 1                                                                
         EX    RF,ORSPAC                                                        
         EX    RF,COMPSPAC                                                      
         BE    FOUT9               ALREADY                                      
         LTR   R4,R4                                                            
         BP    *+8                 SENDING  NON MYSPACES DATA                   
         EX    RF,MOVESPAC         CLEARING SCREEN                              
         FOUT  (R2)                                                             
         SPACE 1                                                                
FOUT9    LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3               LAST FIELD                                   
         BL    FOUT1                                                            
         XIT1                                                                   
         SPACE 1                                                                
ORSPAC   OC    8(0,R2),WORK2                                                    
COMPSPAC CLC   8(0,R2),WORK2                                                    
MOVESPAC MVC   8(0,R2),WORK2                                                    
         EJECT                                                                  
****>>>                                                                         
*    CHKCMPSP:  CHECK PROFILE TO SEE IF COMPENSATION S/P IS BEING               
*        USED BY EACH REP.                                                      
*                                                                               
CHKCMPSP NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           SET KEY FOR REP RECORD                       
         MVC   KEY+25(2),TWAAGY    INSERT REP CODE                              
         GOTO1 HIGH                READ REP RECORD                              
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - HOW COULD IT BE MISSING?               
         MVC   SAVEAIO,AIO         SAVE ORIGINAL AIO                            
         MVC   AIO,AIO2                                                         
         L     R3,AIO2             USE SECOND IO AREA FOR TKO                   
         USING RREPREC,R3          RETRIEVE S/P REC INTO IO AREA 2              
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         MVC   AIO,SAVEAIO         RESET ORIGINAL AIO                           
                                                                                
         LA    R2,RREPELEM         X'01' ELEMENT                                
CCMP0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CCMP0800            YES - NO PROFILE ELEMENT                     
         CLI   0(R2),4             PROGRAM PROFILE ELEMENT?                     
         BE    CCMP0040            YES                                          
         ZIC   RE,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RE                                                            
         B     CCMP0020                                                         
CCMP0040 EQU   *                                                                
         ZIC   RF,2(R2)            NUMBER OF PROFILE ENTRIES                    
         LA    R2,4(R2)            SET A(1ST PROFILE)                           
CCMP0060 EQU   *                                                                
         CLI   0(R2),RREPQCNT      CONTRACT PROFILE?                            
         BE    CCMP0080            YES - TEST PROPER SETTING                    
         LA    R2,RREPPGML(R2)     NO  - BUMP TO NEXT PROFILE                   
         BCT   RF,CCMP0060         GO BACK FOR NEXT                             
         B     CCMP0800            NO CONTRACT PROFILE                          
CCMP0080 EQU   *                                                                
         TM    8(R2),X'80'         PAY S/P PROFILE SET?                         
         BNO   CCMP0800            NO  - EXIT                                   
         L     RF,AMISFLGS         YES - SET FLAG                               
         USING MISFLGS,RF                                                       
         OI    MISFLAGS,X'10'      TURN ON 'COMP S/P USED'                      
CCMP0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
****>>>                                                                         
*                                                                               
*   CHEKAGI: VALIDATE EXISTENCE OF AGENCY.                                      
*        P1  ->  SCREEN FIELD HEADER                                            
*        P2  ->  STORAGE FOR OUTPUT OF ROUTINE                                  
*        P3  ->  STORAGE FOR DARE EQUIVALENCY AGY/OFFICES                       
*        IF POS 1, P1 = 0, DON'T CHECK AGENCY OFFICE                            
*        IF POS 1, P1 = 1, CHECK AGENCY OFFICE REQUIREMENT                      
*        IF NOT FOUND, CC NOT ZERO WILL PRODUCE ERROR MESSAGE.                  
*                                                                               
CHEKAGI  NTR1                                                                   
*                                                                               
         MVC   BYTE,DMCB           SAVE FLAG                                    
         ZICM  R2,DMCB+1,3         SET A(FIELD IN QUESTION)                     
         L     R3,4(R1)            SET A(STORAGE FOR OUTPUT)                    
         L     R4,8(R1)            SET A(STORAGE FOR EQUIVS)                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'           SET KEY FOR AGENCY RECORD                    
         MVC   KEY+19(6),SPACES    SPACE FILL AGY PORTION                       
         LR    RF,R2               SET A(AGENCY FIELD IN QUESTION)              
         ZIC   R0,5(RF)            SET COUNT FOR LOOP - USE FIELD LEN           
         LA    RF,8(RF)            BUMP TO DATA IN FIELD                        
         LA    RE,KEY+19           SET A(KEY FIELD)                             
CAGI0040 EQU   *                                                                
         CLI   0(RF),C'-'          SEPARATOR ENCOUNTERED?                       
         BNE   CAGI0080            NO  - MOVE CHARACTER                         
         LA    RF,1(RF)            YES - SKIP SEPARATOR                         
         LA    RE,KEY+23           NEXT CHARS MUST BE OFFICE                    
*                                                                               
*    NOTE:  FIELD MAY RUN OVER ACTUAL KEY.  THIS SHOULDN'T BE                   
*        A PROBLEM, AND IT SHOULD BE CLEANED UP WITHIN KEY BUILDING             
*                                                                               
CAGI0080 EQU   *                                                                
         MVC   0(1,RE),0(RF)       MOVE CHARACTER TO KEY                        
         LA    RF,1(RF)            BUMP TO NEXT POSITION                        
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         BCT   R0,CAGI0040         GO BACK FOR NEXT CHARACTER                   
*                                  ALL CHARS PROCESSED                          
         MVC   KEY+25(2),SRCEREP   INSERT SOURCE REP CODE                       
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    CAGI0160            YES - KEY VALID AS ENTERED                   
CAGI0120 EQU   *                   NO  - KEY NOT FOUND                          
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     CAGI0200                                                         
CAGI0160 EQU   *                                                                
         MVC   0(6,R3),KEY+19      SAVE KEY FOR FILTERING                       
         CLI   BYTE,0              NEED AGENCY OFFICE?                          
         BE    CAGI0180            NO                                           
         CLC   4(2,R3),=C'  '      YES - OFFICE ENTERED ALREADY?                
         BH    CAGI0180            YES - NO CHECK NEEDED                        
CAGI0170 EQU   *                                                                
         GOTO1 SEQ                 NO  - READ FOR ANOTHER KEY                   
         CLC   KEY+25(2),SRCEREP   SAME REP?                                    
         BNE   CAGI0170            NO  - SKIP THIS RECORD                       
         CLC   KEY+19(4),0(R3)     YES - SAME AGENCY FOUND?                     
         BNE   CAGI0180            NO  - NO OFFICE NEEDED                       
         MVI   BYTE,X'FF'          SET ERROR FLAG: NEED OFFC                    
         B     CAGI0120            RETURN WITH ERROR                            
CAGI0180 EQU   *                                                                
         BAS   RE,CHEKAG2                                                       
*                                                                               
         MVI   BYTE,0              CLEAR ERROR RETURN FLAG                      
         SR    R0,R0               SET CC = ZERO                                
CAGI0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
CHEKAG2  NTR1                                                                   
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'1A'                                                        
         MVC   KEY+19(6),0(R3)     INSERT FILTER KEY                            
         MVC   KEY+25(2),SRCEREP                                                
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE THERE                          
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         LA    R2,34(R2)                                                        
CHAG0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
         CLI   0(R2),X'10'         AGENCY FAX ELEMENT?                          
         BE    CHAG0040            YES - PROCESS                                
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     CHAG0020            GO BACK FOR NEXT                             
CHAG0040 EQU   *                                                                
         USING RAGY2FXE,R2                                                      
         MVC   0(20,R4),RAGY2DAR                                                
         DROP  R2                                                               
         LA    R2,4                                                             
         LR    R3,R4                                                            
CHAG0060 EQU   *                                                                
         OC    0(5,R3),0(R3)       ANY ENTRY?                                   
         BNZ   CHAG0080            YES                                          
         MVI   0(R3),X'FF'         NO  - FORCE HIGH                             
CHAG0080 EQU   *                                                                
         LA    R3,5(R3)                                                         
         BCT   R2,CHAG0060         GO BACK FOR NEXT                             
*                                                                               
*    AGY/OFF EQUIVS WILL BE SORTED.                                             
*        R4 -> TABLE FROM AGY2 RECORD                                           
         LA    R3,4                SET NUMBER OF RECORDS                        
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),5,5,0                                   
         XIT1                                                                   
         EJECT                                                                  
****>>>                                                                         
****>>>                                                                         
*                                                                               
*   CHEKSAL: ACCESS THE REP SALESPERSON RECORD TO VALIDATE S/P                  
*        EXISTENCE.                                                             
*        P1        =  A(INPUT FIELD HEADER)                                     
*        P1/BYTE 0 =  FLAG:  0 = S/P FILTER, 1 = NEW S/P                        
*        P2        =  A(OUTPUT STORAGE)                                         
*                                                                               
CHEKSAL NTR1                                                                    
*                                                                               
         XC    DUB,DUB                                                          
         MVC   FLAGSAL,0(R1)       SAVE FLAG BYTE                               
         ZICM  R2,1(R1),3          SET A(INPUT HEADER)                          
         MVC   FULL,4(R1)          SAVE A(OUTPUT STORAGE)                       
         MVI   BYTE,0              CLEAR S/P ERROR FLAG                         
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,6               INSERT RECORD TYPE                           
         MVC   KEY+22(2),TWAAGY    INSERT REP CODE                              
         MVC   KEY+24(3),SPACES    CLEAR CODE IN KEY                            
         ZIC   RF,5(R2)            SET L(INPUT)                                 
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,CSAL0020         MOVE CODE BY LENGTH                          
         B     CSAL0040                                                         
CSAL0020 MVC   KEY+24(0),8(R2)     MOVE CODE BY LENGTH                          
CSAL0040 EQU   *                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BE    CSAL0200            YES - ACCEPTED                               
CSAL0160 EQU   *                                                                
         LTR   RB,RB               NO  - SET CC NOT = ZERO                      
         B     CSAL0600                                                         
CSAL0200 EQU   *                                                                
         CLI   FLAGSAL,0           SALESPERSON FILTER?                          
         BE    CSAL0700            YES - ON FILE: ACCEPTED                      
*                                  NO  - NEW S/P CHECK FURTHER                  
         MVC   SAVEAIO,AIO         SAVE ORIGINAL AIO                            
         MVC   AIO,AIO2                                                         
         L     R3,AIO2             USE SECOND IO AREA FOR TKO                   
         USING RSALREC,R3          RETRIEVE S/P REC INTO IO AREA 2              
*                                                                               
         GOTO1 GETREC,DMCB,RSALREC                                              
*                                                                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL AIO                           
*                                                                               
         OC    RSALLEAV,RSALLEAV   S/P LEAVE DATE?                              
         BZ    CSAL0210            NO  - CONTINUE                               
         MVI   BYTE,1              SET S/P LEAVE FLAG                           
         LTR   RB,RB               NO  - SET CC NOT = ZERO                      
         B     CSAL0600                                                         
CSAL0210 EQU   *                                                                
         OC    OFFTEAMS,OFFTEAMS   ANY OFFICE/TEAMS?                            
         BZ    CSAL0280            NO  - USE AS IS                              
         LA    R4,15               15 SETS MAXIMUM                              
         LA    R5,OFFTEAMS                                                      
*                                                                               
CSAL0220 EQU   *                                                                
         CLC   RSALOFF,0(R5)       OFFICE IN LIST?                              
         BE    CSAL0240            YES                                          
         LA    R5,4(R5)            BUMP TO NEXT ENTRY                           
         BCT   R4,CSAL0220                                                      
         B     CSAL0280            OFFICE NOT IN LIST - USE                     
*                                                                               
*                                  OFFICE MAY BE IN LIST MORE                   
*                                     THAN ONE TIME                             
*                                                                               
CSAL0240 DS    0H                  OFFICE IS IN OFFTEAM LIST,                   
         CLC   RSALOFF,0(R5)       OFFICE IN LIST?                              
         BNE   CSAL0260            NO                                           
         CLC   RSALTEAM,2(R5)      YES - TEAM FOR THIS OFFICE?                  
         BNE   CSAL0260            NO                                           
         MVC   SAVESPOF,RSALOFF    SAVE SALESPERSON OFFICE                      
         MVC   SAVESPTM,RSALTEAM   SAVE SALESPERSON TEAM                        
         B     CSAL0280                                                         
CSAL0260 LA    R5,4(R5)            NO  - BUMP TO NEXT OFFICE                    
         BCT   R4,CSAL0240         GO BACK FOR NEXT OFF/TEAM                    
         LA    RF,288              TEAM NOT ALLOWED TO SELL OFFICE              
         ST    RF,DUB              SAVE ERROR                                   
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CSAL0600                                                         
CSAL0280 EQU   *                   MATCHUP:  USE IT                             
         L     RF,FULL             SET A(OUTPUT STORAGE)                        
         MVC   0(3,RF),KEY+24      SAVE S/P DEFAULT VALUE                       
         MVC   SAVESPOF,RSALOFF    SAVE SALESPERSON OFFICE                      
         MVC   SAVESPTM,RSALTEAM   SAVE SALESPERSON TEAM                        
         B     CSAL0800                                                         
CSAL0600 EQU   *                                                                
         NI    4(R2),X'FF'-X'20'                                                
         B     CSAL0800                                                         
*                                  TURN OFF PREVALID                            
CSAL0700 EQU   *                                                                
         L     RF,FULL             SET A(OUTPUT STORAGE)                        
         MVC   0(3,RF),KEY+24      SAVE S/P FILTER VALUE                        
         SR    R0,R0               SET CC ZERO                                  
CSAL0800 EQU   *                                                                
*                                                                               
         DROP  R3                                                               
*                                                                               
         XIT1                                                                   
FLAGSAL  DS    CL1                                                              
         DS    0H                                                               
         EJECT                                                                  
*******************************************************************             
* DISPLAY CONTRACT INFORMATION                                                  
*    1ST PARAMETER HAS A(STARTING FIELD HEADER)                                 
*    AIO HAS ADDRESS OF CONTRACT RECORD                                         
*******************************************************************             
DISCON   NTR1                                                                   
         L     R4,0(R1)            SET A(NEXT SCREEN LINE)                      
*                                                                               
         L     R5,AIO                                                           
         USING RCONREC,R5                                                       
*                                                                               
         MVC   CON#KEY,KEY         SAVE CONTRACT RECORD X'8D/E' KEY             
*                                                                               
         MVC   LISTAR,SPACES       CLEAR DISPLAY LINES                          
         MVC   LISTAR2,SPACES                                                   
*                                                                               
         LA    R2,LISTAR+1         SET TO DATA FIELD OF LINE                    
         USING DISLIN1,R2                                                       
         LA    R3,LISTAR2          SET TO DATA FIELD OF LINE                    
         USING DISLIN2,R3                                                       
         GOTO1 HEXOUT,DMCB,RCONKCON,DL1CON#,4,=C'TOG'                           
*                                  INSERT CONTRACT NUMBER ON LINE               
         MVC   DL1AGY(4),RCONKAGY                                               
         CLC   RCONKAGY+4(2),SPACES                                             
*                                  AGENCY OFFICE IN KEY?                        
         BNH   DISC0010            NO                                           
         MVI   DL1AGY+4,C'-'       YES                                          
         MVC   DL1AGY+5(2),RCONKAGY+4                                           
DISC0010 EQU   *                                                                
         MVC   DL1ADV,RCONKADV     INSERT ADVERTISER CODE                       
         MVC   DL1STA,SPACES       CLEAR STATION FIELD                          
         MVC   DL1STA(4),RCONKSTA  INSERT STATION CALLS                         
         CLI   RCONKSTA+4,C'T'     TELEVISION STATION?                          
         BE    DISC0020            YES - NO MEDIA DISPLAYED                     
         CLI   RCONKSTA+4,C' '     NO MEDIA?                                    
         BE    DISC0020            YES - NO MEDIA DISPLAYED                     
         MVI   DL1STA+4,C'-'                                                    
         MVC   DL1STA+5(1),RCONKSTA+4                                           
*                                  INSERT MEDIA CODE                            
DISC0020 EQU   *                                                                
         CLI   DL1STA+3,C' '       THREE-CHARACTER STATION CODE?                
         BNE   DISC0030            NO                                           
         MVC   DL1STA+3(2),DL1STA+4                                             
*                                  YES - SLIDE IT OVER 1 POSITION               
         MVI   DL1STA+6,C' '       CLEAR LAST CHARACTER                         
DISC0030 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,DL1FLITE)                            
         MVI   DL1FLITE+8,C'-'                                                  
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,DL1FLITE+9)                        
         MVC   DL1SALEP,RCONSAL    INSERT SALESPERSON                           
*                                                                               
*   NO NEED TO                                                                  
*       RESTORE CONTRACT RECORD AS LAST ONE READ                                
*                                                                               
****     GOTO1 GETREC                                                           
*                                                                               
*   CYCLE X'03' ELEMENTS, ACCUMULATING ESTIMATED DOLLARS                        
*                                                                               
*                                                                               
*   TEST BUCKETS                                                                
*        L     RF,TESTCTR                                                       
*        LA    RF,1(RF)                                                         
*        ST    RF,TESTCTR                                                       
*        CLI   TESTCTR+3,2                                                      
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*   TEST BUCKETS END                                                            
*                                                                               
         SR    RE,RE               CLEAR ACCUMULATOR                            
         LA    R1,RCONELEM                                                      
DISC0080 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    DISC0160            YES                                          
         CLI   0(R1),3             ESTIMATE DOLLAR ELEMENT?                     
         BE    DISC0090            YES - ACCUMULATE IT                          
         CLI   0(R1),X'63'         TRADE ESTIMATE DOLLAR ELEMENT?               
         BNE   DISC0100            NO  - SKIP IT                                
DISC0090 EQU   *                                                                
         ZICM  RF,6(R1),4          YES - GET AMOUNT FROM ELEMENT                
         AR    RE,RF                                                            
DISC0100 EQU   *                                                                
         ZIC   RF,1(R1)            L(ELEMENT)                                   
         AR    R1,RF               BUMP TO NEXT ELEMENT                         
         B     DISC0080            GO BACK FOR NEXT                             
DISC0160 EQU   *                                                                
         EDIT  (RE),(16,DL1DOLRS),2,COMMAS=YES                                  
*                                                                               
         MVC   8(74,R4),LISTAR     LOAD 1ST LINE TO SCREEN                      
         MVI   8(R4),C'>'          REINSERT LINE FLAG                           
         LA    R4,SPSDAR#H-SPSCONH(R4)                                          
*                                  BUMP TO NEXT LINE                            
         DROP  R2                                                               
*                                                                               
****>>>  MVC   8(5,R4),=C'DARE>'   REINSERT LINE LITERAL                        
         MVI   ELCODE,X'17'        FIND COMBO CONTROL ELEMENT                   
         LA    R6,RCONREC                                                       
         MVC   DUB(4),RCONKCON     SAVE CONTRACT NUMBER                         
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   DISC0165            NO ELEMENT                                   
*                                                                               
*   PUT MAIN CONTRACT NUMBER IN COMBO ON SCREEN TABLE                           
*                                                                               
         L     RF,ACMBSCIT         SET A(NEXT OPEN SLOT)                        
         MVC   0(4,RF),RCONKCON    INSERT CONTRACT NUMBER                       
         LA    RF,4(RF)            SET TO NEXT SLOT                             
         ST RF,ACMBSCIT            SAVE A(NEXT OPEN SLOT)                       
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         SR    RE,RE                                                            
         SH    RF,=H'2'            SUBTRACT CONTROL FROM LENGTH                 
         LA    R1,9                                                             
         DR    RE,R1               DIVIDE ELT LEN BY DATA LEN                   
         LR    R0,RF               SET RESULT FOR LOOP                          
         LTR   R0,R0               ANYTHING IN REG?                             
         BZ    DISC0165            NO  - NO OUTPUT                              
         MVC   DL2COMBO,=C'COMBO:'                                              
         LA    R6,2(R6)            BUMP TO FIRST STATION IN LIST                
         LA    R2,DL2STA1          SET A(1ST STATION IN LINE)                   
*                                                                               
DISC0162 EQU   *                                                                
         CLC   DUB(4),5(R6)        CONTRACT OF ORDER?                           
         BE    DISC0163            YES - DON'T PUT ON NEXT LINE                 
*                                                                               
         MVC   0(4,R2),0(R6)       INSERT STATION CALLS                         
         MVI   4(R2),C'-'          INSERT SEPARATOR                             
*                                  COMBOS WILL ALWAYS BE RADIO                  
         MVC   5(1,R2),4(R6)       INSERT MEDIA                                 
         MVI   6(R2),C'='          INSERT SEPARATOR                             
         LA    RF,5(R6)                                                         
         ST    RF,DMCB                                                          
         LA    RF,7(R2)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 HEXOUT,DMCB,,,4,=C'TOG'                                          
*                                  INSERT CONTRACT NUMBER                       
         LA    R2,16(R2)           BUMP TO NEXT POSITION IN LINE                
DISC0163 EQU   *                                                                
         LA    R6,9(R6)            BUMP TO NEXT POSITION IN ELEMENT             
         BCT   R0,DISC0162         GO BACK FOR NEXT SLOT                        
*                                                                               
DISC0165 EQU   *                                                                
         MVI   ELCODE,X'A2'        FIND ESTIMATE ELEMENT                        
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   DISC0170            NO DARE                                      
         USING RCONIEL,R6                                                       
         CLC   RCONIEST,SPACES     SHORT EST# ENTERED?                          
         BNH   DISC0168            NO                                           
         MVC   DL2EST#(4),RCONIEST YES                                          
         B     DISC0170                                                         
DISC0168 EQU   *                                                                
         MVC   DL2EST#,RCONXEST    INSERT ESTIMATE NUMBER                       
DISC0170 EQU   *                                                                
         MVI   ELCODE,X'1D'        FIND DARE ELEMENT                            
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   DISC0175            NO DARE                                      
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      LINKED?                                      
         BNO   DISC0172            NO  - DON'T DISPLAY                          
         GOTO1 HEXOUT,DMCB,RCONDRLK,DL2DARE,4,=C'TOG'                           
*                                  INSERT DARE AGENCY ORDER #                   
DISC0172 EQU   *                                                                
         DROP  R3,R6                                                            
*                                                                               
DISC0175 EQU   *                                                                
         MVC   8(74,R4),LISTAR2    LOAD 2ND LINE TO SCREEN                      
         FOUT  (R4)                                                             
*                                                                               
         L     R2,AEQUNTRY         SET A(EQUIVALENCY TABLE ENTRY)               
         USING EQUITABL,R2                                                      
*                                                                               
         MVC   ETOAGYOF,RCONKAGY   LOAD ORIGINAL AGENCY/OFFICE                  
         MVC   ETOADVRT,RCONKADV   LOAD ORIGINAL ADVERTISER                     
         MVC   ETOSALEP,RCONSAL    LOAD ORIGINAL SALESPERSON                    
         MVC   ETOCNTYP,RCONTYPE   LOAD ORIGINAL CONTRACT TYPE                  
         MVC   ETOCONUM,RCONKCON   LOAD ORIGINAL CONTRACT NUMBER                
         MVC   ETDSKADR,CON#KEY+28 LOAD ORIGINAL CONTRACT DISK ADDR             
         OC    ETFLAG(1),CNFORVER  SET CONFIRM-OR-VERSION FLAG                  
*                                                                               
         LA    RF,RCONELEM         SET A(1ST ELEMENT)                           
DISC0180 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    DISC0220            YES - FINISHED                               
         CLI   0(RF),X'18'         DEVELOPMENTAL ELEMENT?                       
         BE    DISC0200            YES                                          
         ZIC   RE,1(RF)                                                         
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     DISC0180            GO BACK FOR NEXT                             
DISC0200 EQU   *                                                                
         MVC   ETODVSAL,2(RF)      LOAD ORIGINAL DEV SALESPERSON                
         MVC   ETODVTYP,5(RF)      LOAD ORIGINAL DEV CON TYPE                   
DISC0220 EQU   *                                                                
*                                                                               
         MVC   EQIVKEY,KEY         SAVE KEY OF CONTRACT FOR RESTART             
*                                                                               
         GOTO1 =A(CHKCOMBO),DMCB,(RC),(R2),(R5),RR=Y                            
*                                  CHECK IF PARTICIPATING IN COMBO              
         LA    R2,LEQUITBL(R2)     BUMP TO NEXT EQUIV ENTRY                     
         ST    R2,AEQUNTRY         SAVE A(NEXT SLOT)                            
         XIT1                                                                   
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
         EJECT                                                                  
*******************************************************************             
* GET ADVERTISER EXPANDED NAME                                                  
* P1 HAS ADV CODE                                                               
* WORK WILL CONTAIN EXPANDED NAME                                               
*******************************************************************             
ADVNAME  NTR1                                                                   
         MVC   SEQKEY,KEY                                                       
         XC    WORK,WORK                                                        
         L     R1,0(R1)                                                         
         MVC   WORK(L'RADVKADV),0(R1)                                           
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,0(R1)                                                   
         MVC   RADVKREP,SRCEREP                                                 
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   ADVNAMX                                                          
                                                                                
         MVC   SAVEAIO,AIO         SAVE A(IO AREA)                              
         MVC   AIO,AIO2            RESET A(IO AREA) TO IOAREA2                  
         L     R6,AIO2                                                          
         USING RADVREC,R6                                                       
                                                                                
         GOTO1 GETREC,DMCB,RADVREC                                              
*                                                                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
         MVC   WORK(L'RADVNAME),RADVNAME                                        
                                                                                
         DROP  R6                                                               
                                                                                
ADVNAMX  DS    0H                                                               
         MVC   KEY,SEQKEY          RE-ESTABLISH SEQ                             
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* GET AGENCY EXPANDED NAME                                                      
* P1 HAS AGY CODE AND OFFICE                                                    
* WORK WILL CONTAIN EXPANDED NAME                                               
*******************************************************************             
AGYNAME  NTR1                                                                   
         MVC   SEQKEY,KEY                                                       
         XC    WORK,WORK                                                        
         L     R1,0(R1)                                                         
         MVC   WORK(L'RAGYKAGY+L'RAGYKAOF),0(R1)                                
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGYKEY,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(L'RAGYKAGY+L'RAGYKAOF),0(R1)                            
         MVC   RAGYKREP,SRCEREP                                                 
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   AGYNAMX                                                          
*                                                                               
         MVC   SAVEAIO,AIO         SAVE A(IO AREA)                              
         MVC   AIO,AIO2            RESET A(IO AREA) TO IOAREA2                  
         L     R6,AIO2                                                          
         USING RAGYREC,R6                                                       
                                                                                
         GOTO1 GETREC,DMCB,RAGYREC                                              
*                                                                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
         MVC   WORK(L'RAGYNAM1),RAGYNAM1                                        
                                                                                
         LA    RF,IO                                                            
         ST    RF,AIO                                                           
         DROP  R6                                                               
                                                                                
AGYNAMX  DS    0H                                                               
         MVC   KEY,SEQKEY          RE-ESTABLISH SEQ                             
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* GET PRODUCT EXPANDED NAME                                                     
* P1 HAS ADV CODE                                                               
* P2 HAS PRD CODE                                                               
* WORK WILL CONTAIN EXPANDED NAME                                               
*******************************************************************             
PRDNAME  NTR1                                                                   
         MVC   SEQKEY,KEY                                                       
         XC    WORK,WORK                                                        
         L     RF,0(R1)                                                         
         MVC   ADVCODE,0(RF)                                                    
         L     RF,4(R1)                                                         
         MVC   WORK(L'RCONPRD),0(RF)                                            
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRDKEY,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,ADVCODE                                                 
         MVC   RPRDKPRD,WORK                                                    
         MVC   RPRDKREP,SRCEREP                                                 
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RPRDKEY),KEYSAVE                                           
         BNE   PRDNAMX                                                          
                                                                                
         MVC   SAVEAIO,AIO         SAVE A(IO AREA)                              
         MVC   AIO,AIO2            RESET A(IO AREA) TO IOAREA2                  
         L     R6,AIO2                                                          
         USING RPRDREC,R6                                                       
*                                                                               
         GOTO1 GETREC,DMCB,RPRDREC                                              
*                                                                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
         MVC   WORK(L'RPRDNAME),RPRDNAME                                        
                                                                                
         DROP  R6                                                               
                                                                                
PRDNAMX  DS    0H                                                               
         MVC   KEY,SEQKEY          RE-ESTABLISH SEQ                             
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* ERROR MESSAGES                                                                
*******************************************************************             
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
                                                                                
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
                                                                                
SLOCKOUT MVC   RERROR,=AL2(55)                                                  
         B     ERREND                                                           
                                                                                
INVLPER  MVC   RERROR,=AL2(441)    MMMYY(-MMMYY)                                
         B     ERREND                                                           
                                                                                
INVLDAT  MVC   RERROR,=AL2(INVDAT)                                              
         B     ERREND                                                           
                                                                                
INVLDAT2 MVC   RERROR,=AL2(706)                                                 
         B     ERREND                                                           
                                                                                
SRCCLOSD MVC   RERROR,=AL2(658)                                                 
         B     ERREND                                                           
                                                                                
NEXTLIST MVC   RERROR,=AL2(15)     PRESS ENTER FOR NEXT                         
         B     INFEND                                                           
                                                                                
NEXTUPDT MVC   RERROR,=AL2(198)    UPDATED: PRESS ENTER FOR NEXT                
         B     INFEND                                                           
MAXSERCH MVC   RERROR,=AL2(171)    MAX IO:  ENTER TO CONTINUE                   
         B     INFEND                                                           
MAXDATA  MVC   RERROR,=AL2(172)    MAX IO:  DATE FOUND                          
         B     INFEND                                                           
                                                                                
                                                                                
SRCENOGD MVC   RERROR,=AL2(709)    SOURCE REP NOT FOUND ON FILE                 
         B     ERREND                                                           
STABARRD EQU   *                                                                
         CLC   =C'JNDT',DUB        JOIN DATE NOT A MONDAY ERROR?                
         BE    STAD0020                                                         
         MVC   RERROR,=AL2(714)    TARGET STA NOT CLEARED FOR XFER              
         B     ERREND                                                           
STAD0020 EQU   *                                                                
         MVC   RERROR,=AL2(730)    TARGET STA JOIN DATE NOT MONDAY              
         B     ERREND                                                           
STABARRS MVC   RERROR,=AL2(723)    SOURCE STA NOT CLEARED FOR XFER              
         B     ERREND                                                           
OLDBARRS MVC   RERROR,=AL2(823)    SOURCE REP BARS TAKEOVER                     
         B     ERREND                                                           
AGENCYNF MVC   RERROR,=AL2(633)    NO MATCHES TO FILTERS SUBMITTED              
         B     ERREND                                                           
INVSALPR EQU   *                                                                
         OC    DUB,DUB             OFF/TEAM ERROR MESSAGE?                      
         BZ    INVS0020            NO                                           
         MVC   RERROR,DUB+2        TEAM NOT ALLOWED TO SELL STATION             
         B     ERREND                                                           
INVS0020 EQU   *                                                                
         MVC   RERROR,=AL2(716)    SALESPERSON CODE NOT ON FILE                 
         B     ERREND                                                           
INVSPLEV EQU   *                                                                
         MVC   RERROR,=AL2(843)    SALESPERSON ERROR: LEAVE DATE                
         B     ERREND                                                           
INVFLD   EQU   *                                                                
         MVC   RERROR,=AL2(2)      FIELD INVALID AS ENTERED                     
         B     ERREND                                                           
INVCMPSP EQU   *                                                                
         MVC   RERROR,=AL2(923)    REP NOT USING COMP S/P                       
         B     ERREND                                                           
INVAGY   MVC   RERROR,=AL2(152)    AGENCY CODE NOT ON FILE                      
         B     ERREND                                                           
INVOFF   MVC   RERROR,=AL2(053)    OFFICE CODE NOT ON FILE                      
         B     ERREND                                                           
NGDAREQV MVC   RERROR,=AL2(950)    DARE AGY/OFF EQUIV DIFFERENCE                
         B     ERREND                                                           
                                                                                
NEEDOFFC MVC   RERROR,=AL2(094)    AGENCY OFFICE REQUIRED                       
         B     ERREND                                                           
                                                                                
INVADV   MVC   RERROR,=AL2(153)    ADVERT CODE NOT ON FILE                      
         B     ERREND                                                           
                                                                                
ENDLIST  MVC   RERROR,=AL2(16)     END OF LIST                                  
         B     INFEND                                                           
                                                                                
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
                                                                                
INFEND   DS    0H                                                               
         LA    R2,SPSSTAH          PUT CURSOR HERE                              
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
*                                                                               
         DS    50F                 PUSH GETEL INTO 2ND BASE REGISTER            
*          DATA SET RESFM34    AT LEVEL 158 AS OF 07/05/00                      
***********************************************************************         
* HHOOK - HEADHOOK ROUTINE                                                      
***********************************************************************         
HHOOK    NTR1                                                                   
         XIT1                                                                   
*                                                                               
***********************************************************************         
* HEADSPECS                                                                     
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,38,C'CONTRACT SWITCH LIST '                                   
         PSPEC H2,38,C'---------------------'                                   
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REQUESTOR                                                  
         PSPEC H5,02,C'CONTRACT'                                                
         PSPEC H5,11,C'AGENCY'                                                  
         PSPEC H5,19,C'ADV'                                                     
         PSPEC H5,24,C'S/P'                                                     
         PSPEC H5,29,C'STATION'                                                 
         PSPEC H5,48,C'AMOUNT'                                                  
         PSPEC H5,55,C'FLIGHT'                                                  
         PSPEC H6,55,C'ESTIMATE'                                                
         PSPEC H6,66,C'DARE#'                                                   
         PSPEC H7,02,C'--------'                                                
         PSPEC H7,11,C'------'                                                  
         PSPEC H7,19,C'----'                                                    
         PSPEC H7,24,C'---'                                                     
         PSPEC H7,29,C'-------'                                                 
         PSPEC H7,41,C'-------------'                                           
         PSPEC H7,55,C'-----------------'                                       
         DC    X'00'                                                            
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* LOCAL STORAGE AREA                                                            
*******************************************************************             
RELO     DS    A                                                                
SAVEAIO  DS    A                                                                
SAVEAIO2 DS    A                                                                
VSWITCH  DS    F                                                                
VSYSFAC  DC    V(SYSFAC)                                                        
VREPFACS DS    V                                                                
VBINSRCH DS    F                                                                
AEQUITBL DS    A                   A(EQUIVALENT TABLE)                          
AEQUNTRY DS    A                   A(EQUIVALENT TABLE ENTRY IN USE)             
ACMBOTBL DS    A                   A(COMBO PARTICIPANT TABLE)                   
ACMBNTRY DS    A                   A(COMBO PARTIC TABLE ENTRY IN USE)           
ANEWCMBO DS    A                   A(NEW PARTIC 17 ELT)                         
ACMBOSCR DS    A                   A(COMBOS ON SCREEN TABLE)                    
ACMBSCIT DS    A                   A(COMBOS ON SCREEN ITEM)                     
CMBODISP DS    F                   COMBO TABLE DISPLACEMENT                     
ACONTROL DS    A                   A(CONTROL INFORMATION)                       
AMISFLGS DS    A                   A(MISCELLANEOUS FLAGS)                       
ACONTRCT DS    A                   A(CONTRACT RECORD)                           
CONCTR   DS    F                                                                
TESTCTR  DS    F                                                                
TRGTREP  DS    CL3                 TARGET REP ID                                
NEXTSCRN DS    C                   Y=USER PRESSED ENTER/DISP NEXT SCRN          
UPDTDONE DS    CL1                                                              
SEQKEY   DS    CL(L'KEY)                                                        
ADVCODE  DS    CL(L'RCONKADV)                                                   
SAVCON#S DS    CL8                 SAVE AREA FOR OLD/NE CON #S                  
SAVGRPS  DS    CL4                 SAVE TARGET/SOURCE GROUPS                    
SAVSTYPS DS    CL2                 SAVE TARGET/SOURCE STATION TYPES             
CONTSTAT DS    X                                                                
BUCKFLGS DS    X                   BUCKETING FLAGS                              
CTLCOMBO DS    C                   N  =  NOT COMBO                              
*                                  Y  =  COMBO RECOGNIZED                       
*                                  OTHER  =  COUNT (NOT ZERO RELATIVE)          
CONSAVED DS    C                   Y  =  CONTRACT SAVED - NEEDS TO BE           
*                                        WRITTEN AFTER BUYS DONE                
*                                  N  =  CONTRACT NOT SAVED - OUTPUT            
*                                        AS FIRST RECORD                        
*                                                                               
COPYMGHD DS    C                   Y = COPY MAKEGOOD HEADER                     
*                                  N = DON'T COPY MAKEGOOD HEADER               
*                                  L = COPY FOR THE LAST TIME AND EXIT          
MGHEADCD DS    CL2                 MG HEADER GROUP CODE                         
MGSAVEKY DS    CL27                LAST MG KEY FOR SEQ                          
*                                                                               
CNFORVER DS    CL1                 0  =  CONFIRM                                
*                                  1  =  VERSION                                
TODAY    DS    CL3                 DATE OF RUN                                  
NEWCOMBO DS    CL38                NEW COMBO BUILD AREA:                        
*                                  FOUR STATIONS MAX                            
WORK2    DS    CL80                                                             
LISTAR2  DS    CL80                                                             
IOAREA   DS    CL128                                                            
CTYPTABL DS    CL64                CONTRACT TYPE TABLES                         
         DS    0F                                                               
*CONTABLE DS    2000C               500 CONTRACT NUMBER TABLE                   
         EJECT                                                                  
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM99D          (HEADER FILTER SCREEN OVERLAY)               
       ++INCLUDE RESFMWTWA                                                      
       ++INCLUDE RESFMWORKD                                                     
CONTRACT DSECT                                                                  
       ++INCLUDE REGENCON                                                       
ADV      DSECT                                                                  
       ++INCLUDE REGENADV                                                       
AGY      DSECT                                                                  
       ++INCLUDE REGENAGY                                                       
AGY2     DSECT                                                                  
       ++INCLUDE REGENAGY2                                                      
PRD      DSECT                                                                  
       ++INCLUDE REGENPRD                                                       
SALESMAN DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
TKO      DSECT                                                                  
       ++INCLUDE REGENTKO                                                       
STATION  DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
MKGRECD  DSECT                                                                  
       ++INCLUDE REGENMKG                                                       
CFCRECD  DSECT                                                                  
       ++INCLUDE REGENCFC                                                       
COVRECD  DSECT                                                                  
       ++INCLUDE REGENCOV                                                       
CONTYP   DSECT                                                                  
       ++INCLUDE REGENCTY                                                       
REPROPOS DSECT                                                                  
       ++INCLUDE REGENPRO                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SFMISFLG                                                       
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLCON                                                        
*                                                                               
*                                                                               
* WORK DSECT FOR CALLING REGENVER IN REPFACS                                    
*                                                                               
GVWORKD  DSECT                                                                  
GVDUB    DS    D                                                                
GVDUB2   DS    D                                                                
GVDMCB   DS    6F                                                               
GVWORKQ  EQU   *-GVWORKD                                                        
*                                                                               
*                                                                               
EQUITABL DSECT                                                                  
ETFLAG   DS    CL1                 FLAG BYTE FOR CONTRACT                       
*                                  X'80'  =  'SELECTED'                         
*                                  X'40'  =  ALREADY PROCESSED                  
*                                  X'20'  =  OUTPUT-SCANNED FOR CODES           
*                                  X'10'  =  PAPERWORK GENERATED                
*                                  X'01'  =  CONFIRMED ORDER                    
ETCDEFLG DS    CL4                 FLAG BYTES FOR CODES                         
*                                                                               
*        BYTE 1 VALUES:                                                         
*                                  X'80'  =  AGENCY CODE ON FILE                
AGONFILE EQU   X'80'                                                            
*                                  X'40'  =  ADD CODE RECORD                    
ADDAGY   EQU   X'40'                                                            
*                                  X'20'  =  ADVERT CODE ON FILE                
ADONFILE EQU   X'20'                                                            
*                                  X'10'  =  ADD CODE RECORD                    
ADDADV   EQU   X'10'                                                            
*                                  X'08'  =  S/P    CODE ON FILE                
SPONFILE EQU   X'08'                                                            
*                                  X'04'  =  ADD CODE RECORD                    
ADDSP    EQU   X'04'                                                            
*                                  X'02'  =  CONTYP CODE ON FILE                
CTONFILE EQU   X'02'                                                            
*                                  X'01'  =  ADD CODE RECORD                    
ADDCT    EQU   X'01'                                                            
*                                                                               
*        BYTE 2:  COMBO CONTRACTS IN SET (EXCLUDING BASE ORDER)                 
*                                                                               
*                                                                               
*        BYTE 3:  'DO NOT USE' FLAGS                                            
*                                  X'80' - AGENCY SET TO 'DO NOT USE'           
*                                  X'40' - ADVERT SET TO 'DO NOT USE'           
*                                                                               
ETOAGYOF DS    CL6                 ORIG AGY/OFF CODES                           
ETNAGYOF DS    CL6                 NEW  AGY/OFF CODES                           
ETOADVRT DS    CL4                 ORIG ADVERT  CODE                            
ETNADVRT DS    CL4                 NEW  ADVERT  CODE                            
ETNCATGY DS    CL2                 NEW  CATEGRY CODE                            
ETOSALEP DS    CL3                 ORIG S/P     CODE                            
ETNSALEP DS    CL3                 NEW  S/P     CODE                            
ETOCNTYP DS    CL1                 ORIG CONTYPE CODE                            
ETNCNTYP DS    CL1                 NEW  CONTYPE CODE                            
ETODVTYP DS    CL2                 ORIG DEVTYPE CODE                            
ETNDVTYP DS    CL2                 NEW  DEVTYPE CODE                            
ETODVSAL DS    CL3                 ORIG DEV S/P CODE                            
ETNDVSAL DS    CL3                 NEW  DEV S/P CODE                            
ETOCONUM DS    CL4                 ORIG CONTRACT NUM                            
ETNCONUM DS    CL4                 NEW  CONTRACT NUM                            
ETNSPOFF DS    CL2                 NEW SALESPERSON OFFICE                       
ETNSPTEM DS    CL2                 NEW SALESPERSON TEAM                         
ETDSKADR DS    CL4                 ORIG CONTRACT DISK ADDRESS                   
LEQUITBL EQU   *-ETFLAG            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE CTGENSTAD                                                      
         EJECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
*                                                                               
*                                                                               
DISLIN1  DSECT                                                                  
DL1CON#  DS    CL8                                                              
         DS    CL1                                                              
DL1AGY   DS    CL07                                                             
         DS    CL1                                                              
DL1ADV   DS    CL04                                                             
         DS    CL1                                                              
DL1SALEP DS    CL03                                                             
         DS    CL2                                                              
DL1STA   DS    CL07                                                             
         DS    CL2                                                              
DL1DOLRS DS    CL13                                                             
         DS    CL4                                                              
DL1FLITE DS    CL20                                                             
         DS    CL1                                                              
*                                                                               
DISLIN2  DSECT                                                                  
DL2COMBO DS    CL06                                                             
DL2STA1  DS    CL07                                                             
DL2SCON1 DS    CL08                                                             
         DS    CL01                                                             
DL2STA2  DS    CL07                                                             
DL2SCON2 DS    CL08                                                             
         DS    CL01                                                             
DL2STA3  DS    CL07                                                             
DL2SCON3 DS    CL08                                                             
         DS    CL01                                                             
DL2EST#  DS    CL10                                                             
         DS    CL01                                                             
DL2DARE  DS    CL08                                                             
*                                                                               
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
SAVEKEY  DS    CL(L'KEY)           NEED FOR PAGING                              
CON#KEY  DS    CL(L'KEY)           DURING SCREEN HANDLING                       
FIRSTKEY DS    CL(L'KEY)           FOR WHEN WE LOOP AROUND AGAIN                
HIGHKEY  DS    CL(L'KEY)           FOR WHEN WE LOOP AROUND AGAIN                
SCRNKEY  DS    CL(L'KEY)           FIRST KEY ON SCREEN                          
EQIVKEY  DS    CL(L'KEY)           EQUIV KEY RESTART                            
CHECKKEY DS    CL(L'KEY)           EXTRA KEY FOR AGENCY OFFICE CHECK            
KEYA2SAV DS    CL(L'KEY)           EXTRA KEY FOR A201 LOCKOUT  CHECK            
SETKEYA2 DS    CL(L'KEY)           EXTRA KEY FOR A201 LOCKOUT  CHECK            
MAXIOKEY DS    CL(L'KEY)           EXTRA KEY FOR MAX IO SITUATIONS              
LASTSCRN DS    C                                                                
TAKEALL  DS    CL1                 Y  =  S+ FOUND: TAKE ALL                     
CTKEYFLG DS    CL1                 Y  =  CONTYPE FILTER DONE                    
MAXIOFLG DS    CL1                                                              
*                                                                               
CTRLAREA DS    0F                  CONTROL AREA TO SAVE                         
EFFDATE  DS    CL6                 START DATE IN EBCDIC                         
ENDDT    DS    CL6                 END DATE IN EBCDIC                           
SCANDATE DS    XL2                 START DATE FOR 8E KEYS (COMPRESSED)          
MONDATE  DS    XL2                 COMPRESSED 'THIS WEEK' MONDAY                
BENDDT   DS    XL3                 END DATE OF END B'CAST MONTH                 
SAVSTDAT DS    XL3                 TARGET STATION START DATE                    
SRCECON  DS    CL4                 SOURCE CONTRACT NUMBER                       
TRGTCON  DS    CL4                 TARGET CONTRACT NUMBER                       
TARGREP  DS    CL2                 TARGET REP                                   
*                                                                               
*   SRCEGRP/TRGTGRP AND SRCETYPE/TRGTTYPE ARE SAVED AND RESTORED                
*        AS SINGLE FIELDS.  DO NOT INSERT OTHER FIELDS BETWEEN                  
*        THE PAIRS OF FIELDS                                                    
*                                                                               
SRCEGRP  DS    CL2                 SOURCE GROUP                                 
TRGTGRP  DS    CL2                 TARGET GROUP                                 
SRCETYPE DS    CL1                 SOURCE STATION TYPE:                         
*                                  O=OTHER                                      
*                                  G=GRAPHNET                                   
*                                  A=ACE                                        
TRGTTYPE DS    CL1                 TARGET STATION TYPE:                         
*                                  O=OTHER                                      
*                                  G=GRAPHNET                                   
*                                  A=ACE                                        
SKIPCLER DS    XL1                                                              
KEYTYPE  DS    XL1                 EITHER 8D OR 8E                              
SRCEREP  DS    CL2                 SOURCE REP                                   
TEMPTGRP DS    CL2                 TEMPORARY GROUP STORAGE: NEW                 
TEMPSGRP DS    CL2                 TEMPORARY GROUP STORAGE: OLD                 
TEMPTTYP DS    CL1                 TEMPORARY STATION TYPE STORAGE: NEW          
TEMPSTYP DS    CL1                 TEMPORARY STATION TYPE STORAGE: OLD          
*                                                                               
SRCESTAT DS    CL5                 SOURCE STATION                               
SRCESIGN DS    CL5                 SOURCE SIGNON                                
SRCENAME DS    CL20                SHORT NAME                                   
SRCEUTL  DS    CL1                 SOURCE UTL NUMBR                             
ORIGUTL  DS    CL1                 ORIGINAL UTL NUMBER                          
SAVESALP DS    CL3                 DEFAULT SALESPERSON CODE                     
SAVESPOF DS    CL2                 DEFAULT SALESPERSON OFFICE                   
SAVESPTM DS    CL2                 DEFAULT SALESPERSON TEAM                     
SAVECTYP DS    CL1                 DEFAULT CONTRACT TYPE                        
SAVELUID DS    CL8                 LUID MAKING THE REQUEST                      
ORIGOFFC DS    CL2                 ORIGINAL OFFICE OF ORDER                     
OFFFILT  DS    CL2                 SAVE AREA FOR OFFICE FILTER                  
AGYFILT  DS    CL6                 SAVE AREA FOR AGENCY FILTER                  
AGYNEW   DS    CL6                 SAVE AREA FOR NEW AGENCY                     
SPFILT   DS    CL3                 SAVE AREA FOR S/P    FILTER                  
SPNEW    DS    CL3                 SAVE AREA FOR NEW S/P                        
ADVFILT  DS    CL4                 SAVE AREA FOR ADVERT FILTER                  
DAREFLAG DS    CL1                 Y/N/B                                        
EFDTCOMP DS    CL2                 COMPRESSED CUTOFF DATE                       
BEFFDATE DS    XL3                 BINARY CUTOFF DATE                           
COMPSP   DS    CL1                 COMPENSATION S/P OPTION                      
SPSAVE   DS    CL5                                                              
OFFTEAMS DS    CL80                OFFICE TEAMS ALLOWED FOR STATION             
*                                  POS 1 - 2  =  OFFICE                         
*                                  POS 3 - 4  =  TEAM ALLOWED                   
LCTLAREA EQU   *-CTRLAREA          LENGTH OF CONTROL AREA                       
*                                                                               
ALTP     DS    CL132               ALTERNATE PRINT AREA                         
GLOBSV   DS    CL24                GLOBBER SAVE AREA                            
*                                                                               
NEWADDR  DS    F                   NEW CONTRACT DISK ADDRESS                    
SAVAGASS DS    F                   SAVE AREA: AGENCY ASSIGNMENT                 
SAVEQUIV DS    F                   SAVE AREA: EQUIV TBL IN PROCESS              
SVTOTSPT DS    F                   TOTAL SPOTS WORK AREA                        
SVTOTWKS DS    F                                                                
CONCOUNT DS    F                   CONTRACT COUNTER                             
IOCOUNT  DS    F                   I/O COUNTER                                  
MAXIOCTR DS    F                   I/O COUNTER                                  
DARAGYS  DS    CL20                AGENCY ASSIGNMENT SAVE AREA                  
DARAGYCD DS    CL4                 AGENCY CODE FROM AGENCY RECORD               
DARKTYP  DS    XL1                                                              
CONCTFLG DS    XL1                                                              
MGBITFLG DS    CL1                 X'80' = X'56' ELT DELETED FROM REC           
*                                  X'40' = X'05' ELT DELETED FROM REC           
*                                  X'20' = X'07' ELT DELETED FROM REC           
*                                                                               
BARFLAG  DS    CL1                 X'80' - STATION BARRED BY OLD REP            
MG05ELT  DS    CL1                                                              
WORK3    DS    CL64                                                             
OAGY2DAR DS    CL3       AN        DARE AGENCY EQUIVALENCY CODE                 
OAGY2DOF DS    CL2       AN        DARE AGENCY OFFICE EQUIVALENCY CODE          
OAGY2DR2 DS    CL3       AN        DARE AGENCY EQUIVALENCY CODE                 
OAGY2DF2 DS    CL2       AN        DARE AGENCY OFFICE EQUIVALENCY CODE          
OAGY2DR3 DS    CL3       AN        DARE AGENCY EQUIVALENCY CODE                 
OAGY2DF3 DS    CL2       AN        DARE AGENCY OFFICE EQUIVALENCY CODE          
OAGY2DR4 DS    CL3       AN        DARE AGENCY EQUIVALENCY CODE                 
OAGY2DF4 DS    CL2       AN        DARE AGENCY OFFICE EQUIVALENCY CODE          
LOAGY    EQU   *-OAGY2DAR                                                       
NAGY2DAR DS    CL3       AN        DARE AGENCY EQUIVALENCY CODE                 
NAGY2DOF DS    CL2       AN        DARE AGENCY OFFICE EQUIVALENCY CODE          
NAGY2DR2 DS    CL3       AN        DARE AGENCY EQUIVALENCY CODE                 
NAGY2DF2 DS    CL2       AN        DARE AGENCY OFFICE EQUIVALENCY CODE          
NAGY2DR3 DS    CL3       AN        DARE AGENCY EQUIVALENCY CODE                 
NAGY2DF3 DS    CL2       AN        DARE AGENCY OFFICE EQUIVALENCY CODE          
NAGY2DR4 DS    CL3       AN        DARE AGENCY EQUIVALENCY CODE                 
NAGY2DF4 DS    CL2       AN        DARE AGENCY OFFICE EQUIVALENCY CODE          
NOAGY    EQU   *-NAGY2DAR                                                       
*                                                                               
SAVEKEY2 DS    XL(L'KEY)                                                        
SAVEKEY3 DS    XL(L'KEY)                                                        
LSYSPARE EQU   *-SYSD                                                           
*                                                                               
         EJECT                                                                  
*   UPDTCON:  MODIFY CONTRACT RECORD ON TARGET SIDE.                            
*                                                                               
T81835   CSECT                                                                  
*                                                                               
*   CHEKADV:                                                                    
*                                                                               
*                                                                               
CHEKADV  NMOD1 0,*CADV*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R2,SPSADVH          SET A(OFFICE FILTER HEADER)                  
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'           SET KEY FOR AGENCY RECORD                    
         MVC   KEY+21(4),SPSADV    INSERT ADV CODE                              
         OC    KEY+21(4),SPACES    SET LOW-ORDER TO SPACES                      
         MVC   KEY+25(2),SRCEREP   INSERT REP CODE                              
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    CADV0160            YES - KEY VALID AS ENTERED                   
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     CADV0200                                                         
CADV0160 EQU   *                                                                
         MVC   ADVFILT,KEY+21      SAVE ADVERT ENTERED                          
         SR    R0,R0               SET CC = ZERO                                
CADV0200 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHEKOFF:                                                                    
*                                                                               
*                                                                               
CHEKOFF  NMOD1 0,*COFF*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R2,SPSOFFH          SET A(OFFICE FILTER HEADER)                  
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'           SET KEY FOR AGENCY RECORD                    
         MVC   KEY+23(2),SRCEREP   INSERT REP CODE                              
         MVC   KEY+25(2),SPSOFF    INSERT OFFICE CODE                           
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    COFF0160            YES - KEY VALID AS ENTERED                   
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     COFF0200                                                         
COFF0160 EQU   *                                                                
         MVC   OFFFILT,SPSOFF      SAVE OFFICE ENTERED                          
         SR    R0,R0               SET CC = ZERO                                
COFF0200 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VKEYNMOD NMOD1 0,**VKEY**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*                                                                               
*   RETRIEVE ORIGINAL SYSTEM NUMBER, TO SUPPRESS SYSTEM SWITCH                  
*        IF SOURCE AND TARGET REPS ARE ON SAME SYSTEM                           
*        DO THIS ONLY IN VKNM, SO IT ONLY GETS ACCOMPLISHED ONCE                
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   ORIGUTL,FASYS       SAVE ORIGINAL SYSTEM NUMBER                  
         MVC   SAVELUID,FASYM      GET LUID                                     
         DROP  RF                                                               
*                                                                               
         MVI   NEXTSCRN,C'N'       WE'RE NOT SCROLLING                          
*                                                                               
*                                  IF NO FIELDS WERE CHANGED,                   
*                                     USER WANTS TO PAGE TO NEXT                
*                                     SET OF CONTRACTS                          
         XC    CONCOUNT,CONCOUNT   RESET CONTRACT COUNTER                       
         MVI   CONCTFLG,C'N'       SET CONTRACT COUNT FLAG 'NO'                 
         CLC   =C'COUNT',SPSOPT    CONTRACT COUNT REQUEST?                      
         BNE   VKNM0020            NO                                           
*                                                                               
*   COUNT REQUESTED:  START AGAIN AT THE BEGINNING - REVALIDATE                 
*        EVERYTHING, AND START AT THE FIRST RECORD.  OPTION IS                  
*        CLEARED, AND A FLAG IS SET TO PERFORM THE FUNCTION                     
*                                                                               
         MVC   SPSOPT,SPACES       YES - CLEAR OPTION FIELD                     
         MVI   LASTSCRN,C'N'       TURN OFF SWITCH                              
         MVI   CONCTFLG,C'Y'       SET CONTRACT COUNT FLAG 'YES'                
         FOUT  SPSOPTH             SET FIELD TO RETRANSMIT EMPTY                
         B     VKNM0060                                                         
VKNM0020 EQU   *                                                                
         MVI   SKIPCLER,0          SET CLEAR SCREEN = NO                        
         TM    SPSAGIH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         TM    SPSADVH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         TM    SPSSTAH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         TM    SPSOFFH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         TM    SPSSALH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         TM    SPSDARH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         TM    SPSNAGH+4,X'20'                                                  
         BZ    VKNM0050                                                         
         TM    SPSNSPH+4,X'20'                                                  
         BZ    VKNM0050                                                         
         TM    SPSDATEH+4,X'20'                                                 
         BZ    VKNM0060                                                         
         TM    SPSCSPH+4,X'20'                                                  
         BZ    VKNM0060                                                         
*                                                                               
*                                  IF FIELDS WERE CHANGED, BUT TO               
*                                     THE SAME DATA, USER WANTS TO              
*                                     LIST FROM THE BEGINNING                   
         TM    SPSAGIH+4,X'80'                                                  
         BO    VKNM0060                                                         
         TM    SPSADVH+4,X'80'                                                  
         BO    VKNM0060                                                         
         TM    SPSSTAH+4,X'80'                                                  
         BO    VKNM0060                                                         
         TM    SPSOFFH+4,X'80'                                                  
         BO    VKNM0060                                                         
         TM    SPSSALH+4,X'80'                                                  
         BO    VKNM0060                                                         
         TM    SPSDARH+4,X'80'                                                  
         BO    VKNM0060                                                         
         TM    SPSNAGH+4,X'80'                                                  
         BO    VKNM0050                                                         
         TM    SPSNSPH+4,X'80'                                                  
         BO    VKNM0050                                                         
         TM    SPSDATEH+4,X'80'                                                 
         BO    VKNM0060                                                         
         TM    SPSCSPH+4,X'80'                                                  
         BO    VKNM0060                                                         
                                                                                
         L     RF,ACONTROL         SET A(CONTROL INFORMATION)                   
         MVC   CTRLAREA(LCTLAREA),0(RF)                                         
*                                  RESET CONTROL AREA FROM PRIOR RUN            
         MVI   NEXTSCRN,C'Y'                                                    
         B     VKNM0800                                                         
                                                                                
VKNM0050 DS    0H                                                               
         MVI   SKIPCLER,1          SET CLEAR SCREEN = YES                       
VKNM0060 DS    0H                                                               
         XC    FIRSTKEY,FIRSTKEY                                                
         OI    SPSAGIH+4,X'20'     SET VALIDATED                                
         OI    SPSADVH+4,X'20'     SET VALIDATED                                
         OI    SPSSTAH+4,X'20'     SET VALIDATED                                
         OI    SPSOFFH+4,X'20'     SET VALIDATED                                
         OI    SPSSALH+4,X'20'     SET VALIDATED                                
         OI    SPSDARH+4,X'20'     SET VALIDATED                                
         OI    SPSNAGH+4,X'20'     SET VALIDATED                                
         OI    SPSNSPH+4,X'20'     SET VALIDATED                                
         OI    SPSDATEH+4,X'20'    SET VALIDATED                                
         OI    SPSCSPH+4,X'20'     SET VALIDATED                                
         SR    R0,R0               SET CC  ZERO                                 
         B     VKNM0900                                                         
VKNM0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
VKNM0900 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
UPDTCON  NMOD1 0,*UPCN*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         L     R5,AIO                                                           
         USING RCONREC,R5                                                       
*                                  INSERT ORIGINAL CONTRACT NUMBER              
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
NWCOMELT DC    X'003C'     (2)     NEW COMMENT ELEMENT W/TAKEOVER               
         DC    X'FF'       (1)     FORCE TO SORT LAST                           
         DC    C'TKO '     (4)                                                  
         DC    C'EFF '     (4)                                                  
NWCOMDAT DS    CL8         (8)                                                  
         DC    C' OLD REP/CON '  (13)                                           
NWOLDREP DS    CL19              (19)                                           
         DC    C'#'              (1)                                            
NWOLDCON DS    CL8               (8)                                            
LNWCOMEL EQU   *-NWCOMELT                                                       
*                                                                               
         EJECT                                                                  
*                                                                               
****>>>>                                                                        
*   CHKCOMBO:  CHECK FOR PRESENCE OF X'17' ELEMENT IN CONTRACT REC.             
*        IF PRESENT, INSPECT EACH ENTRY.  IF STATION ON BOTH SOURCE             
*        AND TARGET ARE PREP'D FOR TAKEOVER, INSERT AN ENTRY INTO               
*        THE EQUITABL, WHICH WILL GENERATE A TAKEOVER ORDER.                    
*                                                                               
CHKCOMBO NMOD1 0,*CMBO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(EQUITABL)                            
         USING EQUITABL,R2                                                      
*                                                                               
         L     R5,8(R1)            RESET A(CONTRACT)                            
         USING RCONREC,R5                                                       
*                                                                               
         XC    CMBODISP,CMBODISP   CLEAR DISP INTO COMBO TABLE                  
         MVI   ELCODE,X'17'        FIND COMBO CONTRACT ELEMENT                  
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   CMBO0800            ELEMENT NOT FOUND                            
         ZIC   RF,1(R6)            DETERMINE NUMBER OF PARTICIPATING            
         BCTR  RF,0                SUBTRACT TWO FOR CONTROL                     
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         D     RE,=F'9'            DIVIDE BY LENGTH OF STA/CON#                 
         LR    R0,RF               SET LOOP CONTROL                             
*                                                                               
         LA    R6,2(R6)            BUMP TO 1ST COMBO ENTRY                      
*                                                                               
*   R4 WILL SERVE AS A POSITIONAL INDICATOR.  UP TO FOUR ORDERS MAY             
*        PARTICIPATE IN A COMBO ORDER.  REG4 WILL INITIALLY CONTAIN             
*        8 (X'00001000').  IF THE FIRST ORDER IS PARTICIPATING (AND NOT         
*        THE 'BASE' ORDER), THE REG WILL BE OR'D WITH THE FLAG BYTE.            
*        IN ALL CASES, THE BIT WILL BE SLID RIGHT ONE POSITION FOR              
*        EACH TEST.  AT THE END, IF THE FIRST CONTRACT PARTICIPATES,            
*        THE X'08' BIT WILL BE SET.  IF THE SECOND, THE X'04' BIT               
*        WILL BE SET, ETC.                                                      
*                                                                               
         LA    R4,8                SET PARTICIPATING FLAG                       
*                                                                               
*                                  FIRST TEST IS ALREADY ON SOURCE SIDE         
CCOM0020 EQU   *                                                                
         CLC   5(4,R6),ETOCONUM    IS THIS BASE CONTRACT?                       
         BE    CCOM0120                                                         
*                                                                               
CCOM0040 EQU   *                                                                
         ZIC   RF,ETCDEFLG+1       COMBO ORDER FOUND                            
*                                     UPDATE PARTICIPATING FLAGS                
         OR    RF,R4               MERGE FLAG INTO PREVIOUS                     
         STC   RF,ETCDEFLG+1       PUT COUNT BACK                               
*                                                                               
*                                                                               
*   EACH COMBO ENTRY CONSISTS OF UP TO THREE ORDERS.  EACH ORDER                
*      IS COMPOSED OF:                                                          
*        BYTES 0  -  4  = STATION CALL LETTERS                                  
*              5  -  8  = ORIGINAL CONTRACT NUMBER                              
*              9  - 12  = NEW      CONTRACT NUMBER                              
*             13  - 14  = OLD GROUP/SUBGROUP                                    
*             15  - 16  = NEW GROUP/SUBGROUP                                    
*             17  -     = OLD STATION TYPE                                      
*             18  -     = NEW STATION TYPE                                      
*   EQUATES FOR FIELDS                                                          
ACMBSTAT EQU   0                                                                
ACMBOCON EQU   5                                                                
ACMBNCON EQU   9                                                                
ACMBOGRP EQU   13                                                               
ACMBNGRP EQU   15                                                               
ACMBOSTY EQU   17                                                               
ACMBNSTY EQU   18                                                               
LCMBNTRY EQU   19                                                               
LCMBBUCK EQU   57                                                               
*                                                                               
         L     RF,ACMBNTRY         GET COMBO TABLE ENTRY IN USE                 
         A     RF,CMBODISP         ADD DISPLACEMENT TO NEXT SLOT                
         MVC   ACMBSTAT(5,RF),0(R6)   MOVE IN STATION LETTERS                   
         MVC   ACMBOCON(4,RF),5(R6)   MOVE ORIGINAL CON# TO SLOT                
         MVC   ACMBOGRP(2,RF),TEMPSGRP                                          
*                                  INSERT SOURCE GROUP INTO TABLE               
         MVC   ACMBNGRP(2,RF),TEMPTGRP                                          
*                                  INSERT TARGET GROUP INTO TABLE               
         MVC   ACMBOSTY(1,RF),TEMPSTYP                                          
*                                  INSERT SOURCE TYPE  INTO TABLE               
         MVC   ACMBNSTY(1,RF),TEMPTTYP                                          
*                                  INSERT TARGET TYPE  INTO TABLE               
         L     RF,CMBODISP         BUMP DISPLACEMENT TO NEXT SLOT               
         LA    RF,LCMBNTRY(RF)                                                  
         ST    RF,CMBODISP         PUT IT BACK                                  
CCOM0100 EQU   *                                                                
CCOM0120 EQU   *                                                                
         SRL   R4,1                MOVE BIT DOWN ONE POSITION                   
         LA    R6,9(R6)            BUMP TO NEXT PARTICIPATING STA               
         BCT   R0,CCOM0020         GO BACK FOR NEXT                             
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
CMBO0800 EQU   *                                                                
         L     RF,ACMBNTRY         BUMP COMBO TABLE UP                          
         LA    RF,LCMBBUCK(RF)     BUMP PAST THREE ENTRIES FOR ORDER            
         ST    RF,ACMBNTRY         PUT IT BACK                                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***>>>>                                                                         
*   GENERIC ADDREC FOR CONTRACT, AND CREATION OF PASSIVE POINTERS               
*        TO BE CALLED REGARDLESS AT WHICH STAGE CONTRACT IS OUTPUT              
*                                                                               
CONCREAT NMOD1 0,*CCRE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                  REREAD ORIGINAL ORDER BY KEY                 
*                                                                               
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO2            SET ALTERNATE IO AREA                        
*                                                                               
         OI    GENSTAT1,RDUPAPPL                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO  - WHY NOT?                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              RETRIEVE THE RECORD INTO ALTERNATE           
         MVC   AIO,SAVEAIO         RESTORE ORIGINAL IO AREA                     
*                                                                               
         L     R4,AIO              SET A(CONTRACT RECORD)                       
         USING RCONREC,R4                                                       
*                                                                               
         MVC   TEMPKEY,KEY         SAVE KEY PRIOR TO CHANGE                     
*                                                                               
         GOTO1 SETPTRS,DMCB,0      SET OLD PTRS                                 
*                                                                               
         MVC   ORIGOFFC,RCONKOFF   SAVE ORIGINAL OFFICE OF ORDER                
         OC    AGYNEW,AGYNEW       NEW AGENCY REQUESTED?                        
         BZ    CCRE0020            NO  - LEAVE OLD AGENCY/OFFICE                
*                                     AND DON'T DROP MAIN KEY                   
         MVC   RCONKAGY(6),AGYNEW  YES - RESET AGENCY/OFFICE                    
CCRE0020 EQU   *                                                                
         OC    SPNEW,SPNEW         NEW S/P    REQUESTED?                        
         BZ    CCRE0040            NO  - LEAVE OLD S/P                          
         MVC   RCONSAL,SPNEW       YES - RESET S/P                              
         MVC   RCONKOFF,SAVESPOF   RESET S/P OFFICE IN KEY                      
         MVC   RCONTEM,SAVESPTM    RESET S/P TEAM IN RECORD                     
*                                                                               
CCRE0040 EQU   *                                                                
         GOTO1 SETPTRS,DMCB,1      SET NEW PTRS                                 
*                                                                               
*   A CHANGE OF ONLY S/P DOES NOT CHANGE THE STATUS OF THE ORDER.               
*        ONLY AN AGENCY CHANGE DOES THAT.                                       
*                                                                               
         OC    AGYNEW,AGYNEW       NEW AGENCY REQUESTED?                        
         BZ    CCRE0080            NO  - STATUS OF ORDER REMAINS                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    CCRE0050                                                         
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREV.                      
CCRE0050 OI    RCONCONF,X'80'      NOT CONFIRMED                                
         SPACE 1                                                                
         ZIC   R3,1(R6)            GET NEXT ELEMENT                             
         AR    R6,R3                                                            
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BE    CCRE0055                                                         
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         SPACE 1                                                                
CCRE0055 EQU   *                                                                
         TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BZ    CCRE0080                                                         
         DROP  R6                                                               
*                                     THE SAME - NO CHANGES                     
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATE                                   
*                                                                               
***      MVC   WORK(4),HELLO                                                    
***      MVC   WORK+4(4),DATCON                                                 
***      LA    RE,WORK                                                          
***      ST    RE,DMCB+4                                                        
         XC    DMCB(24),DMCB                                                    
         GOTO1 DOGENVER,DMCB,(C'R',RCONREC)                                     
*                                  VERSION AND PASSIVE UPDATE                   
*                                                                               
*                                                                               
*   REREAD OF ORIGINAL ORDER NOW DONE AFTER REGENVER IS FINISHED,               
*        IN CASE DOGENVER PERFORMED ANY INTERVENING I/O                         
*                                                                               
***>>    GOTOX (RFGENVER,VREPFACS),DMCB,(C'R',RCONREC)                          
*                                                                               
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO2            SET ALTERNATE IO AREA                        
*                                                                               
         OI    GENSTAT1,RDUPAPPL                                                
         L     RF,AIO              RETRIEVE CONTRACT KEY FROM AIO2              
         MVC   KEY(27),0(RF)                                                    
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO  - WHY NOT?                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              RETRIEVE THE RECORD INTO ALTERNATE           
         MVC   AIO,SAVEAIO         RESTORE ORIGINAL IO AREA                     
*                                                                               
*&&DO                                                                           
*                                                                               
* CHANGED TO USE REGENVER IN REPFACS (SKUI 11/8/02)                             
*                                                                               
*        MVI   ELCODE,X'1F'        YES - FIND EXTENDED DESC ELEMENT             
*        LA    R6,RCONREC                                                       
*        BAS   RE,GETEL                                                         
*        BE    *+6                 FOUND                                        
*        DC    H'0'                MUST BE THERE                                
*        USING RCONXEL,R6                                                       
*        TM    RCONCONF,X'40'      ORDER CONFIRMED NOW?                         
*        BNO   CCRE0060            NO  - LEAVE AS IS                            
*        MVI   RCONCONF,X'A0'      YES - SET 'NOT CONFIRMED,                    
*                                     CF PREVIOUSLY'                            
         DROP  R6                                                               
*                                                                               
CCRE0060 EQU   *                                                                
         MVI   ELCODE,X'20'        FIND SEND INFO ELEMENT                       
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   CCRE0080            NO ELEMENT                                   
         USING RCONSEND,R6                                                      
         NI    RCONSENF,X'FF'-X'1F'     CLEAR TOP THREE BITS                    
         OI    RCONSENF,X'40'      SET 'LAST SENT BY STATION'                   
         ZIC   R5,RCONSRV          BUMP VERSION NUMBER                          
         LA    R5,2(R5)                                                         
         STC   R5,RCONSRV          REINSERT CURRENT VERSION NUMBER              
         GOTO1 DATCON,DMCB,(5,WORK),(2,WORK+32)                                 
         STC   R5,WORK+34          SAVE CURRENT VERSION NUMBER                  
         GOTO1 SET20ELT,DMCB,(R6)                                               
*                                                                               
         DROP  R6                                                               
*&&                                                                             
CCRE0080 EQU   *                                                                
         CLI   COMPSP,C' '         ANY COMP S/P WORK NEEDED?                    
         BNH   CCRE0160            NO  -                                        
         MVI   ELCODE,X'1E'        RANDOM FLAG ELEMENT                          
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   CCRE0160            NO ELEMENT                                   
         USING RCONRFEL,R6                                                      
         CLI   COMPSP,C'A'         COMP S/P -> NEW S/P?                         
         BNE   CCRE0100            NO                                           
         MVC   RCONRPSP,RCONSAL    INSERT NEW S/P                               
         MVC   RCONRSPO,RCONKOFF   INSERT NEW S/P OFFICE                        
         B     CCRE0160                                                         
CCRE0100 EQU   *                                                                
         MVI   RCONRPSP,C'$'       YES - BUILD HOUSE ACCOUNT                    
         MVC   RCONRPSP+1(2),ORIGOFFC    INSERT ORIGINAL S/P OFFICE             
*                                                                               
         B     CCRE0160                                                         
CCRE0160 EQU   *                                                                
         GOTO1 PUTREC              REWRITE UPDATED CONTRACT RECORD              
*                                                                               
*                                                                               
         L     R6,AIO3             OLD                                          
*                                                                               
         LA    R8,800(R6)          NEW                                          
*                                                                               
         MVC   TEMPKEY2,RCONREC                                                 
*                                                                               
         GOTO1 =A(ADDPTRS),DMCB,(RC),(R6),(R8),TEMPKEY+28,RR=Y                  
*                                                                               
         MVC   RCONREC(27),TEMPKEY2                                             
*                                  RESTORE CONTRACT X'0C' KEY                   
         GOTO1 =A(UPDPROS),DMCB,(RC),RR=Y                                       
*                                                                               
         GOTO1 =A(UPDMKGS),DMCB,(RC),RR=Y                                       
*                                  UPDATE MAKEGOOD RECORD PASSIVES              
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
TEMPKEY  DS    CL34                STORE KEY PRIOR TO CHANGE                    
TEMPKEY2 DS    CL27                STORE KEY AFTER CHANGE (NO D/A)              
         EJECT                                                                  
*                                                                               
*                                                                               
*   SET20ELT:  INSERT VALUES CONTAINED IN WORK+32->34 INTO THE                  
*        CONTRACT 20 ELT APPROPRIATELY.                                         
*                                                                               
SET20ELT NTR1                                                                   
         L     R6,0(R1)            SET A(20 ELT)                                
         USING RCONSEND,R6                                                      
*                                                                               
         LA    R2,RCONSRD1                                                      
         LA    R0,3                SET LOOP                                     
SETE0020 EQU   *                                                                
         OC    0(2,R2),0(R2)       SLOT EMPTY?                                  
         BZ    SETE0040            YES - INSERT DATA                            
         LA    R2,2(R2)            NO  - BUMP TO NEXT SLOT                      
         BCT   R0,SETE0020         GO BACK AND CHECK NEXT SLOT                  
         B     SETE0060            NO EMPTY SLOTS                               
SETE0040 EQU   *                                                                
         MVC   0(2,R2),WORK+32     INSERT NEW DATE INTO SLOT                    
         LA    R2,6(R2)            BUMP TO VERSION # SLOT                       
         MVC   0(1,R2),WORK+34     INSERT NEW VERSION #                         
         B     SETE0200            FINISHED - EXIT                              
SETE0060 EQU   *                                                                
         LA    R2,RCONSRD1                                                      
         MVC   2(4,R2),0(R2)       SLIDE LAST TWO BUCKETS LEFT                  
         MVC   4(2,R2),WORK+32     INSERT NEW DATE INTO LAST SLOT               
         LA    R2,6(R2)            BUMP TO VERSION # SLOT                       
         MVC   1(2,R2),0(R2)       SLIDE LAST TWO VERSIONS LEFT                 
         MVC   2(1,R2),WORK+34     INSERT NEW VERSION # INTO LAST SLOT          
SETE0200 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   SETPTRS:  GENERATES SET OF KEYS FOR OLD OR NEW CONTRACT                     
*        TO PERMIT CORRECT MAINTENANCE OF COMPLETE SET OF KEYS                  
*        P1  =  0:  OLD PTRS IN WORKSPACE FOR 800 BYTES                         
*        P1  =  1:  NEW PTRS IN WORKSPACE+800 FOR 800 BYTES                     
*                                                                               
SETPTRS  NTR1                                                                   
         L     R6,AIO3             OLD                                          
         LR    RE,R6                                                            
         OC    DMCB(4),DMCB        OLD OR NEW PTRS?                             
         BZ    SPTR0020            OLD - DON'T DISPLACE                         
         LA    RE,800(RE)          CLEAR FOLLOWING SPACE                        
         LA    R6,800(R6)                                                       
SPTR0020 EQU   *                                                                
         XCEF  (RE),800                                                         
*                                                                               
*   BUILD PASSIVE PTRS IN PROPER WORKSPACE                                      
*                                                                               
         GOTO1 =A(PTRS),DMCB,(RC),(R6),0,RR=Y                                   
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CALL REGENVER VIA REPFACS                                                     
*                                                                               
DOGENVER NTR1  BASE=*,WORK=(R5,GVWORKQ),LABEL=*                                 
         USING GVWORKD,R5                                                       
*                                                                               
         MVC   GVDMCB(24),0(R1)    PASS CALLER'S DMCB TO REGENVER               
*                                  IN REPFACS                                   
         MVC   GVDUB(4),HELLO                                                   
         MVC   GVDUB+4(4),DATCON                                                
         L     R2,DMCB+4                                                        
         CLC   GVDUB,0(R2)                                                      
         BE    DOGENV10                                                         
         LA    RE,GVDUB                                                         
         ST    RE,GVDMCB+4                                                      
DOGENV10 DS    0H                                                               
         XC    GVDUB2,GVDUB2       FOR REPFACS                                  
         MVC   GVDUB2(4),ACOMFACS                                               
         MVC   GVDUB2+4(2),TWAAGY                                               
*                                                                               
         PRINT GEN                                                              
         GOTOX (RFGENVRW,VREPFACS),DMCB,(RA),0,GVDMCB,GVDUB2                    
         BNZ   DGENNO                                                           
         PRINT NOGEN                                                            
*                                                                               
DGENYES  SR    RC,RC                                                            
DGENNO   LTR   RC,RC               SET CONDITION CODE AT EXIT                   
DGENX    XIT1  REGS=(R3)                                                        
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* UPDATE PROPOSAL X'02' SWITCH ELEMENTS AND THE PRIMARY STATION ELEMENT         
***********************************************************************         
UPDPROS  CSECT                                                                  
         NMOD1 0,*UPDPRO*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,AIO              IN CASE RECORD IS DELETED BUT THE            
         USING RCONREC,R5             PASSIVE POINTERS AREN'T                   
         MVC   SAVEAIO3,AIO        SAVE A(IO AREA W/CONTRACT)                   
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         USING RPROKEY,RE                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,TWAAGY                                                  
         ZAP   WORK+15(5),=P'0'                                                 
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         MVC   RPROKCON,WORK                                                    
         XC    RPROKPRO,RPROKPRO                                                
         DROP  RE                                                               
*                                                                               
UPDPRO2  GOTO1 HIGH                                                             
         CLC   KEY(RPROKPRO-RPROKMST),KEYSAVE                                   
         BNE   UPDPROX                                                          
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         MVI   ELCODE,RPRSWELQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   UPDPRO30            NO SWITCH ELEMENT TO CHANGE                  
*                                                                               
         MVC   WORK2,KEY                                                        
PK       USING RPROKEY,WORK2                                                    
*                                                                               
         LR    R4,R6                                                            
         USING RPRSWELD,R4                                                      
         CLC   RPRSWSAL,RCONSAL                                                 
         BNE   *+14                                                             
         CLC   RPRSWSTA,RCONKSTA                                                
         BE    UPDPRO9                                                          
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         USING RPROKEY,RE                                                       
         MVI   RPROPTYP,RPROPTYQ                                                
         MVI   RPROPSTY,RPROPSBQ                                                
         MVC   RPROPRCD,TWAAGY                                                  
         MVC   RPROPSAL,RPRSWSAL                                                
         MVC   RPROPSTA,RPRSWSTA                                                
         MVC   RPROPCON,PK.RPROKCON                                             
         MVC   RPROPPRO,PK.RPROKPRO                                             
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     UPDPRO4                                                          
*                                                                               
         OI    KEY+RPROKCTL-RPROKEY,X'80'       DELETED                         
         LTR   R0,R0                                                            
         L     RF,AIO3                                                          
         MVC   SAVE4301,0(RF)      SAVE EXISTING KEY                            
         EJECT                                                                  
         GOTO1 WRITE                                                            
         L     RF,AIO3                                                          
         MVC   0(27,RF),SAVE4301   RESTORE EXISTING KEY                         
         LTR   R0,R0                                                            
*                                                                               
DIE      EQU   *                                                                
UPDPRO4  LA    RE,KEY              NEW PASSIVE KEY                              
         USING RPROKEY,RE                                                       
         MVC   RPROPSAL,RCONSAL                                                 
         MVC   RPROPSTA,RCONKSTA                                                
         NI    RPROKCTL,X'FF'-X'80'                                             
         OC    RPROKDA,RPROKDA                                                  
         BNZ   *+10                                                             
         MVC   RPROKDA,WORK2+(RPROKDA-RPROKEY)                                  
         DROP  RE                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BNE   UPDPRO8                                                          
         NI    KEY+(RPROKCTL-RPROKEY),X'FF'-X'80'                               
         GOTO1 WRITE                                                            
         L     RF,AIO3                                                          
         MVC   0(27,RF),SAVE4301   RESTORE EXISTING KEY                         
         B     UPDPRO9                                                          
*                                                                               
UPDPRO8  MVC   KEY,KEYSAVE                                                      
         GOTO1 ADD                                                              
*                                                                               
UPDPRO9  DS    0H                                                               
         CLC   RPRSWSAL,RCONSAL                                                 
         BE    UPDPRO19                                                         
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         USING RPROKEY,RE                                                       
         MVI   RPROOTYP,RPROOTYQ                                                
         MVI   RPROOSTY,RPROOSBQ                                                
         MVC   RPROORCD,TWAAGY                                                  
         MVC   RPROOOFF,RPRSWOFF                                                
         MVC   RPROOSAL,RPRSWSAL                                                
         MVC   RPROOCON,PK.RPROKCON                                             
         MVC   RPROOPRO,PK.RPROKPRO                                             
         DROP  RE,PK                                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     UPDPR14                                                          
*                                                                               
         OI    KEY+RPROKCTL-RPROKEY,X'80'       DELETED                         
         GOTO1 WRITE                                                            
         L     RF,AIO3                                                          
         MVC   0(27,RF),SAVE4301   RESTORE EXISTING KEY                         
*                                                                               
UPDPR14  LA    RE,KEY              NEW PASSIVE KEY                              
         USING RPROKEY,RE                                                       
         MVC   RPROOOFF,RCONKOFF                                                
         MVC   RPROOSAL,RCONSAL                                                 
         NI    RPROKCTL,X'FF'-X'80'                                             
         OC    RPROKDA,RPROKDA                                                  
         BNZ   *+10                                                             
         MVC   RPROKDA,WORK2+(RPROKDA-RPROKEY)                                  
         DROP  RE                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BNE   UPDPR18                                                          
         NI    KEY+(RPROKCTL-RPROKEY),X'FF'-X'80'                               
         GOTO1 WRITE                                                            
         L     RF,AIO3                                                          
         MVC   0(27,RF),SAVE4301   RESTORE EXISTING KEY                         
         B     UPDPRO19                                                         
*                                                                               
UPDPR18  MVC   KEY,KEYSAVE                                                      
         GOTO1 ADD                                                              
*                                                                               
UPDPRO19 MVC   KEY,WORK2                                                        
*                                                                               
UPDPRO20 MVC   RPRSWSAL,RCONSAL                                                 
         MVC   RPRSWOFF,RCONKOFF                                                
         MVC   RPRSWTEM,RCONTEM                                                 
         MVC   RPRSWSTA,RCONKSTA                                                
         MVC   RPRSWADV,RCONKADV                                                
         MVC   RPRSWAGY,RCONKAGY                                                
         MVC   RPRSWAOF,RCONKAOF                                                
         MVC   RPRSWGRP,RCONKGRP                                                
         MVC   RPRSWFLT,RCONDATE                                                
         XC    RPRSWDSP,RPRSWDSP                                                
         XC    RPRSWDCT,RPRSWDCT                                                
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        DEVELOPMENTAL INVOICE ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   UPDPRO25            NOT FOUND                                    
         USING RCONDVEL,R6                                                      
         MVC   RPRSWDSP,RCONDVSP                                                
         MVC   RPRSWDCT,RCONDVCT                                                
         DROP  R4,R6                                                            
*                                                                               
UPDPRO25 GOTO1 PUTREC                                                           
*                                                                               
UPDPRO30 DS    0H                                                               
         LA    RE,KEY                                                           
         USING RPROKEY,RE                                                       
         ZIC   RF,RPROKPRO                                                      
         LA    RF,1(RF)                                                         
         STC   RF,RPROKPRO                                                      
         XC    RPROKMEL,RPROKMEL                                                
         DROP  RE                                                               
*                                                                               
*   TEST                                                                        
****     MVC   DIE(2),=X'0000'                                                  
*   END TEST                                                                    
*                                                                               
         B     UPDPRO2             PROCESS NEXT RECORD                          
*                                                                               
UPDPROX  DS    0H                                                               
         MVC   AIO,SAVEAIO3        RESTORE A(IO AREA W/CONTRACT)                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         DROP  R5                                                               
SAVE4301 DS    CL27                                                             
SAVEAIO3 DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
***>>>                                                                          
***********************************************************************         
* UPDATE MAKEGOOD RECORD SWITCH ELEMENTS AND PASSIVE KEYS             *         
***********************************************************************         
UPDMKGS  CSECT                                                                  
         NMOD1 0,*UPMG*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,AIO                                                           
         USING RCONREC,R5                                                       
*                                                                               
         MVC   SAVEAIO,AIO         SAVE CURRENT AIO AREA                        
         MVC   AIO,AIO3            SET NEW IO AREA                              
         L     R4,AIO              SET USING FOR MAKEGOOD REC                   
         USING RMKGREC,R4                                                       
*                                                                               
*   BUILD A MAKEGOOD SWITCH ELEMENT, WHICH ALSO CONTROLS PASSIVES               
*                                                                               
         XC    WORK3(64),WORK3     CLEAR ELEMENT BUILD AREA                     
         MVI   WORK3,X'0A'         INSERT ELEMENT CODE                          
         MVI   WORK3+1,RMKGXLNQ    INSERT ELEMENT LENGTH                        
WRK3     USING RMKGXEL,WORK3                                                    
         MVC   WRK3.RMKGXSAL,RCONSAL                                            
*                                  INSERT SALESPERSON CODE                      
         MVC   WRK3.RMKGXOFF,RCONKOFF                                           
*                                  INSERT OFFICE CODE                           
         MVC   WRK3.RMKGXTEM,RCONTEM                                            
*                                  INSERT TEAM CODE                             
         MVC   WRK3.RMKGXSTA,RCONKSTA                                           
*                                  INSERT STATION CODE                          
         MVC   WRK3.RMKGXADV,RCONKADV                                           
*                                  INSERT ADVERTISER CODE                       
         MVC   WRK3.RMKGXAGY,RCONKAGY                                           
*                                  INSERT AGENCY CODE                           
         MVC   WRK3.RMKGXAOF,RCONKAOF                                           
*                                  INSERT AGENCY OFFICE CODE                    
         MVC   WRK3.RMKGXGRP,RCONKGRP                                           
*                                  INSERT GROUP/SUBGROUP                        
         MVC   WRK3.RMKGXFLT,RCONDATE                                           
*                                  INSERT FLIGHT DATES                          
         LA    RF,RCONELEM         FIND DEVELOPMENT ELT                         
UPMG0020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    UPMG0060            YES - NO DEV ELT PRESENT                     
         CLI   0(RF),X'18'         DEVELOPMENT ELT?                             
         BE    UPMG0040            YES -                                        
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     UPMG0020            GO BACK FOR NEXT                             
UPMG0040 EQU   *                                                                
         MVC   WRK3.RMKGXDSP,RCONDVSP-RCONDVEL(RF)                              
*                                  INSERT DEV S/P                               
         MVC   WRK3.RMKGXDCT,RCONDVCT-RCONDVEL(RF)                              
*                                  INSERT DEV CON TYPE                          
UPMG0060 EQU   *                                                                
         DROP  WRK3                                                             
*                                  INSERT DEV CON TYPE                          
         XC    KEY,KEY                                                          
MKGKEY   USING RMKGKEY,KEY                                                      
*                                                                               
         MVI   MKGKEY.RMKGKTYP,X'11'      INSERT MAKEGOOD REC TYPE              
         MVC   MKGKEY.RMKGKREP,TWAAGY     INSERT REP CODE                       
         MVC   MKGKEY.RMKGKOFF,ORIGOFFC                                         
*                                  INSERT OFFICE CODE                           
         MVC   MKGKEY.RMKGKSTA,RCONKSTA                                         
*                                  INSERT STATION CALLS                         
         ZAP   WORK+15(5),=P'0'                                                 
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         PACK  WORK(1),WORK+18(1)   REVERSE THE COMPLIMENT                      
         PACK  WORK+1(1),WORK+17(1)                                             
         PACK  WORK+2(1),WORK+16(1)                                             
         PACK  WORK+3(1),WORK+15(1)                                             
         MVC   MKGKEY.RMKGKCON,WORK                                             
         DROP  MKGKEY                                                           
*                                                                               
UPMG0100 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     UPMG0140                                                         
UPMG0120 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT RECORD                             
UPMG0140 EQU   *                                                                
         CLC   KEY(RMKGKGR1-RMKGKEY),KEYSAVE                                    
*                                  MAKEGOOD KEY FOR CON# FOUND?                 
         BNE   UPMG0300            NO  - EXIT                                   
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         MVI   RDUPDATE,C'Y'       YES                                          
         GOTO1 GETREC,DMCB,RMKGREC                                              
*                                  RETRIEVE THE RECORD                          
         OC    KEY+RMKGKPLN-RMKGKEY(6),KEY+RMKGKPLN-RMKGKEY                     
*                                  YES - HEADER RECORD?                         
         BNZ   UPMG0260            NO  - PROCESS TRAILER                        
****>>                                                                          
         BAS   RE,MGRECUPD         UPDATE M/G REC AND PASSIVES                  
         B     UPMG0280                                                         
****>>                                                                          
UPMG0260 DS    0H                  TRAILER RECORD                               
         CLC   RMKGKOFF,RCONKOFF   SAME OFF?                                    
         BE    UPMG0280            NO NEED TO MODIFY TRAILER RECORD             
*                                                                               
         MVC   RMKGKOFF,RCONKOFF                                                
         GOTO1 PUTREC                                                           
         MVC   SAVEKEY3,RMKGKEY                                                 
*                                                                               
         MVI   KEY+27,X'FF'        DELETE OLD KEY                               
         GOTO1 WRITE                                                            
*                                                                               
         MVI   KEY+27,0                                                         
         MVC   KEY(27),SAVEKEY3    ADD NEW KEY WITH SAME D/A                    
         GOTO1 ADD                                                              
*                                                                               
UPMG0280 DS    0H                                                               
*                                                                               
*   NEED TO RESTART KEY, RETRIEVE ORIG RECORD                                   
*                                                                               
         MVC   KEY(27),SAVEKEY2    RESET KEY                                    
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                RETRIEVE M/G RECORD KEY                      
         B     UPMG0120            GO BACK FOR NEXT RECORD                      
*                                                                               
UPMG0300 DS    0H                                                               
         MVC   AIO,SAVEAIO         RESTORE A(CONTRACT RECORD)                   
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
MGRECUPD NTR1                                                                   
*--->>                                                                          
*   AT THIS POINT, DROP THE SWITCH ELT FROM THE RECORD, ADD THE                 
*        NEW SWITCH ELT FROM WORK3  AREA, PROCESS THE PASSIVE                   
*        KEYS, DELETING OLD, REACTIVATING WHERE NEEDED.                         
*                                                                               
*                                                                               
         BAS   RE,OLD0AELT         GET OLD 0A, SAVE PASSIVE INFO                
*                                     ALSO DROPS ELT FROM RECORD                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RMKGREC,WORK3,0                    
*                                  INSERT NEW OA ELEMENT                        
         MVC   KEYSAVE,KEY         SAVE M/G KEY                                 
         MVC   WORK(27),KEY        SAVE M/G KEY AGAIN                           
         MVC   FULL,KEY+28         SET D/A OF RECORD                            
*                                                                               
         MVC   RMKGKOFF,RCONKOFF                                                
         GOTO1 PUTREC,DMCB,RMKGREC REWRITE M/G W/NEW 0A ELT                     
         MVC   SAVEKEY3,RMKGKEY    SAVE OFF NEW KEY                             
*                                                                               
         CLC   KEY(27),SAVEKEY3                                                 
         BE    RECW10              KEY ALREADY THERE                            
*                                                                               
         MVI   KEY+27,X'FF'        DELETE OLD KEY                               
         GOTO1 WRITE                                                            
*                                                                               
         MVI   KEY+27,0                                                         
         MVC   KEY(27),SAVEKEY3    ADD NEW KEY WITH SAME D/A                    
         GOTO1 ADD                                                              
*                                  REWRITE M/G W/NEW 0A ELT                     
RECW10   BAS   RE,PASSREWR         REWRITE PASSIVE KEYS                         
         B     RECWX                                                            
RECWX    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PASSREWR:  RETRIEVE AND REWRITE PASSIVE KEYS                                
*                                                                               
PASSREWR NTR1                                                                   
*                                                                               
*   DELETE OLD PASSIVE POINTERS                                                 
*                                                                               
WKMGKEY  USING RMKGKEY,WORK                                                     
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'A0'           INSERT FIRST PASSIVE                         
         MVI   KEY+1,X'11'         INSERT ID FOR MG PASSIVE 1                   
         MVC   KEY+16(11),WKMGKEY.RMKGKSTA                                      
*                                  MOVE STA/CON#/GROUP TO NEW POSITIONS         
         MVC   KEY+9(2),WKMGKEY.RMKGKREP                                        
*                                  MOVE REP CODE TO NEW POSITION                
         DROP  WKMGKEY                                                          
*                                                                               
         MVC   KEY+11(5),SPSAVE    INSERT ORIGINAL S/P+TEAM INTO KEY            
PAWR0010 EQU   *                                                                
         MVI   RDUPDATE,C'Y'       SET FOR UPDATE                               
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0020            YES -                                        
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0040            PROCESS NEXT KEY                             
PAWR0020 EQU   *                                                                
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 WRITE               REWRITE KEY AS DELETED                       
         BAS   RE,CHECKPAS                                                      
PAWR0040 EQU   *                                                                
         CLI   KEY,X'A1'           SECOND KEY ALREADY DONE?                     
         BE    PAWR0060            YES                                          
         MVI   KEY,X'A1'           NO  - SET TO NEXT PASSIVE KEY                
         MVC   KEY+11(2),SPSAVE+3  INSERT TEAM INTO KEY HIGH                    
         MVC   KEY+13(3),SPSAVE    INSERT S/P INTO KEY LOW                      
         B     PAWR0010            GO BACK AND DO NEXT KEY                      
PAWR0060 EQU   *                                                                
**-->>                                                                          
WKMGKEY  USING RMKGKEY,WORK                                                     
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'A0'           INSERT FIRST PASSIVE                         
         MVI   KEY+1,X'11'         INSERT ID FOR MG PASSIVE 1                   
         MVC   KEY+16(11),WKMGKEY.RMKGKSTA                                      
*                                  MOVE STA/CON#/GROUP TO NEW POSITIONS         
         MVC   KEY+9(2),WKMGKEY.RMKGKREP                                        
*                                  MOVE REP CODE TO NEW POSITION                
         DROP  WKMGKEY                                                          
**-->>                                                                          
         LA    RF,RMKGELEM         SET A(DESCRIPTOR ELT OF M/G REC)             
PAWR0080 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    RECWX               YES - EXIT ROUTINE: NO PASSIVES              
         CLI   0(RF),X'0A'         SWITCH/PASSIVE ELT OF RECORD?                
         BE    PAWR0100            YES - PROCESS                                
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELT                       
         AR    RF,RE                                                            
         B     PAWR0080            GO BACK FOR NEXT                             
PAWR0100 EQU   *                                                                
         MVC   KEY+11(3),RMKGXSAL-RMKGXEL(RF)                                   
*                                  INSERT SALESPERSON CODE                      
         MVC   KEY+14(2),RMKGXTEM-RMKGXEL(RF)                                   
*                                  INSERT S/P TEAM    CODE                      
PAWR0120 EQU   *                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         MVI   RDUPDATE,C'Y'       SET FOR UPDATE                               
         GOTO1 HIGH                                                             
*                                                                               
         MVC   KEY+28(4),FULL      INSERT D/A                                   
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BNE   PAWR0140            NO                                           
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 WRITE               YES - WRITE THE KEY                          
         B     PAWR0160            GO PROCESS NEXT KEY (IF ANY)                 
PAWR0140 EQU   *                                                                
         MVC   KEY(27),KEYSAVE     RESTORE KEY FOR 27 CHARS                     
         GOTO1 ADD                 ADD NEW KEY                                  
PAWR0160 EQU   *                                                                
         CLI   KEY,X'A1'           SECOND KEY PROCESSED?                        
         BE    PAWR0180            YES - BOTH KEYS DONE                         
         MVI   KEY,X'A1'           NO  - SET SECOND KEY TYPE                    
         MVC   WORK(3),KEY+11      SAVE S/P  COMPONENT OF KEY                   
         MVC   KEY+11(2),KEY+14    SLIDE TEAM UP IN KEY                         
         MVC   KEY+13(3),WORK      INSERT S/P BACK INTO KEY                     
         B     PAWR0120            GO BACK AND PROCESS                          
PAWR0180 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
CHECKPAS EQU   *                                                                
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*   OLD0AELT:  FIND EXISTING 0A ELT, SAVE S/P, TEAM CODES FOR                   
*        DELETING OLD KEY                                                       
*                                                                               
OLD0AELT NTR1                                                                   
         XC    SPSAVE,SPSAVE       USE SPSAVE FOR INTERMEDIATE                  
         LA    R1,RMKGELEM                                                      
OLDA0020 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    OLDA0100            YES - FINISHED - NO ELT                      
         CLI   0(R1),X'0A'         0A ELT?                                      
         BE    OLDA0040            YES - PROCESS IT                             
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,RF                                                            
         B     OLDA0020            GO BACK FOR NEXT                             
OLDA0040 EQU   *                                                                
         USING RMKGXEL,R1                                                       
         MVC   SPSAVE(3),RMKGXSAL  SAVE S/P CODE                                
         MVC   SPSAVE+3(2),RMKGXTEM     SAVE TEAM CODE                          
*                                                                               
         DROP  R1                                                               
*                                                                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0A',RMKGREC),0,0                
*                                  DELETE OLD SWITCH ELEMENT                    
OLDA0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         DROP  R5                                                               
*--->>                                                                          
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
****>>>>                                                                        
*                                                                               
*   RETRIEVE AND TABLE OFFICE TEAMS ALLOWED FOR THIS STATION                    
*                                                                               
OFFCTEAM NMOD1 0,*OFTM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,OFFTEAMS         SET A(OFFICE/TEAM FIELD)                     
         XC    OFFTEAMS,OFFTEAMS   CLEAR THE FIELD                              
         L     R6,AIO                                                           
         USING RSTAOTEL,R6                                                      
         MVI   ELCODE,4            GET OFFICE/TEAM ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   OTEM0100                                                         
*                                                                               
OTEM0080 DS    0H                                                               
         MVC   0(2,R3),RSTAOTOF    INSERT OFFICE INTO TABLE                     
         MVC   2(2,R3),RSTAOTTM    INSERT TEAM   INTO TABLE                     
         OI    3(R3),X'40'         SET BINARY ZERO TO SPACE                     
         LA    R3,4(R3)            SET TO NEXT SLOT                             
         BAS   RE,NEXTEL           GO BACK FOR NEXT ELEMENT                     
         BE    OTEM0080                                                         
*                                                                               
OTEM0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SETCTYPS:  SOURCE FILE CONTRACT TYPE RECORDS ARE READ.  IF                  
*        RCTYFPRA/X'10' BIT IS SET, RECORDS OF THIS CONTRACT                    
*        TYPE ARE TO BE SKIPPED.  TABLE THIS TYPE.                              
*                                                                               
SETCTYPS NMOD1 0,*CTYP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    CTYPTABL,CTYPTABL   CLEAR TABLE                                  
         LA    R2,CTYPTABL         SET A(TABLE)                                 
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'32'           INSERT RECORD TYPE                           
         MVC   KEY+24(2),SRCEREP   INSERT SOURCE REP CODE                       
         GOTO1 HIGH                                                             
         B     SECT0040                                                         
SECT0020 EQU   *                                                                
         GOTO1 SEQ                                                              
SECT0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME RECORD TYPE/REP?                        
         BNE   SECT0160            NO  - FINISHED                               
         GOTO1 GETREC              RETRIEVE STATION RECORD                      
         L     R5,AIO                                                           
         USING RCTYREC,R5                                                       
*                                                                               
         CLI   RCTYFCDE,X'10'      NEW FORMAT RECORD?                           
         BNE   SECT0020            NO  - GO BACK FOR NEXT                       
         TM    RCTYFPRA,X'10'      'DON'T TAKE OVER CTYP' SET?                  
         BNO   SECT0020            NO  - GO BACK FOR NEXT                       
         MVC   0(1,R2),RCTYKCTY    YES - INSERT TYPE INTO TABLE                 
         LA    R2,1(R2)            BUMP TO NEXT ENTRY                           
         B     SECT0020            GO BACK FOR NEXT                             
SECT0160 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TAKEDARE:  READ ALL DARE FOR ORIGINAL ORDER, MOVE TO NEW FILE               
*        WITH NEW CONTRACT NUMBER.                                              
*                                                                               
* THIS ROUTINE WILL TAKEOVER X'41' RECORDS ONLY IF THERE ARE NO                 
* EXISTING X'41' RECORDS IN THE NEW REP'S INBOX. THE REASON IS IF THERE         
* IS A REVISED ORDER SITTING IN THE NEW REP'S INBOX, COPYING THE X'41'          
* RECORDS WILL BLOW AWAY ANY CHANGES THE AGENCY MAY HAVE MADE                   
*                                                                               
TAKEDARE NMOD1 0,*TKDR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(CONTRACT RECORD)                     
         MVC   SAVEQUIV,8(R1)      SAVE A(EQUIV TBL IN PROCESS)                 
*                                                                               
         USING RCONREC,R5                                                       
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         MVI   ELCODE,X'1D'        FIND DARE ELEMENT                            
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   TKDR0180            ELEMENT NOT FOUND: NOT DARE                  
*                                  FOUND:  COPY DARE RECORD(S)                  
         TM    RCONDRFG-RCONDREL(R6),X'04'                                      
*                                  KATZ EDI ORDER?                              
         BO    TKDR0180            YES - TREAT AS NOT DARE                      
*                                                                               
         L     R2,AIO2             SET A(IOAREA 2)                              
         USING RAGY2REC,R2         SET ADDRESSABILITY TO AGENCY RECORD          
*                                                                               
         XC    RAGY2REC(32),RAGY2REC                                            
         MVI   RAGK2TYP,RAGK2TYQ                                                
         L     RF,SAVEQUIV         SET A(EQUIVALENCY TBL ENTRY)                 
         MVC   RAGK2AGY(6),ETOAGYOF-EQUITABL(RF)                                
*                                  INSERT ORIGINAL AGENCY/OFFICE                
         MVC   RAGK2REP,SRCEREP    INSERT SOURCE REP INTO KEY                   
         MVC   KEY,RAGY2REC                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO2            SET IO AREA = IO2                            
         GOTO1 GETREC                                                           
*                                                                               
         MVC   DARAGYS,RAGY2DAR    PULL OUT FOUR (MAX) AGY ASSIGNS              
         MVC   DARAGYCD,RAGK2AGY   SAVE AGENCY CODE                             
*                                                                               
*   DARE TYPE X'41' RECORDS WILL BE PROCESSED ONLY FOR NEVER BEEN               
*     CONFIRMED CONTRACTS. ALL ELSE, X'51' RECORDS WILL BE TAKEN                
*        OVER.  THEREFORE, THE TYPE IS SET AT THE TOP OF THE LOOP.              
*                                                                               
         MVC   DARKTYP,=X'41'      SET DARE TYPE RECORD                         
*                                                                               
         MVI   ELCODE,X'1D'        FIND DARE ELEMENT                            
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                 ALREADY VERIFIED                             
         DC    H'0'                ILLOGICAL DUMP                               
         CLI   RCONDRRV-RCONDREL(R6),0                                          
*                                  REVISION NUMBER PRESENT?                     
         BNE   TKDR0010            YES - PROCESS 41 RECORDS                     
*                                                                               
         MVI   ELCODE,X'1F'        FIND DARE ELEMENT                            
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RCONCONF-RCONXEL(R6),X'40'+X'20'                                 
         BZ    TKDR0010                                                         
*                                                                               
         MVC   DARKTYP,=X'51'      SET DARE TYPE RECORD: FORCE 51S              
*                                                                               
TKDR0010 EQU   *                                                                
         MVC   AIO,SAVEAIO         RESTORE A(ORIGINAL IO AREA)                  
*                                                                               
         DROP  R2                                                               
*                                                                               
         LA    R4,KEY                                                           
         USING RDARKEY,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   RDARKTYP(1),DARKTYP                                              
         MVC   RDARKREP,SRCEREP    INSERT ORIGINAL REP INTO KEY                 
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   TKDR0015                                                         
         MVC   RDARKSTA+4(2),=C'T '                                             
TKDR0015 EQU   *                                                                
         OC    RDARKSTA(6),SPACES                                               
         MVC   RDARKAGY(5),DARAGYS                                              
*                                  LOAD THE 1ST EQUIVALENCY CODE                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RDARKORD,RCONDRLK   ORDER NUMBER                                 
         MVI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    RF,DARAGYS          THERE ARE MAX 4 AGENCY ASSIGNMENT            
*                                     1ST IS ALREADY LOADED                     
         LA    RF,5(RF)            SKIP THE 1ST CODE                            
         ST    RF,SAVAGASS         SAVE A(2ND AGENCY ASSIGNMENT)                
         LA    R0,3                COMBINATIONS WE NEED TO CHECK                
*                                                                               
TKDR0020 DS    0H                                                               
         GOTO1 HIGH                                                             
****     GOTO1 READ                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    TKDR0040                                                         
         MVC   KEY,KEYSAVE         RESET KEY NOT FOUND                          
         L     RF,SAVAGASS         RESTORE A(AGENCY ASSIGNMENT)                 
         MVC   RDARKAGY(5),0(RF)   INSERT NEXT EQUIVALENCY CODE                 
         LA    RF,5(RF)            BUMP TO NEXT EQUIV CODE                      
         ST    RF,SAVAGASS         SAVE A(AGENCY ASSIGNMENT)                    
*                                                                               
*                                                                               
         BCT   R0,TKDR0020                                                      
*                                                                               
         CLI   DARKTYP,X'51'       PROCESSING X'51' RECS?                       
         BE    TKDR0160            YES - MAY BE NO TYPE 51'S                    
*                                                                               
         CLC   =C'SZ',SRCEREP      SELTEL+AGENCY 1342 CHECK                     
         BNE   TKDR0150            NO  - GO CHECK TYPE X'51'S                   
         CLC   =C'1342  ',DARAGYCD YES - CHECK SPECIAL CODE                     
         BNE   TKDR0150            NO  - GO CHECK TYPE X'51'S                   
         MVC   RDARKAGY(5),=C'ED2DE'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    TKDR0040                                                         
         B     TKDR0150            NO  - GO CHECK TYPE X'51'S                   
*                                                                               
         DROP  R4                                                               
*                                                                               
TKDR0040 DS    0H                                                               
*                                                                               
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO2            SET IO AREA TO IO2                           
TKDR0050 DS    0H                                                               
         L     R6,AIO2                                                          
         GOTO1 GETREC,DMCB,(R6)                                                 
         USING RDARREC,R6                                                       
*                                                                               
TKDR0080 DS    0H                                                               
         MVC   RDARKREP,TARGREP    INSERT TARGET REP INTO KEY                   
         CLI   RDARKRT,X'10'       AGENCY HEADER RECORD?                        
         BNE   TKDR0100            NO  - DON'T INSERT NEW CON#                  
         BAS   RE,AGYORDEX         YES - CHECK: AGY ORDER ON NEW REP?           
         BNZ   TKDR0150            YES - DON'T BRING OVER                       
         MVC   RDARREP#,RCONKCON   NO  - INSERT NEW CONTRACT NUMBER             
TKDR0100 EQU   *                                                                
*                                                                               
         GOTO1 ADDREC,DMCB,RDARREC                                              
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         MVC   KEY(27),KEYSAVE     RESTART SOURCE FILE                          
         GOTO1 HIGH                REPOSITION TO LAST KEY ACCESSED              
*                                                                               
         GOTO1 SEQ                 ACCESS NEXT KEY                              
         CLC   KEY(24),KEYSAVE     SAME SET OF DARE RECORDS?                    
         BNE   TKDR0150            FINISHED: CHECK TYPE DARE                    
         MVC   KEYSAVE(27),KEY     SAVE KEY                                     
         B     TKDR0050            GO BACK FOR NEXT DARE                        
TKDR0150 EQU   *                                                                
         CLI   DARKTYP,X'41'       TYPE 41S IN PROGRESS?                        
         BNE   TKDR0160            NO  - ALL FINISHED                           
         MVI   DARKTYP,X'51'       YES - NOW DO TYPE 51S                        
         B     TKDR0010            GO BACK AND DO 51S                           
*                                                                               
TKDR0160 EQU   *                                                                
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
TKDR0180 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AGYORDEX:  CHECK IF AGENCY ORDER EXISTS ALREADY ON THE NEW                  
*        REP'S FILE.  IF IT DOES, INSERT THE NEW CONTRACT NUMBER                
*        AND REWRITE THE RECORD.  THEN SKIP THE RECORDS ON THE                  
*        OLD SIDE.                                                              
*                                                                               
AGYORDEX NTR1                                                                   
         MVC   ELEM+200(27),KEYSAVE                                             
*                                  SAVE KEYSAVE FOR RESTART                     
         MVC   KEY(27),RDARKEY     DARE RECORD ALREADY ON FILE?                 
         MVI   KEY,X'41'           CHECK UNCONFIRMED DARE ORDER                 
         OI    DMINBTS,X'88'       READ DELETED                                 
         OI    GENSTAT1,RDUPAPPL                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         CLC   KEY(27),KEYSAVE     KEY FOUND ON NEW FILE?                       
         BNE   AORD0100            NO  - USE NEW FILE                           
         XC    0(64,R6),0(R6)      CLEAR IO AREA AS INDICATOR                   
         OI    DMINBTS,X'88'       READ DELETED                                 
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC,DMCB,(R6)    YES - GET NEW FILE RECORD                    
         NI    DMINBTS,X'FF'-X'08'                                              
         TM    RDARCNTL,X'80'      RECORD DELETED?                              
         BO    AORD0080            YES - DON'T DO ANYTHING:  SKIP               
*                                     THIS ORDER (ILLOGICAL)                    
         MVC   RDARREP#,RCONKCON   NO  - INSERT NEW CONTRACT NUMBER             
*        MVC   SAVEAIO,AIO         SAVE CURRENT A(IO AREA)                      
         ST    R6,AIO              SET A(RECORD TO BE REWRITTEN)                
         GOTO1 PUTREC              REWRITE THE RECORD WITH                      
*                                     NEW CONTRACT NUMBER                       
*        MVC   AIO,SAVEAIO         RESTORE IO AREA                              
AORD0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     AORD0120            EXIT                                         
AORD0100 EQU   *                                                                
         MVC   KEYSAVE(27),ELEM+200                                             
*                                  RESET ORIG SRCE KEY: DON'T READ!             
         SR    R0,R0               SET CC ZERO FOR RETURN                       
AORD0120 EQU   *                                                                
         XIT1                                                                   
         DROP  R5,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*********************************************************************           
*  VALSCRN:  VALIDATES NEW SCREEN FIELDS, ADDS INTO EQUITABL,       *           
*        PASSES BACK ERROR ADDR/MESSAGE #, IF ENCOUNTERED, FOR      *           
*        EACH LINE SELECTED.  PROPAGATES CODES AS NECESSARY         *           
*     THIS ROUTINE IS A HOLDOVER FROM THE TAKEOVER PROGRAM, FROM    *           
*        WHICH THIS PROGRAM HAS BEEN BUILT.  THE TABLE IS AN        *           
*        INTEGRAL PART OF THE PROGRAM FLOW, ALTHOUGH IT NOW HAS NO  *           
*        EQUIVALENCE CODES STORED IN IT.  THE NEW CODES ARE         *           
*        INSERTED INTO THE RECORD FROM THE SCREEN'S HEADER FIELDS,  *           
*        RATHER FROM THE INDIVIDUAL ORDER LINES                     *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
VALSCRN  NMOD1 0,*VSCR*                                                         
         L     RC,0(R1)                                                         
         L     R2,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
         USING EQUITABL,R2                                                      
*                                                                               
         LA    R3,SPSSELH          SET A(1ST SCREEN SELECT FIELD)               
VASC0020 EQU   *                                                                
         CLI   8(R3),C'S'          CONTRACT SELECTED?                           
         BE    VASC0040            YES - PROCESS FURTHER                        
         CLI   TAKEALL,C'Y'        ALL CONTRACTS SELECTED?                      
         BE    VASC0040            YES - PROCESS FURTHER                        
*                                  NO  - BUMP TO NEXT EQUIV ENTRY               
         NI    ETFLAG,X'FF'-X'80'  TURN OFF 'SELECTED' FLAG                     
VASC0030 EQU   *                                                                
         LA    R2,LEQUITBL(R2)     BUMP TO NEXT EQUIV ENTRY                     
         LA    R3,SPSSEL1H-SPSSELH(R3)                                          
*                                  BUMP TO NEXT SELECT FIELD                    
         LA    RF,SPSTAGH          CHECK FOR END OF SCREEN                      
         CR    R3,RF               END OF SCREEN REACHED?                       
         BL    VASC0020            NO  - GO BACK AND CHECK NEXT                 
         B     VASC0800            YES - FINISHED                               
VASC0040 EQU   *                                                                
         OC    0(LEQUITBL,R2),0(R2)                                             
*                                  ANY ENTRY IN SLOT?                           
         BZ    VASC0030            NO  - SKIP OVER IT                           
         TM    ETFLAG,X'40'        FIELD ALREADY PROCESSED?                     
         BNO   VASC0050            NO                                           
         CLI   TAKEALL,C'Y'        YES - 'PROCESS ALL (S+)' REQUEST?            
         BE    VASC0030            YES - SKIP OVER ALREADY DONE                 
         ST    R3,DUB              SET A(FIELD IN ERROR)                        
         MVC   RERROR,=AL2(717)    FIELD ALREADY PROCESSED                      
         B     VASC0780                                                         
VASC0050 EQU   *                                                                
         OI    ETFLAG,X'80'        TURN ON 'SELECTED' FLAG                      
         B     VASC0030            NO ERROR - DO NEXT LINE                      
**                                                                              
VASC0780 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     VASC0820            EXIT WITH ERROR                              
VASC0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
VASC0820 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AGYCHECK:  VALIDATE THE NEW AGENCY FIELD.  IF NOT PRESENT,                  
*        ATTEMPT TO FILL IT FROM OTHER INFORMATION ON THE SCREEN.               
*        R2 ->  A(EQUIVALENCY TABLE ENTRY FOR CONTRACT)                         
*        R3 ->  AGENCY CODE FIELD IN QUESTION                                   
*                                                                               
AGYCHECK NTR1                                                                   
*                                                                               
         TM    4(R3),X'20'         FIELD PREVIOUSLY VALID?                      
         BNO   AGYC0010            NO  - CHANGED THIS PASS                      
         TM    ETCDEFLG+2,X'80'    YES - 'DO NOT USE' SET?                      
         BO    AGYC0770            YES - ERROR                                  
AGYC0010 EQU   *                                                                
         LR    R5,R3               SET A(AGENCY FIELD HEADER)                   
         CLI   8(R5),C'='          REQUEST FOR BROWSER CALL?                    
         BNE   AGYC0020            NO                                           
**       MVC   DMCB+20(1),SRCEUTL                                               
**       MVC   DMCB+21(1),ORIGUTL                                               
**       MVC   DMCB+22(2),SRCEREP                                               
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,BASERD,(R5),0,        +        
               (X'40',C' AGY'),0                                                
**       GOTO1 =V(REBROWSE),DMCB,ACOMFACS,BASERD,(R5),0,(0,C' AGY'),0,+         
**             RR=Y                                                             
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
AGYC0020 EQU   *                                                                
*                                                                               
*   AGENCY CODE CONSIDERED ALWAYS ENTERED NEW.  AGENCY-OFFICE CHECK WAS         
*        NOT BEING DONE WHEN VALUE OF TKO RECORD WAS INSERTED.                  
*                                                                               
***      TM    4(R3),X'20'         FIELD PREVIOUSLY VALID?                      
***      BO    AGYC0800            YES - EXIT CC ZERO                           
         CLI   5(R3),0             ANYTHING ON LINE?                            
         BNE   AGYC0200            YES                                          
         L     R4,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
AGYC0040 EQU   *                                                                
         CLC   ETOAGYOF,ETOAGYOF-EQUITABL(R4)                                   
*                                  ORIGINAL LINE AGY/OFC = TBL ENTRY?           
         BNE   AGYC0050            NO  - BUMP TO NEXT ENTRY                     
         OC    ETNAGYOF-EQUITABL(6,R4),ETNAGYOF-EQUITABL(R4)                    
*                                  YES - NEW CODE FOR TABLE ENTRY?              
         BZ    AGYC0050            NO  - CAN'T USE IT                           
         CR    R4,R2               YES - TABLE ENTRY ALREADY PROCESSED?         
         BNL   AGYC0720            NO  - CONSIDER ITEM NOT ENTERED              
         TM    ETFLAG,X'80'        YES - TABLE ENTRY SELECTED?                  
         BNE   AGYC0060            NO  -                                        
         MVC   ETNAGYOF,ETNAGYOF-EQUITABL(R4)                                   
*                                  YES - ALREADY VALIDATED -                    
*                                     USE ITS CODE FOR THIS LINE                
         MVC   8(6,R3),ETNAGYOF    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     AGYC0800            EXIT CC ZERO                                 
AGYC0050 EQU   *                                                                
         LA    R4,LEQUITBL(R4)     BUMP TO NEXT TABLE ENTRY                     
         OC    0(12,R4),0(R4)      ANY ENTRY?                                   
         BZ    AGYC0720            NO  - TREAT AS 'NO FIND'                     
         B     AGYC0040            YES - GO BACK AND CHECK IT                   
AGYC0060 EQU   *                                                                
*                                  TABLE ENTRY NOT SELECTED:                    
*                                     CODE MUST HAVE BEEN ON FILE -             
*    NOTE:  SCREEN FIELD MAY HAVE BEEN CHANGED ON UNSELECTED LINE               
*        HOWEVER, ON-FILE VALUE OF CODE IS IN TABLE, AND                        
*        WILL BE USED.                                                          
*                                                                               
         TM    ETCDEFLG,AGONFILE   AGENCY CODE ON FILE?                         
         BO    AGYC0780            NO  - EXIT CC NOT ZERO                       
         MVC   ETNAGYOF,ETNAGYOF-EQUITABL(R4)                                   
*                                  YES - ON-FILE CODE FOR AGY/OFF-              
*                                     USE ITS CODE FOR THIS LINE                
         MVC   8(6,R3),ETNAGYOF    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     AGYC0800            EXIT CC ZERO                                 
AGYC0200 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'           SET KEY FOR AGENCY RECORD                    
         MVC   KEY+19(6),SPACES    SPACE FILL AGY PORTION                       
         LR    RF,R3               SET A(FIELD IN PROCESS)                      
         LA    RF,8(RF)            SET TO DATA IN FIELD                         
         ZIC   R0,5(R3)            SET COUNT FOR LOOP - USE FIELD LEN           
         LA    RE,KEY+19           SET A(KEY FIELD)                             
AGYC0220 EQU   *                                                                
         CLI   0(RF),C'-'          SEPARATOR ENCOUNTERED?                       
         BNE   AGYC0240            NO  - MOVE CHARACTER                         
         LA    RF,1(RF)            YES - SKIP SEPARATOR                         
         LA    RE,KEY+23           NEXT CHARS MUST BE OFFICE                    
*                                                                               
*    NOTE:  FIELD MAY RUN OVER ACTUAL KEY.  THIS SHOULDN'T BE                   
*        A PROBLEM, AND IT SHOULD BE CLEANED UP WITHIN KEY BUILDING             
*                                                                               
AGYC0240 EQU   *                                                                
         MVC   0(1,RE),0(RF)       MOVE CHARACTER TO KEY                        
         LA    RF,1(RF)            BUMP TO NEXT POSITION                        
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         BCT   R0,AGYC0220         GO BACK FOR NEXT CHARACTER                   
*                                  ALL CHARS PROCESSED                          
         MVC   KEY+25(2),TWAAGY    INSERT TARGET REP CODE                       
         MVC   CHECKKEY,KEY        SAVE EXTRA KEY COPY                          
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   AGYC0740            NO  - KEY INVALID AS ENTERED                 
         CLC   CHECKKEY+23(2),SPACES                                            
*                                  KEY CONTAIN OFFICE?                          
         BNE   AGYC0280            YES - CAN USE KEY AS ENTERED                 
*                                  NO  - DOES IT NEED OFFICE?                   
         MVC   ETNAGYOF,KEY+19     INSERT CODE INTO TABLE AS RECEIVED           
AGYC0260 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT KEY                                
         CLC   KEY(23),KEYSAVE     SAME RECORD TYPE/AGY CODE?                   
         BNE   AGYC0300            NO  - DOESN'T NEED OFFICE -                  
*                                     ALREADY IN TABLE                          
         CLC   KEY+25(2),TWAAGY    YES - SAME REP?                              
         BNE   AGYC0260            NO  - CHECK NEXT AGENCY RECORD               
         B     AGYC0760            YES - NEEDS AGENCY/OFFICE                    
AGYC0280 EQU   *                                                                
         MVC   ETNAGYOF,KEY+19     INSERT CODE INTO TABLE                       
AGYC0300 EQU   *                                                                
         MVC   KEY,CHECKKEY        RESTORE EXTRA KEY COPY                       
         GOTO1 HIGH                REREAD FOR REQUESTED KEY                     
         MVC   SAVEAIO,AIO         SAVE A(IO AREA)                              
         MVC   AIO,AIO2            RESET A(IO AREA) TO IOAREA2                  
         L     R6,AIO2                                                          
         USING RAGYREC,R6                                                       
                                                                                
         GOTO1 GETREC,DMCB,RAGYREC                                              
                                                                                
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
         TM    RAGYFLAG,X'02'      'DO NOT USE' SET?                            
         BO    AGYC0770            YES - RETURN ERROR                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         TM    ETCDEFLG,AGONFILE   CODE ALREADY ON FILE?                        
         BO    AGYC0800            YES - DON'T SET 'ADD IT'                     
         OI    ETCDEFLG,ADDAGY     NO  - SET 'ADD IT'                           
         B     AGYC0800                                                         
AGYC0720 EQU   *                                                                
         MVC   RERROR,=AL2(001)    FIELD REQUIRED/MISSING                       
         B     AGYC0780                                                         
AGYC0740 EQU   *                                                                
         MVC   RERROR,=AL2(002)    FIELD INVALID AS ENTERED                     
         B     AGYC0780                                                         
AGYC0760 EQU   *                                                                
         MVC   RERROR,=AL2(715)    FIELD REQUIRES AGENCY OFFICE                 
         B     AGYC0780                                                         
AGYC0770 EQU   *                                                                
         MVC   RERROR,=AL2(906)    'DO NOT USE' IS SET                          
         B     AGYC0780                                                         
AGYC0780 EQU   *                                                                
         ST    R3,DUB              SET A(ERROR FIELD)                           
         LTR   RB,RB               SET CC NOT ZERO                              
         B     AGYC0820            EXIT WITH ERROR                              
AGYC0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
AGYC0820 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ADVCHECK:  VALIDATE THE NEW ADVERT FIELD.  IF NOT PRESENT,                  
*        ATTEMPT TO FILL IT FROM OTHER INFORMATION ON THE SCREEN.               
*        R2 ->  A(EQUIVALENCY TABLE ENTRY FOR CONTRACT)                         
*        R3 ->  ADVERT CODE FIELD IN QUESTION                                   
*                                                                               
ADVCHECK NTR1                                                                   
*                                                                               
         TM    4(R3),X'20'         FIELD PREVIOUSLY VALID?                      
         BNO   ADVC0010            NO  - CHANGED THIS PASS                      
         TM    ETCDEFLG+2,X'40'    YES - 'DO NOT USE' SET?                      
         BO    ADVC0770            YES - ERROR                                  
ADVC0010 EQU   *                                                                
         LR    R5,R3               SET A(AGENCY FIELD HEADER)                   
         CLI   8(R5),C'='          REQUEST FOR BROWSER CALL?                    
         BNE   ADVC0020            NO                                           
**       MVC   DMCB+20(1),SRCEUTL                                               
**       MVC   DMCB+21(1),ORIGUTL                                               
**       MVC   DMCB+22(2),SRCEREP                                               
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,BASERD,(R5),0,        +        
               (0,C' ADV'),0                                                    
**       GOTO1 =V(REBROWSE),DMCB,ACOMFACS,BASERD,(R5),0,(0,C' ADV'),0,+         
**             RR=Y                                                             
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
ADVC0020 EQU   *                                                                
                                                                                
*                                                                               
*   PREVIOUSLY-VALID TEST IS REMOVED TO PERMIT LOADING OF CATEGORY              
*        CODE FOR ADVERTISER IN USE.  THIS MUST BE DONE EACH TIME               
*        FOR EVERY FIELD.                                                       
*                                                                               
***      TM    4(R3),X'20'         FIELD PREVIOUSLY VALID?                      
***      BO    ADVC0800            YES - EXIT CC ZERO                           
         CLI   5(R3),0             ANYTHING ON LINE?                            
         BNE   ADVC0200            YES                                          
         L     R4,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
ADVC0040 EQU   *                                                                
         CLC   ETOADVRT,ETOADVRT-EQUITABL(R4)                                   
*                                  ORIGINAL LINE ADVERT = TBL ENTRY?            
         BNE   ADVC0050            NO  - BUMP TO NEXT ENTRY                     
         OC    ETNADVRT-EQUITABL(4,R4),ETNADVRT-EQUITABL(R4)                    
*                                  YES - NEW CODE FOR TABLE ENTRY?              
         BZ    ADVC0050            NO  - CAN'T USE IT                           
         CR    R4,R2               YES - TABLE ENTRY ALREADY PROCESSED?         
         BNL   ADVC0720            NO  - CONSIDER ITEM NOT ENTERED              
         TM    ETFLAG,X'80'        YES - TABLE ENTRY SELECTED?                  
         BNE   ADVC0060            NO  -                                        
         MVC   ETNADVRT,ETNADVRT-EQUITABL(R4)                                   
*                                  YES - ALREADY VALIDATED -                    
*                                     USE ITS CODE FOR THIS LINE                
         MVC   ETNCATGY,ETNCATGY-EQUITABL(R4)                                   
*                                  LOAD CATEGORY FOR ADVERTISER                 
         MVC   8(4,R3),ETNADVRT    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     ADVC0800            EXIT CC ZERO                                 
ADVC0050 EQU   *                                                                
         LA    R4,LEQUITBL(R4)     BUMP TO NEXT TABLE ENTRY                     
         OC    0(12,R4),0(R4)      ANY ENTRY?                                   
         BZ    ADVC0720            NO  - TREAT AS 'NO FIND'                     
         B     ADVC0040            YES - GO BACK AND CHECK IT                   
ADVC0060 EQU   *                                                                
*                                  TABLE ENTRY NOT SELECTED:                    
*                                     CODE MUST HAVE BEEN ON FILE -             
*    NOTE:  SCREEN FIELD MAY HAVE BEEN CHANGED ON UNSELECTED LINE               
*        HOWEVER, ON-FILE VALUE OF CODE IS IN TABLE, AND                        
*        WILL BE USED.                                                          
*                                                                               
         TM    ETCDEFLG,X'20'      ADVERT CODE ON FILE?                         
         BO    ADVC0780            NO  - EXIT CC NOT ZERO                       
         MVC   ETNADVRT,ETNADVRT-EQUITABL(R4)                                   
*                                  YES - ON-FILE CODE FOR ADVERT-               
*                                     USE ITS CODE FOR THIS LINE                
         MVC   ETNCATGY,ETNCATGY-EQUITABL(R4)                                   
*                                  LOAD CATEGORY FOR ADVERTISER                 
         MVC   8(4,R3),ETNADVRT    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     ADVC0800            EXIT CC ZERO                                 
ADVC0200 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'           SET KEY FOR ADVERT RECORD                    
         MVC   KEY+21(4),SPACES    SPACE FILL ADVERT PORTION                    
         ZIC   RF,5(R3)            GET L(FIELD IN PROCESS)                      
         BCTR  RF,0                MINUS 1 FOR EX STATEMENT                     
         EX    RF,ADVC0210         MOVE FIELD TO KEY BY LENGTH                  
         B     ADVC0220                                                         
ADVC0210 EQU   *                                                                
         MVC   KEY+21(0),8(R3)     INSERT KEY BY LENGTH                         
ADVC0220 EQU   *                                                                
         MVC   KEY+25(2),TWAAGY    INSERT TARGET REP CODE                       
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   ADVC0740            NO  - KEY INVALID AS ENTERED                 
         MVC   ETNADVRT,KEY+21     INSERT CODE INTO TABLE                       
*                                                                               
         MVC   SAVEAIO,AIO         SAVE A(IO AREA)                              
         MVC   AIO,AIO2            RESET A(IO AREA) TO IOAREA2                  
         L     R6,AIO2                                                          
         USING RADVREC,R6                                                       
                                                                                
         GOTO1 GETREC,DMCB,RADVREC                                              
                                                                                
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
         TM    RADVFLGS,X'02'      'DO NOT USE' SET?                            
         BO    ADVC0770            YES - RETURN ERROR                           
*                                                                               
         MVC   ETNCATGY,RADVCATG   INSERT CATEGORY CODE INTO TABLE              
*                                                                               
         DROP  R6                                                               
*                                                                               
         TM    ETCDEFLG,ADONFILE   CODE ALREADY ON FILE?                        
         BO    ADVC0800            YES - DON'T SET 'ADD IT'                     
         OI    ETCDEFLG,ADDADV     NO  - SET 'ADD IT'                           
         B     ADVC0800                                                         
ADVC0720 EQU   *                                                                
         MVC   RERROR,=AL2(001)    FIELD REQUIRED/MISSING                       
         B     ADVC0780                                                         
ADVC0740 EQU   *                                                                
         MVC   RERROR,=AL2(002)    FIELD INVALID AS ENTERED                     
         B     ADVC0780                                                         
ADVC0770 EQU   *                                                                
         MVC   RERROR,=AL2(906)    'DO NOT USE' IS SET                          
         B     ADVC0780                                                         
ADVC0780 EQU   *                                                                
         ST    R3,DUB              SET A(ERROR FIELD)                           
         LTR   RB,RB               SET CC NOT ZERO                              
         B     ADVC0820            EXIT WITH ERROR                              
ADVC0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
ADVC0820 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
DOREPORT NMOD1 0,*REPORT*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*                                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
*   ESTABLISH KEY BASED ON FILTER OPTIONS                                       
*                                                                               
         MVC   RCONSTYP,KEYTYPE    SET TYPE INTO KEY                            
*                                                                               
         CLI   KEYTYPE,X'BF'       AGENCY KEY SEQUENCE?                         
         BE    DORE0020            YES                                          
*                                  NO  - STATION SEQUENCE X'8E'                 
*                                                                               
         MVC   RCON8ERP,SRCEREP    INSERT SOURCE REP                            
         MVC   RCON8EST,SRCESTAT   INSERT TAKEOVER STATION                      
         B     DORE0040                                                         
DORE0020 EQU   *                   AGENCY KEY SEQUENCE X'BF'                    
         MVC   RCONCSRP,SRCEREP    INSERT SOURCE REP                            
         MVC   RCONCSAG,AGYFILT    INSERT AGENCY FILTER                         
DORE0040 EQU   *                                                                
         LA    R6,KEY              RESET USING FOR 'KEY'                        
         GOTO1 HIGH                                                             
         B     DORE0100                                                         
DORE0060 EQU   *                                                                
         LA    R6,KEY              RESET USING FOR 'KEY'                        
         GOTO1 SEQ                                                              
DORE0100 EQU   *                                                                
         MVC   MAXIOKEY,KEY        SAVE KEY FOR MAXIO SITUATION                 
         CLC   KEY(8),KEYSAVE      SAME KEY TYPE/REP/STATION?                   
         BNE   DORE0680            NO  - FINISHED SWEEP                         
*                                                                               
         CLI   KEYTYPE,X'8E'       STATION SEQUENCE RETURN?                     
         BNE   DORE0260                                                         
         CLC   RCON8EFE,EFDTCOMP   KEY END DATE VS EFFECTIVE DATE               
*                                     ENDED BEFORE EFF DATE?:  SKIP             
         BNL   DORE0140            NO  - DATES INCLUDE EFFECTIVE DATE           
DORE0120 EQU   *                                                                
         B     DORE0240            RESTART ON NEXT KEY                          
DORE0140 EQU   *                                                                
*                                                                               
         CLI   SPSAGIH+5,0         AGENCY FILTER?                               
         BE    DORE0180            NO  -                                        
         CLI   RCON8EID,1          YES - TYPE 1 KEY (AGENCY/ADVERT)?            
         BNE   DORE0180            NO  - FILTER TEST ALREADY DONE -             
*                                     NEXT FILTER(S) TO BE DONE                 
         CLC   AGYFILT+4(2),SPACES AGENCY-OFFICE IN FILTER?                     
         BE    DORE0160            NO                                           
         CLC   RCON8AGY,AGYFILT    YES - ORDER FOR AGENCY/OFF FILTER?           
*                                     COMPARE SIX CHARS                         
         BE    DORE0180            YES - CHECK NEXT FILTER                      
         B     DORE0240            NO  - SET TO SKIP                            
DORE0160 EQU   *                                                                
         CLC   RCON8AGY(4),AGYFILT YES - ORDER FOR AGENCY FILTER?               
*                                     COMPARE FOUR CHARS                        
         BNE   DORE0240            NO  - SET TO SKIP THIS CON                   
DORE0180 EQU   *                                                                
         CLI   SPSADVH+5,0         ADVERT FILTER?                               
         BNE   DORE0190            YES -                                        
*                                                                               
         CLC   =C'PV',SRCEREP                                                   
         BE    DORE0200                                                         
         CLC   =C'BL',SRCEREP                                                   
         BE    DORE0200                                                         
*                                                                               
         DC    H'0'                MUST FILTER ON ADVERT                        
DORE0190 EQU   *                                                                
         CLI   RCON8EID,1          YES - TYPE 1 KEY (AGENCY/ADVERT)?            
         BNE   DORE0200            NO  - FILTER TEST ALREADY DONE -             
         CLC   RCON8ADV,ADVFILT    YES - ORDER FOR ADVERT FILTER?               
         BNE   DORE0240            NO  - SET TO SKIP TO NEXT CON                
DORE0200 EQU   *                                                                
         CLI   RCON8EID,2          TYPE 2 KEY (SP/CONTYP/ETC)?                  
         BL    DORE0060            LOW  - GO BACK FOR TYP2                      
         BH    DORE0220            HIGH - CHECK FILTERS ON 3RD KEY              
         CLI   SPSSALH+5,0         S/P FILTER?                                  
         BE    DORE0220            NO                                           
         CLC   RCON8ESP,SPFILT     YES - ORDER FOR S/P?                         
         BNE   DORE0240            NO  - SKIP ORDER                             
DORE0220 EQU   *                                                                
         CLI   SPSOFFH+5,0         OFFICE FILTER?                               
         BE    DORE0380            NO  -                                        
         CLI   RCON8EID,3          YES - TYPE 3 KEY (OFF/DEMO/CREAT)?           
         BNE   DORE0060            NO  - GO BACK FOR NEXT KEY                   
         CLC   RCON8EOF,SPSOFF     ORDER FOR FILTER OFFICE?                     
         BE    DORE0380            YES - PROCESS ORDER                          
DORE0240 EQU   *                                                                
         MVI   RCON8EID,10         NO  - SET TYPE TO SKIP TO NEXT CON           
         XC    RCON8EAG(10),RCON8EAG                                            
*                                  CLEAR LOW KEY                                
         B     DORE0040            RESTART ON NEXT KEY                          
*                                                                               
*   SEQUENCE FOR X'BF' KEY PROCESSING: AGENCY HIGH                              
*                                                                               
DORE0260 DS    0H                                                               
         CLC   RCONCSFE,EFDTCOMP   KEY END DATE VS EFFECTIVE DATE               
*                                     ENDED BEFORE EFF DATE?:  SKIP             
         BNL   DORE0300            NO  - DATES INCLUDE EFFECTIVE DATE           
DORE0280 EQU   *                                                                
         B     DORE0060            READ SEQUENTIAL KEY NEXT                     
DORE0300 EQU   *                                                                
*                                                                               
         CLI   SPSAGIH+5,0         AGENCY FILTER?                               
         BE    DORE0340            NO  -                                        
         CLC   AGYFILT+4(2),SPACES AGENCY-OFFICE IN FILTER?                     
         BE    DORE0320            NO                                           
         CLC   RCONCSAG,AGYFILT    YES - ORDER FOR AGENCY/OFF FILTER?           
*                                     COMPARE SIX CHARS                         
         BE    DORE0340            YES - CHECK NEXT FILTER                      
         B     DORE0360            NO  - SET TO SKIP                            
DORE0320 EQU   *                                                                
         CLC   RCONCSAG(4),AGYFILT YES - ORDER FOR AGENCY FILTER?               
*                                     COMPARE FOUR CHARS                        
         BNE   DORE0360            NO  - SET TO SKIP THIS CON                   
DORE0340 EQU   *                                                                
         CLI   SPSADVH+5,0         ADVERT FILTER?                               
         BNE   DORE0350            YES -                                        
*                                                                               
         CLC   =C'PV',SRCEREP                                                   
         BE    DORE0355                                                         
         CLC   =C'BL',SRCEREP                                                   
         BE    DORE0355                                                         
         DC    H'0'                MUST FILTER ON ADVERT                        
*                                                                               
DORE0350 EQU   *                                                                
         CLC   RCONCSAD,ADVFILT    YES - ORDER FOR ADVERT FILTER?               
         BNE   DORE0360            NO  - SET TO SKIP TO NEXT CON                
*                                                                               
DORE0355 EQU   *                                                                
         CLI   SPSOFFH+5,0         OFFICE FILTER?                               
         BE    DORE0380            NO  -                                        
         CLC   RCONCSOF,SPSOFF     ORDER FOR FILTER OFFICE?                     
         BE    DORE0380            YES - PROCESS ORDER                          
DORE0360 EQU   *                                                                
         B     DORE0060            READ NEXT SEQUENTIAL KEY                     
*                                                                               
         DROP  R6                                                               
*                                                                               
DORE0380 DS    0H                                                               
         OI    DMINBTS,X'08'       READ DELETED                                 
         MVC   HIGHKEY,KEY                                                      
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08' RESET                                        
                                                                                
         L     R5,AIO              IN CASE RECORD IS DELETED BUT THE            
         USING RCONREC,R5             PASSIVE POINTERS AREN'T                   
         TM    RCONCNTL,X'80'      RECORD MARKED AS DELETE?                     
         BNO   DORE0400            NO                                           
         CLI   KEYTYPE,X'BF'       BF KEY SEQUENCE?                             
         BNE   DORE0240            NO  - 8E:  SKIP TO NEXT CONTRACT             
         B     DORE0060            YES - READ NEXT SEQUENTIAL KEY               
*                                                                               
DORE0400 EQU   *                                                                
*                                                                               
*   FOR KEY SEQUENCE BF, THE SALESPERSON CODE IS IN THE RECORD, NOT             
*        THE KEY.  THE FILTER MUST WAIT UNTIL THE RECORD HAS BEEN               
*        READ BEFORE THE TEST CAN BE ACCOMPLISHED.  BY THIS TIME,               
*        SUFFICIENT FILTERING HAS BEEN DONE SO THAT THIS SHOULDN'T              
*        BE TOO GREAT AN OVERHEAD.                                              
*                                                                               
         CLI   KEYTYPE,X'BF'       BF KEY SEQUENCE?                             
         BNE   DORE0420            NO  - SP FILTERING ALREADY DONE              
         CLI   SPSSALH+5,0         S/P FILTER?                                  
         BE    DORE0420            NO  - NO CHECK NEEDED                        
         CLC   RCONSAL,SPSSAL      YES - ORDER FOR S/P?                         
         BNE   DORE0060            NO  - SKIP ORDER                             
DORE0420 EQU   *                                                                
         CLI   DAREFLAG,C'B'       DARE FLAG FILTER?                            
         BE    DORE0438            NO  - ACCEPT BOTH                            
         MVI   ELCODE,X'1D'        LOOK FOR DARE ELEMENT                        
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   DORE0435            NO ELEMENT - NOT DARE                        
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      FOUND:  LINKED?                              
         BNO   DORE0435            NO  - NOT DARE                               
         DROP  R6                                                               
         LA    R6,KEY              RESET R6 FOR SEQ PROCESSING                  
*                                                                               
*   1D FOUND, AND ORDER IS LINKED:  DARE ORDER                                  
*                                                                               
         CLI   DAREFLAG,C'Y'       ACCEPT ONLY DARE?                            
         BE    DORE0438            YES - ACCEPT THIS ONE                        
         CLI   KEYTYPE,X'BF'       BF KEY SEQUENCE?                             
         BNE   DORE0240            NO  - '8E' PROCESSING SEQUENCE               
         B     DORE0060            YES - 'BF' PROCESSING SEQUENCE               
DORE0435 EQU   *                                                                
         LA    R6,KEY              RESET R6 FOR SEQ PROCESSING                  
*                                                                               
*   1D NOT FOUND, OR ORDER IS UNLINKED:  NOT A DARE ORDER                       
*                                                                               
         CLI   DAREFLAG,C'Y'       ACCEPT ONLY DARE?                            
         BNE   DORE0438            NO  - ACCEPT THIS ONE                        
         CLI   KEYTYPE,X'BF'       BF KEY SEQUENCE?                             
         BNE   DORE0240            NO  - '8E' PROCESSING SEQUENCE               
         B     DORE0060            YES - 'BF' PROCESSING SEQUENCE               
*                                                                               
DORE0438 EQU   *                                                                
*                                                                               
         L     R5,AIO                                                           
         USING RCONREC,R5                                                       
*                                                                               
         MVC   CON#KEY,KEY         SAVE CONTRACT RECORD X'8D/E' KEY             
*                                                                               
         MVC   LISTAR,SPACES       CLEAR DISPLAY LINES                          
         MVC   LISTAR2,SPACES                                                   
*                                                                               
         LA    R2,LISTAR+1         SET TO DATA FIELD OF LINE                    
         USING DISLIN1,R2                                                       
         LA    R3,LISTAR2          SET TO DATA FIELD OF LINE                    
         USING DISLIN2,R3                                                       
         GOTO1 HEXOUT,DMCB,RCONKCON,DL1CON#,4,=C'TOG'                           
*                                  INSERT CONTRACT NUMBER ON LINE               
         MVC   DL1AGY(4),RCONKAGY                                               
         CLC   RCONKAGY+4(2),SPACES                                             
*                                  AGENCY OFFICE IN KEY?                        
         BNH   DORE0440            NO                                           
         MVI   DL1AGY+4,C'-'       YES                                          
         MVC   DL1AGY+5(2),RCONKAGY+4                                           
DORE0440 EQU   *                                                                
         MVC   DL1ADV,RCONKADV     INSERT ADVERTISER CODE                       
         MVC   DL1STA,SPACES       CLEAR STATION FIELD                          
         MVC   DL1STA(4),RCONKSTA  INSERT STATION CALLS                         
         CLI   RCONKSTA+4,C'T'     TELEVISION STATION?                          
         BE    DORE0460            YES - NO MEDIA DISPLAYED                     
         CLI   RCONKSTA+4,C' '     NO MEDIA?                                    
         BE    DORE0460            YES - NO MEDIA DISPLAYED                     
         MVI   DL1STA+4,C'-'                                                    
         MVC   DL1STA+5(1),RCONKSTA+4                                           
*                                  INSERT MEDIA CODE                            
DORE0460 EQU   *                                                                
         CLI   DL1STA+3,C' '       THREE-CHARACTER STATION CODE?                
         BNE   DORE0480            NO                                           
         MVC   DL1STA+3(2),DL1STA+4                                             
*                                  YES - SLIDE IT OVER 1 POSITION               
         MVI   DL1STA+6,C' '       CLEAR LAST CHARACTER                         
DORE0480 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,DL1FLITE)                            
         MVI   DL1FLITE+8,C'-'                                                  
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,DL1FLITE+9)                        
         MVC   DL1SALEP,RCONSAL    INSERT SALESPERSON                           
*                                                                               
*   CYCLE X'03' ELEMENTS, ACCUMULATING ESTIMATED DOLLARS                        
*                                                                               
         SR    RE,RE               CLEAR ACCUMULATOR                            
         LA    R1,RCONELEM                                                      
DORE0500 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    DORE0560            YES                                          
         CLI   0(R1),3             ESTIMATE DOLLAR ELEMENT?                     
         BE    DORE0520            YES - ACCUMULATE IT                          
         CLI   0(R1),X'63'         TRADE ESTIMATE DOLLAR ELEMENT?               
         BNE   DORE0540            NO  - SKIP IT                                
DORE0520 EQU   *                                                                
         ZICM  RF,6(R1),4          YES - GET AMOUNT FROM ELEMENT                
         AR    RE,RF                                                            
DORE0540 EQU   *                                                                
         ZIC   RF,1(R1)            L(ELEMENT)                                   
         AR    R1,RF               BUMP TO NEXT ELEMENT                         
         B     DORE0500            GO BACK FOR NEXT                             
DORE0560 EQU   *                                                                
         EDIT  (RE),(16,DL1DOLRS),2,COMMAS=YES                                  
*                                                                               
*   TEST                                                                        
***      MVC   P+1(09),=C'LISTAR:  '                                            
***      MVC   P+12(27),KEY                                                     
***      GOTO1 SPOOL,DMCB,(R8)                                                  
*   TEST                                                                        
*                                                                               
         MVC   P(74),LISTAR        PRINT 1ST LINE                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                  BUMP TO NEXT LINE                            
         DROP  R2                                                               
*                                                                               
         MVI   ELCODE,X'17'        FIND COMBO CONTROL ELEMENT                   
         LA    R6,RCONREC                                                       
         MVC   DUB(4),RCONKCON     SAVE CONTRACT NUMBER                         
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   DORE0600            NO ELEMENT                                   
         USING RCONDREL,R6                                                      
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         SR    RE,RE                                                            
         SH    RF,=H'2'            SUBTRACT CONTROL FROM LENGTH                 
         LA    R1,9                                                             
         DR    RE,R1               DIVIDE ELT LEN BY DATA LEN                   
         LR    R0,RF               SET RESULT FOR LOOP                          
         LTR   R0,R0               ANYTHING IN REG?                             
         BZ    DORE0600            NO  - NO OUTPUT                              
         MVC   DL2COMBO,=C'COMBO:'                                              
         LA    R6,2(R6)            BUMP TO FIRST STATION IN LIST                
         LA    R2,DL2STA1          SET A(1ST STATION IN LINE)                   
*                                                                               
DORE0580 EQU   *                                                                
         CLC   DUB(4),5(R6)        CONTRACT OF ORDER?                           
         BE    DORE0590            YES - DON'T PUT ON NEXT LINE                 
*                                                                               
         MVC   0(4,R2),0(R6)       INSERT STATION CALLS                         
         MVI   4(R2),C'-'          INSERT SEPARATOR                             
*                                  COMBOS WILL ALWAYS BE RADIO                  
         MVC   5(1,R2),4(R6)       INSERT MEDIA                                 
         MVI   6(R2),C'='          INSERT SEPARATOR                             
         LA    RF,5(R6)                                                         
         ST    RF,DMCB                                                          
         LA    RF,7(R2)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 HEXOUT,DMCB,,,4,=C'TOG'                                          
*                                  INSERT CONTRACT NUMBER                       
         LA    R2,16(R2)           BUMP TO NEXT POSITION IN LINE                
DORE0590 EQU   *                                                                
         LA    R6,9(R6)            BUMP TO NEXT POSITION IN ELEMENT             
         BCT   R0,DORE0580         GO BACK FOR NEXT SLOT                        
*                                                                               
         DROP  R6                                                               
*                                                                               
DORE0600 EQU   *                                                                
         MVI   ELCODE,X'A2'        FIND ESTIMATE ELEMENT                        
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   DORE0640            NO DARE                                      
         USING RCONIEL,R6                                                       
         CLC   RCONIEST,SPACES     SHORT EST# ENTERED?                          
         BNH   DORE0620            NO                                           
         MVC   DL2EST#(4),RCONIEST YES                                          
         B     DORE0640                                                         
DORE0620 EQU   *                                                                
         MVC   DL2EST#,RCONXEST    INSERT ESTIMATE NUMBER                       
DORE0640 EQU   *                                                                
         MVI   ELCODE,X'1D'        FIND DARE ELEMENT                            
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   DORE0660            NO DARE                                      
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      LINKED?                                      
         BNO   DORE0660            NO  - DON'T DISPLAY                          
         GOTO1 HEXOUT,DMCB,RCONDRLK,DL2DARE,4,=C'TOG'                           
*                                  INSERT DARE AGENCY ORDER #                   
         DROP  R3,R6                                                            
*                                                                               
DORE0660 EQU   *                                                                
*                                                                               
*   TEST                                                                        
***      MVC   P+1(09),=C'LISTAR2: '                                            
***      MVC   P+12(27),KEY                                                     
***      GOTO1 SPOOL,DMCB,(R8)                                                  
*   TEST                                                                        
*                                                                               
         MVC   P(74),LISTAR2       PRINT 2ND LINE                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)     PUT OUT BLANK LINE                           
*                                                                               
         DROP  R5                                                               
*                                                                               
         CLI   KEYTYPE,X'BF'       BF KEY IN PROCESS?                           
         BE    DORE0060            YES                                          
         XC    KEY+16(11),KEY+16   NO  - CLEAR REMAINDER OF KEY                 
         MVI   KEY+16,X'10'        SET TO SKIP TO NEXT KEYS                     
         B     DORE0040                                                         
DORE0680 EQU   *                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+1(17),=C'**END OF REPORT**'                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO BUILD POINTER LIST IN P1                                           
*  P2 - 0=NEW POINTER                                                           
*       1=OLD POINTER                                                           
***********************************************************************         
PTRS     NMOD1 0,**PTRS**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,AIO              SET A(IO AREA FOR CONTRACT)                  
         USING RCONREC,R5                                                       
*                                                                               
         L     R2,4(R1)                                                         
         LR    RE,R2                                                            
         L     R3,8(R1)                                                         
         XCEF  (RE),500                                                         
* BUILD ACTIVE PTR                                                              
         MVI   0(R2),X'0C'                                                      
         MVC   02(02,R2),TWAAGY                                                 
         MVC   04(02,R2),RCONKGRP                                               
         MVC   06(05,R2),RCONKSTA                                               
         MVC   11(02,R2),RCONKOFF                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 1                                                                  
         MVI   0(R2),X'8C'                                                      
         MVC   21(02,R2),TWAAGY                                                 
         ZAP   DUB(5),=P'99999999'                                              
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         MVC   23(4,R2),WORK                                                    
         LA    R2,32(R2)                                                        
* CREATE PTR 2                                                                  
         MVI   0(R2),X'9C'                                                      
         MVC   02(02,R2),TWAAGY                                                 
         MVC   04(02,R2),RCONKOFF                                               
         MVC   06(02,R2),RCONKGRP                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKADV                                               
         MVC   17(04,R2),RCONKAGY                                               
         MVC   21(02,R2),RCONKAOF                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 3                                                                  
         MVI   0(R2),X'AC'                                                      
         MVC   01(2,R2),TWAAGY                                                  
         MVC   03(2,R2),RCONKOFF                                                
         MVC   05(2,R2),RCONTEM                                                 
* INVERT SALESMAN CODE FOR LAST NAME HIGH                                       
         LA    RE,RCONSAL+2        LAST INITIAL                                 
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   07(1,R2),0(RE)                                                   
         MVC   08(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '      ONLY 2 INITIALS?                             
         BNE   *+8                                                              
         MVI   9(R2),C' '                                                       
         MVC   10(5,R2),RCONKSTA                                                
         MVC   15(4,R2),RCONKAGY                                                
         MVC   19(4,R2),RCONKADV                                                
         MVC   23(4,R2),RCONKCON                                                
         LA    R2,32(R2)                                                        
* CREATE PTR 4                                                                  
         MVI   0(R2),X'BC'                                                      
         MVC   02(02,R2),TWAAGY                                                 
         MVC   04(02,R2),RCONCTGY                                               
         MVC   06(02,R2),RCONKOFF                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR BF                                                                 
BFPTR    MVI   0(R2),X'BF'                                                      
         MVC   02(02,R2),TWAAGY                                                 
         MVC   04(06,R2),RCONKAGY                                               
         MVC   10(04,R2),RCONKADV                                               
         MVC   14(05,R2),RCONKSTA                                               
         MVC   19(02,R2),RCONKOFF                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,21(R2))                            
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 5                                                                  
         MVI   0(R2),X'CC'                                                      
         MVC   01(02,R2),TWAAGY                                                 
         MVC   03(05,R2),RCONKSTA                                               
         MVC   08(02,R2),RCONKOFF                                               
         MVC   10(02,R2),RCONTEM                                                
* INVERT SALESMAN                                                               
         LA    RE,RCONSAL+2                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   12(1,R2),0(RE)                                                   
         MVC   13(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '                                                   
         BNE   *+8                                                              
         MVI   14(R2),C' '                                                      
*                                                                               
         MVC   15(04,R2),RCONKADV                                               
         MVC   19(04,R2),RCONKAGY                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
*        BNE   PTREC               NO BOP                                       
         BNE   PT8D                NO BOP                                       
         SPACE 1                                                                
         USING RCONBPEL,R6                                                      
DCPRTR   MVI   0(R2),X'DC'         CREATE BOP POINTER FOR CHANGE                
         MVC   5(2,R2),TWAAGY                                                   
         MVC   7(4,R2),RCONKADV                                                 
         GOTO1 DATCON,DMCB,(5,WORK),(3,TODAY)                                   
*                                  INSERT DATE                                  
         MVC   11(3,R2),TODAY      NEW POINTER GETS TODAYS DATE                 
         B     DC20                ALWAYS AN 'ADD'                              
*                                                                               
*        CLC   CONACT(3),=C'ADD'                                                
*        BE    DC20                                                             
*        LTR   R3,R3               0=NEW POINTER                                
*        BZ    DC20                                                             
*        MVC   11(3,R2),RCONBPDT   OLD PTR NEEDS OLD BOP CHANGE DATE            
*    (IN ADPTRS, ONLY ADD NEW POINTER IF CHANGE IS OTHER THAN DATE)             
*                                                                               
DC20     MVC   14(4,R2),RCONBPRF                                                
         MVC   18(5,R2),RCONKSTA                                                
         MVC   23(4,R2),RCONKCON                                                
         LA    R2,32(R2)                                                        
         DROP  R6                                                               
*                                                                               
*   CREATE X'8D' POINTERS                                                       
*                                                                               
PT8D     EQU   *                                                                
                                                                                
* - GET FLIGHT START/END DATES AND SAVE IN WORK+40                              
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,WORK+40)    START DATE               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,WORK+42)  END DATE                 
                                                                                
* - GET DEMO FROM BOP OR SAR ELEMENT AND SAVE IN WORK+45                        
* - LOOK FOR DEMO MARKED AS PRIMARY (X'40' IN 1ST BYTE)                         
* - IF NO DEMO MARKED AS PRIMARY, USE 1ST DEMO AS DEFAULT                       
*                                                                               
         XC    WORK+45(3),WORK+45                                               
         LA    R4,RCONELEM                                                      
PPC8DLP  CLI   0(R4),0                                                          
         BE    PPC8D00                                                          
         CLI   0(R4),X'12'         SAR ELEMENT                                  
         BE    PPC8DDD                                                          
         CLI   0(R4),X'10'         BOP ELEMENT                                  
         BE    PPC8DEE                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPC8D00                                                          
         AR    R4,R1                                                            
         B     PPC8DLP             GO TO LOOP                                   
         USING RSARCO,R4                                                        
PPC8DDD  LA    RE,RSARDEM            DEMO                                       
         LA    RF,8                                                             
         MVC   WORK+45(3),RSARDEM    DEMO                                       
PPC8DDE  TM    0(RE),X'40'         IS IT MARKED AS PRIMARY ?                    
         BO    PPC8DDF               YES                                        
         LA    RE,3(RE)                                                         
         MVC   WORK+45(3),0(RE)                                                 
         BCT   RF,PPC8DDE                                                       
         MVC   WORK+45(3),RSARDEM    NO/USE 1ST AS DEFAULT                      
PPC8DDF  NI    WORK+45,X'FF'-X'40'          CLEAR MAIN DEMO INDICATOR           
         B     PPC8D00                                                          
                                                                                
         USING RCONBPEL,R4                                                      
PPC8DEE  LA    RE,RCONBPDM+1                                                    
****     LA    RF,8      THIS CAN'T BE 8!! FIELD IS ONLY 20 LONG                
         LA    RF,6                                                             
         MVC   WORK+45(3),RCONBPDM+1                                            
PPC8DEF  TM    0(RE),X'40'              IS IT MARKED AS PRIMARY DEMO?           
         BO    PPC8DEG                  YES                                     
         LA    RE,3(RE)                 NO/BUMP TO NEXT DEMO                    
         MVC   WORK+45(3),0(RE)                                                 
         BCT   RF,PPC8DEF                                                       
         MVC   WORK+45(3),RCONBPDM+1     NO PRIMARY/USE 1ST AS DEFAULT          
PPC8DEG  NI    WORK+45,X'FF'-X'40'           CLEAR MAIN DEMO INDICATOR          
         B     PPC8D00                                                          
         DROP R4                                                                
                                                                                
* BUILD BASIC KEY IN WORK                                                       
PPC8D00  XC    WORK(32),WORK                                                    
         LA    R4,WORK                                                          
         MVI   0(R4),X'8D'                                                      
         MVC   1(2,R4),RCONKREP                                                 
         MVC   8(2,R4),WORK+40    START DATE                                    
         MVC   10(2,R4),WORK+42    END DATE                                     
         MVC   12(4,R4),RCONKCON   CONTRACT NUMBER                              
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),1                                                         
         MVC   17(6,R2),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R2),RCONKADV       ADVERTISER                               
         LA    R2,32(R2)                                                        
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),2                                                         
         MVC   17(3,R2),RCONSAL                                                 
         MVC   20(1,R2),RCONTYPE                                                
         MVC   21(2,R2),RCONKGRP                                                
         MVC   23(2,R2),RCONCTGY                                                
         MVC   25(2,R2),RCONTEM                                                 
         LA    R2,32(R2)                                                        
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(2,22(R2))                              
         MVC   19(3,R2),WORK+45       DEMO                                      
PPC8DX   LA    R2,32(R2)                                                        
*                                                                               
* END X'8D' PASSIVE POINTERS                                                    
*                                                                               
*   CREATE X'8E' POINTERS                                                       
*   SIMILAR TO X'8D' POINTERS BUT HAVE STATION IN KEY INSTEAD OF 0'S            
*                                                                               
* WORK HAS BASIC KEY- REPLACE ZEROS OF X'8D' WITH STATION                       
PPCON8E  MVI   WORK,X'8E'                                                       
         MVC   WORK+3(5),RCONKSTA                                               
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),1                                                         
         MVC   17(6,R2),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R2),RCONKADV       ADVERTISER                               
         LA    R2,32(R2)                                                        
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),2                                                         
         MVC   17(3,R2),RCONSAL                                                 
         MVC   20(1,R2),RCONTYPE                                                
         MVC   21(2,R2),RCONKGRP                                                
         MVC   23(2,R2),RCONCTGY                                                
         MVC   25(2,R2),RCONTEM                                                 
         LA    R2,32(R2)                                                        
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(2,22(R2))                              
         MVC   19(3,R2),WORK+45    DEMO                                         
PPC8EX   LA    R2,32(R2)                                                        
                                                                                
* END X'8E' PASSIVE POINTERS                                                    
*                                                                               
         EJECT                                                                  
* ADD X'9D' PASSIVE POINTER (RIS/PRODUCT)                                       
*                                                                               
         MVI   0(R2),X'9D'                                                      
         MVC   1(2,R2),RCONKREP                                                 
         MVC   3(5,R2),RCONKSTA                                                 
         MVC   8(4,R2),RCONKADV                                                 
         MVC   23(4,R2),RCONKCON                                                
* NOW SET PRODUCT NAME OR CODE INTO KEY                                         
         MVI   12(R2),X'FF'              SET DEFAULT TO LAST                    
         MVC   13(3,R2),RCONPRD                                                 
         LA    R6,RCONREC          NOW LOOK FOR PRODUCT NAME                    
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   PP9DX                                                            
         USING RCONEXEL,R6                                                      
         MVC   12(9,R2),RCONEXPR   SET PRODUCT NAME                             
         DROP  R6                                                               
PP9DX    LA    R2,32(R2)           9D POINTER CREATED, BUMP                     
*                                                                               
* END X'9D' PASSIVE POINTER                                                     
*                                                                               
PTREC    CLC   CONACT(3),=C'ADD'                                                
         BNE   PTRX                                                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   PTRX                NO SAR                                       
         SPACE 1                                                                
         USING RSAREL,R6                                                        
         MVI   0(R2),X'EC'                                                      
         MVC   21(2,R2),TWAAGY                                                  
         MVC   23(4,R2),TRGTCON                                                 
PTRX     DS    0H                                                               
         XMOD1                                                                  
         DROP  R5,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD POINTERS TO FILE                                               
*              P1=A(WORKSPACE)                                                  
*              P2=A(OLD PTR LIST)                                               
*              P3=A(NEW PTR LIST)                                               
*              P4=A(DISK ADDR)                                                  
***********************************************************************         
ADDPTRS  NMOD1 0,*ADDPTR*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LM    R2,R4,4(R1)                                                      
*                                                                               
         MVI   DMOUTBTS,0          FOR PROPER RECOVERY                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
AP25     CLC   0(27,R2),0(R3)      SAME?                                        
         BE    AP100                                                            
*                                                                               
*   TESTKEY                                                                     
***      CLC   TESTKEY(LTESTKEY),0(R3)       TEST SAMPLE FOUND?                 
***      BNE   TESTGO              NO                                           
***      MVC   DIE3(2),=X'0000'                                                 
TESTGO   EQU   *                                                                
*   TESTKEY END TEST                                                            
*                                                                               
* DIFFERENT                                                                     
* DELETE OLD PTR                                                                
         CLI   0(R2),0             ADD?                                         
         BNE   AP30                                                             
* ADD                                                                           
         MVC   KEY,0(R3)           NEW KEY                                      
         B     AP50                                                             
* CHANGE                                                                        
AP30     MVC   KEY,0(R2)                                                        
         CLI   KEY,X'DC'           BOP POINTER ONLY CHANGED IF                  
         BNE   AP33                                                             
         CLC   0(11,R3),KEY        ADVERTISER OR                                
         BNE   AP33                                                             
         CLC   14(13,R3),KEY+14    REF #, STATION OR CON# CHANGES               
         BE    AP100                                                            
AP33     OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
DIE3     EQU   *                                                                
         BAS   RE,APCHECK                                                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AP40                                                             
         MVI   KEY+27,X'FF'                                                     
         GOTO1 WRITE                                                            
         BAS   RE,APCHECK                                                       
* ADD NEW PTR                                                                   
AP40     MVC   KEY,0(R3)                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         BAS   RE,APCHECK                                                       
         CLC   KEY(27),KEYSAVE     KEY ALREADY THERE?                           
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     AP50                                                             
* UNDELETE OLD PTR                                                              
         MVI   KEY+27,0                                                         
         GOTO1 WRITE                                                            
         BAS   RE,APCHECK                                                       
         B     AP100                                                            
* ADD PTR                                                                       
AP50     MVI   KEY+27,0                                                         
         MVC   KEY+28(4),0(R4)     DISK ADDR                                    
         GOTO1 ADD                                                              
         BAS   RE,APCHECK                                                       
*                                                                               
* NEXT POINTER                                                                  
AP100    LA    R2,32(R2)                                                        
         LA    R3,32(R3)                                                        
         CLI   0(R3),0             LAST?                                        
         BNE   AP25                                                             
         MVI   DMOUTBTS,X'FD'                                                   
         B     EXXIT                                                            
*                                                                               
APCHECK  TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
EXXIT    XMOD1                                                                  
         EJECT                                                                  
TESTKEY  DC    X'8E'                                                            
         DC    C'B3WCANA'                                                       
         DC    X'CD1DCD9D0386528203'                                            
         DC    C'LA'                                                            
LTESTKEY EQU   *-TESTKEY                                                        
         LTORG                                                                  
*                                                                               
***>>>                                                                          
SUBROUT  DS    0H                                                               
         NMOD1 0,**SUBR**                                                       
         L     RC,0(R1)            SET A(WORKSPACE)                             
         L     R2,8(R1)            SET A(EQUIV TABLE ENTRY IN PROCESS)          
         L     R8,ASPOOLD                                                       
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R5,SYSSPARE                                                      
                                                                                
QGOCON   EQU   4                   GO AND SWAP TO CONTRACT                      
QCKGLOB  EQU   6                   CHECK IF GLOBBER CALLS                       
                                                                                
         CLI   4(R1),QGOCON                                                     
         BE    GOCONPGM                                                         
         CLI   4(R1),QCKGLOB                                                    
         BE    CKGLOB                                                           
         DC    H'0'                                                             
*                                                                               
SUBYES   SR    R1,R1                                                            
         B     *+8                                                              
SUBNO    LA    R1,1                SET CONDITION CODE AT EXIT                   
         LTR   R1,R1                                                            
SUBRX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* GO AND SWAP TO CONTRACT                                                       
***********************************************************************         
GOCONPGM DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R3,BLOCK                                                         
         USING GLVXFRSY,R3                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'SFM'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
***      OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R3                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,24,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R3,BLOCK                                                         
         USING GLCONNUM,R3                                                      
*                                                                               
         USING EQUITABL,R2                                                      
         GOTO1 HEXOUT,DMCB,ETNCONUM,GLCONNUM,4,=C'TOG'                          
         MVC   GLCONCA(3),=C'CF*'  PAPERWORK IF 'CONFIRMED ORDER'               
***>>>   MVC   GLCONCA(3),=C'PRI'  PRINT IF 'CONFIRMED ORDER'                   
         TM    ETFLAG,X'01'        'VERSION' IN PROCESS?                        
         BNO   GOCO0020            NO  - 'OFF' = CONFIRMED                      
         MVC   GLCONCA(4),=C'LAST' WKST IF 'VERSION ORDER'                      
GOCO0020 EQU   *                                                                
         MVI   GLCONFLG,X'80'      SET 'RETURN FROM CONTRACT'                   
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,GLCONLNQ,GLRKACT                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*              TRANSFER TO CONTRACT FROM HERE!!                                 
         XMOD1 2                   EXIT ALL THE WAY OUT                         
***********************************************************************         
* CHECK IF ANYTHING IN GLOBBER                                                  
***********************************************************************         
CKGLOB   DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                          
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BO    SUBNO                                                            
         GOTO1 (RF),DMCB,=C'DELE',,,GLVXCTL                                     
*                                                                               
         B     SUBYES                                                           
         DROP  R4                                                               
         EJECT                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027RESFM35   03/05/08'                                      
         END                                                                    
