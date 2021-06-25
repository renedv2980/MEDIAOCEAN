*          DATA SET RECNT11    AT LEVEL 039 AS OF 02/11/04                      
*          DATA SET RECNT11    AT LEVEL 036 AS OF 02/03/97                      
*PHASE T80211C,*                                                                
         TITLE 'RECNT11 (T80211) - FORECAST HEADLINE EDIT/DISP'                 
*                                                                               
*********************************************************************           
*                                                                   *           
*  RECNT11 (T80211) - FORECAST HEADLINE EDIT/DISP                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 12APR94 (SKU)  --- INITIAL ENTRY                                  *           
*                                                                   *           
* 19AUG94 (SKU)  --- UPDATE LAST ACTIVITY DATE FOR BOP COMMENT CHGS *           
*                                                                   *           
* 17OCT94 (SKU)  --- EXPAND X'20' SEND ELEMENT TO SAVE LAST 3       *           
*                    REP AND STA VERSION DATES                      *           
*                                                                   *           
* 18JAN95 (SKU)  --- CALL REGENVER TO MARK CONTRACT UNCONFIMRED     *           
*                    AND BUMP VERSION NUMBERS                       *           
*                                                                   *           
* 23SEP95 (SKU)  --- 2K CONTRACT SUPPORT                            *           
*                                                                   *           
* 22APR96 (SKU)  --- ADD RIS KEY SUPPORT                            *           
*                                                                   *           
* 04OCT96 (SKU)  --- SUPPORT LOW POWER STATIONS                     *           
*                                                                   *           
* 31JAN97 (RHV)  --- FIX OFFICE TEAM VALIDATION BUG                 *           
*                                                                   *           
* 31JAN97 (RHV)  --- BROWSE SUPPORT                                 *           
*                                                                   *           
* 29MAY97 (BU )  --- REMOVE OFFTEAM REQUIREMENT FOR KATZ TV         *           
*                                                                   *           
* 27MAY99 (ROB)  --- NEW SONNET 9F KEY                              *           
*                                                                   *           
* 30NOV99 (SKU)  --- FORECAST COMMENT BUG FIX                       *           
*                                                                   *           
* 23DEC02 (HQ )  --- BYPASS OFFICE/TEAM CHECK FOR MILLENNIUM (SZ)   *           
*                     **  END TOMBSTONE  **                         *           
*HERE****************************************************************           
*                                                                               
T80211   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80211,R9                                                      
         L     RC,0(R1)            WORK AREA                                    
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
                                                                                
         L     R2,4(R1)                                                         
                                                                                
         CLC   =C'CLR',0(R2)                                                    
         BNE   MAIN00                                                           
         GOTO1 =A(DISCON),DMCB,(RC),(R2),RR=Y                                   
         B     EXXMOD                                                           
                                                                                
MAIN00   DS    0H                                                               
         CLC   =C'DISP',0(R2)                                                   
         BNE   MAIN10                                                           
         GOTO1 =A(DISCON),DMCB,(RC),0,RR=Y                                      
         B     EXXMOD                                                           
                                                                                
MAIN10   DS    0H                                                               
         CLC   =C'EDIT',0(R2)                                                   
         BE    CONCHG                                                           
         DC    H'0'                                                             
         EJECT                                                                  
*********************************************************************           
* ADD/UPDATE FORECAST CONTRACT                                                  
*********************************************************************           
CONCHG   DS    0H                                                               
         CLC   =C'ADDF',CONCACT                                                 
         BE    CONADD                                                           
*                                                                               
* GET CONTRACT REC                                                              
*                                                                               
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VMOVEREC,DMCB,AIO4,RCONREC                                       
         L     R8,AIO2                                                          
         GOTO1 VMOVEREC,DMCB,RCONREC,(R8)                                       
                                                                                
         LA    R2,CONCACTH                                                      
         LA    R3,ACTERR                                                        
         L     R4,AIO4                                                          
         TM    29(R4),X'01'        COMPRESSED CONTRACT                          
         BO    ERROR                                                            
                                                                                
         CLC   =C'FC',CONACT                                                    
         BE    CHGCON                                                           
         CLC   =C'FC',BUYACT                                                    
         BE    CHGCON                                                           
         DC    H'0'                                                             
                                                                                
CONADD   XC    RCONREC(250),RCONREC                                             
         MVI   RCONLEN+1,94        REC LEN (34 + 60)                            
         MVC   RCONELEM(2),=X'013C'    DESC ELEM CODE PLUS LENGTH               
         FOUT  CONAGYNH,MYSPACES,20                                             
         FOUT  CONADVNH,MYSPACES,20                                             
         FOUT  CONSTAMH,MYSPACES,20                                             
         FOUT  CONSALNH,MYSPACES,19                                             
         FOUT  CONOFFNH,MYSPACES,16                                             
         FOUT  CONDSPNH,MYSPACES,15                                             
         FOUT  CONDCTNH,MYSPACES,15                                             
         EJECT                                                                  
**********************************************************************          
*              VALIDATE AGENCY(OFFICE) CODE                                     
**********************************************************************          
CHGCON   DS    0H                                                               
*******TEMPORARY TRAP                                                           
          CLI   FORCMT2H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT3H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT4H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
*******                                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        RANDOM FLAGS ELEMENT PRESENT?                
         BAS   RE,GETEL                                                         
         BE    AGYED               YES - SKIP                                   
         LA    R6,WORK             NO - ADD EMPTY ELEMENT                       
         XC    WORK,WORK                                                        
         USING RCONRFEL,R6                                                      
         MVI   RCONRFCD,X'1E'                                                   
         MVI   RCONRFLN,RCONRFLQ                                                
*                                                                               
         CLC   =C'ADD',CONACT      ON ADD ONLY                                  
         BNE   CHGCON15                                                         
         TM    PROFILES+CNTRPEAB,CNTRPEAA   PROF 46 (CHECK FOR L)               
         BZ    CHGCON15                                                         
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET IN PROGRESS?                     
         BZ    CHGCON15                                                         
         OI    RCONRF1,X'20'       MARK CONTRACT AS LOCAL ORDER                 
CHGCON15 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         DROP  R6                                                               
**********************************************************************          
*              VALIDATE AGENCY(OFFICE) CODE                                     
**********************************************************************          
AGYED    DS    0H                                                               
         MVI   UPVER,0    ACE/GRAPHNET FLAG TO UP VERSION & UNCONFIRM           
         LA    R2,CONAGYH                                                       
         LA    R3,AGYERR                                                        
         TM    4(R2),X'20'                                                      
         BO    BUYED                                                            
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   AGY050                                                           
         L     RE,4(RD)            SET TO RECNT00 RD                            
         L     RE,4(RE)                                                         
         GOTO1 (RFBROWSE,VREPFACS),DMCB,ACOMFACS,(RE),(R2),CONAGYNH,   +        
               (0,C' AGY')                                                      
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
AGY050   MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGYKTYP,10         REC TYPE                                     
         GOTO1 VMOVE                                                            
         LA    RE,WORK                                                          
*                                                                               
         MVI   RAGYKAGY,C' '                                                    
         MVC   RAGYKAGY+1(5),RAGYKAGY                                           
*                                                                               
* CHECK FOR AGENCY OFFICE                                                       
*                                                                               
         CLI   0(RE),C'-'                                                       
         BE    AGY100                                                           
         CLI   0(RE),C' '                                                       
         BE    AGY150                                                           
         LA    RE,1(RE)                                                         
         B     *-20                                                             
*              AGENCY OFFICE                                                    
AGY100   MVC   RAGYKAOF,1(RE)      AGENCY OFFICE                                
         LA    RF,WORK+1                                                        
         SR    RE,RF                                                            
         EX    RE,MOVEAGY                                                       
         B     AGY200                                                           
*                                                                               
MOVEAGY  MVC   RAGYKAGY(0),WORK                                                 
*                                                                               
AGY150   MVC   RAGYKAGY(4),WORK                                                 
*                                                                               
AGY200   DS    0H                                                               
         MVC   RAGYKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
AGY210   GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
* CHECK IF DEFAULT                                                              
AGY230   GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   WAGYEXP,RAGYNAM1    SAVE EXPANSION FOR DISPLAY                   
         CLC   KEY+23(2),MYSPACES                                               
         BNE   AGY250                                                           
         SPACE 2                                                                
* DEFAULT - CHECK IF OFFICE EXISTS                                              
         LA    R3,AODERR                                                        
         MVC   KEY+25(2),IOAREA+25                                              
AGY235   GOTO1 VSEQ                                                             
         CLC   KEY(23),IOAREA      SAME AGENCY?                                 
         BNE   AGY250                                                           
         CLC   KEY+23(2),MYSPACES                                               
         BE    AGY235                                                           
         CLC   KEY+25(2),IOAREA+25                                              
         BE    ERROR                                                            
         B     AGY235                                                           
AGY250   FOUT  CONAGYNH,RAGYNAM1,20                                             
         MVC   RCONKAGY(6),RAGYKAGY                                             
         EJECT                                                                  
**********************************************************************          
* BUYER DEFAULT TO 'FORECAST'                                                   
**********************************************************************          
BUYED    DS    0H                                                               
         MVC   RCONBUYR,MYSPACES                                                
         MVC   RCONBUYR(8),=C'FORECAST'                                         
         EJECT                                                                  
**********************************************************************          
*              VALIDATE ADVERTISER                                              
**********************************************************************          
ADVED    LA    R2,CONADVH                                                       
         LA    R3,ADVERR                                                        
         MVC   SVADV,RCONKADV      SAVE LATER FOR PRODUCT LOCK                  
* VALIDATE ADVERTISER EVERYTIME IN CASE A NEW CATEGORY CODE IS                  
*   ASSIGNED.                                                                   
*                                                                               
*        TM    4(R2),X'20'         VALID ADVERTISER?                            
*        BZ    ADVED05                                                          
*                                                                               
*        TM    CONPRDH+4,X'20'     PRD CODE CHANGED?                            
*        BZ    ADVED03                                                          
*        TM    CONCATH+4,X'20'     CAT CODE CHANGED?                            
*        BO    STAED               NEED ADV CAT CODE                            
*                                                                               
*ADVED03  GOTO1 VMOVE                                                           
*         B     ADV50                                                           
*                                                                               
*              VALIDATE ADVERTISER                                              
ADVED05  DS    0H                                                               
         CLC   =C'FC',CONACT                                                    
         BNE   ADVED10                                                          
         GOTO1 =A(CKXFER),DMCB,(RC),RR=Y                                        
         BZ    ADVED10                                                          
         LA    R3,271              CANT CHG ADV ONCE K XFERRED                  
         B     ERROR                                                            
ADVED10  NI    CONPRDH+4,X'DF'     MAKE SURE PRODUCT GETS REVALIDATED           
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   ADVED20                                                          
         L     RE,4(RD)            SET TO RECNT00 RD                            
         L     RE,4(RE)                                                         
         GOTO1 (RFBROWSE,VREPFACS),DMCB,ACOMFACS,(RE),(R2),CONADVNH,   +        
               (0,C' ADV')                                                      
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
ADVED20  MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         GOTO1 VMOVE                                                            
         CLC   =C'FC',CONACT       DO NOT ALLOW CHANGE FROM                     
         BNE   ADV50               'GEN' UNLESS CON HAS SAR                     
         CLC   REPALPHA,=C'BL'                                                  
         BNE   ADV50               ONLY DO THIS CHECK FOR BLAIR                 
         CLC   RCONKADV,=CL4'GEN'                                               
         BNE   ADV50                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    ADV50               FOUND SAR ELEMENT                            
         LA    R3,240              SAR DATA MUST BE INPUT FIRST                 
         B     ERROR                                                            
         SPACE 1                                                                
ADV50    XC    IOAREA(32),IOAREA                                                
         MVI   RADVKTYP,8          RECORD TYPE FOR ADVERTISER                   
         MVC   RADVKADV,WORK                                                    
         MVC   RADVKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
ADV100   GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   WADVEXP,RADVNAME                                                 
         MVC   SVCATADV,RADVCATG   SAVE CAT CODE FOR CATED VALIDATION           
         FOUT  CONADVNH,RADVNAME,20     ADVERTISER NAME                         
*                                                                               
         MVC   RCONKADV,RADVKADV   TO K REC                                     
         EJECT                                                                  
**********************************************************************          
*              VALIDATE STATION                                                 
**********************************************************************          
STAED    DS    0H                                                               
         GOTO1 =A(STAEDIT),DMCB,(RC),RR=Y                                       
         EJECT                                                                  
**********************************************************************          
* PRODUCT DEFAULT TO 'FORECAST'                                                 
**********************************************************************          
PRDED    DS    0H                                                               
         MVC   WORK2(22),MYSPACES                                               
         MVC   WORK2(2),=X'0516'   EXPANSION ELEM CODE + LENGTH                 
         MVC   WORK2+2(8),=C'FORECAST'                                          
         CLC   =C'ADD',CONACT                                                   
         BE    PRDED10                                                          
         GOTO1 VDELELEM,DMCB,(5,RCONREC)   DELETE ELEM IF CHANGE                
                                                                                
PRDED10  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         MVC   RCONPRD,MYSPACES                                                 
         EJECT                                                                  
**********************************************************************          
* IF ADVERTISER PROVIDED A CATEGORY CODE, USE IT                                
**********************************************************************          
CATED    DS    0H                                                               
         TM    CONADVH+4,X'20'                                                  
         BO    SALED                                                            
*                                                                               
         OC    SVCATADV,MYSPACES                                                
                                                                                
         XC    IOAREA(32),IOAREA                                                
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,REPALPHA                                                
         CLC   SVCATADV,MYSPACES   CHECK IF ADV PROVIDES A CATG CODE            
         BE    CATED10                                                          
         MVC   RCTGKCTG,SVCATADV                                                
         B     CATED20                                                          
*                                                                               
CATED10  DS    0H                  NO, DEFAULT TO FC                            
         MVC   RCTGKCTG,=C'FC'                                                  
*                                                                               
CATED20  DS    0H                                                               
         LA    R2,CONADVH                                                       
         LA    R3,297              PLEASE ADD FORECAST CATEGORY CODE            
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   RCONCTGY,RCTGKCTG   CATEGORY                                     
         EJECT                                                                  
**********************************************************************          
*              VALIDATE SALESMAN                                                
**********************************************************************          
SALED    LA    R2,CONSALH                                                       
         LA    R3,SALERR                                                        
         TM    4(R2),X'20'         VALID SALESMAN?                              
         BO    RTGED                                                            
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   SALED05                                                          
         L     RE,4(RD)            SET TO RECNT00 RD                            
         L     RE,4(RE)                                                         
         GOTO1 (RFBROWSE,VREPFACS),DMCB,ACOMFACS,(RE),(R2),CONSALNH,   +        
               (X'80',C' SAL')                                                  
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
* IF SALESMAN CHANGES, IT DOESN'T AFFECT VERSION OR CONFIRMED STATUS            
*                                                                               
SALED05  GOTO1 VMOVE                                                            
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RSALKTYP,6          SALESMAN REC TYPE                            
         MVC   RSALKREP,REPALPHA   REP CODE                                     
         MVC   RSALKSAL,WORK                                                    
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWASALTL,RSALTEL                                                 
         MVC   WSALEXP,RSALNAME                                                 
         FOUT  CONSALNH,RSALNAME,19                                             
*                                  SALESMAN'S NAME                              
*                                                                               
         MVC   RCONTEM,RSALTEAM    TEAM                                         
         MVC   RCONSAL,RSALKSAL    SALESMAN'S CODE                              
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(L'RSALOFF),RSALOFF                                          
*                                                                               
*  TEMPORARILY BYPASS THE OFFTEAM TESTING FOR KATZ TV.                          
*  TEMPORARILY BYPASS THE OFFTEAM TESTING FOR TEST FILES (KATZ TV)              
*  TEMPORARILY BYPASS THE OFFTEAM TESTING FOR MILLENNIUM (SZ)                   
*                                                                               
         CLC   =C'AM',REPALPHA     KATZ AMERICAN?                               
         BE    SALED30             YES - DON'T TEST OFF/TEAM SETUP              
         CLC   =C'CQ',REPALPHA     KATZ CONTINENTAL?                            
         BE    SALED30             YES - DON'T TEST OFF/TEAM SETUP              
         CLC   =C'NK',REPALPHA     KATZ NATIONAL?                               
         BE    SALED30             YES - DON'T TEST OFF/TEAM SETUP              
         CLC   =C'SZ',REPALPHA     MILLENNIUM?                                  
         BE    SALED30             YES - DON'T TEST OFF/TEAM SETUP              
*        CLC   =C'TV',REPALPHA     KATZ TV TEST FILES?                          
*        BE    SALED30             YES - DON'T TEST OFF/TEAM SETUP              
*                                                                               
*                                                                               
* VALIDATE IF SALESPERSON'S OFF/TEAM CODE MATCHES THE STATION'S                 
*                                                                               
         OC    TWASTOTC,TWASTOTC   THERE ARE NONE, THEN SKIP                    
         BZ    SALED30                                                          
         LA    R4,15               15 OFFICE/TEAM SETS MAX                      
         LA    R5,TWASTOTC                                                      
*                                                                               
SALED10  CLC   RSALOFF,0(R5)       IF OFFICE IS NOT IN                          
         BE    SALED15             OFFTEAM LIST, SKIP VALIDATION                
         LA    R5,4(R5)                                                         
         BCT   R4,SALED10                                                       
         B     SALED30                                                          
*                                                                               
SALED15  DS    0H                  OFFICE IS IN OFFTEAM LIST,                   
         CLC   RSALOFF,0(R5)       VALIDATE TEAM                                
         BNE   SALED20                                                          
****>>>  CLC   RSALTEAM,2(R5)                                                   
         CLC   RCONTEM,2(R5)                                                    
         BE    SALED30             SALESPERSON AND STATION OFF/TEAM             
*                                                                               
SALED20  LA    R5,4(R5)                                                         
         BCT   R4,SALED15                                                       
         LA    R3,288                                                           
         B     ERROR               CODE MISMATCH                                
*                                                                               
SALED30  EQU   *                                                                
         OC    RSALLEAV,RSALLEAV   ANY LEAVE DATE?                              
         BZ    OFFED               NO  - SALESPERSON STILL ACTIVE               
         GOTO1 DATCON,DMCB,(5,FULL),(3,FULL)                                    
*                                  YES - CHECK AGAINST TODAY'S DATE             
         CLC   RSALLEAV,FULL       LEAVE DATE VS TODAY'S DATE                   
         BH    OFFED               NOT GONE YET: LD > TD                        
         LA    R3,SALLEFT          GONE - REJECT ENTRY                          
         B     ERROR               GO TO ERROR RTN                              
*                                                                               
SALLEFT  EQU   407                 SALESPERSON GONE ERROR CODE                  
         EJECT                                                                  
OFFED    DS    0H                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   ROFFKTYP,4          OFFICE REC TYPE                              
         MVC   ROFFKREP,REPALPHA   REP CODE                                     
         MVC   ROFFKOFF,WORK                                                    
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    OFF20                                                            
         CLC   TWAACCS(2),=C'O='   TEST FOR OFFICE RESTRICTION                  
         BNE   OFF20                                                            
         TM    TWAAUTH,X'80'       TEST IF TERMINAL ALLOWED ACCESS              
         BO    OFF20               TO ALL OFFICES                               
         CLC   ROFFKOFF,TWAACCS+2  ELSE, COMPARE OFFICES                        
         BE    OFF20                                                            
         LA    R3,55               ERROR, SECURITY LOCKOUT                      
         B     ERROR                                                            
*                                                                               
OFF20    GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAOFAD1,ROFFADD1                                                
         MVC   TWAOFAD2,ROFFADD2                                                
         MVC   TWAOFSTT,ROFFSTT                                                 
         MVC   TWAOFZIP,ROFFZIP                                                 
         MVC   WOFFEXP,ROFFNAME                                                 
*                                                                               
         FOUT  CONOFFNH,ROFFNAME,16                                             
*                                                                               
         MVC   RCONKOFF,ROFFKOFF                                                
         EJECT                                                                  
**********************************************************************          
* IF REP PROFILE FOR NSI DEFAULT RATING SERVICE ON, USE IT                      
**********************************************************************          
RTGED    DS    0H                                                               
         MVI   RCONRTGS,C'N'       NSI                                          
         MVI   RCONRMON,0                                                       
         TM    TWATIME,X'40'                                                    
         BZ    DTED                                                             
         MVI   RCONRTGS,C'A'       ARB                                          
         MVI   RCONRMON,0                                                       
         EJECT                                                                  
**********************************************************************          
*              VALIDATE START DATE                                              
**********************************************************************          
DTED     LA    R2,CONDTESH                                                      
         TM    4(R2),X'20'         VALID?                                       
         BO    STABUDED                                                         
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         GOTO1 SCANNER,DMCB,CONDTESH,(2,WORK2),C',=-='                          
         CLI   DMCB+4,2            IF NOT 2 DATES, ERROR                        
         BE    DTED10                                                           
         LA    R3,EDTERR           MISSING END DATE                             
         B     ERROR                                                            
DTED10   LA    R3,SDTERR                                                        
         GOTO1 DATVAL,DMCB,WORK2+12,WORK                                        
*                                                                               
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,WORK,(3,RCONDATE)      START DATE                    
         EJECT                                                                  
* START DATE MUST BE ON OR AFTER STATION JOIN DATE                              
         LA    R3,287                                                           
         CLC   TWASTJDT,RCONDATE                                                
         BH    ERROR                                                            
*              VALIDATE END DATE                                                
* END DATE IS SECOND IN SCANNER BLOCK                                           
         LA    R3,EDTERR                                                        
         GOTO1 DATVAL,DMCB,WORK2+44,WORK+6                                      
*                                                                               
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
*                                                                               
         LA    R3,64               ERR - EDT BEFORE SDT                         
         CLC   WORK+6(6),WORK      END V START DATE                             
         BL    ERROR                                                            
*              PUT END DATE IN CONREC                                           
         GOTO1 DATCON,DMCB,WORK+6,(3,RCONDATE+3)                                
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
* CHECK IF K DATES EXCEED 1 CALENDAR YR (13 BROADCAST MONTHS MAX)               
         LA    R3,CALERR                                                        
         CLC   RCONDATE(3),RCONDATE+3        YEARS                              
         BH    ERROR                                                            
* COUNT K WEEKS                                                                 
ED5      SR    R4,R4                                                            
         MVC   WORK+12(6),WORK                                                  
ED10     LA    R4,1(R4)                                                         
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,7                                     
         MVC   WORK+12(6),WORK+18                                               
         CLC   WORK+12(6),WORK+6                                                
         BNH   ED10                                                             
         STC   R4,RCONWKS                                                       
* CHECK THAT K DATES NOT INCONSISTENT WITH BUCKETS                              
* TOTAL SPAN OF K DATES AND BUCKETS CANNOT EXCEED 13 MONTHS                     
* GET BROADCAST MONTHS OF CONTRACT                                              
         SPACE 1                                                                
         LA    R3,BUCERR                                                        
         GOTO1 VGTBROAD,DMCB,(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 (RF),(R1),(1,WORK+6),WORK+24                                     
         GOTO1 DATCON,(R1),WORK+18,(3,DUB)    START MONTH                       
         GOTO1 (RF),(R1),WORK+30,(3,DUB+3)  END MONTH                           
         SR    R4,R4                                                            
         LA    R5,RCONELEM         FIRST ELEM                                   
ED15     CLI   0(R5),3             ORD BUCKET?                                  
         BE    ED20                                                             
         CLI   0(R5),4             INV BUCKET?                                  
         BE    ED20                                                             
ED18     IC    R4,1(R5)                                                         
         AR    R5,R4               NEXT ELEM                                    
         CLI   0(R5),0             LAST?                                        
         BE    ED25                                                             
         B     ED15                                                             
*                                                                               
ED20     CLC   2(2,R5),DUB         K START YR-MON                               
         BL    ERROR                                                            
* CHECK AGAINST END                                                             
         CLC   2(2,R5),DUB+3                                                    
         BNH   ED18                                                             
         B     ERROR                                                            
* NOW CHECK IF EARLIEST + 1 YEAR EXCEEDS 13 MONTHS                              
ED25     IC    R4,DUB              EARLIEST YEAR                                
         LA    R4,1(R4)                                                         
         STC   R4,DUB                                                           
         LA    R3,49               ERROR - DATES CANT EXCEED 1 CAL YR           
         CLC   DUB+3(2),DUB                                                     
         BH    ERROR                                                            
*                                                                               
         CLC   =C'FC',CONACT      CHANGE?                                       
         BE    FCASTBUK                                                         
         CLC   =C'FC',BUYACT      CHANGE?                                       
         BNE   STABUDED                                                         
         EJECT                                                                  
**********************************************************************          
*  FCASTBUK:  FOR FLIGHT DATE CHANGES (NOT NEW ADDS), CHECK                     
*       TO DETERMINE IF FORECAST BUCKETS MUST BE RESPREAD.                      
**********************************************************************          
FCASTBUK EQU   *                                                                
         GOTO1 =A(GENFCAST),DMCB,(RC),RR=Y                                      
         EJECT                                                                  
**********************************************************************          
*  VALIDATE STATION BUDGET                                                      
**********************************************************************          
STABUDED DS    0H                                                               
         TM    FORSBUDH+4,X'20'    BUDGET                                       
         BZ    STABUD05                                                         
         TM    FORSGOLH+4,X'20'    SHARE GOAL                                   
         BO    EPEN0100                                                         
                                                                                
STABUD05 DS    0H                                                               
         LA    R4,WORK2            BUILD NEW SAR ELEMENT                        
         USING RSARXEL,R4                                                       
*                                                                               
         XC    RSARXEL(200),RSARXEL                                             
*                                                                               
         LA    R6,RCONREC          START WITH EXISTING DATA IN ELEMENT          
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   STABUD10                                                         
         MVC   RSARXEL(RSARXLTH),0(R6)                                          
*                                                                               
STABUD10 DS    0H                                                               
         MVI   RSARXCO,X'12'                                                    
         MVI   RSARXLEN,RSARXLTH                                                
         GOTO1 DATCON,DMCB,(5,WORK),(3,RSARXLAD)                                
*                                  INSERT LAST ACTIVITY DATE (TODAY)            
         OI    RSARXFLG,X'18'      FLAG AS FORECAST                             
                                                                                
         XC    RSARXBGT,RSARXBGT   CLEAR BUDGET                                 
         XC    RSARXSHG,RSARXSHG   CLEAR SHARE GOAL                             
         NI    RSARXFLG,X'FF'-X'60' CLEAR 0 ENTRY INDICATORS                    
         LA    R2,FORSBUDH         BUDGET FIELD                                 
         LA    R3,2                MISSING                                      
         CLI   5(R2),0             ANY INPUT?                                   
         BE    ERROR               NO - MISSING                                 
                                                                                
         GOTO1 SCANNER,DMCB,(R2),IOAREA,0                                       
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         LA    R6,IOAREA                                                        
         TM    2(R6),X'80'                                                      
         BZ    ERROR               ERROR - NOT NUMERIC                          
         MVC   WORK(4),4(R6)       USE BINARY VALUE                             
         OC    WORK(4),WORK                                                     
         BNZ   EPEN0060                                                         
         OI    RSARXFLG,X'40'      ENTERED AS ZERO$                             
         EJECT                                                                  
**********************************************************************          
*  VALIDATE SHARE GOAL                                                          
**********************************************************************          
EPEN0060 EQU   *                                                                
         LA    R2,FORSGOLH         SHARE GOAL FIELD                             
         LA    R3,2                MISSING                                      
         CLI   5(R2),0             ANY INPUT?                                   
         BE    ERROR                                                            
                                                                                
         TM    RSARXFLG,X'40'      $0 BUDGET IMPLIES 0% SHARE                   
         BNZ   EPEN0065                                                         
                                                                                
         GOTO1 SCANNER,DMCB,(R2),IOAREA,0                                       
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         LA    R6,IOAREA                                                        
         TM    2(R6),X'80'                                                      
         BZ    ERROR               ERROR - NOT NUMERIC                          
         MVC   FULL,4(R6)          CHECK VALUE FOR 100%                         
         L     RF,FULL                                                          
         CH    RF,=H'100'          COMPARE FOR 100%                             
         BH    ERROR               EXCEEDS IT -                                 
         LTR   RF,RF                                                            
         BNZ   EPEN0068                                                         
                                                                                
EPEN0065 DS    0H                                                               
         OI    RSARXFLG,X'20'      0% SHARE GOAL ENTERED                        
         OI    RSARXFLG,X'40'      THIS IMPLIES $0 BUDGET                       
         B     EPEN0069                                                         
                                                                                
EPEN0068 DS    0H                                                               
         MVC   RSARXSHG,7(R6)      USE BINARY VALUE: LAST BYTE                  
*                                                                               
* CALCULATE MARKET BUDGET = STATION BUDGET / SHARE GOAL                         
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,WORK                                                       
         MH    RF,=H'100'                                                       
         XC    WORK,WORK                                                        
         MVC   WORK+3(1),RSARXSHG                                               
         D     RE,WORK                                                          
         STCM  RF,15,RSARXBGT                                                   
                                                                                
EPEN0069 DS    0H                                                               
         CLC   =C'ADD',CONACT                                                   
         BE    EPEN0070                                                         
                                                                                
         GOTO1 VDELELEM,DMCB,(X'12',RCONREC)                                    
                                                                                
EPEN0070 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,(R4)                                       
                                                                                
         GOTO1 =A(SPREDFOR),DMCB,(RC),(R4),RR=Y                                 
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*  VALIDATE COMMENTS                                                            
**********************************************************************          
EPEN0100 DS    0H                                                               
         CLC   =C'ADD',CONACT                                                   
         BE    EPEN0300                                                         
                                                                                
         GOTO1 VDELELEM,DMCB,(X'11',RCONREC)                                    
                                                                                
EPEN0300 DS    0H                                                               
         LA    R2,FORCMT1H         ADD COMMENTS                                 
         LA    R3,4                CHECK 4 COMMENT LINES                        
         MVI   HALF,C'N'           SET 'NO COMMENTS'                            
                                                                                
EPEN0330 CLI   5(R2),0                                                          
         BE    EPEN0340                                                         
                                                                                
         MVI   HALF,C'Y'           SET 'COMMENTS PRESENT'                       
         L     R8,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
         BZ    *+12                                                             
         L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK3+2(0),8(R2)    DATA TO ELEMENT                              
         AH    R1,=H'3'            ELEMENT LENGTH                               
         STC   R1,WORK3+1                                                       
         CLI   WORK3+1,62                                                       
         BNH   *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN!                           
         MVI   WORK3,X'11'                                                      
                                                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK3                                      
                                                                                
EPEN0340 ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT COMMENT LINE                            
         BCT   R3,EPEN0330                                                      
                                                                                
         LA    R6,RCONREC          UPDATE LAST ACTIVITY DATE                    
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   EPEN0350                                                         
                                                                                
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH                                                
         BL    EPEN0350                                                         
         GOTO1 DATCON,DMCB,(5,WORK),(3,RSARXLAD)                                
         DROP  R6                                                               
                                                                                
EPEN0350 DS    0H                                                               
         CLI   HALF,C'N'           ANY COMMENTS?                                
         BNE   ENDEDIT             YES                                          
         LA    R3,383              NO  - MUST BE PRESENT                        
         LA    R2,FORCMT1H         ADD COMMENTS                                 
         B     ERROR                                                            
         EJECT                                                                  
********************************************************************            
* VARIOUS POST SCREEN VALIDATION CHECKS                                         
********************************************************************            
ENDEDIT  DS    0H                                                               
         CLC   =C'FC',CONACT                                                    
         BE    END1                                                             
         CLC   =C'FC',BUYACT                                                    
         BNE   END5                                                             
                                                                                
END1     DS    0H                                                               
*        LA    RE,RBUYREC                                                       
*        LA    RE,1000(RE)                                                      
         L     RE,AIO2                                                          
         CLC   RCONKSTA,RCONKSTA-RCONREC(RE)                                    
         BE    END2                                                             
         LA    R3,SPLERR3                                                       
         LA    R2,CONSTAH                                                       
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    ERROR                                                            
* CHECK FOR DATE CHANGE                                                         
*END2     LA    RE,RBUYREC                                                      
*         LA    RE,1000(RE)         OLD CONREC                                  
END2     L     RE,AIO2             OLD CONREC                                   
         LA    RE,RCONDATE-RCONREC(RE)  OLD DATE                                
         CLC   RCONDATE(6),0(RE)                                                
         BE    END60                                                            
*                                                                               
* CHECK STATION INVOICE CLOSE MONTH - ADD 0 INV BUCKET IF MONTH CLOSED          
END5     OC    TWASTCDT,TWASTCDT   NO CLOSE DATE?                               
         BZ    END50                                                            
*                                                                               
* CHECK K START DATE                                                            
         CLC   RCONDATE(2),TWASTCDT                                             
         BH    END50                                                            
* CHECK MONTHS OF K                                                             
         GOTO1 DATCON,DMCB,(3,RCONDATE),WORK                                    
         GOTO1 (RF),(R1),(3,RCONDATE+3),WORK+6                                  
         XC    HALF,HALF                                                        
*                                                                               
* BUILD ZERO INVOICE BUCKET IN WORK2                                            
         MVC   WORK2(2),=X'040A'   ELEM CODE + LEN                              
         MVC   WORK2+4(2),MONDATE  ACTIVITY DATE                                
         XC    WORK2+6(4),WORK2+6                                               
*                                                                               
* GET BROADCAST MONTH                                                           
END10    GOTO1 VGTBROAD,(R1),(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 DATCON,(R1),WORK+18,(3,FULL)                                     
         CLC   FULL(2),TWASTCDT                                                 
         BH    END50                                                            
         OC    HALF,HALF      FIRST TIME?                                       
         BZ    END15                                                            
*                                                                               
         CLC   HALF,FULL           SAME MONTH?                                  
         BE    END40                                                            
*                                                                               
END15    MVC   HALF,FULL           SAVE                                         
         MVC   WORK2+2(2),FULL                                                  
* NEXT ELEM                                                                     
END30    EQU   *                   LOOK FOR AN INV EL FOR THIS MONTH            
         GOTO1 HELLO,DMCB,(C'G',=C'REPFILE'),(X'04',RCONREC),          X        
               (2,WORK2+2)                                                      
         CLI   DMCB+12,0           FOUND?                                       
         BE    END40               DON'T PUT DUP ELEM                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,RCONREC),(0,WORK2)              
* GET NEXT WEEK                                                                 
END40    EQU   *                                                                
         CLC   WORK(4),WORK+6                                                   
         BE    END50                                                            
         CLC   WORK2+2(2),TWASTCDT                                              
         BE    END50                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+24,7                                        
         MVC   WORK(6),WORK+24                                                  
         B     END10                                                            
*                                                                               
END50    CLC   =C'ADDF',CONACT                                                  
         BE    EDIT2                                                            
*                                                                               
*              CHANGE FUNCTION                                                  
END60    OI    RCONMODR,X'80'      K HEADLINE CHANGE CODE                       
         CLC   TODAY,RCONMODD                                                   
         BE    END100                                                           
         CLC   TODAY,RCONCREA                                                   
         BE    END100                                                           
         OI    TAREQ,1             T/A REQ IND FOR BASE                         
END100   DS    0H                                                               
* IF ACE/GRAPHNET & UPVER=1, UNCONFIRM CONTRACT & UP VERSION NUMBER             
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    EDIT2                                                            
* UPVER=0 IF ONLY CHANGE WAS TO CATG, RTG SRC OR TYPE                           
         CLI   UPVER,0                                                          
         BE    EDIT2                                                            
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    END230                                                           
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREV.                      
END230   OI    RCONCONF,X'80'      NOT CONFIRMED                                
         SPACE 1                                                                
         ZIC   R3,1(R6)            GET NEXT ELEMENT                             
         AR    R6,R3                                                            
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BE    END235                                                           
         DC    H'0',C'MISSING X20 - SEND ELEM'                                  
         USING RCONSEND,R6                                                      
         SPACE 1                                                                
* REP CAN'T MAKE CHANGE IF STATION IS IN MIDDLE OF CHANGES                      
         SPACE 1                                                                
END235   TM    RCONSENF,X'10'      X'10'=STA VERS NOT ADVANCED                  
         BO    *+12                                                             
         LA    R3,167              LATEST VERSION NOT YET SENT                  
         B     ERROR                                                            
         SPACE 1                                                                
         TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BZ    EDIT2                                                            
         DROP  R6                                                               
*                                                                               
* ADVANCE REP VERSION                                                           
* SAVE OFF LAST 3 VERSION DATES. IF OLD ELEMENT (LEN=30), CHANGE TO NEW         
*                                                                               
         MVC   WORK(4),HELLO       SAVE OFF VERSION DATE                        
         MVC   WORK+4(4),DATCON                                                 
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC),WORK                                
         BNZ   ERROR                                                            
         EJECT                                                                  
EDIT2    DS    0H                                                               
*              BUILD OLD POINTERS AT RBUYREC+2000                               
*              BUILD NEW POINTERS AT RBUYREC+2500                               
* 2K           BUILD OLD POINTERS AT IO3                                        
* 2K           BUILD NEW POINTERS AT IO3+500                                    
         SPACE 1                                                                
         CLC   =C'ADDF',CONACT                                                  
         BE    ADDCON                                                           
         CLC   =C'FC',CONACT                                                    
         BE    CE20                                                             
         CLC   =C'FC',BUYACT                                                    
         BNE   EXXMOD                                                           
         EJECT                                                                  
*              CHANGE CONTRACT                                                  
* CHECK FOR CHANGE TO ANY CONTRACT KEY                                          
CE20     DS    0H                                                               
* NEW                                                                           
*        LA    R6,RBUYREC                                                       
*        LA    R6,2500(R6)                                                      
         L     R6,AIO3                                                          
         LA    R6,500(R6)                                                       
         GOTO1 =A(PTRS),DMCB,(RC),(R6),0,RR=Y                                   
         GOTO1 VMOVEREC,DMCB,RCONREC,AIO4     SAVE NEW CONREC IN IO4            
* OLD                                                                           
*        LA    R8,RBUYREC                                                       
*        LA    R8,1000(R8)         OLD CONREC                                   
         L     R8,AIO2                                                          
         GOTO1 VMOVEREC,(R1),(R8),RCONREC   MOVE OLD CONREC TO RCONREC          
* BUILD LIST OF OLD PTRS                                                        
         SPACE 1                                                                
*        LA    R6,RBUYREC                                                       
*        LA    R6,2000(R6)                                                      
         L     R6,AIO3                                                          
         GOTO1 =A(PTRS),DMCB,(RC),(R6),1,RR=Y                                   
         GOTO1 VMOVEREC,(R1),AIO4,RCONREC   MOVE NEW CONREC BACK                
         SPACE 2                                                                
* IF BOP KEY HAS CHANGED (OTHER THAN DATE), PUT TODAY'S DATE                    
*    IN BOP ELEMENT                                                             
         SPACE 1                                                                
         L     R6,AIO3             OLD                                          
         LA    R8,500(R6)          NEW                                          
         LA    R4,7                ALLOW FOR 7 POINTERS                         
CE30     CLI   0(R6),X'DC'         BOP POINTER                                  
         BE    CE40                                                             
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         BCT   R4,CE30                                                          
         B     CE70                                                             
         SPACE 1                                                                
         USING RCONBTYP,R6         BOP POINTER                                  
CE40     CLC   RCONBTYP(11),0(R8)  IF ANYTHING EXCEPT DATE CHANGES,             
         BNE   CE50                 PUT TODAY IN BOP ELEMENT                    
         CLC   RCONBREF(13),RCONBREF-RCONBTYP(R8)                               
         BE    CE70                                                             
         DROP  R6                                                               
CE50     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   CE70                                                             
         USING RCONBPEL,R6                                                      
         MVC   RCONBPDT,TODAY                                                   
         DROP  R6                                                               
*                                                 RCONREC                       
* CHECK IF ANY KEY DIFFERENT                                                    
* DIVISION/TEAM/SALESMAN/CATEGORY CODES MAY CHANGE ANY TIME                     
* BOP KEY MAY CHANGE                                                            
         SPACE 1                                                                
*CE70     LA    R6,RBUYREC                                                      
*         LA    R6,2000(R6)         OLD                                         
CE70     L     R6,AIO3             OLD                                          
         LA    R8,500(R6)          NEW                                          
         SPACE 1                                                                
         USING RCONREC,R6                                                       
         CLC   RCONKSTA(5),RCONKSTA-RCONKTYP(R8)  CHANGED STATION               
         BNE   EEND100                                                          
         CLC   RCONKEY(27),0(R8)                                                
         BNE   C200                CHANGE DATA IN 0C KEY                        
         SPACE 1                                                                
         USING RCONPTYP,R6                                                      
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         CLC   RCONPTYP(27),0(R8)                                               
         BNE   C200                CHANGED THE NUMBER                           
         SPACE 1                                                                
         USING RCONQTYP,R6                                                      
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         CLC   RCONQTYP(27),0(R8)                                               
         BNE   C200                                                             
         SPACE 1                                                                
         USING RCONRTYP,R6                                                      
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         CLC   RCONRTYP(5),0(R8)   TEAM AND SALESMAN MAY CHANGE                 
         BNE   C200                                                             
         CLC   RCONRSTA(17),RCONRSTA-RCONRTYP(R8)                               
         BNE   C200                                                             
         SPACE 1                                                                
         USING RCONDTYP,R6                                                      
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         CLC   RCONDTYP(4),0(R8)   CATEGORY MAY CHANGE                          
         BNE   C200                                                             
         CLC   RCONDOFF(21),RCONDOFF-RCONDTYP(R8)                               
         BNE   C200                                                             
         SPACE 1                                                                
         USING RCONSTYP,R6                                                      
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         CLC   RCONSTYP(10),0(R8)  TEAM AND SALESMAN MAY CHANGE                 
         BNE   C200                                                             
         DROP  R6                                                               
         EJECT                                                                  
*  CAN ONLY ALLOW CALL LETTER CHANGE IF ACE/GRAPHNET STATUS IS                  
*  CONSISTENT BETWEEN THE OLD AND NEW.                                          
*                                                                               
EEND100  XC    KEY,KEY                                                          
*        LA    R3,RBUYREC                                                       
*        LA    R3,2000(R3)         OLD POINTERS                                 
         L     R3,AIO3             OLD POINTERS                                 
         LA    R8,500(R3)          NEW POINTERS                                 
*                                                                               
         LA    R3,RCONREC                                                       
         USING RCONREC,R3                                                       
*                                                                               
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA-RCONKTYP(R8)   STATION                        
         BAS   R5,GETSTA                                                        
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   EEND105                                                          
         SPACE 1                                                                
         OC    10(2,R6),10(R6)     NO RECVNG ID-NEW NOT ACE/GRAPHNET            
         BZ    EEND105                                                          
         CLC   10(2,R6),=X'0406'   NEW IS GRAPHNET                              
         BNE   EEND103                                                          
         TM    RCONMODR+1,X'40'    IS OLD GRAPHNET                              
         BZ    EEND107              NO, ERROR                                   
         B     C200                                                             
*                                                                               
EEND103  TM    RCONMODR+1,X'80'    IS OLD ACE                                   
         BZ    EEND107             NO, ERROR                                    
         B     C200                                                             
*                                                                               
EEND105  TM    RCONMODR+1,X'C0'    IS OLD ACE OR GRAPHNET                       
         BZ    C200                NO, OK                                       
EEND107  LA    R2,CONCACTH                                                      
         LA    R3,62               MUST CHANGE TO LIKE STATION                  
         B     ERROR                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
* ADD ANY CHANGED POINTERS                                                      
C200     DC    0H'0'                                                            
*        LA    R6,RBUYREC                                                       
*        LA    R6,2000(R6)          OLD                                         
         L     R6,AIO3              OLD                                         
         LA    R8,500(R6)          NEW                                          
         GOTO1 =A(ADDPTRS),DMCB,(RC),(R6),(R8),TWAKADDR,RR=Y                    
         SPACE 1                                                                
* PUT CHANGED RECORD                                                            
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
         B     EXXMOD                                                           
         SPACE 3                                                                
         TITLE 'ADD CONTRACT RECORD'                                            
ADDCON   DS    0H                                                               
         MVI   RCONKTYP,12         CONTRACT RECORD TYPE                         
         MVC   RCONKREP,REPALPHA   ALPHA REP CODE                               
         MVC   RCONCREA,TODAY      CREATION OR BUYLINE 1 ADDED DATE             
         MVC   RCONHDRD,TODAY      HEADER CREATION DATE (NEVER CHANGED)         
         TM    RCONMODR+1,X'C0'    ONLY ACE/GRAPHNET SET MOD TO -1              
         BZ    *+8                                                              
         MVI   RCONMOD,X'FF'                                                    
         OI    TAREQ,1             T/A REQ IND FOR BASE                         
*                                                                               
*              GET NEXT REP CONTRACT NUMBER                                     
         ZAP   WORK(5),=P'99999999'                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         CLC   CONACT,=C'ADDN'     TEST FOR ASSIGNED NUMBER ACTION              
         BNE   ADDCON2             NO                                           
         MVC   RCONKCON,INTCONNO   MOVE IN ASSIGNED NUMBER                      
         ZAP   WORK+10(5),=P'99999999'                                          
         MVO   WORK+10(5),INTCONNO FIND NINES COMPLEMENT OF                     
         SP    WORK+5(5),WORK+10(5) ASSIGNED NUMBER                             
         B     ADDCON3                                                          
         SPACE                                                                  
ADDCON2  XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           PASSIVE POINTER KEY TYPE                     
         MVC   KEY+21(2),REPALPHA  ALPHA REP CODE                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(23),KEYSAVE     SAME REP?                                    
         BNE   *+10                                                             
*              GET NEXT CONTRACT NUMBER                                         
         MVO   WORK+5(5),KEY+23(4) K NUMBER                                     
         SP    WORK(5),WORK+5(5)   GET POSITIVE                                 
         AP    WORK(5),=P'1'       NEXT K NUMBER                                
         MVO   WORK+10(5),WORK(5)                                               
         MVC   RCONKCON,WORK+10    TO K KEY                                     
                                                                                
ADDCON3  OC    TWAXCON,TWAXCON                                                  
         BNZ   *+10                                                             
         MVC   TWAXCON,RCONKCON                                                 
                                                                                
ADDCON10 DS    0H                                                               
*                                                                               
* FOR ACE/GRAPHNET CONTRACTS, MARK UNCONFIRMED, & BUILD X'20' SEND EL           
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    ADDCON70                                                         
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDCON15                                                         
         SPACE 1                                                                
         USING RCONXEL,R6          EXTENDED DESCRIPTION ELEMENT                 
         OI    RCONCONF,X'80'      UNCONFIRMED                                  
         TM    TWASTAST,X'02'      IS 'DON'T SEND' ALLOWED ON STATION           
         BZ    *+8                                                              
         OI    RCONSTAT,X'02'      YES                                          
         DROP  R6                                                               
         B     ADDCON20                                                         
         SPACE 1                                                                
ADDCON15 XC    WORK2,WORK2         BUILD NEW ELEM. IN WORK2                     
         MVC   WORK2(2),=X'1F18'                                                
         OI    WORK2+6,X'80'       UNCONFIRMED                                  
         TM    TWASTAST,X'02'      IS 'DON'T SEND' ALLOWED ON STATION           
         BZ    *+8                                                              
         OI    WORK2+7,X'02'       YES                                          
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         SPACE 1                                                                
ADDCON20 DC    0H'0'                ADD SEND ELEMENT                            
         XC    WORK2(RCONSNLQ),WORK2                                            
         MVI   WORK2,X'20'                                                      
         MVI   WORK2+1,RCONSNLQ    NEW ELEMENT LENGTH                           
*        MVC   WORK2(2),=X'201E'                                                
         MVI   WORK2+4,X'10'       START STA VERS. NOT ADVANCED                 
         MVI   WORK2+5,1           SET REP VERSION NUMBER TO 1                  
         MVI   WORK2+14,0          SET STATION VER. NUMBER TO 0                 
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         EJECT                                                                  
ADDCON70 DS    0H                                                               
         GOTO1 VADDREC,DMCB,RCONREC                                             
         SP    WORK+5(5),=P'1'                                                  
ADDCON75 MVO   WORK+15(5),WORK+5(5)                                             
         MVC   TWACNUM,WORK+15                                                  
         PACK  TWACNUM(1),WORK+18(1)    REVERSE THE COMPLIMENT                  
         PACK  TWACNUM+1(1),WORK+17(1)                                          
         PACK  TWACNUM+2(1),WORK+16(1)                                          
         PACK  TWACNUM+3(1),WORK+15(1)                                          
         MVC   TWACDTES,RCONDATE                                                
         MVC   TWAKADDR,KEY        SAVE CONTRACT DISK ADDRESS                   
* ADD PASSIVE PTRS                                                              
*        LA    R6,RBUYREC                                                       
*        LA    R6,2000(R6)         OLD                                          
         L     R6,AIO3             OLD                                          
         LR    RE,R6                                                            
         XCEF  (RE),500                                                         
         MVC   0(27,R6),RCONREC    DO NOT ADD MASTER POINTER                    
* BUILD PASSIVE PTRS                                                            
         LA    R8,500(R6)                                                       
         GOTO1 =A(PTRS),DMCB,(RC),(R8),0,RR=Y                                   
* ADD PTRS                                                                      
         GOTO1 =A(ADDPTRS),DMCB,(RC),(R6),(R8),TWAKADDR,RR=Y                    
*                                                                               
ADDCON80 DS    0H                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,CONCNUM),ALIGN=LEFT                                 
         FOUT  CONCNUMH                                                         
         OI    CONCNUMH+4,X'20'    SET PRE-VALID BIT...                         
         B     EXXMOD                                                           
*                                                                               
CHECK    TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK IF STATION HAS CLOSE OUT DATE.  IF TRUE, ADD X'04' INVOICE              
* ELEMENT TO CONTRACT BEFORE ADDREC.  OTHERWISE DELETE ALL X'04'                
* ELEMENTS (IF ADDED FROM A PREVIOUS COMBO).                                    
***********************************************************************         
CHKCLOUT NTR1                                                                   
         CLC   =C'ADD',CONACT      ONLY IN ACTION ADD!                          
         BNE   CKCOUTX                                                          
* DELETE ALL X'04' ELEMENTS (IF ADDED BY PRECEDING CONTRACT)                    
         GOTO1 VDELELEM,DMCB,(4,RCONREC)                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA  GET THE STATION RECORD USED BY THE           
         MVC   KEY+22(5),RCONKSTA  CONTRACT IN RCONREC                          
         BAS   R5,GETSTA                                                        
*                                                                               
         OC    RSTACLDT,RSTACLDT   CLOSE OUT DATE?                              
         BZ    CKCOUTX             IF NONE, EXIT                                
* CHECK K START DATE                                                            
         CLC   RCONDATE(2),RSTACLDT                                             
         BH    CKCOUTX                                                          
* CHECK MONTHS OF K                                                             
         GOTO1 DATCON,DMCB,(3,RCONDATE),WORK                                    
         GOTO1 (RF),(R1),(3,RCONDATE+3),WORK+6                                  
         XC    HALF,HALF                                                        
*                                                                               
* BUILD ZERO INVOICE BUCKET IN WORK2                                            
         MVC   WORK2(2),=X'040A'   ELEM CODE + LEN                              
         MVC   WORK2+4(2),MONDATE  ACTIVITY DATE                                
         XC    WORK2+6(4),WORK2+6                                               
*                                                                               
* GET BROADCAST MONTH                                                           
CKCOUT05 GOTO1 VGTBROAD,(R1),(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 DATCON,(R1),WORK+18,(3,FULL)                                     
         CLC   FULL(2),RSTACLDT                                                 
         BH    CKCOUTX                                                          
         OC    HALF,HALF      FIRST TIME?                                       
         BZ    CKCOUT10                                                         
*                                                                               
         CLC   HALF,FULL           SAME MONTH?                                  
         BE    CKCOUT20                                                         
*                                                                               
CKCOUT10 MVC   HALF,FULL           SAVE                                         
         MVC   WORK2+2(2),FULL                                                  
* NEXT ELEM                                                                     
*                                  LOOK FOR AN INV EL FOR THIS MONTH            
         GOTO1 HELLO,DMCB,(C'G',=C'REPFILE'),(X'04',RCONREC),          X        
               (2,WORK2+2)                                                      
         CLI   DMCB+12,0           FOUND?                                       
         BE    CKCOUT20            DON'T PUT DUP ELEM                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,RCONREC),(0,WORK2)              
* GET NEXT WEEK                                                                 
CKCOUT20 EQU   *                                                                
         CLC   WORK(4),WORK+6                                                   
         BE    CKCOUTX                                                          
         CLC   WORK2+2(2),RSTACLDT                                              
         BE    CKCOUTX                                                          
         GOTO1 ADDAY,(R1),WORK,WORK+24,7                                        
         MVC   WORK(6),WORK+24                                                  
         B     CKCOUT05                                                         
*                                                                               
CKCOUTX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* ROUTINE TO GET STATION RECORD                                                 
*                                                                               
GETSTA   GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA    GET STATION RECORD                        
         BAS   RE,CHECK                                                         
         BR    R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         ORG   CONSHRTH                                                         
       ++INCLUDE RECNTEBD                                                       
       ++INCLUDE REGLBRW                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    CL3                 BINARY MONTH START DATE                      
BRDEND   DS    CL3                 BINARY MONTH END   DATE                      
BRDWEEKS DS    CL1                 NUM WEEKS IN PERIOD                          
BRDLEN   EQU   *-BRDSTART          LENGTH OF ENTRY                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD POINTER LIST IN P1                                           
*  P2 - 0=NEW POINTER                                                           
*       1=OLD POINTER                                                           
***********************************************************************         
PTRS     CSECT                                                                  
         NMOD1 0,**PTRS**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R2,4(R1)                                                         
         LR    RE,R2                                                            
         L     R3,8(R1)                                                         
         XCEF  (RE),500                                                         
* BUILD ACTIVE PTR                                                              
         MVI   0(R2),X'0C'                                                      
         MVC   02(02,R2),REPALPHA                                               
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
         MVC   21(02,R2),REPALPHA                                               
         ZAP   DUB(5),=P'99999999'                                              
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         MVC   23(4,R2),WORK                                                    
         LA    R2,32(R2)                                                        
* CREATE PTR 2                                                                  
         MVI   0(R2),X'9C'                                                      
         MVC   02(02,R2),REPALPHA                                               
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
         MVC   01(2,R2),REPALPHA                                                
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
         MVC   02(02,R2),REPALPHA                                               
         MVC   04(02,R2),RCONCTGY                                               
         MVC   06(02,R2),RCONKOFF                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 5                                                                  
         MVI   0(R2),X'CC'                                                      
         MVC   01(02,R2),REPALPHA                                               
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
         MVC   5(2,R2),REPALPHA                                                 
         MVC   7(4,R2),RCONKADV                                                 
         MVC   11(3,R2),TODAY      NEW POINTER GETS TODAYS DATE                 
         CLC   CONACT(3),=C'ADD'                                                
         BE    DC20                                                             
         LTR   R3,R3               0=NEW POINTER                                
         BZ    DC20                                                             
         MVC   11(3,R2),RCONBPDT   OLD PTR NEEDS OLD BOP CHANGE DATE            
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
                                                                                
* END X'9D' PASSIVE POINTER                                                     
                                                                                
*                                                                               
         SPACE 1                                                                
PTREC    CLC   CONACT(3),=C'ADD'                                                
         BNE   PTRX                                                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   PTRX                NO SAR                                       
         SPACE 1                                                                
         USING RSAREL,R6                                                        
         MVI   0(R2),X'EC'                                                      
         MVC   21(2,R2),REPALPHA                                                
         MVC   23(4,R2),TWACNUM                                                 
PTRX     DS    0H                                                               
         XMOD1                                                                  
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD POINTERS TO FILE                                               
*              P1=A(OLD PTR LIST)                                               
*              P2=A(NEW PTR LIST)                                               
*              P3=A(DISK ADDR)                                                  
***********************************************************************         
ADDPTRS  CSECT                                                                  
         NMOD1 0,*ADDPTR*                                                       
         L     RC,0(R1)                                                         
         LM    R2,R4,4(R1)                                                      
*                                                                               
         MVI   DMOUTBTS,0          FOR PROPER RECOVERY                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
AP25     CLC   0(27,R2),0(R3)      SAME?                                        
         BE    AP100                                                            
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
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,APCHECK                                                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AP40                                                             
         MVI   KEY+27,X'FF'                                                     
         GOTO1 VWRITE                                                           
         BAS   RE,APCHECK                                                       
* ADD NEW PTR                                                                   
AP40     MVC   KEY,0(R3)                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,APCHECK                                                       
         CLC   KEY(27),KEYSAVE     KEY ALREADY THERE?                           
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     AP50                                                             
* UNDELETE OLD PTR                                                              
         MVI   KEY+27,0                                                         
         GOTO1 VWRITE                                                           
         BAS   RE,APCHECK                                                       
         B     AP100                                                            
* ADD PTR                                                                       
AP50     MVI   KEY+27,0                                                         
         MVC   KEY+28(4),0(R4)     DISK ADDR                                    
         GOTO1 VADD                                                             
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
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*                                                                   *           
* TYPCHG CHECKS TO SEE IF CHANGES ARE VALID UNDER SPOTPAK TRANSFER  *           
* RULES.                                                            *           
*                                                                   *           
* CKXFER CHECKS IF ANY BUYLINES ARE ATTACHED TO K                   *           
*                                                                   *           
* PRDLOCK DECREMENTS PREVIOUS PRD CODE LOCK COUNTER                 *           
*                                                                   *           
* TYPCHG AND CKXFER WILL RETURN WITH CC<>0 FOR AN ERROR             *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
TYPCHG   CSECT                                                                  
         NMOD1 0,*0210*                                                         
         L     RC,0(R1)                                                         
         TM    PROFILES+CNTTYPCB,CNTTYPCA                                       
         BZ    TCGOOD                                                           
*                                                                               
         CLI   RCONTYPE,C'X'                                                    
         BE    TC10                                                             
         CLI   RCONTYPE,C'N'                                                    
         BE    TC10                                                             
* TYPE NOT CURRENTLY N/X, CAN'T BECOME N/X IF BUYS                              
         CLI   8(R2),C'X'                                                       
         BE    TC50                                                             
         CLI   8(R2),C'N'                                                       
         BE    TC50                                                             
         B     TCGOOD                                                           
* TYPE IS CURRENTLY N/X, MUST STAY N/X IF BUYS                                  
TC10     CLI   8(R2),C'X'                                                       
         BE    TCGOOD                                                           
         CLI   8(R2),C'N'                                                       
         BE    TCGOOD                                                           
*                                                                               
TC50     XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM                                                 
         MVC   KEY,RBUYKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(22),KEYSAVE                                                  
         BNE   TCGOOD              NO BUYS - OK                                 
*                                                                               
TCBAD    LA    R0,1                                                             
         B     *+6                                                              
TCGOOD   SR    R0,R0                                                            
         LTR   R0,R0                                                            
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CKXFER CHECKS IF ANY BUYLINES ARE ATTACHED TO K                   *           
*********************************************************************           
*                                                                               
CKXFER   CSECT                                                                  
         NMOD1 0,*0210*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         CLI   RCONTYPE,C'N'                                                    
         BE    CKXFER10                                                         
         CLI   RCONTYPE,C'X'                                                    
         BNE   CXGOOD                                                           
*                                                                               
* GET FIRST BUY RECORD                                                          
CKXFER10 XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM                                                 
         MVC   KEY,RBUYKEY                                                      
         OI    DMINBTS,X'08'       READ DELETE/CANCEL BUYS IF ANY               
         GOTO1 VHIGH                                                            
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
CKXFER20 CLC   KEY(22),KEYSAVE                                                  
         BNE   CXGOOD              NO BUYS - FINE                               
*                                                                               
         OI    DMINBTS,X'08'       READ DELETE/CANCEL BUYS IF ANY               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         CLI   RBUYCHGI,C'X'       ONLY DELETED BUYS ARE ALLOWED                
         BNE   CXBAD                                                            
*                                                                               
         OI    DMINBTS,X'08'       READ DELETE/CANCEL BUYS IF ANY               
         GOTO1 VSEQ                                                             
         NI    DMINBTS,X'FF'-X'08' RESET                                        
         B     CKXFER20                                                         
*                                                                               
CXBAD    LA    R0,1                                                             
         B     *+6                                                              
CXGOOD   SR    R0,R0                                                            
         LTR   R0,R0                                                            
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* PRDLOCK DECREMENTS PREVIOUS PRD CODE LOCK COUNTER                 *           
*********************************************************************           
PRDLOCK  CSECT                                                                  
         NMOD1 0,*0210*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         CLC   SVPRD,MYSPACES      NO PRD CODE, DON'T BOTHER                    
         BE    PRDLXIT                                                          
*                                                                               
         XC    RPRDKEY,RPRDKEY     FIND PREVIOUS PRD REC SO WE                  
         MVI   RPRDKTYP,X'09'      CAN DECREMENT LOCK COUNTER                   
         MVC   RPRDKADV,SVADV      PREVIOUS ADVERTISER                          
         MVC   RPRDKPRD,SVPRD      PREVIOUS PRD CODE                            
         MVC   RPRDKREP,REPALPHA                                                
         MVC   KEY,RPRDKEY                                                      
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   PRDLXIT             IF NOT PRESENT, DON'T WORRY ABOUT IT         
*                                  MIGHT BE DELETED ALREADY                     
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RPRDREC                                             
         MVI   ELCODE,X'01'                                                     
         LA    R6,IOAREA                                                        
         BAS   RE,GETEL                                                         
         BNE   PRDLXIT                                                          
*                                                                               
         OC    RPRDLOCK,RPRDLOCK   ALREADY ZERO, DON'T SUBT.. COUNTER           
         BZ    PRDLXIT             MIGHT NOT BE IN SYNC, BUT LET IT GO          
         ZICM  RF,RPRDLOCK,2       DECREMENT LOCK COUNTER                       
         SH    RF,=H'1'                                                         
         STCM  RF,3,RPRDLOCK                                                    
         GOTO1 VPUTREC,DMCB,RPRDREC                                             
         B     PRDLXIT                                                          
*                                                                               
PRDLXIT  XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* STATION FIELD EDIT                                                            
**********************************************************************          
STAEDIT  CSECT                                                                  
         NMOD1 0,**STAE**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R2,CONSTAH                                                       
         LA    R3,STAERR                                                        
*                                                                               
         TM    4(R2),X'20'         READ STATION RECORD IF STATION               
         BZ    STAED009            1)NOT VALID                                  
         TM    CONSALH+4,X'20'                                                  
         BZ    STAED001            2)SALESPERSON NOT VALID                      
         TM    CONDTESH+4,X'20'                                                 
         BO    STAEDX             3)EFFECTIVE START/END DATES NOT VALID         
*                                                                               
* STATION VALID BUT WE NEED TO RE-READ IT FOR VALIDATION OF                     
* SALESPERSON AND/OR EFFECTIVE CONTRACT START/END DATES                         
*                                                                               
STAED001 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         BAS   R5,GETSTA                                                        
*                                                                               
         LA    R4,15               MAX 15 SETS OF OFF/TEAM PAIRS                
         LA    R5,TWASTOTC                                                      
*                                                                               
         MVC   TWASTJDT,RSTASTRT   SAVE OFF STATION JOIN DATE FOR               
*                                  VALIDATING EFFECTIVE DATES                   
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'04'        OFFICE/TEAM ELEMENT                          
         BAS   RE,GETEL                                                         
         USING RSTAOTEL,R6                                                      
         BNE   STAEDX                                                           
         B     STAED005                                                         
*                                                                               
STAED002 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   STAEDX                                                           
*                                                                               
STAED005 DS    0H                                                               
         MVC   0(4,R5),RSTAOTOF                                                 
         OC    0(4,R5),MYSPACES                                                 
         LA    R5,4(R5)                                                         
         BCT   R4,STAED002                                                      
         B     STAEDX                                                           
         DROP  R6                                                               
*                                                                               
STAED009 DS    0H                                                               
*                                                                               
STAED00C DS    0H                                                               
         MVI   UPVER,1    ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM            
         GOTO1 VMOVE                                                            
         XC    IOAREA(32),IOAREA                                                
         MVI   RSTAKTYP,2          STATION REC TYPE                             
         MVC   RSTAKREP,REPALPHA                                                
*                                                                               
         MVC   RSTAKSTA,WORK                                                    
         CLI   WORK+4,C'-'         BAND?                                        
         BNE   *+14                                                             
         MVC   RSTAKSTA+4(1),WORK+5                                             
         B     *+16                                                             
         CLI   WORK+3,C'-'                                                      
         BNE   *+8                                                              
         MVI   RSTAKSTA+3,C' '                                                  
*                                                                               
         CLI   RSTAKSTA+4,C'T'     TV?                                          
         BNE   *+8                                                              
         MVI   RSTAKSTA+4,C' '                                                  
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   TWASTAST,RSTASTAT                                                
         CLC   CONACT(3),=C'ADD'   FOR ACTION ADD ONLY                          
         BNE   STAED2                                                           
         TM    RSTASTAT,X'40'      TEST FOR STATION LOCKOUT                     
         BZ    STAED1                                                           
         LA    R3,348              STATION LOCKOUT                              
         B     ERROR                                                            
         SPACE 1                                                                
*   MARK CONTRACT AS ACE IF STATION RECORD HAS RECEIVING ID WHEN                
*   CONTRACT IS ADDED.                                                          
*   MARK IT GRAPHNET IF THAT RECEIVING ID IS 'GRAPH' (X'0406')                  
         SPACE 1                                                                
STAED1   DS    0H                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   STAED2              NO X'05' - NOT ACE/GRAPHNET                  
         OC    10(2,R6),10(R6)     IS THERE RECEIVING ID                        
         BZ    STAED2              NO                                           
         CLC   10(2,R6),=X'0406'   GRAPHNET?                                    
         BNE   *+12                                                             
         OI    RCONMODR+1,X'40'    YES,MARK CONTRACT GRAPHNET                   
         B     STAED2                                                           
         OI    RCONMODR+1,X'80'    OTHERWISE, IT'S ACE                          
         SPACE 1                                                                
*                                                                               
*- IF CONTRACT HAS ANY MONEY CALL LETTERS MAY NOT BE CHANGED.                   
STAED2   EQU   *                                                                
         CLC   =C'FC',CONACT                                                    
         BE    STAED10                                                          
         CLC   =C'FC',BUYACT                                                    
         BNE   STAED20                                                          
*                                                                               
STAED10  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    STAED15             ESTIMATE $'S                                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BE    STAED15             INVOICED $'S                                 
*                                                                               
         B     STAED20             NO MONEY.                                    
*                                                                               
STAED15  CLC   RCONKSTA,RSTAKSTA                                                
         BE    STAED20             SAME CALL LETTERS                            
*                                                                               
         LA    R3,349              MONEY ASSIGNED, NO CHANGE ALLOWED            
         B     ERROR                                                            
*                                                                               
STAED20  DS    0H                                                               
* SAVE OFF STATION OFF/TEAM CODES FOR VALIDATION OF SALESPERSON'S               
* OFF/TEAM CODE LATER ON                                                        
         LA    R4,15               MAX 15 SETS OF OFF/TEAM PAIRS                
         LA    R5,TWASTOTC                                                      
*                                                                               
         MVC   TWASTJDT,RSTASTRT   SAVE OFF STATION JOIN DATE FOR               
*                                  VALIDATING EFFECTIVE DATES                   
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'04'        OFFICE/TEAM ELEMENT                          
         BAS   RE,GETEL                                                         
         USING RSTAOTEL,R6                                                      
         BNE   STAED22                                                          
         B     STAED21A                                                         
*                                                                               
STAED21  DS     0H                                                              
         BAS   RE,NEXTEL                                                        
         BNE   STAED22                                                          
*                                                                               
STAED21A DS     0H                                                              
         MVC   0(4,R5),RSTAOTOF                                                 
         OC    0(4,R5),MYSPACES                                                 
         LA    R5,4(R5)                                                         
         BCT   R4,STAED21                                                       
         DROP  R6                                                               
*                                                                               
STAED22  DS    0H                                                               
         CLI   CONSTA+5,C'C'       SPECIAL FOR COMBO STATION                    
         BE    *+12                                                             
         CLI   CONSTA+4,C'C'       TEST IF 3 CHAR CALL LETTERS                  
         BNE   *+6                                                              
         DC    H'0'                COMBO NOT SUPPORTED BY ADDF/FC               
*                                                                               
STAED60  DS    0H                                                               
         MVC   WSTAEXP,RSTAMKT                                                  
         FOUT  CONSTAMH,RSTAMKT,20 MARKET                                       
         MVC   RCONKGRP,RSTAGRUP                                                
         MVC   RCONKSTA,RSTAKSTA   STATION TO CONTRACT KEY                      
         MVC   TWASTCDT,RSTACLDT   CLOSE DATE                                   
*                                                                               
STAEDX   DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   GENFCAST:  CHECK SAR ELEMENT.  IF NEW TYPE (LEN NOT = 120 CHARS)            
*        DELETE EXISTING FORECAST BUCKETS, AND GENERATE NEW ONES                
*        BECAUSE A FLIGHT DATE CHANGE HAS OCCURRED.                             
***********************************************************************         
GENFCAST CSECT                                                                  
         NMOD1 0,*GENF*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        FIND SAR ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   GENF0060            NO SAR ELEMENT                               
         LR    R4,R6               SET A(X'12' ELEMENT)                         
         USING RSARXEL,R4          ESTABLISH USING                              
         CLI   RSARXLEN,120        OLD STYLE ELEMENT?                           
*                                  NEW STYLE ELEMENT > 120 CHARS                
         BE    GENF0060            ORIGINAL: DON'T PROJECT                      
                                                                                
         GOTO1 =A(SPREDFOR),DMCB,(RC),(R4),RR=Y                                 
                                                                                
GENF0060 EQU   *                                                                
         XC    MYP,MYP             CLEAR PRINT AREA FOR OTHERS                  
         XMOD1                                                                  
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         TITLE 'T80211 - BUILD CONTRACT DISPLAY FIELDS'                         
**********************************************************************          
* DISPLAY CONTRACT                                                              
**********************************************************************          
DISCON   CSECT                                                                  
         NMOD1 0,*DISC*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)                                                         
                                                                                
         CLC   =C'CLRN',0(R2)      FOUT NON-SPACES                              
         BNE   DISP0005                                                         
         GOTO1 CLEAR,DMCB,1                                                     
*******TEMPORARY TRAP                                                           
          CLI   FORCMT2H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT3H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT4H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
*******                                                                         
         B     EXXMOD                                                           
                                                                                
DISP0005 DS    0H                                                               
         CLC   =C'CLRS',0(R2)      FOUT SPACES                                  
         BNE   DISP0010                                                         
         GOTO1 CLEAR,DMCB,0                                                     
*******TEMPORARY TRAP                                                           
          CLI   FORCMT2H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT3H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT4H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
*******                                                                         
         B     EXXMOD                                                           
                                                                                
DISP0010 DS    0H                                                               
         GOTO1 CLEAR,DMCB,0                                                     
                                                                                
*******TEMPORARY TRAP                                                           
          CLI   FORCMT2H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT3H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT4H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
*******                                                                         
*              BUILD AGENCY                                                     
DISP0020 XC    IOAREA(32),IOAREA                                                
         MVI   RAGYKTYP,10         AGENCY REC TYPE                              
         MVC   RAGYKAGY,RCONKAGY   AGY                                          
         MVC   RAGYKAOF,RCONKAOF   OFFICE                                       
         MVC   RAGYKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RAGYNAM1,WAGYEXP                                                 
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0120                                                         
*                                                                               
DISP0040 GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAAGNM1,RAGYNAM1                                                
         MVC   TWAAGNM2,RAGYNAM2                                                
*****>   MVC   TWAAGAD1,RAGYADD1                                                
*****>   MVC   TWAAGAD2,RAGYADD2                                                
         MVC   TWAAGSTT,RAGYSTAT                                                
         MVC   TWAAGZIP,RAGYZIP                                                 
         MVC   TWAAEASY,RAGYPRO2   PROFILE FOR EASYLINK AGY COPY                
         MVC   TWAARISK,RAGYRISK   AGENCY CREDIT RISK                           
         MVC   TWAALIAB,RAGYLIAB   AGENCY LIABILITY POSITION                    
*                                                                               
DISP0060 MVC   CONAGY(4),RAGYKAGY  AGENCY                                       
         CLC   RAGYKAOF,MYSPACES   OFFICE?                                      
         BE    DISP0080                                                         
         LA    RE,CONAGY                                                        
         MVI   CONAGY+4,C' '                                                    
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RAGYKAOF    AGENCY OFFICE                                
DISP0080 DS    0H                                                               
         MVC   CONAGYN,RAGYNAM1                                                 
*                                                                               
         XC    CONARSK,CONARSK     ALWAYS CLEAR CREDIT RISK CATEGORY            
         OC    TWAARISK,TWAARISK   NO RATING ASSIGNED, ASSUM OK                 
         BZ    DISP0100                                                         
         CLI   TWAARISK,1          RISK=OK, SHOW NOTHING                        
         BE    DISP0100                                                         
         MVC   CONARSK(9),=C'AGY RISK=' DISPLAY RISK                            
         LA    R4,CONARSK+9                                                     
         EDIT  TWAARISK,(1,(R4))                                                
DISP0100 OI    CONARSKH+6,X'80'    XMIT                                         
*                                                                               
DISP0120 DS    0H                  READ AGENCY PART 2 RECORD TO GET             
         XC    IOAREA(32),IOAREA   PHONE/FAX NUMBERS                            
         MVI   RAGK2TYP,RAGK2TYQ   AGENCY REC TYPE                              
         MVC   RAGK2AGY,RCONKAGY   AGY                                          
         MVC   RAGYKAOF,RCONKAOF   OFFICE                                       
         MVC   RAGYKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0160                                                         
*                                                                               
DISP0140 GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAAGYPH,RAGY2FON                                                
         MVC   TWAAFAX,RAGY2FAX                                                 
         EJECT                                                                  
*              BUYER                                                            
DISP0160 DS    0H                                                               
         OI    CONHBUYH+6,X'0C'    HIDE FIELD                                   
         OI    CONBUYH+6,X'2C'     PROTECT AND HIDE FIELD                       
         MVC   CONBUY,RCONBUYR                                                  
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DISP0180                                                         
*                                                                               
* FOR DDS TERMINALS, DISPLAY D/A OF K, NOT SALESMAN NAME                        
         MVC   CONBUY,MYSPACES                                                  
         MVC   CONBUY(4),=C'D/A='                                               
         GOTO1 HEXOUT,DMCB,TWAKADDR,CONBUY+5,L'TWAKADDR,=C'TOG'                 
*                                                                               
*                                                                               
*              ADVERTISER                                                       
DISP0180 XC    IOAREA(32),IOAREA                                                
         MVI   RADVKTYP,8                                                       
         MVC   RADVKADV,RCONKADV   ADVERTISER                                   
         MVC   RADVKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RADVNAME,WADVEXP                                                 
         OC    WADVEXP,WADVEXP                                                  
         BNZ   DISP0220                                                         
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0240                                                         
*                                                                               
DISP0200 GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
DISP0220 MVC   CONADV(4),RADVKADV                                               
         MVC   CONADVN,RADVNAME                                                 
         EJECT                                                                  
*              STATION                                                          
DISP0240 XC    IOAREA(32),IOAREA   BUILD STATION KEY                            
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         MVC   RSTAMKT,WSTAEXP                                                  
         OC    WSTAEXP,WSTAEXP                                                  
         BNZ   DISP0280                                                         
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VREAD                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWASTCDT,RSTACLDT   CLOSE DATE FOR INVOICES                      
         MVC   TWASTAST,RSTASTAT   STATION STATUS                               
                                                                                
         LA    RE,RSTAELEM                                                      
DISP0260 ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    DISP0280                                                         
         CLI   0(RE),X'08'                                                      
         BL    DISP0260                                                         
         BH    DISP0280                                                         
         MVC   TWAUNIST,7(RE)      UNI STATION STATUS                           
*                                                                               
DISP0280 DS    0H                                                               
         XC    TWAECON,TWAECON     DEFAULT IS NOT EC STATION                    
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'08'        FIND EXTRA DESCRIP ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DISP0300            NOT FOUND:  NOT EC STATION                   
                                                                                
         USING RSTAXXEL,R6                                                      
         TM    RSTAXOPT,X'80'      FOUND - EC STATION?                          
         BZ    DISP0300            NO  -                                        
         DROP  R6                                                               
                                                                                
         MVC   TWAECON,RSTATRAF    YES, GET TRAFFICE SYSTEM                     
                                                                                
DISP0300 DS    0H                                                               
         MVC   CONSTAM,RSTAMKT                                                  
*                                                                               
         MVC   WORK(4),RSTAKSTA                                                 
*                                                                               
         MVC   WORK+4(3),=C'-FM'                                                
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    DISP0320                                                         
*                                                                               
         MVI   WORK+5,C'A'                                                      
         CLI   RSTAKSTA+4,C'A'                                                  
         BE    DISP0320                                                         
*                                                                               
         MVI   WORK+5,C'C'                                                      
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    DISP0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'L '                                                 
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    DISP0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'TV'                                                 
DISP0320 CLI   WORK+3,C' '                                                      
         BNE   *+14                                                             
         MVC   WORK+3(3),WORK+4                                                 
         MVI   WORK+6,C' '                                                      
*                                                                               
         MVC   CONSTA(7),WORK      STATION                                      
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   DISP0380                                                         
         SPACE 1                                                                
*           IF STATION IS USER, CHECK THAT THE STATION IS                       
*           AUTHORIZED TO SEE THIS CONTRACT.                                    
         SPACE 1                                                                
         LA    R2,CONCNUMH                                                      
         LA    R3,55               ERROR, SECURITY LOCKOUT                      
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0360                                                         
         SPACE 1                                                                
         USING RSTASOEL,R6         STATION'S CONTRACTS                          
DISP0340 CLC   TWAUSRID,RSTASID                                                 
         BE    DISP0380                                                         
         BAS   RE,NEXTEL                                                        
         BE    DISP0340                                                         
         SPACE 1                                                                
         DROP  R6                                                               
DISP0360 DS    0H                                                               
         MVC   CONAGY,MYSPACES     BLANK OUT THE OTHER FIELDS                   
         MVC   CONAGYN,MYSPACES                                                 
         MVC   CONBUY,MYSPACES                                                  
         MVC   CONADV,MYSPACES                                                  
         MVC   CONADVN,MYSPACES                                                 
         MVC   CONSTA,MYSPACES                                                  
         MVC   CONSTAM,MYSPACES                                                 
         B     ERROR                                                            
         EJECT                                                                  
DISP0380 DS    0H                                                               
         OI    CONHPRDH+6,X'0C'    HIDE FIELD                                   
         OI    CONPRDH+6,X'2C'     PROTECT AND HIDE FIELD                       
         EJECT                                                                  
*              GET SALESMAN                                                     
DISP0460 XC    IOAREA(32),IOAREA                                                
         MVI   RSALKTYP,6          KEY TYPE - SALESMAN RECORD                   
         MVC   RSALKREP,REPALPHA   REP                                          
         MVC   RSALKSAL,RCONSAL    SALESMAN CODE                                
         MVC   KEY,IOAREA                                                       
         MVC   RSALNAME,WSALEXP                                                 
         OC    WSALEXP,WSALEXP                                                  
         BNZ   DISP0480                                                         
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWASALTL,RSALTEL                                                 
         MVC   TWASALFX,RSALFAX                                                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0480                                                         
         MVC   TWASALAS,22(R6)     SALES ASSISTANT                              
*                                                                               
DISP0480 DC    0H'0'                                                            
         MVC   CONSAL(3),RSALKSAL  SALESMAN CODE                                
         MVC   CONSALN,RSALNAME                                                 
         OI    CONHCATH+6,X'0C'    HIDE FIELD                                   
         OI    CONCATH+6,X'2C'     PROTECT AND HIDE FIELD                       
         MVC   CONCAT(2),RCONCTGY  CATEGORY                                     
         OC    CONCAT(2),CONCAT    TRANSITION?                                  
         BNZ   *+10                                                             
         MVC   CONCAT(2),=C'XX'                                                 
         EJECT                                                                  
*              OFFICE                                                           
         XC    IOAREA(32),IOAREA   BUILD OFFICE REC KEY                         
         MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,REPALPHA                                                
         MVC   ROFFKOFF,RCONKOFF                                                
         MVC   KEY,IOAREA                                                       
         MVC   ROFFNAME,WOFFEXP                                                 
         OC    WOFFEXP,WOFFEXP                                                  
         BNZ   DISP0500                                                         
*                                                                               
         GOTO1 VHIGH                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAOFAD1,ROFFADD1                                                
         MVC   TWAOFAD2,ROFFADD2                                                
         MVC   TWAOFSTT,ROFFSTT                                                 
         MVC   TWAOFZIP,ROFFZIP                                                 
*                                                                               
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    DISP0500                                                         
         CLC   TWAACCS(2),=C'O='   TEST FOR OFFICE RESTRICTION                  
         BNE   DISP0500                                                         
         TM    TWAAUTH,X'80'       TEST IF TERMINAL ALLOWED ACCESS              
         BO    DISP0500            TO ALL OFFICES                               
         CLC   ROFFKOFF,TWAACCS+2  ELSE, COMPARE OFFICES                        
         BE    DISP0500                                                         
         LA    R3,55               ERROR, SECURITY LOCKOUT                      
         MVC   CONAGY,MYSPACES     BLANK OUT THE OTHER FIELDS                   
         MVC   CONAGYN,MYSPACES                                                 
         MVC   CONBUY,MYSPACES                                                  
         MVC   CONADV,MYSPACES                                                  
         MVC   CONADVN,MYSPACES                                                 
         MVC   CONSTA,MYSPACES                                                  
         MVC   CONSTAM,MYSPACES                                                 
         MVC   CONPRD,MYSPACES                                                  
         MVC   CONCAT,MYSPACES                                                  
         MVC   CONSAL,MYSPACES                                                  
         MVC   CONSALN,MYSPACES                                                 
         LA    R2,CONSALH                                                       
         B     ERROR                                                            
*                                                                               
DISP0500 MVC   CONOFFN,ROFFNAME                                                 
*                                  READ OFFICE PART 2 REC FOR FAX #             
         XC    IOAREA(32),IOAREA   BUILD OFFICE REC KEY                         
         MVI   ROFF2TYP,ROFF2TYQ                                                
         MVC   ROFF2REP,REPALPHA                                                
         MVC   ROFF2OFF,RCONKOFF                                                
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0520                                                         
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAOFFFX,ROFF2FAX                                                
*                                                                               
DISP0520 DS    0H                                                               
         LA    R2,CONHDSPH         PROTECT AND HIDE FIELDS FROM DEV SAL         
         LA    RF,CONDCTNH         ALL THE WAY TO DEV TYPE                      
DISP0530 OI    6(R2),X'2C'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,RF                                                            
         BNH   DISP0530                                                         
*              K START DATE                                                     
DISP0620 GOTO1 DATCON,DMCB,(3,RCONDATE),(5,CONDTES)                             
*              K END DATE                                                       
         GOTO1 (RF),(R1),(3,RCONDATE+3),(5,CONDTES+9)                           
         MVI   CONDTES+8,C'-'                                                   
*                                                                               
* SET LENGTH OF DATES FOR RE-INPUT (ADDR)                                       
         MVI   CONDTESH+5,17                                                    
         EJECT                                                                  
*                                                                               
* DISPLAY SAR INFORMATION                                                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DSAR0030            SPL INFO NOT FOUND                           
                                                                                
         USING RSARXEL,R6                                                       
         LA    R8,FORSBUD          ADDRESS OF BUDGET FIELD                      
         MVC   FULL,RSARXBGT       DISPLAY BUDGET, IF ANY                       
         L     R2,FULL                                                          
         LTR   R2,R2               ANY VALUE HERE?                              
         BZ    DSAR0010            NO  - CHECK FOR 'ENTERED AS 0'               
*                                                                               
* STATION BUDGET = MARKET BUDGET * SHARE GOAL                                   
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,FULL                                                       
         XC    WORK,WORK                                                        
         MVC   WORK+1(1),RSARXSHG                                               
         MH    RF,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   WORK+3,100                                                       
         D     RE,WORK                                                          
         LR    R2,RF                                                            
         EDIT  (R2),(8,(R8)),ALIGN=LEFT                                         
         B     DSAR0015                                                         
DSAR0010 EQU   *                                                                
         TM    RSARXFLG,X'40'      BUDGET ENTERED AS ZERO?                      
         BNO   DSAR0015            NO                                           
         MVI   FORSBUD,C'0'        YES - SEND BACK A ZERO                       
DSAR0015 EQU   *                                                                
         CLI   RSARXLEN,120        OLD OR NEW ELEMENT?                          
         BNH   DSAR0020            OLD - NEXT FIELD DOESN'T EXIST               
         ZIC   R2,RSARXSHG         DISPLAY SHARE GOAL, IF ANY                   
         LTR   R2,R2               ANY VALUE HERE?                              
         BZ    DSAR0018            NO  - SKIP IT                                
         EDIT  (R2),(3,FORSGOL),ALIGN=LEFT                                      
         B     DSAR0020                                                         
DSAR0018 EQU   *                                                                
         TM    RSARXFLG,X'20'      SHARE GOAL ENTERED AS ZERO?                  
         BNO   DSAR0020            NO                                           
         MVI   FORSGOL,C'0'        YES - SEND BACK A ZERO                       
DSAR0020 EQU   *                                                                
         CLI   RSARXLEN,120        OLD OR NEW ELEMENT?                          
         BNH   DSAR0030            OLD - NEXT FIELD DOESN'T EXIST               
         OC    RSARXLAD,RSARXLAD   ANY LAST ACTIVITY DATE?                      
         BZ    DSAR0030            NO  - NOTHING TO DISPLAY                     
         GOTO1 DATCON,DMCB,(3,RSARXLAD),(5,FORLUPD)                             
         OI    FORLUPDH+6,X'80'    DISPLAY LAST ACTIVITY DATE                   
         DROP  R6                                                               
                                                                                
DSAR0030 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0800                                                         
         LA    R2,FORCMT1H                                                      
                                                                                
DSAR0040 MVC   WORK3(80),MYSPACES                                               
         ZIC   R3,1(R6)            GET L(COMMENT ELEMENT)                       
         SH    R3,=H'3'            MINUS CONTROL +1 FOR 'EX'                    
         EX    R3,*+8              MOVE IT BY LENGTH TO WORK3                   
         B     *+10                                                             
         MVC   WORK3(0),2(R6)                                                   
         SPACE 1                                                                
         ZIC   R1,0(R2)            GET L(SCREEN FIELD)                          
         SH    R1,=H'9'            MINUS FIELD HDR +1 FOR 'EX'                  
         BNP   DISP0800            NEGATIVE?  DON'T MOVE IT                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK3                                                    
         L     R8,AIO4             CHECK FOR STANDARD COMMENT                   
         GOTO1 VREGENSC,DMCB,(1,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
         BZ    *+10                                                             
         MVC   22(24,R2),=C'***CMNT REC NOT FOUND***'                           
*                                                                               
         ZIC   R1,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            BUMP TO NEXT COMMENT FIELD                   
         AR    R2,R1                                                            
         BAS   RE,NEXTEL           GO BACK AND CHECK IT                         
         BE    DSAR0040                                                         
                                                                                
* SET VALID FIELD BITS ON FOR ALL CONTRACT FIELDS                               
DISP0800 DS    0H                                                               
         LA    R4,CONAGYH                                                       
         LA    R5,FORLAST                                                       
         SR    RE,RE                                                            
         OI    4(R4),X'20'                                                      
         IC    RE,0(R4)            FIELD LEN                                    
         LA    R4,0(RE,R4)         NEXT FIELD                                   
         CR    R4,R5                                                            
         BL    *-14                                                             
         XC    TWALSTKY,TWALSTKY                                                
         XC    TWALSTPL,TWALSTPL                                                
                                                                                
*******TEMPORARY TRAP                                                           
          CLI   FORCMT2H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT3H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT4H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
*******                                                                         
         GOTO1 CLEAR,DMCB,1                                                     
*******TEMPORARY TRAP                                                           
          CLI   FORCMT2H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT3H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
          CLI   FORCMT4H+5,X'BC'                                                
          BL    *+6                                                             
          DC    H'0'                                                            
*******                                                                         
         MVC   TWACDTES,RCONDATE   SAVE K DATES                                 
         XMOD1                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO CLEAR CONTRACT HEADLINE FIELDS                                     
* ROUTINE WAS BORROWED FROM THE SAME IN RECNT00. SOME FIELDS IN THE             
* FORECAST SCREEN NEEDS TO BE HIDDEN AND PROTECTED THAT'S DIFFERENT             
* FROM THE ORIGINAL BASE SCREEN.                                                
* PARM 1   0 = FOUT MYSPACES                                                    
*          1 = FOUT NON-MYSPACES                                                
**********************************************************************          
CLEAR    NTR1                                                                   
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(79),WORK2                                                
         L     R3,0(R1)                                                         
         LA    R1,FLDLIST          SET A(CONTRACT HEADER FIELD LIST)            
                                                                                
CLEAR2   CLI   0(R1),X'FF'                                                      
         BE    CLEARX              END OF LIST                                  
                                                                                
CLEAR3   L     R2,0(R1)            LOAD A(FIELD)                                
         SLL   R2,8                STRIP OFF FIELD TYPE INDICATOR               
         SRL   R2,8                                                             
         AR    R2,RA               SET ADDRESSABILITY                           
         ZIC   RF,0(R2)            SET FIELD LENGTH                             
         SH    RF,=H'9'               - L(CONTROL) + 1 FOR EX STAT              
         TM    1(R2),X'02'         EXTENDED FLD HEADER?                         
         BZ    *+8                 NO                                           
         SH    RF,=H'8'            YES   - 1 FOR 'EX' LEN                       
         EX    RF,ORSPA                                                         
         EX    RF,COMSPA                                                        
         BE    CLEAR10             ALREADY MYSPACES                             
         LTR   R3,R3                                                            
         BP    CLEAR4              FOUT NON-MYSPACES                            
         EX    RF,MOVESPA                                                       
                                                                                
CLEAR4   FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         TM    1(R2),X'02'         EXTENDED FLD HEADER?                         
         BZ    *+8                 NO                                           
         SH    RF,=H'8'            YES - SHORTEN OUTPUT LEN                     
         STC   RF,7(R2)                                                         
         CLI   7(R2),X'BC'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
CLEAR10  DS    0H                  IF FIELD IS UNPROTECTED AND NOT              
         CLI   0(R1),C'N'            NEEDED FOR FORECAST                        
         BNE   CLEAR20                                                          
         OI    6(R2),X'2C'+X'80'   SET PROTECTED AND LOW INTENSITY              
         B     CLEAR30             AND XMIT                                     
                                                                                
CLEAR20  DS    0H                  IF FIELD IS PROTECTED AND NOT                
         CLI   0(R1),X'01'           NEEDED FOR FORECAST                        
         BNE   CLEAR30                                                          
         OI    6(R2),X'0C'+X'80'   SET TO LOW INTENSITY AND XMIT                
         B     CLEAR30                                                          
                                                                                
CLEAR30  DS    0H                                                               
         CLC   =C'FD',CONACT       IF FORECAST DISPLAY                          
         BE    CLEAR40             PROTECT EVERYTHING                           
         CLC   =C'FD',BUYACT       IF FORECAST DISPLAY                          
         BNE   CLEAR50             PROTECT EVERYTHING                           
                                                                                
CLEAR40  DS    0H                                                               
         OI    6(R2),X'20'+X'80'                                                
                                                                                
CLEAR50  DS    0H                                                               
         LA    R1,4(R1)                                                         
         B     CLEAR2                                                           
                                                                                
CLEARX   DS    0H                                                               
         B     EXXMOD                                                           
                                                                                
ORSPA    OC    8(0,R2),WORK2                                                    
COMSPA   CLC   8(0,R2),WORK2                                                    
MOVESPA  MVC   8(0,R2),WORK2                                                    
         EJECT                                                                  
**********************************************************************          
*   FLDLIST:  BYTE 1   =  FIELD TYPE                                            
*                       C'U'    =  UNPROTECTED FIELD                            
*                       C'N'    =  UNPROTECTED FIELD AND NOT NEEDED             
*                       X'00'   =  PROTECTED FIELD                              
*                       X'01'   =  PROTECTED FIELD AND NOT NEEDED               
*                       C'C'    =  COMMENT FIELD                                
*             BYTE 2-4  = ADDRESS OF FIELD IN TWA                               
* NOTE: NOT NEEDED FIELDS ARE SET TO LOW INTENSITY AND IF UNPROTECTED,          
*       THEY WILL BE SET PROTECTED                                              
**********************************************************************          
FLDLIST  DS    0F                                                               
         DC    X'01',AL3(CONHTYPH-TWATASK)                                      
         DC    C'N',AL3(CONTYPEH-TWATASK)                                       
         DC    X'01',AL3(CONMODH-TWATASK)                                       
         DC    C'U',AL3(CONAGYH-TWATASK)                                        
         DC    X'00',AL3(CONAGYNH-TWATASK)                                      
         DC    X'01',AL3(CONHBUYH-TWATASK)                                      
         DC    C'N',AL3(CONBUYH-TWATASK)                                        
         DC    C'U',AL3(CONADVH-TWATASK)                                        
         DC    X'00',AL3(CONADVNH-TWATASK)                                      
         DC    X'01',AL3(CONHPRDH-TWATASK)                                      
         DC    C'N',AL3(CONPRDH-TWATASK)                                        
         DC    X'01',AL3(CONHCATH-TWATASK)                                      
         DC    C'N',AL3(CONCATH-TWATASK)                                        
         DC    C'U',AL3(CONSTAH-TWATASK)                                        
         DC    X'00',AL3(CONSTAMH-TWATASK)                                      
         DC    C'U',AL3(CONDTESH-TWATASK)                                       
         DC    X'01',AL3(CONARSKH-TWATASK)                                      
         DC    C'U',AL3(CONSALH-TWATASK)                                        
         DC    X'00',AL3(CONSALNH-TWATASK)                                      
         DC    X'00',AL3(CONOFFNH-TWATASK)                                      
         DC    X'01',AL3(CONHDSPH-TWATASK)                                      
         DC    C'N',AL3(CONDSPH-TWATASK)                                        
         DC    X'01',AL3(CONDSPNH-TWATASK)                                      
         DC    X'01',AL3(CONHRTGH-TWATASK)                                      
         DC    C'N',AL3(CONRTGH-TWATASK)                                        
         DC    X'01',AL3(CONHIAVH-TWATASK)                                      
         DC    C'N',AL3(CONIADVH-TWATASK)                                       
         DC    X'01',AL3(CONHIPDH-TWATASK)                                      
         DC    C'N',AL3(CONIPRDH-TWATASK)                                       
         DC    X'01',AL3(CONHIETH-TWATASK)                                      
         DC    C'N',AL3(CONIESTH-TWATASK)                                       
         DC    X'01',AL3(CONHDCTH-TWATASK)                                      
         DC    C'N',AL3(CONDCTH-TWATASK)                                        
         DC    X'01',AL3(CONDCTNH-TWATASK)                                      
         DC    C'U',AL3(FORSBUDH-TWATASK)                                       
         DC    C'U',AL3(FORSGOLH-TWATASK)                                       
         DC    C'U',AL3(FORCMT1H-TWATASK)                                       
         DC    C'U',AL3(FORCMT2H-TWATASK)                                       
         DC    C'U',AL3(FORCMT3H-TWATASK)                                       
         DC    C'U',AL3(FORCMT4H-TWATASK)                                       
         DC    X'FF'                                                            
         EJECT                                                                  
MONTABLE DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
*                                                                               
RTGSTAB  DC    CL3'ARB'                                                         
         DC    CL3'NSI'                                                         
         DC    CL3'BIR'            BIRCH                                        
         DC    CL3'SRC'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   ROUTINE TO SPREAD FORECAST DOLLARS FROM SAR ELEMENT OVER FLIGHT             
*        OF ORDER.  CONTRACT FLIGHT DATES ARE USED, NOT SAR FLIGHT              
*        DATES.                                                                 
***********************************************************************         
         DS    0F                                                               
SPREDFOR CSECT                                                                  
         NMOD1 0,*FORE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            RESET A(SAR ELEMENT)                         
         USING RSARXEL,R4                                                       
*                                                                               
*   INITIALIZE WORKSPACE FOR FORECAST SPREADING....                             
*                                                                               
         XC    NEW23ELT,NEW23ELT   SET NEW ELEMENT                              
         MVC   NEW23ELT(2),=X'230A'                                             
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'23',RCONREC)                                    
*                                  DELETE EXISTING FORECAST BUCKETS             
         GOTO1 DATCON,DMCB,(5,WORK),(0,WORK)                                    
*                                  GET TODAY'S DATE EBCDIC                      
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
*                                  GET DAY OF WEEK OF TODAY'S DATE              
         ZIC   R2,DMCB             SAVE DAY OF WEEK RETURNED                    
         BCTR  R2,0                MAKE DAY OF WEEK ZERO/MONDAY REL             
         LNR   R2,R2               NEGATE THE VALUE                             
         SR    RF,R2               SUBTRACT MONDAY ADJUSTMENT                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,NEW23ELT+4)                            
*                                  INSERT IT INTO NEW 23 ELEMENT                
         BAS   RE,GENDAYS          GENERATE DAYTABLE                            
         BAS   RE,SPREDAYS         GENERATE DAYS WITHIN TABLE                   
SFOR0020 EQU   *                                                                
         SR    RF,RF                                                            
         LA    R2,DAYTABLE         ACCUMULATE TOTAL DAYS                        
SFOR0030 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SFOR0040            YES                                          
         ZIC   RE,3(R2)            TAKE DAYS FROM TABLE                         
         AR    RF,RE               ACCUMULATE                                   
         LA    R2,4(R2)            BUMP TO NEXT ENTRY                           
         B     SFOR0030            GO BACK FOR NEXT                             
SFOR0040 EQU   *                                                                
         ST    RF,TOTDAYS          TOTAL DAYS AT THIS POINT                     
         MVC   FULL,RSARXBGT       LOAD MARKET $$ BUDGET FIGURE                 
         L     RF,FULL                                                          
         ZIC   R2,RSARXSHG         ADJUST WITH SHARE GOAL                       
         MR    RE,R2               MULTIPLY MARKET $ BY SHARE GOAL              
*                                     GIVING STATION $$                         
*                                                                               
*   NOW MULTIPLY BY 10 FOR PROPER DECIMAL ALIGNMENT                             
*                                                                               
         M     RE,=F'10'           MULTIPLY BY 10                               
         L     R2,TOTDAYS          DIV STA $$ BY TOTAL DAYS                     
*                                     GIVING $$ PER DAY                         
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AR    RF,R2               ADD TOTDAYS FOR ROUNDING                     
         DR    RE,R2               DIVIDE BY TOTDAYS                            
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,TOTDAYS          SAVE $$ PER DAY                              
         LA    R2,DAYTABLE                                                      
SFOR0050 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SFOR0060            YES - FINISHED                               
         BAS   RE,GENBUCKS         NO  - GEN X'23' FORECAST BUCKET              
         LA    R2,4(R2)            BUMP TO NEXT BUCKET                          
         B     SFOR0050            GO BACK FOR NEXT                             
SFOR0060 EQU   *                                                                
         XC    MYP,MYP             CLEAR PRINT AREA FOR OTHER USERS             
         XMOD1                                                                  
         EJECT                                                                  
GENDAYS  NTR1                                                                   
         MVC   CYCLEDAT,RCONDATE   CONTRACT FLIGHT DATES                        
*                                                                               
*   EXTRA PRINT LINE IS USED TO SET UP BROADCAST MONTH ARRAY.                   
*                                                                               
         LA    R2,MYP              A(DAYTABLE)                                  
         XC    MYP,MYP             INITIALIZE TABLE                             
         USING BROADTBL,R2                                                      
GDAY0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,CYCLEDAT),(0,DAYTABLE)                            
*                                  CONVERT START DATE TO EBCDIC                 
GDAY0040 EQU   *                                                                
         GOTO1 VGTBROAD,DMCB,(1,DAYTABLE),DAYTABLE+6,GETDAY,ADDAY               
         MVC   BRDWEEKS,DMCB       INSERT NUMBER OF WEEKS                       
*                                  GET BROADCAST DATES FOR MONTH                
         CLI   DMCB,X'FF'          ERROR?                                       
         BNE   *+6                 NO                                           
         DC    H'0'                SHOULDN'T HAPPEN!!                           
         GOTO1 DATCON,DMCB,(0,DAYTABLE+6),(3,BRDSTART)                          
*                                  INSERT START DATE IN TABLE                   
         GOTO1 DATCON,DMCB,(0,DAYTABLE+12),(3,BRDEND)                           
*                                  INSERT END   DATE IN TABLE                   
         CLC   CYCLEDAT+3(3),BRDEND                                             
*                                  CONTRACT FLIGHT END REACHED?                 
         BNH   GDAY0060            YES                                          
         GOTO1 DATCON,DMCB,(3,BRDEND),(0,DAYTABLE+6)                            
*                                  CONVERT END DATE TO EBCDIC                   
         LA    RF,1                DATE INCREMENT                               
         GOTO1 ADDAY,DMCB,DAYTABLE+6,DAYTABLE,(RF)                              
*                                  GET NEXT DAY, WHICH IS FIRST                 
*                                     DAY OF NEXT BDCST MONTH                   
         LA    R2,BRDLEN(R2)       BUMP TO NEXT TABLE ENTRY                     
         B     GDAY0040            GO BACK, SET NEXT MONTH                      
GDAY0060 EQU   *                                                                
         XC    DAYTABLE(56),DAYTABLE     CLEAR THE WORKAREA                     
         LA    R2,MYP              RESET A(BDCST MONTH TABLE)                   
         LA    R3,DAYTABLE                                                      
GDAY0080 EQU   *                                                                
         CLI   BRDEND,0            ANY ENTRY?                                   
         BZ    GDAY0100            NO  - FINISHED                               
         MVC   0(2,R3),BRDEND      MOVE BDCST MON END (YM) TO TABLE             
         LA    R2,BRDLEN(R2)       BUMP TO NEXT BDCST MONTH                     
         LA    R3,4(R3)            BUMP TO NEXT DAYTABLE                        
         B     GDAY0080            GO BACK FOR NEXT                             
GDAY0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
SPREDAYS NTR1                                                                   
         LA    R2,DAYTABLE         A(DAYTABLE)                                  
         LA    R3,MYP              A(BDCST MONTH TABLE)                         
         USING BROADTBL,R3                                                      
         CLC   BRDSTART,RCONDATE   IS FLIGHT START FIRST DAY                    
*                                     OF FIRST BROADCAST MONTH?                 
         BE    SPDA0040            YES                                          
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK)                                
*                                  CONVERT FLIGHT START DATE                    
         CLC   RCONDATE+3(3),BRDEND                                             
*                                  IS FLIGHT END DATE EARLIER                   
*                                     THAN BROADCAST MONTH END DATE?            
         BNL   SPDA0020            NO  -                                        
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+6)                            
*                                  CONVERT FLIGHT END   DATE                    
*                                                                               
*   AT THIS POINT, BOTH FLIGHT START AND END ARE WITHIN THE FIRST               
*     BROADCAST MONTH, SO THAT THE NUMBER OF DAYS CALCULATION IS                
*     DONE FROM FLIGHT START TO FLIGHT END .                                    
*                                                                               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   DAYTABLE+2(2),DMCB+8                                             
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0100            EXIT ROUTINE                                 
*                                                                               
*   AT THIS POINT, FLIGHT START IS OTHER THAN BEGINNING OF BDCST                
*     MONTH, AND FLIGHT END IS EITHER AFTER THE BDCST MONTH, OR                 
*     THE LAST DAY OF THE MONTH.  NUMBER OF DAYS IS CALCULATED                  
*     FROM FLIGHT START TO BDCST MONTH END.                                     
*                                                                               
SPDA0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,BRDEND),(0,WORK+6)                                
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   DAYTABLE+2(2),DMCB+8                                             
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0080            FIRST ENTRY DONE                             
SPDA0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SPDA0100            YES                                          
         CLC   RCONDATE+3(3),BRDEND                                             
*                                  END OF FLIGHT REACHED?                       
         BL    SPDA0060            YES - PARTIAL MONTH TO DO                    
         ZIC   RF,BRDWEEKS         NO  - CALCULATE DAYS FROM WEEKS              
         SR    RE,RE                                                            
         LA    R1,7                                                             
         MR    RE,R1               MULT WEEKS BY 7                              
         STC   RF,3(R2)            INSERT # DAYS INTO TABLE                     
         B     SPDA0080            GO TO NEXT SLOT                              
SPDA0060 EQU   *                                                                
*                                                                               
*   AT THIS POINT, FLIGHT END IS OTHER THAN END OF BROADCAST                    
*     MONTH.  NUMBER OF DAYS IS CALCULATED FROM BROADCAST MONTH                 
*     START DATE THROUGH FLIGHT END.                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,BRDSTART),(0,WORK)                                
*                                  CONVERT BROADCAST MONTH START                
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+6)                            
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   2(2,R2),DMCB+8                                                   
*                                  MOVE NUM DAYS TO LAST TABLE NTRY             
         B     SPDA0100            FINISHED                                     
*                                                                               
SPDA0080 EQU   *                                                                
         LA    R2,4(R2)            BUMP DAYTABLE                                
         LA    R3,BRDLEN(R3)       BUMP BDCST MONTH TABLE                       
         B     SPDA0040            GO BACK FOR NEXT                             
SPDA0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
GENBUCKS NTR1                                                                   
         MVC   NEW23ELT+2(2),0(R2) INSERT MONTH INTO 23 ELT                     
         SR    RE,RE                                                            
         ZIC   RF,3(R2)            NUMBER OF DAYS FOR MONTH *                   
         M     RE,TOTDAYS             $$ PER DAY = $$ FOR MONTH                 
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AH    RF,=H'10'           ADD FOR ROUNDING                             
         D     RE,=F'10'           DIVIDE FOR DECIMAL SCALING                   
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,FULL                                                          
         MVC   NEW23ELT+6(4),FULL  INSERT INTO X'23' ELEMENT                    
         GOTO1 VADDELEM,DMCB,RCONREC,NEW23ELT                                   
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*DAYTABLE DS    14F                 FOUR BYTES PER MONTH                        
*                                  BYTES 1-2  =  DATE: YM                       
*                                  BYTES 3-4 =  # DAYS W/IN MONTH               
*                   .0.0.0.0.0.0.0.0.0.1                                        
*                   .1.2.3.4.5.6.7.8.9.0                                        
*NEW23ELT DC    XL10'230A0000000000000000'                                      
*TOTDAYS  DS    F                   TOTAL DAYS IN FLIGHT                        
*CYCLEDAT DS    CL6                 WORK SPACE FOR DATES                        
*DAYCOUNT DC    AL1(31)             JANUARY                                     
*         DC    AL1(28)             FEBRUARY                                    
*         DC    AL1(31)             MARCH                                       
*         DC    AL1(30)             APRIL                                       
*         DC    AL1(31)             MAY                                         
*         DC    AL1(30)             JUNE                                        
*         DC    AL1(31)             JULY                                        
*         DC    AL1(31)             AUGUST                                      
*         DC    AL1(30)             SEPTEMBER                                   
*         DC    AL1(31)             OCTOBER                                     
*         DC    AL1(30)             NOVEMBER                                    
*         DC    AL1(31)             DECEMBER                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039RECNT11   02/11/04'                                      
         END                                                                    
