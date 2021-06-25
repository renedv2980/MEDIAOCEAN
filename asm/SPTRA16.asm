*          DATA SET SPTRA16    AT LEVEL 081 AS OF 07/25/13                      
*PHASE T21616A                                                                  
*INCLUDE OFFOUT                                                                 
         TITLE 'T21616 MARKET/STATION BUY ASSIGN AND LIST'                      
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30) SHIPPING RECORD TO BE MAINTAINED         
*             AIO2 - USED FOR INSTR RECAP RECS IF BUY ACTIVITY OPTION           
*             AIO3 - USED FOR MARKET/STATION LIST IN VALREC RTN                 
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG - VR RTN PTR TO MKT/STA TABLE                            
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG -                                                        
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
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                               
*  LEV 08-09 APR15/86 GOT OUT OF GEND SPARE                                     
*  LEV 10    JAN08/87 ADD OFFICE PROFILE & DELETE TC DAYPART PROFILE            
*  LEV 11    FEB04/87 ADD COPY CODE = EST                                       
*  LEV 12    FEB11/87 COMBINE SVT1PR12/13 WITH SVPROF11                         
*  LEV 13    JUN01/87 ADD PASSIVE KEY FOR TBUYS                                 
*  LEV 14-15 JUL10/87 ADD BUY ACTIVITY FOR TBUYS                                
*  LEV 16    SEP04/87 FIXED BUGS FOR MULTI CLIENT REPORT                        
*  LEV 17-18 SEP16/87 FIXED BUGS FOR ONLINE LIST - SKIPPED BUYS                 
*                     BETWEEN SCREENS                                           
*  LEV 19    SEP21/87 FIXED BUGS FOR STA ERR-SV TWA/ALLOW STARTING MKT          
*  LEV 20-21 OCT01/87 USE MKTLIST ALL CLIENTS, ALLOW DATES FOR EST              
*  LEV 22    DEC03/87 PUT NEW LENGTH IN TRAPERH FOR ES DATES DISPLAY            
*  LEV 23    FEB26/88 ADD =MKT TO STARTING MKT FOR LIST FUNCTION                
*  LEV 24-27 JUN02/88 ADD MGRP FOR LIST FUNCTION                                
*  LEV 28    JUN06/88 CHANGE HEADINGS TO TBUY                                   
*  LEV 29    JUL07/88 PRINT ??? FOR UNKNOWN PROD                                
*  LEV 30    SEP08/88 CK FOR MORE THAN 1 CHAR ENTERED AS COPY CODE              
*  LEV 31-32 DEC22/88 ADD OFFICE FOR OFFLINE LIST                               
*  LEV 33    FEB09/89 FIX =MARKET FOR LIST FUNCTION                             
*  LEV 34    MAR03/89 FIX WRONG PRODUCT PRINTING IN HEADING                     
*  LEV 35-38 MAR19/89 ADD PROD EQUIV TO TBUY LIST ACTIVITY                      
*  LEV 39-40 APR07/89 SHOW EQUIV PRODS IN LIST HEADING IF ANY                   
*  LEV 41-42 MAY18/89 FIX EQUIV PRD ACT FILTER                                  
*  LEV 43-45 JUN06/89 SET OFF SVPROF13, NOT CLTPEQ                              
*  LEV 46    JUL07/89 FIX BUG - SPACES TO LISTAR                                
*  LEV 47-48 JUL13/89 ADD EXCLUDE CLIENT (TA PROFILE)                           
*  LEV 49    NOV17/89 FIX EXCLUDE CLIENT BUG - WILL NOT LIST CLT SPEC           
*  LEV 50    JAN12/90 FIX NOT SAVING TWA BUG/VPER ERR WITH EST        *         
*  LEV 51    JAN17/90 FIX USING SVTWA OFFLINE                         *         
*  LEV 52    MAR07/90 STOP PROG CK ON 1 DATE ENTRY, USE FOR END TOO   *         
*  LEV 53    JUL11/90 GIVE PRODUCT MISSING ERROR MSG FOR EST          *         
*  LEV 54    SEP18/90 ONLY DISPLAY 1 CHAR FOR TBYCODE                 *         
*  LEV 55    DEC20/90 ALLOW ENTRY OF ESTIMATE WITHOUT PROD FOR LIST   *         
*  LEV 56    APR30/91 READ DELETED KEYS IN VLST                       *         
*  LEV 57    JAN22/92 ADD TB PROFILE 4 TO ALLOW NO ESTIMATES          *         
*  LEV 58    JUN26/92 FIX BUG WHEN GOING FROM LIST TO ASSIGN          *         
*  LEV 59    SEP24/92 FIX BUG IN FINDING ANY EST WHEN USING FLIGHTS   *         
*  LEV 60    OCT15/92 FIX LOOP BUG ON EMPTY SCREEN & ERROR MSG        *         
*  LEV 61    DEC22/92 CHANGE FOR MSUNPK                               *         
*  LEV 62    MAR02/93 CHANGE CABLE HEAD                               *         
*  LEV 63    APR01/93 FIX FTRK                                        *         
*  LEV 64    MAY06/93 ADD NEW TRAFFIC SYSTEM                          *         
*  LEV 65    JUL09/93 CHANGE SVSPARE TO SVSPAREX                      *         
*  LEV 66    JUL29/93 ADD BACK VSWIT                                  *         
*  LEV 67    MAR08/94 FIX ACTIVITY OPTION FTRE BUG                    *         
*  LEV 68    APR07/94 TAKE OUT PROD FILTER, USER BPRD FROM KEY FIELD  *         
*                     ADD BUY ACTIVITY FOR DEALER TAGS                *         
*  LEV 69    JUL22/94 CHANGE TO FILENAME                              *         
*  LEV 70    JUL23/94 FIX    TO FILENAME                              *         
*  LEV 71    NOV06/96 CHANGE PRINTING FOR OFFICE CODE- SPRI           *         
*  LEV 72    DEC16/99 USE RECUP FROM FACPAK, FIX BAD R4 USING T216FFD *         
*  LEV 73    APR05/01 PUT IN TRAFFIC OFFICE                           *         
*  LEV 74    JUN25/02 RUN BY CLIENT FOR CLIENT STRING SECURITY        *         
*  LEV 75    NOV02/02 CHANGE INST RECAP RECORD                        *         
*  LEV 76    JAN27/03 FIX INST RECAP RECORD DATES BUG                 *         
*  LEV 77    AUG02/04 SOX                                             *         
*  LEV 78 SM SEP13/05 2 CHAR OFFICE CODE                              *         
*  LEV 80 MN APR11/12 FIX BUG IN ERROR MSG CAUSING DUMPS              *         
*  LEV 81 MN JUL25/13 BUG WHEN RESTORING DELETED TBUY RECORDS         *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21616   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21616**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR16RR                                                      
         L     R0,LSYSD                                                         
         AR    R0,R9                                                            
         LR    R1,R9                                                            
         AHI   R1,ENDSYSD-SYSD                                                  
         CR    R0,R1                                                            
         BNL   *+6                                                              
         DC    H'0'                USED MORE THAN SYSD                          
         LA    R0,STACT                                                         
         AHI   R0,6144                                                          
         CR    R0,R1                                                            
         BNL   *+6                                                              
         DC    H'0'                USED MORE THAN SYSD                          
         MVI   IOOPT,C'Y'                                                       
                                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* CHECK ANY CHANGED KEY FIELDS *                                                
                                                                                
VK       CLI   ACTNUM,ACTDEL       IS ACTION DELETE                             
         BE    DELERR              CAN ONLY DELETE FROM LIST                    
         CLI   ACTNUM,ACTADD       IS ACTION ADD                                
         BE    ADDERR                                                           
         TM    CONACTH+4,X'20'     ANY CHANGE IN ACTION                         
         BZ    VK04                 YES                                         
         TM    TRAMEDH+4,X'20'     ANY CHANGE                                   
         BZ    VK04                 YES                                         
         TM    TRACLTH+4,X'20'                                                  
         BZ    VK04                                                             
         TM    TRAPRDH+4,X'20'                                                  
         BZ    VK04                                                             
         TM    TRAPTRH+4,X'20'                                                  
         BZ    VK04                                                             
         TM    TRACODEH+4,X'20'                                                 
         BZ    VK04                                                             
         TM    TRAPERH+4,X'20'                                                  
         BZ    VK04                                                             
         TM    TRAFLTRH+4,X'20'                                                 
         BZ    VK04                                                             
                                                                                
         BAS   RE,RDTWA            RESTORE SAVED STORAGE                        
                                                                                
         CLI   ACTNUM,ACTLIST      IS ACTION LIST                               
         BE    EXIT                                                             
         B     VLST                VAL STARTING MKT, THEN ANY STATIONS          
         EJECT                                                                  
                                                                                
*        VALIDATE KEY ROUTINE *                                                 
                                                                                
VK04     XC    STACT(TABLE-STACT),STACT                                         
         MVI   PRTSW,C'N'                                                       
                                                                                
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK06                                                             
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK06                                                             
         GOTO1 VSOXERR                                                          
VK06     DS    0H                                                               
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
                                                                                
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         MVI   OFFCD,0                                                          
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK10                                                             
                                                                                
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK07                 NO                                          
                                                                                
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
                                                                                
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
                                                                                
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
                                                                                
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
                                                                                
         MVI   ERROR,0                                                          
                                                                                
VK07     TM    WHEN,X'80'          IF ONLINE LIST, CLIENT NEEDED                
         BZ    VK20                NO                                           
         B     MISSERR                                                          
                                                                                
VK10     CLI   8(R2),C'*'                                                       
         BE    VK14                                                             
         CLI   8(R2),C'$'                                                       
         BE    VK14                                                             
                                                                                
         GOTO1 VALICLT                                                          
                                                                                
         MVC   SVCLT,BCLT          SAVE FOR LIST WITH CLIENT SPECIFIC           
         B     VK20                                                             
                                                                                
VK14     DS    0H                                                               
                                                                                
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
                                                                                
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
                                                                                
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
                                                                                
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
                                                                                
         MVI   ERROR,0                                                          
                                                                                
         BRAS  RE,VOFF                                                          
         BNE   OFFERR                                                           
                                                                                
* GET PROFILE REC(S)                                                            
                                                                                
* READ T0 PROFILE *                                                             
                                                                                
VK20     XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
                                                                                
         MVC   WORK+11(1),SVCLTOFF                                              
                                                                                
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
                                                                                
* READ TB PROFILE *                                                             
                                                                                
         MVI   WORK+3,C'B'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVTBPR04,SVT1PROF+3                                              
                                                                                
* READ T0 PROFILE *                                                             
                                                                                
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   ORGPRF13,SVPROF13                                                
         MVI   SVPROF13,C'N'                                                    
         OI    4(R2),X'20'                                                      
                                                                                
         LA    R2,TRAPRDH          PRODUCT                                      
         XC    BPRD(2),BPRD                                                     
         XC    QPRD,QPRD                                                        
         CLI   5(R2),0                                                          
         BNE   VK24                                                             
         CLI   ACTNUM,ACTLIST      NOT REQUIRED FOR LISTS                       
         BE    VK30                                                             
         B     MISSERR                                                          
VK24     OC    BCLT,BCLT                                                        
         BZ    NOCLTERR                                                         
         GOTO1 VALIPRD                                                          
         CLC   WORK(3),=C'POL'     INVALID                                      
         BE    PRDERR                                                           
         MVC   QPRD,WORK                                                        
         CLI   WORK+4,0            VALID SPOT LENGTH                            
         BNE   VK26                                                             
         CLI   ACTNUM,ACTLIST      NOT REQUIRED FOR LISTS                       
         BE    VK26                                                             
         OI    6(R2),X'80'                                                      
         MVC   8(3,R2),QPRD                                                     
         LA    R1,10(,R2)                                                       
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVC   0(3,R1),=C'-30'                                                  
         MVI   WORK+4,30           DEFAULT FOR NO ENTRY                         
VK26     MVC   BPRD(2),WORK+3                                                   
VK30     OI    4(R2),X'20'                                                      
                                                                                
         LA    R2,TRAPTRH          PARTNER PRODUCT                              
         XC    BPRD2(2),BPRD2                                                   
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 VALIPRD                                                          
         CLC   WORK(3),=C'POL'     INVALID                                      
         BE    PRDERR                                                           
         MVC   QPRD2,WORK                                                       
         CLI   WORK+4,0            VALID SPOT LENGTH                            
         BNE   VK34                                                             
         CLI   ACTNUM,ACTLIST      NOT REQUIRED FOR LISTS                       
         BE    VK34                                                             
         OI    6(R2),X'80'                                                      
         MVC   8(3,R2),QPRD2                                                    
         LA    R1,10(,R2)                                                       
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVC   0(3,R1),=C'-30'                                                  
         MVI   WORK+4,30           DEFAULT FOR NO ENTRY                         
VK34     MVC   BPRD2(2),WORK+3                                                  
                                                                                
VK36     CLC   QPRD,QPRD2          ARE PRODUCTS IN SEQ                          
         BE    EQPRDER                                                          
         BH    PRDSEQER                                                         
                                                                                
VK40     OI    4(R2),X'20'                                                      
                                                                                
         BRAS  RE,VCC                                                           
                                                                                
         BRAS  RE,VPER                                                          
                                                                                
         XC    ELEMPTR(256),ELEMPTR                                             
         MVI   PRGSW,0             SET OFF TABLE BUILT AND ALL ELSE             
                                                                                
         BRAS  RE,VFTR                                                          
         EJECT                                                                  
* NOW BUILD KEY                                                                 
                                                                                
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING TBYKEY,R4                                                        
         MVC   TBYKID,=XL2'0A32'                                                
         MVC   TBYKAM(4),BAGYMD BCLT AND BPRD                                   
         XC    LASTKEY,LASTKEY                                                  
         XC    STMKT,STMKT                                                      
         MVI   STMKTSW,0                                                        
         NI    TRASMKTH+4,X'FF'-X'20' SET OFF VALIDATED                         
         OI    CONACTH+4,X'20'                                                  
                                                                                
         CLI   ACTNUM,ACTLIST      IF LIST, GENERIC KEY OK                      
         BNE   VK50                                                             
                                                                                
         B     EXIT                                                             
                                                                                
VK50     LA    R2,TRASMKTH                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK54                                                             
*                                                                               
         CLI   8(R2),C'0'          ALPHA (MARKET GROUP)                         
         BL    VK56                GO VALIDATE MARKET GROUP                     
*                                                                               
         GOTO1 VALIMKT                                                          
         MVC   STMKT,BMKT                                                       
         XC    TRASMKT,TRASMKT                                                  
         OI    TRASMKTH+6,X'80'                                                 
         OI    PRGSW,X'04'         SET ON NEW STARTING MARKET ENTERED           
VK54     OI    4(R2),X'20'                                                      
                                                                                
VK56     LA    RE,TABLE+1          TABLE ADDRESS                                
         LHI   RF,L'TABLE-1                                                     
         LA    R4,TABLE                                                         
         LA    R5,1                                                             
         MVI   TABLE,0                                                          
         MVCL  RE,R4               CLEAR TABLE                                  
                                                                                
         BAS   RE,BLST             GO BUILD LIST                                
                                                                                
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK60                                                             
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK60                                                             
         CLI   8(R2),C'0'          ALPHA (MARKET GROUP)                         
         BL    VMG                 GO VALIDATE MARKET GROUP                     
                                                                                
VK60     LA    R2,TRAMS1H                                                       
         BAS   RE,CLR              CLEAR ENTRY PART OF SCREEN                   
                                                                                
         BRAS  RE,DLST             GO DISPLAY LIST                              
                                                                                
         B     DOADDS                                                           
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE LIST FOR ANY NEW ENTRIES *                                           
                                                                                
VLST     LA    R2,TRASMKTH         OPTIONAL STARTING MARKET                     
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VLST10                                                           
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VLST06                                                           
*                                                                               
         CLI   8(R2),C'0'          ALPHA (MARKET GROUP)                         
         BL    VMG                 GO VALIDATE MARKET GROUP                     
*                                                                               
         GOTO1 VALIMKT                                                          
         MVC   STMKT,BMKT                                                       
         XC    TRASMKT,TRASMKT                                                  
         OI    TRASMKTH+6,X'80'                                                 
         OI    PRGSW,X'04'         SET ON NEW STARTING MARKET ENTERED           
VLST06   OI    4(R2),X'20'                                                      
                                                                                
* VALIDATE ANY NEW OR DELETED STATIONS *                                        
                                                                                
VLST10   LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING TBYKEY,R4                                                        
         MVC   TBYKID,=XL2'0A32'                                                
         MVC   TBYKAM(4),BAGYMD BCLT AND BPRD                                   
         MVC   SVKEY,KEY           AND SAVE                                     
         DROP  R4                                                               
         LA    R2,TRAMS1H          FIRST FLD                                    
                                                                                
         LA    R3,TABLE                                                         
         LH    R5,STACT                                                         
                                                                                
         BAS   RE,VMS              VALIDATE ALL MKT/STA AND BUILD LIST          
                                                                                
* UPDATE ANY NEW STATIONS                                                       
                                                                                
         LH    R5,STACT                                                         
         USING TABLED,R3                                                        
VLST20   TM    TABMFLG,X'80'       ALREADY UPDATED                              
         BO    VLST40              YES                                          
                                                                                
         LA    R6,ELEM                                                          
         USING TBYDTAEL,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   TBYDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   TBYDTALN,TBYDTAX-TBYDTAEL   LENGTH                               
         MVC   TBYSLN(3),BSLN      SPOT LEN, PARTNER/SPOT LEN                   
         MVC   TBYCODE,CODE        COPY CODE                                    
         MVC   TBYSTART(6),PERDTS  PERIOD START/END DATES                       
         MVC   KEY(L'SVKEY),SVKEY                                               
         MVC   KEY+6(5),TABMKST                                                 
         OI    DMINBTS,X'08'       READ DELETED                                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' SET OFF READ DELETED                         
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VLST30                                                           
         TM    KEY+13,X'80'        THIS DELETED                                 
         BZ    VLST22               NO                                          
         BAS   RE,UNDEL            GO UNDELETE KEY & PASSIVE KEY                
                                                                                
         OI    DMINBTS,X'08'       READ DELETED RECS                            
VLST22   GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08'                                              
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VLST24   BAS   RE,NEXTEL                                                        
         BNE   VLST32                                                           
         CLC   BSLN(3),TBYSLN      SAME SPOT LEN/PARTNER/SPOT LEN               
         BNE   VLST24              NO, BYPASS                                   
         CLC   CODE,TBYCODE        SAME CODE                                    
         BNE   VLST24              NO, BYPASS                                   
         TM    TABMFLG,X'08'       THIS A DELETE                                
         BO    VLST50              YES                                          
         CLC   TBYDTAEL(TBYDTAX-TBYDTAEL),ELEM IF ELEM ALREADY EXISTS           
*MN                                                                             
*        BNE   *+6                                                              
*        DC    H'0'                                                             
         BE    VLST33                                                           
*MN                                                                             
                                                                                
         CLC   PERST,TBYEND        CK OVERLAP                                   
         BH    VLST24              NO                                           
         CLC   PEREND,TBYSTART                                                  
         BL    VLST24                                                           
         B     DATOVLER                                                         
VLST30   TM    TABMFLG,X'08'       THIS A DELETE                                
         BO    VLST40              YES                                          
         XC    0(256,R6),0(R6)                                                  
         MVC   0(13,R6),KEYSAVE                                                 
         MVC   20(2,R6),AGENCY                                                  
         MVI   14(R6),24           EMPTY REC LEN                                
VLST32   GOTO1 ADDELEM                                                          
VLST33   CLC   KEY(13),KEYSAVE     IF KEY NOT FOUND, ADD REC                    
         BNE   VLST34                                                           
                                                                                
         OI    DMOUTBTS,X'08'                                                   
*MN                                                                             
         L     R6,AIO                                                           
         NI    15(R6),X'FF'-X'80'                                               
*MN                                                                             
         GOTO1 PUTREC                                                           
         NI    DMOUTBTS,X'FF'-X'08'                                             
         B     VLST36                                                           
VLST34   MVC   KEY,KEYSAVE                                                      
         GOTO1 ADDREC                                                           
                                                                                
* CREATE PASSIVE KEY                                                            
                                                                                
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                GET DISK ADDR OF ADDED REC                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,KEY                                                           
         USING TBYKEY,R4                                                        
         OI    TBYKID+1,X'80'                                                   
         MVC   TBYPPRD,TBYKPRD                                                  
         MVI   TBYKPRD,X'FF'                                                    
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'TRFDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
                                                                                
VLST36   OI    TABMFLG,X'80'       SET ON DISK EXISTS                           
                                                                                
VLST40   LA    R3,TABNEXT          NEXT ENTRY                                   
         BCT   R5,VLST20                                                        
                                                                                
         TM    PRGSW,X'80'         ANY DELETES                                  
         BZ    VLST44                                                           
         BRAS  RE,DTAB             GO DELETE FROM TABLE AND COUNTS              
                                                                                
VLST44   TM    PRGSW,X'01'         ADDING MARKET GROUPS                         
         BO    VMG40                                                            
                                                                                
         LA    R2,TRAMS1H                                                       
         BAS   RE,CLR              CLEAR ENTRY PART OF SCREEN                   
                                                                                
         BRAS  RE,DLST             GO DISPLAY LIST                              
                                                                                
         B     DOADDS                                                           
                                                                                
* DELETE STATION REALLY DELETES ONLY 1 ELEMENT *                                
                                                                                
VLST50   CLC   TBYDTAEL(TBYDTAX-TBYDTAEL),ELEM IF ELEM ALREADY EXISTS           
         BNE   VLST54                                                           
         GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         GOTO1 PUTREC                                                           
         OI    PRGSW,X'80'         SET ON DELETE NEEDED FROM TABLE              
         B     VLST40                                                           
VLST54   BAS   RE,NEXTEL                                                        
         BE    VLST50                                                           
         DC    H'0'                                                             
         EJECT                                                                  
* VALIDATE MARKET GROUP *                                                       
                                                                                
VMG      CLI   5(R2),2                                                          
         BL    MGRPERR                                                          
         CLI   5(R2),5                                                          
         BH    MGRPERR                                                          
         CLI   8(R2),C'A'                                                       
         BL    MGRPERR                                                          
         CLI   8(R2),C'Z'                                                       
         BH    MGRPERR                                                          
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         LA    R1,9(,R2)                                                        
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
VMG10    CLI   0(R1),C'0'                                                       
         BL    MGRPERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    MGRPERR                                                          
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   RF,VMG10                                                         
         PACK  WORK(3),DUB(5)                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+8(1),8(R2)                                                   
         MVC   KEY+9(2),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   NOMGRPER                                                         
         OI    TRASMKTH+4,X'20'                                                 
VMG14    MVC   LASTKEY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING STLKEY,R1                                                        
         MVC   STLKID,=X'0A31'                                                  
         MVC   STLKAM(3),BAGYMD                                                 
         MVC   STLKPRD,QPRD                                                     
         MVC   STLKMKT,LASTKEY+11                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     FIND THIS MARKET                             
         BE    VMG16                                                            
         MVC   KEY,KEYSAVE                                                      
         XC    STLKPRD,STLKPRD     TRY CLT, NOT PRD SPECIFIC                    
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   MGPMKTER                                                         
         MVC   BMKT,LASTKEY+11                                                  
VMG16    OI    PRGSW,01            SET ON DOING MARKET GROUP ADD                
         LA    R3,TABLE                                                         
         LH    R5,STACT                                                         
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    VMG22                                                            
         DC    H'0'                                                             
         USING STLDTAEL,R6                                                      
                                                                                
VMG20    BAS   RE,NEXTEL                                                        
VMG22    BNE   VMG30                                                            
                                                                                
* SEE IF EXISTS IN TABLE *                                                      
                                                                                
         MVC   BSTA,STLSTA         BMKT FILLED FROM LASTKEY+11                  
         LR    RE,R3                                                            
         LTR   RF,R5                                                            
         BZ    VMG26                                                            
VMG24    CLC   0(5,RE),BMKTSTA     ALREADY IN TABLE                             
         BE    VMG20               NEXT                                         
         LA    RE,TABNEXT-TABENT(,RE)                                           
         BCT   RF,VMG24                                                         
VMG26    MVC   TABMKST-TABENT(5,RE),BMKTSTA                                     
         MVI   TABMFLG-TABENT(RE),X'40'   INDICATE PART OF MARKET               
         LH    R1,STACT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,STACT                                                         
         LA    R5,1(,R5)                                                        
         B     VMG20                                                            
VMG30    GOTO1 XSORT,DMCB,(R3),(R5),L'TABENT,5,0                                
                                                                                
         LR    R1,R5                                                            
         LA    R5,1                                                             
VMG32    CLC   TABMKT,LASTKEY+11   AT THIS MARKET                               
         BE    VMG34                                                            
         LA    R3,TABNEXT                                                       
         LA    R5,1(,R5)                                                        
         BCT   R1,VMG32                                                         
         DC    H'0'                                                             
VMG34    SR    R0,R0                                                            
         ICM   R0,3,STACT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TRADSP(3),DUB                                                    
                                                                                
         LA    R2,TRADSP+21                                                     
         EDIT  (R5),(3,(R2))                                                    
         MVI   TRADSP+24,C'-'                                                   
         OI    TRADSPH+6,X'80'                                                  
         OC    STACT,STACT                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,TRAMS1H                                                       
         LA    R4,TRAMSAH                                                       
         BAS   RE,CLR              CLEAR ENTRY AREA OF SCREEN                   
                                                                                
         MVC   BMKTSTA,TABMKST                                                  
         BAS   RE,FMTSTA                                                        
         XC    WORK,WORK                                                        
         MVC   WORK(L'QMKT),QMKT                                                
         MVC   8(L'TRAMS1,R2),WORK                                              
         OI    1(R2),X'20'         PROTECT                                      
         OI    4(R2),X'20'         VALIDATED                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
VMG36    MVC   WORK(L'STAPRNT),STAPRNT                                          
                                                                                
         OC    STANET,STANET                                                    
         BZ    *+10                                                             
         MVC   WORK(L'STANET),STANET                                            
                                                                                
         CLC   8(L'TRAMS1,R2),WORK                                              
         BE    *+14                                                             
         MVC   8(L'TRAMS1,R2),WORK                                              
         OI    1(R2),X'20'         PROTECT                                      
         OI    4(R2),X'20'         VALIDATED                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R3,TABNEXT                                                       
         LA    R5,1(,R5)                                                        
         CLC   TABMKT,LASTKEY+11                                                
         BNE   VMG38                                                            
         MVC   BMKTSTA,TABMKST                                                  
         BAS   RE,FMTSTA                                                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R4,R2               END OF LINE                                  
         BH    VMG36               NO                                           
         CLI   0(R4),9            AT END OF SCREEN                              
         BNH   VMG38                                                            
         OI    1(R2),X'20'         PROTECT                                      
         OI    4(R2),X'20'         VALIDATED                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R2,TRAMS2H-TRAMS1H(,R4)                                          
         LA    R4,TRAMSAH-TRAMS1H(,R4)                                          
         B     VMG36                                                            
                                                                                
VMG38    BCTR  R5,0                                                             
         LA    R6,TRADSP+25                                                     
         EDIT  (R5),(3,(R6))                                                    
         MVC   TRADSP+28(10),=C' DISPLAYED'                                     
         OI    TRATAGH+1,X'01'     SET ON MODIFIED                              
         OI    TRATAGH+6,X'80'     AND TRANSMIT                                 
                                                                                
         ZIC   R0,0(R2)            POINT TO NEXT FIELD ON SCREEN                
         AR    R2,R0                                                            
                                                                                
         BAS   RE,SVTWA                                                         
                                                                                
         MVC   KEY(L'LASTKEY),LASTKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SEQ                                                              
         L     R1,=A(MKTMORMS)                                                  
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+8                                                              
         L     R1,=A(MKTLSTMS)                                                  
         A     R1,SPTR16RR                                                      
         MVC   CONHEAD,0(R1)                                                    
         B     ERREXITC                                                         
VMG40    MVC   KEY(L'LASTKEY),LASTKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(11),KEYSAVE                                                  
         BE    VMG14                                                            
                                                                                
* DONE WITH MARKET GROUP ADDS, BLANK FIELD,   *                                 
* SET OFF UPDATE PRGSW RESUME NORMAL ADD MODE *                                 
                                                                                
         XC    TRASMKT,TRASMKT                                                  
         OI    TRASMKTH+6,X'80'                                                 
         NI    PRGSW,X'FF'-X'01'   SET OFF DOING MARKET GROUP ADD               
         BAS   RE,SVTWA                                                         
         B     VLST44                                                           
         EJECT                                                                  
* BUILD TABLE OF TRAFFIC BUYS FOR THIS PERIOD *                                 
                                                                                
         DS    0H                                                               
BLST     NTR1                                                                   
         LA    R3,TABLE                                                         
         USING TABLED,R3                                                        
         LA    R4,KEY                                                           
         XC    STACT,STACT                                                      
         XC    KEY,KEY                                                          
         MVC   TBYKID-TBYKEY(,R4),=XL2'0A32'                                    
         MVC   TBYKAM-TBYKEY(4,R4),BAGYMD BCLT AND BPRD                         
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      ANYTHING FOR THIS A/M, CLT, PROD             
         BE    BLST12                                                           
         MVC   TRADSP(21),COUNTMSG                                              
         OI    TRADSPH+6,X'80'                                                  
         B     EXIT                                                             
BLST10   GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE      ANYTHING FOR THIS A/M, CLT, PROD             
         BNE   BLST28              NO, JUST CLEAR                               
BLST12   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     BLST22                                                           
BLST20   BAS   RE,NEXTEL                                                        
BLST22   BNE   BLST10              GET NEXT RECORD                              
         USING TBYDTAEL,R6                                                      
                                                                                
* CHECK PERIOD TO BE SAME *                                                     
                                                                                
         CLC   TBYSTART(6),PERDTS  SEE IF SAME PERIOD                           
         BNE   BLST20                                                           
         CLC   BSLN(3),TBYSLN      SAME SPOT LEN, PARTNER/SPOT LEN              
         BNE   BLST20              NO, BYPASS                                   
         CLC   CODE,TBYCODE        SAME CODE                                    
         BNE   BLST20              NO, BYPASS                                   
         MVC   TABMKST,TBYKMKT-TBYKEY(R4)                                       
         MVI   TABMFLG,X'80'       SET AS OLD ENTRY                             
         LH    R1,STACT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,STACT                                                         
         LA    R3,TABNEXT                                                       
         XC    TABENT,TABENT                                                    
         LA    R1,STACT                                                         
         AHI   R1,6144                                                          
         CR    R1,R3                                                            
         BH    BLST10                                                           
         DC    H'0'                                                             
BLST28   OI    PRGSW,X'08'         SET TABLE BUILT                              
         XC    TABPTR,TABPTR                                                    
         MVC   TABCTR,=X'0001'                                                  
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
                                                                                
LR       L     RE,AIO2             CLEAR ALL AIO2                               
         MVI   0(RE),0                                                          
         L     RF,SIZEIO                                                        
         LR    R1,RF                                                            
         LA    R0,1(,RE)                                                        
         MVCL  R0,RE                                                            
                                                                                
         LA    R2,TRASMKTH         OPTIONAL STARTING MARKET                     
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    LR08                                                             
         XC    STMKT,STMKT                                                      
         MVI   STMKTSW,0                                                        
         LA    RE,MGRPTABL                                                      
         LA    RF,L'MGRPTABL                                                    
         XCEFL                                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    LR08                 NO                                          
                                                                                
         CLI   8(R2),C'='          1 MARKET ONLY OPTION                         
         BNE   LR02                 GO VALIDATE MARKET                          
         MVI   STMKTSW,C'='                                                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,5(R2)                                                         
         MVC   8(L'TRASMKT-1,R2),9(R2)                                          
         MVI   TRASMKT+L'TRASMKT-1,0                                            
                                                                                
         MVC   WORK(L'TRASMKT),=6C'0'                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVN   WORK(0),8(R2)                                                    
         EX    R1,*+8              SEE IF NUMERIC                               
         B     *+10                                                             
         CLC   WORK(0),8(R2)                                                    
         BNE   *+8                                                              
         OI    4(R2),X'08'         SET ON NUMERIC                               
                                                                                
LR02     CLI   8(R2),C'0'          CK MGRP  OPTION                              
         BL    LR04                 GO VALIDATE MGRP                            
                                                                                
         GOTO1 VALIMKT                                                          
         MVC   STMKT,BMKT                                                       
         CLI   STMKTSW,C'='                                                     
         BE    *+14                                                             
         XC    TRASMKT,TRASMKT                                                  
         OI    TRASMKTH+6,X'80'                                                 
         B     LR06                                                             
                                                                                
LR04     BRAS  RE,VMGR                                                          
                                                                                
LR06     XC    KEY,KEY             FORCE NEW START                              
                                                                                
LR08     OI    4(R2),X'20'                                                      
                                                                                
         LA    R4,KEY                                                           
         USING STLKEY,R4                                                        
         OC    KEY,KEY             IN MIDDLE OF LIST                            
         BNZ   LR10                NO, GOTO HIGH                                
         LM    R0,R1,=A(HEADING,HDHK)      HEADING LINE FOR REPORT              
         A     R0,SPTR16RR                                                      
         ST    R0,SPECS            STORE FOR CONTROLLER                         
         A     R1,SPTR16RR                                                      
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         XC    BMKT,BMKT                                                        
                                                                                
* BUILD KEY, AND DO READHI                                                      
                                                                                
         USING TBYKEY,R4                                                        
LR09     MVC   TBYKID,=XL2'0A32'                                                
         MVC   TBYKAM(4),BAGYMD BCLT AND BPRD                                   
         MVC   TBYKMKT,STMKT                                                    
                                                                                
         CLI   OFFCD,0             THIS BY OFFICE                               
         BE    *+10                                                             
         MVC   TBYKCLT,OCLT                                                     
                                                                                
         GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BNE   EXIT                                                             
         CLI   OFFCD,0             THIS BY OFFICE                               
         BE    *+14                                                             
         CLC   TBYKCLT,OCLT                                                     
         BNE   LR26                                                             
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    LR24                                                             
         CLC   TBYKCLT,BCLT                                                     
         BNE   EXIT                                                             
         CLI   BPRD,0              WAS PROD ENTERED                             
         BE    LR24                                                             
         CLC   TBYKPRD,BPRD                                                     
         BE    LR24                                                             
         B     EXIT                                                             
LR10     CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* SEE IF WORKING IN RECORD                                                      
                                                                                
         MVC   KEY(L'LASTKEY),LASTKEY                                           
         OC    ELEMPTR,ELEMPTR                                                  
         BZ    LR14                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   BMKTSTA,TBYKMKT                                                  
         GOTO1 MSUNPK,DMCB,(X'80',BMKTSTA),QMKT,DUB                             
                                                                                
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES                                                  
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
                                                                                
         MVC   QSTA,DUB                                                         
         BAS   RE,FMKT                                                          
         LH    R6,ELEMPTR                                                       
         A     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         B     LRL30                                                            
LR14     GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
LR22     CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   LR60                YES                                          
         LA    R4,KEY                                                           
LR24     MVC   SVKEY(2),=XL2'0A32'                                              
         MVC   SVKEY+2(1),BAGYMD                                                
         CLC   SVKEY(3),KEY                                                     
         BH    LR20                                                             
         BL    LR60                                                             
         CLI   OFFCD,0             THIS BY OFFICE                               
         BE    LR28                                                             
         CLC   OCLT,KEY+3                                                       
         BE    LR28                                                             
LR26     BRAS  RE,NOFF              FIND NEXT CLT FOR OFFICE                    
         BNE   LR60                 ALL OF OFFICE DONE                          
         B     LR09                                                             
                                                                                
LR28     OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    LR30                                                             
         CLC   TBYKCLT,BCLT                                                     
         BNE   LR60                                                             
         CLI   BPRD,0              WAS PROD ENTERED                             
         BE    LR30                                                             
         CLC   TBYKPRD,BPRD                                                     
         BNE   LR60                                                             
LR30     OC    STMKT,STMKT         STARTING MARKET ENTERED                      
         BZ    LR34                                                             
         CLC   TBYKMKT,STMKT       UP TO THIS MARKET                            
         BL    LR20                                                             
         CLI   STMKTSW,C'='        ONLY THIS MKT                                
         BNE   *+18                 NO, STARTING ONLY                           
         CLC   TBYKMKT,STMKT                                                    
         BNE   LR20                                                             
         B     LR34                                                             
                                                                                
         XC    STMKT,STMKT                                                      
LR34     OC    MGRPTABL(2),MGRPTABL  ONLY ENTERED MARKET GROUPS                 
         BZ    LR38                   NONE                                      
         LA    R0,400                                                           
         LA    R1,MGRPTABL                                                      
LR36     CLC   TBYKMKT,0(R1)                                                    
         BE    LR38                                                             
         OC    0(2,R1),0(R1)       END OF TABLE                                 
         BZ    LR20                                                             
         LA    R1,2(,R1)                                                        
         BCT   R0,LR36                                                          
         B     LR20                                                             
                                                                                
LR38     BAS   RE,FTRK             GO FILTER ON KEY FIELDS                      
         BNE   LR20                                                             
         MVC   BMKTSTA,TBYKMKT                                                  
         GOTO1 MSUNPK,DMCB,(X'80',BMKTSTA),QMKT,DUB                             
                                                                                
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES                                                  
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
                                                                                
         MVC   QSTA,DUB                                                         
         CLC   SVMKT,TBYKMKT                                                    
         BE    LR40                                                             
         MVC   SVMKT,TBYKMKT                                                    
         BAS   RE,FMKT                                                          
LR40     CLC   SVCLT,TBYKCLT                                                    
         BE    LR44                                                             
         MVC   SVCLT,TBYKCLT                                                    
         BAS   RE,FCLT                                                          
         BE    LR22                BYPASS EXCLUDED CLIENTS                      
         MVI   FORCEHED,C'Y'                                                    
         B     LR46                                                             
LR44     CLC   SVPRD,TBYKPRD                                                    
         BE    LR50                                                             
LR46     MVC   SVPRD,TBYKPRD                                                    
         BAS   RE,FPRD                                                          
         MVI   FORCEHED,C'Y'                                                    
LR50     GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         LR    R4,R6                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR20                                                             
         USING TBYDTAEL,R6                                                      
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
         EJECT                                                                  
* END OF LIST, SET UP TO RESTART LIST *                                         
                                                                                
LR60     XC    LASTKEY,LASTKEY                                                  
         XC    ELEMPTR,ELEMPTR                                                  
         NI    TRAMEDH+4,X'FF'-X'20'                                            
         NI    TRACLTH+4,X'FF'-X'20'                                            
         NI    TRAPRDH+4,X'FF'-X'20'                                            
         NI    TRAPTRH+4,X'FF'-X'20'                                            
         NI    TRACODEH+4,X'FF'-X'20'                                           
         NI    TRAPERH+4,X'FF'-X'20'                                            
         NI    TRAFLTRH+4,X'FF'-X'20'                                           
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
                                                                                
LRL      MVC   LISTAR,SPACES                                                    
                                                                                
* CHECK DATES HERE                                                              
                                                                                
         CLC   PERST,TBYEND        SEE IF BEFORE PERIOD                         
         BH    LRL30                                                            
         CLC   PEREND,TBYSTART     SEE IF AFTER PERIOD                          
         BL    LRL30                                                            
         BAS   RE,FTRE             GO FILTER ON ELEMENT FIELDS                  
         BNE   LRL30                                                            
                                                                                
         CLI   BSLN,0              ANY SPOT LEN ENTERED                         
         BE    LRL12               NO                                           
         CLC   TBYSLN,BSLN                                                      
         BNE   LRL30                                                            
                                                                                
LRL12    CLI   BPRD2,0             PTR ENTERED                                  
         BE    LRL14               NO                                           
         CLC   TBYPRD2,BPRD2                                                    
         BNE   LRL30                                                            
                                                                                
LRL14    CLI   BSLN2,0             PTR SPOT LEN ENTERED                         
         BE    LRL16               NO                                           
         CLC   TBYSLN2,BSLN2                                                    
         BNE   LRL30                                                            
                                                                                
LRL16    CLI   CODE,0              ANY CODE ENTERED                             
         BE    LRL18               NO                                           
         CLC   TBYCODE,CODE                                                     
         BNE   LRL30                                                            
                                                                                
LRL18    MVC   LMKT,QMKT                                                        
         MVC   LMKTNM,MKTNM                                                     
         MVC   BSTA,TBYKSTA                                                     
         BAS   RE,FMTSTA                                                        
         MVC   LSTA,STAPRNT                                                     
                                                                                
         OC    STANET,STANET       THIS A CABLE HEAD STA                        
         BZ    *+10                                                             
         MVC   LSTA(8),STANET                                                   
                                                                                
         MVC   ELEM+10(1),TBYKPRD                                               
         MVC   ELEM+11(3),TBYSLN AND PRD2, SLN2                                 
         LA    R3,ELEM+10                                                       
         BAS   RE,PPRD                                                          
         MVC   LPRDSLN,ELEM                                                     
                                                                                
         OC    ELEM+12(2),ELEM+12  IS THERE A PTR                               
         BZ    LRL20                                                            
         LA    R3,ELEM+12                                                       
         BAS   RE,PPRD                                                          
         MVC   LPTRSLN,ELEM                                                     
LRL20    MVC   LCODE+1(1),TBYCODE                                               
         CLI   TBYCODE,0                                                        
         BE    LRL24                                                            
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   LRL24                                                            
         EDIT  (B1,TBYCODE),(3,LCODE),ALIGN=LEFT                                
                                                                                
LRL24    BAS   RE,PPER                                                          
         MVC   LPER(5),WORK                                                     
         MVC   LPER+5(9),WORK+8                                                 
         LR    R1,R6                                                            
         S     R1,AIO                                                           
         STH   R1,ELEMPTR                                                       
         MVC   LASTKEY,KEY                                                      
         BAS   RE,SVTWA                                                         
                                                                                
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
                                                                                
LRL30    BAS   RE,NEXTEL                                                        
         BE    LRL                                                              
         XC    ELEMPTR,ELEMPTR                                                  
         B     LR20                                                             
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE                                                    
                                                                                
LRR      MVC   P,SPACES                                                         
                                                                                
LRR10    BAS   RE,FTRE             GO FILTER ON ELEMENT FIELDS                  
         BNE   LRR30                                                            
                                                                                
         CLI   BSLN,0              ANY SPOT LEN ENTERED                         
         BE    LRR12               NO                                           
         CLC   TBYSLN,BSLN                                                      
         BNE   LRR30                                                            
                                                                                
LRR12    CLI   BPRD2,0             PTR ENTERED                                  
         BE    LRR14               NO                                           
         CLC   TBYPRD2,BPRD2                                                    
         BNE   LRR30                                                            
                                                                                
LRR14    CLI   BSLN2,0             PTR SPOT LEN ENTERED                         
         BE    LRR16               NO                                           
         CLC   TBYSLN2,BSLN2                                                    
         BNE   LRR30                                                            
                                                                                
LRR16    CLI   CODE,0              ANY CODE ENTERED                             
         BE    LRR18               NO                                           
         CLC   TBYCODE,CODE                                                     
         BNE   LRR30                                                            
                                                                                
LRR18    MVC   PMKTNM,MKTNM                                                     
         MVC   PMKT,QMKT                                                        
LRR20    BAS   RE,FMTSTA                                                        
         MVC   PSTA,STAPRNT                                                     
                                                                                
         OC    STANET,STANET       THIS A CABLE HEAD STATIONS                   
         BZ    *+10                                                             
         MVC   PSTA(8),STANET                                                   
                                                                                
* PRINT PRD-SLN, PTR-SLN HERE                                                   
         MVC   ELEM+10(1),TBYKPRD                                               
         MVC   ELEM+11(3),TBYSLN                                                
         LA    R3,ELEM+10                                                       
         BAS   RE,PPRD                                                          
         MVC   PPRDSLN,ELEM                                                     
         OC    ELEM+12(2),ELEM+12                                               
         BZ    LRR24                                                            
         LA    R3,ELEM+12                                                       
         BAS   RE,PPRD                                                          
         MVC   PPTRSLN,ELEM                                                     
LRR24    MVC   PCOPYCDE,TBYCODE                                                 
         CLI   TBYCODE,0                                                        
         BE    LRR26                                                            
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   LRR26                                                            
         EDIT  (B1,TBYCODE),(3,PCOPYCDE),ALIGN=LEFT                             
                                                                                
LRR26    BAS   RE,PPER                                                          
         MVC   PPERIOD,WORK                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   PRTSW,C'Y'                                                       
LRR30    BAS   RE,NEXTEL                                                        
         BE    LRR10                                                            
         CLI   PRTSW,C'Y'                                                       
         BNE   LRR40                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
LRR40    MVI   PRTSW,C'N'                                                       
         B     LR20                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* ROUTINE TO CLEAR LIST SCREEN                                                  
         EJECT                                                                  
* FORMAT MARKET/STATION FOR PRINTING                                            
                                                                                
         DS    0H                                                               
FMTSTA   NTR1                                                                   
         GOTO1 MSUNPK,DMCB,(X'80',BMKTSTA),QMKT,DUB                             
                                                                                
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES                                                  
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
                                                                                
         CLI   DUB+4,C' '                                                       
         BNE   *+8                                                              
         MVI   DUB+4,C'T'                                                       
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),DUB                                                   
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),DUB+4                                                    
         CLI   QMED,C'T'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C' '                                                       
*                                                                               
FMTSTAX  B     EXIT                                                             
                                                                                
* UNDELETE ACTIVE AND PASSIVE KEYS *                                            
                                                                                
         DS    0H                                                               
UNDEL    NTR1                                                                   
         NI    KEY+13,X'FF'-X'80'  SET OFF DELETE                               
         OI    DMOUTBTS,X'08'      WRITE DELETED RECS                           
         GOTO1 WRITE                                                            
         NI    DMOUTBTS,X'FF'-X'08'                                             
                                                                                
         MVC   SVKEY,KEY                                                        
         USING TBYKEY,R4                                                        
         OI    TBYKID+1,X'80'      SET FOR PASSIVE KEY                          
         MVC   TBYPPRD,TBYKPRD                                                  
         MVI   TBYKPRD,X'FF'                                                    
         OI    DMINBTS,X'08'       READ DELETED RECS                            
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    KEY+13,X'FF'-X'80'  SET OFF DELETE                               
         OI    DMOUTBTS,X'08'      WRITE DELETED RECS                           
         GOTO1 WRITE                                                            
         NI    DMOUTBTS,X'FF'-X'08'                                             
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R4                                                               
         EJECT                                                                  
* FILTER ON KEY FIELDS                                                          
                                                                                
         USING TBYKEY,R4                                                        
         DS    0H                                                               
FTRK     NTR1                                                                   
         MVC   CURRKEY,TBYKCLT     SAVE CURRENT KEY CLT/PRD/MKT/STA             
                                                                                
         CLI   BPRD,0              IS THERE A PRD FILTER                        
         BE    FTRK10               NO                                          
         CLC   BPRD,TBYKPRD                                                     
         BE    FTRK10                                                           
         B     FTRNO                                                            
                                                                                
FTRK10   OC    BMKTFTR,BMKTFTR     IS THERE A MARKET FILTER                     
         BZ    FTRK20               NO                                          
         CLI   MKTSFTR,0                                                        
         BNE   FTRK12                                                           
         CLC   BMKTFTR,TBYKMKT                                                  
         BNE   FTRNO                                                            
         B     FTRK20                                                           
FTRK12   CLI   MKTSFTR,X'4C'       LESS THAN                                    
         BE    FTRK14               YES, CK IT                                  
         CLI   MKTSFTR,X'6E'       GREATER THAN                                 
         BE    FTRK16                                                           
         DC    H'0'                                                             
FTRK14   CLC   BMKTFTR,TBYKMKT                                                  
         BL    FTRNO                                                            
         B     FTRK20                                                           
FTRK16   CLC   BMKTFTR,TBYKMKT                                                  
         BH    FTRNO                                                            
                                                                                
FTRK20   CR    R1,R1               SET COND CODE FILTERED OK                    
         B     EXIT                                                             
FTRNO    LTR   RB,RB               SET COND CODE NO FILTER                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* FILTER ON ELEMENT FIELDS                                                      
                                                                                
         USING TBYDTAEL,R6                                                      
FTRE     NTR1                                                                   
                                                                                
         CLC   PERST,TBYEND                                                     
         BH    FTRNO                                                            
         CLC   PEREND,TBYSTART                                                  
         BL    FTRNO                                                            
FTRE20   OC    SLNFTR,SLNFTR       SPOT LEN FILTER                              
         BZ    FTRE30              NO                                           
         CLC   SLNFTR,TBYSLN       DOES THIS MATCH                              
         BNE   FTRNO               NO                                           
                                                                                
FTRE30   OC    DATEFTR,DATEFTR     DATE FILTER                                  
         BZ    FTRE40                                                           
         OC    EDATEFTR,EDATEFTR                                                
         BNZ   FTRE36                                                           
         OC    DATESFTR,DATESFTR                                                
         BZ    FTRE34                                                           
         CLI   DATESFTR,X'4C'      GREATER THAN                                 
         BNE   FTRE32              MUST BE LESS THAN                            
         CLC   DATEFTR,TBYSTART    FILTER TO RECALL                             
         BNH   FTRNO               BYPASS                                       
         B     FTRE40              CK NEXT FILTER                               
FTRE32   CLC   DATEFTR,TBYSTART                                                 
         BNL   FTRNO                                                            
         B     FTRE40                                                           
FTRE34   CLC   DATEFTR,TBYSTART                                                 
         BL    FTRNO                                                            
         CLC   DATEFTR,TBYEND                                                   
         BH    FTRNO                                                            
         B     FTRE40                                                           
FTRE36   CLC   EDATEFTR,TBYSTART                                                
         BL    FTRNO                                                            
         CLC   DATEFTR,TBYEND                                                   
         BH    FTRNO                                                            
FTRE40   TM    PRGSW,X'20'         THIS BUY ACTIVITY                            
         BZ    FTRE60                                                           
         MVC   CKCOPY,TBYCODE                                                   
                                                                                
         DROP  R6                                                               
                                                                                
         LR    R5,R6                                                            
         USING TBYDTAEL,R5                                                      
         MVC   BLOCK(96),KEY                                                    
                                                                                
         MVC   EQPRD,CKPRD         SAVED AS CURRKEY                             
         CLI   ORGPRF13,C'Y'       THIS A PROD EQUIV CLT                        
         BNE   FTRE44                                                           
                                                                                
         LA    R0,EQPRD                                                         
         LA    R1,EQVPRD                                                        
         BAS   RE,GEP              CHANGE PROD IF NEEDED                        
                                                                                
FTRE44   LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING INSKEY,R4                                                        
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM,BAGYMD                                                    
         MVC   INSKCLT(8),CURRKEY  CLT/PRD/MKT/STA                              
         MVC   INSKPRD,EQPRD                                                    
         MVC   INSKCOPY,CKCOPY                                                  
                                                                                
         L     R6,AIO2                                                          
         CLC   0(13,R4),0(R6)                                                   
         BE    FTRE46                                                           
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FTRE56                                                           
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
                                                                                
FTRE46   MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FTRE50   BAS   RE,NEXTEL                                                        
         BNE   FTRE56                                                           
         USING INSDTAEL,R6                                                      
                                                                                
         CLI   ORGPRF13,C'Y'       THIS A PROD EQUIV CLT                        
         BNE   FTRE51                                                           
                                                                                
         CLI   TBYPRD2,0                                                        
         BE    FTRE51                                                           
                                                                                
         LA    R0,TBYPRD2                                                       
         LA    R1,EQVPRD2                                                       
         BAS   RE,GEP              CHANGE PROD IF NEEDED                        
                                                                                
FTRE51   CLC   TBYSLN(3),INSSLN1   CK SLN1, PRD2, SLN2                          
         BNE   FTRE50                                                           
                                                                                
         CLC   PERENDP,INSFTD      BEFORE FLIGHT START                          
         BL    FTRE50                                                           
         ZIC   RF,INSDTALN                                                      
         LA    RE,0(R6,RF)                                                      
         AHI   RE,-INSSUBEL                                                     
         CLC   PERSTP,5(RE)        AFTER FLIGHT END                             
         BH    FTRE50                                                           
                                                                                
         LA    R1,INSBSCEL(,R6)    START OF SUB-ELS                             
FTRE52   CLC   =X'FFFFFF',0(RE)    THIS A TBA                                   
         BNE   FTRE54                                                           
         AHI   RE,-INSSUBEL                                                     
         CR    RE,R1                                                            
         BL    FTRE50                                                           
         B     FTRE52                                                           
                                                                                
FTRE54   MVC   DUB(6),PERDTS                                                    
                                                                                
         CLC   TBYSTART,PERST                                                   
         BNH   *+10                                                             
         MVC   DUB(3),TBYSTART                                                  
                                                                                
         CLC   TBYEND,PEREND                                                    
         BNL   *+10                                                             
         MVC   DUB+3(3),TBYEND                                                  
                                                                                
         LR    R0,RE                                                            
         GOTO1 DATCON,DMCB,(3,DUB),(2,FULL)                                     
         GOTO1 (RF),(R1),(3,DUB+3),(2,FULL+2)                                   
                                                                                
         LR    RE,R0                                                            
                                                                                
         CLC   FULL(2),INSFTD      BEFORE FLIGHT START                          
         BL    FTRE50                                                           
         CLC   FULL+2(3),5(RE)     AFTER FLIGHT END                             
         BH    FTRE50                                                           
         MVC   KEY(96),BLOCK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         B     FTRNO                                                            
FTRE56   MVC   KEY(96),BLOCK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
FTRE60   CR    R1,R1               SET COND CODE FILTERED OK                    
         B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
* GET ANY EQUIVALENT PRODUCTS *                                                 
                                                                                
GEP      NTR1                                                                   
                                                                                
         LR    R2,R0               SOURCE BIN PROD                              
         LR    R3,R1               EQV PRD, BASE PRD                            
         CLC   0(1,R2),0(R3)                                                    
         BE    GEP40                                                            
                                                                                
         MVC   0(1,R3),0(R2)                                                    
         LA    R0,220                                                           
         L     R1,ASVCLIST                                                      
GEP10    CLC   0(1,R2),3(R1)       FIND EXPANDED PROD                           
         BE    GEP14                                                            
         LA    R1,4(,R1)                                                        
         CLI   0(R1),0                                                          
         BNH   *+8                                                              
         BCT   R0,GEP10                                                         
         DC    H'0'                                                             
GEP14    MVC   0(1,R3),3(R1)       SAVE SOURCE                                  
         MVI   1(R3),0             SET NO BASE                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PEQKEY,R4                                                        
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQPAM,BAGYMD                                                    
         MVC   PEQPCLT,CURRKEY                                                  
         MVC   PEQPEPRD,0(R1)                                                   
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A EQUIV PRODUCT                         
         BNE   EXIT                 NO, DONE                                    
                                                                                
         LA    R0,220                                                           
         L     R1,ASVCLIST                                                      
GEP20    CLC   PEQPBPRD,0(R1)      FIND EXPANDED BASE PROD                      
         BE    GEP30                                                            
         LA    R1,4(,R1)                                                        
         CLI   0(R1),0                                                          
         BNH   *+8                                                              
         BCT   R0,GEP20                                                         
         DC    H'0'                                                             
GEP30    MVC   0(1,R2),3(R1)       SAVE BASE PRD IN SOURCE                      
         MVC   1(1,R3),3(R1)        AND TABLE                                   
         B     EXIT                                                             
                                                                                
GEP40    CLI   1(R3),0             ANY EQUIV PRD                                
         BE    EXIT                 NO                                          
         MVC   0(1,R2),1(R3)       SUB EQUIV PRD WITH BASE                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE MARKET/STATION LIST                                                  
                                                                                
         DS    0H                                                               
VMS      NTR1                                                                   
         USING TABLED,R3                                                        
                                                                                
VMS10    CLI   5(R2),0             ANY ENTRY                                    
         BE    VMS34               NO                                           
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VMS34                                                            
                                                                                
         TM    4(R2),X'08'         IF NUMERIC                                   
         BO    VMS16               ASSUME MARKET                                
                                                                                
         XC    ELEMPTR,ELEMPTR                                                  
         CLC   =C'D-',8(R2)        THIS A DELETE                                
         BNE   VMS11                                                            
                                                                                
         MVC   ELEMPTR,8(R2)                                                    
         XC    WORK,WORK                                                        
         MVC   WORK(L'TRAMS1),8(R2)                                             
         MVC   8(L'TRAMS1,R2),WORK+2                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         STC   R1,5(R2)                                                         
                                                                                
VMS11    MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALISTA                                                          
         MVI   ERROPT,C'N'                                                      
         CLI   ERROR,0             IF ERROR                                     
         BNE   SVTRAPER             GO SAVE TWA AND EXIT                        
                                                                                
         OC    ELEMPTR,ELEMPTR     WAS THIS A DELETE                            
         BNZ   VMS40                                                            
                                                                                
* SEE IF EXISTS IN TABLE *                                                      
                                                                                
         LR    RE,R3                                                            
         LTR   RF,R5                                                            
         BZ    VMS14                                                            
VMS12    CLC   0(5,RE),BMKTSTA                                                  
         BE    DUPSTAER                                                         
         LA    RE,TABNEXT-TABENT(,RE)                                           
         BCT   RF,VMS12                                                         
VMS14    MVC   TABMKST-TABENT(,RE),BMKTSTA                                      
         MVI   TABMFLG-TABENT(RE),0                                             
         LH    R1,STACT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,STACT                                                         
         LA    R5,1(,R5)                                                        
         OI    4(R2),X'20'         SET ON VALIDATED                             
         B     VMS30                                                            
                                                                                
VMS16    GOTO1 VALIMKT                                                          
                                                                                
* NOW MUST READ MARKET RECORD WITH LIST OF STATIONS                             
                                                                                
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING STLKEY,R4                                                        
         MVC   STLKID,=XL2'0A31'                                                
         MVC   STLKAM(3),BAGYMD AND BCLT                                        
         MVC   STLKPRD,QPRD        MOVE PRODUCT                                 
         MVC   STLKMKT,BMKT        MOVE STATION                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VMS18                                                            
         MVC   KEY,KEYSAVE                                                      
         XC    STLKPRD,STLKPRD     TRY CLT, NOT PRD SPECIFIC                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VMS18                                                            
         MVC   KEY,KEYSAVE                                                      
         XC    STLKCLT,STLKCLT     TRY ALL CLIENT, NOT CLT SPECIFIC             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOMKTST                                                          
                                                                                
VMS18    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    VMS22                                                            
         DC    H'0'                                                             
         USING STLDTAEL,R6                                                      
                                                                                
VMS20    BAS   RE,NEXTEL                                                        
VMS22    BNE   VMS34                                                            
                                                                                
* SEE IF EXISTS IN TABLE *                                                      
                                                                                
         MVC   BSTA,STLSTA         BMKT FILLED IN BY VALIMKT                    
         LR    RE,R3                                                            
         LTR   RF,R5                                                            
         BZ    VMS26                                                            
VMS24    CLC   0(5,RE),BMKTSTA                                                  
         BE    DUPMSER1                                                         
         LA    RE,TABNEXT-TABENT(,RE)                                           
         BCT   RF,VMS24                                                         
VMS26    MVC   TABMKST-TABENT(5,RE),BMKTSTA                                     
         MVI   TABMFLG-TABENT(RE),X'40'   INDICATE PART OF MARKET               
         LH    R1,STACT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,STACT                                                         
         LA    R5,1(,R5)                                                        
         B     VMS20                                                            
VMS30    LH    R5,STACT                                                         
         GOTO1 XSORT,DMCB,(R3),(R5),L'TABENT,5,0                                
VMS34    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    VMS10                                                            
         OC    STACT,STACT         WERE THERE ANY ENTRIES                       
         BZ    NOENTER                                                          
         LH    R5,STACT                                                         
         LA    R3,TABLE                                                         
         GOTO1 XSORT,DMCB,(R3),(R5),L'TABENT,5,0                                
         B     EXIT                                                             
                                                                                
* CHECK OUT DELETE *                                                            
                                                                                
VMS40    MVC   WORK(L'TRAMS1),8(R2) FIRST RESTORE TWA FIELD                     
         MVC   8(2,R2),ELEMPTR                                                  
         MVC   10(L'TRAMS1-2,R2),WORK                                           
         ZIC   R1,5(R2)                                                         
         LA    R1,2(,R1)                                                        
         STC   R1,5(R2)                                                         
         LR    RE,R3                                                            
         LTR   RF,R5               IF NOTHING IN TABLE, CAN'T DELETE            
         BZ    BADELER                                                          
VMS44    CLC   BMKTSTA,0(RE)                                                    
         BE    VMS46                                                            
         LA    RE,TABNEXT-TABENT(,RE)                                           
         BCT   RF,VMS44                                                         
         B     BADELER                                                          
VMS46    OI    TABMFLG-TABENT(RE),X'08' SET ON DELETE FLAG                      
         OI    PRGSW,X'80'         SET ON DELETE NEEDED FROM TABLE              
                                                                                
         XI    TABMFLG-TABENT(RE),X'80' FLIP NO UPDATE FLAG                     
         B     VMS34                                                            
         DROP  R6                                                               
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
                                                                                
FCLT     NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO3                                                         
                                                                                
         LA    R1,KEY                                                           
         USING CLTHDRD,R1                                                       
         XC    KEY,KEY                                                          
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,SVCLT                                                    
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME+3(3),=C'FIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         MVC   CLTNM,CNAME                                                      
         LA    R2,CLIST                                                         
                                                                                
         MVC   SVCLTOFF,COFFICE                                                 
         CLI   CTRAFOFC,0                                                       
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC                                                
         DROP  R1                                                               
                                                                                
         GOTO1 CLUNPK,DMCB,SVCLT,QCLT                                           
         LA    R3,880                                                           
         L     RE,ASVCLIST                                                      
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
                                                                                
* SEE IF CLIENT USES PRODUCT EQUIVALENCY *                                      
* READ T0 PROFILE *                                                             
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
                                                                                
         MVC   WORK+11(1),SVCLTOFF                                              
                                                                                
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
                                                                                
* READ TA PROFILE *                                                             
                                                                                
         MVI   WORK+3,C'A'                                                      
         GOTO1 (RF),(R1),WORK,SVT1PROF,DATAMGR                                  
                                                                                
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
                                                                                
         CLI   SVT1PROF+1,C'Y'     EXCLUDE CLIENT                               
         BNE   *+8                                                              
         MVI   KEY+5,X'FF'                                                      
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         GOTO1 HIGH                                                             
         CLI   SVT1PROF+1,C'Y'     EXCLUDE CLIENT                               
         B     EXIT                                                             
         EJECT                                                                  
* FIND PRODUCT HEADER *                                                         
                                                                                
         DS    0H                                                               
FPRD     NTR1                                                                   
         LA    R0,220              MAX COUNT BUG CATCHER                        
         L     R5,ASVCLIST         TABLE OF CLIENT PROD CODES                   
FPRD10   CLC   3(1,R5),SVPRD       THIS A VALID PROD CODE                       
         BE    FPRD20                                                           
         LA    R5,4(,R5)           BUMP PROD PTR                                
         CLI   0(R5),C' '                                                       
         BL    *+8                                                              
         BCT   R0,FPRD10                                                        
         MVC   QPRD,=C'???'                                                     
         MVC   PRDNM,=CL20'UNKNOWN PRODUCT'                                     
         B     EXIT                                                             
FPRD20   MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO3                                                         
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),0(R5)                                                   
         MVC   QPRD,0(R5)                                                       
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME+3(3),=C'FIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
         L     R5,AIO                                                           
         USING PRDHDRD,R5                                                       
         MVC   PRDNM,PNAME                                                      
         DROP  R5                                                               
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* READ MARKET RECORD FOR NAME *                                                 
                                                                                
         DS    0H                                                               
FMKT     NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(17),SPACES                                                   
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R5,AIO2                                                          
         USING MKTRECD,R5                                                       
*                                                                               
         CLC   KEY(8),0(R5)                                                     
         BE    FMKT10                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO2                     
*                                                                               
FMKT10   LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(8),0(R5)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         MVC   MKTNM,0(R1)                                                      
                                                                                
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
* PRINT PRODUCT CODE AND SPOT LEN - R3 MUST POINT TO BINARY PROD                
                                                                                
         DS    0H                                                               
PPRD     NTR1                                                                   
         LA    R5,ELEM             ADDRESS OF OUTPUT AREA                       
         XC    ELEM(10),ELEM                                                    
         CLI   0(R3),0             ANY PRODUCT CODE                             
         BE    EXIT                NO,DONE                                      
         L     R1,ASVCLIST         ADDRESS OF SAVED C LIST (VALICLT)            
PPRD10   CLI   0(R1),C' '                                                       
         BL    PPRD16                                                           
         CLC   0(1,R3),3(R1)                                                    
         BE    PPRD20                                                           
         LA    R1,4(R1)                                                         
         B     PPRD10                                                           
PPRD16   LA    R1,=C'???'          UNKNOWN PRODUCT                              
PPRD20   MVC   0(3,R5),0(R1)                                                    
         CLI   1(R3),0             ANY SPOT LEN                                 
         BE    EXIT                 NO                                          
         LA    R5,2(,R5)                                                        
         CLI   0(R5),C' '                                                       
         BNH   PPRD24                                                           
         LA    R5,1(,R5)                                                        
PPRD24   MVI   0(R5),C'-'                                                       
         EDIT  (B1,1(R3)),(3,1(R5)),ALIGN=LEFT                                  
         B     EXIT                                                             
                                                                                
* PRINT PERIOD INTO WORK                                                        
                                                                                
         USING TBYDTAEL,R6                                                      
         DS    0H                                                               
PPER     NTR1                                                                   
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(3,TBYSTART),(5,WORK)                                
         MVI   WORK+8,C'-'                                                      
         GOTO1 (RF),(R1),(3,TBYEND),(5,WORK+9)                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* RTN TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                            
                                                                                
SVTWA    XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMWRT'                                                     
         B     CTWA                                                             
                                                                                
* RESTORE SAVED STORAGE *                                                       
                                                                                
RDTWA    XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMREAD'                                                    
         DS    0H                                                               
CTWA     NTR1                                                                   
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,STACT                                 
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
                                                                                
* CLEAR REST OF SCREEN AND SET OFF PROTECT BITS                                 
                                                                                
         DS    0H                                                               
CLR      NTR1                                                                   
         XC    WORK,WORK                                                        
CLR10    CLI   0(R2),9             AT END OF SCREEN                             
         BNH   EXIT                YES                                          
         ZIC   R0,0(R2)                                                         
         LR    R1,R0                                                            
         AHI   R1,-9               GET FLD LEN-1                                
         TM    1(R2),X'20'         PROTECTED                                    
         BO    CLR12                                                            
         EX    R1,CLROC             SEE IF FLD BLANK                            
         BZ    CLR14                                                            
         EX    R1,CLRCLC            SEE IF FLD BLANK                            
         BE    CLR14                                                            
CLR12    EX    R1,CLRXC             MAKE FLD BLANK                              
         OI    6(R2),X'80'                                                      
         NI    1(R2),X'FF'-X'20'   SET OFF PROTECTED                            
CLR14    AR    R2,R0                                                            
         B     CLR10                                                            
CLROC    OC    8(0,R2),8(R2)                                                    
CLRCLC   CLC   8(0,R2),SPACES                                                   
CLRXC    XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
BADELER  L     R1,=A(BADELMS)                                                   
         B     ERREXIT                                                          
DELDONE  L     R1,=A(DELRECMS)                                                  
COMMVC   LA    R2,TRAMEDH                                                       
COMMVCA  A     R1,SPTR16RR                                                      
         MVC   CONHEAD,0(R1)                                                    
         B     ERREXITC                                                         
DOADDS   L     R1,=A(DOADDSMS)                                                  
         B     COMMVC                                                           
DONE     L     R1,=A(DONEMSGD)                                                  
         B     COMMVCA                                                          
DUPMSER1 DS    0H                                                               
         MVC   CONHEAD+10(50),DUPSTMGB                                          
         SR    R0,R0                                                            
         ICM   R0,3,0(RE)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+28(4),DUB                                                
         B     ERREXITB                                                         
DUPSTAER L     R1,=A(DUPSTMGA)                                                  
         B     ERREXIT                                                          
         USING TBYDTAEL,R6                                                      
DATOVLER MVC   CONHEAD+10(50),DATOVLMS                                          
         GOTO1 MSUNPK,DMCB,(X'80',TABMKST),WORK,WORK+4                          
         MVC   CONHEAD+18(4),WORK+4                                             
         MVC   CONHEAD+23(1),WORK+8                                             
                                                                                
         CLC   WORK+9(3),SPACES                                                 
         BE    *+14                                                             
         MVI   CONHEAD+22,C'/'                                                  
         MVC   CONHEAD+23(3),WORK+9                                             
                                                                                
         GOTO1 DATCON,DMCB,(3,TBYSTART),(5,CONHEAD+42)                          
                                                                                
         GOTO1 (RF),(R1),(3,TBYEND),(5,CONHEAD+51)                              
         NI    TRAMEDH+4,X'FF'-X'20'                                            
         B     ERREXITB                                                         
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
OFFERR   L     R1,=A(OFFMSG)                                                    
         B     ERREXIT                                                          
                                                                                
                                                                                
ADDERR   L     R1,=A(ADDMSG)                                                    
         B     ERREXIT                                                          
                                                                                
DELERR   L     R1,=A(DELMSG)                                                    
         B     ERREXIT                                                          
                                                                                
MGRPERR  L     R1,=A(MGRPERMS)                                                  
         B     ERREXIT                                                          
                                                                                
NOMGRPER L     R1,=A(NOMGRPMS)                                                  
         A     R1,SPTR16RR                                                      
         MVC   16(1,R1),8(R2)                                                   
         UNPK  17(5,R1),WORK(3)                                                 
         B     ERREXITA                                                         
MGPMKTER L     R1,=A(MGPMKTMS)                                                  
         A     R1,SPTR16RR                                                      
         SR    R0,R0                                                            
         ICM   R0,3,KEYSAVE+STLKMKT-STLKEY                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  41(4,R1),DUB                                                     
         B     ERREXITA                                                         
NOCLTERR L     R1,=A(NOCLTMSG)                                                  
         LA    R2,TRACLTH                                                       
         B     ERREXIT                                                          
NOMKTST  L     R1,=A(NOMKTMSG)                                                  
                                                                                
ERREXIT  A     R1,SPTR16RR                                                      
ERREXITA MVC   CONHEAD+10(50),0(R1)                                             
ERREXITB MVC   CONHEAD(10),=C'* ERROR * '                                       
                                                                                
ERREXITC CLI   OFFLINE,C'Y'        NO SVTWA OFFLINE                             
         BE    *+8                                                              
         BAS   RE,SVTWA                                                         
                                                                                
         GOTO1 ERREX2                                                           
         EJECT                                                                  
PRDERR   MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
PRDSEQER MVI   ERROR,INVPRDSQ      PRODS MUST BE ENTERED IN ALPHA SEQ           
         B     TRAPERR                                                          
EQPRDER  MVI   ERROR,INVEQPRD      PRD * PTR MUST BE DIFFERENT                  
         B     TRAPERR                                                          
NOENTER  LA    R2,TRAMS1H          MUST ENTER AT LEAST 1 MKT OR STATION         
                                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
SVTRAPER BAS   RE,SVTWA             SAVE TABLES                                 
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
INSBSCEL EQU   15                                                               
INSSUBEL EQU   7                                                                
         EJECT                                                                  
COUNTMSG DS    0CL21                                                            
         DC    C'000 '                                                          
STATION  DC    C'STATIONS BOUGHT, '                                             
DATOVLMS DC    CL50'STATION XXXX-X   BUY EXISTS FOR MONDA/YR-MONDA/YR*'         
DUPSTMGB DC    CL50'STATION IN MARKET 0000 LIST REC ALREADY IN LIST *'          
DUPSTMGA DC    CL50'THIS STATION WAS ALREADY ENTERED IN LIST *'                 
DONEMSGD DC    CL60'STATION BUY LIST HAS BEEN UPDATED-ENTER NEXT REQUESC        
               T'                                                               
OFFMSG   DC    CL50'NO CLIENTS FOUND FOR OFFICE *'                              
BADELMS  DC    CL50'THIS STATION NOT IN BUYS, NOT DELETED *'                    
DOADDSMS DC    CL60'UPDATE STATION BUY LIST WITH ANY NEW STATIONS'              
NOCLTMSG DC    CL50'CLIENT MUST BE ENTERED *'                                   
NOMKTMSG DC    CL50'NO MARKET/STATION LIST RECORD FOR THIS MARKET *'            
MGPMKTMS DC    CL50'NO MARKET/STATION LIST RECORD FOR MARKET 0000 *'            
COPYMSG  DC    CL50'COPY CODE MUST = DPT CODE *'                                
ADDMSG   DC    CL50'USE CHANGE OPTION FOR NEW BUY STATIONS *'                   
LSTDELMS DC    CL50'FOR DELETE MUST BE SPECIFIC CLT/PRD/PER LIST *'             
DELMSG   DC    CL50'CAN ONLY DELETE FROM LIST *'                                
DELADDMS DC    CL50'CAN''T ADD AND DELETE AT SAME TIME *'                       
MGRPERMS DC    CL50'MKT GROUP MUST BE LETTER AND 1-4 DIGITS *'                  
NOMGRPMS DC    CL50'NO MARKET GROUP X0000 FOUND *'                              
DELRECMS DC    CL60'STATION BUY HAS BEEN DELETED-ENTER NEXT REQUEST'            
MKTMORMS DC    CL60'MARKET ADDED TO BUYS, HIT ENTER FOR NEXT MARKET'            
MKTLSTMS DC    CL60'LAST MARKET ADDED TO BUYS-ENTER NEXT REQUEST'               
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H4,3,C'PRODUCT'                                                  
         SSPEC H1,32,C'S T A T I O N  T B U Y  L I S T'                         
         SSPEC H2,32,C'-------------------------------'                         
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,93,PAGE                                                       
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,13,C'MARKET NAME'                                             
         SSPEC H9,13,C'------------------------'                                
         SSPEC H8,39,C'STATION'                                                 
         SSPEC H9,39,C'-------'                                                 
         SSPEC H8,49,C'PROD/SLN'                                                
         SSPEC H9,49,C'--------'                                                
         SSPEC H8,59,C'PTNR/SLN'                                                
         SSPEC H9,59,C'--------'                                                
         SSPEC H8,72,C'COPY'                                                    
         SSPEC H9,72,C'CODE'                                                    
         SSPEC H8,81,C'PERIOD START/END DATES'                                  
         SSPEC H9,81,C'----------------------'                                  
         DC    X'00'               END MARKER FOR SSPEC                         
         DROP  RB,RC,R7                                                         
         EJECT                                                                  
* VALIDATE OFFICE CODE (ONLY SUPPORTED IN OFFLINE REPORT) *                     
*              -READ CLIENT HEADER RECORDS TO BUILD                             
*               TABLE OF CLIENTS FOR REQUESTED OFFICE                           
                                                                                
VOFF     NMOD1 0,**VOFF***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BZ    REPERRA                                                          
                                                                                
         CLI   5(R2),3                                                          
         BH    OFFLNERR                                                         
                                                                                
         CLI   8(R2),C'*'          BY OFFICE                                    
         BNE   VOFF30                                                           
                                                                                
* VALIDATE OFFICE AND CONVERT TO 1 BYTE IF NEEDED                               
                                                                                
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,9(R2)       2 CHAR OFFICE CODE                           
         CLI   5(R2),3                                                          
         BE    *+10                                                             
         OC    OFCOFC2,SPACES      1 CHAR OFFICE PADDED W/SPACE                 
                                                                                
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BNZ   VOFF20              JUST CONVERT, DO NOT VALIDATE                
                                                                                
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VOFF10                                                           
                                                                                
         CLI   T216FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    VOFF10              VALIDATE AND CONVERT                         
                                                                                
         CLI   T216FFD+6,C'$'      TEST OFFICE LIST                             
         BE    VOFF10                                                           
                                                                                
         MVI   LAOFFICE,0          INIT LIMITED ACCESS OFFICE                   
                                                                                
         BAS   RE,GOFF             GET OFFICE FOR THIS CLIENT                   
         B     VOFF20                                                           
                                                                                
VOFF10   MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
                                                                                
VOFF20   XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   INVOFERR                                                         
                                                                                
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    VOFF30              2 CHAR OFFICE IS NOT ON                      
                                                                                
         MVC   OFFCD,OFCOFC        SAVE 1 BYTE OFFICE CODE                      
         B     *+10                                                             
VOFF30   MVC   OFFCD(1),9(R2)                                                   
                                                                                
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
*                                                                               
VOFF40   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BNE   VOFFX                                                            
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   VOFFX                                                            
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    VOFF60               YES                                         
*                                                                               
VOFF50   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         B     VOFF40                                                           
*                                                                               
VOFF60   L     R6,AIO3                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME+3(3),=C'FIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
                                                                                
                                                                                
         MVC   SVCLTOFF,COFFICE                                                 
         CLI   CTRAFOFC,0                                                       
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC                                                
                                                                                
*NOP     CLI   TRACLT,C'$'         USE OFFICER                                  
******   BNE   VOFF80                                                           
                                                                                
         BRAS  RE,COFF                                                          
         BNE   VOFF50                                                           
                                                                                
         CLI   TRACLT,C'*'         REQUESTED BY OFFICE                          
         BE    VOFF80                                                           
         CLI   TRACLT,C'$'         REQUESTED BY OFFICE LIST                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   OFFCD,SVCLTOFF                                                   
         B     VOFFX                                                            
                                                                                
VOFF80   CLC   OFFCD,SVCLTOFF                                                   
         BNE   VOFF50                                                           
                                                                                
VOFFX    MVC   OCLT,CKEYCLT                                                     
         MVC   CLTNM,CNAME                                                      
                                                                                
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         EJECT                                                                  
* GET OFFICE FOR SINGLE CLIENT LIMITED ACCESS                                   
                                                                                
GOFF     NTR1                                                                   
                                                                                
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),T216FFD+6  LIMITED ACCESS CLT                           
                                                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
         CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BE    *+6                                                              
         DC    H'0'                WHAT'S WRONG                                 
                                                                                
         CLC   KEY+1(1),BAGYMD                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
                                                                                
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
                                                                                
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVC   LAOFFICE,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   LAOFFICE,C'A'       IF THERE IS ONE                              
         BNL   *+10                                                             
         MVC   LAOFFICE,COFFICE    USE MEDIA OFFICE                             
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         XIT1                                                                   
                                                                                
***** NOTE ***** DELETED SOME CODE BELLW -- IF NEED IT LOOK AT SPTRA11          
                                                                                
OFFLNERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OFFLNMS),OFFLNMS                                       
         GOTO1 ERREX2                                                           
INVOFERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVOFMS),INVOFMS                                       
         GOTO1 ERREX2                                                           
REPERRA  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REPMSGA),REPMSGA                                       
         GOTO1 ERREX2                                                           
OFFLNMS  DC    C'* ERROR * OFFICE MUST BE * OR $ AND 1 OR 2 CHARS *'            
INVOFMS  DC    C'* ERROR * INVALID OFFICE *'                                    
REPMSGA  DC    C'* ERROR * OFFICE CODE SUPORTED OFFLINE ONLY *'                 
         DROP  R6,RB,RC                                                         
                                                                                
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY LIST OF TRAFFIC BUYS *                                                
                                                                                
DLST     NMOD1 0,**DLST***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         LH    R3,TABPTR                                                        
         LA    R3,TABLE(R3)                                                     
         LH    R5,TABCTR                                                        
         USING TABLED,R3                                                        
         EDIT  (B2,STACT),(3,TRADSP),ZERO=NOBLANK                               
         OC    STACT,STACT                                                      
         BZ    DLST40                                                           
                                                                                
         TM    PRGSW,X'02'         ALLOW EMPTY SCREEN FOR ADDS                  
         BO    DLST40              YES                                          
                                                                                
         TM    PRGSW,X'04'         NEW STARTING MARKET                          
         BZ    DLST08              NO                                           
         LA    R3,TABLE                                                         
         LH    R1,STACT                                                         
         LA    R5,1                                                             
DLST04   CLC   STMKT,TABMKST                                                    
         BNH   DLST06                                                           
         LA    R3,TABNEXT                                                       
         LA    R5,1(,R5)                                                        
         BCT   R1,DLST04                                                        
         B     NOSMKTER                                                         
DLST06   NI    PRGSW,X'FF'-X'04'                                                
                                                                                
DLST08   LA    R2,TRADSP+21                                                     
         EDIT  (R5),(3,(R2))                                                    
         MVI   TRADSP+24,C'-'                                                   
         OI    TRADSPH+6,X'80'                                                  
         XC    SVDSPMKT,SVDSPMKT                                                
                                                                                
         LA    R2,TRAMS1H                                                       
         LA    R4,TRAMSAH                                                       
                                                                                
         MVC   BMKTSTA,TABMKST                                                  
         MVC   SVDSPMKT,TABMKST                                                 
         BAS   RE,FMTSTA1                                                       
DLST10   XC    WORK,WORK                                                        
         MVC   WORK(L'QMKT),QMKT                                                
         MVC   8(L'TRAMS1,R2),WORK                                              
         OI    1(R2),X'20'         PROTECT                                      
         OI    4(R2),X'20'         VALIDATED                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
DLST14   MVC   WORK(L'STAPRNT),STAPRNT                                          
         OC    STANET,STANET                                                    
         BZ    *+10                                                             
         MVC   WORK(L'STANET),STANET                                            
                                                                                
         CLC   8(L'TRAMS1,R2),WORK                                              
         BE    *+14                                                             
         MVC   8(L'TRAMS1,R2),WORK                                              
         OI    1(R2),X'20'         PROTECT                                      
         OI    4(R2),X'20'         VALIDATED                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R3,TABNEXT                                                       
         LA    R5,1(,R5)                                                        
         OC    TABMKST,TABMKST     END OF TABLE                                 
         BZ    DLST34                                                           
DLST20   MVC   BMKTSTA,TABMKST                                                  
         BAS   RE,FMTSTA1                                                       
         CLC   SVDSPMKT,TABMKST                                                 
         BNE   DLST24                                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R4,R2               END OF LINE                                  
         BH    DLST14              NO                                           
         CLI   0(R4),9            AT END OF SCREEN                              
         BNH   DLST30                                                           
         OI    1(R2),X'20'         PROTECT                                      
         OI    4(R2),X'20'         VALIDATED                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R2,TRAMS2H-TRAMS1H(,R4)                                          
         LA    R4,TRAMSAH-TRAMS1H(,R4)                                          
         B     DLST14                                                           
DLST24   MVC   SVDSPMKT,TABMKST                                                 
         LR    R2,R4                                                            
         LA    R4,TRAMSAH-TRAMS1H(,R4)                                          
         CLI   0(R2),9            AT END OF SCREEN                              
         BH    DLST10                                                           
DLST30   LA    R0,TABLE                                                         
         SR    R3,R0                                                            
         STH   R3,TABPTR                                                        
         STH   R5,TABCTR                                                        
         B     DLST36                                                           
DLST34   XC    TABPTR,TABPTR                                                    
         MVC   TABCTR,=X'0001'                                                  
DLST36   BCTR  R5,0                                                             
         LA    R2,TRADSP+25                                                     
         EDIT  (R5),(3,(R2))                                                    
         MVC   TRADSP+28(10),DSPLYMSG                                           
         OI    TRATAGH+1,X'01'     SET ON MODIFIED                              
         OI    TRATAGH+6,X'80'     AND TRANSMIT                                 
         B     DLSTX                                                            
DLST40   MVC   TRADSP+21(17),NONEMSG                                            
         OI    TRADSPH+6,X'80'                                                  
DLSTX    XIT1                                                                   
         EJECT                                                                  
* FORMAT MARKET/STATION FOR PRINTING                                            
                                                                                
         DS    0H                                                               
FMTSTA1  NTR1                                                                   
         GOTO1 MSUNPK,DMCB,(X'80',BMKTSTA),QMKT,WORK                            
                                                                                
         XC    STANET,STANET                                                    
         CLC   WORK+5(3),SPACES                                                 
         BE    *+14                                                             
         MVC   STANET,WORK                                                      
         MVI   STANET+4,C'/'                                                    
                                                                                
         CLI   WORK+4,C' '                                                      
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
         CLI   QMED,C'T'                                                        
         BE    FMTSTA1X                                                         
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    FMTSTA1X                                                         
         MVI   3(RE),C' '                                                       
                                                                                
FMTSTA1X B     DLSTX                                                            
                                                                                
NOSMKTER MVC   CONHEAD,NOSMKTMS                                                 
         GOTO1 ERREX2                                                           
NOSMKTMS DC    CL60'* ERROR * NO MARKET HIGHER/EQUAL TO STARTING MARKETC        
                *'                                                              
NONEMSG  DS    0CL17                                                            
         DC    CL4'NONE'                                                        
DSPLYMSG DS    0CL9                                                             
         DC    CL13' DISPLAYED   '                                              
         LTORG                                                                  
         DROP  R3,RB,RC                                                         
         EJECT                                                                  
* GET NEXT CLIENT FOR THIS OFFICE CODE (ONLY IN OFFLINE REPORT) *               
*          END OF CLIENTS, RETURN NE COND CODE                                  
                                                                                
NOFF     NMOD1 0,**NOFF***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BZ    REPERRB                                                          
                                                                                
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),OCLT       LAST CLIENT THIS OFFICE                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         GOTO1 HIGH                                                             
*                                                                               
NOFF10   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BNE   NOFFXIT                                                          
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   NOFFX                                                            
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    NOFF30               YES                                         
*                                                                               
NOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         B     NOFF10                                                           
*                                                                               
NOFF30   L     R6,AIO3                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME+3(3),=C'FIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
                                                                                
         MVC   SVCLTOFF,COFFICE                                                 
         CLI   CTRAFOFC,0                                                       
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC                                                
                                                                                
*NOP     CLI   TRACLT,C'$'         USING OFFICER AS FILTER                      
*****    BNE   NOFF34                                                           
         BRAS  RE,COFF             GO CK OFFICE                                 
         BNE   NOFF20               NOT OK                                      
                                                                                
         CLI   TRACLT,C'*'                                                      
         BE    NOFFX                                                            
                                                                                
         MVC   OFFCD,SVCLTOFF                                                   
         B     NOFFX                                                            
                                                                                
** CODE BELOW IS NOT USED (DONE IN OFFICER)                                     
                                                                                
NOFF34   CLC   SVCLTOFF,OFFCD      TEST DESIRED OFFICE CODE                     
         BNE   NOFF20                                                           
                                                                                
NOFF36   DS   0H                                                                
*                                                                               
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    NOFFX                                                            
*                                                                               
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    NOFF60                                                           
*                                                                               
         CLI   T216FFD+6,C'$'          TEST OFFICE LOCKOUT                      
         BE    NOFF70                                                           
*                                                                               
         CLC   T216FFD+6(2),CKEYCLT   ELSE SINGLE CLIENT ACCESS                 
         BNE   NOFF20                                                           
         B     NOFFX                                                            
*                                                                               
NOFF60   CLC   T216FFD+7(1),OFFCD    MATCH OFFICE CODE                          
         BNE   NOFF20                                                           
         B     NOFFX                                                            
                                                                                
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
                                                                                
NOFF70   BRAS  RE,COFF             GO CK OFFICE                                 
         BNE   NOFF20                                                           
                                                                                
NOFFX    MVC   OCLT,CKEYCLT                                                     
         MVC   CLTNM,CNAME                                                      
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
NOFFXIT  XIT1                                                                   
                                                                                
* CHECK OFFICE TO BE VALID *                                                    
                                                                                
COFF     NMOD1 0,**COFF***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         GOTO1 CLUNPK,DMCB,KEY+2,DUB                                            
                                                                                
         USING CLTHDRD,R6                                                       
                                                                                
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
                                                                                
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R1,WORKSEC                                                       
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         MVC   OFCCLT,DUB                                                       
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1                                                               
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         XIT1                                                                   
REPERRB  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REPMSGB),REPMSGB                                       
         GOTO1 ERREX2                                                           
REPMSGB  DC    C'* ERROR * OFFICE CODE SUPORTED OFFLINE ONLY *'                 
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* VALIDATE FILTER ROUTINES - DATE, PROD, TYPE, LEN                              
                                                                                
VFTR     NMOD1 0,**VFTR**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         LA    R2,TRAFLTRH         FILTER                                       
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,4                                                             
         B     VFTR04                                                           
VFTR02   ZIC   R1,5(R2)                                                         
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VFTR08                                                           
VFTR06   B     VFTRERR                                                          
VFTR08   GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(5,BLOCK)                             
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRA            NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         DATE                                         
         BNE   VFTR20                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         L     R6,DMCB             GET LENGTH OF FIELD                          
         LTR   R6,R6                                                            
         BZ    DATERRA                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,DATEFTR)                                 
         CLM   R6,1,1(R4)          ONLY 1 DATE ENTERED                          
         BE    VFTR18              YES                                          
                                                                                
         LA    R5,1(R6,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),WORK                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRA                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,EDATEFTR)                                
VFTR18   MVC   DATESFTR,HOLDSIGN                                                
         B     VFTR70                                                           
VFTR20   EX    R1,VFTRCLCD         MARKET                                       
         BNE   VFTR40                                                           
         MVC   ELEM,TRAFLTRH                                                    
         PACK  ELEM+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   ELEM+5(1),1(R4)     DATA LEN                                     
         MVC   ELEM+8(10),22(R4)      SPOT LEN                                  
         MVI   ERROPT,C'Y'                                                      
         LA    R2,ELEM                                                          
         GOTO1 VALIMKT                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   TRAPERRA            GO PRINT ERROR                               
         MVC   BMKTFTR,BMKT                                                     
         MVC   MKTFTR,QMKT                                                      
         MVC   MKTSFTR,HOLDSIGN                                                 
         B     VFTR70                                                           
VFTR40   EX    R1,VFTRCLCE         SPOT LEN                                     
         BNE   VFTR50                                                           
         TM    3(R4),X'80'         WAS SPOT LEN NUMERIC                         
         BZ    NUMERRA                                                          
         MVC   ELEM,TRAFLTRH                                                    
         PACK  ELEM+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   ELEM+5(1),1(R4)     DATA LEN                                     
         MVC   ELEM+8(10),22(R4)      SPOT LEN                                  
         MVI   ERROPT,C'Y'                                                      
         LA    R2,ELEM                                                          
         GOTO1 VALISLN                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   TRAPERRA            GO PRINT ERROR                               
         MVC   SLNFTR,WORK                                                      
         B     VFTR70                                                           
VFTR50   EX    R1,VFTRCLCF         ACTIVITY                                     
         BNE   VFTR60                                                           
         OI    PRGSW,X'20'         SET ON BUY ACTIVITY                          
         B     VFTR70                                                           
VFTR60   EX    R1,VFTRCLCG         ADD (LEAVE SCREEN EMPTY FOR ADDS)            
         BNE   VFTRERR                                                          
         OI    PRGSW,X'02'         SET ON ADD OPTION                            
VFTR70   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
VFTRX    OI    4(R2),X'20'                                                      
         XIT1                                                                   
VFTRERR  MVC   CONHEAD,FTRHELP                                                  
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+10                                                             
         MVC   CONHEAD+33(11),=CL11'ACTIVITY= *'                                
         GOTO1 ERREX2                                                           
NUMERRA  MVI   ERROR,NOTNUM                                                     
         B     TRAPERRA                                                         
MISSCLT  LA    R2,TRACLTH                                                       
MISSERRA MVI   ERROR,MISSING                                                    
         B     TRAPERRB                                                         
DATERRA  MVI   ERROR,INVDATE                                                    
TRAPERRA GOTO1 ERREX                                                            
VFTRCLCA CLC   12(0,R4),=CL5'DATE'                                              
VFTRCLCD CLC   12(0,R4),=CL4'MKT'                                               
VFTRCLCE CLC   12(0,R4),=CL4'LEN'                                               
VFTRCLCF CLC   12(0,R4),=CL9'ACTIVITY'                                          
VFTRCLCG CLC   12(0,R4),=CL3'ADD'                                               
FTRHELP  DC    CL60'VALID FILTERS - DATE/MKT/LEN/ADD *'                         
         DROP  RB,RC                                                            
         EJECT                                                                  
************************************************************                    
* VALIDATE COPY CODE (MAY BE ESTIMATE IF T1 PROFILE 12 ON) *                    
************************************************************                    
                                                                                
VCC      NMOD1 0,**+VCC**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         LA    R2,TRACODEH         CODE                                         
                                                                                
         MVI   CODE,0                                                           
         MVI   CODESW,C'N'                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VCCX                                                             
                                                                                
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VCC40                                                            
                                                                                
         CLI   ACTNUM,ACTLIST      IS ACTION LIST                               
         BE    VCC10                YES, PROD NOT REQUIRED                      
                                                                                
         CLI   BPRD,0              MUST HAVE ENTERED PROD                       
         BE    MISSPRD                                                          
                                                                                
VCC10    GOTO1 VALINUM                                                          
                                                                                
         MVC   CODE,ACTUAL                                                      
         MVI   CODESW,C'Y'                                                      
                                                                                
         LA    R3,QPRD                                                          
         OC    QPRD,QPRD                                                        
         BNZ   VCC20                                                            
         L     R3,ASVCLIST                                                      
                                                                                
VCC20    XC    KEY,KEY                                                          
                                                                                
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM(3),BAGYMD                                                 
         MVC   EKEYPRD,0(R3)                                                    
         MVC   EKEYEST,CODE                                                     
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCC30                                                            
         OC    QPRD,QPRD                                                        
         BNZ   ESTCDER                                                          
         LA    R3,4(,R3)                                                        
         CLI   0(R3),C' '                                                       
         BH    VCC20                                                            
         B     ESTCDERA                                                         
                                                                                
VCC30    MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         MVC   FILENAME+3(3),=C'FIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         CLI   ECOPY,0             MUST NOT BE COPY CODE                        
         BNE   ESTCCER                                                          
         GOTO1 DATCON,DMCB,(0,ESTART),(3,ESTSTR)                                
         GOTO1 (RF),(R1),(0,EEND),(3,ESTEND)                                    
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         B     VCCX                                                             
         DROP  R4                                                               
                                                                                
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
                                                                                
VCC40    CLI   5(R2),1             ONLY 1 CHARACTER COPY CODE                   
         BNE   CPYCDER                                                          
         GOTO1 ANY                                                              
                                                                                
         MVC   CODE,WORK                                                        
                                                                                
VCCX     OI    4(R2),X'20'                                                      
                                                                                
         XIT1                                                                   
MISSPRD  LA    R2,TRAPRDH                                                       
         MVC   CONHEAD,MISPRMS                                                  
         B     VCCEREX                                                          
CPYCDER  MVC   CONHEAD,CPYCDMS                                                  
         B     VCCEREX                                                          
ESTCCER  MVC   CONHEAD,ESTCCMS                                                  
VCCEREX  GOTO1 ERREX2                                                           
ESTCDER  MVC   CONHEAD,ESTCDMS                                                  
         MVC   CONHEAD+33(3),QPRD                                               
         B     VCCEREX                                                          
ESTCDERA MVC   CONHEAD,ESTCDMSA                                                 
         B     VCCEREX                                                          
ESTCDMS  DC    CL60'* ERROR * ESTIMATE NOT FOUND FOR XXX *'                     
ESTCDMSA DC    CL60'* ERROR * ESTIMATE NOT FOUND FOR ANY PRODUCT *'             
ESTCCMS  DC    CL60'* ERROR * ESTIMATE HAS COPY CODE *'                         
CPYCDMS  DC    CL60'* ERROR * COPY CODE MUST BE 1 CHARACTER *'                  
MISPRMS  DC    CL60'* ERROR * MUST ENTER PRODUCT FOR ESTIMATE *'                
         DROP  RB,RC                                                            
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE VALIDATES START/END DATES FOR PERIOD             *                 
***************************************************************                 
                                                                                
VPER     NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         LA    R2,TRAPERH          PERIOD                                       
         CLI   8(R2),C'?'          IF QUESTION MK, TELL MEL FLT DATES           
         BNE   VPER30                                                           
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    NOCLTERA            NO, CAN'T FIND FLIGHTS                       
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   *+12                 NO                                          
         CLI   CODE,0              COPY CODE = EST ENTERED                      
         BNE   VPER26               YES                                         
         CLI   5(R2),1             SEE IF DATE ENTERED TOO                      
         BE    VPER04               NO                                          
         GOTO1 DATVAL,DMCB,9(R2),WORK                                           
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERRB                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,PERST)                                   
         B     VPER06                                                           
VPER04   GOTO1 DATCON,DMCB,(5,0),(3,PERST) TODAY'S DATE                         
                                                                                
VPER06   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(4),BAGYMD BCLT AND BPRD                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         GOTO1 HIGH                                                             
*                                                                               
VPER10   CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   PERST,KEY+6         FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER12                                                           
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         GOTO1 HIGH                                                             
         MVC   PERST(1),KEY+6      FORCE DATE TO YR OF LAST FLT                 
*                                                                               
VPER12   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         LA    R3,4                                                             
         LA    R5,CONHEAD+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL1                                                        
         B     *+8                                                              
VPER14   BAS   RE,NEXTEL1                                                       
         BNE   VPER20                                                           
         USING FLTDTAEL,R6                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,11(,R5)                                                       
         BCT   R3,VPER14                                                        
VPER20   MVI   0(R5),C'*'                                                       
         LA    R2,TRACLTH                                                       
         CLI   KEY+5,0             WAS FLIGHT PRD SPECIFIC                      
         BE    ERREXIT3                                                         
         LA    R2,TRAPRDH                                                       
         B     ERREXIT3                                                         
                                                                                
VPER26   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(13),=C'ESTIMATE RUNS'                                    
         GOTO1 DATCON,DMCB,(3,ESTSTR),(5,CONHEAD+14)                            
         MVI   CONHEAD+23,C'-'                                                  
         GOTO1 (RF),(R1),(3,ESTEND),(5,CONHEAD+25)                              
         B     ERREXIT3                                                         
                                                                                
* DO DATE VALIDATION                                                            
                                                                                
VPER30   XC    PERDTS,PERDTS                                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VPER34                                                           
         CLI   ACTNUM,ACTLIST      IF LIST, DO TODAY                            
         BNE   MISSERRB                                                         
         GOTO1 DATCON,DMCB,(5,0),(3,PERST) TODAY'S DATE 3 BYE BINARY            
VPER32   MVC   PEREND,=X'FFFFFF'   MAKE END DATE FOREVER                        
         B     VPERX                                                            
                                                                                
VPER34   CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VPER36                                                           
         CLI   CODE,0              EST ENTERED                                  
         BE    VPER36                                                           
         CLC   =C'ES',8(R2)        USE ESTIMATE DATES                           
         BNE   VPER36                                                           
         GOTO1 DATCON,DMCB,(3,ESTSTR),(5,TRAPER)                                
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,ESTEND),(5,TRAPER+9)                                
         MVC   PERDTS,ESTSTR                                                    
         OI    TRAPERH+6,X'80'                                                  
         MVI   TRAPERH+5,17        PUT IN NEW LENGTH                            
         B     VPERX                                                            
VPER36   LA    R5,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R5),WORK                                            
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERRB                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,PERST)                                   
                                                                                
         CLM   R4,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VPER40              YES                                          
                                                                                
         LA    R5,1(R4,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),WORK                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRB                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,PEREND)                                  
         CLC   PERST,PEREND                                                     
         BH    DATSQERR                                                         
VPER40   CLI   ACTNUM,ACTLIST      IF LIST, ANY DATES OK                        
         BE    VPER50                                                           
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   *+12                 NO                                          
         CLI   CODE,0              EST ENTERED                                  
         BNE   VPER44               YES                                         
         BAS   RE,FFLT             FIND FLIGHT DATES                            
                                                                                
         BAS   RE,CEST             GO CHECK ESTIMATES                           
         B     VPERX                                                            
                                                                                
VPER44   BAS   RE,FEST             FIND ESTIMATE DATES                          
         B     VPERX                                                            
VPER50   OC    PEREND,PEREND       ANY END DATE                                 
         BZ    VPER32              MAKE FOREVER                                 
VPERX    OI    4(R2),X'20'                                                      
         GOTO1 DATCON,DMCB,(3,PERST),(2,PERSTP)                                 
         GOTO1 (RF),(R1),(3,PEREND),(2,PERENDP)                                 
         XIT1                                                                   
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE DETERMINES FLIGHT DATES FOR GIVEN TELECAST DATES *                 
***************************************************************                 
                                                                                
FFLT     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(4),BAGYMD BCLT AND BPRD                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    CHKF2                                                            
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         GOTO1 HIGH                                                             
*                                                                               
CHKF2    CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   PERST,KEY+6         FIRST TLCST DATE TO RECORD END DATE          
         BNH   CHKF4                                                            
         GOTO1 SEQ                                                              
         B     CHKF2                                                            
*                                                                               
CHKF4    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL1                                                        
         B     *+8                                                              
*                                                                               
CHKF6    BAS   RE,NEXTEL1                                                       
         BNE   FLTELER                                                          
*                                                                               
         USING FLTDTAEL,R6                                                      
*                                                                               
         CLC   PERST,FLTEND        FIRST TLCST AFTER FLIGHT END                 
         BH    CHKF6                                                            
         CLC   PEREND,FLTSTART     LAST TLCST BEFORE FLIGHT START               
         BL    CHKF6                                                            
                                                                                
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS FLIGHT *                      
                                                                                
         CLC   PEREND,FLTEND       LAST TLCST DATE TO FLT END                   
         BH    FLTOVLER                                                         
         CLC   PERST,FLTSTART                                                   
         BL    FLTOVLER                                                         
         B     VPERX                                                            
         EJECT                                                                  
*****************************************************************               
* SUBROUTINE DETERMINES ESTIMATE DATES FOR GIVEN TELECAST DATES *               
*****************************************************************               
                                                                                
         DS    0H                                                               
FEST     NTR1                                                                   
                                                                                
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                    
                                                                                
         CLC   PERST,ESTEND        START DATE AFTER EST END                     
         BH    ESTDTERR                                                         
         CLC   PERST,ESTSTR        START DATE BEFORE EST STR                    
         BL    ESTDTERR                                                         
                                                                                
         OC    PEREND,PEREND       ANY END DATE ENTERED                         
         BZ    FEST10                                                           
                                                                                
         CLC   PERST,PEREND        PER STR TO PER END                           
         BH    DATSQERR                                                         
         CLC   PEREND,ESTSTR       PER END TO EST STR                           
         BL    ESTDTERR                                                         
         CLC   PEREND,ESTEND       PER END TO EST END                           
         BH    ESTDTERR                                                         
                                                                                
         B     VPERX                                                            
                                                                                
* ONLY ONE DATE GIVEN, MUST MATCH ESTIMATE START                                
                                                                                
FEST10   CLC   PERST,ESTSTR        PER START MATCH ESTIMATE START               
         BNE   ESTSDTER                                                         
         MVC   PEREND,ESTEND                                                    
         GOTO1 DATCON,DMCB,(3,PERST),(5,TRAPER)                                 
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,PEREND),(5,TRAPER+9)                                
         OI    TRAPERH+6,X'80'                                                  
         B     VPERX                                                            
         EJECT                                                                  
***********************************************************                     
* SUBROUTINE FINDS ANY ESTIMATES FOR GIVEN TELECAST DATES *                     
***********************************************************                     
                                                                                
         DS    0H                                                               
CEST     NTR1                                                                   
         CLI   SVTBPR04,C'Y'       ALLOW WITH NO ESTIMATES                      
         BE    VPERX                                                            
         GOTO1 DATCON,DMCB,(3,PERST),(0,WORK)                                   
         GOTO1 (RF),(R1),(3,PEREND),(0,WORK+6)                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
                                                                                
         MVC   KEY+4(3),QPRD                                                    
                                                                                
         MVI   KEY+7,1                                                          
                                                                                
CEST10   MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   NOESTERR                                                         
                                                                                
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   CEST20                                                           
                                                                                
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME+3(3),=C'FIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   ESTART,WORK+6       EST START AFTER REQ END                      
         BH    CEST20                                                           
         CLC   EEND,WORK           EST END BEFORE REQ START                     
         BL    CEST20                                                           
                                                                                
*  FOUND AT LEAST 1 ESTIMATE COVERING PART OF TELECAST DATES                    
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         B     VPERX                                                            
                                                                                
CEST20   MVC   KEY+8(5),=5X'FF'    FORCE TO NEXT EST                            
         B     CEST10                                                           
         DROP  R6                                                               
                                                                                
GETEL1   AH    R6,DATADISP                                                      
FIRSTEL1 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL1  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL1                                                         
         EJECT                                                                  
NOESTERR L     R1,=A(NOESTMS)                                                   
         B     ERRVPERX                                                         
ESTDTERR L     R1,=A(ESTDTEMS)                                                  
         B     ERRVPERX                                                         
ESTSDTER L     R1,=A(ESTSDTMS)                                                  
         B     ERRVPERX                                                         
DATSQERR L     R1,=A(DATSEQMS)                                                  
         B     ERRVPERX                                                         
NOCLTERA L     R1,=A(NOCLTMSG)                                                  
         LA    R2,TRACLTH                                                       
         B     ERRVPERX                                                         
ERRVPERX A     R1,SPTR16RR                                                      
         MVC   CONHEAD+10(50),0(R1)                                             
ERREXIT2 MVC   CONHEAD(10),=C'* ERROR * '                                       
ERREXIT3 GOTO1 ERREX2                                                           
MISSERRB MVI   ERROR,MISSING                                                    
         B     TRAPERRB                                                         
FLTRECER MVI   ERROR,NOFLTREC                                                   
         B     TRAPERRB                                                         
FLTELER  MVI   ERROR,NOFLTEL                                                    
         B     TRAPERRB                                                         
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         B     TRAPERRB                                                         
DATERRB  MVI   ERROR,INVDATE                                                    
TRAPERRB GOTO1 ERREX                                                            
         LTORG                                                                  
ESTDTEMS DC    CL50'DATE(S) NOT IN ESTIMATE PERIOD *'                           
ESTSDTMS DC    CL50'START DATE MUST MATCH ESTIMATE START *'                     
DATSEQMS DC    CL50'END DATE BEFORE START DATE *'                               
NOESTMS  DC    CL50'NO ESTIMATES COVERING TELECAST DATES *'                     
         DROP  RB,RC                                                            
         EJECT                                                                  
* VALIDATE MARKET GROUP FOR LIST RTN *                                          
                                                                                
VMGR     NMOD1 0,**VMGR**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLI   5(R2),2                                                          
         BL    MGRPERRA                                                         
         CLI   5(R2),5                                                          
         BH    MGRPERRA                                                         
         CLI   8(R2),C'A'                                                       
         BL    MGRPERRA                                                         
         CLI   8(R2),C'Z'                                                       
         BH    MGRPERRA                                                         
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         LA    R1,9(,R2)                                                        
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
VMGR10   CLI   0(R1),C'0'                                                       
         BL    MGRPERRA                                                         
         CLI   0(R1),C'9'                                                       
         BH    MGRPERRA                                                         
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   RF,VMGR10                                                        
         PACK  WORK(3),DUB(5)                                                   
         MVC   MGRPSV(1),8(R2)     SAVE MARKET GROUP FOR HEADING                
         MVC   MGRPSV+1(4),DUB                                                  
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+8(1),8(R2)                                                   
         MVC   KEY+9(2),WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BE    VMGR14                                                           
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    NOMGRPEA             NO, ERROR                                   
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      ZERO CLIENT                                  
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   NOMGRPEA                                                         
VMGR14   OI    TRASMKTH+4,X'20'                                                 
         LA    R3,400                                                           
         LA    R4,MGRPTABL                                                      
VMGR20   MVC   0(2,R4),KEY+11                                                   
         LA    R4,2(,R4)                                                        
         GOTO1 SEQ                                                              
         CLC   KEY(11),KEYSAVE                                                  
         BNE   VMGRX                                                            
         BCT   R3,VMGR20                                                        
         DC    H'0'                                                             
                                                                                
VMGRX    DS   0H                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         XIT1                                                                   
MGRPERRA LA    R1,MGRPERMA                                                      
         B     VMGRPERX                                                         
NOMGRPEA LA    R1,NOMGRPMA                                                      
         MVC   NOMGRPMA+16(1),8(R2)                                             
         UNPK  NOMGRPMA+17(5),WORK(3)                                           
VMGRPERX MVC   CONHEAD(10),=C'* ERROR * '                                       
         MVC   CONHEAD+10(50),0(R1)                                             
         GOTO1 ERREX2                                                           
         LTORG                                                                  
MGRPERMA DC    CL50'MKT GROUP MUST BE LETTER AND 1-4 DIGITS *'                  
NOMGRPMA DC    CL50'NO MARKET GROUP X0000 FOUND *'                              
         DROP  RB,RC                                                            
         EJECT                                                                  
* PURGE DELETED TRAFFIC BUYS FROM TABLE *                                       
                                                                                
DTAB     NMOD1 0,**DTAB**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         LA    R3,TABLE                                                         
         USING TABLED,R3                                                        
         LH    R5,STACT                                                         
         SR    R2,R2                                                            
DTAB10   TM    TABMFLG,X'08'       DELETED                                      
         BZ    DTAB12                                                           
         MVC   TABMKST,=X'FFFFFFFFFF'                                           
         LA    R2,1(,R2)                                                        
DTAB12   LA    R3,TABNEXT                                                       
         BCT   R5,DTAB10                                                        
         NI    PRGSW,X'FF'-X'80'   DELETES REMOVED                              
         LA    R3,TABLE                                                         
         LH    R5,STACT                                                         
         GOTO1 XSORT,DMCB,(R3),(R5),L'TABENT,5,0                                
         LR    R4,R5                                                            
         SR    R1,R1                                                            
DTAB20   CLC   TABMKST,=X'FFFFFFFFFF'                                           
         BNE   DTAB22                                                           
         TM    TABMFLG,X'08'                                                    
         BO    *+6                                                              
         DC    H'0'                                                             
         XC    TABENT,TABENT                                                    
         BCTR  R1,0                                                             
DTAB22   LTR   R1,R1                                                            
         BZ    DTAB24                                                           
         LH    RE,TABPTR                                                        
         LA    RE,TABLE(RE)                                                     
         CR    R3,RE                                                            
         BH    DTAB24                                                           
         XC    TABPTR,TABPTR                                                    
         MVC   TABCTR,=X'0001'                                                  
DTAB24   LA    R3,TABNEXT                                                       
         BCT   R5,DTAB20                                                        
         LPR   R1,R1                                                            
         CR    R2,R1                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R5,STACT                                                         
         SR    R5,R2                                                            
         STH   R5,STACT                                                         
         XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
                                                                                
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         MVC   H3+10(L'QCLT),QCLT                                               
         MVC   H3+15(L'CLTNM),CLTNM                                             
         MVC   H4+10(L'QPRD),QPRD                                               
         MVC   H4+15(L'PRDNM),PRDNM                                             
         OC    MGRPTABL(2),MGRPTABL  ONLY ENTERED MARKET GROUPS                 
         BZ    HDHK10                                                           
         MVC   H4+42(5),=C'MGR ='                                               
         MVC   H4+48(5),MGRPSV                                                  
                                                                                
HDHK10   TM    PRGSW,X'20'         BUY ACTIVITY REPORT                          
         BZ    HDHK20                                                           
         MVC   H3+41(13),=CL13'TBUY ACTIVITY'                                   
*                                                                               
HDHK20   CLI   TRACLT,C'$'         TEST OFFICER CODE GIVEN                      
         BNE   HDHK30                                                           
         MVI   H4+60,C'('                                                       
         MVC   H4+61(2),TRACLT     SHOW OFFICE CODE                             
         MVI   H4+63,C')'                                                       
                                                                                
HDHK30   CLI   OFFCD,0             TEST OFFICE CODE GIVEN                       
         BE    HDHK40                                                           
         MVC   H4+44(6),=C'OFFICE'                                              
*                                                                               
         BAS   RE,CNVOFF           CONVERT 1 BYTE OFFICE CODE                   
         BE    HDHK40              OFFICE PRINTED                               
*                                                                               
         GOTO1 =V(OFFOUT),DMCB,OFFCD,HEXOUT,H4+51                               
*                                                                               
                                                                                
HDHK40   CLI   ORGPRF13,C'Y'       THIS CLIENT USING PROD EQUIVALENCY           
         BNE   HDHKX                                                            
         CLC   QPRD,=C'???'                                                     
         BE    HDHKX                                                            
         MVC   H5+50(13),KEY                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PEQKEY,R4                                                        
         MVC   PEQKID,=X'0A37'                                                  
         MVC   PEQKAM(3),BAGYMD                                                 
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BNZ   *+10                                                             
         MVC   PEQKCLT,SVCLT                                                    
         MVC   PEQKPRD,QPRD                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A BASE                                  
         BNE   HDHK44               NO                                          
         MVC   H5+10(6),=C'(BASE)'                                              
         B     HDHK50                                                           
                                                                                
HDHK44   XC    KEY,KEY                                                          
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQPAM(3),BAGYMD                                                 
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BNZ   *+10                                                             
         MVC   PEQKCLT,SVCLT                                                    
         MVC   PEQPEPRD,QPRD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS AN EQUIV                                
         BNE   HDHK46               NO                                          
         MVC   H5+10(26),=C'EQUIVALENT TO BASE PRODUCT'                         
         MVC   H5+10+27(3),PEQPBPRD                                             
         B     HDHK50                                                           
         DROP  R4                                                               
                                                                                
HDHK46   MVC   H5+10(33),=C'PRODUCT IS NOT BASE OR EQUIVALENT'                  
                                                                                
HDHK50   MVC   KEY(13),H5+50                                                    
         MVC   H5+50(13),SPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
HDHKX    XIT1                                                                   
         EJECT                                                                  
* CONVERT 1 BYTE OFFICE CODE AND PRINT 2 CHAR CODE                              
                                                                                
CNVOFF   NTR1                                                                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,OFFCD                                                     
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   CNVOFFX                                                          
                                                                                
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    CNVOFFX             2 CHAR OFFICE IS NOT ON                      
                                                                                
         MVC   H4+51(2),OFCOFC2                                                 
         CR    RB,RB               SET EQ CC                                    
                                                                                
CNVOFFX  XIT1                                                                   
                                                                                
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
       ++INCLUDE SPTRBUY                                                        
         EJECT                                                                  
       ++INCLUDE SPTRSTAL                                                       
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
         PRINT OFF                                                              
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAB6D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR16RR DS    F                                                                
                                                                                
* SAVED AREA STARTS HERE FOR 6144 *                                             
                                                                                
STACT    DS    H                                                                
STMKT    DS    XL2                 STARTING MARKET FOR LIST/DISPLAY             
STMKTSW  DS    CL1                 = ONLY SHOW 1 MKT LIST/DISPLAY               
OFFCD    DS    CL1                                                              
LAOFFICE DS    CL1                 OFFICE FOR THIS LIMITED ACCESS               
OCLT     DS    XL2                                                              
SVMKT    DS    XL2                                                              
CODE     DS    CL1                                                              
CODESW   DS    CL1                                                              
                                                                                
PERDTS   DS   0XL6                                                              
PERST    DS    CL3                                                              
PEREND   DS    CL3                                                              
                                                                                
PERDTP   DS   0XL4                                                              
PERSTP   DS    CL2                                                              
PERENDP  DS    CL2                                                              
                                                                                
ESTSTR   DS    CL3                                                              
ESTEND   DS    CL3                                                              
                                                                                
SVCLT    DS    XL2                                                              
SVPRD    DS    XL1                 USED FOR PAGE BREAK COMPARES                 
                                                                                
ORGPRF13 DS    CL1                                                              
EQPRD    DS    XL1                                                              
                                                                                
* SAVED EQUIVALENCE AND BASE PRODS TO SAVE I/O'S *                              
                                                                                
EQVPRD   DS    XL1                                                              
BASPRD   DS    XL1                                                              
                                                                                
EQVPRD2  DS    XL1                                                              
BASPRD2  DS    XL1                                                              
                                                                                
CURRKEY  DS   0XL8                 CURRENT KEY                                  
CKCLT    DS    XL2                                                              
CKPRD    DS    XL1                                                              
CKMKT    DS    XL2                                                              
CKSTA    DS    XL3                                                              
                                                                                
CKCOPY   DS    XL1                                                              
                                                                                
PRTSW    DS    XL1                                                              
PRGSW    DS    XL1                                                              
*                        80 - STATION DELETED, DROP FROM TABLE                  
*                        40 -                                                   
*                        20 - BUY ACTIVITY                                      
*                        10 - TABLE UPDATED, DO SVTWA                           
*                        08 - TABLE BUILT FOR VR OR LR RTNS                     
*                        04 - NEW STARTING MARKET ENTERED                       
*                        02 - OPTION ADD - DON'T DISPLAY ANY OLD                
*                        01 - IN MARKET GROUP ADD, MORE TO DO                   
LASTKEY  DS    CL13                                                             
SVDSPMKT DS    XL2                                                              
FILTERS  DS   0CL16                                                             
BMKTFTR  DS    XL2                                                              
MKTFTR   DS    CL4                                                              
MKTSFTR  DS    CL1                                                              
SLNFTR   DS    XL1                                                              
DATEFTR  DS    XL3                                                              
EDATEFTR DS    XL3                                                              
DATESFTR DS    CL1                                                              
HOLDSIGN DS    CL1                                                              
                                                                                
SVTBPR04 DS    CL1                                                              
                                                                                
ELEMPTR  DS    H                                                                
TABPTR   DS    H                                                                
TABCTR   DS    H                                                                
MGRPSV   DS    CL5' '                                                           
*                      HOLDS 400 ENTRIES FOR LIST                               
MGRPTABL DS   0XL800                                                            
*                      HOLDS 1010 ENTRIES MAX                                   
TABLE    DS    CL6006                                                           
                                                                                
ENDSYSD  EQU   *      IF THIS ADDRESS EXCEEDS 2190, PAST END OF                 
*                     SAVED TWA (7268 = X'1C64')+X'52C' (ADDR OF STACT)         
                                                                                
* DSECT FOR MARKET/STATION TABLE (TO VALIDATE ALL BEFORE UPDATING DISK)         
                                                                                
TABLED   DSECT                                                                  
TABENT   DS    0CL6                                                             
TABMKT   DS    0XL2                                                             
TABMKST  DS    XL5                                                              
TABMFLG  DS    XL1                                                              
*                                  80 - OLD ENTRY ALREADY EXISTS                
*                                  40 - ENTRY FROM MARKET/STATION LIST          
*                                  08 - DELETE THIS STATION                     
TABNEXT  EQU   *                                                                
         EJECT                                                                  
* OFFLINE REPORT                                                                
                                                                                
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PMKT     DS    CL4                                                              
         DS    CL5                                                              
PMKTNM   DS    CL24                                                             
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL3                                                              
PPRDSLN  DS    CL7                                                              
         DS    CL3                                                              
PPTRSLN  DS    CL7                                                              
         DS    CL8                                                              
PCOPYCDE DS    CL1                                                              
         DS    CL8                                                              
PPERIOD  DS    CL17                                                             
                                                                                
* ONLINE LIST                                                                   
                                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LMKT     DS    CL4                                                              
         DS    CL1                                                              
LMKTNM   DS    CL24                                                             
         DS    CL1                                                              
LSTA     DS    CL7                                                              
         DS    CL2                                                              
LPRDSLN  DS    CL7                                                              
         DS    CL1                                                              
LPTRSLN  DS    CL7                                                              
         DS    CL1                                                              
LCODE    DS    CL3                                                              
         DS    CL1                                                              
LPER     DS    CL15                                                             
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081SPTRA16   07/25/13'                                      
         END                                                                    
