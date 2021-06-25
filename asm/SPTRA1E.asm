*          DATA SET SPTRA1E    AT LEVEL 069 AS OF 03/30/15                      
*PHASE T2161EA                                                                  
*INCLUDE GETBROAD                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T2161E - TRAFFIC NETWORK SCHEDULE UNIT BUYS                 *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM WILL ASSIGN SCHEDULE UNITS TO ALLOW         *         
*            NETWORK INSTRUCTIONS TO BE RUN WITHOUT MEDIA DEPT        *         
*            BUYS BEING ENTERED - LIKE THE TRAFFIC SPOT BUY SYSTEM.   *         
*                                                                     *         
*  CALLED FROM: TRAFFIC CONTROLLER (T21600), WHICH CALLS              *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    NETIO (T00A27)                                        *         
*                                                                     *         
*  INPUTS: SEE SCREEN SPTRABE (T216BE)                                *         
*          SPTRAWORKD (SYSD)                                          *         
*          DDSPLWORKD (GEND)                                          *         
*                                                                     *         
*  OUTPUTS: UPDATED NETWORK UNIT RECORDS                              *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - POINTER TO NETBLOCKD                                  *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG                                                        
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 -                                                  *         
*             AIO3 - NETIO                                            *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
*  LEV 47 MAY19/86 FIX 2 BUGS-DATE AND VPER PRINT                     *         
*  LEV 48    JAN08/87 ADDED OFFICE PROFILE                            *         
*  LEV 49    MAR10/87 FIX AGENCY ALPHA IN RECS                        *         
*  LEV 50-51 APR25/88 ERROR MSG (NOT DUMP) FOR MORE THAN 26 SUBLINES  *         
*  LEV 52    JUL11/89 CK EQUAL PRD PTR ERROR                          *         
*  LEV 53    DEC11/90 ADD PROGRAM EQUIVALENCY                         *         
*  LEV 54 EJOR JUL16/92 SET STATUS BYTE FOR MEDIA ON ADD              *         
*  LEV 55 SMUR AUG04/95 FIX P/B PRD LENGTH                            *         
*  LEV 56 SMUR AUG08/95 IF P/B & PRD 1 LEN =0 THEN TOTAL LEN/2        *         
*  LEV 57 BGRI NOV03/98 ADD AN 02 ELEM FOR TALENT TRANSFER            *         
*  LEV 58 BGRI DEC15/98 ADD A 21 ELEM                                 *         
*  LEV 59 SMUR APR10/01 USE TRAFFIC OFFICE                            *         
*  LEV 61 SMUR OCT10/03 BRAND LEVEL SECURITY                          *         
*  LEV 62 SMUR JUL26/04 SOX                                           *         
*  LEV 63 BGRI SEP07/05 MORE PRODUCTS                                 *         
*  LEV 64 MNAS          FIX DISPLAY OF 12 CHAR COMMERCIAL CODES       *         
*  LEV 65 MNAS JUL07/08 NEW TRAFFIC TYPE ON MASTER RECORD             *         
*  LEV 66 SMUR JUN19/09 DISABLE REPORTING - TELL'EM TO RUN NET LIST   *         
*  LEV 67 SMUR JUN30/09 BUG FIX OF DISPLAY 12 CHAR COMML CODES        *         
*                       ADD NEW FILTER "ADID"                         *         
*  LEV 68 MNAS JUN11/13 PROFILE TO INCLUDE/EXCLUDE DIGITAL            *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2161E NETWORK TRAFFIC - SCHEDULE UNIT BUYS'                    
T2161E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SKED**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR1ERR                                                      
         L     R3,=A(STNETBLK-SYSD)                                             
         AR    R3,R9                                                            
         USING NETBLOCKD,R3                                                     
         LA    R1,TOTUNITS                                                      
         ST    R1,ASVSTOR                                                       
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
*NOP     BE    LR                                                               
         BNE   MAIN05                                                           
         MVC   GERROR,=Y(RUNETLST)                                              
         GOTO1 VTRAERR                                                          
                                                                                
MAIN05   CLI   MODE,XRECADD        AFTER ADD REC                                
         BE    AAR                                                              
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
*------------------                                                             
* VALIDATE KEYS                                                                 
*------------------                                                             
                                                                                
VK       DS    0H                                                               
         TM    WHEN,X'30'          TEST SOON/OVERNIGHT                          
         BZ    VK01                                                             
         MVC   GERROR,=Y(RUNETLST) TELL THEM TO RUN NET LIST                    
         GOTO1 VTRAERR                                                          
                                                                                
VK01     CLI   ACTNUM,ACTDEL                                                    
         BE    VK01C                                                            
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
VK01C    TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
                                                                                
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
                                                                                
         GOTO1 VSOXERR                                                          
                                                                                
VK02     DS    0H                                                               
         CLI   ACTNUM,ACTCHA       ACTION CHANGE INVALID                        
         BE    CHAERR              TO CHANGE, DELETE AND ADD                    
*                                                                               
         BRAS  RE,INITSPOT         SET FILES TO SPOT                            
*                                                                               
         BAS   RE,VMD              FAKE VALIDATE MEDIA                          
*                                                                               
         LA    R2,TRACLTH                                                       
         GOTO1 VALICLT                                                          
*                                                                               
VK10     XC    WORK,WORK           GET USER PROFILE INTO NBUSER                 
         MVC   WORK(4),=C'S0N0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VK20                                                             
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    VK20                                                             
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
*                                                                               
VK20     GOTO1 GETPROF,DMCB,WORK,NBUSER,DATAMGR PUT PROFILE IN NBUSER           
*                                                                               
         LA    R2,TRANETH          NETWORK                                      
         XC    NETWORK,NETWORK                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, MUST BE ENTRY                            
         BAS   RE,VNET                                                          
*                                                                               
         LA    R2,TRAPROGH         PROGRAM                                      
         XC    PROGRAM,PROGRAM                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK34                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VK40                                                             
         B     MISSERR                                                          
VK34     BAS   RE,VPROG                                                         
*                                                                               
VK40     LA    R2,TRAPERH          PERIOD                                       
         XC    DATES,DATES                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK44                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VK50                                                             
         B     MISSERR                                                          
VK44     BRAS  RE,VPER                                                          
         EJECT                                                                  
* VALIDATE FILTERS *                                                            
*                                                                               
VK50     LA    R2,TRAFLTRH                                                      
         BRAS  RE,VFTR                                                          
*                                                                               
*MNV                                                                            
         TM    SVOPTION,OPTDIGI                                                 
         BO    VK51                                                             
                                                                                
         XC    WORK,WORK           CLEAR                                        
         MVC   WORK(4),=C'STN2'    READ TN2 PROFILE AFTER VNET                  
         NI    WORK,X'FF'-X'40'    MAKE S LOWERCASE                             
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'         BY SPECIFIC MEDIA!!!                         
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         MVC   WORK+16(16),WORK    SAVE PROFILE KEY                             
         L     R1,AIO                                                           
         GOTO1 GETPROF,DMCB,WORK,SVTN2PRO,DATAMGR                               
*                                                                               
         CLI   SVTN2PRO+14,C'Y'                                                 
         BNE   VK51                                                             
         CLI   SVMEDIA,C'V'                                                     
         BNE   VK51                                                             
         LA    R2,TRANETH                                                       
         MVC   GERROR,=Y(BDNETMED)                                              
         GOTO1 VTRAERR                                                          
                                                                                
VK51     DS    0H                                                               
*MNV                                                                            
         CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VK80                                                             
         LA    R2,TRADATEH                                                      
         GOTO1 DATVAL,DMCB,(1,TRADATE),DATE   ACCEPT MONTH/DAY                  
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         CLC   DATE+2(2),STDATE+2                                               
         BNE   VK52                                                             
         MVC   DATE(2),STDATE                                                   
         B     VK54                                                             
VK52     CLC   DATE+2(2),ENDATE+2                                               
         BNE   DTPERER                                                          
         MVC   DATE(2),ENDATE                                                   
VK54     GOTO1 DATCON,(R1),(0,DATE),(2,DATEP)                                   
*                                                                               
         CLC   DATEP,STDATEP       DATE MUST BE IN PERIOD                       
         BL    DTPERER                                                          
         CLC   DATEP,ENDATEP                                                    
         BH    DTPERER                                                          
*                                                                               
         BRAS  RE,INITNET          SET FILES TO NET                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKEY,R4                                                         
         MVI   NUKTYPE,04                                                       
         MVC   NUKAM(3),BAGYMD AND BCLT                                         
         MVC   NUKDATE,DATEP                                                    
*                                                                               
* TIME ZERO                                                                     
*                                                                               
         MVC   NUKNET(10),NETWORK AND PROGRAM                                   
*                                                                               
* ESTIMATE ZERO                                                                 
*                                                                               
* VALIDATE SUBLINE *                                                            
*                                                                               
         LA    R2,TRASUBLH                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    VK58                                                             
         CLI   5(R2),0             IF NO ENTRY, ASSUME A                        
         BE    VK56                                                             
         TM    4(R2),X'04'         MUST BE VALID ALPHA                          
         BZ    BADSUBL                                                          
         CLI   TRASUBL,C'A'                                                     
         BL    BADSUBL                                                          
         CLI   TRASUBL,C'Z'                                                     
         BH    BADSUBL                                                          
         MVC   SUBLINE,TRASUBL                                                  
         B     *+8                                                              
VK56     MVI   SUBLINE,C'A'                                                     
*                                                                               
         MVC   NUKSUB,SUBLINE                                                   
*                                                                               
* DAYPART ZERO                                                                  
*                                                                               
         B     EXIT                                                             
*                                                                               
VK58     MVI   NUKSUB,C'A'                                                      
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   VK70                                                             
VK60     MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         CLC   KEY(18),KEYSAVE                                                  
         BE    VK60                                                             
         MVC   KEY,KEYSAVE                                                      
VK64     ZIC   R1,NUKSUB                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,NUKSUB                                                        
         CLI   NUKSUB,C'I'                                                      
         BNH   VK72                                                             
         CLI   NUKSUB,C'J'                                                      
         BL    VK64                                                             
         CLI   NUKSUB,C'R'                                                      
         BNH   VK72                                                             
         CLI   NUKSUB,C'S'                                                      
         BL    VK64                                                             
         CLI   NUKSUB,C'Z'                                                      
         BH    SUBLERR                                                          
         B     VK72                                                             
*                                                                               
VK70     MVC   KEY,KEYSAVE                                                      
VK72     NI    DMINBTS,X'F7'       SET OFF PASS DELETED RECORDS                 
         MVC   SUBLINE,NUKSUB                                                   
*                                                                               
VK80     BRAS  RE,INITNET          SET FROM SPOT TO NET                         
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*-------------------------------------------                                    
* VALIDATE RECORD (PRODUCT AND PARTNER)                                         
*-------------------------------------------                                    
*                                                                               
VR       DS    0H                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VR01     DS    0H                                                               
         CLI   ACTNUM,ACTCHA       ACTION CHANGE INVALID                        
         BE    CHAERR                                                           
         CLI   ACTNUM,ACTSEL       ACTION CHANGE INVALID                        
         BE    CHAERR                                                           
*                                                                               
* SET STATUS BITS FOR MEDIA                                                     
         L     R1,AIO1                                                          
         LA    R1,NURSTAT-NURECD(R1)  R1 TO STATUS                              
                                                                                
         NI    0(R1),X'FF'-X'03'      START WITH DEFAULT : NET = 00             
         CLI   SVMEDIA,C'N'                                                     
         BE    VR5                                                              
                                                                                
         CLI   SVMEDIA,C'C'                                                     
         BNE   *+12                                                             
         OI    0(R1),X'01'            CABLE = 01                                
         B     VR5                                                              
                                                                                
         CLI   SVMEDIA,C'S'                                                     
         BNE   *+12                                                             
         OI    0(R1),X'02'            SYND = 02                                 
         B     VR5                                                              
                                                                                
         CLI   SVMEDIA,C'O'                                                     
         BNE   *+12                                                             
         OI    0(R1),X'03'            OTHER = 03                                
         B     VR5                                                              
                                                                                
         NI    0(R1),X'FF'-X'03'      SET OFF LOW 2 BITS (JUST IN CASE)         
*                                                                               
VR5      DS    0H                                                               
         L     RE,AIO3             SAVE ANY EXISTING UNIT REC IN AIO3           
         LA    RF,2000                                                          
         L     R2,AIO1                                                          
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
         MVC   AIO,AIO1                                                         
*                                                                               
         BRAS  RE,INITSPOT         SET FROM NET TO SPOT                         
*                                                                               
* VALIDATE PRODUCT/SPOT LENGTH *                                                
*                                                                               
         LA    R2,TRAPRDH                                                       
         GOTO1 VALIPRD                                                          
*                                                                               
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         MVC   BSLN,WORK+4                                                      
         CLI   BSLN,0                                                           
         BE    MISSLNER                                                         
*                                                                               
* VALIDATE PARTNER/SPOT LENGTH *                                                
*                                                                               
         LA    R2,TRAPTRH                                                       
         XC    QPRD2,QPRD2                                                      
         XC    BPRD2(2),BPRD2                                                   
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR10                                                             
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         CLC   =C'POL',WORK                                                     
         BE    PRDERR                                                           
*                                                                               
         CLC   QPRD,WORK                                                        
         BE    EQPRDERR                                                         
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3                                                     
         MVC   BSLN2,WORK+4                                                     
         CLI   BSLN2,0                                                          
         BE    MISSLNER                                                         
*                                                                               
* VALIDATE FEED *                                                               
*                                                                               
VR10     LA    R2,TRAFEEDH                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR20                                                             
         CLI   5(R2),4                                                          
         BH    FEEDLNER                                                         
         GOTO1 ANY                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING FEEDKEY,R4                                                       
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,BAGYMD                                                   
         MVC   FEEDKNET,NETWORK                                                 
         MVC   FEEDKCLT,BCLT                                                    
         MVC   FEEDKFD,WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VR14                                                             
         MVC   KEY,KEYSAVE                                                      
         XC    FEEDKCLT,FEEDKCLT                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FEEDERR                                                          
         DROP  R4                                                               
VR14     MVC   FEED,WORK                                                        
         MVC   AIO,AIO3                                                         
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         MVI   ELCODE,X'22'                                                     
*                                                                               
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING NUFEDEL,R6                                                       
         MVI   NUFEDEL,X'22'                                                    
         MVI   NUFEDLEN,NUFEDELN                                                
         MVC   NUFEEDCD,FEED                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         BRAS  RE,INITSPOT         SET FROM NET TO SPOT                         
*                                                                               
* NOW FIND PROG REC TO COVER THIS DATE *                                        
*                                                                               
VR20     MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,PROGRAM                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
VR30     CLC   NPGKEND,DATEP                                                    
         BNL   VR40                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(11),KEYSAVE                                                  
         BE    VR30                                                             
         B     PRGDATER                                                         
         DROP  R4                                                               
*                                                                               
* SAVE ALL INFO FOR BUILDING UNITS *                                            
*                                                                               
VR40     L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R6                                                            
         USING NPGEL92,R5                                                       
*                                                                               
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
*                                                                               
         MVC   AIO,AIO3                                                         
*                                                                               
         MVI   ELCODE,01                                                        
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING NUMAINEL,R6                                                      
         MVI   NUMAINEL,01                                                      
         MVI   NUMAINLN,80                                                      
*                                                                               
         TM    SECFLAG,NECONPRD  COMPLETELY CONVERTED                           
         BO    VR42                                                             
*                                                                               
         MVC   NUPRD,BPRD                                                       
         MVC   NUPRD2,BPRD2                                                     
*                                                                               
VR42     DS    0H                                                               
         MVC   NUPROGNM,NPGNAME                                                 
         ZIC   RE,BSLN             SPOT LEN FOR PRD                             
         STC   RE,NULEN1                                                        
         ZIC   RF,BSLN2                                                         
         LTR   RF,RF               ANY SPOT LEN FOR PTR                         
         BZ    VR44                NO                                           
*        LR    R1,RE                                                            
*        SR    R0,R0                                                            
*        MH    R1,=H'10000'                                                     
         AR    RE,RF               COMBINE                                      
*        DR    R0,RE               COMPUTE SHARE PERCENT                        
*        STCM  R1,3,NUP1SHR                                                     
VR44     STC   RE,NULEN                                                         
         MVC   NUDAY,NPGDAY                                                     
         MVC   PROGDAY,NPGDAY                                                   
         MVC   NUTIME,NPGTIME                                                   
         MVC   NUALPHA,AGENCY                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,X'19'                                                     
         XC    ELEM,ELEM                                                        
*                                                                               
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING NUPDED,R6                                                        
         MVI   NUPDEEL,X'19'                                                    
         MVI   NUPDELEN,10                                                      
         MVC   NUPDEPR,QPRD                                                     
*                                                                               
         OC    QPRD2,QPRD2         ANY PARTNER PROD                             
         BZ    VR46                NO                                           
*                                                                               
         MVI   NUPDELEN,17                                                      
         MVC   NUPDEPR+7(L'QPRD2),QPRD2                                         
*                                                                               
VR46     DS    0H                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR50     DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING NUSDRD,R6                                                        
         MVI   NUSDREL,02                                                       
         MVI   NUSDRLEN,20                                                      
         MVC   NUSTATYP,SVPOST                                                  
         MVC   NUSDSBMD,SVMEDIA                                                 
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,X'21'                                                     
         XC    ELEM,ELEM                                                        
*                                                                               
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING NUCMLEL,R6                                                       
         MVI   NUCMLEID,X'21'                                                   
         MVI   NUCMLELN,NUCMLEND-NUCMLEL                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         B     DR                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
*----------------------------------                                             
* DISPLAY KEY                                                                   
*----------------------------------                                             
*                                                                               
DK       L     R4,AIO                                                           
         USING NUKEY,R4                                                         
         XC    FLD(L'TRACLT),FLD                                                
         GOTO1 CLUNPK,DMCB,NUKCLT,FLD                                           
         CLC   TRACLT,FLD                                                       
         BE    *+14                                                             
         MVC   TRACLT,FLD                                                       
         OI    TRACLTH+6,X'80'                                                  
         XC    FLD(L'TRANET),FLD                                                
         MVC   FLD(4),NUKNET                                                    
         CLC   TRANET,FLD                                                       
         BE    *+14                                                             
         MVC   TRANET,FLD                                                       
         OI    TRANETH+6,X'80'                                                  
         XC    FLD(L'TRAPROG),FLD                                               
         MVC   FLD(6),NUKPROG                                                   
         CLC   TRAPROG,FLD                                                      
         BE    *+14                                                             
         MVC   TRAPROG,FLD                                                      
         OI    TRAPROGH+6,X'80'                                                 
         XC    FLD(L'TRAPER),FLD                                                
         GOTO1 DATCON,DMCB,(2,NUKDATE),(6,FLD)                                  
         CLC   TRAPER,FLD                                                       
         BE    *+14                                                             
         MVC   TRAPER,FLD                                                       
         OI    TRAPERH+6,X'80'                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*---------------------                                                          
* DISPLAY RECORD                                                                
*---------------------                                                          
*                                                                               
DR       DS   0H                                                                
         L     R4,AIO                                                           
*                                                                               
         USING NUKEY,R4                                                         
         XC    FLD(L'TRADATE),FLD                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(5,FLD)                                  
         CLC   TRADATE,FLD                                                      
         BE    *+14                                                             
         MVC   TRADATE,FLD                                                      
         OI    TRADATEH+6,X'80'                                                 
*                                                                               
         XC    FLD(L'TRASUBL),FLD                                               
         MVC   FLD(1),NUKSUB                                                    
         CLC   TRASUBL,FLD                                                      
         BE    *+14                                                             
         MVC   TRASUBL,FLD                                                      
         OI    TRASUBLH+6,X'80'                                                 
*                                                                               
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
         MVI   BSLN,0                                                           
         MVI   BSLN2,0                                                          
         LR    R6,R4                                                            
         MVI   ELCODE,X'19'        NEW PROD ELEM                                
         BAS   RE,GETEL                                                         
         BNE   DR01                                                             
*                                                                               
         USING NUPDED,R6                                                        
         MVC   QPRD,NUPDEPR                                                     
*                                                                               
         CLI   NUPDELEN,10                                                      
         BE    DR01                                                             
         CLI   NUPDELEN,17                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   QPRD2,NUPDEPR+7                                                  
         DROP  R6                                                               
*                                                                               
DR01     DS    0H                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,01                                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R6                                                      
*                                                                               
DR02     DS    0H                                                               
         OC    QPRD2,QPRD2         IS THERE A PARTNER (NEW STYLE)               
         BNZ   DR03                 YES                                         
*                                                                               
         CLI   NUPRD2,0            PARTNER                                      
         BE    DR10                 NO                                          
*                                                                               
DR03     DS    0H                                                               
         ZIC   R1,NULEN1           LENGTH OF THE 1ST PROD                       
         CLI   NULEN1,0            IF PRD 1 LEN IS ZERO                         
         BNE   DR08                                                             
         ZIC   R1,NULEN            THEN TOTAL LEN                               
         SRL   R1,1                DIVIDE BY 2                                  
         LR    R2,R1                                                            
         B     DR09                                                             
*                                                                               
DR08     ZIC   R2,NULEN                                                         
         SR    R2,R1                                                            
DR09     DS    0H                                                               
         STC   R2,BSLN2                                                         
         STC   R1,BSLN                                                          
         B     DR11                                                             
*                                                                               
DR10     MVC   BSLN,NULEN                                                       
*                                                                               
DR11     DS    0H                                                               
         OC    QPRD,QPRD           FOUND PROD?                                  
         BNZ   DR12C                                                            
*                                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
DR12     CLC   NBPRD,3(R1)                                                      
         BE    DR12A                                                            
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,DR12                                                          
*                                                                               
         DC    H'0'                                                             
*                                                                               
DR12A    MVC   QPRD,0(R1)                                                       
*                                                                               
DR12C    DS    0H                                                               
         BRAS  RE,INITSPOT         SET FROM NET TO SPOT                         
         XC    TRAPRD,TRAPRD                                                    
         OI    TRAPRDH+6,X'80'                                                  
         LA    R2,TRAPRDH                                                       
         MVC   TRAPRD(3),QPRD                                                   
         LA    R5,TRAPRD+2                                                      
         CLI   0(R5),C' '                                                       
         BNH   *+8                                                              
         AHI   R5,1                                                             
         MVI   0(R5),C'-'                                                       
         EDIT  (B1,BSLN),(3,1(R5)),ALIGN=LEFT                                   
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    DR13                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 VALIPRD                                                          
         MVC   AIO,AIO3                                                         
*                                                                               
DR13     DS   0H                                                                
         XC    TRAPTR,TRAPTR                                                    
         OI    TRAPTRH+6,X'80'                                                  
*                                                                               
         OC    QPRD2,QPRD2         FOUND PROD 2?                                
         BNZ   DR13J                YES                                         
*                                                                               
         CLI   NBPRD2,0            ANY PTR PROD                                 
         BE    DR15                 NO                                          
*                                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
DR13F    CLC   NBPRD2,3(R1)                                                     
         BE    DR13H                                                            
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,DR13F                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
DR13H    DS    0H                                                               
         MVC   QPRD2,0(R1)                                                      
*                                                                               
DR13J    DS    0H                                                               
*        BRAS  RE,INITSPOT         SET FROM NET TO SPOT                         
         OC    QPRD2,QPRD2         ANYTHING TO SHOW                             
         BZ    DR15                 NO                                          
         LA    R2,TRAPTRH                                                       
         MVC   TRAPTR(3),QPRD2                                                  
         LA    R5,TRAPTR+2                                                      
         CLI   0(R5),C' '                                                       
         BNH   *+8                                                              
         AHI   R5,1                                                             
         MVI   0(R5),C'-'                                                       
         EDIT  (B1,BSLN2),(3,1(R5)),ALIGN=LEFT                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
DR15     DS    0H                                                               
         XC    TRAFEED,TRAFEED                                                  
         OI    TRAFEEDH+6,X'80'                                                 
*                                                                               
         MVI   ELCODE,X'22'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         BRAS  RE,INITSPOT         SET FROM NET TO SPOT                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING FEEDKEY,R2                                                       
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,BAGYMD                                                   
         MVC   FEEDKNET,NUKNET                                                  
         MVC   FEEDKCLT,BCLT                                                    
         MVC   FEEDKFD,2(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DR16                                                             
         MVC   KEY,KEYSAVE                                                      
         XC    FEEDKCLT,FEEDKCLT                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
DR16     XC    FLD(L'TRAFEED),FLD                                               
         MVC   FLD(4),FEEDKFD                                                   
         DROP  R2                                                               
         CLC   TRAFEED,FLD                                                      
         BE    *+14                                                             
         MVC   TRAFEED,FLD                                                      
         OI    TRAFEEDH+6,X'80'                                                 
*                                                                               
DR20     MVI   ELCODE,X'21'                                                     
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
*                                                                               
* EDIT CML ELEMENT (IF ANY) HERE - FUTURE USE IF ANY *                          
*                                                                               
DR30     MVC   KEY(20),0(R4)                                                    
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    EXIT                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*-------------------------------------------------                              
* ADD PASSIVE POINTER FOR NEW UNIT RECORD                                       
*-------------------------------------------------                              
*                                                                               
AAR      MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKEY,R4                                                         
         MVI   NUKTYPE,04                                                       
         MVC   NUKAM(3),BAGYMD AND BCLT                                         
         MVC   NUKDATE,DATEP                                                    
* TIME ZERO                                                                     
         MVC   NUKNET(10),NETWORK AND PROGRAM                                   
* EST ZERO                                                                      
         MVC   NUKSUB,SUBLINE                                                   
* DP ZERO                                                                       
         GOTO1 HIGH                GET DISK ADDR FOR ADDED REC                  
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NUKPKEY,R4                                                       
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM(3),BAGYMD AND BCLT                                        
         MVC   NUKPNET(10),NETWORK AND PROGRAM                                  
         MVI   NUKPEST,0                                                        
         MVC   NUKPDATE,DATEP                                                   
         MVI   NUKPEST,0                                                        
         MVC   NUKPSUB,SUBLINE                                                  
         MVI   NUKPDP,0                                                         
* DP ZERO                                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'UNTDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUKDKEY,R4                                                       
         MVI   NUKDTYPE,X'94'                                                   
         MVC   NUKDAM(3),BAGYMD AND BCLT                                        
         MVI   NUKDEST,0                                                        
         MVC   NUKDNET,NETWORK                                                  
         MVC   NUKDDAY,PROGDAY                                                  
         MVI   NUKDTIME,0                                                       
         MVC   NUKDPROG,PROGRAM                                                 
         MVC   NUKDDATE,DATEP                                                   
         MVC   NUKDSUB,SUBLINE                                                  
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'UNTDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*---------------------------------------------------                            
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*---------------------------------------------------                            
*                                                                               
LR       OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR10                NO, GET THIS KEY                             
         L     R0,=A(HEADING)      HEADING LINE FOR REPORT                      
         A     R0,SPTR1ERR                                                      
         ST    R0,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
*                                                                               
         XC    0(256,R3),0(R3)                                                  
         XC    256(256,R3),256(R3)                                              
         XC    512(256,R3),512(R3)                                              
         XC    768(256,R3),768(R3)                                              
         XC    TOTUNITS,TOTUNITS                                                
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BNE   LR10                GO DO ONLINE LIST                            
         BAS   RE,SVTWA                                                         
*                                                                               
LR10     CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LRL                 GO DO ONLINE LIST                            
*NOP     CLI   MODE,PRINTREP       OFFLINE REPORT                               
******   BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
*                                                                               
* FORMAT OFFLINE REPORT                                                         
*                                                                               
LRR      BAS   RE,NETI                                                          
         MVC   NBSELNET,NETWORK                                                 
         MVC   NBSELPRG,PROGRAM                                                 
         MVC   NBSELSTR(12),STDATE                                              
LRR04    GOTO1 ANETIO,DMCB,(R3)                                                 
         CLI   NBERROR,NBGOOD                                                   
         BE    LRR06                                                            
         DC    H'0'                                                             
LRR06    CLI   NBMODE,NBREQLST                                                  
         BE    LRR50                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRR04                                                            
         B     LRR20                                                            
LRR10    GOTO1 ANETIO,DMCB,(R3)                                                 
         CLI   NBERROR,NBGOOD                                                   
         BE    LRR14                                                            
         DC    H'0'                                                             
LRR14    TM    NBSUBMSK,NBSBMCLI                                                
         BO    LRR50                                                            
         OC    NETWORK,NETWORK                                                  
         BZ    LRR16                                                            
         TM    NBSUBMSK,NBSBMNET                                                
         BO    LRR50                                                            
         OC    NBSELPRG,NBSELPRG                                                
         BZ    LRR16                                                            
         TM    NBSUBMSK,NBSBMPRG                                                
         BO    LRR50                                                            
LRR16    CLI   NBMODE,NBREQLST                                                  
         BE    LRR50                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRR10                                                            
LRR20    L     R6,NBAIO                                                         
         BRAS  RE,INITNET          SET FILES TO NET                             
*                                                                               
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
         MVI   BSLN,0                                                           
         MVI   BSLN2,0                                                          
*                                                                               
         MVI   ELCODE,X'19'        NEW PROD ELEM                                
         BAS   RE,GETEL                                                         
         BNE   LRR22                                                            
*                                                                               
         USING NUPDED,R6                                                        
         MVC   QPRD,NUPDEPR                                                     
*                                                                               
         CLI   NUPDELEN,10                                                      
         BE    LRR22                                                            
         CLI   NUPDELEN,17                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   QPRD2,NUPDEPR+7                                                  
         DROP  R6                                                               
*                                                                               
LRR22    DS    0H                                                               
         BAS   RE,LFTR             FILTER                                       
         BNE   LRR10                                                            
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,01                                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R6                                                      
*                                                                               
         OC    QPRD,QPRD                                                        
         BNZ   LRR23                                                            
*                                                                               
         CLI   NUPRD,0             UNALLOCATED                                  
         BE    LRR10                                                            
*                                                                               
LRR23    DS    0H                                                               
         CLI   TRAPERH+5,0         ANY DATE                                     
         BE    LRR24                                                            
         CLC   STDATEP,NBACTDAT                                                 
         BH    LRR10                                                            
         CLC   ENDATEP,NBACTDAT                                                 
         BL    LRR10                                                            
LRR24    MVC   PPROG,NUPROGNM                                                   
*                                                                               
* CALC UNIT LENGTH *                                                            
*                                                                               
         OC    QPRD2,QPRD2         PARTNER?                                     
         BNZ   LRR25                YES                                         
*                                                                               
         CLI   NBPRD2,0            PARTNER                                      
         BE    LRR30                NO                                          
*                                                                               
LRR25    DS    0H                                                               
         ZIC   R1,NBLEN1           LEN OF 1ST PRD                               
         CLI   NBLEN1,0            IF PRD 1 LEN IS ZERO                         
         BNE   LRR26                NO, COMPUTE IT                              
*                                                                               
         ZIC   R1,NBLEN            THEN TOTAL LEN                               
         SRL   R1,1                DIVIDE BY 2                                  
         LR    R2,R1                                                            
         B     LRR27                                                            
*                                                                               
LRR26    ZIC   R2,NBLEN                                                         
         SR    R2,R1                                                            
*                                                                               
LRR27    DS   0H                                                                
         STC   R1,BSLN                                                          
         STC   R2,BSLN2                                                         
         B     LRR31                                                            
*                                                                               
LRR30    DS   0H                                                                
         MVC   BSLN,NBLEN                                                       
*                                                                               
LRR31    DS   0H                                                                
         OC    QPRD,QPRD           NEW STYLE PROD                               
         BNZ   LRR32                NO                                          
*                                                                               
         CLI   NBPRD,0             ANY PROD AT ALL?                             
         BE    LRR10                NO                                          
*                                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
LRR31C   CLC   NBPRD,3(R1)                                                      
         BE    LRR31E                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,LRR31C                                                        
*                                                                               
         DC    H'0'                                                             
*                                                                               
LRR31E   DS    0H                                                               
         MVC   QPRD,0(R1)                                                       
*                                                                               
LRR32    DS    0H                                                               
         XC    FLD,FLD                                                          
*                                                                               
         LA    R2,FLDH                                                          
         MVC   FLDH,=X'0A01000184070001'                                        
         MVC   FLD(3),QPRD                                                      
         LA    R5,FLD+2                                                         
         CLI   0(R5),C' '                                                       
         BNH   *+8                                                              
         AHI   R5,1                                                             
         MVI   0(R5),C'-'                                                       
         EDIT  (B1,BSLN),(3,1(R5)),ALIGN=LEFT                                   
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    LRR33                                                            
*                                                                               
         MVI   ERROPT,C'Y'                                                      
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   LRR10                YES, GET NEXT                               
*                                                                               
LRR33    DS    0H                                                               
         MVC   PPROD,FLD                                                        
*                                                                               
         BRAS  RE,INITSPOT         SET TO SPOT FILES                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD                                                  
         MVC   KEY+4(3),QPRD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         USING PRDHDRD,R1                                                       
         MVC   PPRODNM,PNAME                                                    
*                                                                               
         XC    FLD,FLD                                                          
*                                                                               
         OC    QPRD2,QPRD2         IS THERE A NEW STYLE PTR                     
         BNZ   LRR36                YES                                         
*                                                                               
         CLI   NUPRD2,0                                                         
         BE    LRR40                                                            
*                                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
LRR34    CLC   NBPRD2,3(R1)                                                     
         BE    LRR34C                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,LRR34                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
LRR34C   DS    0H                                                               
         MVC   QPRD2,0(R1)                                                      
*                                                                               
LRR36    DS    0H                                                               
         MVC   FLD(3),QPRD2                                                     
         LA    R5,FLD+2                                                         
         CLI   0(R5),C' '                                                       
         BNH   *+8                                                              
         AHI   R5,1                                                             
         MVI   0(R5),C'-'                                                       
         EDIT  (B1,BSLN2),(3,1(R5)),ALIGN=LEFT                                  
*                                                                               
         MVC   PPROD,FLD                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD                                                  
         MVC   KEY+4(3),QPRD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         USING PRDHDRD,R1                                                       
         MVC   PPRODNM+132,PNAME                                                
         DROP  R1,R6                                                            
LRR40    MVC   PCMML+3(3),=C'N/A'                                               
         L     R6,NBAIO                                                         
         BAS   RE,GETEL                                                         
         BNE   LRR48                                                            
         LR    R2,R6                                                            
         USING NUCMLEL,R2                                                       
*                                                                               
         TM    NUCMLFLG,X'E0'      ANY CHANGE                                   
         BZ    *+14                                                             
         MVC   PCMML(15),=CL15'REASSIGN NEEDED'                                 
         B     LRR48                                                            
*                                                                               
         BRAS  RE,INITSPOT         SET TO SPOT FILES                            
         OC    NUCML1,NUCML1                                                    
         BZ    LRR44                                                            
         MVC   PCMML,NUCML1                                                     
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,NUCML1                                                   
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       DELETED                                      
         BZ    *+20                                                             
         MVC   PCMML(15),=CL15'REASSIGN NEEDED'                                 
         MVC   PCMMLT,=CL15'** DELETED **'                                      
         B     LRR44                                                            
         MVC   PCMMLT,CMLTITLE                                                  
         DROP  R6                                                               
*                                                                               
LRR44    OC    NUCML2,NUCML2                                                    
         BZ    LRR46                                                            
         MVC   PCMML+132,NUCML2                                                 
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,NUCML2                                                   
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       DELETED                                      
         BZ    *+14                                                             
         MVC   PCMMLT,=CL15'** DELETED **'                                      
         B     LRR46                                                            
         MVC   PCMMLT+132,CMLTITLE                                              
         DROP  R6                                                               
LRR46    MVC   PPOS,NUCMLPOS                                                    
         DROP  R2                                                               
*                                                                               
LRR48    BRAS  RE,INITNET          SET TO NET FILES                             
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   PFEED,2(R6)                                                      
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,PDATE1)                              
         MVC   PDATE1+8(1),NBACTSUB                                             
         GOTO1 UNDAY,DMCB,NBDAY,PDAY                                            
         GOTO1 UNTIME,DMCB,NBTIME,PTIME                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRR10                                                            
LRR50    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*-------------------------------                                                
* FORMAT ONLINE LIST                                                            
*-------------------------------                                                
*                                                                               
LRL      BAS   RE,RDTWA                                                         
         OC    TOTUNITS,TOTUNITS   IN MIDDLE OF LIST                            
         BNZ   LRL04               YES                                          
         L     R3,=A(STNETBLK-SYSD)                                             
         AR    R3,R9                                                            
         USING NETBLOCKD,R3                                                     
*                                                                               
         XC    0(256,R3),0(R3)                                                  
         XC    256(256,R3),256(R3)                                              
         XC    512(256,R3),512(R3)                                              
         XC    768(256,R3),768(R3)                                              
*                                                                               
         BAS   RE,NETI                                                          
         MVC   NBSELNET,NETWORK                                                 
         MVC   NBSELPRG,PROGRAM                                                 
         MVC   NBSELSTR(12),STDATE                                              
*                                                                               
LRL02    GOTO1 ANETIO,DMCB,(R3)                                                 
         MVI   NBFUNCT,NBFNORM                                                  
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LRL50                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRL02                                                            
         B     LRL20                                                            
*                                                                               
LRL04    BAS   RE,NETR                                                          
         MVI   NBFUNCT,NBFRDHI                                                  
LRL06    GOTO1 ANETIO,DMCB,(R3)                                                 
         MVI   NBFUNCT,NBFNORM                                                  
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LRL50                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRL06                                                            
         B     LRL14                                                            
*                                                                               
LRL10    GOTO1 ANETIO,DMCB,(R3)                                                 
         CLI   NBERROR,NBGOOD                                                   
         BE    LRL14                                                            
         DC    H'0'                                                             
LRL14    TM    NBSUBMSK,NBSBMCLI                                                
         BO    LRL50                                                            
         OC    NETWORK,NETWORK                                                  
         BZ    LRL16                                                            
         TM    NBSUBMSK,NBSBMNET                                                
         BO    LRL50                                                            
         OC    NBSELPRG,NBSELPRG                                                
         BZ    LRL16                                                            
         TM    NBSUBMSK,NBSBMPRG                                                
         BO    LRL50                                                            
*                                                                               
LRL16    CLI   NBMODE,NBREQLST                                                  
         BE    LRL50                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRL10                                                            
LRL20    L     R6,NBAIO                                                         
         BRAS  RE,INITNET          SET FILES TO NET                             
*                                                                               
         BAS   RE,LFTR             FILTER                                       
         BNE   LRL10                                                            
*                                                                               
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
         MVI   BSLN,0                                                           
         MVI   BSLN2,0                                                          
*                                                                               
         MVI   ELCODE,X'19'        NEW PROD ELEM                                
         BAS   RE,GETEL                                                         
         BNE   LRL22                                                            
*                                                                               
         USING NUPDED,R6                                                        
         MVC   QPRD,NUPDEPR                                                     
*                                                                               
         CLI   NUPDELEN,10                                                      
         BE    LRL22                                                            
         CLI   NUPDELEN,17                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   QPRD2,NUPDEPR+7                                                  
         DROP  R6                                                               
*                                                                               
LRL22    L     R6,NBAIO                                                         
         MVI   ELCODE,01                                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R6                                                      
         OC    QPRD,QPRD           PROD FOUND ALREADY?                          
         BNZ   LRL23                YES                                         
*                                                                               
         CLI   NUPRD,0             UNALLOCATED                                  
         BE    LRL10                                                            
*                                                                               
LRL23    DS    0H                                                               
         CLI   TRAPERH+5,0         ANY DATE                                     
         BE    LRL24                                                            
         CLC   STDATEP,NBACTDAT                                                 
         BH    LRL10                                                            
         CLC   ENDATEP,NBACTDAT                                                 
         BL    LRL10                                                            
*                                                                               
LRL24    MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   LNET,NBACTNET                                                    
         MVC   LPROG,NBACTPRG                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,LDATE)                               
*                                                                               
         MVC   LSUB,NBACTSUB                                                    
*                                                                               
         OC    QPRD2,QPRD2         PROD FOUND ALREADY?                          
         BNZ   *+12                                                             
*                                                                               
         CLI   NBPRD2,0            PARTNER                                      
         BE    LRL30                                                            
*                                                                               
         ZIC   R1,NBLEN1           LEN OF 1ST PRD                               
         CLI   NBLEN1,0            IF PRD 1 LEN IS ZERO                         
         BNE   LRL26                                                            
         ZIC   R1,NBLEN            THEN TOTAL LEN                               
         SRL   R1,1                DIVIDE BY 2                                  
         LR    R2,R1                                                            
         B     LRL27                                                            
*                                                                               
LRL26    ZIC   R2,NBLEN                                                         
         SR    R2,R1                                                            
*                                                                               
LRL27    DS   0H                                                                
         STC   R1,BSLN                                                          
         STC   R2,BSLN2                                                         
         B     LRL31                                                            
*                                                                               
LRL30    DS   0H                                                                
         MVC   BSLN,NBLEN                                                       
         DROP  R6                                                               
*                                                                               
LRL31    DS    0H                                                               
         OC    QPRD,QPRD           ALREADY HAVE PROD?                           
         BNZ   LRL33C                                                           
*                                                                               
         CLI   NBPRD,0             BINARY PROD                                  
         BE    LRL10                NO, BYPASS                                  
*                                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
LRL32    CLC   NBPRD(1),3(R1)                                                   
         BE    LRL33                                                            
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,LRL32                                                         
         B     LRL10                                                            
*                                                                               
LRL33    MVC   QPRD,0(R1)                                                       
*                                                                               
LRL33C   DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    LRL34                                                            
*                                                                               
         XC    FLD,FLD                                                          
*                                                                               
         LA    R2,FLDH                                                          
         MVC   FLDH,=X'0A01000184070001'                                        
         MVC   FLD(3),QPRD                                                      
         LA    R5,FLD+2                                                         
         CLI   0(R5),C' '                                                       
         BNH   *+8                                                              
         AHI   R5,1                                                             
         MVI   0(R5),C'-'                                                       
         EDIT  (B1,BSLN),(3,1(R5)),ALIGN=LEFT                                   
*                                                                               
         MVI   ERROPT,C'Y'                                                      
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   LRR10                YES, GET NEXT                               
*                                                                               
LRL34    LA    R5,LPROD                                                         
         LA    R1,QPRD                                                          
         ZIC   R0,BSLN                                                          
         BAS   RE,PPRD                                                          
*                                                                               
         OC    QPRD2,QPRD2         PROD FOUND ALREADY?                          
         BNZ   LRL37                                                            
*                                                                               
         CLI   NBPRD2,0                                                         
         BE    LRL38                                                            
*                                                                               
         MVC   FLDH(1),NBPRD2                                                   
         STC   R2,FLDH+1                                                        
*                                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
LRL35    CLC   FLDH(1),3(R1)                                                    
         BE    LRL36                                                            
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,LRL35                                                         
         B     LRL10                                                            
*                                                                               
LRL36    DS    0H                                                               
         MVC   QPRD2,0(R1)                                                      
*                                                                               
LRL37    LA    R5,LPPTR                                                         
         LA    R1,QPRD2                                                         
         ZIC   R0,BSLN2                                                         
         BAS   RE,PPRD                                                          
*                                                                               
LRL38    GOTO1 UNDAY,DMCB,NBDAY,LDAY                                            
         GOTO1 UNTIME,DMCB,NBTIME,LTIME                                         
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRL40                                                            
         MVC   LFEED,2(R6)                                                      
*                                                                               
LRL40    L     R6,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRL44                                                            
         USING NUCMLEL,R6                                                       
         TM    NUCMLFLG,X'E0'      ANY CHANGE                                   
         BZ    LRL42                                                            
         MVC   LCML,=CL15'REASSIGN NEEDED'                                      
         B     LRL44                                                            
                                                                                
LRL42    DS    0H                                                               
         MVC   LCML,NUCML1                                                      
         TM    SVOPTION,OPTADISW   WAS ADID FILTER ENTERED                      
         BZ    LRL42C                                                           
         MVC   LCML,SPACES                                                      
         MVC   LFEED(8),NUCML1     YES, SHOW CMLS OVER FEED                     
                                                                                
LRL42C   TM    NUCMADFL,NUCMADF1   IS THIS ADID                                 
         BZ    LRL44                                                            
                                                                                
         TM    SVOPTION,OPTADISW   AND ADID FILTER ENTERED                      
         BZ    LRL43                NO                                          
                                                                                
         GOTO1 VTRPACK,DMCB,(C'U',NUCML1),SVADID                                
         MVC   LFEED(L'SVADID),SVADID                                           
         B     *+10                                                             
LRL43    MVC   LCML,=C'SEE ADID'                                                
                                                                                
*!! THIS WILL NEVER WORK (WORK < KEY+KEYSAVE)                                   
*NOP     MVC   WORK(L'KEY+L'KEYSAVE),KEY                                        
*        MVC   WORK(L'KEY),KEY                                                  
         BRAS  RE,INITSPOT                                                      
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLPIDAD,=XL2'0AC1'                                              
         MVC   CMLKAM(3),BAGYMD     MOVES IN AGENCY/MEDIA/CLIENT                
         MVC   CMLKCML,NUCML1                                                   
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    LRL44                                                            
                                                                                
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVI   KEY+1,X'21'         READ 0A21                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    LRL44                                                            
         MVC   LCML(7),=CL7'NOT FND'                                            
                                                                                
*        MVC   LCML,CMLKCML                                                     
*        MVC   KEY(L'KEY+L'KEYSAVE),WORK                                        
*        MVC   KEY,WORK                                                         
         BRAS  RE,INITNET                                                       
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LRL44    L     R1,TOTUNITS                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,TOTUNITS                                                      
         MVC   KEY,NBKEY                                                        
         MVC   DMDSKADD,KEY+21                                                  
         BAS   RE,SVTWA                                                         
         GOTO1 LISTMON                                                          
         B     LRL10                                                            
*                                                                               
LRL50    XC    TOTUNITS,TOTUNITS  SET END OF LIST-NOT IN MIDST OF LIST          
         BAS   RE,SVTWA                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*----------------------------------------                                       
* FAKE VALIDATE MEDIA                                                           
*----------------------------------------                                       
*                                                                               
VMD      NTR1                                                                   
         LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
         B     EXIT                                                             
*                                                                               
*----------------------------------------                                       
* VALIDATE NETWORK                                                              
*----------------------------------------                                       
*                                                                               
VNET     NTR1                                                                   
         GOTO1 ANY                                                              
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   NETERR                                                           
         MVC   NETWORK,WORK                                                     
         L     R4,AIO                                                           
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,NETMKT                                                        
*                                                                               
* SAVE OFF MEDIA FOR STATUS BYTE IN KEY                                         
*                                                                               
         MVC   SVPOST,STRTYPE                                                   
         MVC   SVMEDIA,STRTYPE                                                  
         B     EXIT                                                             
         EJECT                                                                  
VPROG    NTR1                                                                   
*                                                                               
         OC    NETWORK,NETWORK     WAS NETWORK ENTERED                          
         BZ    MISSNET                                                          
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   PROGERR                                                          
*                                                                               
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BE    VPROG10                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGEKEY,R4                                                        
         MVI   PGEKID,X'24'                                                     
         MVC   PGEKAM(3),BAGYMD   & BCLT                                        
         MVC   PGEKNET,NETWORK                                                  
         MVC   PGEKPRG,WORK                                                     
*                                                                               
         BRAS  RE,INITNET          SET FILES TO NET                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         BRAS  RE,INITSPOT         SET FILES TO SPOT                            
*                                                                               
         CLC   KEY(14),KEYSAVE     THIS AN EQUIVALENT PROG                      
         BE    EQVPRGER                                                         
*                                                                               
VPROG10  MVC   PROGRAM,WORK                                                     
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*----------------------------------------                                       
* INITIALIZE NETIO *                                                            
*----------------------------------------                                       
*                                                                               
NETI     NTR1                                                                   
         USING NETBLOCKD,R3                                                     
*                                                                               
         MVC   NBSELAGY,AGENCY                                                  
         MVC   NBSELMED,QMED                                                    
         MVC   NBSELAM,BAGYMD                                                   
         MVC   NBSELCLI,QCLT                                                    
         MVC   NBSELCL2,BCLT                                                    
         MVC   NBSELPRD,=C'ALL'                                                 
         MVI   NBSELUOP,C'A'                                                    
         MVI   NBSELTRF,C'T'                                                    
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBMODE,NBPROCUN                                                  
         MVC   NBAIO,AIO3                                                       
         L     R1,SYSPARMS                                                      
         L     RF,16(,R1)           COMFACS ADDRESS                             
         ST    RF,NBACOM                                                        
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A27')                                 
         L     RF,0(,R1)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ANETIO                                                        
         B     EXIT                                                             
*                                                                               
*----------------------------------------                                       
* RESTART NETIO *                                                               
*----------------------------------------                                       
*                                                                               
NETR     NTR1                                                                   
         USING NETBLOCKD,R3                                                     
*                                                                               
         MVC   NBAIO,AIO3                                                       
         L     R1,SYSPARMS                                                      
         L     RF,16(,R1)           COMFACS ADDRESS                             
         ST    RF,NBACOM                                                        
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A27')                                 
         L     RF,0(,R1)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ANETIO                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------               
* PRINT PRODUCT CODE AND SPOT LEN - R5 MUST POINT TO BINARY PROD                
*----------------------------------------------------------------               
*                                                                               
PPRD     NTR1                                                                   
*                                                                               
         MVC   0(3,R5),0(R1)                                                    
         LTR   R0,R0              ANY SPOT LEN                                  
         BZ    PPRD16              NO                                           
         LA    R5,2(,R5)                                                        
         CLI   0(R5),C' '                                                       
         BNH   PPRD14                                                           
         LA    R5,1(,R5)                                                        
PPRD14   MVI   0(R5),C'-'                                                       
         EDIT  ((R0)),(3,1(R5)),ALIGN=LEFT                                      
PPRD16   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
* FILTER UNITS ON ENTRIES IN OPTION FIELD WHILE DOING LIST FUNCTIONS            
*----------------------------------------------------------------------         
*                                                                               
LFTR     NTR1                                                                   
         OC    FILTERS,FILTERS                                                  
         BZ    EXIT                                                             
*                                                                               
         CLI   PRDOPT,0                                                         
         BE    LFTR10                                                           
*                                                                               
         CLI   QPRD,0              WAS THERE A NEW PROD                         
         BE    LFTR06               NO                                          
*                                                                               
         CLC   PRODOPT,QPRD                                                     
         BE    LFTR10                                                           
*                                                                               
         CLC   PRODOPT,QPRD2                                                    
         BE    LFTR10                                                           
         B     LFTRNE                                                           
*                                                                               
LFTR06   DS    0H                                                               
         CLC   PRDOPT,NBPRD                                                     
         BE    LFTR10                                                           
*                                                                               
         CLI   NBPRD2,0                                                         
         BE    LFTRNE                                                           
*                                                                               
         CLC   PRDOPT,NBPRD2                                                    
         BNE   LFTRNE                                                           
*                                                                               
LFTR10   CLI   SLNOPT,0                                                         
         BE    LFTR20                                                           
         CLC   SLNOPT,NBLEN                                                     
         BNE   LFTRNE                                                           
*                                                                               
LFTR20   OC    DATEOPT,DATEOPT                                                  
         BZ    LFTR30                                                           
         CLI   DATESOPT,0                                                       
         BE    LFTR22                                                           
         CLI   DATESOPT,X'4C'      LESS THAN                                    
         BE    LFTR24                                                           
         CLI   DATESOPT,X'6E'      GREATER THAN                                 
         BE    LFTR26                                                           
         DC    H'0'                                                             
LFTR22   OC    DATE2OPT,DATE2OPT                                                
         BNZ   LFTR28                                                           
         CLC   DATEOPT,NBACTDAT                                                 
         BE    LFTR30                                                           
         B     LFTRNE                                                           
LFTR24   CLC   DATEOPT,NBACTDAT                                                 
         BNH   LFTR30                                                           
         B     LFTRNE                                                           
LFTR26   CLC   DATEOPT,NBACTDAT                                                 
         BNL   LFTR30                                                           
         B     LFTRNE                                                           
LFTR28   CLC   DATEOPT,NBACTDAT                                                 
         BH    LFTRNE                                                           
         CLC   DATE2OPT,NBACTDAT                                                
         BL    LFTRNE                                                           
LFTR30   OC    CMLOPT,CMLOPT                                                    
         BZ    LFTREQ                                                           
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   LFTRNE                                                           
         USING NUCMLEL,R6                                                       
         CLC   CMLOPT,NUCML1                                                    
         BE    LFTREQ                                                           
         OC    NUCML2,NUCML2                                                    
         BZ    LFTRNE                                                           
         CLC   CMLOPT,NUCML2                                                    
         BNE   LFTRNE                                                           
LFTREQ   CR    R1,R1                                                            
         B     EXIT                                                             
LFTRNE   LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
* RTN TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT                              
*----------------------------------------------------------------------         
*                                                                               
SVTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    SET TERMINAL NUMBER                          
         MVI   DMCB+8,2            SET PAGE                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ASVSTOR                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
*---------------------------                                                    
* RESTORE STABLE                                                                
*---------------------------                                                    
*                                                                               
RDTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ASVSTOR                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*-------------------------------------------------------                        
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*-------------------------------------------------------                        
*                                                                               
HDHK     NTR1                                                                   
         OC    STDATE(12),STDATE                                                
         BZ    HDHK10                                                           
         MVC   H3+28(6),=C'PERIOD'                                              
         GOTO1 DATCON,(R1),(0,STDATE),(8,H3+35)                                 
         MVC   H3+44(2),=C'TO'                                                  
         GOTO1 DATCON,(R1),(0,ENDATE),(8,H3+48)                                 
HDHK10   MVC   H4+12(L'QCLT),QCLT                                               
         MVC   H4+17(L'CLTNM),CLTNM                                             
         MVC   H5+12(4),NETWORK                                                 
         B     EXIT                                                             
         SPACE 2                                                                
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
PRLENER  L     R1,=A(PRLENMS)                                                   
         B     ERREXITM                                                         
BADSUBL  L     R1,=A(BADSUBMS)                                                  
         B     ERREXITM                                                         
CHAERR   L     R1,=A(CHAMS)                                                     
         B     ERREXITM                                                         
FEEDERR  L     R1,=A(FEEDERMS)                                                  
         B     ERREXITM                                                         
FEEDLNER L     R1,=A(FEEDLNMS)                                                  
         B     ERREXITM                                                         
DTPERER  L     R1,=A(DATPERMS)                                                  
         B     ERREXITM                                                         
EQPRDERR L     R1,=A(EQPRDMS)                                                   
         B     ERREXITM                                                         
MISSLNER L     R1,=A(MISSLNMS)                                                  
         B     ERREXITM                                                         
SUBLERR  L     R1,=A(SUBLMSG)                                                   
         B     ERREXITM                                                         
EQVPRGER L     R1,=A(EQVPRGMS)                                                  
         B     ERREXITM                                                         
PROGERR  L     R1,=A(PROGERMS)                                                  
         LA    R2,TRAPROGH                                                      
         B     ERREXITM                                                         
PRGDATER L     R1,=A(PRGDATMS)                                                  
         LA    R2,TRADATEH                                                      
         B     ERREXITM                                                         
NETERR   L     R1,=A(NETERMS)                                                   
ERREXITM MVC   CONHEAD(10),=C'* ERROR * '                                       
         A     R1,SPTR1ERR                                                      
         MVC   CONHEAD+10(50),0(R1)                                             
ERREXIT  GOTO1 ERREX2                                                           
         SPACE 3                                                                
VCMLERR  MVI   ERROR,INVCOMM       NO SUCH COMMERCIAL FOR CLT                   
         B     TRAPERR                                                          
CMLENER  MVI   ERROR,INVCMMLN      COMMERCIAL MUST BE 8 CHAR                    
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
PRDERR   MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
MISSNET  LA    R2,TRANETH                                                       
         B     MISSERR                                                          
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PRLENMS  DC    CL50'PRODUCT NOT MORE THAN 3 CHARACTERS *'                       
BADSUBMS DC    CL50'SUBLINE MUST BE A-Z *'                                      
SUBLMSG  DC    CL50'NO MORE THAN 26 SUBLINES FOR 1 PROG FOR 1 DAY *'            
CHAMS    DC    CL50'NO ACTION CHANGE, DELETE, THEN ADD CORRECT UNIT *'          
FEEDERMS DC    CL50'NO FEED RECORD FOUND *'                                     
FEEDLNMS DC    CL50'FEED MUST BE 1 - 4 CHARACTERS *'                            
DATPERMS DC    CL50'DATE MUST FALL WITHIN PERIOD *'                             
EQVPRGMS DC    CL50'CAN''T ENTER EQUIVALENT PROGRAM CODE *'                     
EQPRDMS  DC    CL50'PRODUCT AND PARTNER EQUAL, MUST BE DIFFERENT *'             
MISSLNMS DC    CL50'MISSING UNIT LENGTH *'                                      
PROGERMS DC    CL50'NO PROGRAM FOUND *'                                         
NETERMS  DC    CL50'NO NETWORK FOUND *'                                         
PRGDATMS DC    CL50'NO PROGRAM RECORD WITH END DATE FOR THIS DATE *'            
*                                                                               
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,36,C'TRAFFIC UNITS LIST'                                      
         SSPEC H2,36,C'------------------'                                      
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H5,3,C'NETWORK'                                                  
         SSPEC H4,85,RUN                                                        
         SSPEC H4,73,REPORT                                                     
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'PRODUCT/PARTNER NAME'                                     
         SSPEC H9,3,C'--------------------'                                     
         SSPEC H8,34,C'COMMERCIAL-TITLE'                                        
         SSPEC H9,34,C'-------------------------'                               
         SSPEC H8,60,C'FEED'                                                    
         SSPEC H9,60,C'----'                                                    
         SSPEC H8,66,C'PROGRAM'                                                 
         SSPEC H9,66,C'-------'                                                 
         SSPEC H8,74,C'DATE'                                                    
         SSPEC H9,74,C'--------'                                                
         SSPEC H8,84,C'TIME'                                                    
         SSPEC H9,84,C'-----'                                                   
         SSPEC H8,95,C'DAY'                                                     
         SSPEC H9,95,C'---'                                                     
         SSPEC H8,107,C'POS'                                                    
         SSPEC H9,107,C'---'                                                    
         DC    X'00'               END MARKER FOR SSPECS                        
         SPACE 3                                                                
         DROP  RB,RC                                                            
         DS    0H                                                               
*                                                                               
*                                                                               
*-------------------------------------------------------                        
* RESET FILES TO SPOT                                                           
*-------------------------------------------------------                        
*                                                                               
         USING GEND,RC                                                          
INITSPOT MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         BR    RE                                                               
*                                                                               
*-------------------------------------------------------                        
* RESET FILES TO NET *                                                          
*-------------------------------------------------------                        
*                                                                               
INITNET  MVI   DATADISP+1,27       SET FROM SPOT TO NET                         
         MVI   LKEY+1,20                                                        
         MVI   SYSDIR,C'U'                                                      
         MVI   SYSDIR+1,C'N'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*-------------------------                                                      
* VALIDATE FILTERS                                                              
*-------------------------                                                      
*                                                                               
VFTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SVOPTION,0          INIT OPTION FLAG                             
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTR74              NO                                           
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
VFTR06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTHELP),OPTHELP                                       
         B     ERREXIT                                                          
VFTR08   GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(5,BLOCK+64)                          
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK+64            ADDRESS OF FIRST BLOCK                    
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         MVI   HOLDSIGN,0                                                       
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         DATE                                         
         BNE   VFTR20                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERR              NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(2,DATEOPT)                                 
         CLM   R6,1,1(R4)          WAS THERE ONLY 1 DATE                        
         BE    VFTR18              YES                                          
         LA    R5,1(R6,R5)                                                      
         GOTO1 DATVAL,(R1),(0,(R5)),DATE                                        
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERR              NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(2,DATE2OPT)                                
         B     VFTR70                                                           
VFTR18   MVC   DATESOPT,HOLDSIGN                                                
         B     VFTR70                                                           
VFTR20   EX    R1,VFTRCLCB         PRD (PRODUCT)                                
         BE    VFTR22                                                           
         EX    R1,VFTRCLCC         PROD (PRODUCT)                               
         BNE   VFTR30                                                           
VFTR22   CLC   =C'POL',22(R4)      IS PRD=POL                                   
         BE    PRDERR              YES, ERROR                                   
         CLC   =C'ALL',22(R4)      IS PRD=ALL                                   
         BE    PRDERR              YES, ERROR                                   
         CLI   1(R4),3             MAX 3 CHAR                                   
         BH    PRLENER                                                          
         BE    *+8                                                              
         MVI   24(R4),C' '                                                      
         LA    R0,256              MAX COUNT BUG CATCHER                        
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
VFTR24   CLC   0(4,R1),=F'0'       AT END OF TABLE?                             
         BE    PRDERR              YES, ERROR                                   
         CLC   0(3,R1),22(R4)      THIS A VALID PROD CODE                       
         BE    VFTR26                                                           
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,VFTR24                                                        
         B     PRDERR              YES, ERROR                                   
*                                                                               
VFTR26   MVC   PRODOPT(4),0(R1)    PROD AND PRD                                 
         OI    SVOPTION,OPTPRDSW   TURN ON PROD FILTER USED                     
         B     VFTR70                                                           
VFTR30   EX    R1,VFTRCLCD         COMMERCIAL                                   
         BNE   VFTR40                                                           
         MVC   SVCML,22(R4)        PRESET 8 CHAR ISCII                          
         CLI   1(R4),8             MUST BE 8 CHAR                               
         BE    VFTR32                                                           
         CLI   1(R4),12            NOT MORE THAN 12                             
         BH    CMLENER                                                          
         GOTO1 VTRPACK,DMCB,(C'P',22(R4)),SVCML                                 
                                                                                
VFTR32   GOTO1 ANY                                                              
                                                                                
         XC    KEY(13),KEY                                                      
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
         MVC   CMLKCML,SVCML                                                    
         DROP  R4                                                               
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(13),KEYSAVE                                                  
         BE    VFTR35                                                           
                                                                                
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVI   KEY+1,X'C1'         READ 0AC1 KEY                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCMLERR                                                          
                                                                                
VFTR35   MVC   CMLOPT,SVCML                                                     
         B     VFTR70                                                           
                                                                                
VFTR40   EX    R1,VFTRCLCE         SPOT LEN                                     
         BNE   VFTR50                                                           
         TM    3(R4),X'80'         WAS SPOT LEN NUMERIC                         
         BZ    NUMERR                                                           
         MVC   FLDH,TRAFLTRH                                                    
         PACK  FLDH+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   FLDH+5(1),1(R4)     DATA LEN                                     
         MVC   FLD(10),22(R4)      SPOT LEN                                     
         MVI   ERROPT,C'Y'                                                      
         LA    R2,FLDH                                                          
         GOTO1 VALISLN                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   TRAPERR             GO PRINT ERROR                               
         MVC   SLNOPT,WORK                                                      
         B     VFTR70                                                           
                                                                                
VFTR50   DS    0H                                                               
         EX    R1,VFTRCLCF         SHOW ADID ?                                  
*MNV     BNE   VFTR80                                                           
         BNE   VFTR60                                                           
         OI    SVOPTION,OPTADISW                                                
         B     VFTR70                                                           
                                                                                
*MNV                                                                            
VFTR60   DS    0H                                                               
         EX    R1,VFTRDIGI         SHOW DIGITAL                                 
         BNE   VFTR80                                                           
         OI    SVOPTION,OPTDIGI                                                 
         B     VFTR70                                                           
*MNV                                                                            
                                                                                
VFTR70   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
         TM    SECFLAG,BLSSW                                                    
         BZ    VFTR74                                                           
*                                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
VFTR72   CLC   PRODOPT,0(R1)                                                    
         BE    VFTR74                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,VFTR72                                                        
*                                                                               
         LA    R2,TRAFLTRH                                                      
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     TRAPERR                                                          
*                                                                               
VFTR74   OI    4(R2),X'20'         SET VALIDATED                                
         J     EXIT                                                             
VFTR80   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTMSG+L'OPTHELP),OPTMSG                               
         B     ERREXIT                                                          
OPTMSG   DC    C'* ERROR *'                                                     
OPTHELP  DC    C'VALID FILTERS - DATE/PRD/CML/ADID/LEN=/DIGITAL'                
VFTRCLCA CLC   12(0,R4),=CL4'DATE'                                              
VFTRCLCB CLC   12(0,R4),=CL4'PRD'                                               
VFTRCLCC CLC   12(0,R4),=CL4'PROD'                                              
VFTRCLCD CLC   12(0,R4),=CL4'CML'                                               
VFTRCLCE CLC   12(0,R4),=CL3'LEN'                                               
VFTRCLCF CLC   12(0,R4),=CL4'ADID'                                              
*MNV                                                                            
VFTRDIGI CLC   12(0,R4),=CL7'DIGITAL'                                           
*MNV                                                                            
         DROP  RC                                                               
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------------------              
* VALIDATE PERIOD - MONTH AND YR, AND DEVELOP                                   
*                   CALENDAR OR BROADCAST START/END DATES                       
*                                  BROADCAST MONTH                              
*-----------------------------------------------------------------              
*                                                                               
VPER     NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         LA    R4,TRAPER                                                        
         CLI   0(R4),C'?'          QUESTION MARK HELP                           
         BNE   VPER04                                                           
         LA    R4,1(,R4)                                                        
VPER04   GOTO1 DATVAL,DMCB,(2,0(R4)),STDATE                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,STDATE),(3,PERIOD)                                
         CLI   NBUSER+3,C'B'       BROADCAST MONTH                              
         BE    VPER20                                                           
         MVC   STDATE+4(2),=C'01'                                               
         GOTO1 DATCON,(R1),(0,STDATE),(2,STDATEP)                               
         GOTO1 ADDAY,(R1),STDATE,ENDATE,F'31'                                   
VPER10   GOTO1 (RF),(R1),ENDATE,ENDATE,F'-1'                                    
         CLC   STDATE(4),ENDATE                                                 
         BNE   VPER10                                                           
         GOTO1 DATCON,(R1),(0,ENDATE),(2,ENDATEP)                               
         B     VPER30                                                           
VPER20   MVC   STDATE+4(2),=C'15'                                               
         MVC   WORK(6),STDATE                                                   
         GOTO1 =V(GETBROAD),DMCB,(1,WORK),STDATE,GETDAY,ADDAY,         C        
               RR=SPTR1ERR                                                      
         GOTO1 DATCON,(R1),(0,STDATE),(2,STDATEP)                               
         GOTO1 DATCON,(R1),(0,ENDATE),(2,ENDATEP)                               
*                                                                               
VPER30   CLI   TRAPER,C'?'         QUESTION MARK HELP                           
         BNE   VPERX                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=CL31'PERIOD IS CALENDAR  MONTH FROM'                
         CLI   NBUSER+3,C'B'       BROADCAST MONTH                              
         BNE   VPER42                                                           
         MVC   CONHEAD+10(9),=CL9'BROADCAST'                                    
VPER42   GOTO1 DATCON,DMCB,(2,STDATEP),(5,CONHEAD+32)                           
         MVC   CONHEAD+41(2),=C'TO'                                             
         GOTO1 (RF),(R1),(2,ENDATEP),(5,CONHEAD+44)                             
         GOTO1 ERREX2                                                           
*                                                                               
VPERX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE SPTRNFEED                                                      
         EJECT                                                                  
       ++INCLUDE SPTRNEQPRG                                                     
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRABED                                                       
         PRINT OFF                                                              
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
NETBLOCKD      DSECT                                                            
       ++INCLUDE NETBLOCKD                                                      
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR1ERR DS    F                                                                
ANETIO   DS    F                                                                
ASVSTOR  DS    F                                                                
TOTUNITS DS    F                                                                
VTRPACK  DS    A                   ADDRESS OF TRPACK                            
*                                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL64                                                             
NETMKT   DS      H                                                              
*                                                                               
SVCML    DS    CL8                 8 CHAR ISCII OR 8 BYTES PACKED               
SVADID   DS    CL12                12 CHAR ADID                                 
*                                                                               
DATE     DS    CL6                                                              
DATEP    DS    XL2                                                              
*                                                                               
* STDATE, ENDATE, STDATEP, AND ENDATEP MUST BE TOGETHER AND IN ORDER            
*                                                                               
DATES    DS    0CL25                                                            
PERIOD   DS    XL3                                                              
STDATE   DS    CL6                                                              
ENDATE   DS    CL6                                                              
STDATEP  DS    XL2                                                              
ENDATEP  DS    XL2                                                              
STDATE3  DS    XL3                                                              
ENDATE3  DS    XL3                                                              
NETWORK  DS    CL4                                                              
PROGRAM  DS    CL6                                                              
FEED     DS    CL4                                                              
SUBLINE  DS    CL1                                                              
PROGDAY  DS    XL1                                                              
FILTERS  DS   0CL18                                                             
PRODOPT  DS    CL3                                                              
PRDOPT   DS    XL1                                                              
CMLOPT   DS    CL8                                                              
SLNOPT   DS    XL1                                                              
DATEOPT  DS    XL2                                                              
DATE2OPT DS    XL2                                                              
DATESOPT DS    CL1                                                              
HOLDSIGN DS    CL1                                                              
SVPOST   DS    CL1                 SAVE POST MEDIA FROM STA REC                 
SVMEDIA  DS    CL1                 SAVE MEDIA FROM STA REC                      
*                                                                               
SVOPTION DS    CL1                 SAVE OPTIONS                                 
OPTPRDSW EQU   X'80'               OPTION PRODUCT                               
OPTADISW EQU   X'40'               OPTION SHOW ADID                             
*MNV                                                                            
OPTDIGI  EQU   X'20'               SHOW DIGITAL                                 
*MNV                                                                            
         DS    0D                                                               
STNETBLK EQU   *                                                                
         EJECT                                                                  
* OFFLINE REPORT LINE                                                           
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PPROD    DS    CL7                                                              
         DS    CL2                                                              
PPRODNM  DS    CL20                                                             
         DS    CL2                                                              
PCMML    DS    CL8                                                              
         DS    CL2                                                              
PCMMLT   DS    CL15                                                             
         DS    CL1                                                              
PFEED    DS    CL4                                                              
         DS    CL2                                                              
PPROG    DS    CL6                                                              
         DS    CL2                                                              
PDATE1   DS    CL8                                                              
*        ORG   PNET                                                             
*DATE2   DS    CL6                                                              
         DS    CL2                                                              
PTIME    DS    CL10                                                             
         DS    CL1                                                              
PDAY     DS    CL8                                                              
         DS    CL4                                                              
PPOS     DS    CL4                                                              
         SPACE 3                                                                
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LNET     DS    CL4                                                              
         DS    CL1                                                              
LPROG    DS    CL6                                                              
         DS    CL1                                                              
LDATE    DS    CL8                                                              
LSUB     DS    CL1                                                              
         DS    CL1                                                              
LPROD    DS    CL7                                                              
         DS    CL1                                                              
LPPTR    DS    CL7                                                              
         DS    CL1                                                              
LDAY     DS    CL8                                                              
         DS    CL1                                                              
LTIME    DS    CL11                                                             
         DS    CL1                                                              
LFEED    DS    CL4                                                              
         DS    CL1                                                              
LCML     DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069SPTRA1E   03/30/15'                                      
         END                                                                    
