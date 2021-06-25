*          DATA SET SPSFM14    AT LEVEL 109 AS OF 01/28/20                      
*PHASE T21714A                                                                  
T21714   TITLE 'SPSFM14 - STATION MASTER'                                       
***********************************************************************         
*                                                                               
* HOW TO DELETE A STATION MASTER RECORD:                                        
* 1) ADD/CHANGE A MARKET NAME TO *PURGE                                         
* 2) CHANGE THE MASTER RECORD(S) TO THE MARKET IN STEP 1                        
* 3) RUN AN SSZ TO DELETE ALL MARKET RECS NAMED *PURGE AND ALL                  
*    STATIONS THAT LIVE IN THOSE MARKETS                                        
*                                                                               
* NN    NN   OOOO   TTTTTTTT EEEEEEEE                                           
* NNN   NN  OOOOOO  TTTTTTTT EEEEEEEE                                           
* NNNN  NN OO    OO    TT    EE                                                 
* NN NN NN OO    OO    TT    EEEEEE                                             
* NN  NNNN OO    OO    TT    EEEEEE                                             
* NN   NNN OO    OO    TT    EE                                                 
* NN    NN  OOOOOO     TT    EEEEEEEE                                           
* NN    NN   OOOO      TT    EEEEEEEE                                           
*                                                                               
* NOTE: IF YOU ADD A NEW MASTER PASSIVE, YOU MUST UPDATE SPGENSSX!              
*                                                                               
***********************************************************************         
*                                                                               
*                                                                               
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* 23MAR17  108 MHER   ADD FIELD FOR PARENTS+SATELLITES                          
* 23JUN/14 103 AKAT - ABEND IF WE TRY AND ADD X PTR W/DUP SEQ NUM     *         
* MAR21/14 102 AKAT - FIX BUG IN VOLDMKT FOR CANADA                   *         
* OCT09/12 101 AKAT - MAKE SURE DSTA RECORD EXISTS FOR BANDS D AND S  *         
* FEB21/12 100 AKAT - ALLOW MEDIA R TO CHANGE TO/FROM MKT 0000        *         
* DEC14/11 099 AKAT - NEW P&G OPTION                                  *         
* NOV10/11 098 AKAT - DISABLE EMAIL TO RECYCLE DESKTOP SERVERS        *         
* OCT14/10 091 AKAT - NEW VENDOR LOCK SUPPORT                         *         
* APR29/10 090 AKAT - CHECK FOR BUYS WHEN CHANGING A MARKET           *         
* MAR15/10 089 AKAT - DON'T SET GENCON SWITCH FOR CABLE IN DK LOGIC!  *         
* AUG01/08 086 AKAT - DON'T SEND E-MAIL FOR AGENCY T1                 *         
* JUL09/08 085 AKAT - SEND E-MAIL WHEN CANADIAN MASTER RECORD IS ADDED*         
* JUL02/08 084 AKAT - ALLOW BOOKTYPE L ON STA LEVEL MASTER RECORD     *         
*                   - SO DSKTOP SERVERS CAN BE RECYCLED & GET STA TABL*         
* JUN16/08 083 AKAT - COPY CABLE SYSTEM NAME TO CLT SPECIFIC MASTER   *         
* APR03/07 082 AKAT - NEW NETWORK TYPE FIELD FOR CANADIAN DESKTOP     *         
* MAR07/07 081 WHOA - ALLOW CAS TO ADD MISSING CABLE SYSCODES         *         
* JAN25/07 080 MCHO - ADDED C'K' "PASSIVE" KEYS  (STKKTYPE)           *         
* NOV14/06 079 EJOR - TWO CHARACTER BOOK TYPES                        *         
* MAR08/06 078 AKAT - DO NOT ALLOW MKT 0000 FOR NON-OFFICE MASTER REC *         
*                   - AND MAKE SURE THAT USER ENTERS A MARKET         *         
* NOV01/05 077 AKAT - DISPLAY ALPHA MARKET FOR CANADIAN SOFT DEMOS    *         
* OCT12/05 076 AKAT - SUPPORT 2 CHARACTER OFFICE CODES                *         
* AUG18/05 075 AKAT - DO NOT ALLOW CANADA TO ADD IF 'X' POINTER EXISTS*         
* MAR04/05 074 AKAT - FIX BUG IN DK WHEN DISPLAYING AN OFFICE         *         
* NOV12/03 071 PWES - CANADA DEMO LOOKUP LINK (RTG SVC CALL LETTERS)  *         
*                     + ENSURE MIN REC LEN STANCLNQ NOT STARLNQ       *         
* OCT03/03 070 HWON - ?                                               *         
* JUL09/03 067 PWES - FIX X-REC SEQ# BOUNDARY PROBLEM/DUPES           *         
* JUN 2/98 021 BPOO - CLT SPECIFIC DONT HAVE NETWORKS, DISPLAY MESSAGE*         
* MAY20/98 001 BPOO - XSORT NETWORK OUTPUT LIST AND CHANGE NETWORK    *         
*                     FROM REFERENCE OF CABLE TABLE TO USING MSUNPK   *         
*                     CERTAIN ROUTINES WERE CHANGED ALSO              *         
*                     SUCH AS VALIDATION OF DELETING NETWORKS         *         
* ====== REFORMMATED LEVEL TO 1 ==================================    *         
* MAR26/98 090 BPOO - SCBL64,SCBLSEQ WERE ADDED FOR NEW STAPACK       *         
*                                                                     *         
* OCT28/97 086 MHER   DO NOT ALLOW -L FOR CANADIAN STATIONS           *         
*                                                                     *         
* SEP24/97 085 GLEE - CLEAR  ERROPT  AT  TRAPEND  ERROR EXIT          *         
*                                                                     *         
* MAR13/97 080 EJOR - NO SF X-POINTERS FOR AGY XD                     *         
*                                                                     *         
* SEP18/96 076 SPRI - DISPLAY NTI STATION                             *         
*                                                                     *         
* JUL17/96 075 GLEE - MAKE ROOM FOR A 3RD "CABLE NETWORK LIST" LINE   *         
*              GLEE - MOVED DISPNET ROUTINE TO ITS OWN NMOD           *         
*                                                                     *         
* APR20/95 073 SPRI - RESTRICTIONS FOR ACTIONS ADD/DEL/CHA            *         
*                                                                     *         
* FEB21/95 066 EJOR - ADD NON-SEQUENTIAL STATION NUMBERS (UGH)        *         
*                                                                     *         
* FEB02/95 059 EJOR - ALLOW FORCE OF CANADIAN STATION NUMBER          *         
*                                                                     *         
* JAN30/94 056 GLEE - CHECK CLT DFAULT EXIST BEFORE ADDING CLT EXCPTN *         
*                                                                     *         
* NOV30/94 047 GLEE - SUPPORT ALPHAMKT ON STATION LEVEL FOR RADIO     *         
***********************************************************************         
T21714   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21714,R7,RR=R8                                                
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         USING OFFICED,OFCBLK                                                   
         B     MAIN05                                                           
RELO     DS    A                                                                
*                                                                               
MAIN05   XC    ERRDISP,ERRDISP                                                  
         XC    MYMKT,MYMKT                                                      
         XC    MYFORM,MYFORM                                                    
         XC    MYEFFDTE,MYEFFDTE                                                
         XC    MYTEXT,MYTEXT                                                    
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    MAIN15              YES - ALL ACTIONS AUTHORIZED                 
         TM    12(RA),X'80'        AUTHORIZED FOR ADD/DEL/CHA?                  
         BZ    MAIN15              YES                                          
         MVI   ERROR,NOTAUTH       NOT AUTHORIZED FOR THIS FUNCTION             
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    NOTAUTHD            YES - NOT AUTHORIZED                         
         CLI   ACTNUM,ACTCHA       ACTION CHANGE?                               
         BE    NOTAUTHD            YES - NOT AUTHORIZED                         
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         BNE   MAIN15              NO - ALL OTHER ACTIONS O.K.                  
*                                                                               
NOTAUTHD MVI   GETMSYS,2           CHANGE TO X'02' ERROR SYSTEM                 
         LA    R2,CONACTH          POINT TO THE ACTION FIELD                    
         B     TRAPERR             GIVE THE NOT AUTHORIZED ERROR                
*                                                                               
MAIN15   BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,SETFILE        SET FILE NAME                                
         BE    SF                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,XRECDEL        DELETED RECORD                               
         BE    DELR                                                             
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         BE    XRP                                                              
         CLI   MODE,XRECREST       RESTORED RECORD                              
         BE    RESTR                                                            
         J     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
* SET FILE                                                                      
*                                                                               
SF       DS    0H                                                               
         BAS   RE,SETDEF           SET FILENAME & OTHER DEFAULTS                
         J     EXIT                                                             
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VK       XC    PASSKEY,PASSKEY                                                  
         MVI   MISCFLG1,0          CLEAR OUT THE FLAG                           
         MVI   CABLE,C'N'                                                       
*                                                                               
         OI    SSTMKTQH+1,X'08'    TURN ON HIGH INTENSITY                       
         OI    SSTMKTQH+6,X'80'                                                 
         OI    SSTTSRQH+1,X'08'    TURN ON HIGH INTENSITY                       
         OI    SSTTSRQH+6,X'80'                                                 
         OI    SSTTRAQH+1,X'08'    TURN ON HIGH INTENSITY                       
         OI    SSTTRAQH+6,X'80'                                                 
         OI    SSTORDTH+1,X'08'    TURN ON HIGH INTENSITY                       
         OI    SSTORDTH+6,X'80'                                                 
         OI    SSTLOCTH+1,X'08'    TURN ON HIGH INTENSITY                       
         OI    SSTLOCTH+6,X'80'                                                 
*                                                                               
         NI    SSTMKTH+1,X'FF'-X'20'    UNPROTECT IT!                           
         OI    SSTMKTH+6,X'80'                                                  
         NI    SSTPREPH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTPREPH+6,X'80'                                                 
         NI    SSTTSRPH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTTSRPH+6,X'80'                                                 
         NI    SSTTRAFH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTTRAFH+6,X'80'                                                 
         NI    SSTFORMH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTFORMH+6,X'80'                                                 
         NI    SSTCATH+1,X'FF'-X'20'    UNPROTECT IT!                           
         OI    SSTCATH+6,X'80'                                                  
         NI    SSTTYPEH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTTYPEH+6,X'80'                                                 
         NI    SSTNTISH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTNTISH+6,X'80'                                                 
         NI    SSTCHANH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTCHANH+6,X'80'                                                 
         NI    SSTAFFH+1,X'FF'-X'20'    UNPROTECT IT!                           
         OI    SSTAFFH+6,X'80'                                                  
         NI    SSTARBFH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTARBFH+6,X'80'                                                 
         NI    SSTFAXH+1,X'FF'-X'20'    UNPROTECT IT!                           
         OI    SSTFAXH+6,X'80'                                                  
         NI    SSTDAYLH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTDAYLH+6,X'80'                                                 
         NI    SSTTAXH+1,X'FF'-X'20'    UNPROTECT IT!                           
         OI    SSTTAXH+6,X'80'                                                  
         NI    SSTSIZEH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTSIZEH+6,X'80'                                                 
         NI    SSTCTYH+1,X'FF'-X'20'    UNPROTECT IT!                           
         OI    SSTCTYH+6,X'80'                                                  
         NI    SSTCTAXH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTCTAXH+6,X'80'                                                 
         NI    SSTFEEH+1,X'FF'-X'20'    UNPROTECT IT!                           
         OI    SSTFEEH+6,X'80'                                                  
         NI    SSTGSTH+1,X'FF'-X'20'    UNPROTECT IT!                           
         OI    SSTGSTH+6,X'80'                                                  
         NI    SSTBOOKH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTBOOKH+6,X'80'                                                 
         NI    SSTEIXH+1,X'FF'-X'20'    UNPROTECT IT!                           
         OI    SSTEIXH+6,X'80'                                                  
         NI    SSTPSTH+1,X'FF'-X'20'    UNPROTECT IT!                           
         OI    SSTPSTH+6,X'80'                                                  
         NI    SSTDEACH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTDEACH+6,X'80'                                                 
         NI    SSTLOCKH+1,X'FF'-X'20'   UNPROTECT IT!                           
         OI    SSTLOCKH+6,X'80'                                                 
*                                                                               
         LA    R2,SSTMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         BAS   RE,TSTCOMS          TEST COMSCORE                                
*                                                                               
VK03     CLI   SVAPROF+7,C'C'      CANADA AGENCY?                               
         BNE   *+8                 NO                                           
         OI    SSTTAXH+1,X'20'     NEED TO PROTECT IT                           
         MVC   FIRSTLBL,=C'****'   UNIQUE  LABEL                                
         MVC   FIRSTLBL+1(2),AGENCY                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO "01" ELEMENT FOUND                        
         USING AGYEL,R6                                                         
         MVC   SVAGYFL2,AGYFLAG2                                                
         DROP  R6                                                               
*                                                                               
         TM    SVAGYFL2,AGYFLAG2_UID                                            
         BZ    VK05                                                             
*                                                                               
         NI    SSTUNIQH+1,X'FF'-X'20'-X'0C'   UNPROTECT, VISIBLE                
         OI    SSTUNIQH+6,X'80'                                                 
         NI    SSTUNQTH+1,X'FF'-X'0C'                                           
         OI    SSTUNQTH+6,X'80'                                                 
         B     VK06                                                             
*                                                                               
VK05     DS    0H                  MEDIA NOT R - HIDE UNIQUE ID FIELD           
         OI    SSTUNIQH+1,X'2C'                                                 
         OI    SSTUNIQH+6,X'80'                                                 
         OI    SSTUNQTH+1,X'0C'                                                 
         OI    SSTUNQTH+6,X'80'                                                 
*                                                                               
VK06     DS    0H                  MEDIA NOT R - HIDE UNIQUE ID FIELD           
         BRAS  RE,HIDEFLDS         SHOW/HIDE SCREEN FIELDS                      
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   VK10                                                             
         CLI   QMED,C'N'           DON'T ALLOW MEDIA N OR C                     
         BE    INVERR                                                           
         CLI   QMED,C'C'                                                        
         BE    INVERR                                                           
*                                                                               
VK10     MVC   QCLT,ZEROES                                                      
         XC    BCLT,BCLT                                                        
         LA    R2,SSTCLIH          CLIENT FIELD                                 
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         NI    SSTLOCTH+1,X'F7'    TURN OFF HIGH INTENSITY FOR LOCK             
         OI    SSTLOCKH+1,X'20'    PROTECT LOCK FIELD                           
*                                                                               
*****  WE HAVE NEW CODE TO CHECK IF CLIENT FIELD HAS OFFICE CODE INSTD          
         CLI   8(R2),C'*'          DO WE HAVE A * ?                             
         BNE   VK10A               NOPE, CONTINUE NORMALLY                      
*                                                                               
VKOFF    XC    WORK,WORK           WE'RE GONNA CHECK THE A0A PROFILE            
*                                                                               
         MVC   WORK+16(4),=C'SA0A' KEY FOR GETPROF                              
         NI    WORK+16,X'BF'       'S' LOWER CASE FOR 3 CHAR PROFILE            
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         CLI   WORK+13,C'Y'        IS OFFICE CODE ALLOWED?                      
         BNE   VKOFFNA             NO - OFFICE CODE NOT ALLOWED ERROR           
*                                                                               
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T217FFD+6                                                
         MVC   OFCLMT,T217FFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,9(R2)                                                    
         OI    OFCOFC2+1,X'40'                                                  
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         TM    OFCINDS,OFCIOINV    INVALID OFFICE?                              
         BNZ   VKOFFINV            YES                                          
         CLI   DMCB,0              ERROR?                                       
         BNE   VKOFFINV            YES - INVALID OFFICE                         
*                                                                               
         OI    MISCFLG1,MF1OFFCD   TURN ON THE FLAG                             
         LA    R1,SSTPREPH         SAVE THE ADDRESS OF PAYING REP               
         ST    R1,AFRSTREC         SO IT WILL POINT THERE FIRST                 
*                                                                               
         MVC   SVPREPH,SSTPREPH    SAVE OFF THE LENGTH OF PAYREP                
         MVC   SVPREP,SSTPREP      SAVE OFF THE PAYING REP                      
*                                                                               
         TWAXC SSTMKTH                                                          
*                                                                               
         MVC   SSTPREPH,SVPREPH                                                 
         MVC   SSTPREP,SVPREP                                                   
*                                                                               
         NI    SSTMKTQH+1,X'FF'-X'08'   TURN OFF HIGH INTENSITY                 
         NI    SSTTSRQH+1,X'FF'-X'08'   TURN OFF HIGH INTENSITY                 
         NI    SSTTRAQH+1,X'FF'-X'08'   TURN OFF HIGH INTENSITY                 
         NI    SSTORDTH+1,X'FF'-X'08'   TURN OFF HIGH INTENSITY                 
*                                                                               
         LA    R1,SSTMKTH                                                       
         LA    R2,SSTPREPH                                                      
         LA    R3,SSTUNIQH                                                      
VKOFF20  CR    R1,R3                                                            
         BE    VK20                WE'RE DONE, WENT TO THE END                  
         CR    R1,R2               ARE WE ON PAYING REP?                        
         BE    VKOFFNXT            YEAH, SKIP PROTECTING THE FIELD              
         OI    1(R1),X'20'         PROTECT THE FIELD                            
         OI    6(R1),X'80'                                                      
*                                                                               
VKOFFNXT XR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         B     VKOFF20                                                          
*                                                                               
VK10A    CLI   T217FFD+1,C'*'      DDS TERMINAL?                                
         BNE   VK15                NO                                           
         CLC   =C'NO CLT',SSTOPT   WANT TO BYPASS ADDING A CLT REC?             
         BE    VK11                YES                                          
         CLC   =C'NFC',SSTOPT      WANT TO BYPASS ADDING A CLT REC?             
         BNE   VK15                NO, MUST HAVE ONE                            
*                                                                               
VK11     GOTO1 ANY                                                              
         MVI   ERROR,INVCLI                                                     
         MVC   QCLT(3),WORK                                                     
         CLI   5(R2),2                                                          
         BL    TRAPERR                                                          
*                                                                               
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   TRAPERR                                                          
         B     VK20                                                             
*                                                                               
VK15     GOTO1 VALICLT             CHECK FIRST IF THERE'S A CLIENT              
*                                                                               
VK20     LA    R2,SSTSTAH          STATION FIELD                                
         GOTO1 ANY                                                              
         MVI   ERROR,INVSTAT       ASSUME INVALID STATION INPUT                 
         CLI   8(R2),C'0'          IF IT IS A CABLE INPUT, THEN                 
         BL    VK25                                                             
         CLI   8(R2),C'9'                                                       
         BH    VK25                                                             
         CLI   5(R2),4             L'INPUT S/B <=4!                             
         BH    TRAPERR                                                          
         CLC   8(4,R2),ZEROES      DON'T ALLOW STATION 0000                     
         BE    TRAPERR                                                          
*                                                                               
VK25     LA    R4,BLOCK                                                         
         USING STABLKD,R4                                                       
         XC    0(STBLNQ,R4),0(R4)  CLEAR INTERFACE BLOCK                        
         MVC   STBMED,QMED         SET MEDIA                                    
         ST    R2,STBADDR          SET A(STATION FIELD)                         
         MVI   STBCTRY,C'U'                                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STBCTRY,C'C'                                                     
         MVC   STBACOM,ACOMFACS                                                 
         GOTO1 STAVAL,DMCB,(R4)                                                 
         CLI   STBERR,0                                                         
         BNE   TRAPERR                                                          
         MVC   QSTANEW,STBSTA      SET OUTPUT STATION                           
         CLI   QSTANEW,C'0'        IF THE FIRST CHAR IS A DIGIT                 
         BNL   VK30                                                             
* NOT CABLE - SHOULD BE ALL ALPHA                                               
         LA    R1,QSTANEW+3                                                     
         LA    R0,4                                                             
         CLI   0(R1),C' '          LAST CHAR MAY BE A SPACE                     
         BE    VK28                                                             
VK26     CLI   0(R1),C'A'                                                       
         BL    TRAPERR                                                          
         CLI   0(R1),C'Z'                                                       
         BH    TRAPERR                                                          
VK28     BCTR  R1,0                                                             
         BCT   R0,VK26                                                          
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+12                                                             
         CLI   QSTANEW+4,C'L'      CANADA HAS NO LOW POWER STATIONS             
         BE    TRAPERR                                                          
         B     VK35                                                             
* CABLE                                                                         
VK30     CLI   QSTANEW,C'9'        THEN THIS IS CABLE                           
         BH    VK35                                                             
         MVI   CABLE,C'Y'                                                       
         MVI   QSTANEW+4,C'T'                                                   
         MVC   8(8,R2),QSTANEW                                                  
         MVI   12(R2),C' '                                                      
         CLC   STBNET,SPACES                                                    
         BNH   *+8                                                              
         MVI   12(R2),C'/'                                                      
         OI    6(R2),X'80'                                                      
         CLI   ACTNUM,ACTADD       IF ADDING A CABLE STATION                    
         BNE   VK35                                                             
         CLI   SSTCLIH+5,0         (GLOBAL)                                     
         BNE   VK35                                                             
*****                                                                           
         CLI   T217FFD+1,C'*'      DDS TERMINAL?                                
         BNE   VK33                NO                                           
         CLC   =C'ADDSYSCODE',SSTOPT   ALLOW DDS TO ADD ANY SYSCODE             
         BNE   VK33                      IF OPTION IS ADDSYSCODE                
         XC    SSTOPT,SSTOPT           CLEARS OUT AFTER EVERY ADD               
         MVI   SSTOPTH+5,0                                                      
         OI    SSTOPTH+6,X'80'                                                  
         B     VK35                                                             
*****                                                                           
VK33     CLC   QSTANEW(4),=C'7000' IT CAN ONLY BE BETWEEN 7000 - 7500           
         BL    INVERR                                                           
         CLC   QSTANEW(4),=C'7500' (THE REST ARE RESERVED FOR NCA)              
         BH    INVERR                                                           
         DROP  R4                                                               
*                                                                               
VK35     LA    R2,SSTOPTH          OPTIONS                                      
         XC    CANSTA,CANSTA                                                    
         MVI   REACTIVE,C'N'                                                    
         MVI   OLDMKTS,C'N'                                                     
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         CLC   =C'NFC',8(R2)                                                    
         BE    VK40                                                             
         CLC   =C'NO CLT',8(R2)                                                 
         BE    VK40                                                             
         CLC   =C'STUPID',8(R2)    CHECK PASSWORD TO REACTIVATE                 
         BNE   *+12                                                             
         MVI   REACTIVE,C'Y'                                                    
         B     VK40                                                             
*                                                                               
         CLC   =C'OLD',8(R2)       DISPLAYS OLD MKT NUMBERS IN NAME FLD         
         BNE   *+12                                                             
         MVI   OLDMKTS,C'Y'                                                     
         B     VK40                                                             
*                                                                               
         CLC   =C'NEXT',8(R2)      FORCE NEXT CANADIAN STATION #                
         BNE   *+14                                                             
         MVC   CANSTA,=X'FFFF'                                                  
         B     VK40                                                             
*                                                                               
         CLC   =C'XNUM=',8(R2)     FORCE SPECIFIC CANADIAN STATION #            
         BNE   INVERR                                                           
         GOTO1 SCANNER,DMCB,0(R2),(1,BLOCK)                                     
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         CLI   BLOCK+1,4                                                        
         BNE   INVERR                                                           
         GOTO1 HEXIN,DMCB,BLOCK+22,DUB,4                                        
         OC    DMCB+12(4),DMCB+12                                               
         BZ    INVERR                                                           
         CLC   DUB(2),=X'F000'     COMPARE INPUT TO MAX                         
         BNL   INVERR                                                           
         MVC   CANSTA,DUB                                                       
*                                                                               
VK40     DS    0H                  CHECK CLIENT DEFAULT & EXCEPTION             
         CLI   ACTNUM,ACTADD       FOR ACTION=ADD,                              
         BNE   VK50                                                             
         LA    R2,SSTSTAH          STATION FIELD                                
         CLI   QSTANEW+4,C'D'      DIGITAL VIDEO?                               
         BE    VK41                YES                                          
         CLI   QSTANEW+4,C'S'      STREAMING?                                   
         BE    VK41                YES                                          
         CLI   QSTANEW+4,C'C'      IHEARTRADIO?                                 
         BNE   VK45                NO                                           
VK41     BRAS  RE,CHKDSTA          DOES DSTA RECORD EXIST?                      
         BNE   NODSTA              NO - ERROR                                   
*                                                                               
VK45     LA    R2,SSTCLIH          IF CLIENT FIELD HAS INPUT                    
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         BRAS  RE,CKCLTDEF         MAKE SURE THE CLT DEFAULT EXISTS             
         BNE   NOCLTDEF                                                         
*                                                                               
VK50     DS    0H                                                               
         USING STARECD,R6                                                       
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   STAKEY(STAKEYLN),ZEROES                                          
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVC   STAKMED,QMED        MEDIA                                        
         MVC   STAKCALL,QSTANEW    CALL LETTERS                                 
         MVC   STAKAGY,AGENCY      AGENCY                                       
         MVC   STAKCLT,QCLT        CLIENT EXCEPTION                             
         TM    MISCFLG1,MF1OFFCD   WE HAVE OFFICE CODE?                         
         BZ    VKX                 NO                                           
         MVI   STAKCLT,C'*'                                                     
         MVC   STAKCLT+1(1),OFCOFC NEW 1 BYTE INTERNAL OFFICE CODE              
         MVI   STAKCLT+2,C' '      PAD LAST CHAR WITH SPACE                     
*                                                                               
VKX      BAS   RE,SETDEF           SET FILENAME & OTHER DEFAULTS                
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR20                YES                                          
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         CLC   STAKEY,KEYSAVE      DID WE GET THE REQUESTED RECORD?             
         BNE   RECNFERR            NO - REC NOT FOUND ERROR                     
         BAS   RE,GETACTV          GET ACTIVE REC (IN CASE WE HAVE PSV)         
*                                                                               
VR20     MVC   ORIGKEY,KEY         SAVE THE KEY                                 
         MVC   SVKEY,KEY           SAVE THE KEY                                 
         MVI   OLDMIDAS,0          CLEAR OLD MIDAS FIELD                        
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR60                YES                                          
         L     R6,AIO              A(CURRENT MASTER RECORD)                     
         USING STARECD,R6                                                       
         MVC   MYMKT,SMKT          OLD MKT (IF CHA DEL OLD PASSIVE PTR)         
         MVC   MYFORM,SFORMAT      OLD FMT (IF CHA DEL OLD PASSIVE PTR)         
         MVC   MYEFFDTE,SEFFDATE   OLD EDT (IF CHA DEL OLD PASSIVE PTR)         
         MVI   OLDLOCK,C'N'        OLD LOCK = N                                 
         TM    SFLAG1,SLOCK        IS LOCK ON?                                  
         BZ    *+8                 NO                                           
         MVI   OLDLOCK,C'Y'        YES - OLD LOCK = Y                           
         CLC   STAKLEN,=Y(STACRLNQ) RECORD LENGTH BIG ENOUGH FOR MIDAS          
         BNH   *+10                NO                                           
         MVC   OLDMIDAS,STMIDAS    YES - SAVE OLD MIDAS FIELD                   
         DROP  R6                                                               
*                                                                               
         L     R0,AIO2             SET 'TO' ADDRESS                             
         LA    R1,2000             SET 'TO' LENGTH                              
         L     RE,AIO              SET 'FROM' ADDRESS                           
         LR    RF,R1               SET 'FROM' LENGTH                            
         MVCL  R0,RE                                                            
*                                                                               
VR60     TM    MISCFLG1,MF1OFFCD   OFFICE CODE?                                 
         BO    VR70                YES - GO RIGHT TO THE PAYING REP             
*                                                                               
         LA    R2,SSTMKTH          A(MARKET FIELD)                              
         LLC   RE,5(R2)            INPUT LENGTH                                 
         CHI   RE,0                HAVE ANY DATA?                               
         BE    MISSERR             NO, ERROR                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)       HAVE ANY DATA?                               
         BZ    MISSERR             NO, ERROR                                    
*                                                                               
         GOTO1 VALIMKT             VALIDATE THE MARKET                          
         L     RF,AIO              A(MARKET RECORD)                             
         USING MKTRECD,RF                                                       
         MVC   MYALPMKT,MKTALST    SAVE ALPHA MARKET                            
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY? (DISP ONLY)                 
         BNE   VR65                NO                                           
         OI    SSTAMKDH+6,X'80'    XMIT DATA FIELD                              
         OI    SSTAMKTH+6,X'80'    XMIT INPUT FIELD                             
         OI    SSTAMKDH+1,X'20'    PROTECT FIELD                                
         OI    SSTAMKTH+1,X'20'    PROTECT FIELD                                
         MVC   SSTAMKD,SP@APHMK    MOVE DATA IN                                 
         MVC   SSTAMKT,MYALPMKT    MOVE IN ALPHAMKT FROM MKT REC                
*                                                                               
VR65     CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR70                YES                                          
         L     R0,AIO1             SET 'TO' ADDRESS                             
         LA    R1,2000             SET 'TO' LENGTH                              
         L     RE,AIO2             SET 'FROM' ADDRESS                           
         LR    RF,R1               SET 'FROM' LENGTH                            
         MVCL  R0,RE               RESTORE RECORD TO AIO1                       
*                                                                               
VR70     MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            USE AIO1                                     
         L     R6,AIO              A(MASTER RECORD)                             
         USING STARECD,R6                                                       
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VR80                NO                                           
         CLI   QSTANEW,C'0'        FIRST CHAR A DIGIT (THEN CABLE)?             
         BL    VR75                NO                                           
         CLI   QSTANEW,C'9'        FIRST CHAR A DIGIT (THEN CABLE)?             
         BH    VR75                NO                                           
         XC    STAREC(STARLNQ),STAREC                                           
         XC    SSYSNAME(STANCLNQ-STARLNQ),SSYSNAME                              
         XC    SCBL24(SCBLSQNQ-STANCLNQ),SCBL24                                 
         MVC   STAKLEN,=Y(SCBLSQNQ) RECORD LENGTH                               
         CLI   SSTCLIH+5,0         CLIENT SPECIFIC?                             
         BE    VR80                NO                                           
         MVC   SSYSNAME,SVSYSNAM   YES - SET CABLE SYSTEM NAME!                 
         B     VR80                                                             
*                                                                               
VR75     XC    STAREC(STARLNQ),STAREC                                           
         XC    SSYSNAME(STANCLNQ-STARLNQ),SSYSNAME                              
         MVC   STAKLEN,=Y(STANCLNQ) RECORD LENGTH                               
*                                                                               
VR80     MVC   0(L'STAKEY,R6),SVKEY                                             
         TM    MISCFLG1,MF1OFFCD   OFFICE CODE?                                 
         BNO   VR82                NOPE, CONTINUE NORMALLY                      
         MVC   SMKT,ZEROES         SMKT NEED CHAR 0'S INSTEAD OF NULLS          
         B     VR85                GO RIGHT TO THE PAYING REP                   
*                                                                               
VR82     MVC   SSTMKTN,MKTNM       SET MARKET NAME                              
         OI    SSTMKTNH+6,X'80'    TRASNMIT                                     
*                                                                               
         CLC   QMKT,=C'9999'       MARKET 9999?                                 
         BE    INVLDMKT            YES - INVALID                                
         CLC   QMKT,=C'9998'       MARKET 9998?                                 
         BE    INVLDMKT            YES - INVALID                                
*                                                                               
         MVC   SMKT,QMKT           SET EBCIDIC MARKET                           
         OC    MYMKT,MYMKT         WAS THERE A MARKET TO BEGIN WITH?            
         BZ    VR80B               NO                                           
         CLC   MYMKT,SMKT          DID THE MKT CHANGE?                          
         BNE   VR80A               YES                                          
         XC    MYMKT,MYMKT         NO - CLEAR MYMKT TO INDICATE NO CHG          
         B     VR85                                                             
*                                                                               
VR80A    CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   VR80AA              NO                                           
         CLI   QMED,C'R'           RADIO                                        
         BE    VR80AA              YES - ALLOW CHANGE TO/FROM MKT 0000          
**********************************************************************          
* SINCE WE NOW CHECK FOR EXISTING BUYS, THIS CHECK IS PROBABLY NOT              
* NEEDED, BUT INSTEAD OF REMOVING THE RESTRICTION ENTIRELY, I'VE                
* RELAXED THE CHECK TO ALLOW MEDIAOCEAN INTERNAL USERS TO MAKE THE              
* CHANGE                                          -HWON 7/6/2016                
**********************************************************************          
*                                                                               
         CLC   SMKT,ZEROES         NEW MARKET CHANGE TO MKT 0000?               
         BE    *+14                YES                                          
         CLC   MYMKT,ZEROES        OLD MKT BEING CHANGED FROM MKT 0000?         
         BNE   VR80AA              YES                                          
         CLI   1(RA),C'*'          DDS/MO TERMINAL?                             
         BNE   CANTCHG             NO, - CANNOT CHANGE TO MKT 0000              
*                                                                               
**********************************************************************          
* SINCE WE NOW CHECK FOR EXISTING BUYS, THIS CHECK IS PROBABLY NOT              
* NEEDED, BUT INSTEAD OF REMOVING THE RESTRICTION ENTIRELY, I'VE                
* RELAXED THE CHECK TO ALLOW MEDIAOCEAN INTERNAL USERS TO MAKE THE              
* CHANGE                                          -HWON 7/6/2016                
**********************************************************************          
*                                                                               
VR80AA   CLI   CABLE,C'Y'          CABLE?                                       
         BNE   VR80B               NO                                           
         OC    MYEFFDTE,MYEFFDTE   DO I HAVE AN EFFECTIVE DATE?                 
         BZ    VR80B               NO                                           
         XC    DMCB,DMCB           CLEAR DMCB                                   
         GOTO1 GETFACT,DMCB        CALL GETFACT TO GET TODAYS DATE              
         L     R1,0(R1)            FIRST PARAMETER                              
         USING FACTSD,R1                                                        
         CLC   MYEFFDTE,FADATEB    TODAY'S DATE PRIOR TO EFF DATE?              
         BH    VR80B               NO                                           
         MVI   ERROR,NOCHGERR      CAN'T CHANGE MARKET ERROR MESSAGE            
         MVC   SMKT,MYMKT          MKT NOT CHANGEABLE, RESTORE IT               
         B     TRAPERR             AND DISPLAY ERROR MESSAGE                    
         DROP  R1                                                               
*                                                                               
VR80B    BRAS  RE,VOLDMKT          ARE THERE BUYS FOR THE ORIG MKT?             
         BE    BUYEXIST            YES - GIVE BUY EXISTS ERROR                  
*                                                                               
VR85     LA    R2,SSTPRNTH         PARENT+SATELLITE                             
         MVI   SPARPLUS,C' '                                                    
         CLI   5(R2),0                                                          
         JE    VR86                                                             
*                                                                               
         CLC   STAKLEN,=Y(STACRLNQ) MAKE SURE REC LONG ENOUGH                   
         JNL   VR85A                                                            
         MVC   STAKLEN,=Y(STACRLNQ) ELSE UPDATE LENGTH                          
         XC    SSYSNAME,SSYSNAME   AND CLEAR NEW FIELDS                         
         XC    SSYSNETS,SSYSNETS                                                
*                                                                               
VR85A    MVC   SPARPLUS,8(R2)                                                   
         CLI   SPARPLUS,C'Y'                                                    
         JE    VR86                                                             
         CLI   SPARPLUS,C'N'                                                    
         JE    VR86                                                             
         J     INVERR                                                           
*                                                                               
VR86     LA    R2,SSTPREPH         PAYING REP                                   
         LA    R3,SSTPRNMH                                                      
         GOTO1 =A(EDTREP),DMCB,RR=Y                                             
         MVC   SPAYREP,MYREP                                                    
*                                                                               
         LA    R2,SSTNETH          NET INV FIELD                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR86A               NO                                           
         CLI   8(R2),C'Y'          INPUT = NO?                                  
         BE    *+12                YES                                          
         CLI   8(R2),C'N'          INPUT IS YES?                                
         BNE   INVERR              NO - ERROR                                   
         MVC   STNETINV,8(R2)                                                   
*                                                                               
VR86A    LA    R2,SSTPREPH         PAYING REP                                   
         TM    MISCFLG1,MF1OFFCD   OFFICE CODE?                                 
         BO    VRX                                                              
*                                                                               
         LA    R2,SSTTSRPH         TIME SHEET REP                               
         LA    R3,SSTTSRNH                                                      
         GOTO1 =A(EDTREP),DMCB,RR=Y                                             
         MVC   SCONREP,MYREP                                                    
*                                                                               
         LA    R2,SSTTRAFH         TRAFFIC REP                                  
         LA    R3,SSTTRANH                                                      
         GOTO1 =A(EDTREP),DMCB,RR=Y                                             
         MVC   STRFREP,MYREP                                                    
*                                                                               
         LA    R2,SSTFORMH         FORMAT                                       
         XC    SFORMAT,SFORMAT                                                  
         CLI   5(R2),0                                                          
         BE    VR87                                                             
         CLI   QMED,C'R'           ONLY FOR RADIO                               
         BNE   INVERR                                                           
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFORMAT(0),8(R2)                                                 
         OC    SFORMAT,SPACES                                                   
*                                                                               
VR87     MVC   QFORM,SFORMAT                                                    
         OC    MYFORM,MYFORM                                                    
         BZ    VR88                                                             
         CLC   QFORM,MYFORM                                                     
         BNE   VR88                                                             
         XC    MYFORM,MYFORM                                                    
*                                                                               
VR88     LA    R2,SSTCATH          CATEGORY                                     
         XC    SCATGRY,SCATGRY                                                  
         CLI   5(R2),0                                                          
         BE    VR89                                                             
         CLI   QMED,C'R'           ONLY FOR RADIO                               
         BNE   INVERR                                                           
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCATGRY(0),8(R2)                                                 
         OC    SCATGRY,SPACES                                                   
*                                                                               
VR89     LA    R2,SSTNTISH         NTI STATION                                  
**NOP    CLI   SSTMED,C'N'                                                      
**NOP    BNE   VR89C                                                            
         MVC   SNTISTA,8(R2)                                                    
         OC    SNTISTA,SPACES                                                   
*                                                                               
VR89C    LA    R2,SSTCHANH         CHANNEL                                      
         XC    SCHNL,SCHNL                                                      
         CLI   5(R2),0                                                          
         BE    VR100                                                            
         CLI   QMED,C'T'                                                        
         BNE   VR90                                                             
         CLI   5(R2),2                                                          
         BNE   INVERR              2 CHARACTERS FOR TV                          
         MVC   SCHNL(2),SSTCHAN                                                 
         B     VR100                                                            
*                                                                               
VR90     CLI   5(R2),4             4 CHARACTERS FOR RADIO                       
         BNE   INVERR                                                           
         MVC   SCHNL(4),SSTCHAN                                                 
*                                                                               
VR100    LA    R2,SSTAFFH          NETWORK AFF                                  
         XC    SNETWRK,SNETWRK                                                  
         XC    SCANNTWK,SCANNTWK                                                
         CLI   5(R2),0                                                          
         BE    VR120                                                            
         TM    4(R2),X'04'         TEST ALPHA                                   
         BZ    INVERR                                                           
         CLI   SVAPROF+7,C'C'      SEE IF CANADIAN NETWORK                      
         BNE   VR110                                                            
         MVC   SCANNTWK,8(R2)                                                   
         OC    SCANNTWK,SPACES                                                  
         B     VR120                                                            
*                                                                               
VR110    CLI   5(R2),3                                                          
         BH    INVERR                                                           
         MVC   SNETWRK,8(R2)                                                    
         OC    SNETWRK,SPACES                                                   
*                                                                               
VR120    LA    R2,SSTARBFH         ARB F/94                                     
         NI    SFLAG1,X'FF'-SQARBF94                                            
         CLI   5(R2),0                                                          
         BE    VR125                                                            
         CLI   8(R2),C'N'                                                       
         BE    VR125                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
         OI    SFLAG1,SQARBF94                                                  
*                                                                               
VR125    LA    R2,SSTTYPEH         STATION TYPE                                 
         MVI   STYPE,C' '                                                       
         MVI   SUBMEDIA,C' '                                                    
         CLI   5(R2),0                                                          
         BE    VR130                                                            
         MVC   STYPE,8(R2)                                                      
         MVC   SPTYPE,STYPE        MOVE DEFAULT                                 
*                                                                               
VR130    LA    R2,SSTFAXH          FAX NUMBER                                   
         XC    SFAX,SFAX                                                        
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   SFAX,8(R2)                                                       
*                                                                               
         LA    R2,SSTDAYLH         DAYLIGHT TIME                                
         NI    SFLAG1,X'FF'-SQNODST                                             
         CLI   5(R2),0                                                          
         BE    VR140                                                            
         CLI   8(R2),C'Y'                                                       
         BE    VR140                                                            
         CLI   8(R2),C'N'                                                       
         BNE   INVERR                                                           
         OI    SFLAG1,SQNODST                                                   
*                                                                               
VR140    LA    R2,SSTTAXH                                                       
         XC    SNEWTAX,SNEWTAX     NEW TAX RATE                                 
         CLI   5(R2),0                                                          
         BE    VR160                                                            
         LLC   R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(3,8(R2)),(R5)                                      
         CLI   DMCB,X'FF'          INVALID                                      
         BE    INVERR                                                           
         L     R5,DMCB+4                                                        
         C     R5,=F'32767'                                                     
         BH    INVERR                                                           
         LTR   R5,R5                                                            
         BM    INVERR                                                           
         STCM  R5,3,SNEWTAX                                                     
*                                                                               
VR160    LA    R2,SSTSIZEH                                                      
         MVI   SSIZE,0                                                          
         CLI   5(R2),0                                                          
         BE    VR170                                                            
         CLI   8(R2),C'A'                                                       
         BL    INVERR                                                           
         CLI   8(R2),C'Z'                                                       
         BH    INVERR                                                           
         MVC   SSIZE,8(R2)                                                      
*                                                                               
VR170    LA    R2,SSTCTYH          COUNTRY                                      
         MVI   SCOUNTRY,0                                                       
         CLI   SVAPROF+7,C'C'      ONLY FOR CANADA                              
         BNE   VR200                                                            
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    VR200                                                            
         CLI   5(R2),3                                                          
         BH    INVERR                                                           
         BCTR  RE,0                                                             
         LA    R1,EDTCOUN                                                       
*                                                                               
VR180    CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),8(R2)                                                    
         BE    VR190                                                            
         LA    R1,3(R1)                                                         
         B     VR180                                                            
*                                                                               
VR190    MVC   SCOUNTRY,0(R1)                                                   
*                                                                               
VR200    LA    R2,SSTSOPTH         SPECIAL OPTIONS FIELD                        
         MVI   STMIDAS,0           DEFAULT TO NO MIDAS                          
         NI    SFLAG1,X'FF'-STPG   DEFAULT TO NO P&G (NO RSTA)                  
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR210               NO                                           
         CLC   =C'RSTA',8(R2)      RESTRICTED STATION?                          
         BE    VR202               YES                                          
         CLC   =C'P&&G',8(R2)      P&G OPTION?                                  
         BNE   VR205               NO                                           
VR202    OI    SFLAG1,STPG         TURN ON P&G                                  
         B     VR210               NO MIDAS IF P&G                              
*                                                                               
VR205    CLI   8(R2),C'M'          MIDAS STATION?                               
         BE    *+12                YES                                          
         CLI   8(R2),C'C'          MIDAS TRADE CREDIT STA?                      
         BNE   INVERR              NO - ERROR                                   
         MVC   STMIDAS,8(R2)       MOVE M/C TO RECORD                           
*                                                                               
VR210    CLI   OLDMIDAS,0          MIDAS FIELD WAS NULL?                        
         BE    VR215               YES - CAN BE CHANGED                         
         CLC   STMIDAS,OLDMIDAS    HAS FIELD CHANGED                            
         BNE   ERRMIDAS            YES - ERROR                                  
*                                                                               
VR215    XC    SCANTAX,SCANTAX                                                  
         XC    SSVCFEE,SSVCFEE                                                  
         CLI   SVAPROF+7,C'C'      ONLY FOR CANADA                              
         BNE   VR220                                                            
         LA    R2,SSTCTAXH         CANADIAN C-58 TAX                            
         BRAS  RE,VAMT                                                          
         STCM  R5,3,SCANTAX                                                     
*                                                                               
         LA    R2,SSTFEEH          CANADIAN C-58 MEDIA SERVICE FEE              
         BRAS  RE,VAMT                                                          
         STCM  R5,3,SSVCFEE                                                     
*                                                                               
VR220    CLI   SSTCLIH+5,0         CLIENT SPECIFIC?                             
         BNE   VR235               YES - LOCK FIELD PROTECTED                   
         NI    SFLAG1,X'FF'-SLOCK  DEFAULT TO NO LOCK                           
         MVI   BYTE,C'N'           LOCK = NO                                    
         LA    R2,SSTLOCKH         VENDOR LOCK FIELD                            
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR230               NO                                           
         CLI   8(R2),C'N'          INPUT = NO?                                  
         BE    VR230               YES                                          
         CLI   8(R2),C'Y'          INPUT IS YES?                                
         BNE   INVERR              NO - ERROR                                   
         OI    SFLAG1,SLOCK        SET LOCK=Y FLAG                              
         MVI   BYTE,C'Y'           LOCK = YES                                   
*                                                                               
VR230    CLC   OLDLOCK,BYTE        DID LOCK VALUE CHANGE?                       
         BE    VR235               NO                                           
         OI    MISCFLG1,MF1LOCK    YES - SET LOCK CHANGED                       
*                                                                               
VR235    LA    R2,SSTGSTH          GOODS & SERVICE TAX CODE                     
         MVI   SGSTCODE,0                                                       
         CLI   5(R2),0                                                          
         BE    VR270                                                            
         LA    R5,GSTTAB                                                        
*                                                                               
VR250    CLC   0(1,R5),SSTGST     VALID GST CODE?                               
         BE    VR260                                                            
         CLI   0(R5),X'FF'        END OF TABLE?                                 
         BE    INVERR             INVALID GST CODE                              
         LA    R5,1(R5)           NEXT ENTRY IN GST TABLE                       
         B     VR250                                                            
*                                                                               
VR260    MVC   SGSTCODE,SSTGST    MOVE IN CORRECT GST CODE                      
*                                                                               
VR270    MVI   SBKTYPE,0                                                        
         LA    R2,SSTBOOKH         BOOKTPE                                      
         CLI   SSTBOOKH+5,0        ANY INPUT?                                   
         BE    VR275                                                            
         CLI   SSTBOOK,C'T'        LCON WANTS 'T' BOOKS TO BE VALID             
         BE    VR272               EVEN WITHOUT A CLIENT CODE                   
         CLI   SSTBOOK,C'P'        JVAN WANTS 'P' BOOKS TO BE VALID             
         BE    VR272               EVEN WITHOUT A CLIENT CODE                   
         CLI   SSTBOOK,C'L'        RPLA WANTS 'L' BOOKS TO BE VALID             
         BE    VR272               EVEN WITHOUT A CLIENT CODE                   
         CLC   QCLT,ZEROES                                                      
         BE    INVERR              INVALID INPUT                                
*                                                                               
VR272    OC    SSTBOOK,SPACES                                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
VR274    CLI   0(RF),X'FF'                                                      
         BNE   *+12                                                             
         MVI   ERROR,BOOKERR                                                    
         B     TRAPERR                                                          
         CLC   SSTBOOK,SPBKTYPA                                                 
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     VR274                                                            
         MVC   SBKTYPE,SPBKTYPN                                                 
         DROP  RF                                                               
*                                                                               
VR275    LA    R2,SSTEIXH          EIX                                          
         MVI   SEIXSTA,0                                                        
         CLI   SSTEIXH+5,0                                                      
         BE    VR276                                                            
         CLI   8(R2),C'Y'                                                       
         BE    VR275A                                                           
         CLI   8(R2),C'N'                                                       
         BE    VR275A                                                           
         B     INVERR                                                           
VR275A   MVC   SEIXSTA,8(R2)                                                    
*                                                                               
VR276    DS    0H                  ALPHA MKT FIELD                              
         CLI   QMED,C'R'            ONLY FOR RADIO                              
         BNE   VR277                                                            
         XC    SMKTALPH,SMKTALPH    ASSUME NO INPUT                             
         LA    R2,SSTAMKTH                                                      
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VR280                                                            
         BAS   RE,VALAMKT            YEP, GO VALIDATE IT                        
         BZ    INVERR                                                           
         MVC   SMKTALPH,DUB         IF VALID, DUB(3) HAS ALPHA-MKT              
         B     VR280                                                            
*                                                                               
VR277    CLI   SVAPROF+7,C'C'      ALPHAMKT FOR CANADA TV TOO                   
         BNE   VR280                                                            
         CLI   QMED,C'T'                                                        
         BNE   VR280                                                            
         MVC   SMKTALPH,MYALPMKT   COMES DIRECTLY FROM MKT RECORD               
*                                                                               
VR280    LA    R2,SSTPSTH          PROVINCE SERVICE TAX CODE                    
         XC    SPST,SPST                                                        
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BRAS  RE,VALPST                                                        
*                                                                               
         XC    SRS1CALL(L'SRS1CALL+L'SRS2CALL),SRS1CALL                         
         NI    SFLAG1,X'FF'-(SQNORS1I+SQNORS2I)                                 
         MVI   BYTE,C'N'                                                        
         BRAS  RE,VALRSC           VALIDATE RTG SVC CALL LETTER FIELDS          
         BNE   INVERR                                                           
         MVI   BYTE,C'A'                                                        
         BRAS  RE,VALRSC                                                        
         BNE   INVERR                                                           
*                                                                               
VR285    DS    0H                                                               
*VR285    CLI   CABLE,C'Y'          IF THIS IS CABLE                            
*         BNE   VR330                                                           
*         LA    R2,SSTCSNH          GET SYSTEM NAME                             
*         OI    4(R2),X'80'                                                     
*         GOTO1 ANY                                                             
*         MVI   4(R2),X'80'                                                     
*         MVC   SSYSNAME,WORK                                                   
*         OC    SSYSNAME,SPACES                                                 
*         MVC   SVNAME,SSYSNAME                                                 
*                                                                               
*         MVC   SVSYSNET(16),SSYSNETS   SAVE NETWORKS                           
*         MVC   SVSCBL24,SCBL24    BEFORE WE CHANGE SCBL24 LETS SAVE IT         
*         MVC   SVSEQORG,SCBLSEQ                                                
*         LA    R2,SSTCNL1H        CHECK ALL NETWORK LINE TO SEE IF             
*         TM    4(R2),X'20'            PREVIOUSLY VALIDATED                     
*         BZ    VR287                                                           
*         LA    R2,SSTCNL2H                                                     
*         TM    4(R2),X'20'                                                     
*         BZ    VR287                                                           
*         LA    R2,SSTCNL3H                                                     
*         TM    4(R2),X'20'                                                     
*         BNZ   VR320               YES, LEAVE NETWORKS ALONE                   
*                                                                               
*VR287    LA    R2,SSTCNL1H         & NETWORK LIST                              
*         XC    SSYSNETS(16),SSYSNETS                                           
*         XC    SCBL24,SCBL24      CLEAR FIELDS SO WE CAN BUILT IT FROM         
**        XC    SCBLSEQ,SCBLSEQ    NETWORK LIST ON THE SCREEN                   
**                                                                              
*         CLC   STAKCLT,=C'000'    IF CLT SPECIFIC DONT CHECK NETWORKS          
*         BNE   VR325                                                           
**                                                                              
*VR290    CLI   5(R2),0                                                         
*         BE    VR310                                                           
*         LA    R5,SSTWORK          NEED BIG BLOCK - USE TWA                    
*         AH    R5,=H'6000'                                                     
*         GOTO1 SCANNER,DMCB,0(R2),(25,(R5))                                    
*         CLI   4(R1),0                                                         
*         BE    INVERR                                                          
*         LLC   R3,4(R1)                                                        
**                                                                              
*VR300    GOTO1 =A(CHKNET),DMCB,(RC),RR=RELO                                    
*         BAS   RE,SETSYS                                                       
*         LA    R5,32(R5)           BUMP TO NEXT SCANNER ENTRY                  
*         BCT   R3,VR300                                                        
**                                                                              
*VR310    DS    0H                                                              
*         LA    R1,SSTCNL1H                                                     
*         CR    R2,R1                                                           
*         BNE   *+12                                                            
*         LA    R2,SSTCNL2H                                                     
*         B     VR290                                                           
*                                                                               
*         LA    R1,SSTCNL2H         DID WE DO THE SECOND LINE                   
*         CR    R2,R1                                                           
*         BNE   *+12                                                            
*         LA    R2,SSTCNL3H         & 2ND NETWORK LIST                          
*         B     VR290                                                           
*                                                                               
*         LA    R1,SSTCNL3H                                                     
*         CR    R2,R1                                                           
*         BE    VR320                                                           
*         DC    H'0'                                                            
**                                                                              
VR320    CLI   ACTNUM,ACTADD       IF ADD                                       
         BE    VR330                THEN GO EXIT                                
*         LA    R2,SSTCNL1H                                                     
*                                                                               
*  CKDELNT2 CKECKS IF NETWORK DELETE BY CHECKING THE SCBL24 AND SCBLSEQ         
*  FIELDS.  SVNTEW IS STORAGE OF NEW TOP 24 LIST FROM SCREEN                    
*                                                                               
*         MVC   SVNTNEW,SCBL24                                                  
*         MVC   SVSEQNEW,SCBLSEQ                                                
*         GOTO1 =A(CKDELNT2),DMCB,RR=RELO                                       
*                                                                               
*         BNE   CHGERR                                                          
VR325    XC    SEFFDATE,SEFFDATE    ELSE, CLEAR EFFECTIVE DATE                  
         MVC   SSTEFF,SPACES                                                    
         OI    SSTEFFH+6,X'80'                                                  
*                                                                               
VR330    LA    R2,SSTDEACH         DEACTIVATED FIELD                            
         TM    SSYSDACT,X'FF'                                                   
         BO    VR340                                                            
         CLI   5(R2),0                                                          
         BE    VR360                                                            
*                                                                               
VR340    CLI   REACTIVE,C'Y'                                                    
         BNE   VR350                                                            
         CLI   8(R2),C' '                                                       
         BE    VR345                                                            
         CLI   8(R2),C'N'                                                       
         BNE   INVERR                                                           
                                                                                
VR345    MVI   SSYSDACT,0                                                       
         BRAS  RE,DEACBL           REACTIVATE CBL RECORD                        
         MVI   8(R2),C' '                                                       
         OI    6(R2),X'80'                                                      
         B     VR360                                                            
                                                                                
VR350    CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
         OI    SSYSDACT,X'FF'                                                   
         BRAS  RE,DEACBL           DEACTIVATE CBL RECORD                        
*                                                                               
* UNIQUE ID VALIDATION                                                          
*                                                                               
VR360    DS    0H                                                               
***      CLI   QMED,C'R'              RADIO?                                    
***      BNE   VR390                  NO - DO NOT VALIDATE FIELD                
         TM    SVAGYFL2,AGYFLAG2_UID  UID FLAG SET?                             
         BZ    VR390                  NO - DO NOT VALIDATE FIELD                
*                                                                               
         LA    R2,SSTUNIQH            SCREEN UNIQ ID FIELD                      
         CLC   STAKCLT,ZEROES         CLIENT-SPECIFIC?                          
         BE    *+12                   NO - SKIP INPUT CHECK                     
         CLI   5(R2),0                                                          
         BNE   INVERR                 NO UNIQUE IDS ALLOWED                     
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR380                                                            
         CLI   SVAPROF+7,C'C'     CANADIAN?                                     
         BE    VR380                                                            
         CLC   STAKCLT,ZEROES         CLIENT-SPECIFIC?                          
         BNE   VR380                  YES - DO NOT AUTO ADD UNIQ ID             
*                                                                               
* READ RAD PASSIVE POINTER:  ACTION ADD, MEDIA=R, NON-CANADA ONLY               
*                                                                               
         XC    STUNIQID,STUNIQID                                                
         MVC   SVKEY48,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CT9ARECD,R2                                                      
         MVI   CT9AKTYP,CT9AKTYQ                                                
         MVI   CT9AKMED,C'R'                                                    
         MVC   CT9AKCLL,SVKEY48+STAKCALL-STAKEY  STA CALL LETTERS               
         L     R2,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R2)                      
         CLC   0(CT9AKDTE-CT9AKEY,R2),KEY     SAME KEY?                         
         BNE   VR370                    NO - CHECK SCREEN INPUT                 
         CLC   CT9AUID,SPACES           ANY UNIQUE ID'S?                        
         BNH   *+10                     NO - DO NOT COPY                        
         MVC   STUNIQID,CT9AUID                                                 
         MVC   KEY,SVKEY48              RESTORE KEY                             
         B     VR390             DONE - TAKE CARE OF RECORD LENGTH              
         DROP  R2                                                               
*                                                                               
VR370    DS    0H                                                               
         MVC   KEY,SVKEY48          RESTORE KEY AFTER READHI                    
*                                                                               
VR380    DS    0H                                                               
         LA    R2,SSTUNIQH            SCREEN UNIQ ID FIELD                      
         TM    4(R2),X'80'            INPUT THIS TIME?                          
         BZ    VR390              DO NOT VALIDATE, IF FIELD UNCHANGED           
*                                                                               
         XC    STUNIQID,STUNIQID                                                
*                                                                               
         CLI   5(R2),0                ANY INPUT?                                
         BE    VR390                  NO-SKIP ALL THE VALIDATION                
*                                                                               
         TM    4(R2),X'08'            VALID NUMERIC?                            
         BNO   VR385                  NO - GO TO ALPHA PART                     
*                                                                               
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  WORK(6),8(0,R2)        PACK AND THEN                             
         UNPK  UNIQTMP(6),WORK(6)     UNPACK TO RIGHT-ALIGN                     
         B     VR387                  CHECK IF VALID                            
*                                                                               
* !!! ONLY DDS TERMINALS ARE ALLOWED ALPANUMERIC INPUTS !!!                     
*                                                                               
VR385    DS    0H                     ALPHANUMERIC INPUT HERE                   
*                                                                               
         CLI   1(RA),C'*'             DDS TERMINAL?                             
         BNE   INVERR                 NO - ALPHA INPUT NOT ALLOWED              
*                                                                               
         CLI   5(R2),6                ALPHA INPUT MUST BE 6 CHARACTERS          
         BNE   INVERR                                                           
         MVC   UNIQTMP,SSTUNIQ                                                  
*                                                                               
VR387    BRAS  RE,RDRAD               CHECK X'99' RECORD                        
         BNE   INVERR                                                           
*                                                                               
         MVC   STUNIQID,UNIQTMP                                                 
*                                                                               
VR390    DS    0H                                                               
         MVI   SNETTYPE,0                                                       
         LA    R2,SSTNTYPH            NETWORK TYPE                              
         CLI   5(R2),0                ANY INPUT?                                
         BE    VR390B                 NO                                        
         CLI   8(R2),C'N'             NATIONAL?                                 
         BE    VR390A                 YES                                       
         CLI   8(R2),C'R'             REGIONAL?                                 
         BE    VR390A                 YES                                       
         CLI   8(R2),C'S'             SPECIAL?                                  
         BNE   INVERR                 NO, ERROR                                 
*                                                                               
VR390A   MVC   SNETTYPE,8(R2)                                                   
*                                                                               
VR390B   MVC   STAKLEN,=AL2(STANCLNQ)                                           
         CLI   CABLE,C'Y'                                                       
         BNE   *+10                                                             
         MVC   STAKLEN,=AL2(SCBLSQNQ)                                           
*                                                                               
VR400    MVI   SCBKTYPE,0                                                       
         LA    R2,SSTCBTYH            COMSCORE BOOKTYPE                         
         CLI   5(R2),0                ANY INPUT?                                
         BE    VRX                    NONE                                      
         CLI   5(R2),1                CURRENT OPTION IS 'L' FOR LIVE            
         BNE   INVERR                                                           
         CLI   8(R2),C'L'                                                       
         BNE   INVERR                                                           
         MVC   SCBKTYPE,8(R2)                                                   
*                                                                               
VRX      MVC   AIO,AIO1            RESET AIO TO STATION MASTER                  
         CLI   ACTNUM,ACTADD       TEST ADD                                     
         BNE   VRX10                                                            
         GOTO1 CNRECS,DMCB,C'A'                                                 
         B     VRX20                                                            
*                                                                               
VRX10    GOTO1 CNRECS,DMCB,C'C'                                                 
*                                                                               
VRX20    TM    MISCFLG1,MF1OFFCD   OFFICE CODE?                                 
         BO    VRXX                WE DON'T NEED PASSIVE OR REQREC              
         BAS   RE,BLDPASS           BUILD PASSIVE POINTER                       
         MVC   KEY(L'SVKEY),SVKEY   RESTORE KEY                                 
         MVC   AIO,AIO1                                                         
*        BAS   RE,REQREC                                                        
         GOTO1 =A(REQREC),DMCB,RR=Y                                             
*                                                                               
VRXX     J     EXIT                                                             
         EJECT                                                                  
*        SET CABLE SYSTEM FIELD                                                 
*                                                                               
*&&DO                                                                           
         USING STARECD,R6                                                       
SETSYS   NTR1                                                                   
         LLC   R1,SVSYSNT          THIS IS THE NUMBER OF BITS TO MOVE           
         XC    WORK(16),WORK                                                    
         LA    R3,WORK                                                          
         CHI   R1,64                                                            
         BNH   *+12                                                             
         LA    R3,8(R3)                                                         
         SHI   R1,64                                                            
         BCTR  R1,0                                                             
         LA    R4,1                SET HIGH ORDER BIT ON IN R4-R5               
         SLL   R4,31                                                            
         SR    R5,R5                                                            
         LTR   R1,R1                                                            
         BZ    SSYS10                                                           
                                                                                
         SRDL  R4,1                                                             
         BCT   R1,*-4                                                           
*                                                                               
SSYS10   STM   R4,R5,0(R3)                                                      
         OC    SSYSNETS(16),WORK                                                
         GOTO1 GENEL                                                            
         MVC   STAKLEN(2),=Y(SCBLSQNQ)                                          
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ============== INSERT TWO FIELDS, SCBL24 AND SCBLSEQ ==============*          
* ============== FOR CABLE STATIONS ONLY ============================*          
GENEL    NTR1                                                                   
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         MVI   STAPACT,C'C'        CHECK CBLNET EXISTS                          
         MVC   STAPQMKT,ZEROES                                                  
         MVC   STAPQSTA,=C'0000/'                                               
         MVC   STAPQNET,SVNETWK                                                 
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0           SHOULDN'T BE ANY ERROR                       
         BE    *+6                 EVERYTHING HAS BEEN VALIDATED                
         DC    H'0'                                                             
*                                                                               
         TM    STAPNFLG,X'40'                                                   
         BNO   GN50                                                             
*                                                                               
         NI    STAPNFLG,X'BF'      TURN OFF X'40' TO GET POSITION NUMB          
         L     RE,=X'01000000'                                                  
*                                                                               
         LLC   R5,STAPNFLG                                                      
         SR    R3,R3                                                            
*                                                                               
GN30     CR    R3,R5                                                            
         BNL   GN40                                                             
         LA    R3,1(R3)                                                         
         SRL   RE,1                                                             
         B     GN30                                                             
*                                                                               
GN40     ST    RE,FULL                                                          
         OC    SCBL24,FULL+1                                                    
         B     GENX                                                             
*                                                                               
GN50     LA    RE,SCBLSEQ                                                       
*                                                                               
GN60     CLI   0(RE),0                                                          
         BNE   *+14                                                             
         MVC   0(1,RE),STAPNSEQ                                                 
         B     GENX                                                             
         CLC   STAPNSEQ,0(RE)                                                   
         BE    GENX                                                             
         LA    RE,1(RE)                                                         
         B     GN60                                                             
*                                                                               
GENX     J     EXIT                                                             
*&&                                                                             
*                                                                               
* TEST IF COMSCORE - DISPLAY FIELDS                                             
*                                                                               
TSTCOMS  NTR1                                                                   
         TM    USRIDFLG,USRRNTKQ   USER HAS ACCESS TO COMSCORE DATA?            
         JZ    *+12                NO                                           
         CLI   QMED,C'T'           TELEVISION?                                  
         JE    TCOMS10                                                          
*                                                                               
         OI    SSTCBTXH+1,X'0C'    NO, DON'T SHOW COMSCORE BKTYP LABEL          
         OI    SSTCBTXH+6,X'80'       LOW INTENSITY AND TRANSMIT                
         XC    SSTCBTY,SSTCBTY        CLEAR COMSCORE BOOKTYPE                   
         OI    SSTCBTYH+1,X'20'       PROTECTED FIELD                           
         OI    SSTCBTYH+6,X'80'       TRANSMIT                                  
         J     TSTCOMSX                                                         
*                                                                               
TCOMS10  NI    SSTCBTXH+1,X'FF'-X'0C' SHOW COMSCORE BOOKTYPE LABEL              
         OI    SSTCBTXH+6,X'80'       REG INTENSITY AND TRANSMIT                
         NI    SSTCBTYH+1,X'FF'-X'20' UNPROTECT FIELD                           
         OI    SSTCBTYH+6,X'80'       TRANSMIT                                  
*                                                                               
TSTCOMSX J     EXIT                                                             
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DRL      NTR1                                                                   
*                                                                               
DR       TWAXC SSTMKTH                                                          
*                                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         CLC   KEYSAVE(STAKEYLN),0(R6)   DID WE GET REQUESTED RECORD            
         BNE   RECNFERR                                                         
*                                                                               
         MVC   WORK(4),=C'****'                                                 
         MVC   WORK+1(2),AGENCY                                                 
         CLC   FIRSTLBL,WORK              UNIQUE LABEL PRESENT?                 
         BE    *+8                        YES - DO NOT READ AGY RECORD          
         BRAS  RE,GETUIDFL                                                      
*                                                                               
***      CLI   QMED,C'R'                                                        
***      BNE   DR02                                                             
         TM    SVAGYFL2,AGYFLAG2_UID                                            
         BZ    DR02                                                             
         NI    SSTUNIQH+1,X'FF'-X'20'-X'0C'   UNPROTECT, VISIBLE                
         OI    SSTUNIQH+6,X'80'                                                 
         NI    SSTUNQTH+1,X'FF'-X'0C'                                           
         OI    SSTUNQTH+6,X'80'                                                 
*                                                                               
DR02     DS    0H                                                               
         BRAS  RE,HIDEFLDS         SHOW/HIDE SCREEN FIELDS                      
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(4),ZEROES                                                   
         MVC   WORK+10(5),STAKCALL                                              
*                                                                               
*  GET THE PACKED MKT/STA SO I MAY USE IT FOR MSUNPK LATER                      
         GOTO1 MSPACK,DMCB,WORK,WORK+10,WORK+15                                 
         MVC   MYMKTSTA,WORK+15                                                 
*                                                                               
         DS    0H                  RESOLVE DICTIONARY REFERENCES                
         GOTO1 DICTATE,DMCB,C'LL  ',DDDCLIST,DDDSLIST                           
                                                                                
         MVC   SSTMKT,SMKT                                                      
*        BAS   RE,FMTMKT                                                        
         BRAS  RE,FMTMKT                                                        
         CLI   OLDMKTS,C'Y'        SHOW OLD MARKETS                             
         BNE   DR05                                                             
         XC    SSTMKTN,SSTMKTN     SET MARKET NAME                              
         MVC   SSTMKTN(4),=C'NONE'                                              
         CLC   STAKLEN,=Y(STACRLNQ)                                             
         BNH   DR05                                                             
         EDIT  (B2,STOLDMK1),(4,SSTMKTN),FILL=0                                 
         OC    STOLDMK2,STOLDMK2                                                
         BZ    DR05                                                             
         MVI   SSTMKTN+4,C','                                                   
         EDIT  (B2,STOLDMK2),(4,SSTMKTN+5),FILL=0                               
                                                                                
DR05     DS    0H                  ALPHA MARKET                                 
         OI    SSTAMKDH+6,X'80'     XMIT DATA FIELD                             
         OI    SSTAMKTH+6,X'80'     XMIT INPUT FIELD                            
         XC    SSTAMKD,SSTAMKD      CLEAR DATA FIELD                            
         OI    SSTAMKDH+1,X'20'     PROTECT FIELDS                              
         OI    SSTAMKTH+1,X'20'                                                 
         CLI   SVAPROF+7,C'C'       CANADIAN AGENCY? (DISP ONLY)                
         BNE   DR05A                NO                                          
         MVC   SSTAMKD,SP@APHMK     MOVE DATA IN                                
         MVC   SSTAMKT,MYALPMKT     YES, MOVE IN ALPHAMKT FROM MT REC           
         B     DR10                                                             
*                                                                               
DR05A    CLI   QMED,C'R'            ALPHA MKT APPLICABLE TO RADIO ONLY          
         BE    DR06                                                             
         CLI   SVAPROF+7,C'C'       AND NOW CANADIAN TV (DISP ONLY)             
         BNE   DR10                                                             
         CLI   QMED,C'T'                                                        
         BNE   DR10                                                             
         B     *+8                  LEAVE PROTECTED                             
DR06     NI    SSTAMKTH+1,X'FF'-X'20'                                           
         MVC   SSTAMKD,SP@APHMK     MOVE DATA IN                                
         OC    SMKTALPH,SMKTALPH    ANYTHING FROM RECORD?                       
         BZ    DR10                  NOPE                                       
         MVC   SSTAMKT,SMKTALPH      YEP, MOVE IN ALPHAMKT                      
*                                                                               
DR10     DS    0H                                                               
*                                                                               
         MVI   SSTPRNT,C' '                                                     
         CLC   STAKLEN,=Y(STACRLNQ)                                             
         JL    *+10                                                             
         MVC   SSTPRNT,SPARPLUS    MOVE PAR+ SAT FLAG                           
         OI    SSTPRNTH+6,X'80'                                                 
*                                                                               
         MVC   SSTPREP,SPAYREP     PAYING REP                                   
         LA    R3,SPAYREP                                                       
         LA    R2,SSTPRNMH                                                      
         BAS   RE,FMTREP                                                        
*                                                                               
         MVC   SSTTSRP,SCONREP                                                  
         LA    R3,SSTTSRP          TIME SHEET REP                               
         LA    R2,SSTTSRNH                                                      
         BAS   RE,FMTREP                                                        
*                                                                               
         MVC   SSTTRAF,STRFREP                                                  
         LA    R3,SSTTRAF         TRAFFIC REP                                   
         LA    R2,SSTTRANH                                                      
         BAS   RE,FMTREP                                                        
*                                                                               
         MVC   SSTFORM,SFORMAT    FORMAT                                        
         MVC   SSTCAT,SCATGRY     CATEGORY                                      
         MVC   SSTTYPE,STYPE                                                    
         MVC   SSTNTIS,SNTISTA    NTI STATION                                   
         MVC   SSTCHAN,SCHNL      CHANNEL                                       
         CLI   SVAPROF+7,C'C'     SEE IF CANADIAN NETWORK                       
         BNE   DR20                                                             
         MVC   SSTAFF(4),SCANNTWK                                               
         B     DR22                                                             
*                                                                               
DR20     MVC   SSTAFF(3),SNETWRK  NETWORK AFF                                   
         TM    SFLAG1,SQARBF94    ARB F/94                                      
         BNO   *+8                                                              
         MVI   SSTARBF,C'Y'                                                     
*                                                                               
DR22     MVC   SSTFAX,SFAX        FAX NUMBER                                    
         MVI   SSTDAYL,C'Y'       DAYLIGHT TIME                                 
         TM    SFLAG1,SQNODST                                                   
         BNO   *+8                                                              
         MVI   SSTDAYL,C'N'                                                     
***                                                                             
         CLI   SVAPROF+7,C'C'      WE BE CANADIAN?                              
         BNE   DR22E                                                            
         XC    SNEWTAX,SNEWTAX                                                  
***                                                                             
DR22E    EDIT  (B2,SNEWTAX),(6,SSTTAX),3,ZERO=BLANK,ALIGN=LEFT                  
         MVC   SSTSIZE,SSIZE                                                    
         CLI   SVAPROF+7,C'C'      ONLY FOR CANADA                              
         BNE   DR40                                                             
         MVC   SSTCTY,SCOUNTRY     COUNTRY                                      
         EDIT  SCANTAX,(5,SSTCTAX),2                                            
         EDIT  SSVCFEE,(5,SSTFEE),2                                             
         CLI   SSTMED,C'T'         ONLY FOR CANADA T                            
         BNE   DR40                                                             
         CLC   STAKLEN,=Y(STANCLNQ)     ENSURE REC HAS FIELDS!                  
         BL    DR40                                                             
         MVC   SSTRS1C,SRS1CALL    CSI CALL LETTERS                             
         OC    SSTRS1C,SPACES      NULLS->SPACES                                
         CLC   SSTRS1C,SPACES                                                   
         BE    DR35                                                             
         MVI   SSTRS1F,C'Y'        IMPS FLAG                                    
         TM    SFLAG1,SQNORS1I                                                  
         BZ    *+8                                                              
         MVI   SSTRS1F,C'N'                                                     
DR35     MVC   SSTRS2C,SRS2CALL    BBM CALL LETTERS                             
         OC    SSTRS2C,SPACES      NULLS->SPACES                                
         CLC   SSTRS2C,SPACES                                                   
         BE    DR40                                                             
         MVI   SSTRS2F,C'Y'        IMPS FLAG                                    
         TM    SFLAG1,SQNORS2I                                                  
         BZ    *+8                                                              
         MVI   SSTRS2F,C'N'                                                     
*                                                                               
DR40     XC    SSTSOPT,SSTSOPT     CLEAR SPECIAL OPTIONS FIELD                  
         TM    SFLAG1,STPG         P&G TURNED ON?                               
         BZ    *+14                NO                                           
******   MVC   SSTSOPT(3),=C'P&&G' YES - DISPLAY P&G                            
         MVC   SSTSOPT(4),=C'RSTA' YES - DISPLAY RSTA                           
         B     DR40AA              NO MIDAS IF P&G                              
         CLC   STAKLEN,=Y(STACRLNQ)                                             
         BNH   *+10                                                             
         MVC   SSTSOPT(1),STMIDAS  MIDAS FIELD                                  
*                                                                               
DR40AA   MVI   SSTLOCK,C'N'        DEFAULT TO NO LOCK                           
         TM    SFLAG1,SLOCK        LOCK=Y FLAG SET?                             
         BZ    *+8                 NO                                           
         MVI   SSTLOCK,C'Y'        YES                                          
*                                                                               
         MVC   SSTGST,SGSTCODE     MOVE IN CORRECT GST CODE                     
*                                                                               
         CLI   SBKTYPE,0                                                        
         BE    DR40T                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
DR40A    CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SBKTYPE,SPBKTYPN                                                 
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     DR40A                                                            
*                                                                               
         MVC   SSTBOOK,SPBKTYPA    BOOK TYPE                                    
         OI    SSTBOOKH+6,X'80'                                                 
         DROP  RF                                                               
*                                                                               
DR40T    CLI   SCBKTYPE,0                                                       
         BE    DR40Z                                                            
         MVC   SSTCBTY(1),SCBKTYPE                                              
         OI    SSTCBTYH+6,X'80'                                                 
*                                                                               
DR40Z    MVC   SSTEIX,SEIXSTA      EIX? Y/N                                     
         OI    SSTEIXH+6,X'80'                                                  
         MVC   SSTEFF,SPACES                                                    
         OI    SSTEFFH+6,X'80'                                                  
*        BAS   RE,DISPPST          DISPLAY PST                                  
         BRAS  RE,DISPPST          DISPLAY PST                                  
*         XC    SSTGRPC,SSTGRPC     CLEAR GROUP CODE                            
*         OI    SSTGRPCH+6,X'80'    TRANSMIT                                    
         XC    SSTORD,SSTORD       CLEAR ORDER DEADLINE                         
         OI    SSTORDH+6,X'80'     TRANSMIT                                     
*                                                                               
         TM    SVAGYFL2,AGYFLAG2_UID  UID FLAG SET?                             
         BZ    DR41                   NO - DO NOT VALIDATE FIELD                
         CLC   STAKLEN,=AL2(STANCLNQ)                                           
         BL    *+14                                                             
         MVC   SSTUNIQ,STUNIQID                                                 
         OI    SSTUNIQH+6,X'80'                                                 
*                                                                               
DR41     DS    0H                                                               
         MVC   SSTNTYP,SNETTYPE      NETWORK TYPE                               
         OI    SSTNTYPH+6,X'80'      TRANSMIT                                   
         CLI   CABLE,C'Y'                                                       
         BNE   DR100                                                            
*                                                                               
         CLC   STAKLEN,=Y(STANCLNQ)  RECORD LENGTH                              
         BL    DR42                                                             
*        MVC   SSTGRPC,SGRPCD      GROUP CODE                                   
         MVC   SSTORD,SORDDLN      ORDER DEADLINE                               
*                                                                               
DR42     DS    0H                                                               
*DR42     MVC   SSTCSN,SSYSNAME     SET CABLE SYSTEM NAME                       
*                                                                               
         XC    DMCB,DMCB            EFFECTIVE DATE (IF IT'S                     
         GOTO1 GETFACT,DMCB                                                     
         L     R1,0(R1)             NOT AFTER TODAY) AND                        
         USING FACTSD,R1                                                        
         LA    RE,FADATEB                                                       
         DROP  R1                                                               
         CLC   SEFFDATE,0(RE)                                                   
         BL    DR45                                                             
         GOTO1 DATCON,DMCB,(X'83',SEFFDATE),(10,SSTEFF)                         
*                                                                               
DR45     DS    0H                  DISPLAY NETWORKS                             
*                                                                               
*         MVC   SVSCBL24,SCBL24                                                 
*         MVC   SVSCBLSQ,SCBLSEQ                                                
*                                                                               
*         CLC   STAKCLT,=C'000'                                                 
*         BE    DR60                                                            
*         XC    SSTCNL1,SSTCNL1                                                 
*         XC    SSTCNL2,SSTCNL2                                                 
*         XC    SSTCNL3,SSTCNL3                                                 
*         MVC   SSTCNL1(32),=C'CHECK AGENCY LEVEL MASTER RECORD'                
*         OI    SSTCNL1H+6,X'80'                                                
*         B     DR100                                                           
*                                                                               
*DR60     OC    SCBL24(67),SCBL24       NETWORK LIST                            
*         BZ    DR100                                                           
*         MVC   MYSYSNET(16),SSYSNETS                                           
*         GOTO1 =A(DISPNET2),DMCB,(RC),RR=RELO                                  
*                                                                               
DR100    DS    0H                                                               
*DR100    OI    SSTCNL1H+4,X'20'    VALIDATE NETWORK LIST LINES                 
*         OI    SSTCNL2H+4,X'20'                                                
*         OI    SSTCNL3H+4,X'20'                                                
*         OI    SSTCNL1H+6,X'80'                                                
*         OI    SSTCNL2H+6,X'80'                                                
*         OI    SSTCNL3H+6,X'80'                                                
*                                                                               
         CLI   STNETINV,C' '                                                    
         BNH   *+14                                                             
         MVC   SSTNET,STNETINV      NET INVOICES                                
         OI    SSTNETH+6,X'80'                                                  
*                                                                               
         TM    SSYSDACT,X'FF'                                                   
         BNO   DR110                                                            
         MVI   SSTDEAC,C'Y'                                                     
*                                                                               
DR110    CLI   THISLSEL,C'C'                                                    
         BE    DRX                                                              
         OC    PASSKEY,PASSKEY     IF WE ARE DISPLAYING PASSIVE                 
         BZ    DRX                 PTR FROM A LIST                              
         MVC   KEY(STAKEYLN),PASSKEY                                            
         GOTO1 HIGH                GET OLD RECORD BACK                          
*                                                                               
DRX      J     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*             ROUTINE TO FORMAT REPS                                            
*                                                                               
FMTREP   NTR1                                                                   
         MVC   SVKEY,KEY                                                        
*                                                                               
         LLC   RF,0(R2)                                                         
         SHI   RF,8                                                             
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SHI   RF,8                                                             
         STC   RF,BYTE             REMEMBER LENGTH OF FIELD                     
         BCTR  RF,0                                                             
         EXMVC RF,8(R2),SPACES                                                  
                                                                                
         OC    0(3,R3),0(R3)       IN CASE SOME IDIOT                           
         BZ    FMTRX               LEFT BINARY ZEROES !                         
         CLC   0(3,R3),ZEROES                                                   
         BE    FMTRX                                                            
*                                                                               
FMTR10   MVC   KEY(STAKEYLN),ZEROES                                             
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(3),0(R3)           R3 POINTS TO REP CODE                   
         MVC   KEY+5(2),AGENCY                                                  
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 HIGH                                                             
                                                                                
         LA    R1,(RNAME-REPRECD)(R6)                                           
         LA    RE,L'RNAME                                                       
         CLC   KEYSAVE(7),0(R6)    CAREFUL HERE !                               
         BE    *+10                                                             
         LAY   R1,=C'**REP NOT ON FILE**'                                       
         LA    RE,19                                                            
                                                                                
         LLC   RF,BYTE             RF = L(DATA PORTION OF TWA FIELD)            
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                                                             
         EXMVC RF,8(R2),0(R1)                                                   
*                                                                               
FMTRX    MVC   AIO,AIO1       RESET AIO                                         
         MVC   KEY,SVKEY                                                        
         OI    6(R2),X'80'                                                      
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
*             ROUTINE TO VALIDATE ALPHA MKT                                     
*                                                                               
* AT ENTRY, AIO=A(STATION RECORD BEING BUILT)                                   
*           R2-->ALPHA-MKT FIELD                                                
         DS    0H                                                               
VALAMKT  NTR1                                                                   
         XC    DUB,DUB                                                          
         GOTO1 ANY                 INPUT EXISTS ALREADY VERIFIED                
         MVC   DUB(L'SSTAMKT),WORK                                              
                                                                                
         DS    0H                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         LA    R1,WORK                                                          
         USING CTDMREC,R1                                                       
         XC    CTDMKEY,CTDMKEY                                                  
         MVI   CTDMKTYP,CTDMKTEQ   RECORD TYPE                                  
         MVI   CTDMKTY2,CTDMKT2E   SUB-RECORD TYPE                              
         MVC   CTDMKMED,QMED       MEDIA                                        
         MVC   CTDMKMKT,DUB        ALPHA MKT                                    
         MVC   CTDMKBKT,SBKTYPE    BOOKTYPE                                     
         CLI   CTDMKBKT,0                                                       
         BNE   *+8                                                              
         XI    CTDMKBKT,X'FF'       ADJUST IF DEFAULT                           
                                                                                
         MVI   CTDMKSRC,C'A'       TRY ARBITRON FIRST                           
         B     VAM20                                                            
VAM10    MVI   CTDMKSRC,C'N'        THEN TRY NIELSEN                            
         DROP  R1,R6                                                            
                                                                                
VAM20    DS    0H                  READ ALPHA-MKT RECORD                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO2                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIO2                                                          
         USING CTDMREC,R1                                                       
         CLC   WORK(CTDMKNUM-CTDMKEY),CTDMREC                                   
         BE    VAM30                                                            
         LA    R1,WORK                                                          
         CLI   CTDMKSRC,C'A'                                                    
         BE    VAM10                                                            
         DROP  R1                                                               
         XC    DUB(L'SSTAMKT),DUB                                               
                                                                                
VAM30    DS    0H                                                               
         OC    DUB(L'SSTAMKT),DUB     SET RETURN CODE FOR CALLER                
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        DISPLAY KEY                                                            
*                                                                               
DK       CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    DK5                                                              
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    DK5                                                              
         MVI   ERROR,NOTAUTH       NOT AUTHORIZED FOR THIS FUNCTION             
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BE    NOTAUTHD                                                         
*                                                                               
DK5      MVI   CABLE,C'N'                                                       
         BAS   RE,GETACTV          GET 'ACTIVE' RECORD                          
         USING STARECD,R6                                                       
         L     R6,AIO                                                           
*                                                                               
DK10     MVC   SSTMED,STAKMED      SET MEDIA                                    
         MVC   SSTSTA(L'STAKCALL),STAKCALL     STATION                          
         MVC   QSTANEW,SPACES                                                   
         MVC   QSTANEW(L'STAKCALL),STAKCALL                                     
         NI    MISCFLG1,X'FF'-MF1OFFCD                                          
         CLI   STAKCLT,C'*'        HAVE AN OFFICE CODE?                         
         BNE   *+12                NO                                           
         OI    MISCFLG1,MF1OFFCD                                                
         B     DK15                                                             
*                                                                               
         MVC   QCLT,STAKCLT                                                     
         OI    SSTLOCTH+1,X'08'    TURN ON HIGH INTENSITY FOR LOCK              
         OI    SSTLOCTH+6,X'80'    TRANSMIT                                     
         NI    SSTLOCKH+1,X'DF'    UNPROTECT LOCK FIELD                         
         OI    SSTLOCKH+6,X'80'    TRANSMIT                                     
*                                                                               
DK15     XC    SSTCLI,SSTCLI       CLEAR CLIENT FIELD                           
         CLC   STAKCLT,ZEROES                                                   
         BE    DK20                                                             
         NI    SSTLOCTH+1,X'F7'    TURN OFF HIGH INTENSITY FOR LOCK             
         OI    SSTLOCKH+1,X'20'    PROTECT LOCK FIELD                           
         TM    MISCFLG1,MF1OFFCD   HAVE AN OFFICE CODE?                         
         BZ    DK16                NO                                           
*                                                                               
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,STAKCLT+1                                                 
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(X'01',ACOMFACS)                     
         MVI   SSTCLI,C'*'                                                      
         MVC   SSTCLI+1(2),OFCOFC2                                              
         B     DK20                                                             
*                                                                               
DK16     MVC   SSTCLI,STAKCLT      & CLIENT                                     
*                                                                               
DK20     CLI   QSTANEW,C'0'        IF THE FIRST CHAR IS A DIGIT                 
         BL    DKX                                                              
         CLI   QSTANEW,C'9'        THEN THIS IS CABLE                           
         BH    DKX                                                              
         MVI   CABLE,C'Y'                                                       
         MVI   SSTSTA+4,C' '                                                    
***      OI    SSTMKTH+6,X'01'     FORCE GENCON TO THINK A FLD CHANGED          
*                                                                               
DKX      CLI   SSTCLI,C' '                                                      
         BNH   DKXX                                                             
         MVI   SSTCLIH+5,3         SET LEN                                      
         CLI   SSTCLI+2,C' '                                                    
         BH    *+8                                                              
         MVI   SSTCLIH+5,2         SET LEN                                      
****   FOR OFFICE CODE STUFF                                                    
         CLI   SSTCLI,C'*'         WE DOING OFFICE CODE?                        
         BNE   DKX50                - NOPE, WE'RE NOT                           
*                                                                               
         NI    SSTMKTQH+1,X'FF'-X'08'   TURN OFF HIGH INTENSITY                 
         NI    SSTTSRQH+1,X'FF'-X'08'   TURN OFF HIGH INTENSITY                 
         NI    SSTTRAQH+1,X'FF'-X'08'   TURN OFF HIGH INTENSITY                 
         NI    SSTORDTH+1,X'FF'-X'08'   TURN OFF HIGH INTENSITY                 
*                                                                               
         LA    R1,SSTMKTH                                                       
         LA    R2,SSTPREPH                                                      
         LA    R3,SSTUNIQH                                                      
DKOFF20  CR    R1,R3                                                            
         BE    DKOFFX              WE'RE DONE, WENT TO THE END                  
         CR    R1,R2               ARE WE ON PAYING REP?                        
         BE    DKOFFNXT             - YEAH, SKIP PROTECTING THE FIELD           
         TM    1(R1),X'02'         EXTENDED FIELD HEADER?                       
         BNZ   DKOFFNXT            YES, DON'T TURN ON BITS!                     
         OI    1(R1),X'20'         PROTECT THE FIELD                            
         OI    6(R1),X'80'                                                      
*                                                                               
DKOFFNXT XR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         B     DKOFF20                                                          
*                                                                               
DKOFFX   B     DKXX                                                             
****                                        MHC  10/27/03                       
*                                                                               
DKX50    MVC   MYKEY2,KEY          SAVE KEY                                     
         BAS   RE,SETSPF           SET FILENAME TO SPTFILE                      
         LA    R2,SSTCLIH          CALL VALICLT TO CHECK SECURITY               
         MVI   USEIONUM,3          DON'T CREAM AIO1                             
         GOTO1 VALICLT                                                          
         BAS   RE,SETDEF           SET FILENAME BACK TO STATION                 
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY2          RESTORE                                      
         GOTO1 READ                                                             
*                                                                               
DKXX     OI    SSTMEDH+6,X'80'                                                  
         OI    SSTSTAH+6,X'80'                                                  
         OI    SSTCLIH+6,X'80'                                                  
         BAS   RE,TSTCOMS          TEST COMSCORE                                
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
*        GET 'ACTIVE' RECORD INTO AIO                                           
*                                                                               
GETACTV  NTR1                                                                   
         XC    PASSKEY,PASSKEY                                                  
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         CLI   STAKEY,C'S'         IF THIS IS A PASSIVE PTR                     
         BE    GAX                                                              
         MVC   PASSKEY,0(R6)       SAVE ORIGINAL KEY                            
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   STAKEY(STAKEYLN),ZEROES                                          
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVC   STAKMED,QMED        MEDIA                                        
         MVC   STAKAGY,AGENCY      AGENCY                                       
*                                                                               
         CLI   PASSKEY,C'N'                                                     
         BNE   GA20                                                             
         MVC   WORK(5),PASSKEY+4   STNKMS                                       
         MVC   STAKCLT,PASSKEY+9   CLIENT EXCEPTION                             
         B     GA40                                                             
*                                                                               
GA20     CLI   PASSKEY,C'F'                                                     
         BNE   GAX                                                              
         MVC   QCLT,ZEROES                                                      
         MVC   BCLT,PASSKEY+13                                                  
         OC    BCLT,BCLT              CLIENT                                    
         BZ    GA30                                                             
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
*                                                                               
GA30     MVC   STAKCLT,QCLT                                                     
         MVC   WORK(5),PASSKEY+8      MARKET/STATION                            
*                                                                               
GA40     GOTO1 MSUNPK,DMCB,WORK,WORK+10,WORK+15                                 
         MVC   STAKCALL,WORK+15    STATION                                      
         CLI   STAKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         GOTO1 READ                                                             
*                                                                               
GAX      J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        RE-READ PASSIVE RECORD AFTER CHANGE/DISPLAY FROM LIST                  
*                                                                               
XRP      TM    MISCFLG1,MF1LOCK     DID LOCK FIELD CHANGE?                      
         BZ    XRP25                NO                                          
         MVC   MYKEY,KEY            SAVE OLD KEY                                
         L     R6,AIO               R6 = A(MASTER RECORD)                       
         USING STARECD,R6           MASTER RECORD DSECT                         
         MVI   OLDLOCK,C'N'         OLD LOCK = N                                
         TM    SFLAG1,SLOCK         IS LOCK ON?                                 
         BZ    *+8                  NO                                          
         MVI   OLDLOCK,C'Y'         YES - OLD LOCK = Y                          
         DROP  R6                                                               
*                                                                               
         BAS   RE,CLTLOCK           UPDATE THE CLT SPECIFIC LOCK FIELDS         
         CLI   SVAPROF+7,C'C'       IS THIS A CANADIAN AGENCY?                  
         BNE   XRP20                NO                                          
         CLI   QMED,C'T'            MEDIA = TV?                                 
         BNE   XRP20                NO                                          
         MVC   KEY(STAKEYLN),MYKEY  RESTORE THE ORIGINAL KEY                    
         MVI   KEY+1,C'N'           PROCESS MEDIA N                             
         MVI   KEY+6,C'N'           PROCESS MEDIA N                             
         BAS   RE,CLTLOCK           UPDATE THE CLT SPECIFIC LOCK FIELDS         
         MVC   KEY(STAKEYLN),MYKEY  RESTORE THE ORIGINAL KEY                    
         MVI   KEY+1,C'C'           PROCESS MEDIA C                             
         MVI   KEY+6,C'C'           PROCESS MEDIA C                             
         BAS   RE,CLTLOCK           UPDATE THE CLT SPECIFIC LOCK FIELDS         
*                                                                               
XRP20    MVC   KEY(STAKEYLN),MYKEY  RESTORE THE ORIGINAL KEY                    
         GOTO1 READ                 READ THE RECORD BACK                        
*                                                                               
XRP25    OC    PASSKEY,PASSKEY                                                  
         BZ    XRPX                                                             
         MVC   KEY(STAKEYLN),PASSKEY                                            
         GOTO1 HIGH                                                             
         XC    PASSKEY,PASSKEY                                                  
*                                                                               
XRPX     J     EXIT                                                             
         EJECT                                                                  
CLTLOCK  NTR1                                                                   
         USING STARECD,R6           MASTER RECORD DSECT                         
         XC    KEY+9(3),KEY+9       CLEAR CLIENT FIELD TO GET CLIENTS           
         GOTO1 HIGH                 READ HIGH TO GET CURRENT REC                
         B     CL10                 GO TEST KEY                                 
*                                                                               
CL05     GOTO1 SEQ                  READ SEQ                                    
*                                                                               
CL10     CLC   KEY(9),KEYSAVE       MATCH UNTIL AGENCY?                         
         BNE   EXIT                 NO - RETURN TO CALLER                       
         CLC   STAKCLT,ZEROES       CLIENT SPECIFIC RECORD?                     
         BE    CL05                 NO - READ SEQ                               
         NI    SFLAG1,X'FF'-SLOCK   TURN OFF VENDOR LOCK FLAG                   
         CLI   OLDLOCK,C'Y'         MASTER LEVEL LOCK FLAG ON?                  
         BNE   *+8                  NO                                          
         OI    SFLAG1,SLOCK         YES - TURN IT ON                            
         GOTO1 WRITE                WRITE THE RECORD BACK                       
         B     CL05                 GO READ SEQ                                 
         DROP  R6                   DROP R6                                     
*                                                                               
*        DELETED RECORD                                                         
*                                                                               
DELR     L     R6,AIO1                                                          
         USING STARECD,R6                                                       
         MVC   QMKT,SMKT                                                        
         GOTO1 CNRECS,DMCB,C'D'                                                 
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         BAS   RE,BLDPASS          PROCESS PASSIVE PTRS                         
         NI    DMINBTS,X'F7'                                                    
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        RESTORED RECORD                                                        
*                                                                               
RESTR    L     R6,AIO1                                                          
         USING STARECD,R6                                                       
         MVC   QMKT,SMKT                                                        
         BAS   RE,DRL                                                           
         GOTO1 CNRECS,DMCB,C'R'                                                 
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         BAS   RE,BLDPASS          PROCESS PASSIVE PTRS                         
         NI    DMINBTS,X'F7'                                                    
         GOTO1 =A(REQREC),DMCB,RR=Y                                             
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        BUILD PASSIVE KEY                                                      
*                                                                               
BLDPASS  NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         BRAS  RE,BLDXPASS         BUILD 'X' RECORDS FOR CANADA                 
         BRAS  RE,BLDYREC          BUILD 'Y' RECORDS FOR CABLE                  
         BRAS  RE,BLDPASSF         FORMAT PASSIVE PTR                           
         USING STARECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   STAKEY(STAKEYLN),ZEROES                                          
         MVI   STNKTYPE,C'N'       RECORD TYPE                                  
         MVC   STNKAGY,AGENCY      AGENCY                                       
         MVC   STNKMED,QMED        MEDIA                                        
         MVC   WORK(8),QSTANEW                                                  
         CLI   WORK+4,C' '                                                      
         BH    *+10                                                             
         MVC   WORK+4(1),QMED                                                   
         GOTO1 MSPACK,DMCB,QMKT,WORK,STNKMS                                     
         MVC   STNKCLT,QCLT        CLIENT EXCEPTION                             
         MVC   STAKLEN,=Y(STNKLNQ) RECORD LENGTH                                
         BRAS  RE,PROCPASS                                                      
*                                                                               
         BRAS  RE,DELMKT           IF MKT CHGED-DELETE OLD PASS PTRS            
****                                                                            
         BRAS  RE,BLDPASSK         K PASSIVE POINTER                            
         OI    MISCFLG1,MF1ONK     WE'RE ON K NOW                               
         OC    MYMKT,MYMKT         ARE WE CHANGING THE MARKET?                  
         BZ    BLDPASS5             - NOPE                                      
**  CHANGING THE MARKET TO THE OLD MARKET FOR DELETE                            
         GOTO1 MSPACK,DMCB,MYMKT,QSTANEW,WORK+16                                
         MVC   MYKEY+4(2),WORK+16                                               
         BRAS  RE,DELMKT                                                        
BLDPASS5 NI    MISCFLG1,X'FF'-MF1ONK   WE'RE DONE WITH K                        
****                                                                            
         BRAS  RE,CANPASS          PROCESS CANADIAN PASSIVE POINTER             
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
DDDCLIST DS    0C                                                               
         DCDDL SP#APHMK,L'SSTAMKD                                               
DDDCLSTX DC    X'00'                                                            
         PRINT NOGEN                                                            
                                                                                
         DS    0F                                                               
ZEROES   DC    20C'0'                                                           
ERRDISP  DS    H                                                                
MYREP    DS    CL3                                                              
ERRNUM   DS    XL2                                                              
MYTEXT   DS    XL16                                                             
*MYSYSNET DS    CL16                                                            
*SVNAME   DS    CL20                                                            
*                                                                               
*        MISC & ERROR ROUTINES                                                  
*                                                                               
SAVEDEF  DS    0H                  SAVE DEFINITION BEFORE SETDEF                
         MVC   MYSYSDIR,SYSDIR                                                  
         MVC   MYSYSFIL,SYSFIL                                                  
         MVC   MYUSEIO,USEIO                                                    
         MVC   MYACELOP,ACTELOPT                                                
         MVC   MYLKEY,LKEY                                                      
         BR    RE                                                               
*                                                                               
SETDEF   MVC   SYSDIR,=C'STATION ' SET TO READ STATION FILE                     
         MVC   SYSFIL,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVC   LKEY,=H'15'         SET LENGTH OF STATION KEY                    
         BR    RE                                                               
*                                                                               
SETSPF   MVC   SYSDIR,=C'SPTDIR  ' SET TO READ SPOT FILE                        
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVI   USEIO,C'N'                                                       
         MVC   LKEY,=H'13'         SET LENGTH OF KEY                            
         BR    RE                                                               
*                                                                               
RSTRDEF  DS    0H                  RESTORE DEFINITION AFTER SETDEF              
         MVC   SYSDIR,MYSYSDIR                                                  
         MVC   SYSFIL,MYSYSFIL                                                  
         MVC   USEIO,MYUSEIO                                                    
         MVC   ACTELOPT,MYACELOP                                                
         MVC   LKEY,MYLKEY                                                      
         BR    RE                                                               
*                                                                               
XEXISTS  MVC   ERRNUM,=AL2(1354)   X POINTER EXISTS - CONTACT DDS               
         B     MSGERR                                                           
*                                                                               
NOCLTDEF MVC   ERRNUM,=AL2(1355)   ADD DEFUALT STA BEFORE CLT EXCEPTION         
         B     MSGERR                                                           
*                                                                               
CANTCHG  MVC   ERRNUM,=AL2(1356)   CANT CHANGE TO OR FROM MKT 0000              
         B     MSGERR                                                           
*                                                                               
VKOFFNA  MVC   ERRNUM,=AL2(1359)   ** ERROR ** OFFICE CODE NOT ALLOWED!         
         B     MSGERR                                                           
*                                                                               
VKOFFINV MVC   ERRNUM,=AL2(1360)   ** ERROR ** INVALID OFFICE CODE!             
         B     MSGERR                                                           
*                                                                               
BUYEXIST MVC   ERRNUM,=AL2(1357)   BUY EXISTS CAN'T CHANGE MARKET               
         B     MSGERR                                                           
*                                                                               
ERRMIDAS MVC   ERRNUM,=AL2(1379)   MIDAS VALUE SET - CANNOT CHANGE              
         B     MSGERR                                                           
*                                                                               
NODSTA   MVC   ERRNUM,=AL2(1398)   DSTA RECORD MUST EXIST                       
         B     MSGERR                                                           
*                                                                               
         MVI   MYTEXT,15           LENGTH                                       
         LA    RE,MYTEXT+1         INSERT TEXT HERE                             
         MVC   0(4,RE),=C'CLT='    CLT=                                         
         MVC   4(3,RE),WORK        CLIENT CODE                                  
         CLI   6(RE),C' '          HAVE 3RD BYTE OF CLIENT CODE?                
         BH    *+10                YES                                          
         BCTR  RE,0                NO - DECREMENT POINTER                       
         MVI   MYTEXT,14           LENGTH NOW 14                                
         MVI   7(RE),C','          COMMA TO SEPARATE CLT & EST                  
         MVC   8(4,RE),=C'EST='    EST=                                         
         MVC   12(3,RE),WORK+3     ESTIMATE                                     
         B     MSGERR                                                           
*                                                                               
MSGERR   OI    GENSTAT2,USGETTXT   USE GETTXT INSTEAD OF GETMSG                 
         LA    RF,GETTXTCB         GETTXT CONTROL BLOCK                         
         USING GETTXTD,RF          GETTXT DSECT                                 
         XC    GTBLOCK,GTBLOCK     CLEAR THE CONTROL BLOCK                      
         MVI   GTMAXL,L'CONHEAD    MAX LEN OF OUTPUT AREA                       
         MVC   GTMSGNO,ERRNUM      2 BYTE ERROR NUMBER                          
         MVI   GTMTYP,GTMERR       ERROR TYPE E                                 
         MVI   GTMSYS,2            SYSTEM 2                                     
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                NO                                           
         MVC   GTLTXT,MYTEXT       YES - PUT LENGTH IN                          
         LA    R1,MYTEXT+1         A(TEXT)                                      
         STCM  R1,7,GTATXT         A(TEXT) NOW IN GTATXT                        
         GOTO1 ERREX                                                            
         DROP  RF                                                               
*                                                                               
RECNFERR MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
INVLDMKT MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
*                                                                               
TRAPERR  OC    ERRDISP,ERRDISP     DO I NEED TO OVERRIDE CURSOR POS.            
         BZ    TRAPEND                                                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         MVC   TIOBCURI,ERRDISP+1  DISPLACMENT INTO FIELD                       
*                                                                               
TRAPEND  DS    0H                                                               
         MVI   ERROPT,0            NEVER TO RETURN                              
         GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
GSTTAB   DC    C'S'               STANDARD                                      
         DC    C'U'                                                             
         DC    C'X'                                                             
         DC    C'Z'               ZERO                                          
         DC    X'FF'              END OF TABLE                                  
*                                                                               
EDTCOUN  DC    CL3'CAN'            CAN STA/CAN $                                
         DC    CL3'USA'            USA STA/USA $                                
         DC    CL3'VSA'            USA STA/CAN $                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
STNKLNQ  EQU   20                  PASSIVE RECORD LENGTH                        
STXKLNQ  EQU   20                  PASSIVE RECORD LENGTH                        
*                                                                               
*                                  POSTING TYPE TABLE FOR NETPAK                
         PRINT GEN                                                              
*DDCLIST DS    0C                                                               
*        DCDDL SP#APHMK,L'SSTAMKD                                               
*DDCLSTX DC    X'00'                                                            
         PRINT NOGEN                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
VAMT     NTR1  BASE=*,LABEL=*                                                   
         SR    R5,R5                                                            
         ICM   R5,1,5(R2)                                                       
         BZ    VAMT10                                                           
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R5)                                      
         CLI   0(R1),0                                                          
         BNE   INVERR                                                           
         L     R5,4(R1)                                                         
         C     R5,=F'32767'                                                     
         BH    INVERR                                                           
         LTR   R5,R5                                                            
         BM    INVERR                                                           
*                                                                               
VAMT10   XIT1  REGS=(R5)                                                        
         EJECT                                                                  
*                                                                               
*        VALIDATE PST CODES                                                     
*                                                                               
VALPST   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,BLOCK                                                         
         USING PSTBLKD,R4                                                       
         USING STARECD,R6                                                       
         XC    0(200,R4),0(R4)     CLEAR INTERFACE BLOCK                        
         XC    200(200,R4),200(R4)                                              
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,SSTPSTH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   ERRDISP,PSTERDSP                                                 
         CLI   PSTERR,0                                                         
         BNE   INVERR                                                           
         MVC   SPST,PSTOUT         OUTPUT                                       
         BRAS  RE,DISPPST          DISPLAY PST                                  
*                                                                               
VPX      J     EXIT                                                             
         DROP  R4,R6                                                            
*                                                                               
*        DEACTIVATE 'Y' RECORD FOR CABLE                                        
*                                                                               
DEACBL   NTR1  BASE=*,LABEL=*                                                   
         CLI   CABLE,C'Y'          FOR CABLE ONLY                               
         BNE   DAX                                                              
         MVC   SVKEY,KEY                                                        
         USING CBLRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   CBLKEY,ZEROES                                                    
         MVI   CBLKTYPE,C'Y'       RECORD TYPE                                  
         MVC   CBLKMED,QMED        MEDIA                                        
         MVC   CBLKCALL,QSTANEW    CALL LETTERS                                 
         MVC   CBLKAGY,AGENCY      AGENCY                                       
         MVC   CBLKCLT,QCLT        CLIENT                                       
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO2                                                          
         MVC   0(L'CBLKEY,R6),KEY                                               
         MVC   MYKEY,KEY           KEY THAT WAS BUILT                           
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                IS THERE A RECORD                            
         NI    DMINBTS,X'F7'                                                    
         CLC   0(L'CBLKEY,R6),MYKEY                                             
         BNE   DA20                IF NOT FOUND - DONE                          
         NI    CBCNTRL,X'7F'                                                    
         CLI   REACTIVE,C'Y'                                                    
         BNE   DA10                                                             
         MVI   CSYSDACT,C' '                                                    
         B     *+8                                                              
                                                                                
DA10     MVI   CSYSDACT,X'FF'                                                   
         GOTO1 WRITE               WRITE IT BACK                                
                                                                                
DA20     MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
*                                                                               
DAX      J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DETERMINE CANADIAN 'X' PASSIVE POINTER STATION SEQ# WHEN CURRENTLY  *         
* HAVE A VALUE LESS THAN X'0100' (DEMOVER RECS THINK SPILL IF FIRST   *         
* BYTE OF THE STATION IS '00' HENCE THE X'0100' MINIMUM)              *         
* ENTRY - R6=A(KEY) / MYKEY CONTAINS X-REC KEY JUST BUILT             *         
* EXIT  - HALF=LOWEST UNUSED SEQ#                                     *         
***********************************************************************         
XPASSEQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,X'100'                                                        
         STH   R1,HALF                                                          
*                                                                               
         USING STARECD,R6                                                       
XP10     XC    KEY,KEY                                                          
         MVC   KEY(3),MYKEY        PASSIVE PTR THAT WAS BUILT                   
         GOTO1 HIGH                                                             
         B     XP20                                                             
XP15     GOTO1 SEQ                                                              
XP20     CLC   KEY(3),MYKEY        REC/AGY MATCH?                               
         BNE   XPX                 - NO, DONE                                   
         CLC   STXKNUM,HALF        SEQ# MATCH? (PREVENT DUPLICATES)             
         BNE   XP15                - NO, CONTINUE SCAN ALL X POINTERS           
         LH    R1,HALF             BUMP SEQ# IN HALF                            
         AHI   R1,1                                                             
         STH   R1,HALF                                                          
         B     XP10                START AGAIN CHECKING THIS SEQ#               
*                                                                               
XPX      J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        BUILD PASSIVE KEY - TYPE F                                             
*                                                                               
BLDPASSF NTR1  BASE=*,LABEL=*                                                   
         CLI   QMED,C'R'           FOR RADIO ONLY                               
         BNE   BFX                                                              
         OC    QFORM,QFORM         FORMAT                                       
         BZ    BFX                                                              
         MVC   SVKEY,KEY                                                        
         USING STARECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   STFKTYPE,C'F'       RECORD TYPE                                  
         MVC   STFKAGY,AGENCY      AGENCY                                       
         MVC   STFKMED,QMED        MEDIA                                        
         MVC   STFKFORM,QFORM      FORMAT                                       
         GOTO1 MSPACK,DMCB,QMKT,QSTANEW,STFKMS                                  
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   *+10                                                             
         MVC   STFKCLT,BCLT                                                     
*                                                                               
         MVC   STAKLEN,=Y(STNKLNQ)  RECORD LENGTH                               
         BRAS  RE,PROCPASS                                                      
*                                                                               
         BRAS  RE,DELFMT           IF FMT CHGED-DELETE OLD PASS PTRS            
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
*                                                                               
BFX      J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        BUILD 'K' PASSIVE RECORDS                                              
*                                                                               
BLDPASSK NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,KEY                                                        
         USING STARECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   STKKTYPE,C'K'       RECORD TYPE                                  
         MVC   STKKAGY,AGENCY      AGENCY CODE                                  
         MVC   STKKMED,QMED                                                     
         MVC   STKKMKT,BMKT                                                     
         MVC   STKKSTA,QSTANEW                                                  
         CLC   =C'000',QCLT                                                     
         BE    *+10                                                             
         MVC   STKKCLT,QCLT                                                     
*                                                                               
         MVC   STKKLEN,=Y(STKKLNQ)   RECORD LENGTH                              
         BRAS  RE,PROCPASS                                                      
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*        BUILD 'Y' RECORD FOR CABLE                                             
*                                                                               
BLDYREC  NTR1  BASE=*,LABEL=*                                                   
         CLI   CABLE,C'Y'          FOR CABLE ONLY                               
         BNE   BYX                                                              
         CLI   SSTCLIH+5,0         CLIENT FIELD                                 
         BNE   BY5                                                              
         CLC   QSTANEW(4),=C'7000' FOR MANUALLY ADDED HEADENDS                  
         BL    BYX                                                              
         CLC   QSTANEW(4),=C'7500'                                              
         BH    BYX                                                              
*                                                                               
BY5      MVC   SVKEY,KEY                                                        
         USING CBLRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   CBLKEY,ZEROES                                                    
         MVI   CBLKTYPE,C'Y'       RECORD TYPE                                  
         MVC   CBLKMED,QMED        MEDIA                                        
         MVC   CBLKCALL,QSTANEW    CALL LETTERS                                 
         MVC   CBLKAGY,AGENCY      AGENCY                                       
         MVC   CBLKCLT,QCLT        CLIENT                                       
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO2                                                          
         MVC   0(L'CBLKEY,R6),KEY                                               
         MVC   MYKEY,KEY           KEY THAT WAS BUILT                           
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                IS THERE A RECORD                            
         NI    DMINBTS,X'F7'                                                    
         CLC   0(L'CBLKEY,R6),MYKEY                                             
         BNE   BY10                IF NOT FOUND - ADD IT                        
         NI    CBCNTRL,X'7F'                                                    
         MVC   CBLKLEN,=Y(CBLLNQ)    RECORD LENGTH                              
         XC    CSYSICN(19),CSYSICN                                              
         GOTO1 WRITE               WRITE IT BACK                                
         B     BY20                                                             
*                                                                               
BY10     XC    0(CBLLNQ,R6),0(R6)                                               
         MVC   0(L'CBLKEY,R6),MYKEY                                             
         MVC   CBLKLEN,=Y(CBLLNQ)    RECORD LENGTH                              
         XC    CSYSICN(19),CSYSICN                                              
         GOTO1 ADD                 ADD RECORD                                   
*                                                                               
BY20     MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
*                                                                               
BYX      J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        BUILD 'X' RECORDS FOR CANADA                                           
*                                                                               
BLDXPASS NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'XD',AGENCY                                                    
         BE    BPX                                                              
         CLI   SVAPROF+7,C'C'      IS THIS A CANADIAN AGENCY                    
         BNE   BPX                                                              
         CLI   QMED,C'T'           MEDIA = TV                                   
         BE    *+12                                                             
         CLI   QMED,C'N'           OR NETWORK                                   
         BNE   BPX                                                              
         CLI   ACTNUM,ACTADD       IF ADD                                       
         BNE   BPX                 <=== NEED TO FIX FOR REACITVATE              
         CLI   SSTCLIH+5,0         TEST CLIENT EXCEPTION RECORD                 
         BNE   BPX                 YES - NO NEW 'X' RECORD                      
*                                                                               
BP10     XC    BLOCK(32),BLOCK                                                  
         LA    R4,BLOCK                                                         
         USING STAPACKD,R4                                                      
         MVI   STAPACT,C'V'                                                     
         MVC   STAPAGY,AGENCY                                                   
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPVRSN,C'N'       AND THIS IS THE NEW VERSION                  
         BNE   BPX                                                              
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO2            SET AIO TO AIO2                              
         XC    FULL,FULL           KEEP LOW IN FULL & HIGH IN FULL + 2          
         USING STARECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   STXKTYPE,C'X'       RECORD TYPE                                  
         MVC   STXKAGY,AGENCY      AGENCY                                       
         MVC   STXKSTA,QSTANEW     STATION                                      
         MVI   STXKSTA+4,C'T'      ALWAYS SET TO MEDIA T                        
*                                                                               
         L     R6,AIO2                                                          
         MVC   0(STXKLNQ,R6),KEY                                                
         MVC   MYKEY,KEY           KEY THAT WAS BUILT                           
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                IS THERE A RECORD                            
         NI    DMINBTS,X'F7'                                                    
         CLC   0(8,R6),MYKEY                                                    
         BE    XEXISTS             YES, ERROR                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   STXKTYPE,C'X'       RECORD TYPE                                  
         MVC   STXKAGY,AGENCY      AGENCY                                       
         MVC   MYKEY,KEY           KEY THAT WAS BUILT                           
         SR    R2,R2                                                            
         GOTO1 HIGH                                                             
         B     BP40                                                             
*                                                                               
BP30     GOTO1 SEQ                                                              
*                                                                               
BP40     L     R6,AIO2                                                          
         CLC   0(3,R6),MYKEY       CHECK SAME REC/AGY                           
         BNE   BP60                                                             
         ICM   R2,3,STXKNUM        SEQUENCE NUMBER                              
         CLC   STXKSTA(4),QSTANEW      STATION                                  
         BH    BP50                CANNOT BE EQUAL - ALREADY CHECKED            
         STH   R2,FULL                                                          
         B     BP30                                                             
*                                                                               
BP50     STH   R2,FULL+2                                                        
*                                                                               
BP60     SR    R2,R2                                                            
         ICM   R2,3,FULL                                                        
         SR    R3,R3                                                            
         ICM   R3,3,FULL+2                                                      
         LTR   R3,R3               IF THERE ARE NO RECORDS - OR THIS            
         BNZ   *+8                    IS THE LAST RECORD                        
         L     R3,=F'61439'           SET R3 TO  MAX                            
         AR    R2,R3                                                            
         SRL   R2,1                DIV BY 2                                     
         STH   R2,HALF             SEQUENCE NUMBER                              
*                                                                               
         OC    CANSTA,CANSTA       FORCE CANADIAN STATION?                      
         BZ    BP65                                                             
         CLC   CANSTA,=X'FFFF'     FORCE NEXT/SPECIFIC STATION                  
         BNE   BP63                   SPECIFIC                                  
         LH    RF,FULL                                                          
         LA    RF,1(RF)                                                         
         STH   RF,HALF                                                          
         B     BP65                                                             
BP63     MVC   HALF,CANSTA                                                      
         B     BP65                                                             
*                                                                               
BP65     GOTO1 =A(NOSEQNUM),DMCB,RR=Y                                           
*                                                                               
BP69     XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   KEY(STXKLNQ),MYKEY                                               
         MVC   STXKSTA,QSTANEW     STATION                                      
         MVI   STXKSTA+4,C'T'      ALWAYS SET TO MEDIA T                        
         CLC   HALF,=X'00FF'       SEQ NUM CANT HAVE X'00' IN 1ST BYTE          
         BH    *+12                                                             
         BRAS  RE,XPASSEQ          GET FIRST AVAILIBLE SEQ# >= X'0100'          
         B     BP69                AND STORE IT IN HALF                         
BP69B    MVC   STXKNUM,HALF        SEQUENCE NUMBER                              
         MVC   STXKLEN,=Y(STXKLNQ) RECORD LENGTH                                
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         MVC   WORK+2(2),STXKNUM   SEQ NUMBER                                   
         MVI   WORK+4,X'02'        MEDIA T IS ALWAYS X'02'                      
         GOTO1 MSUNPK,DMCB,WORK,WORK+10,WORK+15                                 
         OC    WORK+15(4),WORK+15  RETURNED A STATION?                          
         BZ    *+6                 NO                                           
         DC    H'0'                YES - DUP STATION - STAPACK BUG              
*                                                                               
BP70     BRAS  RE,PROCPASS                                                      
         XC    BLOCK(32),BLOCK                                                  
         LA    R4,BLOCK                                                         
         USING STAPACKD,R4                                                      
         MVI   STAPACT,C'V'                                                     
         MVC   STAPAGY,AGENCY                                                   
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPVRSN,C'N'       AND THIS IS THE NEW VERSION                  
         BNE   BP80                                                             
         MVI   STAPACT,C'A'        ADD THE STATION TO  TABLE                    
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPMED,QMED                                                     
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPQMKT,ZEROES                                                  
         MVC   STAPQSTA,QSTANEW                                                 
         MVC   STAPSEQ,STXKNUM     SET SEQUENCE NUMBER                          
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0           TABLE MUST BE FULL                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
BP80     MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
BPX      J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        DO ACTUAL PROCESS OF KEY                                               
*        R6    RECORD                                                           
*                                                                               
         USING STARECD,R6                                                       
PROCPASS NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         L     R6,AIO2                                                          
         MVC   0(STNKLNQ,R6),KEY                                                
         MVC   MYKEY,KEY           PASSIVE PTR THAT WAS BUILT                   
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                IS THERE A RECORD                            
         NI    DMINBTS,X'F7'                                                    
         CLI   MODE,XRECDEL                                                     
         BE    PP10                                                             
         CLI   MODE,XRECREST       RESTORED RECORD                              
         BE    PP20                                                             
         CLC   0(STAKEYLN,R6),MYKEY   CHANGING A RECORD                         
         BNE   PP40                IF NOT FOUND - ADD IT                        
         B     PP20                ELSE CHECK IF IT'S MARKED DELETED            
*                                  DELETING                                     
PP10     CLC   0(STAKEYLN,R6),MYKEY  IF THE RECORD IS THERE                     
         BNE   PPX                                                              
         TM    SCNTL,X'80'         AND ALREADY MARKED DELETED                   
         BO    PPX                 CONTINUE                                     
         OI    SCNTL,X'80'         ELSE - MARK IT DELETED                       
         B     PP30                                                             
*                                  RESTORING                                    
PP20     CLC   KEY(STAKEYLN),MYKEY   IF THE RECORD IS THERE                     
         BNE   PP40                                                             
         TM    SCNTL,X'80'         AND NOT MARKED DELETED                       
         BNO   PPX                 CONTINUE                                     
         NI    SCNTL,X'7F'         ELSE MARK IT UNDELETED                       
*                                                                               
PP30     GOTO1 WRITE               WRITE IT BACK                                
         B     PPX                                                              
*                                                                               
PP40     L     R6,AIO2                                                          
         XC    0(250,R6),0(R6)                                                  
         MVC   0(STNKLNQ,R6),MYKEY                                              
         GOTO1 ADD                 ADD PASSIVE RECORD                           
*                                                                               
PPX      J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        DELETE OLD MKT PASSIVE PTRS                                            
*                                                                               
DELMKT   NTR1  BASE=*,LABEL=*                                                   
         OC    MYMKT,MYMKT         MKT CHANGED                                  
         BZ    DMX                                                              
         MVC   MYKEY2,MYKEY                                                     
         MVC   MYKEY3,KEY                                                       
         USING STARECD,R6                                                       
         LA    R6,KEY              SET KEY FOR OLD MKT                          
         XC    KEY,KEY                                                          
         MVC   KEY,MYKEY                                                        
         GOTO1 MSPACK,DMCB,MYMKT,QSTANEW,WORK                                   
         MVC   STKKMKT,WORK        2 BYTES FROM WORK                            
         TM    MISCFLG1,MF1ONK     WE DOING K PASSIVES?                         
         BNZ   *+10                                                             
         MVC   STNKMS,WORK         5 BYTES FROM WORK                            
         BRAS  RE,CHKKEY                                                        
*                                                                               
DMX      J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        DELETE OLD FORMAT PASSIVE PTRS                                         
*                                                                               
DELFMT   NTR1  BASE=*,LABEL=*                                                   
         OC    MYFORM,MYFORM       IF FORM AND MARKET DID NOT                   
         BNZ   DF10                                                             
         OC    MYMKT,MYMKT         CHANGE - EXIT                                
         BZ    DFX                                                              
*                                                                               
DF10     MVC   MYKEY2,MYKEY                                                     
         MVC   MYKEY3,KEY                                                       
         USING STARECD,R6                                                       
         LA    R6,KEY              SET KEY FOR OLD FORMAT                       
         XC    KEY,KEY                                                          
         MVC   KEY,MYKEY                                                        
         OC    MYFORM,MYFORM       IF FORM CHANGED                              
         BZ    *+10                                                             
         MVC   STFKFORM,MYFORM     SET NEW FORM                                 
         OC    MYMKT,MYMKT         IF MARKET CHANGED SET NEW MARKET             
         BZ    DF20                                                             
         GOTO1 MSPACK,DMCB,MYMKT,QSTANEW,STFKMS                                 
*                                                                               
DF20     BRAS  RE,CHKKEY                                                        
*                                                                               
DFX      J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        CHECK KEY FOR DELETE                                                   
*                                                                               
CHKKEY   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVIO,AIO                                                         
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   MYKEY,KEY                                                        
         GOTO1 HIGH                                                             
         CLC   0(STAKEYLN,R6),MYKEY      FOUND IT                               
         BNE   CK10                                                             
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         TM    SCNTL,X'80'         IS IT ALREADY DELETED                        
         BO    CK10                                                             
         OI    SCNTL,X'80'         MARK IT DELETED                              
         GOTO1 WRITE               WRITE IT BACK                                
*                                                                               
CK10     MVC   MYKEY(L'MYKEY2),MYKEY2                                           
         MVC   KEY(L'MYKEY3),MYKEY3                                             
         MVC   AIO,SVIO                                                         
         J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*                                                                               
*                                                                               
* ON ENTRY R5 POINTS TO SCANNER TABLE ENTRY                                     
*                                                                               
CHKNET   NMOD1 0,**CHKNET                                                       
         L     RC,0(R1)            RESTORE RC                                   
*                                                                               
*                                                                               
         CLI   0(R5),0                                                          
         BE    NETERR                                                           
         CLI   0(R5),3                                                          
         BH    NETERR                                                           
         MVC   SVNETWK,12(R5)                                                   
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPMED,QMED                                                     
         MVC   STAPCTRY,SVAPROF+7                                               
         MVI   STAPACT,C'C'        CHECK CBLNET EXISTS                          
         MVC   STAPQMKT,ZEROES                                                  
         MVC   STAPQSTA,=C'0000/'                                               
         MVC   STAPQNET,SVNETWK                                                 
         GOTO1 VSTAPACK,(R4)                                                    
         TM    STAPERR,QSTP_INVCBL                                              
         BO    NETERR              INVALID CABLE INPUT FROM SCREEN              
*                                                                               
         MVC   SVSYSNT,STAPNSEQ                                                 
         B     CHKNETX                                                          
*                                                                               
NETERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MSG11),MSG11                                           
         MVC   CONHEAD+12(4),12(R5)   MOVE NETWORK TO MESSAGE                   
         MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
MSG11    DC    C'** ERROR ** XXXX IS NOT A VALID CABLE NETWORK'                 
CHKNETX  J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*&&                                                                             
         EJECT                                                                  
*                                                                               
*        PROCESS PASSIVE PTRS FOR CANADIAN MEDIA C/N                            
*                                                                               
CANPASS  NTR1  BASE=*,LABEL=*                                                   
         CLI   SVAPROF+7,C'C'      IS THIS A CANADIAN AGENCY                    
         BNE   CPX                                                              
         CLI   QMED,C'T'           MEDIA = TV                                   
         BNE   CPX                                                              
         MVC   SVKEY,KEY                                                        
         MVC   KEY,MYKEY           PASS PTR WAS JUST BUILT IN BLDPASS           
         USING STARECD,R6                                                       
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   KEY+3,C'N'          CHANGE TO MEDIA N                            
         MVI   3(R6),C'N'                                                       
*                                                                               
CP10     MVC   MYKEY,KEY                                                        
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                IS THERE A RECORD                            
         NI    DMINBTS,X'F7'                                                    
         CLC   0(STAKEYLN,R6),MYKEY                                             
         BNE   CP30                                                             
         CLI   MODE,XRECDEL                                                     
         BNE   CP20                                                             
         OI    SCNTL,X'80'         MARK IT DELETED                              
         B     CP25                                                             
*                                                                               
CP20     TM    SCNTL,X'80'         IS IT DELETED                                
         BNO   CP40                NO CONTINUE                                  
         NI    SCNTL,X'7F'         ELSE MARK IT UNDELETED                       
*                                                                               
CP25     GOTO1 WRITE               WRITE IT BACK                                
         B     CP40                                                             
*                                                                               
CP30     CLI   MODE,XRECDEL        DELETED RECORD                               
         BE    CP40                                                             
*                                                                               
CP35     MVC   0(STNKLNQ,R6),MYKEY                                              
         GOTO1 ADD                 ADD PASSIVE RECORD                           
*                                                                               
CP40     BRAS  RE,DELMKT           IF MKT CHGED-DELETE OLD PASS PTRS            
         CLI   KEY+3,C'C'                                                       
         BE    CP50                                                             
         MVC   KEY,MYKEY           RE-SET KEY                                   
         MVI   KEY+3,C'C'          PASSIVE PTR FOR MEDIA C                      
         MVI   3(R6),C'C'                                                       
         B     CP10                                                             
*                                                                               
CP50     MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
*                                                                               
CPX      J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*            CHECK TO SEE IF BUYS EXIST FOR THE OLD MARKET            *         
*            ---------------------------------------------            *         
*                                                                     *         
*                     CLT SPECIFIC MASTER RECORD                      *         
*                     --------------------------                      *         
* ON AN ADD   - IF THE MKT IS DIFFERENT THEN THE STATION LEVEL MASTER *         
* ---------     & THERE ARE BUYS FOR THIS CLT/OLD MKT/STA GIVE AN ERR *         
*                                                                     *         
* ON A CHANGE - IF THE MKT IS DIFFERENT THEN THE CLT LEVEL OLD MKT    *         
* -----------   & THERE ARE BUYS FOR THIS CLT/OLD MKT/STA GIVE AN ERR *         
*                                                                     *         
*                                                                     *         
*                     STATION LEVEL MASTER RECORD                     *         
*                     ---------------------------                     *         
* ON AN ADD   - DON'T DO ANYTHING                                     *         
* ---------                                                           *         
*                                                                     *         
* ON A CHANGE - IF THE MARKET IS DIFFERENT THEN THE OLD MARKET        *         
* -----------   & THERE ARE BUYS FOR THIS CLT/OLD MKT/STA GIVE AN ERR *         
*                                                                     *         
***********************************************************************         
VOLDMKT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BAS   RE,SAVEDEF          SAVE STATION FILE DEFINITION                 
         BAS   RE,SETSPF           SET SPOT FILE DEFINITION                     
         MVC   SAVEAM,BAGYMD       SAVE THE A/M                                 
*                                                                               
         LA    R2,MYMKT            OLD MARKET                                   
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VOMK10              NO                                           
         CLI   SSTCLIH+5,0         HAVE A CLT SPECIFIC MASTER REC?              
         BE    VOMNO               NO - DONE                                    
         LA    R2,DEFMKT           OLD MKT FROM STA LEVEL MASTER REC            
*                                                                               
VOMK10   CLC   QMKT,0(R2)          OLD MARKET MATCHES THIS ONE?                 
         JE    VOMNO               YES - DID NOT REALLY CHANGE                  
*                                                                               
         LA    R3,4                TEST 5 BYTES OF MKT/STA                      
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   VOMK11              NO                                           
         MVC   BYTE,BAGYMD         A/M                                          
         NI    BYTE,X'0F'          STRIP AGENCY                                 
         CLI   BYTE,X'01'          MEDIA T?                                     
         BNE   VOMK11              NO                                           
         MVC   SAVEAM,BAGYMD       YES - SAVE THE A/M                           
         NI    SAVEAM,X'F0'        STRIP MEDIA                                  
         OI    SAVEAM,X'08'        SWITCH TO MEDIA C                            
         MVC   WORK+5(4),QSTANEW   STATION                                      
         MVI   WORK+9,C'N'         MEDIA N                                      
         GOTO1 MSPACK,DMCB,(R2),WORK+5,WORK                                     
         MVI   WORK+4,0            ONLY TESTING 4 BYTES OF STATION              
         LA    R3,3                TEST 4 BYTES OF MKT/STA                      
         B     VOMK12              NO                                           
*                                                                               
VOMK11   GOTO1 MSPACK,DMCB,(R2),QSTANEW,WORK                                    
*                                                                               
VOMK12   LA    R6,KEY              R6=BUY RECORD KEY                            
         USING BUYKEY,R6           BUY RECORD DSECT                             
         XC    KEY,KEY             CLEAR THE BUY KEY                            
         MVC   BUYKAM,SAVEAM       A/M                                          
         CLI   SSTCLIH+5,0         HAVE A CLT SPECIFIC MASTER REC?              
         BE    *+10                NO                                           
         MVC   BUYKCLT,BCLT        YES - USE BCLT                               
         MVI   BUYKPRD,X'FF'       POL PRODUCT                                  
         EX    R3,*+8              MOVE 4 OR 5 BYTES                            
         B     *+10                                                             
         MVC   BUYKMSTA(0),WORK    MKT/STA                                      
*                                                                               
VOMK15   GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLI   BUYKPRD,X'FF'       POL PRODUCT?                                 
         BE    VOMK20              YES                                          
         MVI   BUYKPRD,X'FF'       FORCE POL PRODUCT                            
         XC    BUYKMKT(9),BUYKMKT  CLEAR EVERYTHING AFTER PRODUCT               
         B     VOMK15              AND READ HIGH                                
*                                                                               
VOMK20   CLI   CABLE,C'Y'          CABLE?                                       
         BNE   *+8                 NO                                           
         NI    BUYKSTA+2,X'80'     YES - CLEAR NETWORK                          
*                                                                               
         CLC   BUYKAM,SAVEAM       HAVE THE SAME A/M?                           
         BNE   VOMNO               NO - SET CC NEQ (BUY DOES NOT EXIST)         
         CLI   SSTCLIH+5,0         HAVE A CLT SPECIFIC MASTER REC?              
         BE    *+14                NO - CHECK ALL CLIENTS                       
         CLC   BUYKCLT,BCLT        HAVE THE SAME CLIENT?                        
         BNE   VOMNO               NO - SET CC NEQ (BUY DOES NOT EXIST)         
         EX    R3,*+8              TEST 4 OR 5 BYTES                            
         B     *+10                                                             
         CLC   BUYKMSTA(0),WORK    SAME MKT/STA?                                
         BE    VOMYES              YES - BUY EXISTS!                            
*                                                                               
         CLI   SSTCLIH+5,0         HAVE A CLT SPECIFIC MASTER REC?              
         BNE   VOMNO               YES - ALL CHECKED BUY DOES NOT EXIST         
         EX    R3,*+8              TEST 4 OR 5 BYTES                            
         B     *+10                                                             
         CLC   BUYKMSTA(0),WORK    BUY KEY MKT/STA < OLD MKT/STA                
         BL    *+14                YES                                          
         MVC   BUYKPRD(10),XFF     NO - BUMP TO NEXT CLIENT                     
         B     VOMK15              AND GO READHIGH                              
         MVC   BUYKMSTA,WORK       BUMP KEY TO THE OLD MKT/STA                  
         XC    BUYKEST(4),BUYKEST  CLEAR THE ESTIMATE/BUYLINE                   
         B     VOMK15              AND GO READHIGH                              
*                                                                               
VOMNO    MVC   KEY,SVKEY           RESTORE THE KEY                              
         BAS   RE,RSTRDEF          RESTORE STATION DEFINITION                   
         J     NO                  SET CC NEQ (BUY DOES NOT EXIST)              
*                                                                               
VOMYES   EDIT  BUYKEST,(3,WORK+3),ALIGN=LEFT,FILL=0                             
         GOTO1 CLUNPK,DMCB,BUYKCLT,WORK                                         
         MVC   KEY,SVKEY           RESTORE THE KEY                              
         BAS   RE,RSTRDEF          RESTORE STATION DEFINITION                   
         J     YES                 SET CC EQU (BUY DOES EXIST)                  
         DROP  R6                                                               
*                                                                               
SAVEAM   DS    X                   SAVED AGENCY/MEDIA                           
XFF      DC    XL10'FFFFFFFFFFFFFFFFFFFF'                                       
         LTORG                                                                  
*                                                                               
EDTREP   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,KEY                                                        
         CLI   5(R2),0                                                          
         BNE   ER10                                                             
         MVC   8(3,R2),ZEROES      SET DEFAULT REP TO 000                       
         MVI   5(R2),3                                                          
*                                                                               
ER10     CLI   5(R2),3                  MUST INPUT 3 CHARS                      
         BNE   INVERR                                                           
         MVC   MYREP(3),8(R2)                                                   
         CLC   MYREP(3),ZEROES                                                  
         BE    ERX                                                              
*                                       READ REP INTO AIO2                      
         MVC   KEY,ZEROES                                                       
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(3),MYREP                                                   
         MVC   KEY+5(2),AGENCY                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         L     R5,AIO2                                                          
         USING REPRECD,R5                                                       
         LLC   RF,0(R3)                                                         
         SHI   RF,8                                                             
         TM    1(R3),X'02'                                                      
         BZ    *+8                                                              
         SHI   RF,8                                                             
         LA    RE,L'RNAME                                                       
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                                                             
         EXMVC RF,8(R3),RNAME                                                   
         OI    6(R3),X'80'                                                      
*                                                                               
ERX      MVC   KEY,SVKEY                                                        
         J     EXIT                     RETURN                                  
         LTORG                                                                  
         DROP  RB                                                               
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*&&UK                                                                           
*        DISPLAY NETWORKS                                                       
*                                                                               
DISPNET  NMOD1 0,**DISPNET,RR=RE                                                
         L     RC,0(R1)            RESTORE RC                                   
         ST    RE,DNRELO                                                        
         B     DN05                                                             
*                                                                               
DNRELO   DS    A                                                                
*                                                                               
DN05     DS    0H                                                               
         L     R3,=A(T21714)                                                    
         A     R3,DNRELO                                                        
         AH    R3,=Y(MYSYSNET-T21714)  R3-->NETWORK LIST                        
         LA    R2,SSTCNL1          A(NETWORK LIST FIELD)                        
         LA    R6,L'SSTCNL1(R2)    A(END NETWORK LIST FIELD)                    
         XC    BYTECNTR,BYTECNTR                                                
*                                                                               
DN10     MVC   MYBYTE,0(R3)        THIS IS THE NUMBER OF BITS TO MOVE           
         XC    BITCNTR,BITCNTR                                                  
*                                                                               
DN20     TM    MYBYTE,X'80'        IS THIS BIT ON                               
         BNO   DN40                                                             
*        BAS   RE,SHOWNET          WILL GET NETWORK IN WORK+20                  
         GOTO1 =A(SHOWNET),DMCB,RR=Y                                            
         CLC   WORK+20(3),SPACES                                                
         BNH   DN40                                                             
         LA    R1,2                                                             
         CLI   WORK+22,C' '        IS OUTPUT 2 OR 3 LONG                        
         BNH   *+8                                                              
         LA    R1,3                                                             
         LR    R0,R2               SEE IF IT WILL FIT ON THIS LINE              
         AR    R0,R1                                                            
         CR    R0,R6                                                            
         BNH   DN30                                                             
         BCTR  R2,0                                                             
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         MVI   0(R2),C' '          BLANK LAST COMMA                             
                                                                                
         DS    0H                  SET POINTERS TO NEXT DISPLAY LINE            
         LA    R0,SSTCNL1+L'SSTCNL1  WERE WE ON 1ST LINE?                       
         CR    R0,R6                                                            
         BNE   *+16                   NOPE                                      
         LA    R2,SSTCNL2             YEP, SET POINTERS TO 2ND LINE             
         LA    R6,L'SSTCNL2(R2)                                                 
         B     DN30                                                             
                                                                                
         LA    R0,SSTCNL2+L'SSTCNL2  WERE WE ON 2ND LINE?                       
         CR    R0,R6                                                            
         BNE   *+16                   NOPE                                      
         LA    R2,SSTCNL3             YEP, SET POINTERS TO 3RD LINE             
         LA    R6,L'SSTCNL3(R2)                                                 
         B     DN30                                                             
                                                                                
         B     DNX                   NO MORE ROOM--EXIT NOW                     
*                                                                               
DN30     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK+20     DISPLAY CODE                                 
         AR    R2,R1                                                            
         LA    R2,1(R2)                                                         
         CR    R2,R6               SEE IF AT END OF LINE                        
         BNL   *+8                                                              
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
DN40     LLC   R1,MYBYTE           TEST NEXT BIT                                
         SLL   R1,1                                                             
         STC   R1,MYBYTE                                                        
         LLC   R1,BITCNTR          INC N'BITS PROCESSED                         
         LA    R1,1(R1)                                                         
         STC   R1,BITCNTR                                                       
         CLI   BITCNTR,8                                                        
         BL    DN20                                                             
*                                                                               
         LA    R3,1(R3)            GET NEXT BYTE                                
         LLC   R1,BYTECNTR         INC N'BYTES PROCESSED                        
         LA    R1,1(R1)                                                         
         STC   R1,BYTECNTR                                                      
         CLI   BYTECNTR,16                                                      
         BL    DN10                                                             
         BCTR  R2,0                                                             
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
*                                                                               
DNX      DS    0H                                                               
         J     EXIT                                                             
*&&                                                                             
*                                                                               
*        RETURN NETWORK IN WORK+20                                              
*                                                                               
SHOWNET  NTR1  BASE=*,LABEL=*                                                   
         LLC   R1,BYTECNTR         N'BYTES PROCESSED                            
         MHI   R1,8                TO GET N'BITS                                
         LLC   R2,BITCNTR          N'BITS PROCESSED                             
         LA    R2,1(R2)                                                         
         AR    R1,R2                                                            
         XC    WORK,WORK                                                        
         OI    WORK+2,X'F0'        CABLE                                        
         STC   R1,WORK+4                                                        
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+10,WORK+15                         
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
NOSEQNUM NTR1  BASE=*,LABEL=*      ASSIGN NON SEQUENTIAL NUM                    
         XC    BLOCK(32),BLOCK                                                  
         LA    R4,BLOCK                                                         
         USING STAPACKD,R4                                                      
         MVI   STAPACT,C'N'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPMED,QMED                                                     
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPSTA(2),HALF     SET SEQUENCE NUMBER                          
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0           TABLE MUST BE FULL                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   HALF,STAPSEQ                                                     
         DROP  R4                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP RB                                                                
*                                                                               
REQREC   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY(L'SVKEY),KEY                                               
         MVC   KEY(L'SVKEY),ORIGKEY   RESTORE KEY                               
         L     R3,AIO2                                                          
         XC    0(110,R3),0(R3)         GENERATE REQUEST RECORD                  
         MVI   10(R3),45                                                        
         MVI   14(R3),106                                                       
         MVC   26(80,R3),SPACES                                                 
         MVC   26(2,R3),=C'45'                                                  
         MVC   28(2,R3),AGENCY                                                  
         MVC   30(1,R3),QMED                                                    
         MVC   31(3,R3),=C'ALL'                                                 
         CLC   KEY+9(3),ZEROES                                                  
         BE    *+10                                                             
         MVC   31(3,R3),QCLT                                                    
         MVC   36(1,R3),KEY                                                     
         MVC   44(5,R3),KEY+2                                                   
         MVC   94(7,R3),=C'CONTROL'                                             
         MVI   93(R3),C'A'                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    *+8                                                              
         MVI   93(R3),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',(R3),(R3)                     
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVKEY),SVKEY                                               
         J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*&&UK                                                                           
****************************************************************                
*=========== DISPLAY NETWORK LIST FROM TWO NEW FIELDS==========*                
*=========== SCBL24 AND SCBLSEQ              ==================*                
*                                                                               
DISPNET2 NTR1  BASE=*,LABEL=*                                                   
         LA    R3,SVSCBL24                                                      
         LA    R7,1                SET BIT NUMBER FOR TOP 24                    
         LA    R5,MYCABTAB                                                      
         XCEF  (R5),264                                                         
*                                                                               
DN2LOOP  TM    0(R3),X'80'                                                      
         BZ    DNEXT24                                                          
*                                                                               
         XC    WORK,WORK                                                        
         STC   R7,WORK+4                                                        
         MVI   WORK+2,X'F0'                                                     
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+10,WORK+15                         
         MVC   0(3,R5),WORK+20                                                  
         LA    R5,3(R5)                                                         
*                                                                               
DNEXT24  ZICM  RE,SVSCBL24,3                                                    
         SLL   RE,1                                                             
         STCM  RE,7,SVSCBL24                                                    
         LA    R3,SVSCBL24                                                      
         LA    R7,1(R7)                                                         
         OC    SVSCBL24,SVSCBL24                                                
         BNZ   DN2LOOP                                                          
*                                                                               
         LA    R6,SVSCBLSQ                                                      
         LA    R7,64(R6)                                                        
         MVI   COUNT,25                                                         
D2LOOP   CLI   0(R6),0                                                          
         BE    DIDN24                                                           
         CR    R6,R7                                                            
         BH    DIDN24                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(5),MYMKTSTA                                                 
         OC    WORK+4(1),COUNT                                                  
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+10,WORK+15                         
         MVC   0(3,R5),WORK+20                                                  
*                                                                               
         LA    R6,1(R6)                                                         
         LLC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNT                                                         
         LA    R5,3(R5)                                                         
         B     D2LOOP                                                           
*                                                                               
DIDN24   DS    0X                                                               
         LA    R5,MYCABTAB                                                      
         XC    COUNT,COUNT                                                      
*   COUNT TOTAL NUMBER OF NETWORKS IN TABLE TO SORT                             
SETCOUNT OC    0(3,R5),0(R5)                                                    
         BZ    SETSORT                                                          
         LLC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNT                                                         
         LA    R5,3(R5)                                                         
         B     SETCOUNT                                                         
*                                                                               
SETSORT  DS    0H                                                               
         LA    R5,MYCABTAB                                                      
         LLC   R7,COUNT                                                         
         GOTO1 XSORT,DMCB,(0,MYCABTAB),(R7),3,3,0                               
*===========NOW WE MUST DISPLAY NON TOP 24 NETWORKS ============*               
         LA    R6,SVSCBLSQ                                                      
         LA    R5,MYCABTAB                                                      
*        B     DN2X                                                             
         LA    R2,SSTCNL1          A(NETWORK LIST FIELD)                        
         LA    R8,L'SSTCNL1(R2)    A(END NETWORK LIST FIELD)                    
*                                                                               
DNON24   OC    0(3,R5),0(R5)       IF WE SEE ZERO THEN STOP                     
         BZ    DN2X                                                             
*                                                                               
         LR    R0,R2                                                            
         LA    RE,3                                                             
         AR    R0,RE                                                            
         CR    R0,R8                                                            
         BH    DNEWLINE                                                         
         B     DN230                                                            
*                                                                               
DNEWLINE DS    0H                                                               
         BCTR  R2,0                                                             
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
         LA    RE,SSTCNL1+L'SSTCNL1  WERE WE ON 1ST LINE?                       
         CR    R2,RE                                                            
         BH    *+16                   NOPE                                      
         LA    R2,SSTCNL2             YEP, SET POINTERS TO 2ND LINE             
         LA    R8,L'SSTCNL2(R2)                                                 
         B     DN230                                                            
*                                                                               
         LA    RE,SSTCNL2+L'SSTCNL2  WERE WE ON 2ND LINE?                       
         CR    R2,RE                                                            
         BH    *+16                   NOPE                                      
         LA    R2,SSTCNL3             YEP, SET POINTERS TO 3RD LINE             
         LA    R8,L'SSTCNL3(R2)                                                 
         B     DN230                                                            
*                                                                               
         B     DN2X                   NO MORE ROOM--EXIT NOW                    
*                                                                               
DN230    DS    0H                                                               
         MVC   0(3,R2),0(R5)                                                    
         CLI   2(R5),C' '                                                       
         BNE   DN240                                                            
         MVI   2(R2),C','                                                       
         LA    R2,3(R2)                                                         
         B     *+12                                                             
*                                                                               
DN240    MVI   3(R2),C','                                                       
         LA    R2,4(R2)                                                         
         LA    R5,3(R5)                                                         
         B     DNON24                                                           
*                                                                               
DN2X     BCTR  R2,0                                                             
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
*        MAKE SURE USER DID NOT DELETE NETWORKS ON CHANGE                       
*        FOR TOP 24 AND NON TOP 24.  ON ENTRY SVNTNEW IS SET TO NEW             
*        OP 24 NETWORK LIST FROM SCREEN                                         
*                                                                               
CKDELNT2 NTR1  BASE=*,LABEL=*                                                   
         LA    R3,SVSCBL24         ORIGINAL NETWORKS                            
         MVC   SVNTORG,0(R3)                                                    
         LA    R4,SVNTNEW                                                       
*                                                                               
CK2D10   DS    0H                                                               
         TM    0(R3),X'80'                                                      
         BZ    CK2D40                                                           
         TM    0(R4),X'80'                                                      
         BZ    CK2DNO                                                           
*                                                                               
CK2D40   ZICM  RE,SVNTORG,3                                                     
         SLL   RE,1                                                             
         STCM  RE,7,SVNTORG                                                     
         LA    R3,SVNTORG                                                       
         ZICM  RE,SVNTNEW,3                                                     
         SLL   RE,1                                                             
         STCM  RE,7,SVNTNEW                                                     
         LA    R4,SVNTNEW                                                       
         OC    SVNTORG,SVNTORG                                                  
         BNZ   CK2D10                                                           
*                                                                               
* IF TOP 24 IS GOOD NOW CHECK NON TOP 24                                        
* EVERY OLD NETWORK IN LIST MUST BE IN THE NEW NETWORK LIST                     
         LA    R3,SVSEQORG                                                      
         LA    R4,SVSEQNEW                                                      
CK2D100  DS    0H                                                               
         CLI   0(R3),0                   IF NO ORIGINALS THEN WE DON'T          
         BE    CK2DYES                    HAVE TO CHECK FOR DELETIONS           
*                                                                               
         CLC   0(1,R3),0(R4)                                                    
         BE    CK2D105                    EQUAL-CHK NEXT ORIGINAL NET           
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                    IF WE CHKED ALL THE NEW ONES          
         BE    CK2DNO                                                           
         LA    RE,SVSEQNEW+64             IF WE HAVE TO CHECK ALL THE           
         CR    R4,RE                                                            
         BH    CK2DNO                     NEW AND DON'T FIND IT.                
         B     CK2D100                                                          
CK2D105  LA    R3,1(R3)                                                         
         LA    R4,SVSEQNEW                                                      
         LA    RE,SVSEQORG+64             IF WE CHK ALL THE ORIGINALS           
         CR    R3,RE                       ALREADY THEN WE ARE FINE             
         BH    CK2DYES                                                          
         B     CK2D100                                                          
*                                                                               
*                                                                               
CK2DYES  SR    RC,RC                                                            
CK2DNO   LTR   RC,RC                                                            
CK2DX    J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*&&                                                                             
*********                                                                       
* SETUP *                                                                       
*********                                                                       
SETUP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   TWASCR,CALLSTCK                                                  
         BNE   *+12                                                             
         MVI   CALLSP,0                                                         
         MVI   CALLSTCK,0                                                       
*                                                                               
         XC    SSTPFKY,SSTPFKY                                                  
*                                                                               
         OI    CONSERVH+6,X'80'                                                 
         OI    CONSERVH+1,X'01'                                                 
*                                                                               
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    SETUP50                                                          
*                                                                               
         CLI   PFKEY,03            DISABLE PF3 HERE                             
         BNE   *+8                                                              
         MVI   PFKEY,X'FF'         DISABLE PF KEY                               
         MVC   SSTPFKY+68(11),=C'PF12=RETURN'                                   
         OI    SSTPFKYH+6,X'80'                                                 
         B     SETUP100                                                         
*                                                                               
SETUP50  DS    0H                   NOTHING TO RETURN TO                        
         CLI   PFKEY,12                                                         
         BNE   *+8                                                              
         MVI   PFKEY,X'FF'                                                      
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    SETUP55             NO CANADIAN FIELDS ON MASTER2                
         MVC   SSTPFKY(12),=C'PF03=MASTER2'                                     
         B     SETUP100                                                         
SETUP55  CLI   PFKEY,03            STOP CANADA PF3ING TO MASTER2                
         BNE   *+8                                                              
         MVI   PFKEY,X'FF'                                                      
         MVC   SSTPFKY(12),SPACES  CLEAR TEXT SET BEFORE SVAPROF SET!           
SETUP100 OI    SSTPFKYH+6,X'80'                                                 
         CLI   PFKEY,0                                                          
         BE    SETUPX                                                           
         GOTO1 INITPFKY,DMCB,PFTABLE                                            
SETUPX   J     EXIT                                                             
***********************************************************************         
*        PFKEYS TABLES                                                *         
***********************************************************************         
PFTABLE  DS    0H                                                               
*        MASTER2 RECORD SCREEN                                                  
         DC   AL1(PF03X-*,03,PFTCPROG,(PF03X-PF03)/KEYLNQ,0)                    
         DC   CL3'PM '                 MAINT                                    
         DC   CL8'MASTER2'             RECORD                                   
         DC   CL8'DISP'                ACTION                                   
PF03     DC   AL1(KEYTYTWA,L'SSTMED-1),AL2(SSTMED-T217FFD)                      
         DC   AL1(KEYTYTWA,L'SSTSTA-1),AL2(SSTSTA-T217FFD)                      
PF03X    EQU  *                                                                 
*        RETURN CALLER                                                          
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*                                                                               
*             ROUTINE TO FORMAT MARKET                                          
*             R6 EXPECTED TO ADDRESS STATION RECORD                             
*                                                                               
FMTMKT   NTR1  BASE=*,LABEL=*                                                   
         USING STARECD,R6                                                       
*                                                                               
         TM    MISCFLG1,MF1OFFCD   WE HAVE OFFICE CODE?                         
         BZ    FMTM05               - NOPE                                      
         CLC   SMKT,ZEROES         ONLY OFFICE CODE REC CAN HAVE CHAR 0         
         BE    FMTM10              ...LEGITIMATELY                              
*                                                                               
FMTM05   MVC   SVKEY,KEY                                                        
         MVC   KEY(STAKEYLN),ZEROES                                             
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),SMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),0(R6)    CAREFUL HERE                                 
         BNE   FMTM10              RECORD NOT FOUND                             
         USING MKTRECD,R6                                                       
         MVC   SSTMKTN,MKTNAME                                                  
         MVC   MYALPMKT,MKTALST                                                 
         B     FMTM20                                                           
*                                                                               
FMTM10   TM    MISCFLG1,MF1OFFCD   WE HAVE OFFICE CODE?                         
         BZ    FMTM15               - YUP, NEED THE MESSAGE                     
         XC    SSTMKTN,SSTMKTN     -- MARKET NAME                               
         MVI   SSTMKTNH+5,0        NO INPUT, DELETED                            
         XC    SSTMKT,SSTMKT       -- MARKET NUMBER                             
         MVI   SSTMKTH+5,0                                                      
         B     FMTM20                                                           
*                                                                               
FMTM15   MVC   SSTMKTN(19),=C'**MKT NOT ON FILE**'                              
*                                                                               
FMTM20   OI    SSTMKTNH+6,X'80'                                                 
         MVC   AIO,AIO1       RESET AIO                                         
         MVC   KEY,SVKEY                                                        
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* READ X'99' (RAD) RECORDS, AND CHECK CALL LETTER HISTORY                       
* R6 EXPECTED TO ADDRESS STATION RECORD                                         
* UNIQTMP EXPECTED TO HAVE THE UNIQUE ID FROM THE SCREEN                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
RDRAD    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVDDISP,DATADISP                                                 
         MVC   DATADISP,=AL2(28)                                                
         LR    R2,R6           POINT R2 TO STA REC. R6 NEEDED FOR GETEL         
         USING STARECD,R2                                                       
*                                                                               
         MVC   SVKEY48,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CT99RECD,R6                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVC   CT99KUID,UNIQTMP                                                 
         L     R6,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R6)                      
         CLC   0(L'CT99KEY,R6),KEY                                              
         BNE   RDRADNO                                                          
*                                                                               
         CLI   1(RA),C'*'             DDS TERMINAL?                             
         BE    RDRADYES               DO NOT CHECK CALL LTR HISTORY             
*                                                                               
         MVI   ELCODE,CRCLELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   RDRADNO                                                          
         USING CRCLD,R6                                                         
         CLC   STAKCALL(4),CRCLCLL      COMPARE TO CURRENT CALL LETTERS         
         BNE   RDRAD30             SAME - PROCEED, IF NOT - CHECK X'30'         
         CLC   STAKCALL+4(1),CRCLBND    COMPARE TO CURRENT BAND                 
         BE    RDRADYES                 SAME - OK                               
         DROP  R6                                                               
*                                       NOT THE SAME - CHECK HISTORY            
RDRAD30  L     R6,AIO3                                                          
         MVI   ELCODE,CRCHELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   RDRADNO                                                          
         USING CRCHD,R6                                                         
         CLC   STAKCALL,CRCHHST1        SAME AS PREVIOUS?                       
         BNE   RDRADNO                  NO - ERROR                              
         DROP  R6                                                               
*                                                                               
         DROP  R2                                                               
*                                                                               
RDRADYES MVI   YNFLAG,C'Y'                                                      
         B     *+8                                                              
RDRADNO  MVI   YNFLAG,C'N'                                                      
*                                                                               
         MVC   KEY,SVKEY48                                                      
         MVC   DATADISP,SVDDISP                                                 
*        MVC   AIO,AIO3                                                         
*        GOTO1 HIGH                                                             
*        CLC   KEY(15),KEYSAVE                                                  
*        BE    *+6                                                              
*        DC    H'0'                KEY RESTORE FAILED                           
*        MVC   AIO,AIO1                                                         
*                                                                               
         CLI   YNFLAG,C'Y'                                                      
         JE    YES                                                              
         J     NO                                                               
*                                                                               
YNFLAG   DS    X                                                                
SVDDISP  DS    H                                                                
         LTORG                                                                  
*======================== CHECK CLIENT DEFAULT =======================*         
                                                                                
* CLIENT DEFAULT MUST NOW EXIST BEFORE ADDING A CLIENT EXCEPTION                
                                                                                
         DS    0H                                                               
CKCLTDEF NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY              BUILD KEY OF CLT DEFAULT RECORD              
         USING STARECD,R6                                                       
         MVC   STAKEY(STAKEYLN),ZEROES                                          
         MVI   STAKTYPE,C'S'       STATION MASTER                               
         MVC   STAKMED,QMED        MEDIA                                        
         MVC   STAKCALL,QSTANEW    CALL LETTERS                                 
         MVC   STAKAGY,AGENCY      ALPHA AGENCY                                 
         DROP  R6                                                               
                                                                                
         BAS   RE,SAVEDEF          SAVE CURRENT DEFINITION                      
         BAS   RE,SETDEF            BEFORE REDEFINING IT                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
         BAS   RE,RSTRDEF          RESTORE DEFINITION TO ORIGINAL               
*                                                                               
         CLC   KEY(STAKEYLN),KEYSAVE                                            
         JNE   EXIT                PASS CC TO CALLER                            
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         MVC   DEFMKT,SMKT                                                      
         MVC   SVSYSNAM,SSYSNAME                                                
         J     EXIT                PASS CC TO CALLER                            
*                                                                               
***********************************************************************         
         EJECT                                                                  
*                                                                               
*                                                                               
GETUIDFL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BAS   RE,SAVEDEF          SAVE CURRENT DEFINITION                      
         BAS   RE,SETSPF            BEFORE REDEFINING IT                        
*                                                                               
         MVC   GETSVK,KEY                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           AGENCY RECORD                                
         MVC   KEY+1(2),AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                NO AGENCY RECORD FOUND                       
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AGYEL,R6                                                         
         MVC   SVAGYFL2,AGYFLAG2                                                
         DROP  R6                                                               
*                                                                               
         BAS   RE,RSTRDEF          RESTORE DEFINITION TO ORIGINAL               
         MVC   KEY,GETSVK                                                       
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
*                                                                               
         MVC   FIRSTLBL,=C'****'   UNIQUE  LABEL                                
         MVC   FIRSTLBL+1(2),AGENCY                                             
*                                                                               
         J     YES                                                              
GETSVK   DS    XL15                                                             
         LTORG                                                                  
**********************************************************************          
*        DISPLAY PST CODES                                                      
*                                                                               
         DS    0H                                                               
DISPPST  NTR1  BASE=*,LABEL=*                                                   
         USING STARECD,R6                                                       
         OC    SPST,SPST           IS THERE ANYTHING TO DISPLAY                 
         BZ    DPX                                                              
*                                                                               
         LA    R4,BLOCK                                                         
         USING PSTBLKD,R4                                                       
         XC    0(200,R4),0(R4)     CLEAR INTERFACE BLOCK                        
         XC    200(200,R4),200(R4)                                              
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,SPST                                                          
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   SSTPST,PSTOUT       OUTPUT                                       
         OI    SSTPSTH+6,X'80'                                                  
*                                                                               
DPX      J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RATING SERVICE CALL LETTER FIELDS                             
*        ENTRY - BYTE=RTG SVC / R2=SCREEN FIELD HDR                             
*        EXIT  - ON ERROR, R2=SCREEN FIELD HDR                                  
         DS    0H                                                               
VALRSC   NTR1  BASE=*,LABEL=*                                                   
         MVI   MYBYTE,C'Y'         PRIME CC EQUAL                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   VALRSCX                                                          
         CLI   QMED,C'T'                                                        
         BNE   VALRSCX                                                          
         LA    R2,SSTRS1CH         RTG SVC 1 (NSI) CALL LETTERS                 
         CLI   BYTE,C'N'                                                        
         BE    *+8                                                              
         LA    R2,SSTRS2CH         RTG SVC 2 (BBM) CALL LETTERS                 
*                                                                               
         USING DBLOCKD,R3                                                       
         LA    R3,BLOCK                                                         
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         L     R1,AIO2                                                          
         ST    R1,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBVLST                                                   
         MVI   DBSELMED,C'C'       FOR CANADIAN TV THE MEDIA IS 'C'             
         MVC   DBSELSRC,BYTE                                                    
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    VALRSCX                                                          
         MVC   DBSELSTA(4),SPACES                                               
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DBSELSTA(0),8(R2)                                                
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         CLI   DBERROR,0                                                        
         BNE   VALRSCNX                                                         
         LA    R1,SRS1CALL-STARECD(R6)                                          
         LA    R2,SSTRS1FH                                                      
         CLI   BYTE,C'N'                                                        
         BE    *+12                                                             
         LA    R1,SRS2CALL-STARECD(R6)                                          
         LA    R2,SSTRS2FH                                                      
         MVC   0(L'SRS1CALL,R1),DBSELSTA                                        
*                                                                               
         CLI   5(R2),0             VALIDATE IMPS FLAG                           
         BE    VALRSCX                                                          
         CLI   8(R2),C'Y'                                                       
         BE    VALRSCX                                                          
         CLI   8(R2),C'N'                                                       
         BNE   VALRSCNX                                                         
         CLI   BYTE,C'N'                                                        
         BNE   *+12                                                             
         OI    SFLAG1-STARECD(R6),SQNORS1I                                      
         B     VALRSCX                                                          
         OI    SFLAG1-STARECD(R6),SQNORS2I                                      
         B     VALRSCX                                                          
VALRSCNX MVI   MYBYTE,C'N'                                                      
VALRSCX  CLI   MYBYTE,C'Y'                                                      
         XIT1  REGS=(R2)           PASS CC/R2 TO CALLER                         
         LTORG                                                                  
***********************************************************************         
*  ROUTINE TO HIDE/SHOW COUNTRY/MEDIA SPECIFIC FIELDS                           
***********************************************************************         
         DS    0H                                                               
HIDEFLDS NTR1  BASE=*,LABEL=*                                                   
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   HIDEFX              (SCREEN DFLT IS HIDDEN/PROT)                 
* INITIALISE - RE-HIDE/PROT/TRANSMIT ALL APPLICABLE FIELDS                      
         OI    SSTRS1TH+1,X'0C'    RATING SERVICE STATION CODES                 
         OI    SSTRS1TH+6,X'80'                                                 
         OI    SSTRS1CH+1,X'2C'                                                 
         OI    SSTRS1CH+6,X'80'                                                 
         OI    SSTRS1IH+1,X'0C'                                                 
         OI    SSTRS1IH+6,X'80'                                                 
         OI    SSTRS1FH+1,X'2C'                                                 
         OI    SSTRS1FH+6,X'80'                                                 
         OI    SSTRS2TH+1,X'0C'                                                 
         OI    SSTRS2TH+6,X'80'                                                 
         OI    SSTRS2CH+1,X'2C'                                                 
         OI    SSTRS2CH+6,X'80'                                                 
         OI    SSTRS2IH+1,X'0C'                                                 
         OI    SSTRS2IH+6,X'80'                                                 
         OI    SSTRS2FH+1,X'2C'                                                 
         OI    SSTRS2FH+6,X'80'                                                 
         OI    SSTAMKTH+1,X'20'     PROTECT ALPH MKT                            
         OI    SSTAMKTH+6,X'80'     XMIT INPUT FIELD                            
         NI    SSTNTYHH+1,255-X'0C' NETWORK TYPE                                
         OI    SSTNTYHH+6,X'80'                                                 
         NI    SSTNTYPH+1,255-X'2C'                                             
         OI    SSTNTYPH+6,X'80'                                                 
* NOW - SHOW/UNPROT APPROPRIATE FIELDS                                          
* FOLLOWING ARE CANADIAN MEDIA 'T' ONLY                                         
         CLI   QMED,C'T'           MEDIA T HAS EXTRA FIELDS                     
         BNE   HIDEFX                                                           
         NI    SSTRS1TH+1,255-X'0C'                                             
         NI    SSTRS1CH+1,255-X'2C'                                             
         NI    SSTRS1IH+1,255-X'0C'                                             
         NI    SSTRS1FH+1,255-X'2C'                                             
         NI    SSTRS2TH+1,255-X'0C'                                             
         NI    SSTRS2CH+1,255-X'2C'                                             
         NI    SSTRS2IH+1,255-X'0C'                                             
         NI    SSTRS2FH+1,255-X'2C'                                             
HIDEFX   J     EXIT                                                             
***********************************************************************         
*  ROUTINE TO HIDE/SHOW COUNTRY/MEDIA SPECIFIC FIELDS                           
***********************************************************************         
         DS    0H                                                               
CHKDSTA  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                CLEAR THE KEY                             
         LA    R2,KEY                 R2 = KEY                                  
         USING DS$STAKEYD,R2          DSTA RECORD USING                         
         MVI   DS$STAKTYP,DS$STAKTYPQ RECORD SUB-TYPE X'5A'                     
         MVC   DS$STAKMEDA,QMED       MEDIA                                     
         MVC   DS$STAKSTIN,QSTANEW    STATION                                   
         MVC   KEYSAVE,KEY            SAVE OFF THE KEY                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEYSAVE,KEY               
         CLC   KEY(24),KEYSAVE        HAVE DSTA RECORD FOR MED/STA?             
         J     EXIT                   CHECK CC ON RETURN                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMD4D                                                       
         EJECT                                                                  
         ORG   SSTWORK                                                          
SVIO     DS    F                                                                
*                                                                               
MYMKT    DS    CL4                                                              
DEFMKT   DS    CL4                 DEFAULT MARKET                               
MYFORM   DS    CL4                                                              
QFORM    DS    CL4                                                              
MYEFFDTE DS    CL(L'SEFFDATE)                                                   
CABLE    DS    CL1                                                              
SVSYSNT  DS    XL1                                                              
MYBYTE   DS    XL1                                                              
SVNETNEW DS    XL1                                                              
SVNETORG DS    XL1                                                              
BITCNTR  DS    XL1                                                              
BYTECNTR DS    XL1                                                              
REACTIVE DS    CL1                                                              
OLDMKTS  DS    CL1                                                              
*SVSYSNET DS    CL(16)                                                          
ORIGKEY  DS    CL(STNKLNQ)                                                      
PASSKEY  DS    CL(STNKLNQ)                                                      
MYKEY    DS    CL(STNKLNQ)                                                      
MYKEY2   DS    CL(STNKLNQ)                                                      
MYKEY3   DS    CL(STNKLNQ)                                                      
STKKEY   DS    CL(STKKLNQ)         K TYPE PASSIVE                               
PSTOUT   DS    CL64                NEED A 64 BYTE FIELD FOR OUTPUT              
*                                                                               
SVNETWK  DS    CL3                 ALPHA NET, 3 CHARACTERS                      
SVSCBL24 DS    CL3                                                              
SVSCBLSQ DS    CL64                                                             
SVNTNEW  DS    XL3                                                              
SVNTORG  DS    XL3                                                              
SVSEQORG DS    CL64                                                             
SVSEQNEW DS    CL64                                                             
MYMKTSTA DS    CL5                                                              
COUNT    DS    X                                                                
MYCABTAB DS    CL264                                                            
FIRSTLBL DS    XL4                                                              
SVAGYFL2 DS    X                                                                
MYALPMKT DS    CL3                 MARKET REC ALPHAMKT (FOR CANADA TV)          
*                                                                               
MISCFLG1 DS    X                   MISCELLANEOUS FLAG 1                         
MF1OFFCD EQU   X'80'                - OFFICE CODE INSTEAD OF CLIENT             
MF1ONK   EQU   X'40'                - WORKING ON K PASSIVES NOW                 
MF1USEK  EQU   X'20'                - USING K KEYS                              
MF1CAN   EQU   X'10'                - WE'RE CANADIAN RIGTH NOW                  
MF1LOCK  EQU   X'08'                - VENDOR LOCK FIELD CHANGED                 
OFFICECD DS    XL1                 1 CHARACTER INTERNAL OFFICE CODE             
SVPREPH  DS    CL8                 SAVES THE PAYING REP HEADER                  
SVPREP   DS    CL3                 SAVES THE PAYING REP                         
OFCBLK   DS    XL(OFCLENQ)                                                      
SVSYSNAM DS    CL24                CABLE SYS NAME FROM NON-CLT SPECIFIC         
OLDLOCK  DS    CL1                 OLD LOCK VALUE                               
OLDMIDAS DS    CL1                 OLD MIDAS VALUE                              
*                                                                               
DDDSLIST DS    0C                                                               
         DSDDL PRINT=YES                                                        
*                                                                               
         ORG   SSTWORK+6000        LEAVE ROOM FOR GENCON'S STUFF                
SCANBLK  DS    CL800                                                            
*                                                                               
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
CBLRECD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSLST                                                      
         EJECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENRAD                                                       
         EJECT                                                                  
* CTGENFILE (NEED CTDMREC)                                                      
       ++INCLUDE CTGENFILE                                                      
                                                                                
* SPDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
         SPACE 1                                                                
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDOFFICED         FOR OFFICED                                  
       ++INCLUDE FAJESMAILD        JESMAIL                                      
       ++INCLUDE FAGETTXTD         GETTXT PARM LIST/CONTROL BLOCK DSECT         
*PREFIX=DS$                                                                     
       ++INCLUDE CTGENSTAD         DSTA RECORD                                  
*PREFIX=                                                                        
         EJECT                                                                  
***********************************************************************         
*===================== SPSFM14 (T21714) SAVE AREA ====================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
*                                                                               
CANSTA   DS    H                   CANADIAN STATION NUMBER                      
NOTAUTH  EQU   0175                ERR-NOT AUTHORIZED FOR THIS FUNCTION         
*                                                                               
UNIQTMP  DS    XL6                                                              
SVKEY48  DS    XL48                                                             
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109SPSFM14   01/28/20'                                      
         END                                                                    
